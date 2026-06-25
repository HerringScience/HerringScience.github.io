# Herring Science Council - AFF44 Contract #

# Set up ####

# load packages

library(tidyverse)
library(lubridate)
library(mapdata)
library(rnaturalearth)
library(ggspatial)
library(sf)
library(rgdal)
library(rgeos)
library(RColorBrewer)

# Build and QC workingsheet by linking tagging events to returns and commercial catch data (rel to returnData) ####
  # Days at Large = RELEASE_DATE - Actual.Date

TaggingEvents <- read.csv("TaggingEvents2023.csv") %>%
  select(-X) %>%
  rename(X_tagging = Lat,
         Y_tagging = Lon,
         TAG_NUMBER = Tag_Num,
         TagDate = Date,
         TagVessel = Vessel) # 120420

returnData <- read.csv("returnData.csv") %>% # created at the end of linkReturnstoCC
  select(-X) # 1139

workingsheet <- merge(TaggingEvents, returnData, by=c('TAG_NUMBER')) %>%
  mutate(days_at_large = time_length(interval(TagDate, Actual.Date), unit ="days")) %>% # this created some -1 and -2 DaL. Through discussions with Jenna, it was decided to make these all equal to 0.
  mutate(days_at_large = ifelse(days_at_large %in% c(-1, -2), 0, days_at_large))
    # 1101
  
    # some tags are missing Actual.Date. should ReturnDate then be used?

# QC: why are there some <-1 days at large??

negDaL <- workingsheet %>%
  filter(days_at_large <= -1)  %>%
  select(TAG_NUMBER, TagVessel, TagDate, ReturnDate, Actual.Date, days_at_large, plantReturnPlace, dataorigin) # 5 tags ... still need to be figured out if they can be resolved

     write.csv(negDaL, 'negDaL.csv')

# QC investigating returnData tags that dont link into workingsheet
    # returnData = 1135 observations
    # workingsheet = 1097 observations
    # 1139 - 1101 = 38 missing from TaggingEvents (down from 78 missing after reviewing tagging logs and adding in missing tag numbers to TaggingEvents)

    missingtags <- returnData %>%
      filter(!TAG_NUMBER %in% workingsheet$TAG_NUMBER)
    
    testtagging <- TaggingEvents %>%
      filter(TAG_NUMBER %in% missingtags$TAG_NUMBER) # check if these tags exist in Tagging Events; nope
    
    write.csv(missingtags, 'missingtags.csv') # 38
  

# Investigate unlinked tag returns ####
    
    # Types of unlinked return tags:
    
      # 1. Incomplete returns - These returns were pulled out as return dataframes were first loaded into linkReturnstoCC 
         # for having insufficient initial data for linking to commercial catch (i.e., missing tag number, boat, and/or return date)
    
      incomplete.returns <- read.csv('incomplete.returns.csv') %>%
        select(-c(X.1)) # 300
    
    
      # 2. Unmatched returns - These tags appear to have all the info needed but do not match into commercial catch; includes both mobile and fixed unmatched
    
      unmatchedreturns <- read.csv('unmatchedreturns.csv') %>%
        select(-c(X.1)) %>%
        mutate(Year = year(DATE)) # 19
    
      # 3. Missing tags - Had info and matched into CC, but the tag number itself is not found in the TaggingEvents.csv
      
      missingtags <- read.csv('missingtags.csv') %>%
        mutate(Year = year(ReturnDate)) %>%
        select(-c(X)) # 38
      
      # 4. Days at Large - matched into commercial catch but give negative days at large 
      negDaL <- read.csv('negDaL.csv') # 5 
    
      
  # QC and sort incomplete returns and unmatched returns into categories ####
      
      # Load unlinkedreturns --> = incomplete.returns + unmatchedreturns
      
      unlinkedreturns <- read.csv("unlinkedreturns.csv") # 319
      
      # Remove ones with no tag numbers as these will not be able to be linked to CC
      
      notags_unlinked <- unlinkedreturns %>% # weed out returns that will not be able to be linked as missing tag info
        filter(!TAG_NUMBER %in% c(NA, '', 'Damaged', 'Damaged ')) %>%
        filter(!nchar(as.character(TAG_NUMBER)) < 5) %>% # there were no tags under 5 digits so remove anything under that as it is an invalid tag number
        filter(!TAG_NUMBER == 516934) # notes indicate they couldn't confidently make out this tag number
          # 302
      
      # assign ReturnCompany to each of these tags in notags_review
      
      temp <- notags_unlinked %>% mutate(across(everything(), as.character)) %>%
        select(!X.1)
      
      company_key <- data.frame(
        ReturnCompany = c("Comeau", "RK Murphy", "Scotia Garden", "Cape Breeze"),
        SearchWord = c("comeau", "murphy", "garden", "breeze"))
      
      connorboats <- c("Canada 100", "Brunswick Provider", "Fundy Monarch")
      ConnorBros <- "Connor Bros"
      
      comeauboats <- c("Sealife II", "Lady Janice", "Lady Melissa")
      Comeau = "Comeau"
      
      weirboats <- c("Andrew & Deane", "Michael Eileen", "Capelco", "Caroline B")
      wier = "Weir"
      
      assigned_unlinked <- temp %>%
        mutate(ReturnCompany = apply(., 1, function(row) {
          matching_companies <- company_key$ReturnCompany[sapply(company_key$SearchWord, function(search_word) any(grepl(search_word, row, ignore.case = TRUE)))]
          if (length(matching_companies) > 0) {
            paste(matching_companies, collapse = ", ")
          } else {
            "Other"
          } 
        })) %>% # assigns companies based on company key above
        mutate(ReturnCompany = ifelse(
          apply(., 1, function(row) any(grepl(paste(connorboats, collapse = "|"), row, ignore.case = TRUE))),
          ConnorBros,
          ReturnCompany # assigns Connor Bros if the name of one of their boats is in a row
        )) %>%
        mutate(ReturnCompany = ifelse(
          apply(., 1, function(row) any(grepl(paste(comeauboats, collapse = "|"), row, ignore.case = TRUE))),
          Comeau,
          ReturnCompany # assigns Comeau if the name of one of their boats is in a row
        )) %>%
        mutate(ReturnCompany = ifelse(
          grepl("weir", GearType, ignore.case = TRUE),
          "Weir",
          ReturnCompany # putting all weirs together
        )) %>%
        mutate(ReturnCompany = ifelse(
          apply(., 1, function(row) any(grepl(paste(weirboats, collapse = "|"), row, ignore.case = TRUE))),
          wier,
          ReturnCompany
        ))
            # 302
      
      # create list of incomplete returns that were frozen/canned
      
      processingterms <- c('Frozen Seine Fish', 'Snack', 'Frozen', 'Snackline', 'Fillets', 'Sardines', 
                           'Canned product', 'Processing herring', 'Found on bait', 'Comeau Fillets', 
                           'Comeau Frozen', 'Bait', 'Seafoods')
      
      processingunlinked <- assigned_unlinked %>%
        filter(rowSums(sapply(., function(x) x %in% processingterms)) > 0) %>% # identifies rows with any of the processingterms in any column
        filter(if_any(c(Can.serial.number, Date_codes), ~ !is.na(.) & . != '')) %>% # remove observations without a daycode or serial number
        filter(!Date_codes %in% c('N/A', 'Comeau Fresh Fillets ')) %>% # not true daycodes...
        filter(!grepl(",", Date_codes)) # remove rows with multiple day codes given
            # 17
      
      # find all returns with daycodes
      
      daycodes <- assigned_unlinked %>%
        filter(!is.na(Date_codes) & Date_codes != "") %>%
        filter(!grepl(",", Date_codes))
            # 61 observations with day codes
      

      # add some filters that remove ones we definitely cannot match + remove processing ones and ones with day codes as it will be a different process to try to link those (involves dealing with plants rather than searching various dataframes)
      
      filtered <- assigned_unlinked %>%
        filter(!grepl(",", Date_codes)) %>% # remove rows with multiple day codes given
        filter((!is.na(DATE) & DATE != '') | (!is.na(Date_codes) & Date_codes != '')) %>% # removes rows that have neither a return date or day code; note, some of these removed have date found? not sure if that could be useful...
        filter(!TAG_NUMBER %in% processingunlinked$TAG_NUMBER) %>%
        filter(!TAG_NUMBER %in% daycodes$TAG_NUMBER)
          # 70
            
            # I suspect DFOmailedin ones may be duplicates that have already been matched in... checking:
            common_values <- intersect(filtered_unlinked$TAG_NUMBER, returnData$TAG_NUMBER) # 4 are duplicates... remove
        
            filtered_unlinked <- filtered %>%
              filter(!TAG_NUMBER %in% common_values) # 70
            
          # begin going through filtered_unlinked line by line to see if there is a way to link these... 
              # load CC data (catches.csv)
              # start by isolating the tag you are investigating
              # use that info to find the relevant commercial catch info if possible
                  # in the case of multiple potential matches when return date is the only info available to match into CC, the option with the highest catch was chosen as long as all observations were in the same ground (and it was known if it was fixed or mobile gear)

              catches <- read.csv('catches.csv') %>%
                select(!X) 
        
              passive <- passive %>%
                mutate(DATE = ymd(DATE)) %>%
                mutate(Year = year(DATE))
              
              
              tag <- filtered_unlinked %>%
                filter(TAG_NUMBER == 449374)
              
              test <- catches %>%
                filter(LAND_DATE %in% tag$DATE) %>%
                arrange(LAND_DATE)
              
              test <- catches %>%
                filter(Year %in% tag$Year) %>%
                arrange(Actual.Date)
          
              test <- passive %>%
                filter(Year %in% tag$Year) %>%
                arrange(DATE)
      

# Make basemaps ####

# basic map

NAFOmajor <- st_read("/Users/brittniscott/Desktop/Herring Science Council/R/shapefiles/NAFO Divisions/NAFO_Divisions_SHP/NAFO_Divisions_2021_line_enclosed.shp")

world <- ne_countries(scale = "large", returnclass = "sf")
na <- ne_countries(scale = "large", continent = "North America", returnclass = "sf")

map <- ggplot(data = na) +
  geom_sf(fill = "antiquewhite") +
  geom_sf(data = NAFOmajor, colour = "lightyellow4", size = 0.5) +
  coord_sf(xlim = c(-68, -61), ylim = c(42.6, 45.9)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "white", fill = NA, size = 0.5)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_nautical)

map

# Herring fishing ground base map

herringgroundsshp <- st_read("/Users/brittniscott/Desktop/Herring Science Council/R/shapefiles/DFO/herring_grounds/herring_grounds.shp") # MAKE SURE to download ALL associated files from the sharepoint and have in the same folder as the .shp. Otherwise, the .shp will not work

mycolours <- colorRampPalette(brewer.pal(8, "Set3"))(21)
mycolours21 <- c("#8DD3C7", "#B4E2C0", "palegoldenrod", "#FBFBB4", "#E4E3C2", "#CECBD0", "#C4B4CF", "#D99FAB",
                          "tomato3", "#E88780", "#BD98A2", "#92A9C4", "cadetblue4", "#C4B294", "#F0B36D", "khaki2",
                          "#D0CD66","darkseagreen", "#DCF1B9", "#E2D2B9", "#FCCDE5") 
                          

herringgrounds <- ggplot(data = na) +
  geom_sf(data = herringgroundsshp, aes(fill = Ground, alpha = 0.1)) +
  scale_fill_manual(values = mycolours21) +
  geom_sf(fill = "antiquewhite", colour = "antiquewhite4") +
  geom_sf(data = NAFOmajor, colour = "black", size = 1, alpha = 0.5) +
  coord_sf(xlim = c(-68, -61), ylim = c(42.6, 45.9)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "white", fill = NA, size = 0.5)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_nautical) +
  guides(fill = guide_legend(title = "Ground",
                             override.aes = list(alpha = 0.6))) +
  scale_alpha(guide = 'none')

herringgrounds

# NAFO subunits base map

NAFOshp <- st_read("/Users/brittniscott/Desktop/Herring Science Council/R/shapefiles/DFO/NAFO_SubUnits/NAFO_SubUnits_CanAtlantic_84.shp")

NAFOtrimmed <- NAFOshp %>%
  filter(UnitArea %in% c("4XS","4XR","4XQ","4XM","4XO","4WL","4WK","4XN","4XP","4WJ","5YB","4WF",
                         "5YC","5YF","4WM","4WJ","4WG","4WE","4TH","4TG","4VN","4WD","4WH"))

mycolours23 <- colorRampPalette(brewer.pal(8, "Set3"))(23)

mycolours23 <- c("#8DD3C7","#B1E1C0","#D5EFBA","goldenrod","#EDECBD","#D8D6CA", # customizing colours a bit
                          "#C3C0D6","#CBACC2","#DF9AA1","tomato3","#D29091","#BD98A2",
                          "#96A8C1","darkslategray4","#B8B29F","#E0B37B","#F6B762","#DEC564",
                          "#B4DD6B","yellow3","#CDD796","#E4D2BD","#FCCDE5")
                          

NAFO1 <- ggplot(data = na) +
  geom_sf(data = NAFOtrimmed, aes(fill = UnitArea, colour = NA, alpha = 0.5)) +
  scale_fill_manual(values = mycolours23) +
  scale_colour_manual(values = mycolours23) +
  geom_sf(fill = "antiquewhite", colour = "antiquewhite4") +
  geom_sf(data = NAFOmajor, colour = "black", size = 1, alpha = 0.5) +
  coord_sf(xlim = c(-68, -61), ylim = c(42.6, 45.9)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "white", fill = NA, size = 0.5)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_nautical) +
  guides(fill = guide_legend(title = "Unit Area",
                             override.aes = list(alpha = 0.6)),
         colour = FALSE,
         alpha = FALSE)

NAFO1


centeroids <- st_centroid(NAFOtrimmed)

NAFO2 <- ggplot(data = na) +
  geom_sf(data = NAFOtrimmed, colour = "darkslategray4", fill = NA) +
  geom_sf(fill = "antiquewhite", colour = "antiquewhite4", size = 0.7) +
  geom_sf(data = NAFOmajor, colour = "black", size = 1, alpha = 0.5) +
  geom_sf_text(data=centeroids, aes(label = UnitArea)) +
  coord_sf(xlim = c(-68, -61), ylim = c(42.6, 45.9)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "white", fill = NA, size = 0.5)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_nautical) +
  guides(colour = FALSE,
         alpha = FALSE)

NAFO2


# Seiner Box base map

seinerboxesshp <- NAFOshp <- st_read("/Users/brittniscott/Desktop/Herring Science Council/R/shapefiles/DFO/seiner_boxes/seiner_boxes.shp")

mycolours3 <- c("#8DD3C7","#B1E1C0","#D5EFBA")

seinerboxes <- ggplot(data = na) +
  geom_sf(data = seinerboxesshp, aes(fill = box_type), colour = NA) +
  scale_fill_manual(values = mycolours3) +
  geom_sf(fill = "antiquewhite", colour = "antiquewhite4") +
  geom_sf(data = NAFOmajor, colour = "black", size = 1, alpha = 0.5) +
  coord_sf(xlim = c(-68, -61), ylim = c(42.6, 45.9)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "white", fill = NA, size = 0.5)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_nautical)


seinerboxes



# Link each tagged fish to a spawning ground or unknown ####

# Criteria: 
# Fish must fit into specific shapefiles for either Scots Bay or German Banks
# If a fish was tagged in the German Banks but is returned in Scots Bay, exclude? (code not fully undated following adding tags from review of tagging logs so som things may not work as they should)
# Martin thesis: 'only fish that were tagged and recaptured on the same spawning ground during spawning season are considered in the evaluation'

# assign spawning ground to all tags in rel - Scots Bay vs German Banks vs. unknown. Note not all tagged fish were tagged in these two spots. so this will exclude some fish.
reINFO <- read.csv('relINFO.csv')
r <- read.csv('r.csv')

# Extract spawning box shapefile
NAFOmajor <- st_read("/Users/brittniscott/Desktop/Herring Science Council/R/shapefiles/NAFO Divisions/NAFO_Divisions_SHP/NAFO_Divisions_2021_line_enclosed.shp")
world <- ne_countries(scale = "large", returnclass = "sf")
na <- ne_countries(scale = "large", continent = "North America", returnclass = "sf")
seinerboxesshp <- NAFOshp <- st_read("/Users/brittniscott/Desktop/Herring Science Council/R/shapefiles/DFO/seiner_boxes/seiner_boxes.shp")

spawningareasshp <- seinerboxesshp %>%
  filter(box_type == "spawning area")

spawningareas <- ggplot(data = na) +
  geom_sf(data = spawningareasshp) +
  geom_sf(fill = "antiquewhite", colour = "antiquewhite4") +
  geom_sf(data = NAFOmajor, colour = "black", size = 1, alpha = 0.5) +
  coord_sf(xlim = c(-68, -61), ylim = c(42.6, 45.9)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "white", fill = NA, size = 0.5)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_nautical)


spawningareas # beuno


# Assign all tagged fish (from df = rel) to either 1.) German Banks spawning ground, 2.) Scots Bay spawning ground, or 3.) none      

rel <- read.csv('alltags.csv')

# convert X & Y to coord points
reltemp <- rel %>% 
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>% 
  st_transform(st_crs(spawningareasshp)) # align coordinate systems

assignedground <- st_join(reltemp, spawningareasshp, left = F) # not left = inner join 

assignedground

# grab all tags in German Bank spawning ground and in Scots Bay spawning ground

GBtags <- assignedground %>%
  filter(Ground == "German Bank")

SBtags <- assignedground %>%
  filter(Ground == "Scots Bay")

# grab tag numbers of all fish that have been assigned
GBTAG_NUMBER <- as.vector(GBtags$TAG_NUMBER) # 27006
SBTAG_NUMBER <- as.vector(SBtags$TAG_NUMBER) # 19550

nogroundtags <- rel %>%
  filter(!TAG_NUMBER %in% c(SBTAG_NUMBER, GBTAG_NUMBER))

unknownTAG_NUMBER <- as.vector(nogroundtags$TAG_NUMBER) # 70500

  # rel 117056 - the above adds up right
 

# add column to rel and workingsheet that assigns each fish a spawning ground or 'unknown'

# vectors of TAG_NUMBERS:
unknownTAG_NUMBER
SBTAG_NUMBER
GBTAG_NUMBER

relassigned <- rel %>%
  mutate(SpawningGround = case_when(
    TAG_NUMBER %in% unknownTAG_NUMBER ~ "unknown",
    TAG_NUMBER %in% SBTAG_NUMBER ~ "Scots Bay",
    TAG_NUMBER %in% GBTAG_NUMBER ~ "German Bank"
  ))

table(relassigned$SpawningGround) # seemed to have worked!

workingsheetassigned <- workingsheet %>%
  mutate(SpawningGround = case_when(
    TAG_NUMBER %in% unknownTAG_NUMBER ~ "unknown",
    TAG_NUMBER %in% SBTAG_NUMBER ~ "Scots Bay",
    TAG_NUMBER %in% GBTAG_NUMBER ~ "German Bank"
  )) %>%
  filter(!is.na(SpawningGround)) # Tag returns that have missing tagging logs are showing up as NA so need to be removed... could also be classified as 'unknown'
    # 1060

# map to make sure it looks right

assigned <- ggplot(data = na) +
  geom_sf(data = spawningareasshp, fill = "#8DD3C7", colour = NA) +
  geom_point(data = workingsheetassigned, aes(x = X_tagging, y = Y_tagging, colour = SpawningGround)) + # this had all been working but now the points are not showing up?
  geom_sf(fill = "antiquewhite") +
  geom_sf(data = NAFOmajor, colour = "lightyellow4", size = 0.5) +
  coord_sf(xlim = c(-68, -61), ylim = c(42.6, 45.9)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "white", fill = NA, size = 0.5)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_nautical)


assigned # great



# Make a version of workingsheet with SpawningGroundTag and SpawningGroundReturn and a column indicating if they are the same (Y/N)

workingsheettest <- workingsheetassigned %>%
  rename('SpawiningGroundtag' = 'SpawningGround')

  # get spawning grounds for return locations

  temp <- workingsheetassigned %>% 
    rename('SpawiningGroundtag' = 'SpawningGround') %>%
    st_as_sf(coords = c("X_return", "Y_return"), crs = 4326) %>% 
    st_transform(st_crs(spawningareasshp)) # align coordinate systems

  wstemp <- st_join(temp, spawningareasshp, left = F) # not left = inner join 

  wstemp

  # grab tag numbers for each GB, SB, and unknown
  
  GBtagsreturn <- wstemp %>%
    filter(Ground == "German Bank")
  
  SBtagsreturn <- wstemp %>%
    filter(Ground == "Scots Bay")
  
  # grab tag numbers of all fish that have been assigned
  GBTAG_NUMBERreturn <- as.vector(GBtagsreturn$TAG_NUMBER)
  SBTAG_NUMBERreturn <- as.vector(SBtagsreturn$TAG_NUMBER)
  
  unknowntagsreturn <- workingsheet %>%
    filter(!TAG_NUMBER %in% c(SBTAG_NUMBERreturn, GBTAG_NUMBERreturn))
  
  unknownTAG_NUMBERreturn <- as.vector(unknowntagsreturn$TAG_NUMBER)
  
  # add new column to assignedworkingsheet
  
  groundsx2 <- workingsheetassigned %>%
    rename('SpawiningGroundtag' = 'SpawningGround') %>%
    mutate(SpawningGroundreturn = case_when(
      TAG_NUMBER %in% unknownTAG_NUMBERreturn ~ "unknown",
      TAG_NUMBER %in% SBTAG_NUMBERreturn ~ "Scots Bay",
      TAG_NUMBER %in% GBTAG_NUMBERreturn ~ "German Bank"
    ))
  
  # WHY NAs? e.g., TAG_NUMBER 459473 <- just straight up not in either vector... why dat....
  '459473' %in% GBTAG_NUMBERreturn
  '459473' %in% SBTAG_NUMBERreturn
  '459473' %in% unknownTAG_NUMBERreturn
        
        table(groundsx2$SpawningGroundreturn) 
        # 51+50+199 --> thus 21 NAs....
  
# Catch Weighting (coding incomplete) ####

        # Follow Ryan Martin's thesis:
            # 1. Start with workingsheetassigned
            # 2. Make df summarizing # of tags/set
            # 3. Find # of tags/1000(?) tonnes
            # 4. For each tag, have days at large and associated adjustment factor (#tag/tonnage for the set it was caught in)
 
   # make id column for sets in workingsheetCW
        
        workingsheetCWtemp <- workingsheetassigned %>%
          mutate(id = paste(ReturnDate, ReturnVessel, CATCH_T)) 
  
    # add count column
    # /1000
      
    CWcount <- workingsheetCWtemp %>%
      group_by(id) %>%
      count()
    
    workingsheetCW <- merge(workingsheetCWtemp, CWcount, by="id") %>%
      rename(numtagsfromset = n) %>%
      mutate(tagsperton = (numtagsfromset/CATCH_T),
             tagsper1000ton = (((1000/CATCH_T)*numtagsfromset)))
    
            table(duplicated(workingsheetCW$TAG_NUMBER)) # ensure no duplicated tags made in the process
    
    # find mean AdstFac by day at large
        # plot DaL vs mean Adj factor
            
    test <- workingsheetCW %>%
      group_by(days_at_large) %>%
      mutate(mean = mean(AdjFactor))
    
    
            
            
# Turnover (coding incomplete) ####
  
# make cumulative proportion of tag returns (%) vs. days at large plot
    ## Step 1: Make table counting number of tags per day at large value (count number of observations/rows as each row = 1 tag)
    ## Step 2: Make the count cumulative
    ## Step 3: Make it proportionate

  proptagsbyyear <- workingsheetCW %>% # workingsheet with assigned grounds and adjustment factor
    filter(!SpawningGround == "unknown") %>%
    mutate(returnyear = year(ReturnDate)) %>%
    group_by(returnyear, SpawningGround) %>%
    arrange(days_at_large) %>%
    count(days_at_large) %>%
    rename(numtags = n) %>%
    mutate(cumtags = cumsum(numtags)) %>%
    mutate(prop = cumtags/sum(numtags)) %>%
    mutate(returnyear = as.factor(returnyear))
  
      # sum how many tags by year to check what totals should be 
      test <- workingsheetCW %>%
        mutate(returnyear = year(ReturnDate)) %>%
        group_by(returnyear, SpawningGround) %>% 
        count(returnyear) # counts total number of observations/rows/tags by year

 
  # plot by year and also by means with st dev = 1 of all years
      # by year
      
      ggplot(proptagsbyyear) +
        geom_point(aes(x = days_at_large, y=prop, colour = returnyear)) +
        geom_line(aes(x = days_at_large, y=prop, fill=returnyear), alpha = 0.1) +
        facet_wrap(~ SpawningGround) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"),
              legend.position = c(0.7, 0.45),
              legend.direction = "vertical",
              legend.title=element_blank(),
              legend.key=element_rect(fill="white"),
              legend.text = element_text(size = 12)) +
        xlab("Days at large") + 
        ylab("Cumulative proportion of tag returns (%)")
      
    # find cumulative mean
      
        # make summary df from workingsheetassigned with mean cumulative # of tags by days at large and standard error for each spawning ground
      
      sumdf <- workingsheetassigned %>%
        filter(!SpawningGround == "unknown") %>%
        mutate(returnyear = year(ReturnDate)) %>%
        group_by(returnyear, SpawningGround) %>%
        arrange(days_at_large) %>%
        count(days_at_large) %>%
        rename(numtags = n) %>%
        mutate(cumtags = cumsum(numtags)) %>%
        mutate(prop = cumtags/sum(numtags)) %>%
        ungroup(returnyear, SpawningGround) %>%
        select(-returnyear) %>%
        group_by(days_at_large, SpawningGround) %>%
        summarize(n = n(), # SPAWNING GROUNDS LOST....
                  meanprop = mean(prop),
                  sd = sd(prop),
                  se = sd/sqrt(n))
      
      sumdf <- as.data.frame(sumdf)
      sumdf  

      # make average/ yr plot
      
      ggplot(sumdf) +
        geom_smooth(aes(x = days_at_large, y = meanprop), se=TRUE) +
        geom_point(aes(x = days_at_large, y=meanprop)) +
        facet_wrap(~ SpawningGround) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"),
              legend.position = c(0.7, 0.45),
              legend.direction = "vertical",
              legend.title=element_blank(),
              legend.key=element_rect(fill="white"),
              legend.text = element_text(size = 12)) +
        xlab("Days at large") + 
        ylab("Mean cumulative proportion of tag returns (%)")
      
    # convert sumdf to be the log linear relationship (followed https://www.youtube.com/watch?v=dGW3zhh2OCA&ab_channel=EugeneO%27Loughlin)
        # logarithmic regression formula = y = b Logx + a (natural log)
        # x = DaL
        # y = meanprop
        # a = expected value of y when x=1
        # b = some form of slope?
      
      str(sumdf)
      
      SBsumdf <- sumdf %>%
        filter(!days_at_large == 0,
               !days_at_large > 50) %>% # trimmed to 50 days at large as per Melvin 2020
        filter(SpawningGround == "Scots Bay")
        
      GBsumdf <- sumdf %>%
        filter(!days_at_large == 0,
               !days_at_large > 50) %>%
        filter(SpawningGround == "German Bank")
      
      logmodelSB <- lm(meanprop ~ log(days_at_large), SBsumdf) # was giving an error cause of days at large having a value of 0 so removed DaL = 0
      summary(logmodelSB) # y = 0.66096 + 0.06555 ln (x) ; R-squared = 0.5655, p = 0.001921
      
      logmodelGB <- lm(meanprop ~ log(days_at_large), GBsumdf) 
      summary(logmodelGB) # y = -0.009049 + 0.219463 ln (x) ; R-squared = 0.7314, p = 5.156e-08

          # When SB and GB combined: # Fitted Model = y = 0.06048 + 0.17708 log(x); R-squared = 0.7855, p < 2.2e-16
      
      
      ggplot(SBsumdf) +
        geom_point(aes(x = days_at_large, y = meanprop)) +
        geom_smooth(aes(x = days_at_large, y = meanprop), method = 'lm', formula = y~log(x), se = FALSE) +
        annotate("text", x=40, y=0.3, label = "P = 0.66096 + 0.06555 ln(day)", size = 5, col="lightslategrey", fontface = 2) +
        annotate("text", x=40, y=0.25, label = quote(R^2~"= 0.5655, p = 0.001921"), size = 4, col="lightslategrey", fontface = 2) +
        xlab("Days at large") + 
        ylab("Mean cumulative proportion of tag returns (%)") +
        theme(panel.background = element_blank(),
              axis.line = element_line(colour = "black"))
 
      ggplot(GBsumdf) +
        geom_point(aes(x = days_at_large, y = meanprop)) +
        geom_smooth(aes(x = days_at_large, y = meanprop), method = 'lm', formula = y~log(x), se = FALSE) +
        annotate("text", x=40, y=0.3, label = "P = -0.009049 + 0.219463 ln(day)", size = 5, col="lightslategrey", fontface = 2) +
        annotate("text", x=40, y=0.25, label = quote(R^2~"= 0.7314, p = 5.156e-08"), size = 4, col="lightslategrey", fontface = 2) +
        xlab("Days at large") + 
        ylab("Mean cumulative proportion of tag returns (%)") +
        theme(panel.background = element_blank(),
              axis.line = element_line(colour = "black"))
  
# Distance traveled vs. days at large plot (coding incomplete) ####

library(geosphere)
library(tidyverse)     
     
 tagcoords <- workingsheet %>%
   arrange(TAG_NUMBER) %>%
   select(Y_tagging, X_tagging)
      
 returncoords <- workingsheet %>%
   arrange(TAG_NUMBER) %>%
   select(Y_return, X_return)
       
dist <- distGeo(tagcoords, returncoords) 
      
distance <- workingsheet %>%
        arrange(TAG_NUMBER) %>%
        mutate(dist.km = (dist/1000))

ggplot(distance) +
  geom_point(aes(x = days_at_large, y = dist.km)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Days at large") + 
  ylab("Distance traveled (km)") +
  geom_vline(xintercept = 365, colour = 'grey')




## straight line distance except when land is in the way, then most direct route is calculated

# check https://www.r-bloggers.com/2020/02/three-ways-to-calculate-distances-in-r/

#add in tagging and returned locations

tagged <- fishinggrounds + geom_point(data = workingsheet, size = 1, colour = "blue", #all fish that were tagged and eventually returned
                                      aes(x = X_tagging, y = Y_tagging))
tagged

alltagged <- fishinggrounds + geom_jitter(data = TaggingEvents, size = 1, colour = 'red', width = 0.01, height = 0.01,
                                          aes(x=X_tagging, y=Y_tagging))

alltagged

alltagged <- fishinggrounds + geom_point(data = TaggingEvents, size = 1, colour = 'red', aes(x=X_tagging, y=Y_tagging))

alltagged

returned <- map + geom_point(data = workingsheet, size = 1, colour = "blue", 
                             aes(x = X_return, y = Y_return))

returned


tagandreturn <- tagged + geom_point(data = workingsheet, size = 1, colour = "blue",
                                    aes(x = X_returned, y = Y_returned))
tagandreturn

alltagandreturn <- alltagged + geom_point(data = workingsheet, size = 1, colour = "blue", #all fish that were tagged and eventually returned
                                          aes(x = X_tagging, y = Y_tagging))

alltagandreturn

# add in arrows to visualize straight line distances:

lines <- ggplot(data = na) +
  geom_sf(fill = "antiquewhite") +
  geom_sf(data = NAFO, colour = "lightyellow4", size = 0.5) +
  coord_sf(xlim = c(-68, -61), ylim = c(42.6, 45.9)) +
  geom_segment(data = workingsheet, colour = "black", lwd = 1, arrow = arrow(length = unit(0.5, 'cm')),
               aes(x=X_tagging, y = Y_tagging, xend = X_return, yend = Y_return)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "white", fill = NA, size = 0.5)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_nautical)

weirtags <- c(44089, 44358, 44374, 44639, 44790)
weirtags <- workingsheet %>%
  filter(TAG_NUMBER %in% weirtags)

weirlines <- ggplot(data = na) +
  geom_polygon(polysT, mapping = aes(x=X, y=Y, fill = Box, alpha = 0.5)) +
  scale_fill_manual(values = mycolours) +
  geom_sf(fill = "antiquewhite") +
  geom_sf(data = NAFO, colour = "lightyellow4", size = 0.5) +
  coord_sf(xlim = c(-68, -61), ylim = c(42.6, 45.9)) +
  geom_segment(data = weirtags, colour = TAG_NUMBER, lwd = 1, arrow = arrow(length = unit(0.5, 'cm')),
               aes(x=X_tagging, y = Y_tagging, xend = X_return, yend = Y_return)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "white", fill = NA, size = 0.5),
        legend.position = c(0.9, 0.4)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_nautical) +
  guides(fill = guide_legend(title = "Ground")) +
  scale_alpha(guide = 'none')

lines
# if you want tag labels:   geom_label(data = workingsheet, aes(label = TAG_NUMBER, x = X_tagging, y = Y_tagging))


# Find distance between points (including around land if needed) 

library(fasterize)
library(raster)
library(dplyr)
library(rgdal)
library(sf)
library(maptools)

world <- ne_countries(scale = "large", returnclass = "sf")
na <- ne_countries(scale = "large", continent = "North America", returnclass = "sf")

# start by converting shapefile to raster
r <- raster(extent(na), nrows = 10000, ncols = 10000) # use 10000

rraster <- fasterize(summarize(na), r)
rraster

# plot to make sure it worked - to use ggplot, convert raster to a df in 2 steps

plot(rraster,
     xlim=c(-68, -61), ylim=c(42.6, 45.9)) # works!

r1 <- rasterToPoints(rraster, spatial = TRUE)
rdf <- data.frame(r1)

ggplot() +
  geom_raster(data = rdf, aes(x = x, y = y)) +
  coord_sf(xlim = c(-68, -61), ylim = c(42.6, 45.9))

# Now need to ID raster cells where points fall (for both tagged and returned coords)

tagxy <- workingsheet %>%
  select(X_tagged, Y_tagged)

returnxy <- workingsheet %>%
  select(X_returned, Y_returned)

### CREATE A FORLOOP TO USE rraster TO CALULATE THE DISTANCE BETWEEN COORESPONDING TAG AND RETURN COORDS

# try working through it with just 2 coords to find the distance between....

testpoints <- returnxy %>%
  filter(row_number() %in% c(1, 21))

rast_pts <- rraster

icell <- cellFromXY(rraster, testpoints)

rast_pts[icell[1]] <- 2

d <- gridDistance(rast_pts, origin = 2,
                  omit = 1)/1000

d[icell]

# create df of tag and return points and make it work using that

returnxy$returnpoints = paste(returnxy$Y_returned, returnxy$X_returned)
tagxy$tagpoints = paste(tagxy$Y_tagged, tagxy$X_tagged)

tagxy <- tagxy %>%
  mutate(refno = 1:343)

returnxy <- returnxy %>%
  mutate(refno = 1:343)

points <- merge(tagxy, returnxy, by = 'refno') %>%
  select(tagpoints, returnpoints)

testpoints <- points %>%
  filter(row_number() %in% c(1,2))

rast_pts <- rraster






# brainstorming....

tagxy <- workingsheet %>%
  select(X_tagged, Y_tagged)

returnxy <- workingsheet %>%
  select(X_returned, Y_returned)

tagcell <- cellFromXY(rraster, tagxy)
returncell <- cellFromXY(rraster, returnxy)


rast_pts[tagcell[1], returncell[1]] <- 2

d <- gridDistance(rast_pts, origin = 2,
                  omit = 1)/1000

d[icell]











# forloop 

for(i in 1:nrows(points)) {
  
  
  
  
  
  
}



# chatGPT code:

library(geosphere)

# Generate some random points for set 1 and set 2
set.seed(123)
set1_points <- matrix(rnorm(2*10, mean = c(-122, 37), sd = c(1, 1)), ncol = 2)
set2_points <- matrix(rnorm(2*10, mean = c(-122.5, 37.5), sd = c(1, 1)), ncol = 2)

# Calculate the distances between each point in set 1 and every point in set 2
distances <- distm(set1_points, set2_points)

# Find the minimum distance
min_distance <- min(distances)

# Print the result
cat("The closest distance between the two sets of points is", min_distance, "meters.")

#In this code, we generated 10 random points for set 1 and set 2, which are located in the ocean around 
# the coordinates (-122, 37) and (-122.5, 37.5), respectively. We then used the distm() function to 
# calculate the distances between each point in set 1 and every point in set 2, excluding the land points. 
# The minimum distance is then found and printed as the result. Again, you may need to adjust this code 
# depending on your specific data requirements.





