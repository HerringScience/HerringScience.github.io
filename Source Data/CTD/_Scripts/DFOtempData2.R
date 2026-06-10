

# Load DFO data and format to get ready to add to CTD_Raw
# turn into a function once complete and QCed

library(cli)
library(lubridate)
library(reprex)
library(tidyverse)
library(geosphere)
library(reshape2)
library(moderndive)
library(skimr)
library(ggridges)
#library(weathercan)
library(GGally)
library(psych)
library(raster)
library(PBSmapping)
#library(rgeos)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(oce)
library(gsw)
library(dplyr)


# this is CTD_Raw

CTD = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/CTD_Raw.csv"))

#load DFO temp data from historic period, prior to 1999

DFO = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/herringLarvalSurveyClimateData.csv"))


DFO$latitude = as.numeric(DFO$latitude)
DFO$longitude = as.numeric(DFO$longitude)
DFO$Lat = DFO$latitude
DFO$Lat = as.numeric(DFO$Lat)
DFO$Lon = DFO$longitude
DFO$Lon = as.numeric(DFO$Lon)
DFO$latitude = NULL
DFO$longitude = NULL
DFO$time = as.POSIXct(DFO$time)
DFO$Date <- paste(DFO$year, DFO$month, DFO$day, sep = "-")
DFO$year = NULL
DFO$month = NULL
DFO$day = NULL
DFO$Date = as.Date(DFO$Date)
DFO$datatype = as.factor(DFO$datatype)
DFO$cruise_id = as.factor(DFO$cruise_id)
DFO$Depth = swDepth(DFO$pressure, DFO$Lat)
DFO$Temperature = DFO$temperature
DFO$temperature = NULL
DFO$Salinity = DFO$salinity
DFO$salinity = NULL
DFO$Pressure = DFO$pressure
DFO$pressure = NULL

electrical_conductivity <- function(T, S, depth, lat) {
  # depth: meters
  # latitude: degrees (needed for pressure conversion)
  # Step 1: depth → pressure (dbar)
  P <- swPressure(depth, lat)
  # Step 2: conductivity ratio (dimensionless)
  R <- swCSTp(S, T, P)
  # Step 3: convert ratio → electrical conductivity (mS/cm)
  # Standard seawater conductivity = 42.914 mS/cm
  C_mScm <- R * 42.914
  # Return conductivity in µS/cm (most CTDs use this)
  C_uScm <- C_mScm * 1000
  return(C_uScm)
}


DFO$Conductivity <- electrical_conductivity(DFO$Temperature, DFO$Salinity, DFO$Depth, DFO$Lat)

DFO$id = paste(DFO$cruise_id, DFO$stn_id, sep = "-")
DFO$Year = year(DFO$Date)
DFO$Sound_velocity = swSoundSpeed(salinity=DFO$Salinity, temperature = DFO$Temperature, pressure = DFO$Pressure)
DFO$Density= gsw_rho(DFO$Salinity, DFO$Temperature, DFO$Pressure)
DFO$Specific_conductance = gsw_C_from_SP(DFO$Salinity, DFO$Temperature, DFO$Pressure)*1000
# Returns conductivity [mS/cm]

# Now need to add ground to the data
# Options are Scots Bay, German Bank or Other

# load two polygons
#Scots Bay

# Scots Bay CTD Box
df <- tribble(
  ~id, ~Box, ~Y, ~X,
  53, "Scots Bay", 44.7500, -65.61,
  54, "Scots Bay", 45.000, -64.67,
  55, "Scots Bay", 45.500, -64.67,
  56, "Scots Bay", 45.5, -65.61
)

sf_points <- df %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326)

# Ensure points are in the correct order
poly_coords <- sf_points %>%
  arrange(id) %>%              # or whatever ordering defines the boundary
  st_coordinates() %>%
  as.matrix()

# Close the polygon by repeating the first coordinate
poly_coords <- rbind(poly_coords, poly_coords[1, ])

# Build polygon
SB_poly <- st_polygon(list(poly_coords)) |> 
  st_sfc(crs = 4326) |> 
  st_sf(Box = unique(sf_points$Box), geometry = _)

#SB_poly


# German Bank CTD Box
df <- tribble(
  ~id, ~Box, ~Y, ~X,
  34, "German Bank", 43.7, -66.229,
  35 , "German Bank",43.56667, -66.229,
  36 , "German Bank",43.56667, -66.075,
  37, "German Bank", 43.233, -66.075,
  38, "German Bank", 43.233, -66.55,
  39, "German Bank", 43.7, -66.55,
  40, "German Bank", 43.7, -66.229   # closes the polygon
)

# Convert to polygon
poly_coords <- df %>%
  arrange(id) %>%
  select(X, Y) %>%
  as.matrix()

GermanBank_sf <- st_polygon(list(poly_coords)) |>
  st_sfc(crs = 4326) |>
  st_sf(Box = "German Bank", geometry = _)


#GermanBank_sf



### split up data into grounds. Function


label_scotsbay_germanbank <- function(df,
                                      lon = "lon", lat = "lat",
                                      ScotsBay, GermanBank,
                                      out_col = "region",
                                      crs_points = 4326,
                                      predicate = c("intersects", "within")) {
  predicate <- match.arg(predicate)
  
  # Points to sf
  pts <- st_as_sf(df, coords = c(lon, lat), crs = crs_points, remove = FALSE)
  
  # Ensure CRS matches
  if (st_crs(ScotsBay) != st_crs(pts)) ScotsBay <- st_transform(ScotsBay, st_crs(pts))
  if (st_crs(GermanBank) != st_crs(pts)) GermanBank <- st_transform(GermanBank, st_crs(pts))
  
  # Choose predicate
  pred_fun <- if (predicate == "within") st_within else st_intersects
  
  in_scots <- lengths(pred_fun(pts, ScotsBay)) > 0
  in_german <- lengths(pred_fun(pts, GermanBank)) > 0
  
  # Priority rule if overlap occurs: ScotsBay wins (you can swap if desired)
  df[[out_col]] <- ifelse(in_scots, "ScotsBay",
                          ifelse(in_german, "GermanBank", "Other"))
  df
}


DFO <- label_scotsbay_germanbank(
  DFO,
  lon = "Lon",
  lat = "Lat",
  ScotsBay = SB_poly,
  GermanBank = GermanBank_sf,
  out_col = "polygon"
)

DFO$ground = DFO$polygon 
DFO$polygon = NULL
DFO$ground = as.factor(DFO$ground)

levels(DFO$ground)[levels(DFO$ground) == "ScotsBay"] <- "Scots Bay"
levels(DFO$ground)[levels(DFO$ground) == "GermanBank"] <- "German Bank"

DFO$plankton_ID = NA
DFO$Survey = DFO$cruise_id
DFO$cruise_id = NULL


# Now some mods for CTD data
CTD$cruise_time = NA
CTD$datatype = "CD"

# Can add this in at some point
x =  aggregate(Depth ~ id, data = CTD, max, na.rm = TRUE)
colnames(x) = c("id", "maximum_depth")
CTD = merge(CTD, x, by = "id")

DFO$flag = NULL
CTD$stn_id = NA
CTD$time = NA
CTD$Source = "HSC"
DFO$Source = "DFO"

# finally, rbind


Ocean1 = rbind(CTD, DFO)



Oceans <- Oceans %>%
  mutate(JulianDay = lubridate::yday(as.Date(Date)))


write.table(Oceans, file= "Oceans.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 


# what is the max depth distributions

hist(Oceans$maximum_depth)




#QC

unique(Oceans$Year)
colnames(Oceans)

can <- gadm(country = "CAN",
            level   = 1,
            path    = "geodata_default_path",
            version = "latest",
            resolution = 1)

NBNS <- can[can$NAME_1 %in% c("New Brunswick", "Nova Scotia"), ]

NBNS_sf <- st_as_sf(NBNS)
####
CP <- ext(-66.5, -62, 44.5, 45.5)
CP <- as.polygons(CP)
crs(CP) <- crs(NBNS)
CP_sf <- sf::st_as_sf(CP)

ggplot(NBNS_sf) +
  geom_sf(data = NBNS_sf, fill = "lightgray", color = "black") +
  geom_sf(data = SB_poly, fill = "lightblue", color = "black")+ geom_sf(data = GermanBank_sf, fill = "purple", color = "black")+
  geom_point(data = Oceans, aes(x=Lon, y=Lat), size=2, colour="red")+
  coord_sf(xlim = c(-67, -64.5), ylim = c(43, 45.5)) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank()) + ggtitle("CTD Data Locations ")




# Divide into grounds
Oceans_SB=Oceans[which(Oceans$ground == "Scots Bay"), ]
Oceans_GB=Oceans[which(Oceans$ground == "German Bank"), ]





ggplot(NBNS_sf) +
  geom_sf(data = NBNS_sf, fill = "lightgray", color = "black") +
  geom_sf(data = SB_poly, fill = "lightblue", color = "black")+ geom_sf(data = GermanBank_sf, fill = "lightgreen", color = "black")+
  geom_point(data = Oceans_SB, aes(x=Lon, y=Lat, colour = Source), size=2)+ geom_point(data = Oceans_GB, aes(x=Lon, y=Lat, colour =  Source), size=2)+
  coord_sf(xlim = c(-67, -64.5), ylim = c(43, 45.5)) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank()) + ggtitle("CTD Data Locations ")




hist(Oceans_SB$Temperature)
hist(Oceans_GB$Temperature)

hist(Oceans_SB$Depth)
hist(Oceans_GB$Depth)


ggplot(data = Oceans, aes(x = JulianDay, y = Temperature, colour = Source)) + geom_point()


head(Oceans_SB)

ggplot(data = Oceans_SB, aes(x = JulianDay, y = maximum_depth, colour = Source)) + geom_point() + ggtitle("Temporal Distribution of Samples in Scots Bay")

# all the overlapping data in Scots Bay from DFO is SST, like 5 casts that will have >30m

subset_SB <- Oceans_SB %>%
  dplyr::filter(JulianDay >= 150, JulianDay <= 250)

ggplot(data = subset_SB, aes(x = JulianDay, y = maximum_depth, colour = Source)) + geom_point() + ggtitle("Temporal Distribution of Samples in Scots Bay")

subset_source <- subset_SB %>%
  dplyr::filter(Source == "DFO")

#ggplot(data = subset_source, aes(x = JulianDay, y = Depth, colour = Source)) + geom_point() + ggtitle("Temporal Distribution of Samples in Scots Bay")

#write.table(subset_source, file= "subset_source.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

# datatype is BO and MB
# BO is Bottle
# MB is MBT (apparently not trustworthy)
# years were 1977-1981

dim(Oceans_SB)
length(unique(Oceans_SB$id))

# 865 casts

ggplot(data = Oceans_GB, aes(x = JulianDay, y = Temperature, colour = Source)) + geom_point() + ggtitle("Temporal Distribution of Samples in German Bank")


dim(Oceans_GB)
length(unique(Oceans_GB$id))

#548 casts

length(unique(Oceans$id))
#total number of casts is 8,148
# Just Scots and German is: 1,413



# Create variables
se <- function(x, na.rm = TRUE) {
  x <- if (na.rm) x[!is.na(x)] else x
  stats::sd(x) / sqrt(length(x))
}

surface_depth = 1

SST <- Oceans %>%
  dplyr::filter(Depth <= surface_depth)


surface_summary <- SST %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    depth1      = max(Depth, na.rm = TRUE),
    avgTemp1    = mean(Temperature, na.rm = TRUE),
    avgSal1     = mean(Salinity, na.rm = TRUE),
    seTemp1     = se(Temperature),
    seSal1      = se(Salinity),
    density1    = mean(Density, na.rm = TRUE),
    seDensity1  = se(Density),
    
    ground      = dplyr::first(ground),
    Source      = dplyr::first(Source),
    Date        = dplyr::first(Date),
    Year        = dplyr::first(Year),
    JulianDay   = dplyr::first(JulianDay),
    
    .groups = "drop"
  )

write.table(surface_summary, file= "SST.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)


surface_summary_SB=surface_summary[which(surface_summary$ground == "Scots Bay"), ]







surface_summary_GB=surface_summary[which(surface_summary$ground == "German Bank"), ]

subsetSurfGB <- surface_summary_GB %>%
  dplyr::filter(JulianDay >= 200, JulianDay <= 350)

ggplot(data = subsetSurfGB, aes(x = JulianDay, y = avgTemp1, colour = Source)) + geom_point() + ggtitle("Temporal Distribution of Samples in German Bank")

# two chunks of overlap. In beginning of survey season and at the end. Try beginning first:

subsetSurfGB <- surface_summary_GB %>%
  dplyr::filter(JulianDay >= 200, JulianDay <= 250)

ggplot(data = subsetSurfGB, aes(x = JulianDay, y = avgTemp1, colour = Source)) + geom_point() + ggtitle("Temporal Distribution of Samples in German Bank")

shapiro.test(subset(subsetSurfGB, Source == "DFO")$avgTemp1)
shapiro.test(subset(subsetSurfGB, Source == "HSC")$avgTemp1)

wilcox.test( avgTemp1 ~ Source,data = subsetSurfGB)
# they are significantly different.
#p-value < 0.05 → significant difference between DFO and HSC
#p-value ≥ 0.05 → no statistically detectable difference
#Confidence interval tells you direction & magnitude



###now the later chuck of SST GB - Hard to really compare as so few HSC data points compared to DFO. I think only 4
subsetSurfGB <- surface_summary_GB %>%
  dplyr::filter(JulianDay >= 290, JulianDay <= 350)

ggplot(data = subsetSurfGB, aes(x = JulianDay, y = avgTemp1, colour = Source)) + geom_jitter(size = 3, alpha = 0.25) + ggtitle("Temporal Distribution of Samples in German Bank")

shapiro.test(subset(subsetSurfGB, Source == "DFO")$avgTemp1)
shapiro.test(subset(subsetSurfGB, Source == "HSC")$avgTemp1)

length(unique(subsetSurfGB$id))

wilcox.test( avgTemp1 ~ Source,data = subsetSurfGB)

write.table(subsetSurfGB, file= "GB_SST_laterChunk.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)




########################################

ggplot(data = surface_summary_SB, aes(x = JulianDay, y = avgTemp1, colour = Source)) + geom_point() + ggtitle("Temporal Distribution of Samples in Scots Bay")


subsetSurfSB <- surface_summary_SB %>%
  dplyr::filter(JulianDay >= 209, JulianDay <= 240)

ggplot(data = subsetSurfSB, aes(x = JulianDay, y = avgTemp1, colour = Source)) + geom_point() + ggtitle("Temporal Distribution of Samples in Scots Bay")


# are there differences? don't look like it:
#p-valueInterpretationp ≥ 0.05✅ Fail to reject H₀ → data are consistent with normalityp 
#< 0.05❌ Reject H₀ → data are not normally distributed
shapiro.test(subset(subsetSurfSB, Source == "DFO")$avgTemp1)
shapiro.test(subset(subsetSurfSB, Source == "HSC")$avgTemp1)

t.test( avgTemp1 ~ Source,data = subsetSurfSB)
#p-value < 0.05 → significant difference between DFO and HSC
#p-value ≥ 0.05 → no statistically detectable difference
#Confidence interval tells you direction & magnitude

# NOT different.

hist(CTDSB$StratTemp)
mean(CTDSB$StratTemp)
hist(CTDGB$StratTemp)
mean(CTDGB$StratTemp)




ggplot(data = surface_summary_GB, aes(x = JulianDay, y = avgTemp1, colour = Source)) + geom_point() + ggtitle("Temporal Distribution of Samples in German Bank")



# at depth. There is something wrong here I think. Need to go through the function and QC


strat <- function(depthLowerLimit, depthUpperLimit, data) {
  
  se <- function(x, na.rm = TRUE) {
    sd(x, na.rm = na.rm) / sqrt(sum(!is.na(x)))
  }
  
  data %>%
    dplyr::filter(
      Depth > depthLowerLimit,
      Depth < depthUpperLimit
    ) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      depth_max     = max(Depth, na.rm = TRUE),
      avgTemp       = mean(Temperature, na.rm = TRUE),
      avgSal        = mean(Salinity, na.rm = TRUE),
      seTemp        = se(Temperature),
      seSal         = se(Salinity),
      density       = mean(Density, na.rm = TRUE),
      seDensity     = se(Density),
      .groups = "drop"
    )
}


Ocean_30m <- strat(
  depthLowerLimit = 29,
  depthUpperLimit = 31,
  data = Oceans
)


dim(Ocean_30m)
length(unique(Ocean_30m$id))

stratified = merge(surface_summary, Ocean_30m, by  = "id")
dim(stratified)
length(unique(stratified$id))
colnames(stratified)
stratified$densDiff = (stratified$density) - (stratified$density1) 
stratified$tempDiff = (stratified$avgTemp) - (stratified$avgTemp1) 
stratified$salDiff = (stratified$avgSal) - (stratified$avgSal1) 

head(stratified)
dim(stratified)

stratSB=stratified[which(stratified$ground == "Scots Bay"), ]
stratGB=stratified[which(stratified$ground == "German Bank"), ]


# don't have anything for Scots - only can do the SST comparison
ggplot(data = stratSB, aes(x = JulianDay, y = avgTemp, colour = Source)) + geom_point() + ggtitle("Temporal Distribution of Samples in Scots Bay")


# limited ability here either. A little overlap, maybe 3 DFO and 12 HSC. 
ggplot(data = stratGB, aes(x = JulianDay, y = avgTemp, colour = Source)) + geom_point() + ggtitle("Temporal Distribution of Samples in German Bank")


# this plot is still striking in terms of the difference. I think it can be used to demonstrate the changes in stratification.
ggplot(data = stratGB, aes(x = JulianDay, y = tempDiff, colour = Source)) + geom_point() + ggtitle("Temporal Distribution of Samples in German Bank")
