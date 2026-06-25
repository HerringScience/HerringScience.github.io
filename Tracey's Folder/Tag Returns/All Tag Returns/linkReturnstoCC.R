
## Links Return Data to Commercial Catch to be able to determine return location and catch amount

setwd(paste0("C:/Users/herri/Herring Science Council"))

# 1. Setup ####

library(tidyverse)
library(lubridate)
library(sf)


  # load functions

    # recoding function
recode_if <- function(x, condition, ...) {
  if_else(condition, recode(x, ...), x)
}

recode_ifNA <- function(x, condition, column, replacement, ...) {
  ifelse(is.na(column) | condition, ifelse(column == x, replacement, column), column)
}



#### 2. Build rawReturn2023
  # 1.1 Load and format all plant spreadsheets 2020-2022 ####
      
    # 2022 plant spreadsheets (Comeau2022, Connors2022, ScotiaGarden2022)
setwd(paste0("C:/Users/herri/Herring Science Council/Tagging Project AFF 44 - Documents/Brittni Scott's offboarding documents/Files for R/workspace/ComeauReturns2022.csv"))  

    Comeau2022 <- read.csv("ComeauReturns2022.csv") %>%
      filter(!row_number() %in% c(94:106)) %>%
      rename("TAG_NUMBER" = 'HERRING.TAG.NO',
             "catchAREA" = 'LOCATION.OF.CATCH',
             "DATE" = 'CATCH.DATE',
             "BOAT" = 'VESSAL.NAME',
             "GearType" = 'GEAR.TYPE',
             'Catch.t' = 'CATCH.VOL...MT.') %>%
      mutate(GearType = 'Seiner',
             dataorigin = "ComeauReturns2022.csv",
             Company = 'Comeau - Sea Life') %>%
      mutate(BOAT = recode_if(BOAT, TAG_NUMBER %in% c(518791,500396,510467), 'LADY MELISSA/SEALIFE II - MIXED TANKER' = 'Sealife II')) %>% # when two boats given, boat with bigger catch chosen
      mutate(BOAT = recode_if(BOAT, TAG_NUMBER %in% c(498420, 502546), 'SEALIFE II/LADY JANICE II MIXED TANKER' = 'Lady Janice II')) %>%
      mutate(BOAT = recode_if(BOAT, TAG_NUMBER == 502044, 'LADY JANICE II/SEALIFE CHANGE OVER' = 'Lady Janice II')) %>%
      mutate(Catch.t = recode(Catch.t, '33/73' = '73',
                              '51/96' = '96',
                              '12/36' = '36',
                              '20/8' = '20')) %>% # when two boats given, boat with bigger catch chosen
      mutate(DATE = mdy(DATE),
             Catch.t = as.numeric(as.character(Catch.t))) %>%
      select(TAG_NUMBER, catchAREA, DATE, BOAT, GearType, Company, Catch.t, dataorigin)
    
    Connors2022 <- read.csv("ConnorsReturn2022.csv") %>%      # removes any NA dates and 'Damaged' tag numbers. Also remove 507494 as it has 2 observations with different dates (found in Section 1.5)
        rename("TAG_NUMBER" = 'X2022..FISH..TAG..INFORMATION',
              "catchAREA" = 'X.9',
              "DATE" = 'X.1',
              "BOAT" = 'X.6',
              "GearType" = 'X.7',
              "Catch.t" = 'X.8') %>%
        mutate(Company = "Connors",
               dataorigin = "ConnorsReturns2022.csv") %>%
        select(TAG_NUMBER, catchAREA, DATE, BOAT, GearType, Company, Catch.t, dataorigin) %>%
        filter(DATE != c('N/A')) %>%
        filter(!row_number() %in% c(1:16)) %>%
        mutate(DATE = mdy(DATE)) %>%
        filter(TAG_NUMBER != c('Damaged')) %>%
        filter(TAG_NUMBER != c('Damaged ')) %>%
        filter(TAG_NUMBER != 507494) %>%
        mutate(Catch.t = gsub("[^0-9.-]", "", Catch.t),
               Catch.t = as.numeric(as.character(Catch.t)))
    
    ScotiaGarden2022 <- read.csv("ScotiaGardenReturns2022.csv") %>%
        rename("TAG_NUMBER" = "Tag..",
             "catchAREA" = "Area.Fished",
             "DATE" = "Date.of.Capture",
             "BOAT" = "Name.of.Vessel",
             'Catch.t' = "Catch.Amount..MT.") %>%
        mutate(Company = "Scotia Garden", 
               GearType = "Seiner",
               dataorigin = "ScotiaGardenReturns2022.csv") %>%
        mutate(BOAT = recode_if(BOAT, TAG_NUMBER == 491315, 'Scotia Garden - Frozen' = 'Brunswick Provider')) %>%  # this tag originally had no boat associated to it but in reviewing the CC data, all catches with that return date were in German Bank/Seal Island and the Brunswick Provider had the largest catch
        mutate(BOAT = recode_if(BOAT, TAG_NUMBER == 491430, 'Scotia Garden - Frozen' = 'Brunswick Provider')) %>%  # this tag originally had no boat associated to it but in reviewing the CC data, all catches with that return date were in German Bank/Seal Island and the Brunswick Provider had the largest catch
        select(TAG_NUMBER, catchAREA, DATE, BOAT, GearType, Company, Catch.t, dataorigin) %>%
        mutate(DATE = mdy(DATE),
               Catch.t = as.numeric(Catch.t)) %>%
      filter(!BOAT %in% c('Comeau Seafood - Frozen', 'Scotia Garden - Frozen'))
    
    CapeBreeze2022 <- read.csv("CapeBreezeReturns2022.csv") %>%
      rename("Catch.t" = "catchsizeMT") %>%
      mutate(Company = "Cape Breeze",
             dataorigin = "CapeBreezeReturns2022.csv") %>%
      mutate(DATE = mdy(DATE),
             Catch.t = as.numeric(Catch.t)) %>%
      select(TAG_NUMBER, catchAREA, DATE, BOAT, GearType, Company, Catch.t, dataorigin)
    
    # 2021 plant spreadsheets (Comeau2021, Connors2021, ScotiaGarden2021)
      
    Comeau2021 <- read.csv("ComeauReturns2021.csv") %>%
      filter(!row_number() %in% c(47:68)) %>%
      rename("TAG_NUMBER" = "HERRING.TAG.NO",
             "catchAREA" = "LOCATION.OF.CATCH",
             "DATE" = "CATCH.DATE" ,
             "BOAT" = "VESSAL.NAME",
             "Catch.t" = "CATCH.VOLUME...MT.") %>%
      mutate(GearType = 'Seiner',
             dataorigin = "ComeauReturns2021.csv",
             Company = 'Comeau - Sea Life') %>%
      select(TAG_NUMBER, catchAREA, DATE, BOAT, GearType, Company, Catch.t, dataorigin) %>%
      mutate(DATE = mdy(DATE))
    
    Connors2021 <-read.csv("ConnorsReturn2021.csv") %>%
      rename("TAG_NUMBER" = "TAG.NO.",
             "catchAREA" = "CATCH.AREA",
             "GearType" = "GEAR.TYPE",
             "Catch.t" = "CATCH.VOLUME") %>%
      mutate(Company = "Connors",
             dataorigin = "ConnorsReturns2021.csv") %>%
      mutate(BOAT = recode_if(BOAT, TAG_NUMBER == 462846, 'Comeau Fillets ' = 'Brunswick Provider')) %>% # this tag originally had no boat associated to it but the Brunswick Provider is the only boat in CC with that return date
      mutate(BOAT = recode_if(BOAT, TAG_NUMBER == 579635, 'Comeau Fillets/Fresh' = 'Lady Melissa')) %>% # this tag originally had no boat associated to it but in reviewing the CC data, all catches with that return date were in Scots Bay and the Lady Melissa had the largest catch
      filter(!is.na(DATE.OF.LANDING) & DATE.OF.LANDING != "") %>%
      mutate(DATE = mdy(DATE.OF.LANDING)) %>%
      select(TAG_NUMBER, catchAREA, DATE, BOAT, GearType, Company, Catch.t, dataorigin) %>%
      mutate(Catch.t = gsub("[^0-9.-]", "", Catch.t),
             Catch.t = as.numeric(as.character(Catch.t)))
    
    ScotiaGarden2021 <- read.csv("ScotiaGardenReturns2021.csv") %>%
      rename("TAG_NUMBER" = "Tag..",
             "catchAREA" = "Area.Fished",
             "DATE" = "Date.of.Capture",
             "BOAT" = "Name.of.Vessel",
             "Catch.t" = "Catch.Amount..MT.") %>%
      mutate(Company = "Scotia Garden", 
             GearType = "Seiner",
             dataorigin = "ScotiaGardenReturns2021.csv") %>%
      select(TAG_NUMBER, catchAREA, DATE, BOAT, GearType, Company, Catch.t, dataorigin) %>%
      mutate(DATE = mdy(DATE),
             Catch.t = as.numeric(Catch.t))
    
    # Load 2020 plant spreadsheets (Comeau2020, Connors2020, ScotiaGarden2020)
      
    Comeau2020 <- read.csv("ComeauReturns2020.csv") %>%
      rename("TAG_NUMBER" = "HERRING.TAG.NO",
             "catchAREA" = "LOCATION.OF.CATCH",
             "DATE" = "CATCH.DATE",
             "BOAT" = "VESSAL.NAME",
             "Company" = "COMPANY.NAME",
             "Catch.t" = "CATCH.VOLUME...MT.") %>%
      mutate(GearType = "Seiner",
             dataorigin = "ComeauReturns2020.csv") %>%
      select(TAG_NUMBER, catchAREA, DATE, BOAT, GearType, Company, Catch.t, dataorigin) %>%
      mutate(DATE = mdy(DATE),
             Catch.t = as.numeric(Catch.t)) %>%
      filter(!is.na(DATE))
    
    Connors2020 <- read.csv("ConnorsReturn2020.csv") %>%
      rename("TAG_NUMBER" = "TAG.NO.",
             "catchAREA" = "CATCH.AREA",
             "Catch.t" = "CATCH.VOLUME",
             "GearType" = "GEAR.TYPE") %>%
      filter(!is.na(DATE.OF.LANDING) & DATE.OF.LANDING != "") %>%
      mutate(Company = "Connors",
             dataorigin = "ConnorsReturns2020.csv") %>%
      mutate(DATE = mdy(DATE.OF.LANDING)) %>%
      filter(TAG_NUMBER != c('Damaged')) %>%
      select(TAG_NUMBER, catchAREA, DATE, BOAT, GearType, Company, Catch.t, dataorigin) %>%
      mutate(Catch.t = gsub("[^0-9.-]", "", Catch.t),
            Catch.t = as.numeric(as.character(Catch.t)))
    
    ScotiaGarden2020 <- read.csv("ScotiaGardenReturns2020.csv") %>%
      rename("TAG_NUMBER" = "Tag..",
             "catchAREA" = "Area.Fished",
             "DATE" = "Date.of.Capture",
             "BOAT" = "Name.of.Vessel",
             "Catch.t" = "Catch.Amount..MT.") %>%
      mutate(Company = "Scotia Garden", 
             GearType = "Seiner",
             dataorigin = "ScotiaGardenReturns2020.csv") %>%
      select(TAG_NUMBER, catchAREA, DATE, BOAT, GearType, Company, Catch.t, dataorigin) %>%
      mutate(DATE = mdy(DATE)) %>%
      filter(!is.na(DATE)) %>%
      mutate(Catch.t = gsub("[^0-9.-]", "", Catch.t),
             Catch.t = as.numeric(as.character(Catch.t)))
    
    CapeBreeze2020 <- read.csv("CapeBreezeReturns2020.csv") %>% # gives a warning but loads properly
      rename("TAG_NUMBER" = "Tag.Number",
             "catchAREA" = "Ground",
             "DATE" = "Landed.Date",
             "BOAT" = "Vessel",
             "Catch.t" = "Catch.Amount") %>%
      mutate(dataorigin = "CapeBreezeReturns2020.csv",
             GearType = "Seiner",
             Company = "Cape Breeze") %>%
      select(TAG_NUMBER, catchAREA, DATE, BOAT, GearType, Company, Catch.t, dataorigin) %>%
      mutate(DATE = dmy(DATE),
             Catch.t = as.numeric(Catch.t))
 
    # Compile all plant sheets together and QC ####
        # make sure all columns are the same data type with str() so that they rbind together (lots of code added above to make the Catch.t columns all the same)
    
    plantbind <- rbind(Comeau2022, ScotiaGarden2022, Connors2022, CapeBreeze2022,
                          Comeau2021, Connors2021, ScotiaGarden2021, 
                          Comeau2020, Connors2020, ScotiaGarden2020, CapeBreeze2020) # 906
    
    # QC plantbind
    
          # check for duplicate tag numbers'
    
                # n_distinct(plantbind$TAG_NUMBER) # there are 7 duplicate tag numbers...

                temp <- plantbind %>%
                  mutate(refno = 1:906)
                
                 dups <- duplicated(temp$TAG_NUMBER)
                 tags <- temp$TAG_NUMBER
                 tempdf <- data.frame(tags, dups)
                 
                 true = temp[which(tempdf$dups == "TRUE"), ]
           
                  # Find the 7 tag numbers in the plantbind (506114, 585883, 500306, 516366, 507351, 495463, 495520). 

                   duptags <- true$TAG_NUMBER
                 
                   duplicates <- filter(temp, TAG_NUMBER %in% duptags) %>%
                     arrange(TAG_NUMBER) # looks like we can just delete one of the TRUEs as they are all the same except 507494 which has 2 different dates - add to incomplete.returns.
                    
                    print(duplicates$refno)
                  
                  # remove the following refnos: 428 429 425 423 427 426 424 (* = differing info - remove; TAG_NUMBER = 507494 from Connors2022)

                 plantbindnodups <- temp %>%
                   filter(!refno %in% as.vector(true$refno)) %>%
                   select(!refno) # 899
    
                 n_distinct(plantbindnodups$TAG_NUMBER) # check for dups to ensure it all worked
                 
          # Ensure proper BOAT names, &
              # Remove "N/A", "Pre Cuts", "Frozen", "Cape Breeze/Frozen" (notes = Silver Harvester?), "Comeau Frozen", "Comeau Fillets",
              # "Comeau Fillets/Fresh", and "Fillets" and put them in Notes section in x dfs
                 
                 plantbindnodups$BOAT <- str_to_title(plantbindnodups$BOAT)
        
                 plantreturns <- plantbindnodups %>%
                   mutate(BOAT = recode(BOAT, 'Sealife Ii' = 'Sealife II', 
                                        'Lady Janice Ii' = 'Lady Janice II', 
                                        'Lady Melissa/Sealife Ii - Mixed Tanker' = 'Lady Melissa/Sealife II',
                                        'Lady Janice Ii/Sealife Change Over' = 'Lady Janice II/Sealife II',
                                        'Leroy & Barry Ii ' = 'Leroy & Barry II',
                                        'Leroy & Barry Ii' = 'Leroy & Barry II',
                                        'Lady Janice/Sealife Ii' = 'Lady Janice II/Sealife II',
                                        'Fundy Monarch ' = 'Fundy Monarch',
                                        'Comeau Fillets ' = 'Comeau Fillets',
                                        'Rk Murphy ' = 'RK Murphy',
                                        'Fillets ' = 'Fillets',
                                        'Sealife Ii/Lady Janice Ii Mixed Tanker' = 'Lady Janice II/Sealife II',
                                        'Sealife Ii/Lady Janice' = 'Lady Janice II/Sealife II',
                                        'Leroy And Barry' = 'Leroy & Barry II',
                                        'Lady Melissa/Lady Janice' = 'Lady Janice II/Lady Melissa',
                                        'Andrew And Dean' = 'Andrew & Deane',
                                        'Capeico' = 'Capelco',
                                        'Leroy & Barry' = 'Leroy & Barry II',
                                        'Leroy & Barry ' = 'Leroy & Barry II',
                                        'Lady Janice' = 'Lady Janice II',
                                        'Provider' = 'Brunswick Provider',
                                        'Tasha Marie ' = 'Tasha Marie',
                                        'Lady Janice Ii ' = 'Lady Janice II',
                                        'Rk Murphy' = 'RK Murphy',
                                        'Lady Janice/Lady Melissa' = 'Lady Janice II/Lady Melissa',
                                        'Morning Star ' = 'Morning Star')) %>%
                   filter(!BOAT %in% c("N/A", "Pre Cuts", "Frozen", 'RK Murphy', "Cape Breeze/Frozen", "Comeau Frozen", "Comeau Fillets", "Comeau Fillets/Fresh", "Fillets"))
                 
                 tempplant <- data.frame(unique(plantreturns$BOAT))
                 

  # 1.2 Load and format DFO mailed in returns #### 
    
    DFOmailed <- read.csv("DFOmailedin2023.csv") %>%
                   filter(!TAG_NUMBER %in% c(500270, 499078, 495992, 600479, 600440, 600457,
                                             580175, 522854, 522563, 487227, 458234, 457506,
                                             455355, 455193, 455133, 455086, 450689, 449786,
                                             448906, 448505, 448029, 448003, 444728, 423786,
                                             603503, 511150, 511097, 491388, 481022, 457452,
                                             442788, 442133, 42937, 42804, 44803)) # removed as they are also in other datasets (these were crosschecked with CC data before removing)
                 
                 
    # format to have consistent columns as other return dfs it will be combined with (TAG_NUMBER, catchAREA, DATE, GearType, Company)
           # Remove ones with no boat names & dates, etc
    
           # are there any tag numbers that were used that are not 6 characters long? check in rel df (from relINFO.R)
           # temp = rel  
           # temp$noChar <- nchar(rel$TAG_NUMBER)
           # unique(temp$nochar) # some have 5 characters? is it just an accidental one?
           # temp <- temp %>% # select tags with 5 characters
           # filter(noChar == 5) # 4800 have 5 characters so not a mistake. tags can be 5 or 6 characters long.
           
           DFOtemp <- DFOmailed %>%
             rename("catchAREA" = "RETURN_LOCATION",
                    "DATE" = "RETURN_DATE",
                    "BOAT" = "RETURN_VESSEL_NAME",
                    "GearType" = "RETURN_GEAR_TYPE",
                    "Company" = "RETURN_FISH_PLANT",
                    "Catch.t" = 'Catch.amount..mt.') %>%
             mutate(dataorigin = 'DFOmailedin2023.csv') %>%
             select(TAG_NUMBER, catchAREA, DATE, BOAT, GearType, Company, Catch.t, dataorigin) %>%
             mutate(nochar = nchar(TAG_NUMBER),
                    Catch.t = as.numeric(Catch.t)) %>%
             filter(nochar > 4) %>% # TAG_NUMBERS should be either 5 or 6 long, this removes short and NA tags
             select(!nochar) %>%
             filter(!BOAT == "") # remove observations without a BOAT name

    # QC DFO mailed in returns ####
        
         # check DFOmailedin2023.csv with other datasheets on sharepoint
              
                # load and format other sheets
              #  dfotest1 <- read.csv("2022-05 Herring_tagging_TS.csv") %>%
              #    mutate(dataorigin = '2022-05 Herring_tagging_TS.csv') %>%
              #    rename("TAG_NUMBER" = 1,
              #           "catchAREA" = 8,
              #           "DATE" = 3,
              #           "BOAT" = 4,
              #           "GearType" = 5,
              #           "Company" = 6) %>%
              #    mutate(across(TAG_NUMBER, factor)) %>%
              #    select(TAG_NUMBER, catchAREA, DATE, BOAT, GearType, Company, dataorigin)
                
              #  dfotest2 <- read.csv("tag return info 2020 DFO.csv") %>% # gives warning but loads fine
               #   mutate(dataorigin = 'tag return info 2020 DFO.csv') %>%
                #  rename("TAG_NUMBER" = 1,
                 #        "catchAREA" = 9,
                  #       "DATE" = 2,
                   #      "BOAT" = 3,
                    #     "GearType" = 6) %>%
                 # mutate(Company = 'NA') %>%
                 # mutate(across(c(TAG_NUMBER, DATE, Company, dataorigin), factor)) %>%
                 # select(TAG_NUMBER, catchAREA, DATE, BOAT, GearType, Company, dataorigin)
                
                # check for duplicates within these dfs
                
               # dfotest <- rbind(dfotest1, dfotest2)
                
               # dups <- duplicated(dfotest$TAG_NUMBER)
               # dups <- as.data.frame(dups)
               # tags <- dfotest$TAG_NUMBER
               # temp <- data.frame(tags, dups)
               # true = temp[which(temp$dups == "TRUE"), ] #tag 42469 duplicated in dfotest
                
               # j <- dfotest %>%
                  # filter(TAG_NUMBER %in% c("42469")) # take a look at that tag number - duplicated as they did not know which boat it was on - add it to xDFOmailreturns
                
                # use dfotest to see if there are observations in dfotest that are not in DFOtemp (i.e. DFOmailedin2023.csv)
                
               # i <- setdiff(dfotest$TAG_NUMBER, DFO$TAG_NUMBER) # finding tag numbers in dfotest that are not in DFO (i.e. DFOmailedin2023.csv)
                       # only 42469 - added these observations to xDFOmailedinreturns.
        
           # Remove any non-boats in $BOAT
           
           unique(DFOtemp$BOAT)
           
           nonboat = c("Pre Cuts","Fillets ","Frozen Fillets Comeau","Frozen Comeau Fillets","Frozen Cape Breeze",
                       "Snackline","Frozen","Cape Breeze/Frozen","Comeau Frozen","Comeau Fillets","Comeau Fillets/Fresh",
                       "Fillets", "Snack Fillets", "Sea Crest (Pre Cuts)", "Comeau Fillets ", "Comeau", "Cape Breeze",
                       "Comeau's Frozen", "Sea Crest ", "Seacrest", "Sea Crest Frozen", "Comeaus", "Cape Breeze Seafood",
                       "2nd GEN", "Comeau's NS Boats", "Cape Breeze NS Boats", 'RK Murphy')  
           
           DFOreturns <- DFOtemp %>%
             mutate(BOAT = recode(BOAT, 'Sealife Ii' = 'Sealife II',
                                'Morning Star ' = 'Morning Star',
                                'Lady Janice Ii' = 'Lady Janice II', 
                                'Lady Melissa/Sealife Ii - Mixed Tanker' = 'Lady Melissa/Sealife II',
                                'Lady Janice Ii/Sealife Change Over' = 'Lady Janice II/Sealife II',
                                'Leroy & Barry Ii ' = 'Leroy & Barry II',
                                'Leroy & Barry Ii' = 'Leroy & Barry II',
                                'Lady Janice/Sealife Ii' = 'Lady Janice II/Sealife II',
                                'Fundy Monarch ' = 'Fundy Monarch',
                                'Comeau Fillets ' = 'Comeau Fillets',
                                'Rk Murphy ' = 'Rk Murphy',
                                'Fillets ' = 'Fillets',
                                'Sealife Ii/Lady Janice Ii Mixed Tanker' = 'Lady Janice II/Sealife II',
                                'Sealife Ii/Lady Janice' = 'Lady Janice II/Sealife II',
                                'Leroy And Barry' = 'Leroy & Barry II',
                                'Lady Melissa/Lady Janice' = 'Lady Janice II/Lady Melissa',
                                'Andrew And Dean' = 'Andrew & Deane',
                                'Capeico' = 'Capelco',
                                'CapeIco' = 'Capelco',
                                'Leroy & Barry' = 'Leroy & Barry II',
                                'Leroy & Barry ' = 'Leroy & Barry II',
                                'Lady Janice' = 'Lady Janice II',
                                'Provider' = 'Brunswick Provider',
                                'RK Murphy ' = 'RK Murphy',
                                'Leroy Barry' = 'Leroy & Barry II',
                                'Tasha Marie ' = 'Tasha Marie',
                                'Lady Janice/Lady Melissa' = 'Lady Janice II/Lady Melissa',
                                'Sealife II/Lady Janice' = 'Lady Janice II/Sealife II')) %>%
             filter(!BOAT %in% nonboat)
           
           tempDFO <- data.frame(unique(DFOreturns$BOAT))
           
           
           
           
  # 1.3 Load and QC rawReturns ####   
           rawReturntemp <- read.csv('rawReturn.csv') %>%
             mutate(Catch.t = NA) %>%
             select(TAG_NUMBER, catchAREA, DATE, BOAT, GearType, Catch.t, Company)
           
           # colnames(plantreturns)
           # colnames(rawReturn)
           # colnames(DFOmailedreturns)
           
           # QC boat names in rawReturns & remove $BOAT 'Undetermined' & 'Comeaus Seiner'
           
           rawReturn <- rawReturntemp %>%
             mutate(BOAT = recode(BOAT, 'Sealife Ii' = 'Sealife II', 
                                  'Lady Janice Ii' = 'Lady Janice II', 
                                  'Morning Star ' = 'Morning Star',
                                  'Lady Melissa/Sealife Ii - Mixed Tanker' = 'Lady Melissa/Sealife II - Mixed Tanker',
                                  'Lady Janice Ii/Sealife Change Over' = 'Lady Janice II/Sealife II Change Over',
                                  'Leroy & Barry Ii ' = 'Leroy & Barry II',
                                  'Leroy & Barry Ii' = 'Leroy & Barry II',
                                  'Lady Janice/Sealife Ii' = 'Lady Janice II/Sealife II',
                                  'Fundy Monarch ' = 'Fundy Monarch',
                                  'Comeau Fillets ' = 'Comeau Fillets',
                                  'Rk Murphy ' = 'Rk Murphy',
                                  'Fillets ' = 'Fillets',
                                  'Sealife Ii/Lady Janice Ii Mixed Tanker' = 'Sealife II/Lady Janice II',
                                  'Sealife Ii/Lady Janice' = 'Sealife II/Lady Janice II',
                                  'Leroy And Barry' = 'Leroy & Barry II',
                                  'Lady Melissa/Lady Janice' = 'Lady Janice II/Lady Melissa',
                                  'Andrew And Dean' = 'Andrew & Deane',
                                  'Capeico' = 'Capelco',
                                  'CapeIco' = 'Capelco',
                                  'Leroy & Barry' = 'Leroy & Barry II',
                                  'Leroy & Barry ' = 'Leroy & Barry II',
                                  'Lady Janice' = 'Lady Janice II',
                                  'Provider' = 'Brunswick Provider',
                                  'RK Murphy ' = 'RK Murphy',
                                  'Leroy Barry' = 'Leroy & Barry II',
                                  'Lady Janice/Lady Melissa' = 'Lady Janice II/Lady Melissa',
                                  'Sealife II/Lady Janice' = 'Sealife II/Lady Janice II',
                                  'Andrew & Deane' = 'Andrew and Dean',
                                  'Leroy and Barry' = 'Leroy & Barry II',
                                  'Tasha Marie ' = 'Tasha Marie')) %>%
             filter(!BOAT %in% c('Undetermined', 'Comeaus Seiner')) %>%
             mutate(dataorigin = "rawReturn.csv")
           
           tempraw <- data.frame(unique(rawReturn$BOAT))       
      
           
  # 1.4 Load HSC mailed in returns ####
           
           HSCmailed <- read.csv("Mail in Tag Returns.csv") %>%
             rename("TAG_NUMBER" = 'Tag.Number',
                  "catchAREA" = 'Ground',
                  "DATE" = 'DayCode',
                  "BOAT" = 'Vessel',
                  "GearType" = 'Gear.Type',
                  'Catch.t' = 'Catch.Amount',
                  'Company' = 'Return.Company') %>%
             mutate(dataorigin = 'Mail in Tag Returns.csv') %>%
             select(-c(Plant.Found, Landed.Date, Address, Name, Lat, Lon, Name.of.document, Link.to.document, Notes, Date.Found)) %>%
             filter(!is.na(TAG_NUMBER)) %>%
             filter(!BOAT == '') %>%
             mutate(DATE = ymd(DATE))
           
  # 1.5 Combine and QC all complete return data (build complete.returns) ####
           # dfs to combine = rawReturn (2019 and previous observartions), plantreturns, DFOmailedreturns, HSCmailed
           
           complete.returnsbind <- rbind(plantreturns, rawReturn, DFOreturns, HSCmailed) # 1449
           
           # colnames(DFOreturns)
           # colnames(plantreturns)
           # colnames(rawReturn)
           # colnames(HSCmailed)
           
           # make sure there's no duplicates as there may be the same DFO returns in other spreadsheets from DFOmailedreturns
           
           n_distinct(complete.returnsbind$TAG_NUMBER) # 1203
           
           
           dups <- duplicated(complete.returnsbind$TAG_NUMBER)
           tags <- complete.returnsbind$TAG_NUMBER
           temp <- data.frame(tags, dups)
           true = temp[which(temp$dups == "TRUE"), ] 
           
           # make vector of all tags in true (all duplicated tags)
           
           duptags <- as.vector(true$tags) # 246 tags with duplicates
           
           alldups <- complete.returnsbind %>% # 492
             filter(TAG_NUMBER %in% duptags) %>%
             arrange(TAG_NUMBER)
           
           # find observations in the duplicates that have the same TAG_NUMBER but different DATE & same TAG_NUMBER but different BOAT (these should be moved to incomplete.returns)
           # Duplicate observations with same TAG_NUMBER, BOAT, and DATE should have one removed
           
           # In alldups, find observations with the same TAG_NUMBER but different DATE
           
           uniquedatecombo <- alldups %>% 
             count(TAG_NUMBER, DATE) %>%  # counts number of unique TAG_NUMBER & DATE combinations in temp. if n=1, than the matching tag number has a different date, so we are interested in n=1
             filter(n == 1) # 6; remove these from complete.returns and add note 'duplicated TAG_NUMBER with different DATE)
           
           duptagdate <- as.vector(uniquedatecombo$TAG_NUMBER) # use this to remove duplicates with differing dates; 6
           
           # In temp, find observations with the same TAG_NUMBER but different BOAT
           
           uniqueboatcombo <- alldups %>% # once the databases were cleaned up to have consistent boat names, all match by boat so only need to worry about observations with differing dates
             count(TAG_NUMBER, BOAT) %>%
             filter(n == 1) # 0
           
           # remove all duptagdate from complete.returnsbind (duplicates with same tag # but different dates as those should all go into incomplete)
           
           complete.returnsbind1 <- complete.returnsbind %>%
             filter(!TAG_NUMBER %in% duptagdate) # 1443
           
           
           # now remove duplicates that are remaining - remove all tags that are duplicated and in a separate df half them and rbind them back together with complete.returnsbindnodups = complete.returns
           
           # first ID the remaining duplicates
           dups <- duplicated(complete.returnsbind1$TAG_NUMBER)
           tags <- complete.returnsbind1$TAG_NUMBER
           temp <- data.frame(tags, dups)
           true = temp[which(temp$dups == "TRUE"), ] # 243
           
           remainingduptags <- as.vector(true$tags) # 243 tags with duplicates remaining
           
           
           # remove remainingduptags that have same info from complete.returnsbind1 (DFOmailedin2023.csv created a lot of duplicates)
           
           complete.returnsbindnodups <- complete.returnsbind1 %>%
             filter(!TAG_NUMBER %in% remainingduptags) # gives 957
           
           
           # make df of duptags and take out every second tag
           odddups <- complete.returnsbind1  %>% 
             filter(TAG_NUMBER %in% remainingduptags) %>%  # should be 243*2=486
             arrange(TAG_NUMBER) %>%
             mutate(refno = 1:486) %>% # used to make sure the filter below is doing what I want it to - removing every second row i.e. all evens
             filter(row_number() %% 2 == 1) %>% # now 243 rows
             select(!refno)
           
           
           # rbind complete.returnsbindnodups and odddups = complete.returns
           
           complete.returns <- rbind(odddups, complete.returnsbindnodups) %>%
             mutate(BOAT = recode(BOAT, 'Andrew and Dean' = 'Andrew & Deane')) %>% # give consistent boat name with CC data
             mutate(id = paste(DATE, BOAT)) %>%
             filter(!TAG_NUMBER == 449374) %>% # this appears to be a duplicate of tag 449347 so removed after discussions with Jenna
             mutate(GearType = recode(GearType, 'Seiner' = 'Purse Seine',
                                      'Seiner ' = 'Purse Seine',
                                      'Seine' = 'Purse Seine',
                                      'Purse seine' = 'Purse Seine')) %>%
             mutate(id = recode(id, '2002-09-08 Lady Janice II/Sealife II' = '2022-09-08 Lady Janice II/Sealife II', # making it match CC data as per discussions with Jenna
                                '2018-08-18 Capelco' = '2018-08-17 Capelco',
                                '2022-08-22 Lady Janice II' = '2022-08-23 Lady Janice II',
                                '2022-08-30 Lady Melissa' = '2022-08-31 Lady Melissa',
                                '2021-10-06 Canada 100' = '2021-10-05 Canada 100',
                                '2022-07-07 Brunswick Provider' = '2022-07-06 Brunswick Provider',
                                '2022-08-25 Canada 100' = '2022-08-23 Canada 100',
                                '2022-08-22 Canada 100' = '2022-08-23 Canada 100',
                                '2022-09-02 Canada 100' = '2022-09-06 Canada 100',
                                '2022-09-14 Fundy Monarch' = '2022-09-13 Fundy Monarch',
                                '2022-09-18 Canada 100' = '2022-09-19 Canada 100')) %>%
             mutate(catchAREA = recode_if(catchAREA, TAG_NUMBER == 522931, 'GERMAN BANK (AREA 7)' = 'Scots Bay')) %>% # discussion with Jenna, was determined tag was likely mislabeled in the wrong catch area
             filter(!id == '2022-09-14 Canada 100') # discrepancies between plant info, CC data, and LFs for this info, so removed after discussions with Jenna
             
                # 1198
           
           # QC: check for gear types & weird date?
           
           # temp <- complete.returns %>%
           # filter(id == '2002-09-08 Lady Janice II/Sealife II') # 2002 - from 2022 dataset so must be 2022
           
           # temp <- data.frame(unique(complete.returns$GearType))
           
           # finalize
           
           write.csv(complete.returns, 'complete.returns.csv')
           
           complete.returns <- read.csv('complete.returns.csv') # 1198
           
           
  # 1.6 Compile observations with incomplete data ####
    # dfs of tag returns with incomplete data made below: 
      
      # DFO mailed in ####
      # Select tag returns with incomplete information (incomplete tag numbers, no return date, no vessel name, duplicates)

      # filter out tag number 42469 from 'tag return info 2020 DFO.csv' as this one is not in DFOmailedin2023.csv and format columns to match
      
      tag42429 <- read.csv("tag return info 2020 DFO.csv") %>%  # gives warning but loads fine
        filter(Tag.Number %in% 42469) %>%
        rename("TAG_NUMBER" = 1,
               "DATE" = 2,
               "BOAT" = 3,
               "GearType" = 6,
               "Y" = 4,
               "X" = 5,
               "Catch.t" = 7,
               "Notes" = 10,
               "catchAREA" = 9) %>%
        filter(!BOAT == 'Morning Star') %>%
        mutate('DATE_ENTERED' = NA,
               'Captured.by' = NA,
               'Address' = NA,
               'Company' = NA,
               'Means.found' = NA,
               'Can.serial.number' = NA,
               'BOAT' = NA,
               'Notes' = 'Plant was not sure if this tag came off Leroy & Barry II or Morning Star') %>%
        select(!weir.number) %>%
        mutate(DATE_ENTERED = as.factor(DATE_ENTERED)) %>%
        mutate(dataorigin = 'tag return info 2020 DFO.csv')     
      

    ` # create xDFO by loading DFOmailedin2023.csv + add tag42429
      
      tDFO <- read.csv("DFOmailedin2023.csv") %>%
        rename("catchAREA" = "RETURN_LOCATION",
               "DATE" = "RETURN_DATE",
               "BOAT" = "RETURN_VESSEL_NAME",
               "GearType" = "RETURN_GEAR_TYPE",
               "Company" = "RETURN_FISH_PLANT",
               "Catch.t" = 'Catch.amount..mt.') %>%
        mutate(dataorigin = 'DFOmailedin2023.csv') %>%
        rename('Y' = "RETURN_LATITUDE",
               'X' = "RETURN_LONGITUDE") %>%
        select(-c(DAYS_AT_LARGE, Phone.number)) %>%
        filter(!TAG_NUMBER %in% DFOreturns$TAG_NUMBER) %>%
        filter(!TAG_NUMBER %in% c(500270, 499078, 495992, 600479, 600440, 600457,
                                  580175, 522854, 522563, 487227, 458234, 457506,
                                  455355, 455193, 455133, 455086, 450689, 449786,
                                  448906, 448505, 448029, 448003, 444728, 423786,
                                  603503, 511150, 511097, 491388, 481022, 457452,
                                  442788, 442133, 42937, 42804, 44803)) # 31
        
      xDFO <- rbind(tag42429, tDFO) %>%
        mutate(BOAT = replace(BOAT, BOAT == "N/A", '')) %>%
        mutate("Notes" = case_when(BOAT == "Cape Breeze NS Boats" ~ "Cape Breeze NS Boats",
                                   BOAT == "Comeau's NS Boats" ~ "Comeau's NS Boats",
                                   BOAT == "2nd GEN" ~ "2nd GEN")) %>%
        mutate(BOAT = replace(BOAT, BOAT %in% nonboat, ''))

      
      # plant digitally sent in ####
    
          #2022 sheets 
            # Kept all of Comeau2022 
      
          xConnors2022 <- read.csv("ConnorsReturn2022.csv") %>% # TAG_NUMBER = 507494 removed from Connors2022 which has 2 observations with different dates
            filter(!row_number() %in% c(1)) %>%
            rename("TAG_NUMBER" = 1,
                   "catchAREA" = 11,
                   "DATE" = 3,
                   "BOAT" = 8,
                   "GearType" = 9,
                   'Date_found' = 2,
                   'Time_found' = 4,
                   'Name' = 5,
                   'Address' = 6,
                   'Date_codes' = 7,
                   'Catch.t' = 10) %>%
            mutate('Company' = 'Connors',
                   'dataorigin' = 'ConnorsReturn2022.csv',
                   'Notes' = '') %>%
            mutate(Notes = as.character(Notes))

      
          xScotiaGarden2022 <- read.csv("ScotiaGardenReturns2022.csv") %>%
            rename("TAG_NUMBER" = 5,
                   "catchAREA" = 3,
                   "DATE" = 1,
                   "BOAT" = 2,
                   'Name' = 6,
                   'Address' = 7,
                   'Catch.t' = 4) %>%
            mutate('Company' = 'Connors',
                   'dataorigin' = 'ScotiaGardenReturns2022.csv',
                   "GearType" = 'Seiner',
                   'Date_found' = '',
                   'Time_found' = '',
                   'Date_codes' = '') %>%
            mutate(Name = as.factor(Name),
                   Time_found = as.factor(Time_found),
                   GearType = as.factor(GearType),
                   catchAREA = as.factor(catchAREA),
                   TAG_NUMBER = as.factor(TAG_NUMBER),
                   Catch.t = as.factor(Catch.t)) %>%
            select(!c('Phone..', ))
      

          # dup <- duplicated(xConnors2022$TAG_NUMBER)
             # tags <- as.vector(xConnors2022$TAG_NUMBER)
             # temp <- data.frame(tags, dup) # seems some of the ones labelled 'damaged' are duplicates
              
             # temp1 <- xConnors2022 %>%
             #   filter(TAG_NUMBER == 'Damaged')
             # temp2  <- xConnors2022 %>%
             #   filter(TAG_NUMBER == 'Damaged ')
             # temp <- rbind(temp1, temp2) # remove one 'Jacob Tinker' and 'Katrina Mitchell' (code added in above into creation of xConnors2022)

          #2021 sheets
            # Kept all of Comeau2021 and ScotiaGarden2021
              
          xConnors2021 <- read.csv("ConnorsReturn2021.csv") %>%
            rename("TAG_NUMBER" = 1,
                   "catchAREA" = 11,
                   "DATE" = 3,
                   "BOAT" = 8,
                   "GearType" = 9,
                   'Date_found' = 2,
                   'Time_found' = 4,
                   'Name' = 5,
                   'Address' = 6,
                   'Catch.t' = 10) %>%
            mutate('Company' = 'Connors',
                   'Date_codes' = '',
                   'dataorigin' = 'ConnorsReturn2021.csv',
                   'Notes' = '') %>%
            select(!c(PHONE.NO.)) %>%
            mutate(Date_codes = as.factor(Date_codes)) %>%
            mutate(TAG_NUMBER = as.factor(TAG_NUMBER),
                   Notes = as.character(Notes)) %>%
            filter(!row_number() %in% c(120:140))
          
          
          #2020 sheets
            # Kept all Comeau2020, ScotiaGarden2020, 
    
          xConnors2020 <- read.csv("ConnorsReturn2020.csv") %>%
            filter(!row_number() %in% c(325:330)) %>%
            rename("TAG_NUMBER" = 1,
                   "catchAREA" = 11,
                   "DATE" = 3,
                   "BOAT" = 8,
                   "GearType" = 9,
                   'Date_found' = 2,
                   'Time_found' = 4,
                   'Name' = 5,
                   'Address' = 6,
                   'Catch.t' = 10) %>%
            select(!c(PHONE.NO.)) %>%
            mutate('Company' = 'Connors',
                   'Date_codes' = NA,
                   'dataorigin' = 'ConnorsReturn2020.csv',
                   'Notes' = '') %>%
            mutate(Date_codes = as.factor(Date_codes),
                   Notes = as.character(Notes))
            

          # combine all incomplete plant data (xConnors2022, xConnors2021, xConnors2020) and filter out !TAG_NUMBER in plantreturns$TAG_NUMBER
            # remove non-boats from $BOATS and add to $Notes ("N/A", "Pre Cuts", "Frozen", "Cape Breeze/Frozen", "Comeau Frozen", "Comeau Fillets", "Comeau Fillets/Fresh", "Fillets")
          
          nonboat = c("Pre Cuts","Fillets ","Frozen Fillets Comeau","Frozen Comeau Fillets","Frozen Cape Breeze",
                      "Snackline","Frozen","Cape Breeze/Frozen","Comeau Frozen","Comeau Fillets","Comeau Fillets/Fresh",
                      "Fillets", "Snack Fillets", "Sea Crest (Pre Cuts)", "Comeau Fillets ", "Comeau", "Cape Breeze",
                      "Comeau's Frozen", "Sea Crest ", "Seacrest", "Sea Crest Frozen", "Comeaus", "Cape Breeze Seafood", 'RK Murphy',
                      'Comeau Seafood - Frozen', 'Scotia Garden - Frozen')  
            
          
          # str(xConnors2022)
          # str(xConnors2020)
          # str(xScotiaGarden2022)
          
          xplantreturnst <- rbind(xConnors2022, xConnors2021, xConnors2020, xScotiaGarden2022) %>%
            filter(!TAG_NUMBER %in% plantreturns$TAG_NUMBER) %>%
            mutate(BOAT = replace(BOAT, BOAT == "N/A", '')) %>%
            mutate("Notes" = case_when(BOAT == "Pre Cuts" ~ "Pre Cuts",
                                       BOAT == "Frozen" ~ "Frozen",
                                       BOAT == "Cape Breeze/Frozen" ~ "Cape Breeze/Frozen - Silver Harvester?",
                                       BOAT == "Comeau Frozen" ~ "Comeau Frozen",
                                       BOAT == "Comeau Fillets" ~ "Comeau Fillets",
                                       BOAT == "Comeau Fillets/Fresh" ~ "Comeau Fillets/Fresh",
                                       BOAT == "Fillets" ~ "Fillets",
                                       BOAT == "Snackline" ~ "Snackline",
                                       BOAT == "Fillets " ~ "Fillets",
                                       BOAT == "Frozen Comeau Fillets" ~ "Frozen Comeau Fillets",
                                       BOAT == "Frozen Cape Breeze" ~ "Frozen Cape Breeze",
                                       BOAT == "Frozen Fillets Comeau" ~ "Frozen Fillets Comeau",
                                       BOAT == "Snack Fillets" ~ "Snack Fillets",
                                       BOAT == "Sea Crest (Pre Cuts)" ~ "Sea Crest (Pre Cuts) - Dual Venture?",
                                       BOAT == "Comeau Fillets " ~ "Comeau Fillets",
                                       BOAT == "Comeau" ~ "Comeau",
                                       BOAT == "Cape Breeze" ~ "Cape Breeze",
                                       BOAT == "Comeau's Frozen" ~ "Comeau's Frozen",
                                       BOAT == "Sea Crest " ~ "Sea Crest ",
                                       BOAT == "Seacrest" ~ "Seacrest",
                                       BOAT == "Sea Crest Frozen" ~ "Sea Crest Frozen",
                                       BOAT == "Comeaus" ~ "Comeaus",
                                       BOAT == "Cape Breeze Seafood" ~ "Cape Breeze Seafood",
                                       BOAT == "Comeau Seafood - Frozen" ~ "Comeau Seafood - Frozen",
                                       BOAT == 'Scotia Garden - Frozen' ~ 'Scotia Garden - Frozen')) %>%
            mutate(BOAT = replace(BOAT, BOAT %in% nonboat, ''))  %>%
            mutate(Notes = if_else(TAG_NUMBER == '507494', '2 observations with different dates and boats', Notes))
 
      # rawReturns incomplete ####
          
          xraw <- rawReturntemp %>%
            mutate(BOAT = recode(BOAT, 'Sealife Ii' = 'Sealife II',
                                 'Morning Star ' = 'Morning Star',
                                 'Lady Janice Ii' = 'Lady Janice II', 
                                 'Lady Melissa/Sealife Ii - Mixed Tanker' = 'Lady Melissa/Sealife II - Mixed Tanker',
                                 'Lady Janice Ii/Sealife Change Over' = 'Lady Janice II/Sealife II Change Over',
                                 'Leroy & Barry Ii ' = 'Leroy & Barry II',
                                 'Leroy & Barry Ii' = 'Leroy & Barry II',
                                 'Lady Janice/Sealife Ii' = 'Lady Janice II/Sealife II',
                                 'Fundy Monarch ' = 'Fundy Monarch',
                                 'Comeau Fillets ' = 'Comeau Fillets',
                                 'Rk Murphy ' = 'Rk Murphy',
                                 'Fillets ' = 'Fillets',
                                 'Sealife Ii/Lady Janice Ii Mixed Tanker' = 'Sealife II/Lady Janice II',
                                 'Sealife Ii/Lady Janice' = 'Sealife II/Lady Janice II',
                                 'Leroy And Barry' = 'Leroy & Barry II',
                                 'Lady Melissa/Lady Janice' = 'Lady Janice II/Lady Melissa',
                                 'Andrew And Dean' = 'Andrew & Deane',
                                 'Capeico' = 'Capelco',
                                 'CapeIco' = 'Capelco',
                                 'Leroy & Barry' = 'Leroy & Barry II',
                                 'Leroy & Barry ' = 'Leroy & Barry II',
                                 'Lady Janice' = 'Lady Janice II',
                                 'Provider' = 'Brunswick Provider',
                                 'RK Murphy ' = 'RK Murphy',
                                 'Leroy Barry' = 'Leroy & Barry II',
                                 'Lady Janice/Lady Melissa' = 'Lady Janice II/Lady Melissa',
                                 'Sealife II/Lady Janice' = 'Sealife II/Lady Janice II',
                                 'Andrew & Deane' = 'Andrew and Dean',
                                 'Leroy and Barry' = 'Leroy & Barry II',
                                 'Tasha Marie ' = 'Tasha Marie')) %>%
            filter(!TAG_NUMBER %in% rawReturn$TAG_NUMBER)
          
          
          
      # HSC mailed incomplete #### 
          
          xHSCmailed <- read.csv("Mail in Tag Returns.csv") %>%
            rename("TAG_NUMBER" = 'Tag.Number',
                   "catchAREA" = 'Ground',
                   "DATE" = 'Landed.Date',
                   "BOAT" = 'Vessel',
                   "GearType" = 'Gear.Type',
                   'Catch.t' = 'Catch.Amount',
                   'Company' = 'Return.Company') %>% 
            mutate(dataorigin = 'Mail in Tag Returns.csv') %>%
            filter(!is.na(TAG_NUMBER)) %>%
            mutate(DATE = ymd(DATE)) %>%
            filter(!TAG_NUMBER %in% HSCmailed$TAG_NUMBER) %>%
            select(!c(Lat, Lon, Name.of.document, Link.to.document))
          
          colnames(xHSCmailed)
          

      # HSC web returns (all incomplete) ####
          
          HSCweb2022 <- read.csv("2022 website or email HSC.csv") %>%
            rename('TAG_NUMBER' = 'Tag_Number',
                   'DATE' = 'Return_Date',
                   'catchAREA' = 'Return_Location',
                   'Notes' = 'Comments') %>%
            filter(!is.na(TAG_NUMBER)) %>%
            select(!9:22) %>%
            mutate(dataorigin = "2022 website or email HSC.csv")
          
          HSCweb2023 <- read.csv("2023 website or email HSC.csv") %>% # 1 observation as of May 25th, 2023
            rename('TAG_NUMBER' = 'Tag_Number',
                   'DATE' = 'Return_Date',
                   'catchAREA' = 'Return_Location',
                   'Notes' = 'Comments') %>%
            filter(!is.na(TAG_NUMBER)) %>%
            select(!11:26) %>%
            mutate(DATE = '',
                   dataorigin = '2023 website or email HSC.csv') %>%
            select(!c(Days_at_Large, ID))
            
            # colnames(HSCweb2023)
            # colnames(HSCweb2022)
            
          xHSCweb <- rbind(HSCweb2022, HSCweb2023)
          
          
      # Combine all incomplete data ####
        #DATASETS TO COMBINE FOR ALL INCOMPLETE RETURNS: xDFOmailedreturns, xplantreturns, xrawReturns, complete.returnsdupdates, xHSCmailed, and xHSCweb
    
          # format to have same columns
          
          xDFOmailedreturns <- xDFO %>%
            rename('Catch.t' = 7,
                   'Name' = 11) %>%
            mutate(DATE = ymd(DATE)) %>%
            mutate(Year = year(DATE)) %>%
            mutate(Date_codes = NA,
                   Time_found = NA,
                   Date_found = NA,
                   Plant.Found = NA) %>%
            mutate(TAG_NUMBER = as.factor(TAG_NUMBER))
          
          xplantreturns <- xplantreturnst %>%
            mutate(Can.serial.number = NA,
                   DATE_ENTERED = NA,
                   X = NA,
                   Y = NA,
                   Means.found = NA,
                   Plant.Found = NA) %>%
            mutate(DATE = replace(DATE, DATE == 'N/A', '')) %>%
            mutate(DATE = mdy(DATE)) %>%
            mutate(Year = year(DATE))
          
          xrawReturn <- xraw %>%
            mutate(DATE = ymd(DATE)) %>%
            mutate(Year = year(DATE)) %>%
            mutate(Can.serial.number = NA,
                   DATE_ENTERED = NA,
                   X = NA,
                   Y = NA,
                   Means.found = NA,
                   Name = NA,
                   Date_codes = NA,
                   dataorigin = 'rawReturn',
                   Company = NA,
                   Time_found = NA,
                   Date_found = NA,
                   Address = NA,
                   Notes = NA,
                   Catch.t = NA,
                   Plant.Found = NA) %>%
            mutate(TAG_NUMBER = as.factor(TAG_NUMBER))
          
          xHSCmailedreturns <- xHSCmailed %>%
            mutate(Year = year(DATE)) %>%
            rename('Date_codes' = 'DayCode',
                   'Date_found' = 'Date.Found') %>%
            mutate(Can.serial.number = NA,
                   X = NA,
                   Y = NA,
                   Means.found = NA,
                   Time_found = NA,
                   DATE_ENTERED = NA) %>%
            mutate(TAG_NUMBER = as.factor(TAG_NUMBER))
            
          xHSCwebreturns <- xHSCweb %>%
            mutate(DATE = recode(DATE, 'Unknown' = '')) %>%
            mutate(DATE = mdy(DATE)) %>%        
            mutate(Year = year(DATE)) %>%
            rename('Can.serial.number' = 'Can.Serial',
                   'DATE_ENTERED' = 'Date_Entered',
                   'Means.found' = 'catchAREA') %>%
            mutate(X = NA,
                   Y = NA,
                   catchAREA = NA,
                   Time_found = NA,
                   Date_codes = NA,
                   Plant.Found = NA,
                   Date_found = NA,
                   BOAT = NA,
                   GearType = NA,
                   Catch.t = NA,
                   Company = NA) %>%
            mutate(TAG_NUMBER = as.factor(TAG_NUMBER))
            
            complete.returnsdupdates <- rbind(plantreturns, rawReturn, DFOreturns, HSCmailed) %>%
            filter(TAG_NUMBER %in% duptagdate) %>% # duptagdate built in complete.returns QC section
            mutate(DATE = ymd(DATE)) %>%
            mutate(Year = year(DATE)) %>%
            mutate(Can.serial.number = NA,
                   DATE_ENTERED = NA,
                   X = NA,
                   Y = NA,
                   Means.found = NA,
                   Name = NA,
                   Date_codes = NA,
                   Company = NA,
                   Time_found = NA,
                   Date_found = NA,
                   Address = NA,
                   Notes = "duplicated TAG_NUMBER with differing DATE in complete.returns",
                   Plant.Found = NA) %>%
            mutate(TAG_NUMBER = as.factor(TAG_NUMBER)) # 6
          
          # combine datasets
          
          incomplete.returns <- rbind(xDFOmailedreturns, xplantreturns, xrawReturn, xHSCmailedreturns, xHSCwebreturns, complete.returnsdupdates) # 300
    
          write.csv(incomplete.returns, 'incomplete.returns.csv')
          
          incomplete.returns <- read.csv('incomplete.returns.csv') # 300
            
            
            
          
# 2. Load and format commercial catch data ####

   # Load Grounds legend - dataframe converts numbers to text so we can actually understand the ground
   grounds <- read.csv("commercialcatchGrounds.csv")
   portCodes <- read.csv("portCodes.csv") 
                
   # Load commercial catch data - this data frame is created from an output of the herring database (up to 2020)
    # overlap in dates between commercialcatches.csv and CC2018-2022.
   
    # load passive gear
   passive <- read.csv("weirShutCatches.csv") %>%
     mutate(DATE = ymd(DATE)) %>%
     left_join(portCodes, by = c("portCode" = "PortCode"), multiple = "all") %>%
     mutate(id = paste(DATE, Vessel)) 
   
# Build catches
    # in CC excel doc, SILVER HARVESTER I 2019-08-24  12:00:00 AM deleted as no coords given

   missingDFOCCdata <- data.frame(Vessel.name = 'Fundy Monarch', # tag 473184 was not linking up as there was no corresponding CC data for it. There was a matching length frequency and this is the info here added in
                                  Lat = 43.216667,
                                  Lon = -66.333333,
                                  NAFO = '4XQ',
                                  Catch.t = 193,
                                  LAND_DATE = '2022-09-13',
                                  Actual.Date = '',
                                  Ground = 'German Bank',
                                  Year = 2022,
                                  id = '2022-09-13 Fundy Monarch') %>%
     mutate(LAND_DATE = ymd(LAND_DATE),
            Actual.Date = ymd(Actual.Date))
   
   catches1 <- read.csv("20152022DFOmobile2.csv") %>% # subsequent file Jonathan sent with 20 more observations than the last
   rename(Catch.t = tonnes,
          Vessel.name = VESSELS_NAME,
          NAFO = ARS_AREA,
          Lon = Long) %>%
     mutate(FishingArea = gsub("HERRING SCIENCE FISHING AREA - ", "", Herring.Science.Fishing.Area)) %>%
     mutate(LAND_DATE = mdy(test.landed),
            Actual.Date = mdy(test.actual)) %>%
     mutate(Vessel.name = str_to_title(Vessel.name),
            Vessel.name = recode(Vessel.name, 'Leroy And Barry' = 'Leroy & Barry II',
                                 'Lady Janice' = 'Lady Janice II',
                                 'Sealife Ii' = 'Sealife II',
                                 'Lady Janice Ii' = 'Lady Janice II',
                                 'Leroy And Barry Noii' = 'Leroy & Barry II'),
            Ground = recode(FishingArea, 'GANNET/DRY LEDGE - ' = 'Gannet Dry Ledge',
                            'GERMAN BANK - 97' = 'German Bank',
                            'GRAND MANAN - 94' = 'Grand Manan',
                            'LONG ISLAND - 93' = 'Long Island',
                            'LURCHER - 92' = 'Lurcher',
                            'NB COASTAL - 89' = 'NB Coastal',
                            'OUTER BANK - 98' = 'Outer Bank',
                            'SCOTS BAY - 91' = 'Scots Bay',
                            'SEAL ISLAND - 96' = 'Seal Island',
                            'SW GROUNDS - 88' = 'SW Grounds',
                            'TRINITY - 99' = 'Trinity',
                            'UNDETERMINED OR UNASSIGNED' = 'Undetermined or unassigned',
                            'WESTERN HOLE - 83' = 'Western Hole',
                            'YANKEE BANK - 86' = 'Yankee Bank'),
            Year = year(LAND_DATE)) %>%
     select(-c(LATITUDE, LONGITUDE, long.neg, test.actual, test.landed, Date.Landed, Herring.Science.Fishing.Area, Total..kg.s., DA_ACTUAL_DATE, FishingArea)) %>%
     mutate(id = paste(LAND_DATE, Vessel.name, Catch.t, Lat, Lon)) %>%
     mutate(id = paste(LAND_DATE, Vessel.name)) # 5134
   
   catches <- bind_rows(catches1, missingDFOCCdata) # 5135
   
   write.csv(catches, 'catches.csv')
   
   # QC steps
        # colnames(catchestemp)
        # str(catchestemp)
        # boats <- data.frame(table(catches$Vessel.name)) # check for boat naming consistency
        # fishingarea <- data.frame(table(catchestemp$FishingArea))
 
# 3. Combine returns to CC ####
    
# Load formatted tag return data  
    
  complete.returns <- read.csv('complete.returns.csv') %>%
     select(!c(X)) # 1198 returns; has 'id' column already 
    

    # QC steps: 
      # temp <- data.frame(unique(complete.returns$BOAT)) # check that boat names make sense
      # temp <- data.frame(duplicated(complete.returns$TAG_NUMBER)) %>% table() # check for duplicated tag numbers; good to go

  # Mobile gear ####            
        
    psReturns <- complete.returns %>%
      filter(!GearType %in% c("Mid Water Trawl", "Weir_ShutOff", "Shut off", "ShutOff", "Weir/Star", "Weir/Black Water", "Weir")) %>% # Remove tag returns that are mid water trawl or weir_shutoff as these catches have not been identified  %>%
      mutate(id = recode(id, '2020-08-12 Lady Janice II/Lady Melissa' = '2020-08-12 Lady Melissa', # manually assigning tags that were given two boats to the boat with the most catch; (as determined below)
                        '2021-08-03 Lady Janice II/Sealife II' = '2021-08-03 Sealife II',
                        '2021-08-19 Lady Janice II/Sealife II' = '2021-08-19 Sealife II',
                        '2021-08-19 Lady Janice II/Lady Melissa' = '2021-08-19 Lady Melissa',
                        '2021-10-06 Lady Janice II/Lady Melissa' = '2021-10-06 Lady Melissa',
                        '2021-10-22 Lady Janice II/Lady Melissa' = '2021-10-22 Lady Melissa',
                        '2022-08-11 Lady Melissa/Sealife II' = '2022-08-11 Sealife II',
                        '2022-08-26 Lady Janice II/Sealife II' = '2022-08-26 Sealife II')) %>%
      mutate(id = recode(id, '2021-07-21 Sealife II' = '2021-07-20 Sealife II', # these ones failed to match cause their date was off by one according to CC and LFs so manually changing the id to make them match
                         '2020-08-13 Leroy & Barry II' = '2020-08-12 Leroy & Barry II',
                         '2022-07-07 Fundy Monarch' = '2022-07-06 Fundy Monarch',
                         '2002-09-08 Lady Janice II' = '2022-09-08 Lady Janice II',
                         '2022-08-11 Canada 100' = '2022-08-12 Canada 100',
                         '2022-08-31 Fundy Monarch' = '2022-08-30 Fundy Monarch',
                         '2022-10-13 Canada 100' = '2022-10-12 Canada 100',
                         '2021-07-05 Brunswick Provider' = '2021-07-06 Brunswick Provider',
                         '2021-08-23 Brunswick Provider' = '2021-08-25 Brunswick Provider',
                         '2021-08-25 Canada 100' =  '2021-08-26 Canada 100',
                         '2021-06-19 Leroy & Barry II' = '2021-06-18 Leroy & Barry II',
                         '2021-08-19 Leroy & Barry II' = '2021-08-18 Leroy & Barry II',
                         '2021-09-02 Leroy & Barry II' = '2021-09-01 Leroy & Barry II',
                         '2021-09-08 Leroy & Barry II' = '2021-09-07 Leroy & Barry II',
                         '2021-09-16 Leroy & Barry II' = '2021-09-14 Leroy & Barry II',
                         '2020-08-10 Canada 100' = '2020-08-11 Canada 100',
                         '2020-10-07 Canada 100' = '2020-10-06 Canada 100',
                         '2020-10-07 Brunswick Provider' = '2020-10-06 Brunswick Provider',
                         '2020-10-16 Canada 100' = '2020-10-15 Canada 100',
                         '2020-10-19 Canada 100' = '2020-10-18 Canada 100',
                         '2020-10-19 Brunswick Provider' = '2020-10-18 Brunswick Provider',
                         '2017-08-28 Lady Janice II' = '2017-08-27 Lady Janice II',
                         '2022-08-18 Morning Star' = '2022-08-17 Morning Star')) # 1166 observations
 
      # QC steps:
          #  table(duplicated(psReturns$TAG_NUMBER)) # check for tag duplicates - looks good
          # temp <- data.frame(unique(psReturns$GearType)) # check for the various nomenclature for weirs and shutoffs
          
          # under BOATS, there is 'Lady Janice II/Sealife II', 'Lady Janice II/Lady Melissa', and 'Lady Melissa/Sealife II'
                # can these observations be assigned to one boat or another given the CC data?
    
                # tempps <- psReturns %>%
                  # filter(BOAT %in% c('Lady Janice II/Sealife II', 'Lady Janice II/Lady Melissa', 'Lady Melissa/Sealife II'))
                  # from ComeauReturns2020, 2021, and 2022. Do the original datsheets have tonnage on them? yes...
                  # tag 462648, 88 tonnes (Lady Janice II/Sealife II) from original datasheet
                      # from CC totaling 2021-08-03 Lady Janice II and 2021-08-03 Sealife II: 14.19+20.81+10.60+21.20+21.20=88
                      # thus, this doesn't help as they just totaled it all in the plants
                      
                # test <- data.frame(unique(tempps$id))
    
                # tempcatches <- catches %>%
                  # filter(id %in% c('2021-08-03 Lady Janice II', '2021-08-03 Sealife II', 
                                  # '2021-08-19 Lady Janice II', '2021-08-19 Sealife II',
                                  # '2021-10-06 Lady Janice II', '2021-10-06 Lady Melissa',
                                  # '2021-10-22 Lady Janice II', '2021-10-22 Lady Melissa',
                                  # '2021-08-19 Lady Janice II', '2021-08-19 Lady Melissa', 
                                  # '2022-08-11 Lady Melissa', '2022-08-11 Sealife II',
                                  # '2022-08-26 Lady Janice II', '2022-08-26 Sealife II',
                                  # '2022-09-01 Lady Melissa', '2022-09-01 Sealife II',
                                  # '2022-09-08 Lady Janice II', '2022-09-08 Sealife II',
                                  # '2020-08-12 Lady Janice II', '2020-08-12 Lady Melissa'))
    
                # unfortunately, looks like these are hard to figure out which vessel is associated to the tag. 
                # check to make sure corresponding options are all in the same grounds - only one that isn't is 2021-10-06 Lady Janice II/Lady Melissa
                # remove 2021-10-06 Lady Janice II/Lady Melissa, (incorporated above in psReturns), then
                # Code to choose option with highest catch as they were in the same general area for the rest 
                    # recode psReturns with appropriate id to match it to the boat in CC with the largest catch that matches the two id options (incorporated above)

                  # tags assigned to the vessel with the largest catch
                    # 502044 -> no date match in CC
                    # 550173 -> Lady Melissa
                    # 462648 -> Sealife II
                    # 589571 -> Sealife II
                    # 589890 -> Sealife II
                    # 463463 -> Sealife II
                    # 494392 -> Sealife II
                    # 590756 -> Lady Melissa
                    # 514221 -> Lady Melissa
                    # 516784 -> Lady Melissa
                    # 518791 -> Sealife II
                    # 500396 -> Sealife II
                    # 498420 -> Sealife II
                    # 502546 -> Sealife II
                    # 510467 -> no date match in CC

      # Continue with analysis - find all matching ids between psReturns and catches
         
        temp <-  psReturns %>% 
          group_by(id) %>% 
          filter(n()>1) %>%
          arrange(id) # looking at returns that potentially came from the same event
        
        # Link CC to return data
        
        listofIDS <- unique(psReturns$id) # 444 unique events/id - some events resulted in more than one return
        
        potentialCatchLocations <- catches %>% # filtering commercial catch data to ids in psReturn
           filter(id %in% listofIDS) # 466 observations match up
                 
                # look for duplicated ids in potentialCatchLocations i.e. where there are multiple possible events that match a return           
                dups <- duplicated(potentialCatchLocations$id)
                ids <- potentialCatchLocations$id
                temp <- data.frame(ids, dups)
                true = temp[which(temp$dups == "TRUE"), ]
        
                dupidsvec <- as.vector(true$ids)
                
                dupids <- potentialCatchLocations %>%
                  filter(id %in% dupidsvec) # 159 events with duplication i.e. same ids in commercial catch data but different events

        # This dataframe gives us all the tag returns linked with commercial catch but there are tag number duplicates                                                    
        catchesAroundReturnsPS <- merge(psReturns, catches, by = "id") %>%
          rename(Catch.plant = Catch.t.x,
                 Catch.DFO = Catch.t.y) # 1375
        
        l <- duplicated(catchesAroundReturnsPS$TAG_NUMBER) # 1375
        
          # remove duplicated tag numbers by choosing event with the largest catch and thus highest probability
              # first check that 
        
          mobile = catchesAroundReturnsPS
                                              
              t <- duplicated(mobile$TAG_NUMBER) 
              l = mobile$TAG_NUMBER # list of all tag numbers that are duplicated in this dataframe
              f = data.frame(t,l)
              table(f$t) # 1120 false (not duplicated), 255 true
              
              fefe <- f %>% filter(t == "TRUE")
              o = unique(fefe) # 174 unique tags that have duplicates
                                              
                    # Remove duplicated tag numbers, choose the one with the largest catch amount 
                          # there are some observations with the same catch amount that may have to be manually sorted out....
                                              
                    temp <- mobile %>%
                      group_by(TAG_NUMBER) %>%
                      filter(Catch.DFO == max(Catch.DFO)) %>%
                      arrange(TAG_NUMBER) # 1160 - this picked the highest but there are still duplicates cause some duplicates also had the same tonnage..
                            
                            # identify remaining duplicates
                            g <- duplicated(temp$TAG_NUMBER) 
                            j = temp$TAG_NUMBER 
                            k = data.frame(g,j)
                            k <- k %>% filter(g == "TRUE")
                            k <- k %>% distinct(j)
                            vec <- pull(k) # 26
                        
                            temp1 <- temp %>%
                              filter(TAG_NUMBER %in% vec) # check that all corresponding observations have the same grounds from the CC dataset
                            
                    mobiletemp <- mobile %>%
                      group_by(TAG_NUMBER) %>%
                      slice(which.max(Catch.DFO)) # 1120 - Automatically chooses the first id with max tonnage
   
                            # check for duplicates
                            temp <- data.frame(table(duplicated(mobiletemp$TAG_NUMBER))) %>% # no more dups
                                        print()
    # formatting
      
    mobileF <- mobiletemp %>%
      select(-c(Vessel.name, Year, Ground, LAND_DATE)) %>% 
      mutate(DATE = ymd(DATE)) %>%
      ungroup() %>%
      as.data.frame() # 1120 observations; mobileF is ready
   
      
      # QC: which tag returns in purse seine did not link to CC and why....
     
      psReturns # 1166 observations
      mobileF # 1120 observations
      
         # find tag returns that did not link
        finaltags <- as.vector(mobileF$TAG_NUMBER)
      
        returnsthatdidntmatch <- psReturns %>%
          mutate(Year = year(DATE)) %>%
          filter(!TAG_NUMBER %in% finaltags,
                 !dataorigin == 'rawReturn.csv') # 8
       
        catches
        
          # investigate returns that don't match in to CC
          # closest match in CC: 
              tag <- mobileF %>%
                filter(TAG_NUMBER == 488145)
              
              testCCdata <- catches %>%
                filter(Vessel.name %in% tag$BOAT,
                       Year %in% tag$Year)

              testCCdata <- catches %>%
                filter(Vessel.name == 'Leroy & Barry II',
                       Year == '2021')
              
              # double check manually assigned returns:
       
              temp <- mobileF %>%
                filter(id %in% c('2021-07-20 Sealife II', '2020-08-12 Leroy & Barry II', '2022-07-06 Fundy Monarch',
                                 '2022-09-08 Lady Janice II', '2022-08-12 Canada 100', '2022-08-30 Fundy Monarch',
                                 '2022-10-12 Canada 100', '2021-07-06 Brunswick Provider', '2021-08-25 Brunswick Provider',
                                 '2021-08-26 Canada 100', '2021-06-18 Leroy & Barry II', '2021-08-18 Leroy & Barry II',
                                 '2021-09-01 Leroy & Barry II', '2021-09-07 Leroy & Barry II', '2021-09-14 Leroy & Barry II',
                                 '2020-08-11 Canada 100', '2020-10-06 Canada 100', '2020-10-06 Brunswick Provider',
                                 '2020-10-15 Canada 100', '2020-10-18 Canada 100', '2020-10-18 Brunswick Provider',
                                  '2017-08-27 Lady Janice II', '2022-08-17 Morning Star')) %>%
                arrange(id)
          
  # Fixed gear ####

# initial prep 

              # The following tags returns in fixed gear were deemed to not have enough info to match into CC (no CC matching the year and months) but are important to keep track of none the less as they were from fixed gear, as per discussions with Jenna:
              unmatchable_fixedreturns <- complete.returns %>%
                filter(TAG_NUMBER %in% c(460200, 442815, 445366, 450805, 449455, 450645))
      
              
                      
# continue with analyses:                           
fixedreturns <- complete.returns %>%
  filter(!GearType %in% c("Purse Seine", "Mid Water Trawl")) %>%
  mutate(DATE = ymd(DATE)) %>%
  mutate(TAG_NUMBER = as.factor(TAG_NUMBER)) %>%
  mutate(id = recode(id, '2019-08-15 Silver King' = '2019-08-14 Silver King')) %>% # tag returns that were not matching in cause they were off by 1 day, manually making match by changing id
  filter(!TAG_NUMBER %in% unmatchable_fixedreturns$TAG_NUMBER) # remove unmatchable_fixedreturns to streamline
      # 27
    
    # temp <- data.frame(unique(complete.returns$GearType))
    # str(fixedreturns)

    duplicated(fixedreturns$TAG_NUMBER) # no duplicated tags
    data.frame(duplicated(fixedreturns$id)) # TRUE = tags caught in the same weir event (e.g. 2019-09-09 Senator Neil)

# prep the passive commercial catch data  
    
    passive <- read.csv("weirShutCatches.csv") %>%
      mutate(DATE = ymd(DATE)) %>%
      left_join(portCodes, by = c("portCode" = "PortCode"), multiple = "all") %>%
      mutate(id = paste(DATE, Vessel)) # 807
    
    write.csv(passive, 'passive.csv')
               
fixed <- merge(fixedreturns, passive, by = "id") # 24
 
duplicated(fixed$TAG_NUMBER) 

fixedF <- fixed  %>%
  group_by(TAG_NUMBER) %>%
  slice(which.max(CATCH_T)) %>%
  ungroup() %>%
  as.data.frame() %>%
  rename(DATE = DATE.x,
         GearType = GearType.x,
         Catch.DFO = CATCH_T,
         Catch.plant = Catch.t) %>%
  select(-c(X, DATE.y, CATCH_KG, GearType.y)) %>%
  mutate(Lat = replace(Lat, is.na(Lat) & TAG_NUMBER == 442788, 44.93033)) %>% # couple tags missing coordinates
  mutate(Lon = replace(Lon, is.na(Lon) & TAG_NUMBER == 442788, -66.9047)) %>%
  mutate(Lat = replace(Lat, is.na(Lat) & TAG_NUMBER == 442133, 44.67364)) %>%
  mutate(Lon = replace(Lon, is.na(Lon) & TAG_NUMBER == 442133, -66.6866)) %>%
  mutate(Lat = replace(Lat, is.na(Lat) & TAG_NUMBER == 44374, 44.7564)) %>%
  mutate(Lon = replace(Lon, is.na(Lon) & TAG_NUMBER == 44374, -66.7342)) %>%
  mutate(Lat = replace(Lat, is.na(Lat) & TAG_NUMBER == 44358, 44.77433)) %>%
  mutate(Lon = replace(Lon, is.na(Lon) & TAG_NUMBER == 44358, -66.7600)) %>%
  mutate(Lat = replace(Lat, is.na(Lat) & TAG_NUMBER == 449347, 44.93033)) %>% 
  mutate(Lon = replace(Lon, is.na(Lon) & TAG_NUMBER == 449347, -66.9047)) %>%
  mutate(Lat = replace(Lat, is.na(Lat) & TAG_NUMBER == 44790, 45.019872)) %>% 
  mutate(Lon = replace(Lon, is.na(Lon) & TAG_NUMBER == 44790, -66.844866))
    # 19  

      # fixedF is ready
     

  # Combine gears ####                         
      
        # combine mobileF and fixedF

        str(mobileF) 
        str(fixedF) 
        
        mobileF <- mobileF %>% mutate(TAG_NUMBER = as.factor(TAG_NUMBER))

      returnData <- bind_rows(fixedF, mobileF) %>%
                      rename(X_return = Lat,
                            Y_return = Lon,
                            ReturnDate = DATE,
                            ReturnNAFO = NAFO,
                            plantReturnPlace = catchAREA,
                            ReturnVessel = BOAT,
                            ReturnCompany = Company,
                            OfficialReturnPlace = Name,
                            ReturnGearType = GearType) %>%
                      select(-id) # 1139

write.csv(returnData, 'returnData.csv')

# Link remaining incomplete.returns, returnsthatdidntmatch, and passive returns that didnt match ####

incomplete.returnstemp <- incomplete.returns %>%
  mutate(whatstage = 'incomplete.returns') %>%
  select(-c(X.1)) %>%
  mutate(DATE = ymd(DATE)) # 300

mobilethatdidntmatch <- returnsthatdidntmatch %>%
  mutate(DATE = ymd(DATE)) %>%
  mutate(Can.serial.number = NA,
         DATE_ENTERED = NA,
         X = NA,
         Y = NA,
         Means.found = NA,
         Name = NA,
         Date_codes = NA,
         Company = NA,
         Time_found = NA,
         Date_found = NA,
         Address = NA,
         Notes = NA,
         Catch.t = NA,
         Plant.Found = NA) %>%
  mutate(TAG_NUMBER = as.factor(TAG_NUMBER)) %>%
  select(-c(id)) %>%
  mutate(whatstage = 'mobilethatdidntmatch') # 8

    a = as.vector(fixedF$TAG_NUMBER)

fixedthatdidntmatch <- complete.returns %>%
  filter(!GearType %in% c("Purse Seine", "Mid Water Trawl")) %>%
  mutate(DATE = ymd(DATE)) %>%
  mutate(Year = year(DATE),
         TAG_NUMBER = as.factor(TAG_NUMBER)) %>%
  mutate(Can.serial.number = NA,
         DATE_ENTERED = NA,
         X = NA,
         Y = NA,
         Means.found = NA,
         Name = NA,
         Date_codes = NA,
         Time_found = NA,
         Date_found = NA,
         Address = NA,
         Notes = NA,
         Catch.t = NA,
         Plant.Found = NA) %>%
  mutate(whatstage = 'fixedthatdidntmatch') %>%
  filter(!TAG_NUMBER %in% a) %>%
  select(!id) %>%
  filter(!TAG_NUMBER == 439808) # 13; 439808 is Brunswick Provider so not fixed gear

unmatchedreturns <- rbind(mobilethatdidntmatch, fixedthatdidntmatch) %>% 
  arrange(TAG_NUMBER) %>%
  slice(-3, -10) # 19;  removing row 3 and 10 duplicates cause they are duplicates and both included gear unspecified

write.csv(unmatchedreturns, 'unmatchedreturns.csv')

     # duplicated(unmatchedreturns$TAG_NUMBER) # tag 3 and 10


unlinkedreturns <- rbind(incomplete.returnstemp, unmatchedreturns) # 319

write.csv(unlinkedreturns, 'unlinkedreturns.csv')
  
# save important dfs ####

write.csv(returnData, 'returnData.csv')

returnData <- read.csv('returnData.csv')

unlinkedreturns <- read.csv('unlinkedreturns.csv')








