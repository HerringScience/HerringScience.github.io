# remove everything in the workspace
rm(list = ls())

# IMPORTANT : SET GROUND, YEAR, AND SURVEY # HERE
surv="GB" #SB or GB or SI
surv2="German Bank" #"German Bank", "Seal Island" or "Scots Bay" as written
year="2024"
surv.no="6"
adhoc = "false" #true or false if an adhoc survey was completed (and "adhoc.csv" exists)
Sample = "Y" #whether ("Y") or not ("N") they caught fish during this survey window
Tow = "Y" #whether or not plankton tow(s) were conducted

#(SB ONLY) Set main-box vessels
ids = c("C1", "FM", "LJ", "MS")

#Area and TS values - From table C
SB1= 476#SB main area
SB2= 0 #SB north area
SB3= 0 #SB east area

GB1 = 826 #GB main area
GB2 = 287 #Seal Island area
GB3 = 0 #Ad-hoc school survey area

##
###
##

#BELOW VALUES SHOULD RARELY CHANGE#
TS1 = -35.5 #TS38

#turnover calculation regression values
GB_y = 0.199392662629964
GB_x_var = 0.528381832773883
GB_days = 31

SB_y = 0.364102758434224
SB_x_var = 0.436969270679439
SB_days = 29

#install.packages("weathercan", repos = "https://dev.ropensci.org")

library(rlang)
library(cli)
library(lubridate)
library(reprex)
library(tidyverse)
library(geosphere)
library(reshape2)
library(moderndive)
library(skimr)
library(ggridges)
library(weathercan)
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
library(readxl)
library(hms)
library(measurements)

##Survey Data import and filtering
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no))
Plankton = read_csv("PlanktonData.csv")
Plankton = Plankton %>%
  mutate(Year = year,
         Ground = surv,
         Survey.No = surv.no,
         TowTime = difftime(Time2, Time1, units = "mins"))
Plankton$Year = as.numeric(Plankton$Year)
Plankton$Swell = as.character(Plankton$Swell)
if(Sample == "Y"){Plankton$Sample = "Y"}
if(Sample == "N"){Plankton$Sample = "N"}

#get CTD data from Plankton
SurveyData = read_csv("Plan Data.csv")
SurveyData$StartDate = as.Date(SurveyData$Date, format = "%d/%m/%Y")

if(!is.na(first(Plankton$CTD_ID))){
  CTDData = read_csv(paste0(Plankton$CTD_ID, ".csv"))
  CTDData = CTDData %>%
    dplyr::select(Pressure = "Pressure (Decibar)", Depth = "Depth (Meter)", Temperature = "Temperature (Celsius)",	Conductivity = "Conductivity (MicroSiemens per Centimeter)", Specific_conductance = "Specific conductance (MicroSiemens per Centimeter)",
                  Salinity = "Salinity (Practical Salinity Scale)", Sound_velocity = "Sound velocity (Meters per Second)", Density = "Density (Kilograms per Cubic Meter)")
  CTDData = CTDData %>%
    mutate(plankton_ID = paste0(first(Plankton$Set_Number), "/", last(Plankton$Set_Number)),
           ground = surv2,
           id = first(Plankton$CTD_ID),
           Date = first(SurveyData$StartDate),
           Lat = first(Plankton$CTD_Lat),
           Lon = first(Plankton$CTD_Lon),
           Year = first(year),
           Survey = first(surv.no))
  setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data"))
  CTDRaw = read_csv("CTD_Raw.csv")
  CTDData$Year = as.numeric(CTDData$Year)
  CTDData$Survey = as.numeric(CTDData$Survey)
  CTDTotal = full_join(CTDRaw, CTDData)
  CTDTotal %>% write_csv("CTD_Raw.csv")
  Plankton = Plankton %>%
    mutate(AvgTemp = mean(CTDData$Temperature),
           AvgSalinity = mean(CTDData$Salinity))
}

if(is.na(first(Plankton$CTD_ID))){
  Plankton = Plankton %>%
    mutate(AvgTemp = NA,
           AvgSalinity = NA)
}

#get Ruskin data
if(Tow == "Y"){
  TowData = read_excel(path = paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no, "/Ruskin.xlsx"), skip = 1, sheet = 'Data')
  TowData$DateTime = TowData$Time
  
#Changed to UTC  
  TowData$DateTime = TowData$DateTime-hours(3)
  TowData$Date = substr(TowData$DateTime,1,10)
  TowData$Time = substr(TowData$DateTime,12,19)
  TowData$Time = hms::as_hms(TowData$Time)

#Filter tow data based on Time1 + Time
Tow1 = TowData[TowData$Time >= first(Plankton$Time1) & TowData$Time <= first(Plankton$Time2),]
Tow2 = TowData[TowData$Time >= last(Plankton$Time1) & TowData$Time <= last(Plankton$Time2),]

#Calculate average and max tow depths for each
Tow1 = Tow1 %>%
  mutate(AvgTowDepth = mean(Depth),
         MaxTowDepth = max(Depth),
         Tow_No = 1)

Tow2 = Tow2 %>%
  mutate(AvgTowDepth = mean(Depth),
         MaxTowDepth = max(Depth),
         Tow_No = 2)

#ggplot the profiles for each tow
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no))

Tow1 %>%
  ggplot(aes(x = Time, y = Depth)) +
  geom_path(linewidth = 1, colour = "blue") +
  scale_y_reverse() +
  labs(y = "Depth (m)")
  ggsave("Tow 1.jpg")

Tow2 %>%
  ggplot(aes(x = Time, y = Depth)) +
  geom_path(linewidth = 1, colour = "blue") +
  scale_y_reverse() +
  labs(y = "Depth (m)")
  ggsave("Tow 2.jpg")

#combine the avg and max values with plankton sheet
TowTbl1 = tibble_row(Tow_No = 1, AvgTowDepth = first(Tow1$AvgTowDepth), MaxTowDepth = first(Tow1$MaxTowDepth))
TowTbl2 = tibble_row(Tow_No = 2, AvgTowDepth = first(Tow2$AvgTowDepth), MaxTowDepth = first(Tow2$MaxTowDepth))
TowCalcs = full_join(TowTbl1, TowTbl2)


Plankton = full_join(Plankton, TowCalcs, by = "Tow_No")

#convert lat/lon before combining
Plankton$Lon1 = as.numeric(conv_unit(Plankton$Lon1, "deg_dec_min", "dec_deg"))*-1
Plankton$Lon2 = as.numeric(conv_unit(Plankton$Lon2, "deg_dec_min", "dec_deg"))*-1
Plankton$Lat1 = as.numeric(conv_unit(Plankton$Lat1, "deg_dec_min", "dec_deg"))
Plankton$Lat2 = as.numeric(conv_unit(Plankton$Lat2, "deg_dec_min", "dec_deg"))

Plankton = Plankton %>%
  rename(id = Set_Number)

PlanData = read_csv((paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no, "/Plan Data.csv")))
PlanData$Survey.No = as.character(PlanData$Survey.No)

SurveyTotal = full_join(Plankton, PlanData)

setwd(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/Source Data/"))
Survey = read_csv("planktonsamplingData.csv")

SurveyTotal$TowTime = as.numeric(SurveyTotal$TowTime)

Total = full_join(Survey, SurveyTotal)

Total = Total %>%
  mutate(Day = as.numeric(substr(Date, 1, 2)),
         Month = as.numeric(substr(Date, 4, 5)),
         Year = as.numeric(substr(Date, 7, 10)),
         TowTime = difftime(Time2, Time1, units ="mins"),
         NoRevs = FlowReading2-FlowReading1,
         DistanceCalc = NoRevs*26873/1000000,
         Volume = DistanceCalc*3.1415)

Total = Total %>%
  dplyr::select(Year, Month, Day, Date, Ground, id, Survey.No, Fishing, Sample, 
                StartTime, Vessel.No, ExtraBox, EVessel, NVessel, PlanktonVessel, 
                Tow_No, Time1, Time2, TowTime, Lat1, Lon1, Lat2, Lon2, No_jars, 
                Speed, Heading, TideDirection, Swell, WindDirection, WindSpeed, 
                AirTemp, Observer, Net, Gear, TowType, FlowmeterType, FlowReading1, 
                FlowReading2, NoRevs, DistanceCalc, Volume, AvgTowDepth, MaxTowDepth, 
                DiscDepthD, DiscDepthA, CTD_ID, CTD_Lat, CTD_Lon, AvgTemp, AvgSalinity, 
                SurfaceTemp, WaterDepth1, WaterDepth2)

Total %>% write_csv("planktonsamplingData.csv")
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/"))
Total %>% write_csv("Survey Data.csv")
}

if(Tow == "N"){
  Survey = read_csv(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/Source Data/planktonsamplingData.csv"))
  Total = read_csv(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv"))
  PlanData = read_csv((paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no, "/Plan Data.csv")))
  PlanData$Survey.No = as.character(PlanData$Survey.No)
  Total = full_join(Total, PlanData)
  Survey = full_join(Survey, PlanData)
  #changed write_csv to write.csv
  Total = write.csv(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv"))
  Survey = write.csv(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/Source Data/planktonsamplingData.csv"))
  
  setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no))
  Depth=1
  Time=1
  Tow1 = tibble(Depth, Time)
  Tow1 %>%
    ggplot(aes(x = Time, y = Depth)) +
    scale_y_reverse() +
    labs(y = "Depth (m)")
    ggsave("Tow 1.jpg")
    ggsave("Tow 2.jpg")
}

##ECHOVIEW DATA##
#Land Data 
## added raster:: in front as it was throwing an error without it.

can<-raster::getData('GADM', download = FALSE, country="CAN", level=1, path = paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io"))
us = raster::getData('GADM', download = FALSE, country = "USA", level = 1, path = paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io"))
can1 = rbind(can,us)
NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec", "Maine"),]

# Proper coordinates for German Bank. Replaced gIntersection with crop
GBMap <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")
proj4string(GBMap) <- CRS(proj4string(NBNS))
GBout <- crop(NBNS, GBMap, byid=TRUE)

# Proper coordinates for Scots Bay. eplaced gIntersection with crop
SBMap <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
proj4string(SBMap) <- CRS(proj4string(NBNS))
SBout <- crop(NBNS, SBMap, byid=TRUE)

#Import All Boxes
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Box Coordinates/"))
boxes = read.csv("surveyBoxes.csv")
SBplankton=boxes[which(boxes$Box == "SBPlanktonBox"), ]
SBCTD=boxes[which(boxes$Box == "SBocean"), ]
GBCTD=boxes[which(boxes$Box == "GBocean"), ]
SUA = read.csv("polygon_GB.csv")
polyGB = as.PolySet(SUA, projection="LL")
SUA = read.csv("polygon_SI.csv")
polySI = as.PolySet(SUA, projection="LL")

setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no))
if(surv == "SB") {
  if(!is.na(PlanData$EVessel)){SUA = read.csv("polygon_SBEastern.csv")
   polyEastern = as.PolySet(SUA, projection="LL")}
  if(!is.na(PlanData$NVessel)){SUA = read.csv("polygon_SBNorthern.csv")
   polyNorthern = as.PolySet(SUA, projection="LL")}
   SUA = read.csv("polygon_SB.csv")
   polySB_main = as.PolySet(SUA, projection="LL")}

if(surv == "GB"){
  SUA = read.csv("polygon_GB.csv")
  polyGB = as.PolySet(SUA, projection="LL")
  SUA = read.csv("polygon_SI.csv")
  polySI = as.PolySet(SUA, projection="LL")}

#Load functions
pathnames <- list.files(pattern="[.]R$", path=paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Functions"), full.names=TRUE)
sapply(pathnames, FUN=source)

#Echoview Data
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no))
Map = list.files(path=paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no), pattern = "Map") %>% 
  map_df(~read_csv(.))
Region = list.files(path=paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no), pattern = "Region") %>% 
  map_df(~read_csv(.))

if(surv=="SB"){NVessel = ifelse(Survey$NVessel == "Lady Janice II", "LJ",
                 ifelse(Survey$NVessel == "Sealife II", "SL",
                        ifelse(Survey$NVessel == "Lady Melissa", "LM",
                               ifelse(Survey$NVessel == "Canada 100", "C1",
                                      ifelse(Survey$NVessel == "Fundy Monarch", "FM",
                                             ifelse(Survey$NVessel == "Brunswick Provider", "BP",
                                                    ifelse(Survey$NVessel == "Leroy and Barry", "LB",
                                                           ifelse(Survey$NVessel == "Morning Star", "MS",
                                                                  ifelse(Survey$NVessel == "Tasha Marie", "TM",
                                                                         NA)))))))))
EVessel = ifelse(Survey$EVessel == "Lady Janice II", "LJ",
                 ifelse(Survey$EVessel == "Sealife II", "SL",
                        ifelse(Survey$EVessel == "Lady Melissa", "LM",
                               ifelse(Survey$EVessel == "Canada 100", "C1",
                                      ifelse(Survey$EVessel == "Fundy Monarch", "FM",
                                             ifelse(Survey$EVessel == "Brunswick Provider", "BP",
                                                    ifelse(Survey$EVessel == "Leroy and Barry", "LB",
                                                           ifelse(Survey$EVessel == "Morning Star", "MS",
                                                                  ifelse(Survey$EVessel == "Tasha Marie", "TM",
                                                                         NA)))))))))
  out = SBout
  map = mapDat(x = Map)
  x = Region
  trans = transects(x= Region, TS38 = TS1, TS50 = NA)
  
#These IDs are specifically for adjusted surveys
  ### Region has start and end times within it.
  
  # ids = c("BP_T01","BP_T02", "BP_T03", "BP_T04")
  # northern = trans[which((trans$RegionName %in% ids)), ]
  # 
  # 
  # ids = c("BP_T01","BP_T02", "BP_T03", "BP_T04")
  # eastern = trans[which((trans$RegionName %in% ids)), ]
  # 
  # ids =c("C1_T01", "C1_T02", "LJ_T01", "LJ_T02","LM_T01", "LM_T02", "LB_T01", "LB_T02")
  # main = trans[which((trans$RegionName %in% ids)), ]
#To here
  
  ### Comment out chunk below if needed to adjust. 
  
  northern = trans[which((trans$Vessel == NVessel)), ]
  eastern = trans[which((trans$Vessel == EVessel)), ]
  main = trans[which((trans$Vessel %in% ids)), ]
  PRCplot=ggplot(map, aes(x=Xend, y=Yend)) + 
    geom_point(aes(colour = Vessel, size = PRC_ABC)) + 
    labs(x=NULL, y=NULL, title = "PRC Area Backscattering Coefficient (m2/m2) for each transect")

  #To here
  
  # Results
  resultsa = biomassCalc(x = main, areaKm = SB1)
  resultsb = biomassCalc(x = northern, areaKm = SB2)
  resultsc = biomassCalc(x = eastern, areaKm = SB3)
  
  tableA = resultTableA(x = main)
  tableB = resultTableB(x = main)
  tableC = resultTableC(x = resultsa)
  
  tableD = resultTableA(x = northern)
  tableE = resultTableB(x = northern)
  tableF = resultTableC(x = resultsb)
  
  tableG = resultTableA(x = eastern)
  tableH = resultTableB(x = eastern)
  tableI = resultTableC(x = resultsc)
  
  tableC$Layer = "Main Box"
  tableF$Layer = "Northern Box"
  tableI$Layer = "Eastern Box"
  
  A = rbind(tableA,tableD, tableG)
  B = rbind(tableB,tableE, tableH)
  C = rbind(tableC,tableF, tableI)
  
  write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
  write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
  write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
}

if(surv=="GB"){
  out=GBMap
  map = mapDat(x = Map)
  x = Region
  trans = transects(x= Region, TS38 = TS1 , TS50 = NA)
  
  ids = c("T01", "T02", "T03", "T04")
  trans1 = trans[which((trans$Transect_No %in% ids)), ]
  ids = c("T05", "T06", "T07")
  trans2 = trans[which((trans$Transect_No %in% ids)), ]
  
  x = surveyTrack2(x=trans1, polyNameA  = polyGB, polyNameB  = polySI, title = name )
  
  if(adhoc=="TRUE"){SUA = list.files(path=paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no), pattern = "adhoc") %>% 
    map_df(~read_csv(.))
  polyAD = as.PolySet(SUA, projection="LL")
  
  x = surveyTrack2(x=trans2, polyNameA  = polyGB, polyNameB  = polyAD, title = name )
  
  ggplot(trans2, aes(x=X, y=Y)) + geom_polygon(data=polyAD,aes(x=X, y=Y, group=PID), fill = "white", colour = "black")  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), linewidth = 1)  + labs(x=NULL, y=NULL) + coord_map()
  }
  
  ids = c("T01", "T02", "T03", "T04")
  map1 = map[which((map$Transect_No %in% ids)), ]
  
  PRCplot=ggplot(map1, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL, title = "PRC Area Backscattering Coefficient (m2/m2) for each transect")
  SI = trans[which(trans$Transect_No == c("T03", "T04")), ]
  ids = c("T01", "T02")
  GB = trans[which((trans$Transect_No %in% ids)), ]
  
  #Results
  resultsa = biomassCalc(x = GB, areaKm = GB1)
  resultsb = biomassCalc(x = SI, areaKm = GB2)
  resultsc = biomassCalc(x = trans2, areaKm = GB3)
  
  tableA = resultTableA(x = GB)
  tableB = resultTableB(x = GB)
  tableC = resultTableC(x = resultsa)
  
  tableD = resultTableA(x = SI)
  tableE = resultTableB(x = SI)
  tableF = resultTableC(x = resultsb)
  
  tableG = resultTableA(x = trans2)
  tableH = resultTableB(x = trans2)
  tableI = resultTableC(x = resultsc)
  
  tableC$Layer = "German Bank"
  tableF$Layer = "Seal Island"
  tableI$Layer = "School Survey"
  
  A = rbind(tableA,tableD, tableG)
  B = rbind(tableB,tableE, tableH)
  C = rbind(tableC,tableF, tableI)
  
  write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
  write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
  write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)}

setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no))
ggsave("PRCplot.jpg", height = 15, width = 15, units = "cm")

###Turnover Calc###
SSB = read_csv(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/Main Data/SSB Estimates.csv"))
SSB = SSB %>% filter(Year == year) %>% dplyr::select(Date = Survey_Date, Biomass = HSC_Estimate, Ground, HSC_Turnover_Adjusted)
SSB = distinct(SSB)

#SB and GB turnover calculation
if(surv == "GB"){
  y_intercept = GB_y
  x_Var_1 = GB_x_var
  daysturnover = GB_days
  SSBSI = SSB %>% filter(Ground == "Seal Island")
  SSB = SSB %>% filter(Ground == "German Bank")
}

if(surv == "SB"){
  y_intercept =  SB_y
  x_Var_1 = SB_x_var
  daysturnover = SB_days
  SSB = SSB %>% filter(Ground == "Scots Bay")
}

SSB$Date = as.Date(SSB$Date)
resultsa$Date = as.Date(substr(resultsa$Date_Time_S, 0, 10))
resultsa = resultsa %>% arrange(Date)
Date = first(resultsa$Date)

if(surv=="GB"){
  Biomass = sum(resultsa$trans_biomass)
  BiomassSI = sum(resultsb$trans_biomass)}

if(surv=="SB"){
  Biomass = sum(resultsa$trans_biomass, resultsb$trans_biomass, resultsc$trans_biomass, na.rm = TRUE)}

Current = tibble(Date, Biomass, surv2)
Current = Current %>% dplyr::select(Date, Biomass, Ground = surv2) %>% mutate(Current = "Y")
if(surv=="GB"){SealIsland = tibble(Year = as.integer(year), Date, BiomassSI, Ground = "Seal Island", Survey_Number = as.integer(surv.no))}
Surveys = full_join(Current, SSB)
Surveys = Surveys %>% arrange(Date)
Date = Surveys$Date
Survey = 1:length(Surveys$Date)
Biomass = Surveys$Biomass

if(surv.no > 1){
  TurnBio = turnoverBio(y_intercept, x_Var_1, daysturnover, Date, Survey, Biomass)
  Previous = Surveys %>% filter(is.na(Current))
  Previous = sum(Previous$Biomass)
  Turnover = TurnBio-Previous
  Current$Turnover = Turnover
  
#Add it to Table C
C$Turnover = Turnover
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no))
write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)}

#Update SSB Estimates
SSB = read_csv(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/Main Data/SSB Estimates.csv"))
SSB$Survey_Date = as.Date(SSB$Survey_Date)
Current$Year = as.integer(year)
Current$Survey_Number = as.integer(surv.no)
   if(surv.no > 1){Current = Current %>% dplyr::select(Year, Ground, Survey_Number, Survey_Date = Date, HSC_Estimate = Biomass, HSC_Turnover_Adjusted = Turnover)}
   if(surv.no == 1){Current = Current %>% dplyr::select(Year, Ground, Survey_Number, Survey_Date = Date, HSC_Estimate = Biomass)}
   if(surv=="GB"){SealIsland = SealIsland %>% dplyr::select(Year, Ground, Survey_Number, Survey_Date = Date, HSC_Estimate = BiomassSI)}
SSB = full_join(SSB, Current)
if(surv=="GB"){SSB = full_join(SSB, SealIsland)}
SSB = SSB %>% arrange(Year)
SSB %>% write_csv(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/Main Data/SSB Estimates.csv"))

#Not adding time into Date.Time.Start etc. Just pulling the date. #Went into transform function to keep the time.
###Performance data import and filtering###
A = read_csv("tableA.csv")
actual = A
actual = actual %>% mutate(Type = "Actual")
plan = list.files(pattern = "*plan.csv") %>% map_df(~read_csv(.))
plan = plan %>% mutate(Type = "Plan")
wd = getwd()
Perform = full_join(actual, plan) %>% mutate(Survey = surv.no) %>% mutate(Location = surv)
Perform = Perform %>% rename(End.Lat="End Lat", End.Lon="End Lon", Start.Lat="Start Lat", Start.Lon="Start Lon", Dist..km.="Dist (km)", Date.Time.Start="Date Time Start", Date.Time.End="Date Time End", Transect.No.="Transect No.")

#why is distance being divided by 1000? These two lines don't seem to make much of a difference, as it changes the distance by around 2km then back to the original km.
Perform = Perform %>% mutate(Distance = distHaversine(cbind(Start.Lon,Start.Lat), cbind(End.Lon,End.Lat))) %>% mutate(Distance = Distance/1000)
Perform = Perform %>% mutate(Distance = ifelse(is.na(Dist..km.), Distance, Dist..km.))

#calculate time/speed
Perform<-Perform %>% mutate(Start=as.POSIXct(Date.Time.Start, origin = "1970-01-01")) %>% 
mutate(End=as.POSIXct(Date.Time.End, origin = "1970-01-01")) %>%
#Duration in seconds. #Removed the /60 as this was causing the speed to be much smaller than it should be. This looks closer to what it should be.
   mutate(Duration = as.numeric(End-Start)*60) %>%
   mutate(Speed = (((Distance*1000)/(Duration))) /60)
Perform<-Perform %>% mutate(Speed = Speed*1.94384) #convert from m/s to knots
Perform<-Perform %>% mutate(Year = as.numeric(substr(Start, 1, 4)))
Perform<-Perform %>% mutate(Date = date(Start)) 

#summarize speed by Transect
Speed<-Perform %>%
  dplyr::group_by(Vessel, Transect.No.) %>%
  dplyr::summarize(Speed = mean(Speed, na.rm = TRUE)) %>%
  filter(!Speed == "NaN") %>%
  mutate(Transect = as.factor(Transect.No.))

#group by survey + vessel + type, summarize distance
Distance<-Perform %>% 
  dplyr::group_by(Vessel, Type) %>%
  dplyr::summarize(Distance = sum(Distance)) %>%
  spread(Type, Distance, fill = 0) %>%
  transmute(Vessel, Difference = Actual-Plan)

Speed %>% write_csv("Speed.csv")
Distance %>% write_csv("Distance.csv")
Perform %>% write_csv("Performance Total.csv")

##CTD Data import and filtering
CTD <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/CTD_Raw.csv"))
CTD <- CTD %>% dplyr::select(-Pressure, -Conductivity, -Specific_conductance, -Sound_velocity, -Density, -plankton_ID)
CTD$Date = ymd(CTD$Date)
CTD <- CTD %>% mutate(Julian = yday(Date)) #add Julian day
CTD <- CTD %>% rename(Ground = ground, ID = id)
Bio <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Biomass.csv"))
CTD <- left_join(CTD, Bio, by = "Date")
CTD <- CTD %>% mutate(Month = as.numeric(substr(Date, 6, 7))) %>% dplyr::select(-Location) #may need to remove other columns
CTD$Month <- as.factor(CTD$Month)
CTD$Year <- as.factor(CTD$Year)

#imports daily ECCC historical data for GB=Yarmouth=50133, SB=Greenwood=6354
ECCC = weather_dl(station_ids = c(50133, 6354), start = "2017-01-01", interval = "day")
ECCC = ECCC %>% 
  dplyr::select(station_name, date, mean_temp, total_precip, total_snow, total_rain, spd_max_gust, 
                min_temp, max_temp, heat_deg_days, cool_deg_days) %>%
  rename(Date = date) %>%
  mutate(Ground = ifelse(station_name == "GREENWOOD A", "Scots Bay", "German Bank"))

#Combine with ECCC data, need to make Scots Bay = Greenwod, German Bank = Yarmouth
CTD = left_join(CTD, ECCC, by = c("Date", "Ground"))

#Cast in or out of box factor
CTD = CTD %>% 
  mutate(In_Box = ifelse(Ground == "Scots Bay" & between(Lat, 45.03, 45.08) & between(Lon, -65.3, -65.1), "1",
                         ifelse(Ground == "German Bank" & between(Lat, 43.50, 43.60) & between(Lon, -66.4, -66.3), "1", "0")))

CTD$In_Box = as.factor(CTD$In_Box)

#SST
SST = CTD %>% 
  filter(between(Depth, 0, 5)) %>%
  filter(grepl('German Bank|Scots Bay', Ground)) %>%
  group_by(Ground, Date, Year, Julian, Month, Survey, In_Box) %>%
  summarize(TempSD = sd(Temperature),
            Temperature = mean(Temperature),
            Biomass = mean(Biomass),
            logTemp = log(Temperature),
            Lat = mean(Lat),
            Lon = mean(Lon),
            logBiomass = log(Biomass),
            SalinitySD = sd(Salinity),
            Salinity = mean(Salinity),
            mean_temp = mean(mean_temp),
            total_precip = mean(total_precip),
            total_snow = mean(total_precip),
            total_rain = mean(total_rain),
            spd_max_gust = max(spd_max_gust),
            min_temp = min(min_temp),
            max_temp = max(max_temp),
            heat_deg_days = mean(heat_deg_days),
            cool_deg_days = mean(cool_deg_days))

SST = SST %>%
  group_by(Year, Month, Ground) %>%
  mutate(Count = length(Temperature))

#At-depth
CTD30 = CTD %>% 
  filter(between(Depth, 28, 32)) %>%
  filter(grepl('German Bank|Scots Bay', Ground)) %>%
  group_by(Ground, Date, Year, Julian, Month, Survey, In_Box) %>%
  summarize(TempSD = sd(Temperature),
            Temperature = mean(Temperature),
            Biomass = mean(Biomass),
            logTemp = log(Temperature),
            Lat = mean(Lat),
            Lon = mean(Lon),
            logBiomass = log(Biomass),
            SalinitySD = sd(Salinity),
            Salinity = mean(Salinity),
            mean_temp = mean(mean_temp),
            total_precip = mean(total_precip),
            total_snow = mean(total_precip),
            total_rain = mean(total_rain),
            spd_max_gust = max(spd_max_gust),
            min_temp = min(min_temp),
            max_temp = max(max_temp),
            heat_deg_days = mean(heat_deg_days),
            cool_deg_days = mean(cool_deg_days))

CTD30 = CTD30 %>%
  group_by(Year, Month, Ground) %>%
  mutate(Count = length(Temperature))

#Adding Stratification
#Take SST temps and salinity and add it to 30m dataframe (30m will always have less), mutate stratified = 30m-1m

SSTTemp = SST %>% ungroup() %>% dplyr::select(Date, Temperature, Salinity)
Strat = left_join(CTD30, SSTTemp, by = "Date")
Strat = Strat %>% rename(Temperature = Temperature.x, SST = Temperature.y, Salinity = Salinity.x, SurfaceSalinity = Salinity.y)
Strat = Strat %>% mutate(StratTemp = SST-Temperature) %>% mutate(StratSalt = Salinity-SurfaceSalinity)
Strat = Strat %>%   
  group_by(Ground, Date, Year, Julian, Month, Survey, In_Box) %>%
  summarize(TempSD = sd(Temperature),
            Temperature = mean(Temperature),
            Biomass = mean(Biomass),
            logTemp = log(Temperature),
            Lat = mean(Lat),
            Lon = mean(Lon),
            logBiomass = log(Biomass),
            SalinitySD = sd(Salinity),
            Salinity = mean(Salinity),
            mean_temp = mean(mean_temp),
            total_precip = mean(total_precip),
            total_snow = mean(total_precip),
            total_rain = mean(total_rain),
            spd_max_gust = max(spd_max_gust),
            min_temp = min(min_temp),
            max_temp = max(max_temp),
            heat_deg_days = mean(heat_deg_days),
            cool_deg_days = mean(cool_deg_days),
            SST = mean(SST),
            SurfaceSalinity = mean(SurfaceSalinity),
            StratTemp = mean(StratTemp),
            StratSalt = mean(StratSalt))

CTD30 = Strat

CTD %>% write_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/CTD Full.csv"))
CTD30 %>% write_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/CTD 30m.csv"))
SST %>% write_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/CTD SST.csv"))

#Larval Data

larv = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/Larval Measurements.csv"))
arc = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/ARC Data.csv"))
arc = arc %>% dplyr::select(id, Larvae_Count, Notes)
survey = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv"))
survey = survey %>% mutate(Ground = substr(id,1,2))

larv = left_join(larv, arc, by="id")
larv = left_join(larv, survey)
larv = larv %>% dplyr::select(Ground, id, Date, Survey.No, No_jars, Lengthmm, Condition, Yolk_sac, Yolk_sac_length, Preservative, ARC_Count=Larvae_Count, ARC_Notes=Notes, Lon1, Lat1, Lon2, Lat2, TowTime, AvgTowDepth, MaxTowDepth, CTDAvgTemp=AvgTemp, CTDAvgSalinity=AvgSalinity, Volume, Month, Year, Day)
larv$Date = dmy(larv$Date)
larv$Survey.No = as.factor(larv$Survey.No)
larv$Year = as.factor(larv$Year)
larv$category =  with(larv, ifelse(larv$Lengthmm < 8 , 1, 
                                   ifelse(larv$Lengthmm < 13 & larv$Lengthmm >= 8, 2, 
                                          ifelse(larv$Lengthmm >= 13  & larv$Lengthmm < 18, 3, 
                                                 ifelse(larv$Lengthmm > 17 & larv$Lengthmm <= 27, 4, 5)))))
larv$category = as.factor(larv$category)
larv$hatchDate = larv$Date - 10 #incubation duration of 10 days
larv$hatchDate = ymd(larv$hatchDate)

#Calculating spawn dates
larv=larv %>% mutate(MAXspawnDate = ifelse(category == 1, hatchDate-14,
                                           ifelse(category == 2, hatchDate-35,
                                                  ifelse(category == 3, hatchDate-56,
                                                         ifelse(category == 4, hatchDate-98,
                                                                ifelse(category == 5, hatchDate-99, "NA"))))))
larv$MAXspawnDate=as.numeric(larv$MAXspawnDate)
larv$MAXspawnDate=as.Date(larv$MAXspawnDate, origin = "1970-01-01")

larv=larv %>% mutate(MINspawnDate = ifelse(category == 1, hatchDate,
                                           ifelse(category == 2, hatchDate-14,
                                                  ifelse(category == 3, hatchDate-35,
                                                         ifelse(category == 4, hatchDate-56,
                                                                ifelse(category == 5, hatchDate-98, "NA"))))))
larv$MINspawnDate=as.numeric(larv$MINspawnDate)
larv$MINspawnDate=as.Date(larv$MINspawnDate, origin = "1970-01-01")

#add Julian
larv<-larv %>% mutate(Julian = yday(Date),
                      JulianMin = yday(MINspawnDate),
                      JulianMax = yday(MAXspawnDate))

#Calculating SE/mean/min/max of larval measurements.

# Changed group_by to (id, Year, Date) from group_by(Survey.No, Year, Date) to fix abundance to count larvae within the tow, vs abundance of larvae from the survey.

larv <- larv %>%
  group_by(id, Year, Date) %>%
  mutate(SD = sd(Lengthmm), 
         MinLength = min(Lengthmm), 
         MaxLength = max(Lengthmm), 
         MeanLength = mean(Lengthmm), 
         Abundance = length(Lengthmm),
         X = ((Lon1 + Lon2)/2),
         Y = ((Lat1 + Lat2)/2)) %>%
  ungroup()

larvsummary <- larv %>% group_by(Ground, id, Year) %>%
  summarize(MinLength = mean(MinLength, na.rm = TRUE), 
            MaxLength = mean(MaxLength, na.rm = TRUE), 
            MeanLength = mean(MeanLength, na.rm = TRUE),
            SD = mean(SD, na.rm = TRUE),
            Abundance = length(Lengthmm)) %>%
  mutate(SE = SD/sqrt(Abundance))
surveysummary = survey %>% dplyr::select(Ground, Survey.No, Year) %>% group_by(Ground, Survey.No, Year) %>% summarize(Survey.No = mean(Survey.No), Year = mean(Year))
surveysummary$Year = as.factor(surveysummary$Year)
surveysummary$Survey.No = as.factor(surveysummary$Survey.No)
larvsummary = left_join(surveysummary, larvsummary)
larvsummary %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/Larval Summary Table.csv"))

larv = larv %>%
  mutate(Larv_per_jar = Abundance/No_jars) %>%
  mutate(Volume = ifelse(Volume < 0.01, NA, Volume)) %>%
  mutate(Density = Larv_per_jar/Volume)

larv = larv %>% group_by(Year) %>%
  mutate(DayDiff = last(Julian)-Julian) %>%
  mutate(LastLength = ifelse(DayDiff == 0, Lengthmm, ((DayDiff*0.24)+Lengthmm))) 

#join CTD + larval data, filter temp and salinity to where depth = avg tow depth +/- 0.25m
CTDLarval = CTD %>%
  dplyr::select(Depth, Temperature, Salinity, Ground, Year, Survey.No=Survey) %>%
  mutate(Ground = ifelse(Ground == "Scots Bay", "SB", "GB"),
         Survey.No = as.factor(Survey.No)) %>%
  left_join(larv, by=c("Ground", "Year", "Survey.No")) %>%
  group_by(Year, Survey.No, Ground, id) %>%
  summarize(Depth, Temperature, Salinity, AvgTowDepth) %>%
  group_by(Year, Survey.No, Ground, id) %>%
  filter(between(Depth, min(AvgTowDepth-0.25), min(AvgTowDepth+0.25))) %>%
  summarize(CTDTemp = mean(Temperature),
            CTDSalinity = mean(Salinity))

larv = left_join(larv,CTDLarval)

larv = larv %>%
  dplyr::select(Ground, id, Date, Survey.No, No_jars, Abundance, Lengthmm, category, MinLength, MaxLength, MeanLength, SD, Abundance, Larv_per_jar, Density, hatchDate, MINspawnDate, MAXspawnDate, Julian, JulianMin, JulianMax, LastLength, Day, Month, Year, Condition, Yolk_sac, Preservative, ARC_Count, ARC_Notes, X, Y, TowTime, AvgTowDepth, MaxTowDepth, CTDTemp, CTDSalinity, Volume)
larv %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/Full Larval.csv"))

