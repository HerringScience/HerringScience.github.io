# remove everything in the workspace
rm(list = ls())

# IMPORTANT : SET GROUND, YEAR, AND SURVEY # HERE
surv="SB"
surv2="Scots Bay"
year="2022"
surv.no="6"
adhoc = "FALSE" #true or false if an adhoc survey was completed (and "adhoc.csv" exists)

#Set vessels for SB only
ids = c("LM","LB", "MS", "SL", "C1", "BP")
NorthVessel = "NA" #set NA if none
EastVessel = "NA" #set NA if none

#Area and TS values
SB1= 640 #SB main area
SB2= 77 #SB north area
SB3= 115 #SB east area

TS1 = -35.5 #TS38

GB1 = 805 #GB main area
GB2 = 267 #Seal Island area
GB3 = NA #Ad-hoc school survey area

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
library(rgeos)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)

##Tagging Data
#Data import and filtering
Tag <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/TaggingEvents.csv"))
Tag$Date = ymd(Tag$Date) 
Tag <- Tag %>% mutate(Julian = yday(Date)) %>% mutate(Year = as.numeric(substr(Date, 1, 4))) %>% dplyr::select(-Tag_Annual)

#Add tags per year per tagger
Tag_Annual = Tag %>%
  group_by(Tagger) %>%
  mutate(count = n_distinct(Year)) %>%
  summarize(n=n(), count2 = mean(count)) %>%
  mutate(Tag_Annual = n/count2) %>%
  dplyr::select(-n, -count2)

Tag = left_join(Tag, Tag_Annual, by = "Tagger")

#Adding Ground
Tag <- Tag %>% 
  mutate(Ground = ifelse(between(Lat, 45.02, 45.4) & between(Lon, -65.5, -64.5), "Scots Bay", 
                         ifelse(between(Lat, 43.15, 43.7) & between(Lon, -66.75, -66.05), "German Bank", 'Other')))

#Change any 'Sealife' to 'Sealife II', and other corrections
Tag$Vessel = as.factor(Tag$Vessel)
summary(Tag$Vessel) #spot any issues
Tag$Vessel[which(Tag$Vessel=="Sealife")] <- "Sealife II"
Tag$Vessel[which(Tag$Vessel=="Lady Meliss")] <- "Lady Melissa"
Tag$Vessel[which(Tag$Vessel=="Lady Janice II")] <- "Lady Janice"
summary(Tag$Vessel) #double check

Tag %>% write_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/TaggingEvents.csv"))

##ECHOVIEW DATA##

#Land Data
can<-getData('GADM', download = FALSE, country="CAN", level=1, path = "C:/Users/herri/Documents/GitHub/HerringScience.github.io")
us = getData('GADM', download = FALSE, country = "USA", level = 1, path = "C:/Users/herri/Documents/GitHub/HerringScience.github.io")
can1 = rbind(can,us)
NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec", "Maine"),]

# Proper coordinates for German Bank
GBMap <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")
proj4string(GBMap) <- CRS(proj4string(NBNS))
GBout <- gIntersection(NBNS, GBMap, byid=TRUE)

# Proper coordinates for Scots Bay
SBMap <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
proj4string(SBMap) <- CRS(proj4string(NBNS))
SBout <- gIntersection(NBNS, SBMap, byid=TRUE)

#Import All Boxes
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Box Coordinates/"))
boxes = read.csv("surveyBoxes.csv")
SBplankton=boxes[which(boxes$Box == "SBPlanktonBox"), ]
SBCTD=boxes[which(boxes$Box == "SBocean"), ]
SUA = read.csv("polygon_SBEastern.csv")
polyEastern = as.PolySet(SUA, projection="LL")
SUA = read.csv("polygon_SBNorthern.csv")
polyNorthern = as.PolySet(SUA, projection="LL")
SUA = read.csv("polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")
GBCTD=boxes[which(boxes$Box == "GBocean"), ]
SUA = read.csv("polygon_GB.csv")
polyGB = as.PolySet(SUA, projection="LL")
SUA = read.csv("polygon_SI.csv")
polySI = as.PolySet(SUA, projection="LL")

#Load functions
pathnames <- list.files(pattern="[.]R$", path=paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Functions"), full.names=TRUE)
sapply(pathnames, FUN=source)

#Echoview Data
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no))
Map = list.files(path=paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no), pattern = "Map") %>% 
  map_df(~read_csv(.))
Region = list.files(path=paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no), pattern = "Region") %>% 
  map_df(~read_csv(.))

if(surv == "SB"){
  out = SBout
  map = mapDat(x = Map)
  x = Region
  trans = transects(x= Region, TS38 = TS1, TS50 = NA)
  x = surveyTrack3(x=trans, polyNameA  = polySB_main, polyNameB  = polyNorthern,  polyNameC  = polyEastern,  title = name)
  northern = trans[which((trans$Vessel == NorthVessel)), ]
  eastern = trans[which((trans$Vessel == EastVessel)), ]
  main = trans[which((trans$Vessel %in% ids)), ]
  ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)
  
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
  
  ids = c("T01", "T02", "T03")
  trans1 = trans[which((trans$Transect_No %in% ids)), ]
  ids = c("T04", "T05", "T06", "T07")
  trans2 = trans[which((trans$Transect_No %in% ids)), ]
  
  x = surveyTrack2(x=trans1, polyNameA  = polyGB, polyNameB  = polySI, title = name )
  
  if(adhoc=="TRUE"){SUA = list.files(path=paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no), pattern = "adhoc") %>% 
    map_df(~read_csv(.))
  polyAD = as.PolySet(SUA, projection="LL")
  
  x = surveyTrack2(x=trans2, polyNameA  = polyGB, polyNameB  = polyAD, title = name )
  
  ggplot(trans2, aes(x=X, y=Y)) + geom_polygon(data=polyAD,aes(x=X, y=Y, group=PID), fill = "white", colour = "black")  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map()
  }
  
  ids = c("T01", "T02", "T03")
  map1 = map[which((map$Transect_No %in% ids)), ]
  
  ggplot(map1, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)
  
  SI = trans[which(trans$Transect_No == "T03"), ]
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

###Turnover Calc###

#Find date of last survey, calculate time between surveys for turnover
Survey <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv"))
surv.no2 = as.numeric(surv.no)
day1 = Survey %>% filter(Ground == surv & Survey.No == (surv.no2) & Year == year) %>% dplyr::select(Date)
day1 = dmy(unique(day1$Date))
day2 = Survey %>% filter(Ground == surv & Survey.No == (surv.no2-1) & Year == year) %>% dplyr::select(Date)
day2 = dmy(unique(day2$Date))
daysturnover = as.numeric(day1-day2)

#SB and GB turnover calculation
if(surv == "GB"){
  y_intercept <- 0.199392662629964
  x_Var_1 <-0.528381832773883
  Bio = C %>% filter(Layer == "German Bank")
}
if(surv == "SB"){
  y_intercept <- 0.364102758434224
  x_Var_1 <-0.436969270679439
  Bio = C %>% filter(Layer == "Scots Bay" | Layer == "Main Box" | Layer == "Eastern Box" | Layer == "Northern Box")
}

resultsa$Date <-
  as.Date(substr(resultsa$Date_Time_S, 0, 10))
Date <- resultsa$Date
Survey <- 1:length(resultsa$Date)
Biomass <- resultsa$trans_biomass

TurnBio = turnoverBio(y_intercept, x_Var_1, daysturnover, Date, Survey, Biomass)

#Add it to Table C
C$Turnover = TurnBio
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no))
write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)

###Performance data import and filtering###
actual = tableA
actual = actual %>% mutate(Type = "Actual")
plan = list.files(pattern = "*plan.csv") %>% map_df(~read_csv(.))
plan = plan %>% mutate(Type = "Plan")
wd = getwd()
Perform = full_join(actual, plan) %>% mutate(Survey = surv.no) %>% mutate(Location = surv)
Perform = Perform %>% rename(End.Lat="End Lat", End.Lon="End Lon", Start.Lat="Start Lat", Start.Lon="Start Lon", Dist..km.="Dist (km)", Date.Time.Start="Date Time Start", Date.Time.End="Date Time End", Transect.No.="Transect No.")
Perform = Perform %>% mutate(Distance = distHaversine(cbind(Start.Lon,Start.Lat), cbind(End.Lon,End.Lat))) %>% mutate(Distance = Distance/1000)
Perform = Perform %>% mutate(Distance = ifelse(is.na(Dist..km.), Distance, Dist..km.))

#calculate time/speed
Perform<-Perform %>% mutate(Start=as.POSIXct(Date.Time.Start, origin = "1970-01-01")) %>% 
  mutate(End=as.POSIXct(Date.Time.End, origin = "1970-01-01")) %>%
  mutate(Duration = as.numeric(End-Start)*60) %>%
  mutate(Speed = (Distance*1000)/(Duration*60))
Perform<-Perform %>% mutate(Speed = Speed*1.94384) #convert from m/s to knots
Perform<-Perform %>% mutate(Year = as.numeric(substr(Start, 1, 4)))
Perform<-Perform %>% mutate(Date = date(Start)) 

#summarize speed by Transect 1 + 2 (remove any box runners)
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

##Survey Data import and filtering
setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/")
Survey = read_csv("planktonsamplingData.csv")
Survey = Survey %>% dplyr::select(Ground, id, Survey.No, Date, StartTime, Sample, Vessel.No, ExtraBox, EVessel, NVessel, PlanktonVessel, No_jars, Lon1, Lat1, Lon2, Lat2, Time1, Time2, TowTime, AirTemp, Speed, Heading, TideDirection, AvgTowDepth, MaxTowDepth, CTD_ID, AvgTemp, AvgSalinity, WindDirection, WindSpeed, Swell, FlowReading1, FlowReading2, NoRevs, DistanceCalc, Volume, DepthDiscD, DepthDiscA) 
Survey = Survey %>%
  mutate(Month = as.numeric(substr(Date, 1, 2)),
         Year = as.numeric(substr(Date, 7, 10)),
         Day = as.numeric(substr(Date, 4, 5)))

setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/"))
Survey %>% write_csv("Survey Data.csv")

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

##Larval Data
Larval <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/Full Larval.csv"))
Survey <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv"))

Survey = Survey %>% 
  dplyr::select(No_jars, id, Volume) %>%
  mutate(No_jars = ifelse(is.na(No_jars), 1, No_jars))

Larval = left_join(Larval, Survey)
Larval = Larval %>%
  mutate(Larv_per_jar = Abundance/No_jars) %>%
  mutate(VolumeSampledLitres = Volume) %>%
  dplyr::select(-Volume) %>%
  mutate(VolumeSampledLitres = ifelse(VolumeSampledLitres < 0, NA, VolumeSampledLitres)) %>%
  mutate(Density = Larv_per_jar/VolumeSampledLitres)
