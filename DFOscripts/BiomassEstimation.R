#### biomass calculation from transects exported from Echoview into excel
#Integrations must be placed in same folder and seperated (e.g. GB and SI)

rm(list=ls())

library(dplyr)
library(geosphere)
library(sp)
library(rgdal)
library(raster)
library(adehabitatHR)
library(plotKML)
library(lubridate)

# Change this to the appropiate survey folder:
setwd('C:/Users/herri/OneDrive/Documents/Surveys/2020/2020-06-27/Exports/')


# Define the TS values to be used

TS38 <- -35.5 ###### Code directly to Length Frequences EVENTUALLY ##########
TS50 <- -35.609 ###### Code directly to Length Frequences EVENTUALLY ##########


file_list <- list.files(path = getwd())
transects <- data.frame()


#identify columns to keep from csv files
keep <- c("Dist_S","Dist_E", "Date_S", "Time_S", "Date_E","Time_E", "Lat_S","Lon_S","Lat_E","Lon_E", "Region_name", "Ping_S", "Ping_E", "EV_filename","Area_Backscatter_Strength", "Frequency")


#for each file in file_list, read in csv, keep specified columns, rbind to dataset to produce one dataset
for (i in 1:length(file_list)){
  temp_data <- read.csv(file_list[i])
  temp_data2 <- temp_data[keep]
  transects <- rbind(transects, temp_data2)
}


# Apply my transects function

transects
      
    # Create unique trans name
        transects$Survey_date = substring(transects$EV_filename, 48,57)
        transects$Survey_date
        
        transects$Region_name = as.character(paste(transects$Survey_date, transects$Region_name, sep = ""))
        transects$Region_name

            # Change to numeric
              transects$Ping_S = as.numeric(transects$Ping_S)
              transects$Ping_E = as.numeric(transects$Ping_E)

                  # Change to time
                  # First Start Date/Time
                  
                  # Changed format with Echoview v11
                    t = parse_date_time(transects$Time_S, orders="HMS", tz = "America/Moncton")
                    y = parse_date_time(transects$Date_S, orders= "ymd", tz = "America/Moncton")
                    w = substring(as.character (t), 12,50)
                    z = paste(y, w)
                    z = as.POSIXct(z)
                    transects$Date_Time_S  = z
                    transects$Date_S = NULL
                    transects$Time_S = NULL

                          # Now End Date/Time
                            t = parse_date_time(transects$Time_E, orders="HMS", tz = "America/Moncton")
                            y = parse_date_time(transects$Date_E, orders= "ymd", tz = "America/Moncton")
                            w  = substring(as.character (t), 12,50)
                            z = paste(y, w)
                            z = as.POSIXct(z)
                            transects$Date_Time_E  = z
                            transects$Date_E = NULL
                            transects$Time_E = NULL

                            head(transects)
                            
                            # Year
                              transects$Year = substring(transects$Survey_date, 1,4)  

                            # Transect_No
                              transects$Transect_No = substring(transects$Region_name, 14,16)

                            # Create a total distance variable (Dist_T)
                              transects$Dist_T = (transects$Dist_E - transects$Dist_S)/1000

                                transects$Y = transects$Lat_S
                                transects$Yend = transects$Lat_E
                                transects$X = transects$Lon_S
                                transects$Xend = transects$Lon_E
                                
                                          transects$Lat_S = NULL
                                          transects$Lat_E = NULL
                                          transects$Lon_S = NULL
                                          transects$Lon_E = NULL
                                          
                                          head(transects)
                                          
                                transects$Vessel = substring(transects$EV_filename, 59,60)  
                                transects$Vessel
                                
                                transects$linearBackscatter = 10^(transects$Area_Backscatter_Strength/10)
                              # Appy target strength                              
                                transects$TS<-ifelse((transects$Frequency>38), TS50, TS38)  
                                transects$biomass_density = 10^((transects$Area_Backscatter_Strength-(transects$TS))/10)
                                
head(transects)                                
                                
                                
### Finished function




# Scots Bay Differentiation:

# For Scots Bay we need some code to be able to identify transects in the eastern, northern and main survey box. Can use distance (Dist_T), and then longitude (Xend).


  # Use Cut off of 25m
    transects$Dist_T
    # Main
      main = transects[which((transects$Dist_T > 25)), ] 
        # Subs
            subs = transects[which((transects$Dist_T < 25)), ] 
            

              # East
                  east = subs[which((subs$Xend > -64.8)), ] 
              # North
                  north = subs[which((subs$Xend < -64.8)), ] 
                  
      
                  
                  
                  # results in three boxes; main, east and north
                      main
                      north
                      # BP
                      east                  
                                                                          
                   
# ALLAN'sGerman Bank Differentiation:                  
                  
# going to develop my own based on transects distance.                  


                      
                      transects_German <- filter(transects, Lat_S <43.67 & Lat_E < 43.67 & Lon_S < -66.259 & Lon_E < -66.259) #need to check whether this is accurate
                      transects_SealIsland <-  filter(transects, !Region_name %in% as.character(transects_German$Region_name))
                      
                      transects_German<- rbind(transects_German, transects_SealIsland[7,])
                      transects_German
                      
                      transects_SealIsland<-transects_SealIsland[1:6,]


rm(temp_data,temp_data2)


# Doesn't seem right...and doesn't work for smaller polygons. Will stick with my manual methods for now.
                
                #calculate area of survey
                area_calc = function(x) {
                          Longitude  <-c(x$X,x$Xend)
                          Latitude  <-c(x$Y, x$Yend)
                            LatLonMat <-matrix(c(x$X,x$Y),ncol=2)
                              survey_coord <-SpatialPoints(LatLonMat)
                                crs.geo <- CRS("+proj=utm +zone=20 +datum=WGS84")
                
                                      proj4string(survey_coord) <- crs.geo
                                        is.projected(survey_coord)
                                            summary(survey_coord)
                
                                                survey_area <- mcp(survey_coord, percent = 100)
                                                    area<-survey_area$area*100000000 #Converts to km squared. originally in Hectares, but not sure why it's million too small.
                return(area)
                }
                
                                                    area<-area_calc(main)
                                                    area
                                                    str(main)
                
                                                    area=area_calc(north)
                                                    area
                                                    
                                                    
# Jenna's Mapping to be able to calculate Survey Area

library(ggplot2) 
library(rgeos)   
library(PBSmapping)                                                    

      # Load Base Data
          can<-getData('GADM', country="CAN", level=1) # provinces
          NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]
                                                    
          # Proper coordinates for Scots Bay
          CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
            proj4string(CP) <- CRS(proj4string(NBNS))
                out <- gIntersection(NBNS, CP, byid=TRUE)
                
                getwd()
                # Load Polygons
                setwd('C:/Users/herri/OneDrive/Documents/Jenna/workspace/')
                
                    SUA = read.csv("polygon_SB5.csv")
                    polySB_main = as.PolySet(SUA, projection="LL")
                    #ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))
                
                        SUA = read.csv("polygon_SBNorthern27.csv")
                        polySB_northern = as.PolySet(SUA, projection="LL")
                        
                        ggplot(transects, aes(x=X, y=Y)) + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID), fill = "white", colour = "black")+ geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black') + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)+ labs(x=NULL, y=NULL) + coord_map() + theme_dark() 
                        
                        area = calcArea(polySB_main) 
                        area - 1.61
                        
                        calcArea(polySB_northern)
                        

                                                    
biomassCalc = function(x, area) {
  #x = transects_German
   x$areaKm = area
  
      # Analysis
      x$distance = (x$Dist_E - x$Dist_S)/1000
      x$sum_trans =sum(x$distance)
      x$Actual_Weighting = (x$distance)/(x$sum_trans)
      x$calc_actual_mean_sa = 10^(x$Area_Backscatter_Strength/10)*x$Actual_Weighting
      x$TS <- as.numeric(ifelse(grepl(38, x$Frequency), TS38, TS50))
      x$biomass_density = 10^((x$Area_Backscatter_Strength-(x$TS))/10) # same as Jenna's Transects.R script calculation
                  x$density = (x$biomass_density)*(x$distance)
                  x$weighted_mean_biomass_calc = (x$biomass_density) * (x$Actual_Weighting)
                  x$trans_biomass = (x$weighted_mean_biomass_calc) * x$areaKm * 1000
                  x$total_biomass = sum(x$trans_biomass)
                  
                  se <- function(x) sqrt(var(x)/length(x))
                  x$se = se(x$biomass_density)
                  x$standard_error_tonnes = x$se * x$areaKm * 1000
                  x$standard_error_perc = x$standard_error_tonnes/x$total_biomass*100
                  x$mean_biomass_density = mean(x$biomass_density)
                  x$meanSa = 10*log10(sum(x$calc_actual_mean_sa))
                  
                  return(x)
                }

biomassCalc(main,640.80)
biomassCalc(north,83.27)

#Seal Island
area<-area_calc(transects_SealIsland)
area
biomassCalc(transects_SealIsland,area)
