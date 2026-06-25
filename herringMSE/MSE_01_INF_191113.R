######################################################################################################################
#  Description: Spreadsheet with Sample IDs and associated data for the Quota Area. Plot of Quota Area
#  Created by: Tim Barrett
#  Created on: Oct, 2019, edited and commented Nov 13, 2019
#  R version: R Studio  Version 1.2.1335
#
#  Required input files and data for script
#  1. csvs: QuotaArea.csv, grounds.csv (coordinates for polygons)
#  2. shapefiles: ne_10m_coastline.shp, canusborder.shp, NAFOStatisticalUnits.shp
#  3. SQL: herring.psinf (data from the PSINF table)
#
#  Outputs
#  1. Map of Quota area
#  2. MSE_INF_YYMMDD.csv 
#
#  Runtime ~ 4.5 mins
#
#  Notes: This script is used to generate a plot of the quota area with fishing grounds and SB adn GB boxes
#         This script is also used to generate inf_YYMMDD.csv with Sample IDs needed for subsequent analyses
#         This script can also plot catches by year
######################################################################################################################

rm(list=ls())
require(ggplot2)
require(shapefiles)
require(grid)
require(geosphere)
require(grDevices)
require(gridExtra)
require(taRifx)
library(MASS)
library(maptools)
library(rgdal)
library(sp)
library(plyr)
library(raster)
library(RODBC) #SQL
require(sqldf) #SQL

right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left = function (string,char){substr(string,1,char)}

setwd("U:/Herring MSE/R")
t1 <- Sys.time()

######################################################################################################################
#Detailed Maps
######################################################################################################################

#### Base Map ####
can <- getData('GADM', country="CAN", level=1) # provinces
#can@data
NBNS <- can[can@data$NAME_1 %in% c("New Brunswick","Nova Scotia"),]
#NBNS@data
NBNS@data$id <- rownames(NBNS@data)
NBNS.points <- fortify(NBNS, NAME_1="id")
NBNS.df <- join(NBNS.points, NBNS@data, by="id")

us1 <- getData('GADM', country="USA", level=1)
#us1@data
ME <- us1[us1@data$NAME_1 =="Maine",] 
#ME@data
ME@data$id <- rownames(ME@data)
ME.points <- fortify(ME, NAME_1="id")
ME.df <- join(ME.points, ME@data, by="id")

Atl.df <- rbind.fill(NBNS.df, ME.df)
Atl.df <- convert.to.simple(read.shp("ne_10m_coastline.shp"))
bor <- convert.to.simple(read.shp("canusborder.shp"))            #was used to draw polygon for quota area
NAFO <- convert.to.simple(read.shp("NAFOStatisticalUnits.shp"))  #was used to draw polygon for quota area
Qarea <- read.csv("QuotaArea.csv", header = TRUE, sep = ",")     #Polygon for quota area - by Tim


######################################################################################################################
#Fishing Ground - modified to cover entire quota area
######################################################################################################################

DF <- read.csv("grounds.csv", header = TRUE, sep = ",")
ground_list=unique(DF$GROUND)[!is.na(unique(DF$GROUND))]
for(i in 1:nrow(DF))
{
  DF$Y2[i] <-   as.numeric(left(DF$Y[i],2))+as.numeric(right(DF$Y[i],2))/60
  DF$X2[i] <- -(as.numeric(left(DF$X[i],2))+as.numeric(right(DF$X[i],2))/60)
  
}

boxes <- DF[,c(1,5,4)]#convert.to.simple(read.shp("boxes.shp"))
names(boxes) <- c("ID","X","Y") #column name

######################################################################################################################
#Lines and Boxes. Bacarro Line is 65 37' 10"
######################################################################################################################

Bline <- rbind(c(-65-37/60-10/3600,42), c(-65-37/60-10/3600,43.5))
Bline <- as.data.frame(cbind(rep(1,nrow(Bline)),Bline))
names(Bline) <- c("ID","X","Y")

German_catch_area <- rbind(c(43,-66.833), c(43.75,-66.833), c(43.75,-66.0833), c(43,-66.0833), c(43,-66.833))
German_catch_area <- as.data.frame(cbind(rep(1,nrow(German_catch_area)),German_catch_area))
names(German_catch_area) <- c("ID","Y","X")

German_spawn_box <- rbind(c(43.1667,-66.6667), c(43.6667,-66.6667), c(43.6667,-66.27), c(43.1667,-66.1667), c(43.1667,-66.6667))
German_spawn_box <- as.data.frame(cbind(rep(1,nrow(German_spawn_box)),German_spawn_box))
names(German_spawn_box) <- c("ID","Y","X")

German_survey_box <- rbind(c(43.233,-66.473), c(43.567,-66.473), c(43.567,-66.26), c(43.233,-66.26), c(43.233,-66.473))
German_survey_box <- as.data.frame(cbind(rep(1,nrow(German_survey_box)),German_survey_box))
names(German_survey_box) <- c("ID","Y","X")

SB_catch_area <- rbind(c(44.92,-65.4), c(45.38,-65.4), c(45.38,-64.6), c(45.205,-64.6), c(44.92,-65.4))
SB_catch_area <- as.data.frame(cbind(rep(1,nrow(SB_catch_area)),SB_catch_area))
names(SB_catch_area) <- c("ID","Y","X")

SB_spawn_box <- rbind(c(44.95,-65.333), c(45.2,-65.333), c(45.333,-64.833), c(45.211,-64.638), c(444.95,-65.333))
SB_spawn_box <- as.data.frame(cbind(rep(1,nrow(SB_spawn_box)),SB_spawn_box))
names(SB_spawn_box) <- c("ID","Y","X")

SB_survey_box <- rbind(c(45.012,-65.202), c(45.174,-65.202), c(45.315,-64.834), c(45.218,-64.672), c(45.012,-65.202))
SB_survey_box <- as.data.frame(cbind(rep(1,nrow(SB_survey_box)),SB_survey_box))
names(SB_survey_box) <- c("ID","Y","X")

######################################################################################################################
#Base plot
######################################################################################################################

xmin <- -68
xmax <- -63
ymin <- 42
ymax <- 46
#Combine for base plot
at <- seq(-xmin,-xmax,-1)
at2 <- seq(ymin,ymax,1)
L <- parse(text=paste(at, "*degree ", sep=""))
L2 <- parse(text=paste(at2, "*degree ", sep=""))

baseplot <- ggplot(Atl.df)  + geom_path(aes(X,Y,group=Id)) + coord_map() + ylab("") + xlab("") +
  theme_bw() + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  geom_path(data=boxes, aes(x=X,y=Y),colour='red') +
  geom_line(data=Bline, aes(x=X,y=Y),colour='green') + 
  #geom_path(data=bor,   aes(x=X,y=Y),colour="purple") +
  #geom_path(data=NAFO,  aes(x=X,y=Y,group=Id),colour="blue") +
  geom_path(data=Qarea,  aes(x=X,y=Y,group=ID),colour="orange") +
  geom_path(data=German_catch_area, aes(x=X,y=Y,group=ID),linetype=2,colour='purple') +
  geom_path(data=German_spawn_box, aes(x=X,y=Y,group=ID),linetype=2,colour='green') +
  geom_path(data=German_survey_box, aes(x=X,y=Y,group=ID),linetype=2,colour='brown') +
  geom_path(data=SB_catch_area, aes(x=X,y=Y,group=ID),linetype=2,colour='purple') +
  geom_path(data=SB_spawn_box, aes(x=X,y=Y,group=ID),linetype=2,colour='green') +
  geom_path(data=SB_survey_box, aes(x=X,y=Y,group=ID),linetype=2,colour='brown') 

P1 <- baseplot +
  scale_x_continuous(breaks=c(xmin:xmax), labels=L, limits=c(xmin,xmax), expand = c(0, 0)) +
  scale_y_continuous(breaks=at2, labels=L2, limits=c(ymin-0.05,ymax), expand = c(0, 0)) 
P1


######################################################################################################################
#Get inf data from SQL and subset for appropraite years
######################################################################################################################
channel <- odbcConnect("ptran", uid="barretttj", pwd="rowrow7o",believeNRows=F) 
inf <- sqlQuery(channel, paste("select SAMPLID, SDATE, GEAR, LAT, LON, MARKET_WEIGHT_KG, FISHING_VESSEL_1 from herring.psinf"))
inf <- inf[inf$SAMPLID>=19980000,]
inf$GROUND <- NA

for(i in 1:nrow(inf)) #this loop converts coordinates to decimal degrees
{
  ny <- nchar(inf$LAT[i])
  nx <- nchar(inf$LON[i])
  if(ny==4)
  {
    inf$Y[i] <- as.numeric(left(inf$LAT[i],2))+as.numeric(right(inf$LAT[i],2))/60
  } 
  if(ny>=6)
  {
    inf$Y[i] <- as.numeric(left(inf$LAT[i],2))+as.numeric(right(inf$LAT[i],ny-2))/60
  }  
  if(nx==4)
  {
    inf$X[i] <- -(as.numeric(left(inf$LON[i],2))+as.numeric(right(inf$LON[i],2))/60)
  } 
  if(nx>=6)
  {
    inf$X[i] <- -(as.numeric(left(inf$LON[i],2))+as.numeric(right(inf$LON[i],nx-2))/60)
  } 
  for(j in 1:length(ground_list)) #this loop assigns a fishing ground by checking to see if coordinates are inside the polygons for the fishing grounds
  {
    DF_sub <- DF[DF[,1]==ground_list[j],]
    DF_sub <- DF_sub[!is.na(DF_sub[,4]),]
    if(point.in.polygon(inf$X[i],inf$Y[i],DF_sub$X2,DF_sub$Y2)>0)
    {
      inf$GROUND[i] <- as.character(ground_list[j])
    }
  }
}

inf$Q_area <- NA
inf$Date <- as.Date("1900/01/01",format='%Y/%m/%d')

for(i in 1:nrow(inf)) #Add Day, Month, Year, and Date columns. Add an indicator variable for inside the quota area
{
  inf$Year[i] <- as.numeric(left(inf$SAMPLID[i],4))
  inf$Month[i] <- as.character(right(left(inf$SDATE[i],7),2))
  inf$Day[i] <- as.character(right(left(inf$SDATE[i],10),2))
  inf$Date[i] <- as.Date(paste0(inf$Year[i],"/",inf$Month[i],"/",inf$Day[i]),format='%Y/%m/%d')
  ####
  
  if(point.in.polygon(inf$X[i],inf$Y[i],Qarea$X,Qarea$Y)>0)
  {
    inf$Q_area[i]="Y"
  }
}

inf$Dat <- as.Date(inf$Date)

inf <- subset(inf,Q_area=="Y") #remove points outside the quota area

######################################################################################################################
#Calculate Quota Year
######################################################################################################################

inf$Q_YEAR <- inf$Year
for(i in 1:nrow(inf))
{
  if(as.numeric(inf$Month[i])>=11 | (inf$Month[i]=="10" & as.numeric(inf$Day[i]>15)))
  {
  inf$Q_YEAR[i] <- inf$Year[i]+1   
  }
}

######################################################################################################################
#Plot catches with market weight not NA by year
######################################################################################################################

#yrs <- c(1999:2019)
#for(i in 1:length(yrs))
#{
#  P <- P1 + geom_point(data=subset(inf,QYear==yrs[i] & !is.na(MARKET_WEIGHT_KG)),  aes(x=X,y=Y),colour="blue") +
#    labs(title = yrs[i])
#  P
#  ggsave(paste0(yrs[i],".jpeg"))
#}

######################################################################################################################
#Save files and calculate total Quota Catch
######################################################################################################################

##remove wier and shut off
#inf_no_NB_weir=inf[!((inf$GEAR==1 | inf$GEAR==4) & (inf$Ground=="NB Coastal" | inf$Ground=="Grand Manan")),]
##remove bottom trawl and mid trawl
#inf_no_NB_weir=inf_no_NB_weir[!(inf$GEAR==5 | inf$GEAR==3),]

#sumstats=aggregate(inf_no_NB_weir$MARKET_WEIGHT_KG, by=list(inf_no_NB_weir$Q_Year),FUN=sum,na.rm=TRUE)


write.csv(inf, "MSE_INF_191113.csv",row.names = F)
#write.csv(sumstats, "PS_INF_Total_Catch_190731.csv",row.names = F)
t2 <- Sys.time()

t2-t1

