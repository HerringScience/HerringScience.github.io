######################################################################################################################
#  Description: Landings by Fleet
#  Created by: Tim Barrett
#  Created on: Nov 5,2019
#  R version: R Studio  Version 1.2.1335
#
#  Required input files and data for script
#  1. SQL: marfissci.pro_spc_info_herring (data from MARFIS from herring account)
#  2. SQL: powerm.herring9902 (COMLAND data from knoxd account)
#  2. "MSE_INF_191113.csv" catch data 1998-present
#  3. "COMLAND_CATCH.csv" catch weights and vessels for 1998-2001
#  4. "fleet_A.csv" fleet description by gear and ground (two for purse seine)
#  5. "grounds.cvs" fishing grounds
#
#  Time - 7 mins for MARFIS
#         5 mins for COMLAND
#  Notes:
#  Gear: 24 shutoff; 31 purse seine; 41 42 43 gill net; 63 weir
#  NAFO: 4Xs 189; 4Xr 188; 4Xq 187; 5Yb 192; 4Xp 186; 4Xo 185
######################################################################################################################

rm(list=ls())
library(RODBC) #SQL
library(sqldf) #SQL
library(sp)
library(data.table)
library(dplyr)
t1 <- Sys.time()
right <- function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left <- function (string,char){substr(string,1,char)}

setwd("U:/Herring MSE/R")
Qarea <- read.csv("QuotaArea.csv", header = TRUE, sep = ",")     #Polygon for quota area - by Tim
channel <- odbcConnect("ptran", uid="herring", pwd="boston99",believeNRows=F) 


##Read in Marfis data
M <- sqlQuery(channel, paste("select RND_WEIGHT_KGS, LANDED_DATE, LATITUDE, LONGITUDE, GEAR_CODE, NAFO_UNIT_AREA_ID from marfissci.pro_spc_info_herring
                             where SPECIES_CODE = 200 and NAFO_UNIT_AREA_ID in ('185','186','187','188','189','192')
                             order by LANDED_DATE"))


M <- M[M$LANDED_DATE>="1998-10-15",]

M$Q <- M$Y <- M$X <- M$YEAR <- M$MONTH <- M$DAY <- NA

for(i in 1:nrow(M))
{
  M$YEAR[i]=as.numeric(left(M$LANDED_DATE[i],4))
  M$MONTH[i]=as.character(right(left(M$LANDED_DATE[i],7),2))
  M$DAY[i]=as.character(right(left(M$LANDED_DATE[i],10),2))
  M$DATE[i]=as.Date(paste0(M$YEAR[i],"/",M$MONTH[i],"/",M$DAY[i],format='%Y/%m/%d'))

  if(!is.na(M$LATITUDE[i]) & !is.na(M$LONGITUDE[i]))
  {
    ny=nchar(M$LATITUDE[i])
    nx=nchar(M$LONGITUDE[i])
    if(ny==4)
    {
      M$Y[i]= as.numeric(left(M$LATITUDE[i],2))+as.numeric(right(M$LATITUDE[i],2))/60
    } 
    if(ny>=6)
    {
      M$Y[i]= as.numeric(left(M$LATITUDE[i],2))+as.numeric(right(left(M$LATITUDE[i],4),2))/60
    }  
    if(nx==4)
    {
      M$X[i]= -(as.numeric(left(M$LONGITUDE[i],2))+as.numeric(right(M$LONGITUDE[i],2))/60)
    } 
    if(nx>=6)
    {
      M$X[i]= -(as.numeric(left(M$LONGITUDE[i],2))+as.numeric(right(left(M$LONGITUDE[i],4),2))/60)
    } 
    if(point.in.polygon(M$X[i],M$Y[i],Qarea$X,Qarea$Y)>0)
    {
      M$Q[i]="Y" #in quota area Y=yes and N=no
    } else {M$Q[i]="N"}
  }

}

####Catches with no coordinates
w_id <- which(M$NAFO_UNIT_AREA_ID==189)
M$Q[w_id]="Y"
M$X[w_id]=-66.5
M$Y[w_id]=45

w_id <- which(M$NAFO_UNIT_AREA_ID==188)
M$Q[w_id]="Y"
M$X[w_id]=-66
M$Y[w_id]=44.25

w_id <- which(M$NAFO_UNIT_AREA_ID==187)
M$Q[w_id]="Y"
M$X[w_id]=-66
M$Y[w_id]=43.55

w_id <- which(M$NAFO_UNIT_AREA_ID==186)
M$Q[w_id]="Y"
M$X[w_id]=-66
M$Y[w_id]=42.5

w_id <- which(M$NAFO_UNIT_AREA_ID==192)
M$Q[w_id]="Y"
M$X[w_id]=-67
M$Y[w_id]=44

class(M$DATE) <- "Date"


M <- M[!M$Q=="N",] #remove rows not in quota area

M$Q_YEAR <- M$YEAR
for(i in 1:nrow(M))
{
  if(as.numeric(M$MONTH[i])>=11 | (M$MONTH[i]=="10" & as.numeric(M$DAY[i]>15)))
  {
    M$Q_YEAR[i] <- M$Q_YEAR[i]+1   
  }
}
######################################################################################################################
#Fleets and Fishing Grounds
######################################################################################################################
FLT <- read.csv("fleet.csv", header = TRUE, sep = ",")
M$GEAR <- 0
M$GEAR[M$GEAR_CODE==24] <- 4
M$GEAR[M$GEAR_CODE==63] <- 1
M$GEAR[M$GEAR_CODE==41 | M$GEAR_CODE==42 | M$GEAR_CODE==43] <- 8
M$GEAR[M$GEAR_CODE==31] <- 2 #PS

DF=read.csv("grounds.csv", header = TRUE, sep = ",")
for(i in 1:nrow(DF))
{
  DF$Y2[i] <-   as.numeric(left(DF$Y[i],2))+as.numeric(right(DF$Y[i],2))/60
  DF$X2[i] <- -(as.numeric(left(DF$X[i],2))+as.numeric(right(DF$X[i],2))/60)
  
}
colnames(DF)[1] <- "GROUND"
ground_list <- unique(DF$GROUND)[!is.na(unique(DF$GROUND))]
M$GROUND <- NA
for(i in 1:nrow(M))
{
  for(j in 1:length(ground_list)) #this loop assigns a fishing ground by checking to see if coordinates are inside the polygons for the fishing grounds
  {
    DF_sub <- DF[DF[,1]==ground_list[j],]
    DF_sub <- DF_sub[!is.na(DF_sub[,4]),]
    if(point.in.polygon(M$X[i],M$Y[i],DF_sub$X2,DF_sub$Y2)>0)
    {
      M$GROUND[i] <- as.character(ground_list[j])
    }
  }
}
##Define FLEETS
FLT$GROUND<-as.character(FLT$GROUND)
M<- M %>% left_join(FLT, by = c("GEAR","GROUND"))
M$FLEET[is.na(M$FLEET)]="OTHER"

###Put NS weir and shutoff in "other"
NSw_id <- which((M$NAFO_UNIT_AREA_ID==188 | M$NAFO_UNIT_AREA_ID==187) & (M$GEAR==1 | M$GEAR==4))
M$FLEET[NSw_id] <- "OTHER"

LAND <- aggregate(M$RND_WEIGHT_KGS/1000,by=list(M$Q_YEAR,M$FLEET),FUN=sum)
colnames(LAND) <- c("Q_YEAR","FLEET","LANDINGS_mt")

write.csv(LAND, "MSE_12_LANDINGS_2003-2018.csv",row.names = F)

t2 <- Sys.time()
t2-t1

#write.csv(M, "M.csv")

######################################################################################################################
#COMLAND
######################################################################################################################
rm(list=ls())
library(RODBC) #SQL
library(sqldf) #SQL
library(sp)
library(data.table)
library(dplyr)
t1 <- Sys.time()

##
##Gears 24 shutoff; 31 purse seine; 41 42 43 gill net; 63 weir
##

setwd("U:/Herring MSE/R")
Qarea <- read.csv("QuotaArea.csv", header = TRUE, sep = ",")     #Polygon for quota area - by Tim


right <- function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left <- function (string,char){substr(string,1,char)}

##################################################################################################################
channel <- odbcConnect("ptran", uid="knoxd", pwd="sunny1",believeNRows=F) 
CLC9902 <- sqlQuery(channel, paste("select CATCHERS_RECID, CFV_NUMBER, LATITUDE, LONGITUDE, LAND_DATE, LIVE_WT, GEAR_TYPE, NAFO_DIVISION_CODE, NAFO_UNIT_AREA from powerm.herring9902"))
CLC8998 <- sqlQuery(channel, paste("select CATCHERS_RECID, CFV_NUMBER, LATITUDE, LONGITUDE, LAND_DATE, LIVE_WT, GEAR_TYPE, NAFO_DIVISION_CODE, NAFO_UNIT_AREA from powerm.herring8998"))

CLC <- rbind(CLC9902,CLC8998)

CLC$NAFO <- paste0(CLC$NAFO_DIVISION_CODE,CLC$NAFO_UNIT_AREA)
CLC <- CLC[CLC$NAFO %in% c("4Xs","4Xr","4Xq","4Xo","4Xp","5Yb"),]
##################################################################################################################

M <- CLC[!is.na(CLC$LIVE_WT),]
M$Q <- M$Y <- M$X <- M$YEAR <- M$MONTH <- M$DAY <- NA

w_id <- which(M$NAFO=="4Xs")
M$LONGITUDE[w_id]=-66.5
M$LATITUDE[w_id]=45

w_id <- which(M$NAFO=="4Xr")
M$LONGITUDE[w_id]=-66
M$LATITUDE[w_id]=44.25

w_id <- which(M$NAFO=="4Xq")
M$LONGITUDE[w_id]=-66
M$LATITUDE[w_id]=43.55

w_id <- which(M$NAFO=="4Xp")
M$LONGITUDE[w_id]=-66
M$LATITUDE[w_id]=42.5

           

#########
for(i in 1:nrow(M))
{
  M$YEAR[i] <- as.numeric(left(M$LAND_DATE[i],4))
  M$MONTH[i] <- as.character(right(left(M$LAND_DATE[i],7),2))
  M$DAY[i] <- as.character(right(left(M$LAND_DATE[i],10),2))
  M$DATE[i] <- as.Date(paste0(M$YEAR[i],"/",M$MONTH[i],"/",M$DAY[i],format='%Y/%m/%d'))
  
  if(point.in.polygon(M$LONGITUDE[i],M$LATITUDE[i],Qarea$X,Qarea$Y)>0)
  {
    M$Q[i] <- "Y"
  } else {M$Q[i] <- "N"}
  
  
}

class(M$DATE) <- "Date"

M <- M[!M$Q=="N",]

M$Q_YEAR <- M$YEAR
for(i in 1:nrow(M))
{
  if(as.numeric(M$MONTH[i])>=11 | (M$MONTH[i]=="10" & as.numeric(M$DAY[i]>15)))
  {
    M$Q_YEAR[i] <- M$Q_YEAR[i]+1   
  }
}

#########################GROUNDS
M$GEAR <- 0
M$GEAR[M$GEAR_TYPE==24] <- 4
M$GEAR[M$GEAR_TYPE==63] <- 1
M$GEAR[M$GEAR_TYPE==41 | M$GEAR_TYPE==42 | M$GEAR_TYPE==43] <- 8
M$GEAR[M$GEAR_TYPE==31] <- 2 #PS

DF <- read.csv("grounds.csv", header = TRUE, sep = ",")
for(i in 1:nrow(DF))
{
  DF$Y2[i] <-   as.numeric(left(DF$Y[i],2))+as.numeric(right(DF$Y[i],2))/60
  DF$X2[i] <- -(as.numeric(left(DF$X[i],2))+as.numeric(right(DF$X[i],2))/60)
  
}

colnames(DF)[1] <- "GROUND"
ground_list <- unique(DF$GROUND)[!is.na(unique(DF$GROUND))]
M$GROUND <- NA
for(i in 1:nrow(M))
{
  for(j in 1:length(ground_list)) #this loop assigns a fishing ground by checking to see if coordinates are inside the polygons for the fishing grounds
  {
    DF_sub <- DF[DF[,1]==ground_list[j],]
    DF_sub <- DF_sub[!is.na(DF_sub[,4]),]
    if(point.in.polygon(M$LONGITUDE[i],M$LATITUDE[i],DF_sub$X2,DF_sub$Y2)>0)
    {
      M$GROUND[i] <- as.character(ground_list[j])
    }
  }
}
##Define FLEETS
FLT <- read.csv("fleet_A.csv", header = TRUE, sep = ",")
FLT$GROUND <- as.character(FLT$GROUND)
M <- M %>% left_join(FLT, by = c("GEAR","GROUND"))
M$FLEET[is.na(M$FLEET)] <- "OTHER"

LAND <- aggregate(M$LIVE_WT,by=list(M$Q_YEAR,M$FLEET),FUN=sum)
colnames(LAND) <- c("Q_YEAR","FLEET","LANDINGS_mt")

write.csv(LAND, "MSE_11_LANDINGS_1998-2002.csv",row.names = F)

t2 <- Sys.time()

t2-t1


######################################################################################################################
#Combine two datasets
######################################################################################################################

LAND9802 <- read.csv("MSE_11_LANDINGS_1998-2002.csv", header = TRUE, sep = ",") 
LAND9802 <- LAND9802[LAND9802$Q_YEAR>=1999 & LAND9802$Q_YEAR<=2002,]
LAND0318 <- read.csv("MSE_12_LANDINGS_2003-2018.csv", header = TRUE, sep = ",")    
LAND0318 <- LAND0318[LAND0318$Q_YEAR>=2003 & LAND0318$Q_YEAR<=2018,]    

LAND <- rbind(LAND9802,LAND0318)
LAND <- LAND[order(LAND$Q_YEAR,LAND$FLEET),]

write.csv(LAND, "MSE_10_LANDINGS_1999-2018.csv",row.names = F)

C_hist <- as.data.frame(matrix(data=0,nrow=length(unique(LAND$Q_YEAR)),ncol=length(unique(LAND$FLEET))))
colnames(C_hist) <- sort(unique(LAND$FLEET))
rownames(C_hist) <- sort(unique(LAND$Q_YEAR))

for(i in 1:nrow(LAND))
{
  rid <- which(rownames(C_hist) == LAND$Q_YEAR[i] & which(colnames(C_hist) == LAND$FLEET[i]))
  cid <- which(colnames(C_hist) == LAND$FLEET[i])
  C_hist[rid,cid] <- LAND$LANDINGS_mt[i]
}
                               
saveRDS(C_hist, "U:/Herring MSE/R/RDS/Chist_191115.rds")


######################################################################################################################
# Fleet B
######################################################################################################################

C_histx <- C_hist

C_histx$PS_juv <- C_histx$PS_juv + C_histx$PS_spa
C_histx <- C_histx[,-4]
colnames(C_histx)[3] <- "PS"

saveRDS(C_histx, "U:/Herring MSE/R/RDS/Chistx_191115.rds")

write.csv(C_histx, "C_histx_19115.csv",row.names = F)
