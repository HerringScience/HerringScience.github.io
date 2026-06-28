######################################################################################################################
#  Description: Acoustic Catch at Length
#  Created by: Tim Barrett & Allan Debertin
#  Created on: Feb 16, 2021
#  R version: 4.02 R Studio  Version 1.3.1056
#
#  Required input files and data for script
#  1. DFL_191113.csv - Data frame exported from MSE_02_CAL with length frequencies for the catch-at-age
#  2. survey_dates_191211.csv - Survey dates and Biomasses

#
#  Outputs:
#  1. ADAIndex_191213.rds - Age disaggregated Acoustic Index by year
#
#  Time 1.2 min 
#
######################################################################################################################

rm(list=ls())
library(ggplot2)
library(grid)
library(grDevices)
library(gridExtra)
library(taRifx)
library(MASS)
library(sp)
library(RODBC) #SQL
library(sqldf) #SQL
#library(ROracle) #SQL
library(ggpubr) #ggarrange
library(xlsx) #write to Excel
library(dplyr)
library(data.table)

t1 <- Sys.time()
right <- function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left <- function (string,char){substr(string,1,char)}

# Already set with my rprofile
  getwd()
    #setwd("C:/Users/DebertinA/Documents/4VWX Herring/MSE data processing")

minyr <- 2019
maxyr <- 2019
ages <- c(1:11)
lengths <- c(6:40)

######################################################################################################################
# Bring in LFs and Survey dates
######################################################################################################################
DFL <- read.csv("DFLpre_0.5_210215.csv")
DFL$DATE <- as.Date(DFL$DATE,format='%Y-%m-%d')
#SURV <- read.csv("survey_dates_2020.csv",stringsAsFactors = F)
SURV <- read.csv("survey_dates_2019.csv",stringsAsFactors = F)

SURV <- SURV[SURV$BIOMASS!=0,]
# Had to edit date format y-Y and also how it's recorded in .csv. Using 2 digits for all months.
SURV$DATE<-as.Date(SURV$DATE,format = "%m/%d/%Y")

SURV <- subset(SURV, S_GROUND != "SI")
SURV

######################################################################################################################
# Bring in Detailed Samples
# Mat code: 0 Undetermined; 1 and 2 immature; 3 and 4 ripening; 5 ripe; 6 spawning; 7 spent; 8 and 9 recovering
######################################################################################################################
#channel <- odbcConnect("ptran64_2", uid="DEBERTINA", pwd="**",believeNRows=F) 

DFD = read.csv("DFD_2019.csv") # UPDATE THIS PATH # UPDATE THIS PATH # UPDATE THIS #
head(DFD)


# DFD <-sqlQuery(channel, paste("select SAMPLID, LEN, WT, SEX, AGE, MAT from herring.psdet where SAMPLID >= 20200000"))



DFD$LEN <- DFD$LEN/10*1.02 #adjustment of 2% for frozen samples and convert len to cm



# Define spatial areas for samples

# I need to plot each of these boxes with my records. Plotted in 'TagLog2020Figure' and looks fine to me.

# Doesn't include Seal Island in the case that there are no catches on GB
# What about an error when there is not any data within the 5 days?

GB_spawn_box <- rbind(c(43.1667,-66.6667), c(43.6667,-66.6667), c(43.6667,-66.27), c(43.1667,-66.1667), c(43.1667,-66.6667))


GB_spawn_box <- as.data.frame(cbind(rep(1,nrow(GB_spawn_box)),GB_spawn_box))
names(GB_spawn_box) <- c("ID","Y","X")

SB_catch_area <- rbind(c(44.92,-65.4), c(45.38,-65.4), c(45.38,-64.6), c(45.205,-64.6), c(44.92,-65.4))

SB_catch_area <- as.data.frame(cbind(rep(1,nrow(SB_catch_area)),SB_catch_area))
names(SB_catch_area) <- c("ID","Y","X")

#Data from with Sample IDS on the spawning grounds after 1999 with PS gear
SIDS <- DFL[((DFL$GROUND %in% c("Scots Bay","German Bank","Gannet Dry Ledge")) & DFL$YEAR>=minyr) & DFL$GEAR==2,]
SIDS <- SIDS[,c("SAMPLID","Y","X","DATE","GROUND")]
SIDS <- unique.data.frame(SIDS)

SIDS$S_GROUND <- NA

for(i in 1:nrow(SIDS))
{
  if(point.in.polygon(SIDS$X[i],SIDS$Y[i],GB_spawn_box$X,GB_spawn_box$Y)>0)
  {
    SIDS$S_GROUND[i] <- "GB"
  } else if(point.in.polygon(SIDS$X[i],SIDS$Y[i],SB_catch_area$X,SB_catch_area$Y)>0) {
    SIDS$S_GROUND[i] <- "SB"
  } else { SIDS$S_GROUND[i] = NA }
}

SIDS <- SIDS[!is.na(SIDS$S_GROUND),]  ##SIDS is the data frame of LFs on the spawning grounds

head(SIDS)

SIDS$MEANL <- 0

for(i in 1:nrow(SIDS))
{
  si <- which(DFL$SAMPLID==SIDS$SAMPLID[i])
  SIDS$MEANL[i] <- round(DFL$CLEN[si]%*%DFL$LEN[si] / sum(DFL$CLEN[si]),0)
}

SIDS <- SIDS[SIDS$MEANL>=24,]  ##SIDS is the data frame of LFs on the spawning grounds
head(SIDS)
SAMPLIST_L <- list()
SAMPLIST_D <- list()

for(i in 1:nrow(SURV)) # get samplids within one day of survey DATE
{
  l_id <- which((SIDS$DATE >= (SURV$DATE[i]-1) & SIDS$DATE <= (SURV$DATE[i]+1)) & SIDS$S_GROUND==SURV$S_GROUND[i])
  if(length(l_id)==0)
  {
    TEMP <- SIDS[SIDS$S_GROUND==SURV$S_GROUND[i],]
    TEMP$DATE_Dif <- abs(TEMP$DATE - SURV$DATE[i])
    l_id <- which(SIDS$DATE %in% TEMP$DATE[which(TEMP$DATE_Dif == min(TEMP$DATE_Dif))])
  }
  SAMPLIST_L[[i]] <- SIDS[l_id,]$SAMPLID
}


# This is where you specify the number of days around the survey for detail samples

for(i in 1:nrow(SURV))  # get samplids within 5 days of survey DATE (for 2020)
{
  a_id <- which((SIDS$DATE >= (SURV$DATE[i]-5) & SIDS$DATE <= (SURV$DATE[i]+5)) & SIDS$S_GROUND==SURV$S_GROUND[i])
  TEMP <- SIDS[SIDS$S_GROUND==SURV$S_GROUND[i],]
  TEMP$DATE_Dif <- abs(TEMP$DATE - SURV$DATE[i])
  if(length(a_id)==0)
  {
    a_id <- which(SIDS$DATE %in% TEMP$DATE[which(TEMP$DATE_Dif == min(TEMP$DATE_Dif))])
  }
  
  CK <- DFD[DFD$SAMPLID %in% SIDS[a_id,]$SAMPLID & !is.na(DFD$AGE),] 
  if(nrow(CK)==0)
  {
    TEMP$DATE_Dif[TEMP$SAMPLID %in% SIDS[a_id,]$SAMPLID] <- 9999
    a_id <- which(SIDS$DATE %in% TEMP$DATE[which(TEMP$DATE_Dif == min(TEMP$DATE_Dif))] | SIDS$DATE %in% TEMP$DATE[which(TEMP$DATE_Dif == 9999)])
  }
  CK <- DFD[DFD$SAMPLID %in% SIDS[a_id,]$SAMPLID & !is.na(DFD$AGE),] 
  
  while(nrow(CK)<=30) ###bring in more samples if < 25
  {
    TEMP$DATE_Dif[TEMP$SAMPLID %in% SIDS[a_id,]$SAMPLID] <- 995599
    a_id <- which(SIDS$DATE %in% TEMP$DATE[which(TEMP$DATE_Dif == min(TEMP$DATE_Dif))] | SIDS$DATE %in% TEMP$DATE[which(TEMP$DATE_Dif == 995599)])
    CK <- DFD[DFD$SAMPLID %in% SIDS[a_id,]$SAMPLID & !is.na(DFD$AGE),] 
  }
  
  CK <- DFD[DFD$SAMPLID %in% SIDS[a_id,]$SAMPLID & !is.na(DFD$AGE),] 
  
  SAMPLIST_D[[i]] <- SIDS[a_id,]$SAMPLID
}

#Determine mean length

plotlist <- list()

TS <- NULL
for(i in 1:nrow(SURV))  #Counts by Length
{
LF <- DFL[DFL$SAMPLID %in% SAMPLIST_L[[i]],]
DET <- DFD[DFD$SAMPLID %in% SAMPLIST_D[[i]],]
r_id <- which(!is.na(DET$WT) & !is.na(DET$LEN) & DET$LEN>=20)
cond_model <- lm(log10(DET$WT[r_id])~log10(DET$LEN[r_id])) #plot(log10(DET$LEN[r_id]),log10(DET$WT[r_id]))
out_id<-which(abs(studres(cond_model))>4)
if(length(out_id)>0) #Remove STUD > 4
{
  cond_model <- lm(log10(DET$WT[r_id])[-out_id]~log10(DET$LEN[r_id])[-out_id]) #plot(log10(DET$LEN[r_id])[-out_id],log10(DET$WT[r_id])[-out_id])
  r_id<-r_id[-out_id]
}
out_id2<-which(abs(studres(cond_model))>4)
if(length(out_id2)>0) #Remove STUD > 4 a second time
{
  cond_model <- lm(log10(DET$WT[r_id])[-out_id2]~log10(DET$LEN[r_id])[-out_id2]) #plot(log10(DET$LEN[r_id])[-out_id2],log10(DET$WT[r_id])[-out_id2])
  r_id<-r_id[-out_id2]
}

#CD_id <- which(cooks.distance(cond_model) > 4/((length(DET$WT[r_id]))-length(cond_model$coefficients)-2))

a <- 10^(cond_model$coefficients[1])
b  <- cond_model$coefficients[2]   
n  <- cond_model$df.residual+2 

### Plots
plotlist[[i]]<- ggplot(data=data.frame(X=log10(DET$LEN[r_id]),Y=log10(DET$WT[r_id])),aes(x=X,y=Y)) + geom_point() +   geom_smooth(method=lm, se=F) +
  labs(title = paste0(SURV$YEAR[i],"_",SURV$S_GROUND[i],"_",SURV$DATE[i]))


### Counts by TS
TLF<-aggregate(CLEN~LEN,data=LF,FUN=sum, na.rm=TRUE)
TLF$WT <- a*TLF$LEN^b
TLF$TWT <- TLF$WT /1000 ###in kg
TS[i] <-
  (20 * log10(sum(TLF$LEN * TLF$CLEN) / sum(TLF$CLEN)) - 71.9) - 
  10 * log10(sum(TLF$TWT *TLF$CLEN) / sum(TLF$CLEN))

}


head(TLF)

#REFERENCE VALUES FOR OTHER FREQUENCIES

#TS50 <- -0.10727
#TS75 <- -0.26575
#TS120 <- -0.44946

SURV$TS <- TS

head(SURV)

write.csv(SURV, "C:/Users/DebertinA/Documents/4VWX Herring/MSE data processing/TS_est_2020.csv")
write.csv(SURV, file= "TS_2019.csv") 

