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

minyr <- 2020
maxyr <- 2020
#channel <- odbcConnect("ptran64_2", uid="DEBERTINA", pwd="*",believeNRows=F) 


######################################################################################################################
# Bring in Data and Survey dates
######################################################################################################################


#SURVEY DATE
SURV <- read.csv("survey_dates_2020.csv",stringsAsFactors = F)
SURV <- SURV[SURV$BIOMASS!=0,]
SURV$DATE<-as.Date(SURV$DATE,format = "%m/%d/%Y")
SURV <- subset(SURV, S_GROUND != "GB")

#SURV<-SURV[1,] # FOR TESTING 

#DFL <- sqlQuery(channel, paste ("select * from herring.pslen where SAMPLID >= 20200000 and SAMPLID <= 20210000"))
#DFD <-sqlQuery(channel, paste("select * from herring.psdet where SAMPLID >= 20200000 and SAMPLID <= 20210000"))
#inf <-sqlQuery(channel, paste("select * from herring.psinf where SAMPLID >= 20200000 and SAMPLID <= 20210000"))

DFL <- read.csv("DFL_2020.csv")
DFD <- read.csv("DFD_2020.csv")
inf <- read.csv("inf_2020.csv")

inf <-filter(inf, GEAR ==2 & AREA %in% c(466, 467))



for(i in which(!is.na(inf$LAT) & !is.na(inf$LON))) #this loop converts coordinates to decimal degrees
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
}


#SURVEY BOX FOR GERMAN!
German_survey_box <- rbind(c(43.215,-66.47), c(43.57,-66.47), c(43.57,-66.269), c(43.215,-66.269), c(43.215,-66.47))
German_survey_box <- as.data.frame(cbind(rep(1,nrow(German_survey_box)),German_survey_box))
names(German_survey_box) <- c("ID","Y","X")

German_catch_area <- rbind(c(43,-66.833), c(43.75,-66.833), c(43.75,-66.0833), c(43,-66.0833), c(43,-66.833))
German_catch_area <- as.data.frame(cbind(rep(1,nrow(German_catch_area)),German_catch_area))
names(German_catch_area) <- c("ID","Y","X")


SB_area <- rbind(c(44.6666667,-66), c(46,-66), c(46,-63), c(44.6666667,-63), c(44.6666667,-66))
SB_area<- as.data.frame(cbind(rep(1,nrow(SB_area)),SB_area))
names(SB_area) <- c("ID","Y","X")

inf$S_GROUND <- NA    
for(i in 1:nrow(inf))
  if(point.in.polygon(inf$X[i],inf$Y[i],German_catch_area$X,German_catch_area$Y)>0)
  {
    inf$S_GROUND[i] <- "GB"
  }  else if(point.in.polygon(inf$X[i],inf$Y[i],SB_area$X,SB_area$Y)>0) {
    inf$S_GROUND[i] <- "SB"
}

#inf <- inf[!is.na(inf$S_GROUND),]  ##inf is the data frame of LFs on the spawning grounds

SIDS <- merge(inf,DFL, by = "SAMPLID")
SIDS$DATE <- as.Date(SIDS$SDATE,format='%Y-%m-%d')

SIDet <- right_join(inf,DFD, by = "SAMPLID")
SIDet$DATE <- as.Date(SIDet$SDATE,format='%Y-%m-%d')

#SIDS <- SIDS[SIDS$MEANL>=24,]  ##SIDS is the data frame of LFs on the spawning grounds

SAMPLIST_L <- list()
SAMPLIST_D <- list()



for(i in 1:nrow(SURV))
{
    l_id <- which((SIDS$DATE >= (SURV$DATE[i]) & SIDS$DATE <= (SURV$DATE[i]+2))) 
    if(length(l_id)==0)
    {
      l_id = NA
    }
    SAMPLIST_L[[i]] <- SIDS[l_id,]$SAMPLID  
}

# List of length frequency samples used
SAMPLIST_L
sort(unique(SAMPLIST_L[[3]]))





for(i in 1:nrow(SURV))  # get samplids within 5 days of survey DATE
{
  a_id <- which((SIDet$DATE >= (SURV$DATE[i]-5) & SIDet$DATE <= (SURV$DATE[i]+5)) & SIDet$S_GROUND==SURV$S_GROUND[i])
  TEMP <- SIDet[SIDet$S_GROUND==SURV$S_GROUND[i],]
  TEMP$DATE_Dif <- abs(TEMP$DATE - SURV$DATE[i])
  if(length(a_id)==0)
  {
    a_id  = NA
  }
 
  SAMPLIST_D[[i]] <- SIDet[a_id,]$SAMPLID
}

SAMPLIST_D
sort(unique(SAMPLIST_D[[3]]))




SURV$TS <- NULL
SURV$Mean_length_mm <- NULL
SURV$Mean_weight <- NULL
for(i in 1:nrow(SURV))
SURV$TS[i]<- NA 
SURV$Mean_length_mm[i] <- NA
SURV$Mean_weight[i] <- NA
SURV$n[i] <- NA
SURV$No_Measured[i]<-NA
for(i in 1:nrow(SURV))
if (any(c(is.na(SAMPLIST_L[[i]][1]),  is.na(SAMPLIST_D[[i]][1] ))) == TRUE){ 
 SURV$TS[i] <- -35.5

}

for(i in 1:nrow(SURV))
  if (any(c(is.na(SAMPLIST_L[[i]][1]),  is.na(SAMPLIST_D[[i]][1] ))) == TRUE){ 
    SAMPLIST_L[[i]] <-  NULL
    SAMPLIST_D[[i]] <-  NULL
  }


SURV2 <-SURV[(is.na(SURV$TS)) == 0, ]   # Standard target strength for surveys without samples
SURV <-SURV[(is.na(SURV$TS)) != 0, ]   #Keeping surveys with samples for next steps


plotlist <- list()

TS <- NULL
Mean_weight <- NULL
Mean_length_mm <- NULL
Mean_length_cm <- NULL
n <- NULL
No_Measured <- NULL
for(i in 1:nrow(SURV))  #Counts by Length
{
  LF <- SIDS[SIDS$SAMPLID %in% unique(SAMPLIST_L[[i]]),]
LF_summary <-LF%>%
  group_by(LEN) %>%
  summarise(No_Measured = sum(CLEN), wTED_NO_MEAS = ceiling(sum(CLEN*MARKET_WEIGHT_KG/1000)))
LF_summary <- as.data.frame(LF_summary)  
LF_summary$measured <- LF_summary$LEN*LF_summary$No_Measured

No_Measured[i] <- sum(LF_summary$No_Measured) #sum(LF_summary$wTED_NO_MEAS)# Number measured (which makes no sense... but # in catch I guess.)
Mean_length_mm[i] <- round(sum(LF_summary$LEN*LF_summary$wTED_NO_MEAS) / sum(LF_summary$wTED_NO_MEAS),0) 
Mean_length_cm[i] <- round(sum((LF_summary$LEN/10)*LF_summary$wTED_NO_MEAS) / sum(LF_summary$wTED_NO_MEAS),0)


DET <- DFD[DFD$SAMPLID %in% unique(SAMPLIST_D[[i]]),]
DET$logwt <- log10(DET$WT)
DET$loglen <- log10(DET$LEN*1.02)


#Linear Regression model used to determine mean weight
r_id <- which(!is.na(DET$WT) & !is.na(DET$LEN))
cond_model <- lm(DET$logwt[r_id]~DET$loglen[r_id]) #plot(log10(DET$LEN[r_id]),log10(DET$WT[r_id]))
a <-  cond_model$coefficients[1]
b  <- cond_model$coefficients[2]   
n[i]  <- cond_model$df.residual+2 

Mean_weight[i] <- (10^(b*log10(Mean_length_mm[i])+a))/1000

### Plots
plotlist[i]<- ggplot(data=data.frame(X=log10(DET$LEN[r_id]),Y=log10(DET$WT[r_id])),aes(x=X,y=Y)) + geom_point() +   geom_smooth(method=lm, se=F) +
  labs(title = paste0(SURV$YEAR[i],"_",SURV$S_GROUND[i],"_",SURV$DATE[i]))

TS[i] <-(20*log10(Mean_length_mm[i]/10)-71.9)-(10*log10(Mean_weight[i]))

}

plotlist

#REFERENCE VALUES FOR OTHER FREQUENCIES

#TS50 <- -0.10727
#TS75 <- -0.26575
#TS120 <- -0.44946

SURV$TS <- TS
SURV$Mean_length_mm <- Mean_length_mm
SURV$Mean_weight <- Mean_weight
SURV$n <- n
SURV$No_Measured <- No_Measured
SURV <-rbind(SURV, SURV2)

SURV


write.csv(SURV, "C:/Users/DebertinA/Documents/4VWX Herring/MSE data processing/TS_est_2020_SB.csv")


