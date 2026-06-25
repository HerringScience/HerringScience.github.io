######################################################################################################################
#  Description: Acoustic Catch at Length
#  Created by: Tim Barrett & Allan Debertin
#  Created on: Feb 16, 2021
#  R version: 4.02 R Studio  Version 1.3.1056
#
#  Required input files and data for script
#  1. 
#  2. 
#
#  Outputs:
#  1. Target strength for Scots Bay for a given year
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
#library(RODBC) #SQL
#library(sqldf) #SQL
#library(ROracle) #SQL
library(ggpubr) #ggarrange
library(xlsx) #write to Excel
library(dplyr)
library(data.table)
library(ggthemes)
library(plotly)
library(scales)

t1 <- Sys.time()
right <- function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left <- function (string,char){substr(string,1,char)}
#setwd("C:/Users/DebertinA/Documents/4VWX Herring/MSE data processing")


#channel <- odbcConnect("ptran64_2", uid="DEBERTINA", pwd="*",believeNRows=F) 


######################################################################################################################
# Bring in Data and Survey dates
######################################################################################################################



#SURV<-SURV[1,] # FOR TESTING 

#DFL <- sqlQuery(channel, paste ("select * from herring.pslen where SAMPLID >= 20200000 and SAMPLID <= 20210000"))
#DFD <-sqlQuery(channel, paste("select * from herring.psdet where SAMPLID >= 20200000 and SAMPLID <= 20210000"))
#inf <-sqlQuery(channel, paste("select * from herring.psinf where SAMPLID >= 20200000 and SAMPLID <= 20210000"))

DFL <- read.csv("DFL_2020.csv")
DFD <- read.csv("DFD_2020.csv")
inf <- read.csv("inf_2020.csv")

inf <-filter(inf, GEAR ==2 & AREA == 465)

voidSAMPLID <-c(20201115,
                20201117,
                20201121,
                20201125,
                20201127,
                20200507)

DFL <- filter(DFL,!SAMPLID %in% voidSAMPLID)
DFD <- filter(DFD,!SAMPLID %in% voidSAMPLID)
inf <- filter(inf,!SAMPLID %in% voidSAMPLID)


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



filt_LF <- c(20200561, 20200562, 20200563, 20200569, 20200662, 20200680) # these are LFs for German_bank 08-16
filt_Det <- c(20200662, 20200671) #These are the DETs for German 08-16

both <- c(20200561, 20200562, 20200563, 20200569, 20200662, 20200671, 20200680)

DFL <- filter(DFL,SAMPLID %in% filt_LF)
DFD <- filter(DFD,SAMPLID %in% filt_Det)
inf <- filter(inf,SAMPLID %in% both)


SIDS <- merge(inf,DFL)
head(SIDS)
SIDS$SDATE

# this is not the correct date format
SIDS$DATE <- as.Date(SIDS$SDATE,format="%m/%d/%Y")

# Had to change this date format as well.
SIDet <- right_join(inf, DFD, by = "SAMPLID")
SIDet$DATE <- as.Date(SIDet$SDATE,format="%m/%d/%Y")

DET <- DFD[DFD$SAMPLID %in% unique(filt_Det),]
DET$logwt <- log10(DET$WT)
DET$loglen <- log10(DET$LEN*1.02)
stage1prop <- label_percent(accuracy = 0.01)(length(DET[which(DET$MAT==1),]$MAT) / length(DET$MAT))
stage5prop <- label_percent(accuracy = 0.01)(length(DET[which(DET$MAT==5),]$MAT) / length(DET$MAT))
stage6prop <- label_percent(accuracy = 0.01)(length(DET[which(DET$MAT==6),]$MAT) / length(DET$MAT))
N_DET <- length(unique(filt_Det))

#Linear Regression model used to determine mean weight
r_id <- which(!is.na(DET$WT) & !is.na(DET$LEN))
cond_model <- lm(DET$logwt[r_id]~DET$loglen[r_id]) #plot(log10(DET$LEN[r_id]),log10(DET$WT[r_id]))
a <-  cond_model$coefficients[1]
b  <- cond_model$coefficients[2]   
n  <- cond_model$df.residual+2 



SIDS

LF <- SIDS[SIDS$SAMPLID %in% unique(filt_LF),]
LF_summary <-LF%>%
  group_by(LEN) %>%
  summarise(CLEN = CLEN,
            No_Measured = sum(CLEN), 
            wTED_NO_MEAS = ceiling(sum(CLEN*MARKET_WEIGHT_KG/1000)),
            Len_cm = LEN/10,
            len_mm_x_freq = LEN*CLEN,
            lencm_x_freq <- (LEN/10)*CLEN,
            prop_length = CLEN/(sum(CLEN)),
            mid_point_length = LEN+2.5)

Mean_length_mm <- round(sum(LF_summary$LEN*LF_summary$wTED_NO_MEAS) / sum(LF_summary$wTED_NO_MEAS),0) 
Mean_length_cm <- round(sum((LF_summary$LEN/10)*LF_summary$wTED_NO_MEAS) / sum(LF_summary$wTED_NO_MEAS),0)

Mean_weight <- (10^(b*log10(Mean_length_mm)+a))/1000
MidpointCalcWt <- (10^(b*log10(LF_summary$mid_point_length)+a))/1000
TS_number_midpoint_nowt <- (20*log10(LF_summary$mid_point_length/10)-71.9)
TS_number_midpoint <- (20*log10(LF_summary$mid_point_length/10)-71.9) - 10*log10(MidpointCalcWt)
TS_number_midpoint_to_arithmetic <- 10^(TS_number_midpoint_nowt/10)
TS_no_measured <- TS_number_midpoint_to_arithmetic*LF_summary$CLEN
perc_TS_no_meas <- TS_no_measured/(sum(TS_no_measured))
sum(perc_TS_no_meas) #should equal 1


#Read in Acoustic Surveys Summary for German Bank#1



German_08_16 <-read.csv("German_2020-08-16.csv")
German_08_16$meanSa[1] #weighted mean

Arith_mSa <-10^(German_08_16$meanSa[1]/10)
10*log10(sum(Arith_mSa)) #check with weighted mean Sa

Prop_Sa_length <- Arith_mSa*perc_TS_no_meas
Sa_prop_by_TS_no_meas <-10*log10(Prop_Sa_length)
Cal_dens_in_nos <-10^((Sa_prop_by_TS_no_meas-TS_number_midpoint_nowt)/10)

Nos_by_area <-Cal_dens_in_nos*German_08_16$areaKm[1]*1000
Wt_by_area <-Nos_by_area*MidpointCalcWt

Avg_dens_by_TS_perc_no_meas <-German_08_16$mean_biomass_density[1]*perc_TS_no_meas
sum(Avg_dens_by_TS_perc_no_meas) #should match biomass_density

German_08_16$mean_biomass_density[1]

Log_density_by_perc <- 10*log10(Avg_dens_by_TS_perc_no_meas)
Log_density_by_perc
biomass_by_length_by_area <-Avg_dens_by_TS_perc_no_meas*German_08_16$areaKm[1]*1000
sum(biomass_by_length_by_area)
LF_summary

LF_summary <-as.data.frame(LF_summary)

LF_summary$biomass_by_length_by_area <- biomass_by_length_by_area

L23 <-filter(LF_summary, LEN < 230)
sum(L23$biomass_by_length_by_area) # Penalty to apply to biomass
