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
#library(RODBC) #SQL
#library(sqldf) #SQL
#library(ROracle) #SQL
library(ggpubr) #ggarrange
#library(xlsx) #write to Excel
# this package won't work.
library(dplyr)
library(data.table)
library(ggthemes)
library(plotly)
library(scales)

t1 <- Sys.time()
right <- function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left <- function (string,char){substr(string,1,char)}
# channel <- odbcConnect("ptran64_2", uid="DEBERTINA", pwd="*",believeNRows=F) 


######################################################################################################################
# Bring in Data and Survey dates
######################################################################################################################


#SURVEY DATE
SURV <- read.csv("survey_dates_2020.csv",stringsAsFactors = F)
SURV <- SURV[SURV$BIOMASS!=0,]
SURV$DATE<-as.Date(SURV$DATE,format = "%m/%d/%Y")

# German Bank
  SURV <- subset(SURV, S_GROUND != "SB")
# Scots Bay
  SURV <- subset(SURV, S_GROUND != "GB")

SURV = SURV[order(as.Date(SURV$DATE, format="%m/%d/%Y")),]




DFL <- read.csv("DFL_2020.csv")
DFD <- read.csv("DFD_2020.csv")
inf <- read.csv("inf_2020.csv")

        # German Bank
        inf <-filter(inf, GEAR ==2 & AREA == 465)

        # Scots Bay
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
  if(point.in.polygon(inf$X[i],inf$Y[i],German_survey_box$X,German_survey_box$Y)>0)
  {
    inf$S_GROUND[i] <- "GB"
  }  else if(point.in.polygon(inf$X[i],inf$Y[i],SB_area$X,SB_area$Y)>0) {
    inf$S_GROUND[i] <- "SB"
  }
inf <- inf[!is.na(inf$S_GROUND),]  ##inf is the data frame of LFs on the spawning grounds


head(inf)

# Check you only have the spawning groun you're currently looking at 
unique(inf$S_GROUND)

SIDS <- merge(inf,DFL)
head(SIDS)

SIDS$DATE <- as.Date(SIDS$SDATE,format="%m/%d/%Y")


SIDet <- right_join(inf, DFD, by = "SAMPLID")
SIDet$DATE <- as.Date(SIDet$SDATE,format="%m/%d/%Y")

#length
SAMPLIST_L <- list()
#detail
SAMPLIST_D <- list()

for(i in 1:nrow(SURV))
  if(isTRUE (any(SIDS$DATE == SURV$DATE[i]))){
    l_id <- which((SIDS$DATE == (SURV$DATE[i]))) 
    if(length(l_id)==0)
    {
      l_id = NA
    }
    SAMPLIST_L[[i]] <- SIDS[l_id,]$SAMPLID  
  } else if(isTRUE (any(SIDS$DATE != SURV$DATE[i]))){
    l_id <- which((SIDS$DATE >= (SURV$DATE[i]-2) & SIDS$DATE <= (SURV$DATE[i]+2))) 
    if(length(l_id)==0)
    {
      l_id = NA
    }
    SAMPLIST_L[[i]] <- SIDS[l_id,]$SAMPLID  
  }

# List of length frequency samples being used
SAMPLIST_L

SURV

sort(unique(SAMPLIST_L[[1]]))
sort(unique(SAMPLIST_L[[2]]))
sort(unique(SAMPLIST_L[[3]]))
sort(unique(SAMPLIST_L[[4]]))
sort(unique(SAMPLIST_L[[5]]))
sort(unique(SAMPLIST_L[[6]]))
# Only for Scots Bay
sort(unique(SAMPLIST_L[[7]]))
sort(unique(SAMPLIST_L[[8]]))
sort(unique(SAMPLIST_L[[9]]))

for(i in 1:nrow(SURV))  # get samplids within 5 days of survey DATE
{
  a_id <- which((SIDet$DATE >= (SURV$DATE[i]-4) & SIDet$DATE <= (SURV$DATE[i]+4)))
  TEMP <- SIDet[SIDet$S_GROUND==SURV$S_GROUND[i],]
  TEMP$DATE_Dif <- abs(TEMP$DATE - SURV$DATE[i])
  if(length(a_id)==0)
  {
    a_id  = NA
  }
  
  SAMPLIST_D[[i]] <- SIDet[a_id,]$SAMPLID
}
SAMPLIST_D

SURV
#SAMPIDS for each survey for details (this gives us the length/weight relationship)
sort(unique(SAMPLIST_D[[1]]))
sort(unique(SAMPLIST_D[[2]]))
sort(unique(SAMPLIST_D[[3]]))
sort(unique(SAMPLIST_D[[4]]))
sort(unique(SAMPLIST_D[[5]]))
sort(unique(SAMPLIST_D[[6]]))





######

SURV$TS <- NULL
SURV$Mean_length_mm <- NULL
SURV$Mean_weight <- NULL
for(i in 1:nrow(SURV))
  SURV$TS[i]<- NA 
SURV$Mean_length_mm[i] <- NA
SURV$Mean_weight[i] <- NA
SURV$n[i] <- NA
SURV$N_LF[i] <- NA
SURV$N_DET[i] <- NA
SURV$No_Measured[i]<-NA
SURV$lessthan23[i]<-NA
SURV$stage5prop[i]<-NA
SURV$stage6prop[i]<-NA

for(i in 1:nrow(SURV))
  if (any(c(is.na(SAMPLIST_L[[i]][1]),  is.na(SAMPLIST_D[[i]][1] ))) == TRUE){ 
    SURV$TS[i] <- -35.5
    
  }

for(i in 1:nrow(SURV))
  if (any(c(is.na(SAMPLIST_L[[i]][1]),  is.na(SAMPLIST_D[[i]][1] ))) == TRUE){ 
    SAMPLIST_L[[i]] <-  NULL
    SAMPLIST_D[[i]] <-  NULL
  }

for(i in 1:nrow(SURV))
  if (any(c(is.na(SAMPLIST_L[[i]][1]),  is.na(SAMPLIST_D[[i]][1] ))) == TRUE){ 
    SAMPLIST_L[[i]] <-  NULL
    SAMPLIST_D[[i]] <-  NULL
  }




SURV2 <-SURV[(is.na(SURV$TS)) == 0, ]   # Standard target strength for surveys without samples
SURV <-SURV[(is.na(SURV$TS)) != 0, ]   #Keeping surveys with samples for next steps



plotlist <- list()
plotlist_LF <- list()

TS <- NULL
Mean_weight <- NULL
Mean_length_mm <- NULL
Mean_length_cm <- NULL
N_LF <- NULL
n <- NULL
No_Measured <- NULL
lessthan23 <- NULL
stage5prop <- NULL
stage6prop <- NULL
N_DET <- NULL
for(i in 1:nrow(SURV))  #Counts by Length
  
  # I'm getting an error here:
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
  N_LF[i] <- length(unique(SAMPLIST_L[[i]]))
  L23 <-filter(LF_summary, LEN <= 230)
  (sum(L23$No_measured)/sum(LF_summary$No_Measured))
  lessthan23[i]<-label_percent(accuracy = 0.01)(sum(L23$No_Measured)/sum(LF_summary$No_Measured))
  
  DET <- DFD[DFD$SAMPLID %in% unique(SAMPLIST_D[[i]]),]
  DET$logwt <- log10(DET$WT)
  DET$loglen <- log10(DET$LEN*1.02)
  stage5prop[i] <- label_percent(accuracy = 0.01)(length(DET[which(DET$MAT==5),]$MAT) / length(DET$MAT))
  stage6prop[i] <- label_percent(accuracy = 0.01)(length(DET[which(DET$MAT==6),]$MAT) / length(DET$MAT))
  N_DET[i] <- length(unique(SAMPLIST_D[[i]]))
  
  #Linear Regression model used to determine mean weight
  r_id <- which(!is.na(DET$WT) & !is.na(DET$LEN))
  cond_model <- lm(DET$logwt[r_id]~DET$loglen[r_id]) #plot(log10(DET$LEN[r_id]),log10(DET$WT[r_id]))
  a <-  cond_model$coefficients[1]
  b  <- cond_model$coefficients[2]   
  n[i]  <- cond_model$df.residual+2 
  
  Mean_weight[i] <- (10^(b*log10(Mean_length_mm[i])+a))/1000
  
  LF_all <- data.frame(LEN = LF_summary$LEN, No_Measured = LF_summary$No_Measured)
  LF_all$Size <- rep(">23 & <30cm", length(LF_all$LEN))
  LF_LOW <- filter(LF_all, LEN <= 230)
  LF_LOW$Size <- rep(paste0(intToUtf8(8804), "23cm"), length(LF_LOW$LEN))
  LF_HI <- filter(LF_all, LEN >= 300)
  LF_HI$Size <- rep(paste0(intToUtf8(8805),"30cm"), length(LF_HI$LEN))
  LF_all <- filter(LF_all, LEN >230 & LEN <300)
  LF_ploty <- do.call("rbind", list(LF_all, LF_LOW, LF_HI))
  
  ### Plots
  theme_set(theme_bw())
  plotlist[[i]]<- ggplot(data=data.frame(X=(DET$loglen[r_id]),Y=(DET$logwt[r_id])),aes(x=X,y=Y)) + geom_point() +   geom_smooth(method=lm, se=F) +
    labs(title = paste0("Length Weight Relationship for ",SURV$S_GROUND[i]," ",SURV$DATE[i]))+
    xlab(expression("Log"[10]*" Length (mm)")) + 
    ylab(expression("Log"[10]*" Weight (g)"))+
    theme(plot.title = element_text(size=9))
  
  
  ggsave(paste0("GB_LEN_WT",SURV$DATE[i],".png"), plot = plotlist[[i]])
  
  theme_set(theme_bw())
  plotlist_LF[[i]]<- ggplot(data = LF_ploty, aes(x = (LEN / 10), y = No_Measured, fill = Size)) +
    geom_bar(color="black", stat ="identity" , position = position_dodge()) +
    scale_fill_manual(values=c("white","dark grey","black")) +
    labs(title = paste0("Fishery Samples used for survey on ", SURV$DATE[i])) +
    xlab("Length (cm)") + ylab("No. Measured") +
    xlim(15.0, 37.0) +
    theme(plot.title = element_text(size=9), legend.position = c(0.20,0.80))
  
  ggsave(paste0("GB_LF",SURV$DATE[i],".png"), plot = plotlist_LF[[i]])
  
  TS[i] <-(20*log10(Mean_length_mm[i]/10)-71.9)-(10*log10(Mean_weight[i]))
  
}



plotlist
plotlist_LF

#REFERENCE VALUES FOR OTHER FREQUENCIES

#TS50 <- -0.10727
#TS75 <- -0.26575
#TS120 <- -0.44946

SURV$TS38 <- TS
SURV$N_LF <- N_LF
SURV$No_Measured <- No_Measured
SURV$Mean_length_mm <- Mean_length_mm
SURV$N_DET <- N_DET
SURV$n <- n
SURV$Mean_weight <- Mean_weight
SURV$lessthan23 <- lessthan23
SURV$stage5prop <- stage5prop
SURV$stage6prop <- stage6prop
SURV$TS50 = SURV$TS38-0.10727


SURV <-rbind(SURV, SURV2)

SURV

SURV = SURV[order(as.Date(SURV$DATE, format="%m/%d/%Y")),]


write.csv(SURV, "SB_TS_March24.csv")


