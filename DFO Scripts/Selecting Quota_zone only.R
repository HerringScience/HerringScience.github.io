library(RODBC) #SQL
library(sqldf) #SQL

library(sp)
library(dplyr)
library(ggplot2)
library(grid)
library(grDevices)
library(gridExtra)
library(taRifx)
library(MASS)

library(RODBC) #SQL
library(sqldf) #SQL
#library(ROracle) #SQL
library(ggpubr) #ggarrange
library(xlsx) #write to Excel

library(data.table)
library(ggthemes)
library(plotly)
library(scales)
library(hrbrthemes)
library(viridis)

# setwd("C:/Users/DebertinA/Documents/4VWX Herring/Acoustic Index Review Files")

DFL<-read.csv("DFL.csv")
DFD<-read.csv("DFD.csv")
inf<-read.csv("inf.csv")
Qarea <- read.csv("QuotaArea.csv", header = TRUE, sep = ",")   


inf$Q <- NA

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
  if(point.in.polygon(inf$X[i],inf$Y[i],Qarea$X,Qarea$Y)>0)
  {
    inf$Q[i]="Y" #in quota area Y=yes and N=no
  } else {inf$Q[i]="N"}
}

Quota_inf <- subset(inf, Q == "Y")

SAMPLID_select <-unique(Quota_inf$SAMPLID)

Quota_DFL <-filter(DFL, SAMPLID %in% SAMPLID_select)
Quota_DFD <-filter(DFD, SAMPLID %in% SAMPLID_select)
