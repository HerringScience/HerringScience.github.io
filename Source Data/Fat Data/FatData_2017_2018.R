

RLibrary("lubridate", "ggplot2", "reshape")


SG = read.csv ("SG_FatData_2017_2018.csv")
SL = read.csv ("SL_FatData_2017_2018.csv")

#Format Data - Scotia Gardens 
head(SG)
SG$Recorded_date = as.Date(SG$Recorded_date, "%d-%m-%Y")
SG$ground =as.factor(SG$Location)
SG$id =as.factor(SG$Vessel)

#create a month variable
SG$month <- month(SG$Recorded_date)

str(SG)
head(SG)
tail(SG)

#Format data - SeaLife
head(SL)

SL$Date = as.Date(SL$Date,"%Y-%m-%d")
SL$ground =as.factor(SL$Location)
SL$id =as.factor(SL$Vessel)

#Create a month variable
SL$month <- month(SL$Date)

str(SL)
head(SL)
