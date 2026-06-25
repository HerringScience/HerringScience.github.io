# remove everything in the workspace
rm(list = ls())

library(lubridate)
library(ggplot2)
library(dplyr)
library(reprex)
library(tidyverse)
library(geosphere)
library(reshape2)


#load actual data
s1a<-read.csv("2021-05-24 tableA.csv") %>% mutate(Type = "Actual") 
s2a<-read.csv("2021-06-05 tableA.csv") %>% mutate(Type = "Actual")
s3a<-read.csv("2021-06-19 tableA.csv") %>% mutate(Type = "Actual")
s4a<-read.csv("2021-07-04 tableA.csv") %>% mutate(Type = "Actual")
s5a<-read.csv("2021-07-17 tableA.csv") %>% mutate(Type = "Actual") 
s6a<-read.csv("2021-08-01 tableA.csv") %>% mutate(Type = "Actual") 
s7a<-read.csv("2021-08-15 tableA.csv") %>% mutate(Type = "Actual")
s8a<-read.csv("2021-08-23 tableA.csv") %>% mutate(Type = "Actual")
s9a<-read.csv("2021-08-29 tableA.csv") %>% mutate(Type = "Actual")
s10a<-read.csv("2021-09-05 tableA.csv") %>% mutate(Type = "Actual")
s11a<-read.csv("2021-09-13 tableA.csv") %>% mutate(Type = "Actual")
s12a<-read.csv("2021-09-26 tableA.csv") %>% mutate(Type = "Actual")

s14a<-read.csv("2021-09-20 tableA.csv") %>% mutate(Type = "Actual")
s13a<-read.csv("2021-10-03 tableA.csv") %>% mutate(Type = "Actual")
s15a<-read.csv("2021-10-18 tableA.csv") %>% mutate(Type = "Actual")
s16a<-read.csv("2021-11-02 tableA.csv") %>% mutate(Type = "Actual")



#load planned data
s1p<-read.csv("may24_2021plan.csv") %>% mutate(Type = "Plan")
s2p<-read.csv("june5_2021plan.csv") %>% mutate(Type = "Plan")
s3p<-read.csv("june19_2021plan.csv") %>% mutate(Type = "Plan")
s4p<-read.csv("july4_2021plan.csv") %>% mutate(Type = "Plan")
s5p<-read.csv("july17_2021plan.csv") %>% mutate(Type = "Plan")
s6p<-read.csv("aug1_2021plan.csv") %>% mutate(Type = "Plan") 
s7p<-read.csv("aug15_2021plan.csv") %>% mutate(Type = "Plan")
s8p<-read.csv("aug22_2021plan.csv") %>% mutate(Type = "Plan")
s9p<-read.csv("aug29_2021plan.csv") %>% mutate(Type = "Plan")
s10p<-read.csv("sept5_2021plan.csv") %>% mutate(Type = "Plan")
s11p<-read.csv("sept13_2021plan.csv") %>% mutate(Type = "Plan")
s12p<-read.csv("sept26_2021plan.csv") %>% mutate(Type = "Plan")

s13p<-read.csv("oct3_2021plan.csv") %>% mutate(Type = "Plan")
s14p<-read.csv("sept20_2021plan.csv") %>% mutate(Type = "Plan")
s15p<-read.csv("oct18_2021plan.csv") %>% mutate(Type = "Plan")
s16p<-read.csv("nov02_2021plan.csv") %>% mutate(Type = "Plan")


#combine data
s1<-full_join(s1a, s1p) %>% mutate(Survey = "1")
s2<-full_join(s2a, s2p) %>% mutate(Survey = "2")
s3<-full_join(s3a, s3p) %>% mutate(Survey = "3")
s4<-full_join(s4a, s4p) %>% mutate(Survey = "4")
s5<-full_join(s5a, s5p) %>% mutate(Survey = "5")
s6<-full_join(s6a, s6p) %>% mutate(Survey = "6")
s7<-full_join(s7a, s7p) %>% mutate(Survey = "7")
s8<-full_join(s8a, s8p) %>% mutate(Survey = "8")
s9<-full_join(s9a, s9p) %>% mutate(Survey = "9")
s10<-full_join(s10a, s10p) %>% mutate(Survey = "10")
s11<-full_join(s11a, s11p) %>% mutate(Survey = "11")
s12<-full_join(s12a, s12p) %>% mutate(Survey = "11")

s13<-full_join(s13a, s13p) %>% mutate(Survey = "13")
s14<-full_join(s14a, s14p) %>% mutate(Survey = "14")
s15<-full_join(s15a, s15p) %>% mutate(Survey = "15")
s16<-full_join(s16a, s16p) %>% mutate(Survey = "16")


#Surveys<-bind_rows(list(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12)) #ignore if analyzing single survey, use s## below and set Surveys<-s##
#rm(list=setdiff(ls(), "Surveys"))

#calculate distance 
#Surveys<-Surveys %>% mutate(Distance = distHaversine(cbind(Start.Lon,Start.Lat), cbind(End.Lon,End.Lat))) %>% mutate(Distance = Distance/1000)
s12<-s12 %>% mutate(Distance = distHaversine(cbind(Start.Lon,Start.Lat), cbind(End.Lon,End.Lat))) %>% mutate(Distance = Distance/1000)
Surveys<-s12

s13<-s13 %>% mutate(Distance = distHaversine(cbind(Start.Lon,Start.Lat), cbind(End.Lon,End.Lat))) %>% mutate(Distance = Distance/1000)
s14<-s14 %>% mutate(Distance = distHaversine(cbind(Start.Lon,Start.Lat), cbind(End.Lon,End.Lat))) %>% mutate(Distance = Distance/1000)

s15<-s15 %>% mutate(Distance = distHaversine(cbind(Start.Lon,Start.Lat), cbind(End.Lon,End.Lat))) %>% mutate(Distance = Distance/1000)

s16<-s16 %>% mutate(Distance = distHaversine(cbind(Start.Lon,Start.Lat), cbind(End.Lon,End.Lat))) %>% mutate(Distance = Distance/1000)


Surveys<-s14
Surveys<-s15
Surveys<-s16



#calculate time/speed
Surveys<-Surveys %>% mutate(Start=as.POSIXct(Date.Time.Start, origin = "1970-01-01")) %>% 
  mutate(End=as.POSIXct(Date.Time.End, origin = "1970-01-01")) %>% 
  select(-Date.Time.Start, -Date.Time.End) %>%
  mutate(Duration = as.numeric(End-Start)*60) %>%
  mutate(Speed = (Distance*1000)/(Duration*60)) %>%
  select(-Duration)
Surveys<-Surveys %>% mutate(Speed = Speed*1.94384) #convert from m/s to knots
Surveys<-Surveys %>% mutate(Year = as.numeric(substr(Start, 1, 4)))
Surveys<-Surveys %>% mutate(Date = date(Start))

#summarize speed by vessel
Speed<-Surveys %>%
  group_by(Vessel, Survey) %>%
  summarize(Speed = mean(Speed, na.rm = TRUE))
  #filter(!Vessel == "TM")

#summarize speed by vessel so far in 2021
SpeedTotal<-Speed %>%
  group_by(Vessel) %>%
  summarize(Speed = mean(Speed, na.rm = TRUE))

#group by survey + vessel + type, summarize distance
Summary<-Surveys %>% 
  group_by(Vessel, Type, Survey) %>%
  summarize(Distance = sum(Distance)) %>%
  spread(Type, Distance, fill = 0) %>%
  transmute(Survey, Vessel, Difference = Actual-Plan)
  #filter(!Vessel == "LJ") #error with LJ in GB1
  #filter(!Vessel == "TM") %>% #error in TM's single survey
  #filter(Difference > -20) #error where FM missed entire line

#total amount cut so far per vessel
Total<-Summary %>%
  group_by(Vessel) %>%
  summarize(Difference = sum(Difference))

##CALCULATIONS/PLOTS##
#Speed plot
ggplot(Speed, aes(Vessel, Speed)) + geom_boxplot() + ylab("Speed (knots)") + theme_light() + geom_hline(yintercept=8, linetype="dashed", color="red", size=1)+labs(title="Speed per Vessel")

ggplot(SpeedTotal, aes(Vessel, Speed)) + geom_col() + ylab("Speed (knots)") + theme_light() + geom_hline(yintercept=8, linetype="dashed", color="red", size=1)+labs(title="Speed per Vessel")+ coord_cartesian(ylim=c(8.0,NA))

ggplot(Speed, aes(Survey, Speed)) + geom_boxplot() + ylab("Speed (knots)") + theme_light() + geom_hline(yintercept=8, linetype="dashed", color="red", size=1)+labs(title="Speed per Survey")

#Totals by Vessel
ggplot(Summary, aes(Vessel, Difference)) + geom_boxplot() + scale_y_reverse() + ylab("Distance cut from Planned Lines (KM)") + theme_light() + geom_hline(yintercept=0, linetype="dashed", color="red", size=1)+labs(title="Distance Cut per Vessel")

#Totals by Survey
ggplot(Summary, aes(Vessel, Difference)) + geom_point() + scale_y_reverse() + ylab("Distance cut from Planned Lines (KM)") + facet_wrap(~Survey) + theme_light() + geom_hline(yintercept=0, linetype="dashed", color="red", size=1)+labs(title="Distance Cut per Survey")

#Cumulative totals by Vessel
ggplot(Total, aes(Vessel, Difference)) + geom_col() + scale_y_reverse() + ylab("Total Distance cut in 2021 (KM)") + theme_light() +labs(title="Cumulative Distance Cut per Vessel in 2021")

#filter each boat#filter each boatgeom_bar()
BP<-Summary %>% filter(Vessel == "BP")
C1<-Summary %>% filter(Vessel == "C1")
FM<-Summary %>% filter(Vessel == "FM")
LB<-Summary %>% filter(Vessel == "LB")
LJ<-Summary %>% filter(Vessel == "LJ")
LM<-Summary %>% filter(Vessel == "LM")
MS<-Summary %>% filter(Vessel == "MS")
SL<-Summary %>% filter(Vessel == "SL")

ggplot(BP, aes(Survey, Difference)) + geom_point() + geom_hline(yintercept=0, linetype="dashed", color="red", size=1) + scale_y_reverse() + ylab("Distance cut from Planned Lines (KM)") +labs(title="Brunswick Provider") + coord_cartesian(ylim=c(NA,-9)) + theme_light()
ggplot(C1, aes(Survey, Difference)) + geom_point() + geom_hline(yintercept=0, linetype="dashed", color="red", size=1) + scale_y_reverse() + ylab("Distance cut from Planned Lines (KM)") +labs(title="Canada 100") + coord_cartesian(ylim=c(NA,-9)) + theme_light()
ggplot(FM, aes(Survey, Difference)) + geom_point() + geom_hline(yintercept=0, linetype="dashed", color="red", size=1) + scale_y_reverse() + ylab("Distance cut from Planned Lines (KM)") +labs(title="Fundy Monarch") + coord_cartesian(ylim=c(NA,-9)) + theme_light() 
ggplot(LB, aes(Survey, Difference)) + geom_point() + geom_hline(yintercept=0, linetype="dashed", color="red", size=1) + scale_y_reverse() + ylab("Distance cut from Planned Lines (KM)") +labs(title="Leroy and Barry") + coord_cartesian(ylim=c(NA,-9)) + theme_light() 
ggplot(LJ, aes(Survey, Difference)) + geom_point() + geom_hline(yintercept=0, linetype="dashed", color="red", size=1) + scale_y_reverse() + ylab("Distance cut from Planned Lines (KM)") +labs(title="Lady Janice II") + coord_cartesian(ylim=c(NA,-9)) + theme_light()
ggplot(LM, aes(Survey, Difference)) + geom_point() + geom_hline(yintercept=0, linetype="dashed", color="red", size=1) + scale_y_reverse() + ylab("Distance cut from Planned Lines (KM)") +labs(title="Lady Melissa") + coord_cartesian(ylim=c(NA,-9)) + theme_light() 
ggplot(MS, aes(Survey, Difference)) + geom_point() + geom_hline(yintercept=0, linetype="dashed", color="red", size=1) + scale_y_reverse() + ylab("Distance cut from Planned Lines (KM)") +labs(title="Morning Star") + coord_cartesian(ylim=c(NA,-9)) + theme_light() 
ggplot(SL, aes(Survey, Difference)) + geom_point() + geom_hline(yintercept=0, linetype="dashed", color="red", size=1) + scale_y_reverse() + ylab("Distance cut from Planned Lines (KM)") +labs(title="Sealife II") + coord_cartesian(ylim=c(NA,-9)) + theme_light()

Speed %>% write.csv("C:/Users/Darren/Desktop/HSC/Line Completion/SpeedRaw.csv")
SpeedTotal %>% write.csv("C:/Users/Darren/Desktop/HSC/Line Completion/SpeedTotal.csv")
Surveys %>% write.csv("C:/Users/Darren/Desktop/HSC/Line Completion/SurveyTotal.csv")
