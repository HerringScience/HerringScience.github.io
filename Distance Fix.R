rm(list = ls())

library(tidyverse)
library(googlesheets4)
library(janitor)

surv.no=4
surv="SB"

###Performance data import and filtering###
A = read_csv("C:/Users/kellyd17/Documents/GitHub/HerringScience.github.io/Surveys/2026/SB4/tableA.csv")
A = A %>% mutate(Type = "Actual")
plan = read_csv("C:/Users/kellyd17/Documents/GitHub/HerringScience.github.io/Surveys/2026/SB4/survey plan.csv")
plan = plan %>% mutate(Type = "Plan")
Perform = full_join(A, plan) %>% mutate(Survey = surv.no) %>% mutate(Location = surv)
Perform = Perform %>% rename(Yend="End Lat", Xend="End Lon", Y="Start Lat", X="Start Lon", Dist..km.="Dist (km)", Date.Time.Start="Date Time Start", Date.Time.End="Date Time End", Transect.No.="Transect No.")
Perform = Perform %>% mutate(Distance = distHaversine(cbind(X,Y), cbind(Xend,Yend))) %>% mutate(Distance = Distance/1000)
Perform = Perform %>% mutate(Distance = ifelse(is.na(Dist..km.), Distance, Dist..km.))

#group by survey + vessel + type, summarize distance
Distance<-Perform %>% 
  dplyr::group_by(Vessel, Type) %>%
  dplyr::summarize(Distance = sum(Distance)) %>%
  spread(Type, Distance, fill = 0) %>%
  transmute(Vessel, Difference = Actual-Plan)