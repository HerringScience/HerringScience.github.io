
multiplyNumbers <- function() {
  3*4
}

print(multiplyNumbers())



multiplyGivenNumbers <- function(x, y) {
  x*y
}

print(multiplyGivenNumbers(4, 5))
print(multiplyGivenNumbers(2, 3))


## Creates point graph with survey biomass and julian day for specific ground
### pass the survey ground into the function so it can change every time it is called

Ground = 'SB'
Ground = 'GB'

graphJulianSurveyBiomass <- function(surveyGround) {
  
  JulianAndBiomass <- subset(Survey_Factors, select=c("Survey_Date", "Ground", "Julian", "DFO_Estimate"))
  JulianAndBiomass <- na.omit(JulianAndBiomass)
  JulianAndBiomass<- subset(JulianAndBiomass, Survey_Date < '2023-05-22') #As only one DFO factor for 2023 is in the system, removed to keep years nice.
  
  ## BRIDGET - ground is now the surveyGround variable passed into the function
  Turnover <- subset(JulianAndBiomass, Ground==surveyGround)
  
  PointGraph <- ggplot(Turnover, aes(Julian, DFO_Estimate)) +geom_point() +geom_smooth(span = 1)
  print(PointGraph + labs(y = "Survey Biomass (mt)", x = "Julian Day"))
  
  summary(aov(DFO_Estimate ~ Julian, data = ScotsBay_Turnover))
}


##

vesselBiomass <- function(surveyGround) {

  VesselsBiomass <- subset(Survey_Factors, select=c("Survey_Date", "No_of_Vessels", "Julian", "DFO_Estimate", "Ground"))
  VesselsBiomass <- na.omit(VesselsBiomass)
  VesselsBiomass <- subset(VesselsBiomass, Ground == surveyGround, Survey_Date < '2023-05-22')
  VesselsBiomass$No_of_Vessels <- as.factor(VesselsBiomass$No_of_Vessels)
  
  BoxplotVessels <-boxplot(VesselsBiomass$DFO_Estimate~VesselsBiomass$No_of_Vessels, xlab="Number of Vessels in Survey", ylab="Survey Biomass (mt)")
  
  summary(aov(DFO_Estimate ~ No_of_Vessels, data = VesselsBiomass))
  
}


graphJulianSurveyBiomass(Ground)
graphJulianSurveyBiomass('GB')

vesselBiomass(Ground)
vesselBiomass(Ground)


