

## Calculate new biomass for 2020 using updated TS and area



# Scots Bay #1 June 2, 2020
  regions = read.table("RegionJun02_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
  
  trans = transects(x= regions, TS38 =-35.2581709, TS50 = -35.3654409 )
  
  # Specify the survey you are doing an analysis for  
  trans_survey= trans[which(trans$Survey_date == "Jun02_2020"), ]
  
      # QC
      unique(trans_survey$Survey_date)
      unique(trans_survey$Vessel)
      unique(trans_survey$Transect)
      
      ids=c("T04", "T05")
  
  
        # Northern Box
        trans_survey1=trans_survey[which(trans_survey$Transect != "T04"), ]
        trans_survey2=trans_survey1[which(trans_survey1$Transect != "T05"), ]
        
        # Main
        trans_survey3 = subset(trans_survey, (trans_survey$Transect_No %in% ids))
              
              # Results
              resultsa = biomassCalc(x = trans_survey3, areaKm = 91.972)
              a = unique(resultsa$total_biomass)
              
              resultsb = biomassCalc(x = trans_survey2, areaKm = 60.147)
              b = unique(resultsb$total_biomass)
              
              a+b
              
                #13,921              
              
              
  
# Scots Bay #2 June 13, 2020
  regions = read.table("RegionJune13_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
  
  trans = transects(x= regions, TS38 =-35.48241173, TS50 = -35.58968173 )
  
  # Specify the survey you are doing an analysis for  
  trans_survey= trans[which(trans$Survey_date == "Jun13_2020"), ]
  
      # QC
      unique(trans_survey$Survey_date)
      unique(trans_survey$Vessel)
      
      # remove FM
      trans_survey1=trans_survey[which(trans_survey$Vessel != "FM"), ]
      
      # Northern
      northern=trans_survey[which(trans_survey$Vessel == "FM"), ]
      
          # Results
          resultsa = biomassCalc(x = trans_survey1, areaKm = 623.5986)
          a = unique(resultsa$total_biomass)
          
          resultsb = biomassCalc(x = northern, areaKm = 77.94116)
          b = unique(resultsb$total_biomass)
          a+b
          
            #11,906
          
          
# Scots Bay #3 June 27, 2020
          regions = read.table("RegionJun27_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
          
          trans = transects(x= regions, TS38 = -35.5, TS50 = -35.609 )
          
          ids = c("BP")
          mastersub = trans[which((trans$Vessel %in% ids)), ]
          
          # Specify the survey you are doing an analysis for  
          trans_survey= trans[which(trans$Survey_date == "Jun27_2020"), ]
          
          # QC
          unique(trans_survey$Survey_date)
          unique(trans_survey$Vessel)
          
          # remove FM
          trans_survey1=trans_survey[which(trans_survey$Vessel != "BP"), ]
          
          # Northern
          northern=trans_survey[which(trans_survey$Vessel == "BP"), ]
          
          # Results
          resultsa = biomassCalc(x = trans_survey1, areaKm = 640.7993)
          a = unique(resultsa$total_biomass)
          
          resultsb = biomassCalc(x = northern, areaKm = 83.27)
          b = unique(resultsb$total_biomass)
          a+b

            #42,174          
          
          
# Scots Bay #4 July 11, 2020
          
          regions = read.table("RegionJul11_2020_2.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
          
          trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.60927 )
          
          # Specify the survey you are doing an analysis for  
          trans_survey= trans[which(trans$Survey_date == "Jul11_2020"), ]
          
          # QC
          unique(trans_survey$Survey_date)
          unique(trans_survey$Vessel)
          
                # remove C1 and LJ
                trans_survey1=trans_survey[which(trans_survey$Vessel != "C1"), ]
                trans_survey2=trans_survey1[which(trans_survey1$Vessel != "LJ"), ]
                
                # Northern
                northern=trans_survey[which(trans_survey$Vessel == "C1"), ]
                
                # eastern
                eastern=trans_survey[which(trans_survey$Vessel == "LJ"), ]
          
                          resultsa = biomassCalc(x = trans_survey2, areaKm = 640.79)
                          j  = unique(resultsa$total_biomass)
                          
                          resultsb = biomassCalc(x = northern, areaKm = 77.15)
                          g = unique(resultsb$total_biomass)
                          
                          resultsc = biomassCalc(x = eastern, areaKm = 113.06)
                          m = unique(resultsc$total_biomass)
                          
                          
                             j+g+m
                      
                             
                                #28,119                             
                             
# Scots Bay #5 July 26, 2020
                             regions = read.table("Region_Jul26_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
                           
                             trans = transects(x= regions, TS38 = -35.44747951, TS50 = -35.55474951 )
                             trans_survey= trans[which(trans$Survey_date == "Jul26_2020"), ]
                             
                             # QC
                             unique(trans_survey$Survey_date)
                             unique(trans_survey$Vessel)
                             
                             # remove MS
                             trans_survey1=trans_survey[which(trans_survey$Vessel != "MS"), ]
                             
                             # eastern
                             eastern=trans_survey[which(trans_survey$Vessel == "MS"), ]
                             
                             resultsa = biomassCalc(x = trans_survey1, areaKm = 629.72)
                             j  = unique(resultsa$total_biomass)
                             
                             
                             resultsc = biomassCalc(x = eastern, areaKm = 113.06)
                             m = unique(resultsc$total_biomass)
                             
                             
                             j+m
                             
                                #23,578                             
                             
                             
                             
                             
                             
# Scots Bay #6 August 9, 2020
                             
                             regions = read.table("Region_Aug09_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
                             
                             trans = transects(x= regions, TS38 = -35.37598998, TS50 = -35.48325998)
                             trans_survey= trans[which(trans$Survey_date == "Aug08_2020"), ]
                             
                             # QC
                             unique(trans_survey$Survey_date)
                             unique(trans_survey$Vessel)
                             
                             # Results
                             resultsa = biomassCalc(x = trans_survey, areaKm = 630.1874)
                             unique(resultsa$total_biomass)
                             
                                #40,735                             
                             
                             
                             
# Scots Bay #7 August 23, 2020
                             
                             	
                             
                             
                             regions = read.table("Region_Aug23_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
                             
                             trans = transects(x= regions, TS38 =-35.33862507, TS50 =-35.44589507)
                             # Specify the survey you are doing an analysis for  
                             trans_survey= trans[which(trans$Survey_date == "Aug23_2020"), ]
                             
                             # QC
                             unique(trans_survey$Survey_date)
                             unique(trans_survey$Vessel)
                             resultsa = biomassCalc(x = trans_survey, areaKm = 630.1874)
                             unique(resultsa$total_biomass)
                             
                              #27,132                             
                             
                             
# Scots Bay #8 September 6, 2020
                             
                             
                             
                             regions = read.table("RegionSep06_20.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
                             
                             
                             
                             
                             trans = transects(x= regions, TS38 = -35.17759287, TS50 = 	-35.28486287)
                             
                             ids_main = c("T01", "T02")
                             main = trans[which((trans$Transect %in% ids_main)), ]
                             
                             # Specify the survey you are doing an analysis for  
                             trans_survey= main[which(main$Survey_date == "Sep06_2020"), ]
                             
                             # QC
                             unique(trans_survey$Survey_date)
                             unique(trans_survey$Vessel)
                             
                             resultsa = biomassCalc(x = trans_survey, areaKm = 630.1874)
                             unique(resultsa$total_biomass)
                             
                             
                                  #18,742                             
                             
                             
                             
                             
                             
                             
                             
                             
# Scots Bay #9 September 20, 2020
                             
                             regions = read.table("RegionSep20_20.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
                             
                             trans = transects(x= regions, TS38 =-35.40201958
, TS50 = -35.50928958
 )
                             
                             trans_survey= trans[which(trans$Survey_date == "Sep20_2020"), ]
                             
                             # QC
                             unique(trans_survey$Survey_date)
                             unique(trans_survey$Vessel)
                             resultsa = biomassCalc(x = trans_survey, areaKm = 319.8)
                             unique(resultsa$total_biomass)
                                      #9,896                             
                             
                             
          
          