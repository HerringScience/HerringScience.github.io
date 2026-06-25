

## German Bank 2020

# August 16, 2020

                                        regions = read.table("Region_Aug18_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
                                        trans = transects(x= regions, TS38 =-34.01641696, TS50 = -34.12368696)
                                        
                                        # Specify the survey you are doing an analysis for  
                                        trans_survey= trans[which(trans$Survey_date == "Aug16_2020"), ]
                                        
                                        # QC
                                        unique(trans_survey$Survey_date)
                                        unique(trans_survey$Vessel)
                                        
                                        # remove seal vessels
                                        
                                        ids_seal = c("T03" )
                                        sealBox = trans_survey[which((trans_survey$Transect_No == ids_seal)), ]
                                        
                                        ids_gb = c("T01", "T02")
                                        gbBox = trans_survey[which((trans_survey$Transect_No %in% ids_gb)), ]
                                        # German Bank        
                                                resultsa = biomassCalc(x = gbBox, areaKm =853.5)
                                                unique(resultsa$total_biomass)
                                                # 31,047.23 mt
                                        
                                        # Seal Island
                                                resultsb = biomassCalc(x = sealBox, areaKm =357.2)
                                                unique(resultsb$total_biomass)
                                                # 449 mt


## August 31, 2020
        
        regions = read.table("RegionAug31_20.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
        
        trans = transects(x= regions, TS38 =-35.3251306, TS50 = -35.4324006)
        
        
        # Specify the survey you are doing an analysis for  
        trans_survey= trans[which(trans$Survey_date == "Aug31_2020"), ]
        
        # QC
        unique(trans_survey$Survey_date)
        unique(trans_survey$Vessel)
        unique(trans_survey$Transect)
        
        # remove seal vessels
        
        ids_seal = c("TM", "MS" )
        sealBox = trans_survey[which((trans_survey$Vessel %in% ids_seal)), ]
        
        ids_gb = c("C1","BP", "FM", "LJ", "LM", "LB","SL")
        gbBox = trans_survey[which((trans_survey$Vessel %in% ids_gb)), ]
        
        resultsa = biomassCalc(x = gbBox, areaKm =853.5)
        unique(resultsa$total_biomass)
        # 28,052 mt
        
        resultsb = biomassCalc(x = sealBox, areaKm =357.2)
        unique(resultsb$total_biomass)
        # 602 mt

        
        
        
        
## September 13, 2020
        
        regions = read.table("RegionSep13_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
        
        trans = transects(x= regions, TS38 =-35.23368608, TS50 = -35.34095608)
        
        # Specify the survey you are doing an analysis for  
        trans_survey= trans[which(trans$Survey_date == "Sep13_2020"), ]
        
        # QC
        unique(trans_survey$Survey_date)
        unique(trans_survey$Vessel)
        unique(trans_survey$Transect)
        
        # remove seal vessels
        ids_seal = c("FM", "LJ" )
        sealBox = trans_survey[which((trans_survey$Vessel %in% ids_seal)), ]
        
        ids_gb = c("C1","BP", "MS", "TM", "LM", "LB","SL")
        gbBox = trans_survey[which((trans_survey$Vessel %in% ids_gb)), ]
                         
                        # German Bank
                        resultsa = biomassCalc(x = gbBox, areaKm =853.5)
                        unique(resultsa$total_biomass)
                        # 65,219
                        
                        resultsb = biomassCalc(x = sealBox, areaKm =357.2)
                        unique(resultsb$total_biomass)
                        # 214
                        
                        
                        
                        
        
## September 27, 2020
        
        regions = read.table("RegionSep27_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
        
        trans = transects(x= regions, TS38 =-35.22249942 , TS50 = -35.32976942)
        
        # Specify the survey you are doing an analysis for  
        trans_survey= trans[which(trans$Survey_date == "Sep27_2020"), ]
        
        # QC
        unique(trans_survey$Survey_date)
        unique(trans_survey$Vessel)
        unique(trans_survey$Transect)
        
        # remove seal vessels
        
        ids_seal = c("LB", "C1" )
        sealBox = trans_survey[which((trans_survey$Vessel %in% ids_seal)), ]
        
        ids_gb = c("FM","BP", "MS", "TM", "LM", "LJ","SL")
        gbBox = trans_survey[which((trans_survey$Vessel %in% ids_gb)), ]
        
        # German Bank
        resultsa = biomassCalc(x = gbBox, areaKm =853.5)
        unique(resultsa$total_biomass)
        # 15,507
        
        resultsb = biomassCalc(x = sealBox, areaKm =357.2)
        unique(resultsb$total_biomass)
        # 1,689
        
        
        
        
        
## October 11, 2020
        
        regions = read.table("Region_Oct11_20.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
        
        	
        
        trans = transects(x= regions, TS38 =-35.266771 , TS50 = -35.374041)
        # Specify the survey you are doing an analysis for  
        trans_survey= trans[which(trans$Survey_date == "Oct11_2020"), ]
        
        # QC
        unique(trans_survey$Survey_date)
        unique(trans_survey$Vessel)
        
        ids_seal = c("T03" )
        sealBox = trans_survey[which((trans_survey$Transect_No == ids_seal)), ]
        
        ids_gb = c("T01", "T02")
        gbBox = trans_survey[which((trans_survey$Transect_No %in% ids_gb)), ]
        
        resultsa = biomassCalc(x = gbBox, areaKm =853.5)
        unique(resultsa$total_biomass)
        # 11,417 mt
        
        resultsb = biomassCalc(x = sealBox, areaKm =357.2)
        unique(resultsb$total_biomass)
        # 641 mt
        
        
## October 25, 2020
        
        regions = read.table("RegionOct25_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
        trans = transects(x= regions, TS38 =-35.5 , TS50 = -35.607 )
        trans_survey= trans[which(trans$Survey_date == "Oct25_2020"), ]
        unique(trans_survey$Survey_date)
        unique(trans_survey$Vessel)
        ids_seal = c("T03" )
        sealBox = trans_survey[which((trans_survey$Transect_No == ids_seal)), ]
        
        ids_gb = c("T01", "T02")
        gbBox = trans_survey[which((trans_survey$Transect_No %in% ids_gb)), ]
        
        resultsa = biomassCalc(x = gbBox, areaKm =853.5)
        unique(resultsa$total_biomass)
        #7,704
        
        resultsb = biomassCalc(x = sealBox, areaKm =357.2)
        unique(resultsb$total_biomass)
        # 637                
        