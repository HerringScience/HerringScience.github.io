


# Analysis for single beam hydroacoustic survey data from the Scots Bay/German Bank regions

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")


# This is where the echoview exported data is read in:
# To use the original exports from the Desktop-Oct7 Folder put a 1 in front of each filename

# Skeleton
regions = read.table("Region.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
mapping = read.table("Map.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

# ----------------------------------------------------------------------------------------
          
          # For 2015-2016 data..
            # Regions Exports
          regionsRaw = read.table("Region_Total.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
          regionsHac = read.table("Region_totalHAC.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
          # Add Sep06-2016 data
          regionsRaw_nonstandard = read.table("Region_total2.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
          
          # Mapping Exports
          mappingRaw = read.table("Map_Total.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
          mappingHac = read.table("Map_totalHAC.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
          # Add Sep06-2016 data
          mappingRaw_nonstandard = read.table("Map_total2.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
                        
                        # Calibration Screw Up - Test (just one survey to get an idea of calibration mistake effect)
                        regions1 = read.table("LMafter.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
                        regions2 = read.table("LMbefore.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
                        trans1 = transects(x= regions1)
                        trans2 = transects(x= regions2)
                        
                        results1 = biomassCalc(x = trans1, areaKm = 100)
                        results2 = biomassCalc(x = trans2, areaKm = 100)
          
                  # Re-do with Calibration Screw Up Fixed (i.e Lady Melissa) for the entire 2016 survey year
                  regionsRaw = read.table("Region_June2017.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
                  
                  regionsHAC = read.table("Regions_HAC_June2017.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
                  
                  regions  = rbind(regionsRaw, regionsHAC)
                  
# 2017 Data
  # july 29, 2017 Scots Bay
          regions = read.table("RegionJul29.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
          
          mapping = read.table("MapJul29.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
          
          # august 21, 2017 German Bank
          regions = read.table("regionAug21German.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
          
          mapping = read.table("MapAug21German.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
          
                    # Basedata      
          regions  = rbind(regionsRaw, regionsHac, regionsRaw_nonstandard)
          mapping = rbind(mappingRaw, mappingHac, mappingRaw_nonstandard)    

#---------------------------------------------------------------------------
# Form regions database
  trans = transects(x= regions)
  
  
            # To investigate trens between areas
              trans$Area = NA 
              germanT=trans[which(trans$Y < 44), ]
              germanT$Area = ("German Bank")
              scotsT=trans[which(trans$Y > 44), ]
              scotsT$Area = ("Scots Bay")
  
              trans = rbind(germanT, scotsT)
head(trans)
# Specify the survey you are doing an analysis for  
  trans_survey= trans[which(trans$Survey_date == "Aug21_2017"), ]
  # QC
  unique(trans_survey$Survey_date)
  unique(trans_survey$Vessel)
  
  # remove SH from Oct 7
  trans_survey1=trans_survey[which(trans_survey$Vessel != "SH"), ]
 
   # Remove BP and DV for Jul 16, 2016
  trans_survey1=trans_survey[which(trans_survey$Vessel != "C1"), ]
  
  # Remove BP and for Jul 01, 2017
  trans_survey1=trans_survey[which(trans_survey$Vessel != "BP"), ]
  northern=trans_survey[which(trans_survey$Vessel == "BP"), ]
  
  
  main=trans_survey1[which(trans_survey1$Vessel != "BP"), ]
  


  
# Analysis for survey box-> trans_survey and spawning box-> minor_survey (if ran)
  
  resultsa = biomassCalc(x = trans_survey, areaKm = 653.58)
  unique(resultsa$total_biomass)

  resultsb = biomassCalc(x = northern, areaKm = 81.33)
  unique(resultsb$total_biomass)
  
  resultsc = biomassCalc(x = eastern, areaKm = 116.94)
  unique(resultsc$total_biomass)

#-------------------------------------------------------------------------------      
  # Run results
      tableA = resultTableA(x = trans_survey)
      tableB = resultTableB(x = trans_survey)
      tableC = resultTableC(x = resultsa)
      
      tableD = resultTableA(x = northern)
      tableE = resultTableB(x = northern)
      tableF = resultTableC(x = resultsb)
      
      tableG = resultTableA(x = eastern)
      tableH = resultTableB(x = eastern)
      tableI = resultTableC(x = resultsc)
      
      tableC$Layer = "Main Box"
      tableF$Layer = "Northern Box"
      tableI$Layer = "Eastern Box"
      
      A = rbind(tableA,tableD, tableG)
      B = rbind(tableB,tableE, tableH)
      C = rbind(tableC,tableF, tableI)
      
      # Scots
      # Only the main box
      write.table(tableA, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
      
      write.table(tableB, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      
      write.table(tableC, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      
      # All three boxes
       write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
      
      write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      
      write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      
      # German
      write.table(tableA, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
      
      write.table(tableB, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      
      write.table(tableC, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      
      
      
      
      
      
      ##
      

      
      tableS = rbind(tableC, tableD, tableE)
      
      write.table(resultsa, file= "results_Sep26.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
      
      write.table(tableA, file= "tableAJul01_2017.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      
      write.table(tableB, file= "tableBJul01_2017.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      
      write.table(tableC, file= "tableCJul01_2017.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
 #--------------------------------------------------------------------------------  
  
# Form mapping database
  map = mapDat(x = mapping)
  map$Area = NA
  
        # To investigate trens between areas
          germanM=map[which(map$Y < 44), ]
          germanM$Area = ("German Bank")
          scotsM=map[which(map$Y > 44), ]
          scotsM$Area = ("Scots Bay")
  
          map = rbind(germanM, scotsM)
  
  map_survey= map[which(map$Survey_date == "Jun09_2018"), ]
  head(map_survey)
  
#------------------------------------------------------------------------------

  
  
#------------------------------------------------------------------------------  
  # Survey specific work
  
  # For Oct 7 remove Silver Harvester data
    test=trans_survey[which(trans_survey$Vessel != "SH"), ]
    unique(trans_survey$Vessel)
    trans_survey=test  

    