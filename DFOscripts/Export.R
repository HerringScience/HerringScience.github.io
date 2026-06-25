
# Script to integrate acoustic editing by survey
# Gives you the region exports that can be then used to calculate survey biomass
# The exports are located in the survey folder, under 'Exports'


# December 18/20 Scripts work fine when you only have .raw files but not properly when you have .HAC files as well.

# Make sure you don't have Echoview open when you run this script or it won't work properly.

# Should work to add mapping component (grid) and to choose what variables I want to export 


# Load Packages, etc.
  install.packages("RDCOMClient",repos="http://www.omegahat.net/R")
  if (!requireNamespace("devtools",quietly=TRUE)) install.packages("devtools") 
  devtools::install_github("AustralianAntarcticDivision/EchoviewR", build_opts = c("--no-resave-data", "--no-manual"), force = TRUE)
  rm(list = ls(all = TRUE))
  library(EchoviewR)
  library(RDCOMClient)

  
  library(EchoviewR)
  library(RDCOMClient)
  

# Set up Pathways  
  
  # Won't change inbetween surveys
    calfile.path = 'C:/Users/herri/OneDrive/Documents/Calibrations/2020/'
  
  # These will need to be edited every survey to specify the folder by date
    home.path = 'C:/Users/herri/OneDrive/Documents/Surveys/2020/2020-06-27/'
    evfile.path = home.path  
    data.path = 'C:/Users/herri/OneDrive/Documents/Surveys/2020/2020-06-27/Data/'
    
  
  # These folders will be created within the script
    transect.path = 'C:/Users/herri/OneDrive/Documents/Surveys/2020/2020-06-27/Transects/'
    region.path = 'C:/Users/herri/OneDrive/Documents/Surveys/2020/2020-06-27/Regions/'
    export.path = 'C:/Users/herri/OneDrive/Documents/Surveys/2020/2020-06-27/Exports/' 
    map.path =  'C:/Users/herri/OneDrive/Documents/Surveys/2020/2020-06-27/Map/' 

  #template.path = 'D:/Trawl Survey Acoustic Estimates/T2018023_4VW/RVsurvey_acoustics_template.EV'

# Create the folders
  dir.create(region.path) 
  dir.create(transect.path)
  dir.create(export.path)
  dir.create(map.path)
  
# Get a list of .EV files in your data directory
  evFiles = dir(evfile.path,full.names=T,pattern='.EV$')

# Create Functions: call files to determine calibration list - using string to determine vessel calibration
  right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
  left = function (string,char){substr(string,1,char)}

        #substrRight <- function(x, n){  substr(x, nchar(x)-n+1, nchar(x))}

# Setting up calibrations
# Need to change the first number according to the pathway
# Isolate the vessel two letters (i.e. LM)

      
          cal_file_search <- NULL
          
          for(i in 1:length(evFiles)){
            cal_file_search [i] <- right(left(evFiles[i],60),2)
          }
          
          cal_file_search

          calFiles = dir(calfile.path,full.names=T,pattern='.ecs$')
          calFiles <- grep("Cal_2020.e", calFiles, value =TRUE, fixed =TRUE) 

                ordered_calFiles <- NULL
                for(i in 1:length(calFiles)){
                  ordered_calFiles[i] <- grep(cal_file_search[i], calFiles, value = TRUE, fixed = TRUE)
                }
                
                ordered_calFiles
              

                
                #check if they match
  evFiles
  rawFiles = dir(data.path, full.names=TRUE, pattern='.raw$')
  hacFiles = dir(data.path, full.names=TRUE, pattern='.HAC$')
  rawFiles <- c(rawFiles,hacFiles)

# Need to code this for time that there is more than one Raw File.
# edit the larger number, looking to isolate the first 4 characters, vessel name_ and first letter of the month. This is to account for the face that there can be multiple data files for one EV.


    Files_Search<-NULL
    for(i in 1:length(rawFiles)){
    Files_Search[i] <- right(left(rawFiles[i],67),4)
}

    Files_Search #check to see if there are doubles
    Files_Search <- unique(Files_Search) #eliminate the duplicates
    rawFileslist<-list()

      for(i in 1:length(evFiles)){
        rawFileslist[[i]] <- grep(Files_Search[i], rawFiles, value = TRUE, fixed = TRUE)
      }


# HAC files does not have a cal, use this script to point to   NULL CAL 
              # if there are no HAC files:

            ordered_calFiles <- c(ordered_calFiles, rep("", length(hacFiles)))
            lengthRawFiles <-(length(rawFiles) - length(hacFiles))
            lengthHacFiles <- length(rawFiles)
            #for hac files go from lengthRawFiles+1:lengthHacFiles 
            
            if(lengthHacFiles == lengthRawFiles){
              lengthHacFiles <- 0
            }

            # Why is this showing 9, should be 1 (For German Bank surveys).
            lengthHacFiles
            


# Export Region Logs (Transects)
for(i in 1:length(rawFileslist)){  
  
  # Open Echoview COM connection
  EvAppObj = COMCreate('EchoviewCom.EvApplication')
  
  #open template for export
  EVFile <- EvAppObj$OpenFile(evFiles[i])
    #EVFile <- EvAppObj$OpenFile(evFiles[1])
  
  EVFile <- EvAppObj$OpenFile(evFiles[i])
    #EVFile <- EvAppObj$OpenFile(evFiles[1])
  
  
  #Export Regions
  EVRegionClass <- EVFile[["RegionClasses"]]
  EVTransect<-EVRegionClass$FindByName("Transect")
  EVTransect$ExportDefinitions(paste0(transect.path,cal_file_search[i],'.EVR'))
  #EVTransect$ExportDefinitions(paste0(transect.path,cal_file_search[1],'.EVR'))
  
  EvAppObj$Quit()
}


#Export Region Logs
for(i in 1:length(rawFileslist)){  
  
  # Open Echoview COM connection
  EvAppObj = COMCreate('EchoviewCom.EvApplication')
  
  #open template for export
  EVFile <- EvAppObj$OpenFile(evFiles[i])
  
  #Export Regions
  EVRegionClass <- EVFile[["RegionClasses"]]
  EVTransect<-EVRegionClass$FindByName("Unclassified regions")
  EVTransect$ExportDefinitions(paste0(region.path,cal_file_search[i],'.EVR'))
  EvAppObj$Quit()
}

transectFiles = dir(transect.path,full.names=T,pattern='.EVR$')
transectFiles
regionFiles = dir(region.path,full.names=T,pattern='.EVR$')
regionFiles







#For EV files with RAW files. Meat and Potatoes for Exporting



for(i in 1:lengthRawFiles){  
  
  # Open Echoview COM connection
      EvAppObj = COMCreate('EchoviewCom.EvApplication')
      
      #open template for export
      EVFile <- EvAppObj$OpenFile(evFiles[i])
    
      
  
      
            # Load existing EV File with regions.
            EVClearRawData(EVFile=EVFile,filesetName='Fileset 1')
            
            EVRegionClass <- EVFile[["RegionClasses"]]
            EVunclassified <-EVRegionClass$FindByName("Unclassified regions")
            EVtransect <-EVRegionClass$FindByName("Transect")
            EVDeleteRegionClass(EVFile=EVFile,EVunclassified)
            EVDeleteRegionClass(EVFile=EVFile,EVtransect)
            
                # Add raw data to the new file
                EVAddRawData(EVFile, 'Fileset1', unlist(rawFileslist[i]))
                # Add calibration files
                EVAddCalibrationFile(EVFile, 'Fileset1', ordered_calFiles[i])
                
                      #Import Regions
                      #open Files for import
                      EVFile$Import(transectFiles[i])
                      EVFile$Import(regionFiles[i])
                    
                      #perform export integretation on region classes called "Transect" only.
                      EVVar <- EVFile[["Variables"]]$FindByName("Processed data 1")
                      if(is.null(EVVar) == TRUE){
                        EVVar <- EVFile[["Variables"]]$FindByName("Attenuated signal removal 1")
                      }
                      if(is.null(EVVar) == TRUE){
                        EVVar <- EVFile[["Variables"]]$FindByName("Sv raw pings T1")
                      }
                     # EVminThresholdSet(EVVar, -70)
                      varAnaly <- EVVar[["Properties"]][["Analysis"]]
                      varAnaly[["ExcludeAbove"]] <- "Surface" 
                      varAnaly[["ExcludeBelow"]] <- "Bottom" 
                      
                    # Add grid  
                      EVChangeVariableGrid = function(EVFile,acoVarName = EVVar,timeDistanceGridType = 5,depthGridType = 1,timeDistanceGridDistance = 1000, depthGridDistance = 150, EVLineName = NULL, showenum = TRUE)
                      
                                        EVRegionClass <- EVFile[["RegionClasses"]]
                                        EVTransect<-EVRegionClass$FindByName("Transect")
                                        
                                        
                                        
                                      # Export by Region                                  
                                        EVVar$ExportIntegrationByRegions(paste0(export.path,cal_file_search[i],'.csv'),EVTransect)
                                       
                                        

                                        EvAppObj$Quit()
                                      }










# By regions and cell

# Export by Region by cell



for(i in 1:lengthRawFiles){  
  
  # Open Echoview COM connection
  EvAppObj = COMCreate('EchoviewCom.EvApplication')
  
  #open template for export
  EVFile <- EvAppObj$OpenFile(evFiles[i])
  
 # EVIntegrationByRegionsByCellsExport(EVFile, EVVar, "Transect", exportFn = paste0(map.path, cal_file_search[i], '.csv'))
  

  EvAppObj$Quit()
  
  
}














## End(Not run)




### Finished here unless you have .HAC files:







# This script

#For EV files with HAC files. 
for(i in lengthRawFiles+1:lengthHacFiles){  
  
  # Open Echoview COM connection
  EvAppObj = COMCreate('EchoviewCom.EvApplication')
  
  #open template for export
  EVFile <- EvAppObj$OpenFile(evFiles[i])
  
  # Load existing EV File with regions.
  EVClearRawData(EVFile=EVFile,filesetName='Fileset 1')
  # Add raw data to the new file
  EVAddRawData(EVFile, 'Fileset1', unlist(rawFileslist[i]))
  
  EVRegionClass <- EVFile[["RegionClasses"]]
  EVunclassified <-EVRegionClass$FindByName("Unclassified regions")
  EVtransect <-EVRegionClass$FindByName("Transect")
  EVDeleteRegionClass(EVFile=EVFile,EVunclassified)
  EVDeleteRegionClass(EVFile=EVFile,EVtransect)
  
  #Import Regions
  #open Files for import
  EVFile$Import(transectFiles[i])
  EVFile$Import(regionFiles[i])
  
  #perform export integretation on region classes called "Transect" only.
  EVVar <- EVFile[["Variables"]]$FindByName("Processed data 1")
  if(is.null(EVVar) == TRUE){
    EVVar <- EVFile[["Variables"]]$FindByName("Attenuated Signal Removal 1")
  }
  if(is.null(EVVar) == TRUE){
    EVVar <- EVFile[["Variables"]]$FindByName("Sv raw pings T1")
  }
  if(is.null(EVVar) == TRUE){
    EVVar <- EVFile[["Variables"]]$FindByName("Sv raw pings T1")
  }
  if(is.null(EVVar) == TRUE){
    EVVar <- EVFile[["Variables"]]$FindByName("Sv pings")
  }
  EVminThresholdSet(EVVar, -70)
  varAnaly <- EVVar[["Properties"]][["Analysis"]]
  varAnaly[["ExcludeAbove"]] <- "Surface" 
  varAnaly[["ExcludeBelow"]] <- "Bottom" 
  
  EVRegionClass <- EVFile[["RegionClasses"]]
  EVTransect<-EVRegionClass$FindByName("Transect")
  EVVar$ExportIntegrationByRegions(paste0(export.path,cal_file_search[i],'.csv'),EVTransect)
  
  EvAppObj$Quit()
}



