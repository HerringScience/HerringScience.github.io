



rm(list = ls(all = TRUE))
library(EchoviewR)
library(RDCOMClient)
library(stringr)

home.path = 'C:/Users/herri/OneDrive/Documents/Surveys/2022/2022-05-29/'
data.path = 'C:/Users/herri/OneDrive/Documents/Surveys/2022/2022-05-29/'
export.path = paste0(home.path,'Integration/') 
evfile.path = home.path
calfile.path = 'C:/Users/herri/OneDrive/Documents/Calibrations/2022/'
transect.path =  paste0(data.path,'Transect Regions/')
region.path = paste0(home.path,'Local Regions/')
#template.path = 'D:/Trawl Survey Acoustic Estimates/T2018023_4VW/RVsurvey_acoustics_template.EV'
dir.create(transect.path)
dir.create(export.path)
dir.create(region.path)

# Get a list of .EV files in your data directory
evFiles = dir(evfile.path,full.names=T,pattern='.EV$')

#call files to determine calibration list - using string to determine vessel calibration
right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left = function (string,char){substr(string,1,char)}
#substrRight <- function(x, n){  substr(x, nchar(x)-n+1, nchar(x))}


#Setting up calibrations
vessel_search <- NULL
for(i in 1:length(evFiles)){
  vessel_search [i] <- right(left(evFiles[i],60),2) 
  #This will need to be modified to fit the characters where the prefixes are located (i.e., "BP")
}

vessel_search


cal_file_search<-vessel_search
cal_file_search
calFiles = dir(calfile.path,full.names=T,pattern='.ecs$')
calFiles <- grep("cal_2022.e", calFiles, value =TRUE, fixed =TRUE) #really odd R bug, had to code it this way.
ordered_calFiles <- NULL
for(i in 1:length(cal_file_search)){
  ordered_calFiles[i] <- grep(cal_file_search[i], calFiles, value = TRUE, fixed = TRUE)
}
ordered_calFiles<-ordered_calFiles[!is.na(ordered_calFiles)]
ordered_calFiles



#check if they match
evFiles 
rawFiles = dir(data.path, full.names=TRUE, pattern='.raw$')


rawFileslist<-list()
for(i in 1:length(evFiles)){
  rawFileslist[[i]] <- grep(vessel_search[i], rawFiles, value = TRUE, fixed = TRUE)
}

rawFileslist


#Export Region Logs
for(i in 1:length(rawFileslist)){  
  
  # Open Echoview COM connection
  EvAppObj = COMCreate('EchoviewCom.EvApplication')
  
  #open template for export
  EVFile <- EvAppObj$OpenFile(evFiles[i])
  
  #Export Regions
  EVRegionClass <- EVFile[["RegionClasses"]]
  EVTransect<-EVRegionClass$FindByName("Unclassified regions")
  EVTransect$ExportDefinitions(paste0(region.path,vessel_search[i],'.EVR'))
  
  #Export Regions
  EVRegionClass <- EVFile[["RegionClasses"]]
  EVTransect<-EVRegionClass$FindByName("Transect")
  EVRegionKeep <- EVFile[["Regions"]]
  
  EVTransect$ExportDefinitions(paste0(transect.path,vessel_search[i],'.EVR'))
  
  EvAppObj$Quit()
}


transectFiles = dir(transect.path,full.names=T,pattern ='.EVR$')
transectFiles
regionFiles = dir(region.path,full.names=T,pattern='.EVR$')
regionFiles



#For EV files with RAW files. 
for(i in 1:length(rawFileslist)){  
  
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

  EVFile$Import(regionFiles[i])
  EVFile$Import(transectFiles[i])
  
  
  
  #perform export integretation on region classes called "Transect" only.
  EVVar <- EVFile[["Variables"]]$FindByName("Processed data 1")
  if(is.null(EVVar) == TRUE){
    EVVar <- EVFile[["Variables"]]$FindByName("Attenuated Signal Removal 1")
  }
  if(is.null(EVVar) == TRUE){
    EVVar <- EVFile[["Variables"]]$FindByName("Sv raw pings T1")
  }
  EVminThresholdSet(EVVar, -70)
  
  varObj = EVVar
  varDat <- varObj[["Properties"]][["Data"]]
  preThresApplyFlag <- varDat$ApplyMaximumThreshold()
  varDat[["ApplyMaximumThreshold"]] <- TRUE
  postThresApplyFlag <- varDat$ApplyMaximumThreshold()
  varDat[["LockSvMaximum"]] <- FALSE
  varDat[["MaximumThreshold"]] <- -10
  varAnaly <- EVVar[["Properties"]][["Analysis"]]
  varAnaly[["ExcludeAbove"]] <- "Surface Line"  
  varAnaly[["ExcludeBelow"]] <- "Bottom offset - 0.5 meters" 
  
  EVRegionClass <- EVFile[["RegionClasses"]]
  EVTransect<-EVRegionClass$FindByName("Transect")
  EVVar$ExportIntegrationByRegions(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
  EvAppObj$Quit()
}

#Remember to close Echoview

