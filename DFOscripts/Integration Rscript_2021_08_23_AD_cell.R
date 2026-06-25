rm(list = ls(all = TRUE))
library(EchoviewR)
library(RDCOMClient)
library(stringr)

home.path = 'F:/2016 to present acoustics/2021 Acoustics/German Bank/EV Files Allan/2021-08-23/'
data.path = 'F:/2016 to present acoustics/2021 Acoustics/German Bank/08-23-21/'
export.path = paste0(home.path,'Integrationbycell/')  #I assume Claire wants the integrations here.
evfile.path = home.path
calfile.path = 'F:/2016 to present acoustics/2021 Acoustics/Calibration Files/'
transect.path = 'F:/2016 to present acoustics/2021 Acoustics/German Bank/Transect Regions/2021-08-23/'
region.path = paste0(home.path,'Local Regions/')
#template.path = 'D:/Trawl Survey Acoustic Estimates/T2018023_4VW/RVsurvey_acoustics_template.EV'

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
  vessel_search [i] <- right(left(evFiles[i],84),2)
}
calFiles = dir(calfile.path,full.names=T,pattern='.ecs$')
#calFiles <- grep("Cal_2020.e", calFiles, value =TRUE, fixed =TRUE) #really odd R bug, had to code it this way.
ordered_calFiles <- NULL
for(i in 1:length(vessel_search)){
  ordered_calFiles[i] <- grep(vessel_search[i], calFiles, value = TRUE, fixed = TRUE)
}
ordered_calFiles<-ordered_calFiles[!is.na(ordered_calFiles)]
ordered_calFiles




#check if they match
evFiles #so TM is still a .HAC file... should convert to .ecs to make life easier.
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
  EvAppObj$Quit()
}

transectFiles = dir(transect.path,full.names=T,pattern='.EVR$')
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
  
  varGrid <-varObj[["Properties"]][["Grid"]]
  varGrid$SetTimeDistanceGrid(5, 926)
  varGrid$SetDepthRangeGrid(1, 400)
  
  varProp <- EVFile[["Properties"]][["Export"]][["Variables"]]
  Ex_P_PRC_NASC <- varProp$Item("PRC_NASC")
  Ex_P_PRC_NASC[["Enabled"]] <- TRUE
  ExPropLat_M <- varProp$Item("Lat_M")
  ExPropLat_M[["Enabled"]] <- TRUE
  ExPropLon_M <- varProp$Item("Lon_M")
  ExPropLon_M[["Enabled"]] <- TRUE
  
  
  EVRegionClass <- EVFile[["RegionClasses"]]
  EVTransect<-EVRegionClass$FindByName("Transect")
  EVVar$ExportIntegrationByRegionsByCells(paste0(export.path,vessel_search[i],'.csv'),EVTransect)
  EvAppObj$Quit()
}
