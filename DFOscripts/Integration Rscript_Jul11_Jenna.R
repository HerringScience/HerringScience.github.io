
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "mapproj", "ggmap", "sp","geosphere")

install.packages("RDCOMClient",repos="http://www.omegahat.net/R")

if (!requireNamespace("devtools",quietly=TRUE)) install.packages("devtools") 


###
devtools::install_github("AustralianAntarcticDivision/EchoviewR", build_opts = c("--no-resave-data", "--no-manual"), force = TRUE)


rm(list = ls(all = TRUE))
library(EchoviewR)
library(RDCOMClient)

home.path = 'D:/4VWXHER Acoustics/Jenna EVs/Scots bay/'
data.path = 'F:/2016 to present acoustics/2020 Acoustics/Scots Bay/2020-07-11/'


#I assume Claire wants the integrations here.
export.path = 'D:/4VWXHER Acoustics/Jenna EVs/Scots bay/Integration/'  
evfile.path ='D:/4VWXHER Acoustics/Jenna EVs/Scots bay/'
calfile.path = 'F:/2016 to present acoustics/2020 Acoustics/Calibration Files/'
transect.path = 'F:/2016 to present acoustics/2020 Acoustics/Scots Bay/2020-07-11/Transect Regions/'
region.path = 'D:/4VWXHER Acoustics/Jenna EVs/Scots Bay/Local Regions/'
#template.path = 'D:/Trawl Survey Acoustic Estimates/T2018023_4VW/RVsurvey_acoustics_template.EV'


dir.create(region.path) # creates folder'

# Get a list of .EV files in your data directory
evFiles = dir(evfile.path,full.names=T,pattern='.EV$')

#call files to determine calibration list - using string to determine vessel calibration
right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left = function (string,char){substr(string,1,char)}
#substrRight <- function(x, n){  substr(x, nchar(x)-n+1, nchar(x))}


#Setting up calibrations
cal_file_search <- NULL
for(i in 1:length(evFiles)){
  cal_file_search [i] <- right(left(evFiles[i],43),2)
}
cal_file_search
calFiles = dir(calfile.path,full.names=T,pattern='.ecs$')
calFiles <- grep("Cal_2020.e", calFiles, value =TRUE, fixed =TRUE) #really odd R bug, had to code it this way.
ordered_calFiles <- NULL
for(i in 1:length(calFiles)){
  ordered_calFiles[i] <- grep(cal_file_search[i], calFiles, value = TRUE, fixed = TRUE)
}


#check if they match
evFiles #so TM is still a .HAC file... should convert to .ecs to make life easier.
rawFiles = dir(data.path, full.names=TRUE, pattern='.raw$')
hacFiles = dir(data.path, full.names=TRUE, pattern='.HAC$')
rawFiles <- c(rawFiles,hacFiles)

#need to code this for time that there is more than one Raw File.
Files_Search<-NULL
for(i in 1:length(rawFiles)){
  Files_Search[i] <- right(left(rawFiles[i],69),4)
}
Files_Search #check to see if there are doubles
Files_Search <- unique(Files_Search) #eliminate the duplicates
rawFileslist<-list()
for(i in 1:length(evFiles)){
  rawFileslist[[i]] <- grep(Files_Search[i], rawFiles, value = TRUE, fixed = TRUE)
}


#HAC files does not have a cal, use this script to point to NULL CAL 
ordered_calFiles <- c(ordered_calFiles, rep("", length(hacFiles)))
lengthRawFiles <-(length(rawFiles) - length(hacFiles))
lengthHacFiles <- length(rawFiles)
#for hac files go from lengthRawFiles+1:lengthHacFiles 

if(lengthHacFiles == lengthRawFiles){
  lengthHacFiles <- 0
}
lengthHacFiles
#Export Region Logs
for(i in 1:length(rawFileslist)){  
  
  # Open Echoview COM connection
  EvAppObj = COMCreate('EchoviewCom.EvApplication')
  
  #open template for export
  EVFile <- EvAppObj$OpenFile(evFiles[i])
  
  #Export Regions
  EVRegionClass <- EVFile[["RegionClasses"]]
  EVTransect<-EVRegionClass$FindByName("Transect")
  EVTransect$ExportDefinitions(paste0(transect.path,cal_file_search[i],'.EVR'))
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



#For EV files with RAW files. 
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
  EVminThresholdSet(EVVar, -70)
  varAnaly <- EVVar[["Properties"]][["Analysis"]]
  varAnaly[["ExcludeAbove"]] <- "Surface" 
  varAnaly[["ExcludeBelow"]] <- "Bottom" 
  
  EVRegionClass <- EVFile[["RegionClasses"]]
  EVTransect<-EVRegionClass$FindByName("Transect")
  EVVar$ExportIntegrationByRegions(paste0(export.path,cal_file_search[i],'.csv'),EVTransect)
  EvAppObj$Quit()
}



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



