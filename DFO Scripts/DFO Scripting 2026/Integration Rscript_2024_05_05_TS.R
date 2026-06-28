rm(list = ls(all = TRUE))
library(EchoviewR)
library(RDCOMClient)
library(stringr)

source("E:/Acoustic Index Review Files/Rscripting and automation/2022_07_06_R_Integretation_Functions/Echoview_Integration_Functions.R")
surveydate <- '2024-05-05'

home.path = paste0('E:/2016 to present acoustics/2024 Acoustics/Scots Bay/',surveydate,'/EV files/')
data.path = paste0('E:/2016 to present acoustics/2024 Acoustics/Scots Bay/',surveydate,'/')
export.path = paste0('E:/Acoustic Index Review Files/Integration Versions/2022_07_06/2024/Scots Bay/',surveydate,'/') 
exportbycell.path = paste0('E:/Acoustic Index Review Files/Integrationbycell Versions/2022_07_06/2024/Scots Bay/',surveydate,'/') 
evfile.path = home.path
calfile.path = 'E:/Acoustic Index Review Files/Calibration Files/2024/'
transect.path = paste0('E:/Acoustic Index Review Files/Transect Regions/2024/Scots Bay/',surveydate,'/')
region.path = paste0('E:/Acoustic Index Review Files/Local Regions/2024/Scots Bay/',surveydate,'/')
dir.create(export.path)
dir.create(region.path)
dir.create(transect.path)

# Get a list of .EV files in your data directory
evFiles = dir(evfile.path,full.names=T,pattern='.EV$')

#call files to determine calibration list - using string to determine vessel calibration
right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left = function (string,char){substr(string,1,char)}
#substrRight <- function(x, n){  substr(x, nchar(x)-n+1, nchar(x))}


#Setting up calibrations
vessel_search <- NULL
for(i in 1:length(evFiles)){
  vessel_search [i] <- right(left(evFiles[i],76),2)
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
regionFiles<-Local_Regions_export(rawFileslist, evFiles,region.path,vessel_search)
regionFiles


transectFiles<-Transect_Regions_export(rawFileslist, evFiles,transect.path,vessel_search)
transectFiles



evFiles

Integration_original_2022_07_06(rawFileslist,
                                evFiles,
                                ordered_calFiles,
                                transectFiles,
                                regionFiles,
                                export.path,
                                vessel_search)


