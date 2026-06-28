rm(list = ls(all = TRUE))
#if (!require("remotes")) install.packages("remotes")
#remotes::install_github("jkylearmstrong-temple/RDCOMClient")
library(RDCOMClient)
#if (!require("devtools")) install.packages("devtools")
#devtools::install_github("AustralianAntarcticDivision/EchoviewR")
library(EchoviewR)
library(stringr)

surv="SB" #SB or GB or SI
year="2026"
day="21"
month="06"
surv.no="4"

source(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/DFO Scripts/DFO Scripting 2026/Echoview_Integration_Functions.R"))

home.path = paste0("C:/Users/", Sys.info()[7], "/Desktop/Echoview/Surveys/", year, "/", surv, surv.no, " ", day, "-", month, "-", year, "/")
data.path = paste0("C:/Users/", Sys.info()[7], "/Desktop/Echoview/Surveys/", year, "/", surv, surv.no, " ", day, "-", month, "-", year,"/Data/")
export.path = paste0("C:/Users/", Sys.info()[7], "/Desktop/Echoview/Surveys/", year, "/", surv, surv.no, " ", day, "-", month, "-", year, "/Export/")
exportbycell.path = paste0("C:/Users/", Sys.info()[7], "/Desktop/Echoview/Surveys/", year, "/", surv, surv.no, " ", day, "-", month, "-", year, "/By Cell/")
evfile.path = home.path
calfile.path = paste0("C:/Users/", Sys.info()[7], "/Desktop/Echoview/Calibration Files/", year)
transect.path = paste0("C:/Users/", Sys.info()[7], "/Desktop/Echoview/Surveys/", year, "/", surv, surv.no, " ", day, "-", month, "-", year, "/Transect/")
region.path = paste0("C:/Users/", Sys.info()[7], "/Desktop/Echoview/Surveys/", year, "/", surv, surv.no, " ", day, "-", month, "-", year, "/Region/")
dir.create(exportbycell.path)
dir.create(export.path)
dir.create(region.path)
dir.create(transect.path)

# Get a list of .EV files in your data directory
evFiles = dir(evfile.path,full.names=T,pattern='.EV$')

#call files to determine calibration list - using string to determine vessel calibration
right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left = function (string,char){substr(string,1,char)}

#Setting up calibrations
vessel_search <- NULL
for(i in 1:length(evFiles)){
  vessel_search [i] <- right(left(evFiles[i],62),2)
}
calFiles = dir(calfile.path,full.names=T,pattern='.ecs$')
ordered_calFiles <- NULL
for(i in 1:length(vessel_search)){
  ordered_calFiles[i] <- grep(vessel_search[i], calFiles, value = TRUE, fixed = TRUE)
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
transectFiles<-Transect_Regions_export(rawFileslist, evFiles,transect.path,vessel_search)

regionFiles<-Local_Regions_export(rawFileslist, evFiles,region.path,vessel_search)

Integration_original_2022_07_06(rawFileslist,
                                evFiles,
                                ordered_calFiles,
                                transectFiles,
                                regionFiles,
                                export.path,
                                vessel_search)

#DFO Original Pathways Repository

#source("E:/Acoustic Index Review Files/Rscripting and automation/2022_07_06_R_Integretation_Functions/Echoview_Integration_Functions.R")
#region.path = paste0('E:/Acoustic Index Review Files/Local Regions/2024/Scots Bay/',surveydate,'/')
#home.path = paste0('E:/2016 to present acoustics/2024 Acoustics/Scots Bay/',surveydate,'/EV files/')
#data.path = paste0('E:/2016 to present acoustics/2024 Acoustics/Scots Bay/',surveydate,'/')
#export.path = paste0('E:/Acoustic Index Review Files/Integration Versions/2022_07_06/2024/Scots Bay/',surveydate,'/') 
#exportbycell.path = paste0('E:/Acoustic Index Review Files/Integrationbycell Versions/2022_07_06/2024/Scots Bay/',surveydate,'/') 
#calfile.path = 'E:/Acoustic Index Review Files/Calibration Files/2024/'
#transect.path = paste0('E:/Acoustic Index Review Files/Transect Regions/2024/Scots Bay/',surveydate,'/')

#calFiles <- grep("Cal_2020.e", calFiles, value =TRUE, fixed =TRUE) #really odd R bug, had to code it this way.
#substrRight <- function(x, n){  substr(x, nchar(x)-n+1, nchar(x))}
