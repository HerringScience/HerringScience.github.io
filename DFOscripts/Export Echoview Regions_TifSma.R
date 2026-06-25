#This code was created by Allan Debertin and modified by Tiffany Small
#The purpose of this code is to export echoview regions as .EVR for transects that will be
#standardized from the transects created by Jenna Munden
#
#To run this code:
#             -Change the 'surveydate' to match what folder you are working in
#             -This code was created to export each vessels transect region by survey date
#             if the transects are different from survey to survey, please run for all regions.
#             -The regions will be saved in a folder in each of the survey date folders, please upload
#             the .EVR files in the dropbox/google drive where the survey data is shared

rm(list = ls(all = TRUE))
library(EchoviewR)
library(RDCOMClient)
library(stringr)

surveydate <- "2021-06-05" #YYYY-MM-DD ###date of survey


#CHANGE THESE TO MATCH YOUR HOME PATHS:
#This one is where your EV files are
home.path = (paste0("J:/Documents Back up/2021/",surveydate,"/"))

#home.path = (paste0('D:/4VWXHER Acoustics/Tiffany EV/2021/Scots Bay/2021-08-01/'))

vessel_folder <- list.files(home.path)

evfile.path = home.path #Change this path to where your EV files are located
region.path = paste0(evfile.path,'Local Regions/')


dir.create(region.path)#This creates a folder for all the EVR files to send me

# Get a list of .EV files in your data directory
evFiles = dir(evfile.path,full.names=T,pattern='.EV$')

#call files to determine calibration list - using string to determine vessel calibration
right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left = function (string,char){substr(string,1,char)}

vessel_search <- NULL
for(i in 1:length(evFiles)){
  vessel_search [i] <- right(left(evFiles[i],39),2)
}
vessel_search

#Export Region Logs
for(i in 1:length(evFiles)){  
  
  # Open Echoview COM connection
  EvAppObj = COMCreate('EchoviewCom.EvApplication')
  
  #open template for export
  EVFile <- EvAppObj$OpenFile(evFiles[i])
  
  #Export Regions
  EVRegionClass <- EVFile[["RegionClasses"]]
  EVTransect<-EVRegionClass$FindByName("Transect")
  EVTransect$ExportDefinitions(paste0(region.path,vessel_search[i],'.EVR'))
  EvAppObj$Quit()
}

regionFiles = dir(region.path,full.names=T,pattern='.EVR$')
regionFiles

                  
                  