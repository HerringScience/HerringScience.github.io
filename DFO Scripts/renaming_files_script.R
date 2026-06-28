#Script to change the naming convention of the new EK80 software 
rm(list = ls())

##################
#CHANGE ME TO MATCH
#All that needs to be changed are these values to match your folder specs.
date <- "2022-06-12"
vessel <- "Lady Janice"
prefix <- "LJ-"
##################


#call files to determine calibration list - using string to determine vessel calibration
right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
left = function (string,char){substr(string,1,char)}
#substrRight <- function(x, n){  substr(x, nchar(x)-n+1, nchar(x))}


filepath <- (paste0("C:/Users/herri/OneDrive/Documents/Surveys/2022/",date,"/",vessel,"/")) #CHANGE ME TO MATCH
#filepath <- (paste0("D:/2016 to present acoustics/2021 Acoustics/Scots Bay/",date,"/",vessel,"/"))
#filepath <- (paste0("C:/Users/smallt/Desktop/Juvenile 2021/",date,"/",vessel,"/"))

files <- list.files(filepath,pattern = ".raw",full.names = T)
files <- right(files, 21) 

newname <- NULL
for (i in 1:length(files)){
  newname[i] <- paste0(rep(prefix, length(files)),files[i])
}
newname
?file.rename

setwd(filepath)

file.rename(files, newname)

