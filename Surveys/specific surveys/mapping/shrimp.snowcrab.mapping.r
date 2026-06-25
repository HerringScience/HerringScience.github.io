  
yr="2013"

####- need to add inside / outside line!!!!!


load("C:\\Rsaves\\logs.2013.rdata")
crab=logs
rm(logs)


#import shrimp log info in rdata file
# Need to ensure that the date.fished is in format of day/month/year , all numeric (not "Apr")
shrimp=read.csv("C:\\Rsaves\\shrimplogs_2013.csv", header=T)
names(shrimp)=tolower(names(shrimp))


#get dates in format able to extract day of year, week, etc+

source("C:/Scripts/functions/date.functions.r")
require(chron)
shrimp$date=as.Date(shrimp$date.fished, "%d/%m/%Y") 
shrimp$month=months(shrimp$date)
shrimp$month=factor(shrimp$month,levels=c("January","February","March",
"April","May","June","July","August","September",
"October","November","December"),ordered=TRUE)
shrimp$chron=as.chron(shrimp$date)
shrimp$dayofyear= convert.datecodes(shrimp$chron, y="julian")
shrimp$week=ceiling(shrimp$dayofyear/7)
shrimp$year=as.factor(years(shrimp$date))

crab$month=months(crab$date_fished)
crab$month=factor(crab$month,levels=c("January","February","March",
"April","May","June","July","August","September",
"October","November","December"),ordered=TRUE)
crab$chron=as.chron(crab$date_fished)
crab$dayofyear= convert.datecodes(crab$chron, y="julian")
crab$week=ceiling(crab$dayofyear/7)
crab$year=as.factor(years(crab$date_fished))

# remove an erroneous log book entry to avoid confusion
crab=crab[which(crab$ doc_id!="MD-277903"),]

###Convert Shrimp Positions for plotting
# Format to allow conversion to EventData for pbsMapping
#--------------------------------------
source("C:/Scripts/functions/position.functions.r")

shrimp=pos.2.dec.deg(shrimp)
shrimp$X=-shrimp$lon
shrimp$Y=shrimp$lat
shrimp$EID=1:nrow(shrimp)

crab= pos.2.dec.deg(crab)
crab$X=-crab$lon
crab$Y=crab$lat
crab$EID=1:nrow(crab)


focusyear=yr
shr=shrimp[shrimp$year==focusyear,]
crb=crab[crab$year==focusyear,]



#Produce actual map

fleets.map=function(timeframe="month", focus="April", area="shrimp.crab"){
  source("C:/Scripts/functions/snow.crab.mapping.functions.r")
  
 
  if (timeframe=="month"){
  makemap(x, area=area, title=paste(focus, focusyear, sep=" ")) #Create Basemap based on "area" above
  }
  
  if (timeframe=="week"){
  makemap(x, area=area, title=paste("Week ", focus,", ", focusyear, sep="")) #Create Basemap based on "area" above
  }
  
  #Plot whichever shrimp boxes apply based on time ("focus" above) 
  #if an error about "clipped vertices" appears, "area" does not capture all boxes plotted 
  
  boxes=get.boxes2plot(datetype=timeframe,dateitem=focus) 
    for (b in boxes){
        plotdata=get.poly.data(object=b)
        addPolys(plotdata, border="red")
        }

#Add shrimp fishing events in red
if (timeframe=="month"){
    i=which(shr$month==focus)
    sh=shr[i,]
    sh=as.EventData(sh, projection="LL")
    }

if (timeframe=="week"){
    i=which(shr$week==focus)
    sh=shr[i,]
    sh=as.EventData(sh, projection="LL")
    } 

addPoints(data=sh, col="red", pch=20, cex=0.7)

#Add crab fishing events in black
if (timeframe=="month"){
    i=which(crb$month==focus)
    cr=crb[i,]
    cr=as.EventData(cr, projection="LL")
    }

if (timeframe=="week"){
    i=which(crb$week==focus)
    cr=crb[i,]
    cr=as.EventData(cr, projection="LL")
    } 

addPoints(data=cr, col="black", pch=20, cex=0.7)



#Create Legend (location based on which area plotted)
if (area=="shrimp.crab"){
  points(x=-58.2, y=43.9, col="red", pch=20, cex=2)
  text("Shrimp Effort", font=2, x=-58.17, y= 43.9, pos=4)
  points(x=-58.2, y=44, col="black", pch=20, cex=2)
  text("Crab Effort", font=2, x=-58.17, y= 44, pos=4)
  }

if (area=="all.holes"){
  points(x=-58.75, y=44.2, col="red", pch=20, cex=2)
  text("Shrimp Effort", font=2, x=-58.72, y= 44.2, pos=4)
  points(x=-58.75, y=44.3, col="black", pch=20, cex=2)
  text("Crab Effort", font=2, x=-58.72, y= 44.3, pos=4)
  }
if (focus=="May"){
   text("* Sable Box after May 15th", font=2, x=-60, y=44.05, pos=4, col="red")
  }
if (focus=="August"){
   text("* Sable Box before August 15th", font=2, x=-60, y=44.05, pos=4, col="red")
  }
  if (focus %in% c("23","24")){
   text("* Shrimp Survey Ongoing", font=1.5, x=-59.6, y=44.45, pos=4, col="red")
  }
  if (focus %in% c("25","26")){
   text("* Central Portion Opened Early by Fleet Agreement", font=2, cex=0.7, x=-59.6, y=44.45, pos=4, col="red")
  }
}


### These scripts will create maps in separate windows 
#Loop on Months
months=c("March","April","May","June","July")
for (m in months){
windows()
fleets.map(timeframe="month", focus=m, area="shrimp.crab") 
} 

#Loop on Weeks

weeks=c(20:26)
#weeks=c(11:28)
for (w in weeks){
windows()
fleets.map(timeframe="week", focus=w, area="shrimp.crab") 
} 

### These scripts will create maps in separate windows 
#Loop on Months

pdf.months=function(months=c("April")){
pdf()
filename=paste("C:/Rsaves/maps/fleets.interaction/bothfleets",focusyear,"bymonth.pdf", sep="")
pdf(file=filename, onefile=TRUE)
for (m in months){
fleets.map(timeframe="month", focus=m, area="shrimp.crab")} 
dev.off()
print(paste("Find file here: ",filename,sep=""))
} 

#pdf.months(c("March","April","May","June","July","August"))

#Loop on Weeks
pdf.weeks=function(weeks=c(12:20)){
pdf()
filename=paste("C:/Rsaves/maps/fleets.interaction/bothfleets",focusyear,"byweek.pdf", sep="")
pdf(file=filename, onefile=TRUE)
for (w in weeks){
fleets.map(timeframe="week", focus=w, area="shrimp.crab")} 
dev.off()
print(paste("Find file here: ",filename,sep=""))
} 
 
pdf.weeks(c(11:30))


