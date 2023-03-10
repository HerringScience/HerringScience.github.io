
RLibrary("lubridate", "ggplot2", "reshape")

# Look at Connors Fat Content Data for Historical Context
C2004 = read.table("2004_Connors.csv", header=TRUE,sep=",", stringsAsFactors=FALSE, quote = "\"")
C2005 = read.table("2005_Connors.csv", header=TRUE,sep=",", stringsAsFactors=FALSE, quote = "\"")
C2006 = read.table("2006_Connors.csv", header=TRUE,sep=",", stringsAsFactors=FALSE, quote = "\"")
C2007 = read.table("2007_Connors.csv", header=TRUE,sep=",", stringsAsFactors=FALSE, quote = "\"")
C2008 = read.table("2008_Connors.csv", header=TRUE,sep=",", stringsAsFactors=FALSE, quote = "\"")
C2009 = read.table("2009_Connors.csv", header=TRUE,sep=",", stringsAsFactors=FALSE, quote = "\"")
# 2010-2013
Rest = read.table("Rest_Connors.csv", header=TRUE,sep=",", stringsAsFactors=FALSE, quote = "\"")
# 2017
Recent = read.table("Recent_Connors.csv", header=TRUE,sep=",", stringsAsFactors=FALSE, quote = "\"")
# Recent data date format is different than previous years so has to be computed separately
Recent$DATE = parse_date_time(Recent$DATE, orders= "mdy", tz = "America/Moncton")

Connors = rbind(C2004, C2005, C2006, C2007, C2008, C2009, Rest)
Connors$DATE = parse_date_time(Connors$DATE, orders= "dmy", tz = "America/Moncton")

# Final DF
Connors = rbind(Connors, Recent)

# Create a month variable
Connors$Month<-month(Connors$DATE)

# Select for records only for Nov and Dec
ids =c(11,12)
ids =c(12)

FishKill = Connors[which((Connors$Month %in% ids)), ]

# Restructure df - Stack
FK <- data.frame(FishKill[1:4], stack(FishKill[5:14]))
dim(FK)

FKdata= FK[complete.cases(FK),]
dim(FK)
dim(FKdata)


# Plot histogram
hist(FK$values, col="blue", main="Fat % Connors (Nov and Dec)", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,20, 2))

# All data Connors
FK <- data.frame(Connors[1:4], stack(Connors[5:14]))
hist(FK$values, col="navyblue", main="Fat % Connors (2004-2013, 2017)", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,48, 2))
head(Connors)

dim(FK)

# Just Nov
ids =c(11)
Nov = Connors[which((Connors$Month %in% ids)), ]
write.table(Nov, file= "Nov.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
# Restructure df - Stack
FK <- data.frame(Nov[1:4], stack(Nov[5:14]))
# Plot histogram
hist(FK$values, col="navyblue", main="Fat % Connors - November", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,20, 2))
write.table(FK, file= "Nov.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)

# Just Dec
ids =c(12)
Dec = Connors[which((Connors$Month %in% ids)), ]
write.table(Dec, file= "Dec.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
# Restructure df - Stack
FK <- data.frame(Dec[1:4], stack(Dec[5:14]))
# Plot histogram
hist(FK$values, col="navyblue", main="Fat % Connors - December", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(3,15, 1))

# Oct
ids =c(10)
Oct = Connors[which((Connors$Month %in% ids)), ]
write.table(Oct, file= "Oct.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
# Restructure df - Stack
FK <- data.frame(Oct[1:4], stack(Oct[5:14]))
# Plot histogram
hist(FK$values, col="navyblue", main="Fat % Connors - October", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))

# September
ids =c(09)
Sep = Connors[which((Connors$Month %in% ids)), ]
write.table(Sep, file= "Sep.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
# Restructure df - Stack
FK <- data.frame(Sep[1:4], stack(Sep[5:14]))
# Plot histogram
hist(FK$values, col="navyblue", main="Fat % Connors - September", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))

# August
ids =c(08)
Aug = Connors[which((Connors$Month %in% ids)), ]
write.table(Aug, file= "Aug.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
# Restructure df - Stack
FK <- data.frame(Aug[1:4], stack(Aug[5:14]))
# Plot histogram
hist(FK$values, col="navyblue", main="Fat % Connors - August", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,50, 1))

# July
ids =c(07)
Jul = Connors[which((Connors$Month %in% ids)), ]
write.table(Jul, file= "Jul.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
# Restructure df - Stack
FK <- data.frame(Jul[1:4], stack(Jul[5:14]))
# Plot histogram
hist(FK$values, col="navyblue", main="Fat % Connors - July", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,50, 1))

# June
ids =c(06)
Jun = Connors[which((Connors$Month %in% ids)), ]
write.table(Jun, file= "Jun.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
# Restructure df - Stack
FK <- data.frame(Jun[1:4], stack(Jun[5:14]))
# Plot histogram
hist(FK$values, col="navyblue", main="Fat % Connors - June", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,50, 1))

# May
ids =c(05)
May = Connors[which((Connors$Month %in% ids)), ]
write.table(May, file= "May.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
# Restructure df - Stack
FK <- data.frame(May[1:4], stack(May[5:14]))
# Plot histogram
hist(FK$values, col="navyblue", main="Fat % Connors - May", xlab="Fat %",xaxt = "n", breaks = 25 )
axis(side=1, at=seq(0,20, 1))

# March
ids =c(03)
Mar = Connors[which((Connors$Month %in% ids)), ]
write.table(Mar, file= "Mar.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
# Restructure df - Stack
FK <- data.frame(Mar[1:4], stack(Mar[5:14]))
# Plot histogram
hist(FK$values, col="navyblue", main="Fat % Connors - March", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))

# Februaru
ids =c(02)
Feb = Connors[which((Connors$Month %in% ids)), ]
write.table(Feb, file= "Feb.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
# Restructure df - Stack
FK <- data.frame(Feb[1:4], stack(Feb[5:14]))
# Plot histogram
hist(FK$values, col="navyblue", main="Fat % Connors - February", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))

# January
ids =c(01)
Jan = Connors[which((Connors$Month %in% ids)), ]
write.table(Jan, file= "Jan.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
# Restructure df - Stack
FK <- data.frame(Jan[1:4], stack(Jan[5:14]))
# Plot histogram
hist(FK$values, col="navyblue", main="Fat % Connors - January", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))

dat2a <- data.frame(Connors[1:4], stack(Connors[5:14]))
tail(dat2a)

dat2a$Month = month(Connors$DATE)
ids =c(11,12)
DieO = dat2a[which((dat2a$Month %in% ids)), ]
dim(DieO)

hist(dat2a$values, col="blue", main="Fat % Connors (2004-2013, 2017)", xlab="Fat %", breaks = 20 )
axis(side=1, at=seq(0,50, 5))

mist = dat2a[which((dat2a$value > 30)), ]


hist(DieO$values, col="blue", main="Fat % Connors (Nov and Dec)", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,20, 2))


test = stack(Connors)
head(test)

write.table(Connors, file= "Connors.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)




#---------------------------------------------------------------------------------------
# Maine's Fat Data
maine = read.table("maineFat.csv", header=TRUE,sep=",", stringsAsFactors=FALSE, quote = "\"")

maine$Date = as.Date(maine$Date)
maine$Month<-month(maine$Date)
maine$Year<-year(maine$Date)

# November and December
ids =c(12)
maine = maine[which((maine$Month %in% ids)), ]


# January
ids =c(01)
Jan = maine[which((maine$Month %in% ids)), ]
# Plot histogram
hist(Jan$Percent_Fat, col="dark green", main="Fat % Maine Gov - January", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))
dim(Jan)
mean(Jan$Percent_Fat)
text(8, 300, "n=2459, mean=4.24",cex = .9)
head(Jan)

# February
ids =c(02)
Feb = maine[which((maine$Month %in% ids)), ]
# Plot histogram
hist(Feb$Percent_Fat, col="dark green", main="Fat % Maine Gov - February", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))
dim(Feb)
mean(Feb$Percent_Fat)
text(6, 200, "n=1483, mean=3.57",cex = .9)

# March
ids =c(03)
Mar = maine[which((maine$Month %in% ids)), ]
# Plot histogram
hist(Mar$Percent_Fat, col="dark green", main="Fat % Maine Gov - March", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))
dim(Mar)
mean(Mar$Percent_Fat)
text(6, 200, "n=1867, mean=2.56",cex = .9)


# April
ids =c(04)
Apr = maine[which((maine$Month %in% ids)), ]
# Plot histogram
hist(Apr$Percent_Fat, col="dark green", main="Fat % Maine Gov - April", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))
dim(Apr)
mean(Apr$Percent_Fat)
text(5, 200, "n=1597, mean=4.51",cex = .9)

# May
ids =c(05)
May = maine[which((maine$Month %in% ids)), ]
# Plot histogram
hist(May$Percent_Fat, col="dark green", main="Fat % Maine Gov - May", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))
dim(May)
mean(May$Percent_Fat)
text(10, 75, "n=788, mean=7.09",cex = .9)


# June
ids =c(06)
Jun = maine[which((maine$Month %in% ids)), ]
# Plot histogram
hist(Jun$Percent_Fat, col="dark green", main="Fat % Maine Gov - June", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,30, 1))
dim(Jun)
mean(Jun$Percent_Fat)
text(20, 60, "n=757, mean=9.77",cex = .9)

# July
ids =c(07)
Jul = maine[which((maine$Month %in% ids)), ]
# Plot histogram
hist(Jul$Percent_Fat, col="dark green", main="Fat % Maine Gov - July", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))
dim(Jul)
mean(Jul$Percent_Fat)
text(12, 250, "n=1181, mean=10.37",cex = .9)

# Aug
ids =c(08)
Aug = maine[which((maine$Month %in% ids)), ]
# Plot histogram
hist(Aug$Percent_Fat, col="dark green", main="Fat % Maine Gov - Aug", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))
dim(Aug)
mean(Aug$Percent_Fat)
text(13, 175, "n=2061, mean=9.56",cex = .9)

# Sep
ids =c(09)
Sep = maine[which((maine$Month %in% ids)), ]
# Plot histogram
hist(Sep$Percent_Fat, col="dark green", main="Fat % Maine Gov - September", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))
dim(Sep)
mean(Sep$Percent_Fat)
text(13, 350, "n=2420, mean=7.51",cex = .9)

# October
ids =c(10)
Oct = maine[which((maine$Month %in% ids)), ]
# Plot histogram
hist(Oct$Percent_Fat, col="dark green", main="Fat % Maine Gov - October", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))
dim(Oct)
mean(Oct$Percent_Fat)
text(13, 350, "n=2928, mean=7.90",cex = .9)

# November
ids =c(11)
Nov = maine[which((maine$Month %in% ids)), ]
# Plot histogram
hist(Nov$Percent_Fat, col="dark green", main="Fat % Maine Gov - November", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))
dim(Nov)
mean(Nov$Percent_Fat)
text(11, 150, "n=1536, mean=5.94",cex = .9)

# December
ids =c(12)
Dec = maine[which((maine$Month %in% ids)), ]
# Plot histogram
hist(Dec$Percent_Fat, col="dark green", main="Fat % Maine Gov - December", xlab="Fat %",xaxt = "n", breaks = 20 )
axis(side=1, at=seq(0,25, 1))
dim(Dec)
mean(Dec$Percent_Fat)
text(11, 150, "n=1275, mean=5.25",cex = .9)

# All data combined
hist(maine$Percent_Fat, col="dark green", main="Fat % Maine Gov (2007-2013)", xlab="Fat %",xaxt = "n", breaks = 30 )
axis(side=1, at=seq(0,35, 1))
dim(maine)
mean(maine$Percent_Fat)
text(16, 1500, "n=20,349, mean=6.38",cex = .9)
