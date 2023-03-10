
RLibrary("lubridate", "ggplot2", "reshape")


# Fat Die Off Samples

# Load in the Comeau's data
comeaus =  read.csv("comeaus.fs.csv", header=TRUE, sep=",",stringsAsFactors=FALSE, quote = "\"")
comeaus2 =  read.csv("comeaus.fs2.csv", header=TRUE, sep=",",stringsAsFactors=FALSE, quote = "\"")

# Load in Scotia Garden's data
sg =  read.csv("scotiaGardenFat.csv", header=TRUE, sep=",",stringsAsFactors=FALSE, quote = "\"")

# Load in Seacrest
sc =  read.csv("seacrest.csv", header=TRUE, sep=",",stringsAsFactors=FALSE, quote = "\"")

# Remove unneeded columns from comeaus
comeaus$Product =NULL
comeaus$Lab. =NULL
comeaus$Length =NULL
comeaus$Weight =NULL
comeaus2$Length.cm. =NULL
comeaus2$Weight.g. =NULL

#
dieOff = rbind(comeaus, sg, comeaus2, sc)
dieOff$id = 1:54
dieOff$name = "Samples"
dieOff$Date_Collected = as.Date(dieOff$Date_Collected, "%m/%d/%Y")
dieOff$Year =year(dieOff$Date_Collected)
dieOff$Month =month(dieOff$Date_Collected)

# Histogram of the fat content of samples collected during the die off
myHistogram = ggplot(dieOff, aes(dieOff$Fat_Content)) + theme(legend.position = "none")
myHistogram + geom_histogram(aes(stat="identity", colour = 'Fat_Content'), bins = 35) + labs(x = "Fat Content", y = "Frequency") +xlim(0,35) + ggtitle("Fat Content Range of Fish Collected during the 2016 Die Off")

# Boxplot of the fat content of samples collected during the die off
allfat = ggplot(dieOff, aes(name, Fat_Content))
allfat + geom_boxplot(fill = "white", colour = "#3366FF") + labs(x = NULL, y = "Fat Content")  +ylim(0,35) + ggtitle("Fat Content Range of Fish Collected during the 2016 Die Off") + scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32)) + coord_flip()

# Investigation of separate Locations
festivalBox = ggplot(dieOff, aes(Location, Fat_Content))
festivalBox + geom_boxplot() + labs(x = "Location", y = "Fat Content")  +ylim(0,35) + ggtitle("Fat Content Range of Fish Collected during the 2016 Die Off")


# Investigation of separate collection dates
festivalBox = ggplot(dieOff, aes(Date_Collected, Fat_Content))
festivalBox + geom_boxplot() + labs(x = "Date Collected", y = "Fat Content") +ylim(0,35) + ggtitle("Fat Content Range of Fish Collected during the 2016 Die Off")


head(dieOff)

######################################################################

# Compare to baseline data - load from script fat_data.R

# Use only those years which are comparable
ids =c(2008, 2009, 2010, 2011)


# Format to combine with dieOff
# Connors
head(FK)
colnames(FKdata) = c("id", "Date_Collected", "Boat", "Location", "Fat_Content", "Size_Class")
FK = FKdata
FK$name = "BaseConnors"
FK$Plant = "Connors"
FK$Boat = NULL
FK$Size_Class=NULL
FK$Year =year(FK$Date_Collected)
FK$Month =month(FK$Date_Collected)

dim(FK)
unique(FK$Year)
unique(FK$Location)

# Histogram with locations
ggplot(FK, aes(Location)) + geom_histogram(binwidth = 10)
ggplot(FK, aes(x = Location)) + geom_bar(aes(y = ..count..), stat = "count")


#FK_ = FK[which((FK$Year %in% ids)), ]

# Gov Maine
head(maine)
maine$Method.Name = NULL
maine$SampleID = NULL
maine$Time = NULL
maine$SampleWt_gm = NULL
maine$Final1Wt_gm = NULL
maine$Percent_Moisture = NULL

colnames(maine) = c("id", "Date_Collected", "Fat_Content", "Month", "Year")
maine$name = "BaseMaine"
maine$Location = NA
maine$Plant = NA
maine_ = maine[which((maine$Year %in% ids)), ]

unique(maine$Year)

ids=c(12)
maineDec = maine_[which((maine_$Month %in% ids)), ]
FKDec = FK_[which((FK_$Month %in% ids)), ]


FKFinal = FK_[complete.cases(FK_),]


# Combine all data
combo = rbind(FK_, dieOff, maine_)
# Only december
combo2 = rbind(maine, FK, dieOff)

both = ggplot(combo, aes(name, Fat_Content))
both + geom_boxplot(fill = "white", colour = "#3366FF") + labs(x = NULL, y = "Fat Content")  +ylim(0,35) + ggtitle("Fat Content of Base Data from Nov/Dec with Die Off 2016 Samples") + scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32)) + coord_flip()  + annotate("text", x = 1.5, y = 22, label = "Base data from the years 2008, 2009, 2010 & 2011")   

both = ggplot(combo2, aes(name, Fat_Content))
both + geom_boxplot(fill = "white", colour = "#3366FF") + labs(x = NULL, y = "Fat Content")  +ylim(0,35) + ggtitle("Baseline Data from Dec and 2016 Samples") + scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32)) + coord_flip()  

# Statistics
head(FK)
ids =c(12)
decFK = FK[which((FK$Month %in% ids)), ]
dim(decFK)


t.test(dieOff$Fat_Content,FKDec$Fat_Content)
t.test(maineDec$Fat_Content,FKDec$Fat_Content)
t.test(maineDec$Fat_Content,dieOff$Fat_Content)
