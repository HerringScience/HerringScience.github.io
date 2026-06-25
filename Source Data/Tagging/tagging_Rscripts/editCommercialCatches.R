


catches = read.csv("commercialcatches.csv")

catches$LAND_DATE = as.Date(catches$LAND_DATE, "%Y-%m-%d", origin = "1960-10-01")

head(catches)
catches$Year = year(catches$LAND_DATE)

unique(catches$Vessel.name)


d2016=catches[which(catches$Year == 2016), ]
fm = d2016[which(d2016$Vessel.name == "Fundy Monarch"), ]
fm$Vessel.name = "Dual Venture"

head(fm)

d=d2016[!(d2016$Vessel.name=="Fundy Monarch"),]
dim(d)
dim(d2016)
dim(fm)


combine = rbind(d,fm)
dim(combine)
head(combine)
unique(combine$Vessel.name)


cAtches=catches[!(catches$Year==2016),]
dim(cAtches)
dim(catches)
dim(combine)


love = rbind(cAtches, combine)
dim(love)
dim(catches)


# export love
write.table(love, file= "love.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
