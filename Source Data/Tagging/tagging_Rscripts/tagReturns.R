


head(relINFO)

sets = c(8,74,64,66,67,63,56,76,42,48,50,62)
# select sets that correspond from relINFO
sEvents = relINFO[which((relINFO$set %in% sets)), ]
write.table(sEvents, file= "sEvents.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
            




# The length of relINFO should be more as it contains all the tagging events, r1 is only those events for which there were returns

# Those tagging events with no tag returns

f = unique(r1$unique)
noReturns = subset(relINFO, !(unique %in% f))
Returns = subset(relINFO, (unique %in% f))
noReturns$Return_ = c("No")
Returns$Return_ = c("Yes")


head(noReturns)
head(Returns)

# df that list all the tagging events and whether there was a return associated with them or not
all = rbind(noReturns, Returns)
head(all)

head(noReturns)
head(Returns)

# Combine relINFO and r1 - contains the information for all tagging events




# Combine m and r2 
c = merge(m, r2, by = "group")
head(c)

# Clean up the data
c$type.x = NULL
c$type.y = NULL
c$div.x = NULL
c$daysAtLarge.x = NULL

c$release_month = c$month.x
c$month.x = NULL  

c$daysAtLarge = c$daysAtLarge.y
c$daysAtLarge.y = NULL

c$div = c$div.y
c$div.y = NULL

c$return_month = c$month.y
c$month.y = NULL

# Order by date
c1 = c[order(c$div),]
head(c1)




# Add distance between points
c1$dist = earth.dist(long1 = c1$Release_X, lat1 =c1$Release_Y, long2 = c1$Return_X, lat2 = c1$Return_Y)  

c1$dist = round(c1$dist, digits = 1)
# Convert days to seconds
c1$seconds = c1$daysAtLarge * 86400
# convert tdist to meters
c1$meters = c1$dist* 1000
# calculate speed
c1$speed = c1$meters/c1$seconds
# one observation was captured the same day, need to correc
head(c1)
dim(c1)
c1$no =1:149 
unique(c1$Return_Date)

# fix the tag erturn that was caught the same day and speed is listed as                                Inf
c1[27,21] = 0
c1[27,]
c1$no = NULL



head(c1)   
write.table(c1, file= "c1.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
# not working from here


# Create a data frame with div, no tags and no tags return, calculate return rate - u
u <- aggregate(x = c1$count, by = list(unique.values = c1$div, unique.values = c1$no_tags, unique.values = c1$Release_Date, unique.values = c1$Release_Vessel), FUN = sum)

colnames(u) = c("div", "no_tags", "release_date",  "tagging_vessel", "no_tag_returns")
head(u)
u$no_tag_returns = as.numeric(u$no_tag_returns)
u$no_tags=as.numeric(u$no_tags)
u$rate = u$no_tag_returns/u$no_tags * 100

head(u)
head(all)

c2 = c1
length(unique(r2$div))

c2$group = NULL
c2$id.x = NULL
c2$set = NULL
c2$id.y = NULL
c2$Release_X = NULL
c2$Release_Y = NULL
c2$Return_X = NULL
c2$Return_Y = NULL


# make this a function that I can apply to all data


# Export taggin events with no tag returns
write.table(noReturns, file= "noReturns.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)


