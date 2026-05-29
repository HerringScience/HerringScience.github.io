





















# Look at certain weeks to do temporal comparisons, i.e. between years 2017 & 2018   
# reduce    
          Dw=data[which(data$week == 29), ]
          unique(Dw$week)
          Dr=data[which(data$week == 30), ]
          unique(Dr$week)
          d = rbind(Dw, Dr)
          
          d = d[which(d$Depth <= 45.7), ]
          max(d$Depth)
          Dw = d
              salinity<-aggregate(Salinity~id, Dw, FUN=mean)
              temperature<-aggregate(Temperature~id, Dw, FUN=mean)
              measure = merge(salinity, temperature, by = "id")
              evento = merge(events, measure, by = "id")
               
                    minT<-aggregate(Temperature~id, Dw, FUN=min)
                    MaxT<-aggregate(Temperature~id, Dw, FUN=max)
                    temp = merge(minT, MaxT, by = "id")
                    colnames(temp) = c("id", "MinT", "MaxT")
                    temp$rangeT = (temp$MaxT-temp$MinT)
                    
                        minS<-aggregate(Salinity~id, Dw, FUN=min)
                        maxS<-aggregate(Salinity~id, Dw, FUN=max)
                        salin = merge(minS, maxS, by = "id")
                        colnames(salin) = c("id", "minS","maxS")
                        salin$rangeS = (salin$maxS-salin$minS)
        
                          more = merge(salin, temp, by = "id")
                            eventsB = merge(evento, more, by = "id")
                            MaxDepth = aggregate(Depth~id, Dw, FUN = max)    
                            eventsC = merge(eventsB, MaxDepth, by = "id")
                            
                                write.table(eventsC, file= "w29_30.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 


y2018=german[which(german$Year == 2018), ]


unique(y2017$Date)
head(german)

boxes

calc
# Calculate temperatures for each plankton tow based on the average tow depth. If no average tow depth recorded
  # use the surface temp (1m)

id = "10K100766_20180609_230434"  
test=data[which(data$id == id), ]
max(test$Depth)

#select for depth of tow 
    d = 4
    testy=test[which(test$Depth <= d), ]
    mean(test$Temperature)
    mean(test$Salinity)
    se <- function(x) sqrt(var(x)/length(x))
    se(test$Temperature)
    se(test$Salinity)



ggplot(testy,aes(Depth, Temperature))+ geom_point() + geom_smooth(span = 0.1)  + scale_y_continuous(breaks = c(7,7.5,8), limits=c(7,8)) 

##############Average temperature for each CTD temp profile, but we need to standardize for depth, lets say 5m
# what is the best depth to compare? most stable...????

reduced = data[which(data$Depth <= 3), ]
# I don't think 3m gives reliable trends..    
reduced = data[which(data$Depth <= 5), ]
    reduced=reduced[which(reduced$id != "10K100766_20180609_230250"), ]
    reduced=reduced[which(reduced$id != "10K100766_20170622_062437"), ]
    
reduced = data[which(data$Depth <= 15), ]
        reduced=reduced[which(reduced$id != "10K100766_20180609_230250"), ]
        reduced=reduced[which(reduced$id != "10K100766_20170622_062437"), ]  
        reduced=reduced[which(reduced$id != "10K100766_20170730_065538"), ]
        reduced=reduced[which(reduced$id != "10K100766_20170908_224604"), ]
        
        

        # 15 and 5 give similar trends

# if we take away two casts (SB, one for each year)we can try a deeper depth

# we can do 5m with the rest
# and if we remove  we can do 15m

# we also might want to incorporate tidal cycle..seems to be alot of variability that some other factor that date might explain...turns out you just need to go deeper, alot of variability exists in upper 2m.


  
temperatures<-aggregate(Temperature~id, reduced, FUN=mean)
temperaturesSE<-aggregate(Temperature~id, reduced, FUN=se)
temps = merge(temperatures, temperaturesSE, by = "id")

salinity<-aggregate(Salinity~id, reduced, FUN=mean)
salinitySE<-aggregate(Salinity~id, reduced, FUN=se)
salin = merge(salinity, salinitySE, by = "id")
tw0 = merge(temps, salin, by = "id")

soundVelocity <-aggregate(Sound_velocity~id, reduced, FUN=mean)
soundVelocitySE <-aggregate(Sound_velocity~id, reduced, FUN=se)
sound = merge(soundVelocity, soundVelocitySE, by = "id")
three = merge(tw0, sound, by = "id")              
colnames(three) = c("id", "AvgTemp", "TempSE", "AvgSal", "SalSE", "AvgSoundVelo", "SouSE")

MaxDepth = aggregate(Depth~id, data, FUN = max)
julian = aggregate(julianDay~id, data, FUN = max)


eventsD = merge(MaxDepth, events, by = "id")

dataSum = merge(three, MaxDepth, by = "id")        
overviewCTD = merge(dataSum, events, by = "id")
head(overviewCTD)

overviewCTD$Depth

# export overview CTD
write.table(eventsD, file= "eventsD.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 


means = merge(temperatures, salinity, by = "id")
average = merge(means, soundVelocity, by = "id")

head(average)

averages = merge(average, events, by = "id")


    #test one "county"
    id = "10K100766_20180721_224124"  
    test=data[which(data$id == id), ]
    head(test)
    
    ggplot(test,aes(Depth, Temperature))+ geom_point() + geom_smooth(span = 0.1)  + scale_y_continuous(breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), limits = c(5,20)) 
    
    
    #+ scale_y_reverse()
      geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_path(aes(group=group, colour = daysAtLarge), lwd = 1.1, alpha = 0.7) + geom_point(aes(shape = type, colour = daysAtLarge), size = 3, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()  + theme(panel.background = element_rect(fill = "grey68"))
    
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(county_list)) { 
    
    # create plot for each county in df 
    plot <- 
      ggplot(subset(r, r$div==county_list[i]),
             aes(X, Y)) + 
      geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_path(aes(group=group, colour = daysAtLarge), lwd = 1.1, alpha = 0.7) + geom_point(aes(shape = type, colour = daysAtLarge), size = 3, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()  + theme(panel.background = element_rect(fill = "grey68")) +
      
      ggtitle(paste(county_list[i]))
    
    # save plots as .png
    ggsave(plot, file=paste(results, county_list[i], ".png", sep="_"), scale=2)
    
    
    # print plots to screen
    print(plot)
  }
}


# average bottom temp would be useful but we don't have casts to the bottom...how to compare between casts..use a standard depth?







#OCE
# create a loop for each .csv file
ids = events$id
events$id

		
		



d = read.csv("10K100766_20180812_230248.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
names(d)
names(d) <- c("p", "depth", "T", "conductivity","conductance","S","v","density")
head(d,2)
ctd <- as.ctd(d$S, d$T, d$p, longitude=-65.24, latitude=45.115)
summary(ctd)
plot(ctd)

plot(ctd,eos="unesco")
summary(swRho(ctd,eos="unesco") - d$density)




# Calculate the range for both temperautre and salinity to get an idea of mixing 

head(data)
reduced = data[which(data$Depth <= 30), ]
reduced=reduced[which(reduced$id != "10K100766_20180609_230250"), ]
reduced=reduced[which(reduced$id != "10K100766_20170622_062437"), ]  
reduced=reduced[which(reduced$id != "10K100766_20170730_065538"), ]
reduced=reduced[which(reduced$id != "10K100766_20170908_224604"), ]
# only for 30
reduced=reduced[which(reduced$id != "10K100766_20170917_225337"), ]


scotsT<-aggregate(Temperature~id, reduced, FUN=min)
colnames(scotsT) = c("id", "minT")
scotsT2<-aggregate(Temperature~id, reduced, FUN=max)
colnames(scotsT2) = c("id", "maxT")
scots3 = aggregate(Temperature~id, reduced, FUN=mean)
tempS = merge(scotsT, scotsT2, by = "id")
tempSS = merge(tempS, scots3, by = "id")
colnames(tempSS) = c("id", "minT", "maxT", "averageT")

scotsST<-aggregate(Salinity~id, reduced, FUN=min)
colnames(scotsST) = c("id", "minS")
scotsS2<-aggregate(Salinity~id, reduced, FUN=max)
colnames(scotsS2) = c("id", "maxS")
scotsS3 = aggregate(Salinity~id, reduced, FUN=mean)
colnames(scotsS3) = c("id", "averageS")
salinS = merge(scotsST, scotsS2, by = "id")
salinSS = merge(salinS, scotsS3, by = "id")

range = merge(salinSS, tempSS, by = "id")
head(events)
events1=events[which(events$id != "10K100766_20180609_230250"), ]
events2=events1[which(events1$id != "10K100766_20170622_062437"), ]  
events3=events2[which(events2$id != "10K100766_20170730_065538"), ]
events4=events3[which(events3$id != "10K100766_20170908_224604"), ]
events5=events4[which(events4$id != "10K100766_20170917_225337"), ]

MaxDepth = aggregate(Depth~id, data, FUN = max)
rangeR = merge(events4, range, by = "id")

tous = merge(MaxDepth, rangeR, by = "id")

# export overview CTD
write.table(tous, file= "RangeAverage.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 


# look at individual cast data
dataI=data[which(data$id == "10K100766_20180812_230248"), ]
r = min(dataI$Temperature)
f = max(dataI$Temperature)

f-r

str(dataI)

test=dataI[which(dataI$Temperature == 17.53737), ]

head(dataI)
dataI[10,]



# Look at the two replicates
ids = c("10K100766_20180609_230250")
ids = c("10K100766_20180609_230434")
test1 = data[which((data$id %in% ids)), ]
test2 = data[which((data$id %in% ids)), ]

tail(test1)
tail(test2)

test11=test1[which(test1$Depth <= 4.6), ]
test22=test2[which(test2$Depth <= 4.6), ]

mean(test11$Density)
mean(test22$Density)

t.test(test11$Salinity,test22$Salinity)
t.test(test11$Temperature,test22$Temperature)
t.test(test11$Density,test22$Density)
