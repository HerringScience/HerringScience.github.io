
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")




# Load Data

data = read.table("TSLW.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

aCCL = read.table("averageLength.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
aCCW = read.table("averageWeight.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)





# Format Data
    
    aCCL$Year = as.factor(aCCL$Year)
    aCCW$Year = as.factor(aCCW$Year)
    
    head(aCCL)
    
    data$Year = as.factor(data$Year)


          # Need to reformat these to have Year Age and Length/Weight as the three columns
          
              aCAAW = melt(aCCW)
                head(aCAAW)
                colnames(aCAAW) = c("Ground", "Year", "Age", "Weight")
                    aCAAW$Weight = as.numeric(aCAAW$Weight)
              
                  aCAAL = melt(aCCL)
                    head(aCAAL)
                      colnames(aCAAL) = c("Ground", "Year", "Age", "Length")
                        aCAAL$Length = as.numeric(aCAAL$Length)
                        
                        # create dfs for each ground
                        
                          unique(aCAAW$Ground)
                          aCAAW_All=aCAAW[which(aCAAW$Ground == "OverallSWNSBoF"), ]
                          aCAAW_G=aCAAW[which(aCAAW$Ground == "GermanBank"), ]
                          aCAAW_S=aCAAW[which(aCAAW$Ground == "ScotsBay"), ]
                          
                          
                        
                          unique(aCAAL$Ground)
                          aCAAL_All=aCAAL[which(aCAAL$Ground == "OverallSWNSBoF"), ]
                          aCAAL_G=aCAAL[which(aCAAL$Ground == "GermanBank"), ]
                          aCAAL_S=aCAAL[which(aCAAL$Ground == "ScotsBay"), ]
                          
                          
              
                        # remove rows with NA
                        dataC = na.omit(data)
                    
                            # format date:
                            dataC$Date<-as.Date(dataC$Date,format = "%Y-%m-%d")

                                  dataC$Spawning.Ground = as.factor(dataC$Spawning.Ground)
                                        # Surveys with juveniles: remove rows with mean length <23cm as these are presumably not spawning fish
                                        data_NoJuv = dataC[dataC$MeanLengthmm > 230, ]
                                        str(data_NoJuv)
                          
                                            data_NoJuv$MeanLengthmm = as.numeric(data_NoJuv$MeanLengthmm)
                                            data_NoJuv$MeanWeightgm = as.numeric(data_NoJuv$MeanWeightgm)
                                          

                                            ids = c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
                                            
                                            
                                            
                                                                
# Create New DFs
# create German Bank and Scots Bat DFs

ids = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

        German=data_NoJuv[which(data_NoJuv$Spawning.Ground == "GB"), ]
        last10G = German[which((German$Year %in% ids)), ] 
        Scots=data_NoJuv[which(data_NoJuv$Spawning.Ground == "SB"), ]
        last10S = Scots[which((Scots$Year %in% ids)), ] 

              last10 = data_NoJuv[which((data_NoJuv$Year %in% ids)), ] 
              
              grounds<-aggregate(TS~Year+Spawning.Ground, data_NoJuv, FUN=mean)

              
              # Need to remove years 1999/2000 before any weight.length calcs. weight more so. it's listed as 0.

                      YearAverageW<-aggregate(MeanWeightgm~Year, data_NoJuv, FUN=mean)
                      YearAverageL<-aggregate(MeanLengthmm~Year, data_NoJuv, FUN=mean)
                      
                      YearAverageL$MeanLengthmm = (YearAverageL$MeanLengthmm)/10

                            # Take an average weight and length for each year:
                      
                      # remove 1999-2000
                      
                      ids = c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
                    
                      data_NoJuv1 = data_NoJuv[which((data_NoJuv$Year %in% ids)), ] 
                      
                      
                      
                            AverageWS<-aggregate(MeanWeightgm~Year, Scots, FUN=mean)
                            AverageWG<-aggregate(MeanWeightgm~Year, German, FUN=mean)
                      
                            AverageWS_ = AverageWS[which((AverageWS$Year %in% ids)), ] 
                            AverageWG_ = AverageWG[which((AverageWG$Year %in% ids)), ] 
                            
                            
                            
                            
                            
                            AverageLS<-aggregate(MeanLengthmm~Year, Scots, FUN=mean)
                            AverageLG<-aggregate(MeanLengthmm~Year, German, FUN=mean)
                            
                            
                                    # Create average length and weight for each age
                            
                            
                                        averageAL_G = aggregate(Length~Age, aCAAL_G, FUN=mean)
                                        averageAW_G = aggregate(Weight~Age, aCAAW_G, FUN=mean)
                                        
                                        averageAL_S = aggregate(Length~Age, aCAAL_S, FUN=mean)
                                        averageAW_S = aggregate(Weight~Age, aCAAW_S, FUN=mean)
                                        
                                        
                                        
                                        
                                        
                                        averageLY_S = aggregate(Length~Year, aCAAL_S, FUN=mean)
                                        averageWY = aggregate(Weight~Year, aCAAW_S, FUN=mean)
                                        
      
                                        
                                        
                                        
                                              
            
            
# Plots

# Target Strength overall
      ggplot(data_NoJuv, aes(x=Date, y=TS)) + geom_point(aes(colour = Year, shape = Spawning.Ground))  + ggtitle("Target Stength Values for Acoustic Surveys") + theme_bw()
      
      ggplot(data_NoJuv, aes(x=Date, y=TS)) + geom_point(aes(colour = Year, shape = Spawning.Ground))  + ggtitle("Target Stength Values for Acoustic Surveys") + theme_bw() + geom_smooth(span = 0.5)
      ggplot(data_NoJuv, aes(x=Date, y=TS)) + geom_point(aes(colour = Year, shape = Spawning.Ground))  + ggtitle("Target Stength Values for Acoustic Surveys") + theme_bw() + geom_smooth(method='lm')
      
      ggplot(data_NoJuv, aes(x=Date, y=TS)) + geom_point(aes(colour = Year, shape = Spawning.Ground))  + ggtitle("Target Stength Values for Acoustic Surveys") + theme_bw()+ geom_hline (yintercept=-35.5, linetype = "dashed", colour = "red")
      
      ggplot(data_NoJuv, aes(x=Date, y=TS)) + geom_point(aes(colour = Year, shape = Spawning.Ground)) + geom_hline (yintercept=-35.5, linetype = "dashed", colour = "red") + ggtitle("Target Stength Values for Acoustic Surveys") + theme_bw() + geom_smooth(span = 0.5)
      
      ggplot(data_NoJuv, aes(x=Date, y=TS)) + geom_point(aes(colour = Spawning.Ground), size = 2) + theme_bw() + ggtitle("Comparing Spawning Grounds")
      
      
      ggplot(last10, aes(x=Date, y=TS)) + geom_point(aes(colour = Spawning.Ground), size = 2) + theme_bw() + ggtitle("Comparing Spawning Grounds")

      #t-test
      str(data_NoJuv)
      t.test(TS ~ Spawning.Ground, data = data_NoJuv)
      t.test(TS ~ Spawning.Ground, data = last10)
      
      
      
      
      
      ggplot(grounds, aes(x=Year, y=TS)) + geom_point(aes(colour = Spawning.Ground), size = 4) + theme_bw() + ggtitle("Comparing Spawning Grounds")
      
      
            
mean(data_NoJuv$TS)      

# Acoustic catch at age

ggplot(aCAAL, aes(x=Age, y=Length)) + geom_point(aes(colour = Year), size = 2) + theme_bw() + ggtitle("Comparing Spawning Grounds")

ggplot(aCAAW, aes(x=Age, y=Weight)) + geom_point(aes(colour = Year), size = 2) + theme_bw() + ggtitle("Comparing Spawning Grounds")

ggplot(averageAL_G, aes(x=Age, y=Length)) + geom_point()+ theme_bw() + ggtitle("Comparing Spawning Grounds")


head(averageAL_G)

ggplot(aCAAW, aes(x=Age, y=Weight)) + geom_point(aes(colour = Year), size = 2) + theme_bw() + ggtitle("Comparing Spawning Grounds")






head(averageAL)
ggplot(averageAL, aes(x=Age, y=Length)) + geom_point( size = 2) + theme_bw() + ggtitle("Comparing Spawning Grounds")

head(YearAverageL)
head(averageLY)

ggplot(averageLY, aes(x=Year, y=Length)) + geom_point(alpha = 0.3) + theme_bw() + ggtitle("Comparing Spawning Grounds") + geom_point(data = YearAverageL, aes(x = Year, y = MeanLengthmm ), colour = "red")


ggplot(z, aes(x=X, y=Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box, fill = Box), colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black')  + geom_point(pch=21, size = 2, fill = "White") + ggtitle("Tag Releases") + labs(x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey68"))


      
# Weight/Length

ggplot(data_NoJuv1, aes(x=Date, y=MeanWeightgm)) + geom_point(aes(colour = Year, shape = Spawning.Ground), size = 2) + ggtitle("Average Weight Trends") + theme_bw() + geom_point(data=data_NoJuv1, aes(x=Date, y=MeanLengthmm))


ggplot(data_NoJuv1, aes(x = Date, y = MeanWeightgm, color=MeanLengthmm)) +geom_point()


+ geom_text(label = data_NoJuv1$MeanLengthmm)



ggplot(data_NoJuv1, aes(x=Date, y=MeanWeightgm)) + geom_point(aes(colour = Year, shape = Spawning.Ground), size = 2) + ggtitle("Average Weight Trends") + theme_bw()+ 



ggplot(data_NoJuv, aes(x=Date, y=MeanLengthmm)) + geom_point(aes(colour = Year, shape = Spawning.Ground), size = 2) + ggtitle("Average Length Trends") + theme_bw()+ geom_smooth(span = 0.5)

   
#t-test
str(data_NoJuv)
t.test(MeanWeightgm ~ Spawning.Ground, data = data_NoJuv)
t.test(MeanWeightgm ~ Spawning.Ground, data = last10)

t.test(MeanLengthmm ~ Spawning.Ground, data = data_NoJuv)
t.test(MeanLengthmm ~ Spawning.Ground, data = last10)










 # Standards
    
    ggplot(data_NoJuv, aes(x=Date, y=MeanWeightgm)) + geom_point(aes(colour = Year, shape = Spawning.Ground), size = 2) + ggtitle("Average Weight Trends") + theme_bw()+ geom_hline (yintercept=180, linetype = "dashed", colour = "red")
    
      Sweight=data_NoJuv[which(data_NoJuv$MeanWeightgm >= 180), ]
      dim(Sweight)
      dim(data_NoJuv)    
    
    ggplot(data_NoJuv, aes(x=Date, y=MeanLengthmm)) + geom_point(aes(colour = Year, shape = Spawning.Ground), size = 2) + ggtitle("Average Length Trends") + theme_bw()+ geom_hline (yintercept=280, linetype = "dashed", colour = "red")
    
    + geom_smooth(span = 0.5)

    Slength=data_NoJuv[which(data_NoJuv$MeanLengthmm >= 280), ]
    dim(Slength)
    dim(data_NoJuv)    
    

ggplot(data_NoJuv, aes(x=Date, y=MeanWeightgm)) + geom_point(aes(colour = Year, shape = Spawning.Ground), size = 2) + ggtitle("Average Weight Trends") + theme_bw()






ggplot(German, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year), size = 2) + ggtitle("Average Weight Trends on German Bank") + theme_bw()




ggplot(Scots, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year), size = 2) + ggtitle("Average Weight Trends on Scots Bay") + theme_bw()

# remove




ggplot(AverageWG, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year), size = 3) + ggtitle("Average of Averages for Weight German Bank") + theme_bw()

ggplot(AverageWS, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year), size = 3) + ggtitle("Average of Averages for Weight Scots Bay") + theme_bw()




ggplot(German, aes(x=Date, y=MeanWeightgm)) + geom_point(aes(colour = Year)) + ggtitle("Average Weight Trends on German Bank")+ geom_smooth(colour = "red")

ggplot(last10G, aes(x=Date, y=MeanWeightgm)) + geom_point(aes(colour = Year)) + ggtitle("Average Weight Trends on German Bank")+ geom_smooth(span = 0.5, se = FALSE)

AverageWG<-aggregate(MeanWeightgm~Year, last10G, FUN=mean)
AverageWG1<-aggregate(MeanWeightgm~Year, Scots, FUN=mean)

ggplot(AverageWG_, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year, size = 5)) + ggtitle("Average Weight German Bank")+  geom_hline (yintercept=155.2575, linetype = "dashed", colour = "red")


head(AverageWG_)
mean(AverageWG_$MeanWeightgm)


ggplot(AverageWS_, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year, size = 5)) + ggtitle("Average Weight Scots Bay")+  geom_hline (yintercept=154.6662, linetype = "dashed", colour = "red")


mean(AverageWS_$MeanWeightgm)


ggplot(AverageLG, aes(x=Year, y=MeanLengthmm)) + geom_point(aes(colour = Year, size = 5)) + ggtitle("Average Length German Bank")+  geom_hline (yintercept=270.968, linetype = "dashed", colour = "red")


mean(AverageLG$MeanLengthmm)



ggplot(AverageLS, aes(x=Year, y=MeanLengthmm)) + geom_point(aes(colour = Year, size = 5)) + ggtitle("Average Length Scots Bay")+  geom_hline (yintercept=269.1297, linetype = "dashed", colour = "red")

mean(AverageLS$MeanLengthmm)









ggplot(last10G, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year))


ggplot(last10G, aes(x=Year, y=MeanLengthmm)) + geom_point(aes(colour = Year))
ggplot(German, aes(x=Year, y=MeanLengthmm)) + geom_point(aes(colour = Year)) +geom_line() 



# Scots Bay
ggplot(Scots, aes(x=MeanLengthmm, y=MeanWeightgm)) + geom_point(aes(colour = Year))
ggplot(last10S, aes(x=MeanLengthmm, y=MeanWeightgm)) + geom_point(aes(colour = Year))
ggplot(last10S, aes(x=Year, y=TS)) + geom_point(aes(colour = Year))
ggplot(last10S, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year))
ggplot(last10S, aes(x=Year, y=MeanLengthmm)) + geom_point(aes(colour = Year))
ggplot(Scots, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year))
ggplot(Scots, aes(x=Year, y=MeanLengthmm)) + geom_point(aes(colour = Year))
ggplot(NoJuvS, aes(x=Year, y=MeanLengthmm)) + geom_point(aes(colour = Year)) + geom_line()
ggplot(NoJuvS, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year))+geom_line()





