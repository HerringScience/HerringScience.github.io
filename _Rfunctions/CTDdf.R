

# Script that uses ctd raw data and event data to create a spreadsheet that has the event data with min, max average, etc, values for each cast

CTDdf = function(data, events) {

  
  
  
# Averages
          AvgSalinity<-aggregate(Salinity~id, data, FUN=mean)
          events_1 = merge(events,AvgSalinity, by = "id")
          AvgTemperature<-aggregate(Temperature~id, data, FUN=mean)
          events_2 = merge(events_1,AvgTemperature, by = "id")
          AvgDensity<-aggregate(Density~id, data, FUN=mean)
          events_3 = merge(events_2,AvgDensity, by = "id")

          head(events_3)          
          colnames(events_3) = c("id", "ground", "plankton_ID","sampler", "Date", "Lon", "Lat","Year","julianDay", "Week","device", "Depth", "DepthCat", "AvgSalinity", "AvgTemperature", "Avg Density")  
  
        
          
# Standard error
          SeSalinity<-aggregate(Salinity~id, data, FUN=se)
          events_4 = merge(events_3,SeSalinity, by = "id")
          SeTemperature<-aggregate(Temperature~id, data, FUN=se)
          events_5 = merge(events_4,SeTemperature, by = "id")
          SeDensity<-aggregate(Density~id, data, FUN=se)
          events_6 = merge(events_5,SeDensity, by = "id")
      
          colnames(events_6) = c("id", "ground", "plankton_ID", "sampler", "Date", "Lon", "Lat","Year","julianDay", "Week", "device", "Depth", "DepthCat" ,"AvgSalinity", "AvgTemperature","AvgDensity", "SeSalinity", "SeTemperature", "SeDensity")  
          
      
      
# Max
          MaxSalinity<-aggregate(Salinity~id, data, FUN=max)
          events_7 = merge(events_6,MaxSalinity, by = "id")
          MaxTemperature<-aggregate(Temperature~id, data, FUN=max)
          events_8 = merge(events_7,MaxTemperature, by = "id")
          MaxDensity<-aggregate(Density~id, data, FUN=max)
          events_9 = merge(events_8,MaxDensity, by = "id")
          
          colnames(events_9) = c("id", "ground", "plankton_ID", "sampler", "Date", "Lon", "Lat","Year","julianDay", "Week", "device", "Depth", "DepthCat" , "AvgSalinity", "AvgTemperature","AvgDensity", "SeSalinity", "SeTemperature", "SeDensity", "MaxSalinity", "MaxTemperature",  "MaxDensity")
          
  head(events_9)
  
  
  # Min
              MinSalinity<-aggregate(Salinity~id, data, FUN=min)
              events_10 = merge(events_9,MinSalinity, by = "id")
              MinTemperature<-aggregate(Temperature~id, data, FUN=min)
              events_11 = merge(events_10,MinTemperature, by = "id")
              MinDensity<-aggregate(Density~id, data, FUN=min)
              events_12 = merge(events_11,MinDensity, by = "id")
              
              colnames(events_12) = c("id", "ground", "plankton_ID", "sampler", "Date", "Lon", "Lat","Year","julianDay", "Week", "device", "Depth", "DepthCat", "AvgSalinity", "AvgTemperature","AvgDensity", "SeSalinity", "SeTemperature", "SeDensity", "MaxSalinity", "MaxTemperature",  "MaxDensity", "MinSalinity", "MinTemperature", "MinDensity")
 
              
              head(events_12)
              
                           
  events_12$RangeTemp <-(events_12$MaxTemperature) - (events_12$MinTemperature)
  events_12$RangeSalinity <-(events_12$MaxSalinity) - (events_12$MinSalinity)
  events_12$RangeDensity <-(events_12$MaxDensity) - (events_12$MinDensity)
  
  
  return(events_12)
  
  
}