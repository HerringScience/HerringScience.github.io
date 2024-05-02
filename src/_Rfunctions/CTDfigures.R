

# Create figures for the report

CTDfigs = function(events, oneOff, atDepth, stratification) {

  
  
  # Figure that looks at the temporal distribution of CTD casts amoungst years using Julian data
  # Need to separate data by ground - only look at German Bank and Scots Bay as those are the only grounds   # that have multi year data
  
        # First take the event data and separate:
        german=events[which(events$ground == "German Bank"), ]
        scots=events[which(events$ground == "Scots Bay"), ]

                # German Bank
                GBTemporalCoverage = ggplot(german, aes(x = julianDay, y = Year)) + geom_point(shape = 21, colour = "black", fill = "white", size = 2.5, stroke = 2.5) + scale_x_continuous(limits = c(200,300)) + theme(axis.title.x = element_text( size=12)) + theme(axis.title.y = element_text( size=12)) + theme(axis.text.x = element_text(size=12))+ theme(axis.text.y = element_text(size=12)) + ggtitle("German Bank")
                
                ggsave("GBTemporalCoverage.png")
                
                
                # Scots Bay
                SBTemporalCoverage = ggplot(german, aes(x = julianDay, y = Year)) + geom_point(shape = 21, colour = "black", fill = "white", size = 2.5, stroke = 2.5) + scale_x_continuous(limits = c(200,300)) + theme(axis.title.x = element_text( size=12)) + theme(axis.title.y = element_text( size=12)) + theme(axis.text.x = element_text(size=12))+ theme(axis.text.y = element_text(size=12)) + ggtitle("Scots Bay")
                
                ggsave("SBTemporalCoverage.png")
                
                
                
                
                
                


# Surface Temperature/Salinity and Julian Day
              scots=oneOff[which(oneOff$ground == "Scots Bay"), ]
              german=oneOff[which(oneOff$ground == "German Bank"), ]
             
              
              
              
              # Scots Bay 
              SBAverageSST = ggplot(scots, aes(x = julianDay, y = avgTemp1)) + geom_point(aes(colour = Year),pch = 21, size = 2.5, fill = "white", stroke = 2.5) + ylab("Average Sea Surface Temperature") + ggtitle("Scots Bay")

              ggsave("SBAverageSST.png")
              
              # German Bank
              GBAverageSST = ggplot(german, aes(x = julianDay, y = avgTemp1)) + geom_point(aes(colour = Year),pch = 21, size = 2.5, fill = "white", stroke = 2.5) + ylab("Average Sea Surface Temperature") + ggtitle("German Bank")
              
              ggsave("GBAverageSST.png")
              
              
            
              
              
              
# Average salinity at the surface
                  # German  
                  GBSurfaceSalinity = ggplot(german, aes(x = julianDay, y = avgSal1)) + geom_point(aes(colour = Year),pch = 21, size = 2.5, fill = "white", stroke = 2.5) + ylab("Average Salinity at Surface") + ggtitle("German Bank")
              
                  ggsave("GBSurfaceSalinity.png")
                  
                  
                  SBSurfaceSalinity = ggplot(scots, aes(x = julianDay, y = avgSal1)) + geom_point(aes(colour = Year),pch = 21, size = 2.5, fill = "white", stroke = 2.5) + ylab("Average Salinity at Surface") + ggtitle("Scots Bay")
                  
                  ggsave("SBSurfaceSalinity.png")
                  
                  
              
                  
                  
                  
                  
              
# at Depth
                  
            # using atDepth
            scots=atDepth[which(atDepth$ground == "Scots Bay"), ]
            german=atDepth[which(atDepth$ground == "German Bank"), ]
            
            
            
              # Temperature:        
                # Scots Bay
                SBTempDepth  = ggplot(scots, aes(x = julianDay, y = avgTemp30)) + geom_point(aes(colour = Year),pch = 21, size = 2.5, fill = "white", stroke = 2.5) + ylab("Average At Depth Temperature") + ggtitle("Scots Bay")
    
                    ggsave("SBTempDepth.png")
                    
                # German Bank
                SBTempDepth  = ggplot(scots, aes(x = julianDay, y = avgTemp30)) + geom_point(aes(colour = Year),pch = 21, size = 2.5, fill = "white", stroke = 2.5) + ylab("Average At Depth Temperature") + ggtitle("Scots Bay")
                
                ggsave("SBTempDepth.png")
                
                
                
                
                
                # Average Salinities at the depth
                # Scots Bay
                SBSalDepth  = ggplot(scots, aes(x = julianDay, y = avgSal30)) + geom_point(aes(colour = Year),pch = 21, size = 2.5, fill = "white", stroke = 2.5) + ylab("Average At Depth Salinity") + ggtitle("Scots Bay")
                
                ggsave("SBSalDepth.png")
                
                # German Bank
                GBSalDepth  = ggplot(german, aes(x = julianDay, y = avgSal30)) + geom_point(aes(colour = Year),pch = 21, size = 2.5, fill = "white", stroke = 2.5) + ylab("Average At Depth Salinity") + ggtitle("German Bank")
                
                ggsave("GBSalDepth.png")
                
                
            
                
                
                
                
                
# Stratification
          
          # using stratification
            colnames(stratification)

            scots=stratification[which(stratification$ground == "Scots Bay"), ]
            german=stratification[which(stratification$ground == "German Bank"), ]
            
            
            
            
            
            # Temperature
            GBTempStrat = ggplot(german, aes(x = julianDay, y = tempDiff)) + geom_point(aes(colour = Year),pch = 21, size = 2.5, fill = "white", stroke = 2.5) + ylab("Temperature Difference") + ggtitle("German Bank")

                        ggsave("GBTempStrat.png")
            
            
            SBTempStrat = ggplot(scots, aes(x = julianDay, y = tempDiff)) + geom_point(aes(colour = Year),pch = 21, size = 2.5, fill = "white", stroke = 2.5) + ylab("Temperature Difference") + ggtitle("Scots Bay")
                        
                        ggsave("SBTempStrat.png")
                        
                        
                        
                        
                        

            
            # salinity stratification
                        GBSalStrat = ggplot(german, aes(x = julianDay, y = salDiff)) + geom_point(aes(colour = Year),pch = 21, size = 2.5, fill = "white", stroke = 2.5) + ylab("Salinity Difference") + ggtitle("German Bank")
                        
                        ggsave("GBSalStrat.png")
                        
                        
                        SBSalStrat = ggplot(scots, aes(x = julianDay, y = salDiff)) + geom_point(aes(colour = Year),pch = 21, size = 2.5, fill = "white", stroke = 2.5) + ylab("Salinity Difference") + ggtitle("Scots Bay")
                        
                        ggsave("SBTempStrat.png")
                        
                        
                        
                        
                        
                        
                        
                        
            # density stratification
                        GBDensStrat = ggplot(german, aes(x = julianDay, y = densDiff)) + geom_point(aes(colour = Year),pch = 21, size = 2.5, fill = "white", stroke = 2.5) + ylab("Density Difference") + ggtitle("German Bank")
                        
                        ggsave("GBDensStrat.png")
                        
                        
                        SBDensStrat = ggplot(scots, aes(x = julianDay, y = densDiff)) + geom_point(aes(colour = Year),pch = 21, size = 2.5, fill = "white", stroke = 2.5) + ylab("Density Difference") + ggtitle("Scots Bay")
                        
                        ggsave("SBDensStrat.png")

}
                        