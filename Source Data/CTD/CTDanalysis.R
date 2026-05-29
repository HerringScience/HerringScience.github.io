

## CTD Analysis

load("oneOff.RData")
load("stratified.RData")


# Look at surface temps with oneOff:

# plot julian day and surface temp

colnames(oneOff)
head(oneOff)
colnames(stratified)

# Temperatures
ggplot(oneOff,aes(x=julianDay, y=avgTemp1)) + geom_point(aes(colour = ground, shape = Year), size = 2) + ggtitle("Surface Temperatures by Location")

# 2019 was warmer than 2017
# 2018 was warmer than 2019
# Scots is warmer than Lurcher

ggplot(stratified,aes(x=julianDay, y=avgTemp30)) + geom_point(aes(colour = ground, shape=Year), size = 2) + ggtitle("At Depth Temperatures by Location")


# Scots is warmest at depth
# Scots is warmer than Lurcher
# 2017 was coolest, than 2019, and warmest was 2018 - should estimate by how much..variability by year

ggplot(stratified,aes(x=julianDay, y=tempDiff)) + geom_point(aes(colour = ground, shape = Year), size = 2) + ggtitle(" Temperature Stratification by Location")


# Conclusions
# Scots Bay is the least stratified
                # Lurcher is colder than Scots Bay overall (depth and surface) at the same time of year
                # A outlier for stratification for each spawning ground - over 6 degrees for GB and over 2 degrees for Scots Bay
    
            
            
            
            
            
            
    # Salinity
            ggplot(oneOff,aes(x=julianDay, y=avgSal1)) + geom_point(aes(colour = ground, shape = Year), size = 2) + ggtitle("Surface Salinity by Location and Year")
            
              # Saltiest average surface measurement was in Scots Bay...very surprising..
              # 2019 was saltiest year
            
                    
            
            ggplot(stratified,aes(x=julianDay, y=avgSal30)) + geom_point(aes(colour = ground, shape = Year), size = 2) + ggtitle("At Depth Salinity by Location and Year")
              # Saltiest cast at depth was also Scots Bay -by 1 PSS, weird
            
            ggplot(stratified,aes(x=julianDay, y=tempDiff)) + geom_point(aes(colour = ground), size = 2) + ggtitle(" Temperature Stratification by Location")
            
            
    
    