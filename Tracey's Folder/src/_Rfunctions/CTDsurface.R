


CTDsurface = function(data, events) {


# Calculate averages for the surface
      SST=data[which(data$Depth <= 1), ]


    # For SST
        id = unique(SST$id)
        id = as.data.frame(id)
        r = data.frame( cbind( id=id, depth1 = NA, avgTemp1 = NA, avgSal1 = NA, seTemp1 = NA, seSal1  = NA, density1=NA, seDensity1=NA ), stringsAsFactors = FALSE )  

      for (i in 1:nrow(r)){
        test = which(SST$id==r[i, "id"])
        test1 = SST[test, ]
        r$depth1[i] = max(test1$Depth)
        r$avgTemp1[i] = mean(test1$Temperature)
        r$avgSal1[i] = mean(test1$Salinity)
        r$seTemp1[i] = se(test1$Temperature)
        r$seSal1[i] = se(test1$Salinity)
        r$density1[i] = mean(test1$Density)
        r$seDensity1[i] = se(test1$Density)
      }


        one = r
        
      # All casts have measurements at the surface
      oneOff  = merge(one, events, by = "id")

      return(oneOff)
      
}
      