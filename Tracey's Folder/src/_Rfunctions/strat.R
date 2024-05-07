

strat = function(depthLowerLimit, depthUpperLimit, data, events) {
  
  depth = data[which(data$Depth > depthLowerLimit), ]
  depth1 = depth[which(depth$Depth < depthUpperLimit), ]
  
  
  # For 30m measurements (atDepth)
  id = unique(depth1$id)
  id = as.data.frame(id)
  
  
  
  r = data.frame( cbind( id=id, depth30= NA, avgTemp30 = NA, avgSal30 = NA, seTemp30 = NA, seSal30  = NA, density30 = NA, seDensity30 = NA), stringsAsFactors = FALSE )  
  
  for (i in 1:nrow(r)){
    test = which(depth1$id==r[i, "id"])
    test1 = depth1[test, ]
    r$depth30[i] = max(test1$Depth)
    r$avgTemp30[i] = mean(test1$Temperature)
    r$avgSal30[i] = mean(test1$Salinity)
    r$seTemp30[i] = se(test1$Temperature)
    r$seSal30[i] = se(test1$Salinity)
    r$density30[i] = mean(test1$Density)
    r$seDensity30[i] = se(test1$Density)
    
    
  }
  
  
  # not sure why one ID I get NA's for the se values.
      whyNA=data[which(data$id == "10J101609_20190721_224308"), ]
      se(whyNA$Salinity)
      se(whyNA$Temperature)
      se(whyNA$Density)
      
      r[20,5] = se(whyNA$Temperature)
      r[20,6] = se(whyNA$Salinity)
      r[20,8] = se(whyNA$Density)

      thirty = r
  
  
  
  
       atDepth  = merge(thirty, events, by = "id")
  
  
  
  return(r)
  
  }
  