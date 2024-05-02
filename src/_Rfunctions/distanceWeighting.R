

# Distance weighting function:

distanceWeighting = function(x){
  
  
  x$OverallD = sum(x$Dist_T)  
  x$weightFactor = (x$Dist_T)/(x$OverallD)
  x$WeightedEmpirical = x$linearBackscatter*x$weightFactor
  x$overallABS = sum(x$WeightedEmpirical)
  x$WeightedABS = 10*log10(x$overallABS)  
    
  
  return(x)
  
}