
# add categories for depth, as in if the cast was greater than 30m and can be used in stratification analyses, also whether the cast was located within a plankton box.

ctdEvent_2 = function(events){
  
  
  # now we need a category to differentiate casts that are > 30m
  # Can subset, add the category then recombine
  
          Events30 = events[which(events$Depth > 29.9), ]
          Events30$DepthCat = 1
          
                Events1 = events[which(events$Depth < 29.9), ]
                Events1$DepthCat = 0
  
                      eve = rbind(Events1, Events30)
                          eve$DepthCat = as.factor(eve$DepthCat)
                          
                          

                    
                        
  
  return(eve)
                  }
  
  