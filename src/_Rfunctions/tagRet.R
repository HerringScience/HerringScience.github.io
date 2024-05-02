
tagRet = function (r1, r2) {

  # Sort by group
  r1 = r1[order(r1$group),]
  r2 = r2[order(r2$group),]

        # Add month, year and format
        r1$month = month(r1$date)
        r1$month = as.factor(r1$month)
        
        r2$month = month(r2$date)
        r2$month = as.factor(r2$month)
            
            r1$year = year(r1$date)
            r1$year = as.factor(r1$year)
            
            r2$year = year(r2$date)
            r2$year = as.factor(r2$year)
              
            # Round GPS coordinates all to 4 sig digets.
              r1$X = round(r1$X, digits = 4)
              r1$Y = round(r1$Y, digits = 4)
              r2$X = round(r2$X, digits = 4)
              r2$Y = round(r2$Y, digits = 4)

                    x = r1$div
                      r2$div = x
                        r = rbind(r1, r2)
                        
                        # Format as factor  
                          r$group = as.factor(r$group)
                            r$daysAtLarge = as.factor(r$daysAtLarge)

                            
                            return (r)


}
