
resultTableA = function(x) {

# Table with transect details
  A = data.frame(x$Vessel, x$Transect_No, x$Date_Time_S, x$Date_Time_E, x$X, x$Y, x$Xend, x$Yend, x$Dist_T) 

 colnames(A) = c("Vessel", "Transect No.", "Date Time Start", "Date Time End", "Start Lon", "Start Lat", "End Lon", "End Lat", "Dist (km)")
 
 return (A)

}      

    


  
  