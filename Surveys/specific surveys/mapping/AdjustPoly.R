     
# Use this script to custom/adjust polygons for area calucaltions of German Bank or Scot's Bay
  
  # Determine which vessel's line needs to be adjusted and create a df which only those lines
  test=surveyLines_major[which(surveyLines_major$Vessel == "MS"), ]
  # The names of the transects
  unique(test$Region_name)
  
  # Plot the lines of which you have specified in tests
  ggplot(test, aes(x=X, y=Y))  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Region_name), size = 1)+ labs(x=NULL, y=NULL) 
  # If you want land too...
  + geom_polygon(data=out,aes(x=long, y=lat, group=group))
  
  # Create a dataframe with only the desired transect
  test2=test[which(test$Region_name == "SL_T01"), ]
  
  # Determine the last point of the tranect at the end that needs to be adjusted
  # For the top Y
  max(test2$Yend)
  max(test2$Xend)
  # For the bottom Y  
  min(test2$Y)
  min(test2$X)
  
  # For the X coordinate  
  mean(test2$X)
  
  # To determine the order of the polygons
  ggplot(data=polyGB, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))
  
  # Make changes in the saved .csv file saved in the workspace

----------------------------------------------------------------------------------
  # Test out the changes made in the .csv file
  
  SUA = read.csv("polygon_SB3.csv")
  polySB_main = as.PolySet(SUA, projection="LL")
  
  ggplot(poly6,aes(x=X, y=Y)) + geom_polygon() + geom_segment(data=surveyLines_major,aes(x = X, y = Y, xend = Xend, yend = Yend)) 
  
  # Determine new points (tool is not super accurate to where you point so can exaggerate)
  gglocator()

  # Look at the  order of the polygons
  ggplot(data=poly6, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))
 
  

  
  

  
  
  
  
  
  
  