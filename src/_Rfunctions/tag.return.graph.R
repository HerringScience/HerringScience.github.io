
tag.return.graph <- function(r, boxes, na.rm = TRUE){
  
  # where do I want the maps saved?
  results = "C:/Users/herri/OneDrive/Documents/Jenna/workspace/"
  

  # create list of counties in data to loop over 
  county_list <- unique(r$no)
  
  
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(county_list)) { 
    
    # create plot for each county in df 
    plot <- 
      ggplot(subset(r, r$no==county_list[i]),
             aes(X, Y)) + 
      geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey78',col='black') + geom_path(aes(group=TAG_NUMBER, colour = daysAtLarge), lwd = 0.9, alpha = 0.7) + geom_point(aes(shape = TYPE, colour = daysAtLarge), size = 2, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map() + theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      
      ggtitle(paste(county_list[i]))
    
    # save plots as .png
    ggsave(plot, file=paste(results, county_list[i], ".png", sep="_"), scale=2)
    
    
    # print plots to screen
    print(plot)
  }
}