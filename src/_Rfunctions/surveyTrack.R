
surveyTrack = function(x, polyNameA,  title) {

  plot =    ggplot(x, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black') + geom_polygon(data=polyNameA,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map() + theme_dark() 
  print(plot)
  
}
   
# geom_polygon(data=polyNameC,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + 