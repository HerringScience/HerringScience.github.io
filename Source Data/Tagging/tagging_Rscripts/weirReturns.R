

### Weir/Shut Off Return

weir = read.csv("returnWeir.csv")
boxes = read.csv("catchBoxes_.csv")

# only plot return point

ggplot( weir, aes(x=X, y=Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey80',col='black') + geom_text_repel(aes(x = X, y = Y, label = TAG_NUMBER, colour = "blue"))

ids = weir$TAG_NUMBER
length(ids)
dim(ids)

# try a subset:
ids = c( 442927,444728, 430067, 442133) 

# finished
452393, 450805, 449132, 442103, 445385, 450542,  44790, 44358, 452653, 452321, 443959, 456629 ,449455, 450645, 442815, 445366 ,445556, 449480,  44374,  44089,  44639, 204314, 449786, 449786 ,450689, 450446, 442788, 442510, 449347, 442864 

weiry = returnComplete[which((returnComplete$TAG_NUMBER %in% ids)), ]

## Plot tag returns that were tagged on feeding grounds in the prespawn season and recaptured in Scots
ggplot(weiry,aes(X, Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_path(aes(group=group, colour = daysAtLarge), lwd = 1, alpha = 0.7) + geom_point(aes(shape = type, colour = daysAtLarge), size = 1.6, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()  + theme(panel.background = element_rect(fill = "grey68"))



weird=weiry[which(weiry$type == 2), ]


weiry

ggplot(weiry,aes(X, Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='white',colour='black') + geom_point(aes(colour = type), size = 4, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()  + theme(panel.background = element_rect(fill = "grey68"))



weiry
# Plot each individual tag return


prescots$type = as.factor(prescots_$type)

county_list <- unique(prescots$TAG_NUMBER)

r = prescots
r$div = prescots$TAG_NUMBER

head(r)
str(r)


# create for loop to produce ggplot2 graphs 
for (i in seq_along(county_list)) { 
  
  # create plot for each county in df 
  plot <- 
    ggplot(subset(r, r$div==county_list[i]),
           aes(X, Y)) + 
    geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_path(aes(group=group, colour = daysAtLarge), lwd = 1.5, alpha = 0.7) + geom_point(aes(shape = type, colour = daysAtLarge), size = 3, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()  + theme(panel.background = element_rect(fill = "grey68")) +
    
    ggtitle(paste(county_list[i]))
  
  # save plots as .png
  ggsave(plot, file=paste(results, county_list[i], ".png", sep="_"), scale=2)
  
  
  # print plots to screen
  print(plot)
}

