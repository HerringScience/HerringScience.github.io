
#----------------------------------------------------
# generate map using PBSmapping plotting functions
#----------------------------------------------------

# makemap= function(x,area="all", wd="C:/Rsaves/maps", addlabels=T, title="" ){
  require(PBSmapping)
  wd = "D:/Jenna/workspace"
  area="all"
  
  borders= read.csv(file=file.path(wd,"areaborders.csv"), head=T, sep=",")
  b=borders[which(borders$area==area),]

# read in shapefiles
#--------------------------------------
  basemap= importShapefile(file.path(wd,"map_base_region"))
  dm200= importShapefile(file.path(wd,"dm200_region"))
  dm100= importShapefile(file.path(wd,"dm100_region"))
  zones= importShapefile(file.path(wd,"sczones2010_polyline"))
  land= importShapefile(file.path(wd,"landmass_region"))
  coast=importShapefile(file.path(wd,"coastline_polyline"))
  axis=importShapefile(file.path(wd,"axis_polyline"))

# Provide projection information
#---------------------------------
  proj.abbr=attr(basemap, "projection") # abbreviated projection info
  proj.full=attr(basemap, "prj") # full projection info

  ylim=c(b$slat,b$nlat)
  xlim=c(-(b$wlon),-(b$elon))

   plotPolys(basemap, projection=proj.abbr, col="royalblue2", border="black",
   font.lab=2,  xlab="Longitude", ylab="Latitude", axes=T, tck=-.01,
   tckLab=TRUE, ylim=ylim, xlim=xlim)
     
  title(main=title, line=2)
  addPolys(dm200, col="steelblue2", border="steelblue2")
  addPolys(dm100, col="lightblue1", border="lightblue1")
  # this is for separating fishing areas, not needed for herring
      # addLines(zones, col="darkgoldenrod1", lwd=2)


#Overlay land and coastline such that any bad data (on land) is hidden
  addPolys(land, col="khaki", border="khaki")
  addLines(coast, col="black")
  abline(h=b$slat, lwd=3)
  abline(h=b$nlat, lwd=3)
  abline(v=-b$wlon, lwd=3)
  abline(v=-b$elon, lwd=3)
  
# Add points for herring
  points(Y~X, sets, col="indianred3", cex = 1, lwd = 2)
  legend("topleft", bg="whitesmoke", col=icol, lty= 1, lwd=0.2, legend=as.character(isob))

  #function to add area labels
#--------------------------------------------
  if (addlabels) {
    text("CFA 23", x=-58.05, y=44.55, font=2, cex=1.0)
    text("CFA 24", x=-60.9, y=43.75, font=2, cex=1.0)
    text("CFA 4X", x=-64.2, y=43.25, font=2, cex=1.0)
    text("N-ENS", x= -59.15, y=46.65, font=2, cex=1.0)
  }
 

}




cover= function(x,area="all"){
  require(PBSmapping)

  borders= read.csv(file=file.path("C:/Rsaves/maps/areaborders.csv"), head=T, sep=",")
  b=borders[which(borders$area==area),]

# read in shapefiles
#--------------------------------------
  land= importShapefile(file.path(wd,"landmass_region"))
  coast=importShapefile(file.path(wd,"coastline_polyline"))


#Overlay land and coastline such that any bad data (on land) is hidden
  addPolys(land, col="khaki", border="khaki")
  addLines(coast, col="black")
  
  
  abline(h=b$slat, lwd=3)
  abline(h=b$nlat, lwd=3)
  abline(v=-b$wlon, lwd=3)
  abline(v=-b$elon, lwd=3)

}


#### Add Shrimp Boxes

 bn=read.csv("C:\\Rsaves\\maps\\shrimpboxes\\bn.csv", header=T)
 bn$PID=as.numeric(paste(1:nrow(bn),  sep=""))
 bn$POS=bn$PID
 attr(bn,"projection")<-"LL"
 
 addPolys(bn)


 sb=read.csv("C:\\Rsaves\\maps\\shrimpboxes\\sb.csv", header=T)
 sb$PID=as.numeric(paste(1:nrow(sb),  sep=""))
 sb$POS=sb$PID
 attr(sb,"projection")<-"LL"
 
 addPolys(sb, col="black")

 #Create own draw.bubble function
 #use to create maps with graduated symbols (such as weights)
 

draw.bubble= function (x, y, z, maxradius = 1, ...) 
{
    cex <- 2 * maxradius/par("cxy")[2]/0.375
    maxz <- max(z, na.rm = T)
    points(x, y, cex = cex * sqrt(z)/sqrt(maxz), ...)
}

 #Create own legend.bubble function
 #use to create legends for maps with graduated symbols (such as weights)
 
legend.box= function (x, y = NULL, maxradius, mab = 1.2, inset = 0, double = F) 
{
    auto <- if (is.character(x)) 
        match.arg(x, c("bottomright", "bottom", "bottomleft", 
            "left", "topleft", "top", "topright", "right", "center"))
    else NA
    asp <- get.asp()
    h <- mab * 2 * maxradius
    w <- h * asp
    if (double) 
        h <- h * 2
    usr <- par("usr")
    inset <- rep(inset, length.out = 2)
    if (!is.na(auto)) {
        insetx <- inset[1L] * (usr[2L] - usr[1L])
        left <- switch(auto, bottomright = , topright = , right = usr[2L] - 
            w - insetx, bottomleft = , left = , topleft = usr[1L] + 
            insetx, bottom = , top = , center = (usr[1L] + usr[2L] - 
            w)/2)
        insety <- inset[2L] * (usr[4L] - usr[3L])
        top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] + 
            h + insety, topleft = , top = , topright = usr[4L] - 
            insety, left = , right = , center = (usr[3L] + usr[4L] + 
            h)/2)
    }
    else {
        left <- x - 1.2 * asp * maxradius
        top <- y + 1.2 * maxradius
    }
    return(c(left, top, left + w, top - h))
}
 
legend.bubble= function (x, y = NULL, z, maxradius = 1, n = 3, round = 0, bty = "o", 
    mab = 1.2, bg = NULL, inset = 0, pch = 21, pt.bg = NULL, 
    txt.cex = 1, txt.col = NULL, font = NULL, txt.adj=c(0.5, -0.5), ...) 
{
    if (length(z) == 1) 
        legend <- round((seq(0, sqrt(z), length.out = n + 1)^2)[-1], 
            round)
    else legend <- round(sort(z), round)
    radius <- maxradius * sqrt(legend)/sqrt(max(legend))
    cex <- 2 * radius/par("cxy")[2]/0.375
    box <- legend.box(x, y, maxradius, mab, inset)
    if (bty == "o") 
        rect(box[1], box[2], box[3], box[4], col = bg)
    x <- (box[1] + box[3])/2
    y <- box[2] - mab * maxradius + maxradius
    for (i in length(radius):1) {
        ri <- radius[i]
        cex <- 2 * ri/par("cxy")[2]/0.375
        points(x, y - ri, cex = cex, pch = pch, bg = pt.bg, ...)
        text(x, y - ri * 2, legend[i], adj = txt.adj, cex = txt.cex, 
            col = txt.col, font = font)
    }
}

 
#-----------------------------------------------------
# Add Data Points
#-----------------------------------------------------


#pointplot= function(x,area="ens", wd="C:/Rsaves/maps", addlabels=T){
#  require(PBSmapping)
#  makemap(x, area=area)
#
## Format dataframe to be able to convert to EventData
#  x=x[!is.na(x$lat),]
#  x=x[!is.na(x$lon),]
#  x$X=-(x$lon)
#  x$Y=x$lat
#  x$EID=as.numeric(paste(1:nrow(logs),  sep=""))
#  
#  # Convert to EventData
#  
#  x=as.EventData(x, projection="LL")
#  
#





#makemap(x,area="cfa23")

#addPoints(data=, xlim=xlim, ylim=ylim, col=red, pch=20, x=-(x$lon),y=x$lat)
#
## note that importShapefile reads the .prj file if it exists, but it
## does not adopt the proj4 format used by the above approaches
#proj.abbr=attr(land, "projection") # abbreviated projection info
#proj.full=attr(land, "prj") # full projection info
#print(proj.abbr)
## [1] "LL"
#
## generate map using PBSmapping plotting functions
#
#
#addlabels(x)
## add labels to various polygons
#
#

