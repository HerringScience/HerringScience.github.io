
library(dplyr)
library(geosphere)
library(sp)
library(raster)
library(adehabitatHR)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires) #devtools::install_github("ropensci/rnaturalearthhires") needed to run this to install it with my version of R
library(sf)
library(ggplot2)
library(units)
library(magrittr)
library(sjmisc)

options(warn = -1)


###########################################################################
# Delta-lognormal mean and variance of the mean
# Based on Pennington (1996) equations:
#   - Eq. (1): minimum-variance unbiased estimator of mean c
#   - Eq. (2): minimum-variance unbiased estimator of variance d
#   - Eq. (3): gm(t) series expansion
#   - Eq. (4): var(c) estimator
################################################################################

delta_ln_ftn <- function(d){
  
  # Extract positive (non-zero) values
  # n = total observations ; m = number of nonzeros (Pennington p.499)
  pd <- d[d>0]
  n <- length(d)
  m <- length(pd)
  
  # s = sd(log(pd)) = sqrt(s^2), used to compute gm(t) where t = s^2/2 etc.
  s <- sd(log(pd))
  
  # ---------------------------------------------------------------------------
  # gm(t) function
  # Implements Pennington’s Eq. (3):
  #   gm(t) = 1 + (m−1)/m * t + ... infinite series
  # The code implements the multiplicative recursion described under Eq. (3)
  # using the recurrence formula for the j-th term:
  #   term_j = term_{j−1} * [((m−1)^2)/( m*(m + 2j − 3)*j )] * t
  #
  # The series converges quickly unless variance is very large.
  # ---------------------------------------------------------------------------
  gm <- function(t){
    j <- 2
    
    # This is term_2 from Eq. (3): ((m−1)^3)/((m+1)*m^2*2) * t^2
    # Code uses 's' as the accumulating term, not to be confused with sd(log(pd))
    s <- (((m - 1)^3)/((m + 1) * (m^2) * 2)) * t^2
    
    # a = 1 + first term ((m−1)/m * t) + term_2
    a <- 1 + (((m - 1)/m) * t) + s
    
    # Add terms until convergence (Pennington: series stops when < 1e-6)
    while (s > 1e-06) {
      
      j <- j + 1
      
      # Recurrence ratio (Pennington Eq. 3 denominator expansion)
      b <- (((m - 1)^2)/(m * (m + 2 * j - 3) * j)) * t
      
      # Next term in series
      s <- s * b
      
      # Add to total
      a <- a + s
    }
    a
  }
  
  # ---------------------------------------------------------------------------
  # Special cases from Pennington (1996)
  # ---------------------------------------------------------------------------
  
  # m = 0 → estimator is 0 (Eq. 1)
  if(m==0){
    c <- 0
    vc <- 0
    varX <- 0
  }
  
  # m = 1 → Eq. (1) reduces to x1/n
  # variance = (x1/n)^2 (the only possible nonzero outcome)
  if(m==1){
    c <- pd/n
    vc <- (pd/n)^2
    p    <- 1/n
    varX <- p * pd^2 - (p * pd)^2
  }
  
  # ---------------------------------------------------------------------------
  # m > 1: General case
  # Implements Eq. (1) and Eq. (4) from Pennington (1996)
  # ---------------------------------------------------------------------------
  if(m>1){
    
    # ----- Eq. (1): mean estimator c -----
    # c = (m/n) * exp(mean(log(pd))) * gm(s^2/2)
    c <- m/n * exp(mean(log(pd))) * gm(0.5 * s^2)
    
    # ----- Eq. (4): variance of c (Pennington p.500) -----
    # vc = m/n * exp(2*mean(log(pd))) *
    #         [ (m/n)*gm(s^2/2)^2  −  ((m−1)/(n−1))*gm( ((m−2)/(m−1))*s^2 ) ]
    vc <- m/n * exp(2 * mean(log(pd))) *
      ( m/n * gm(0.5 * s^2)^2 -
          ((m - 1)/(n - 1) * gm((m - 2)/(m - 1) * s^2)) )
   
    # ----- Delta-lognormal population variance (not Var(mean)) -----
    p    <- m/n
    muL  <- mean(log(pd))
    sig2 <- s^2
    
    EY2 <- exp(2 * muL + 2 * sig2)
    varX <- p * EY2 - c^2
  }
  
  # Output
  mylist <- list(mean = c,
                 varmean = vc,
                 var = varX)
  names(mylist) <- c("mean", "var_mean", "var_data")
  return(mylist)
}


delta_ln_ftn_LF <- function(d){
  
  # Extract positive (non-zero) values
  # n = total observations ; m = number of nonzeros (Pennington p.499)
  pd <- d[d>0]
  n <- length(d)
  m <- length(pd)
  
  # s = sd(log(pd)) = sqrt(s^2), used to compute gm(t) where t = s^2/2 etc.
  s <- sd(log(pd))
  
  # ---------------------------------------------------------------------------
  # gm(t) function
  # Implements Pennington’s Eq. (3):
  #   gm(t) = 1 + (m−1)/m * t + ... infinite series
  # The code implements the multiplicative recursion described under Eq. (3)
  # using the recurrence formula for the j-th term:
  #   term_j = term_{j−1} * [((m−1)^2)/( m*(m + 2j − 3)*j )] * t
  #
  # The series converges quickly unless variance is very large.
  # ---------------------------------------------------------------------------
  gm <- function(t){
    j <- 2
    
    # This is term_2 from Eq. (3): ((m−1)^3)/((m+1)*m^2*2) * t^2
    # Code uses 's' as the accumulating term, not to be confused with sd(log(pd))
    s <- (((m - 1)^3)/((m + 1) * (m^2) * 2)) * t^2
    
    # a = 1 + first term ((m−1)/m * t) + term_2
    a <- 1 + (((m - 1)/m) * t) + s
    
    # Add terms until convergence (Pennington: series stops when < 1e-6)
    while (s > 1e-06) {
      
      j <- j + 1
      
      # Recurrence ratio (Pennington Eq. 3 denominator expansion)
      b <- (((m - 1)^2)/(m * (m + 2 * j - 3) * j)) * t
      
      # Next term in series
      s <- s * b
      
      # Add to total
      a <- a + s
    }
    a
  }
  
  # ---------------------------------------------------------------------------
  # Special cases from Pennington (1996)
  # ---------------------------------------------------------------------------
  
  # m = 0 → estimator is 0 (Eq. 1)
  if(m==0){
    c <- 0
    vc <- 0
    varX <- 0
  }
  
  # m = 1 → Eq. (1) reduces to x1/n
  # variance = (x1/n)^2 (the only possible nonzero outcome)
  if(m==1){
    c <- pd/n
    vc <- (pd/n)^2
    p    <- 1/n
    varX <- p * pd^2 - (p * pd)^2
  }
  
  # ---------------------------------------------------------------------------
  # m > 1: General case
  # Implements Eq. (1) and Eq. (4) from Pennington (1996)
  # ---------------------------------------------------------------------------
  if(m>1){
    
    # ----- Eq. (1): mean estimator c -----
    # c = (m/n) * exp(mean(log(pd))) * gm(s^2/2)
    c <- m/n * exp(mean(log(pd))) * gm(0.5 * s^2)
    
    # ----- Eq. (4): variance of c (Pennington p.500) -----
    # vc = m/n * exp(2*mean(log(pd))) *
    #         [ (m/n)*gm(s^2/2)^2  −  ((m−1)/(n−1))*gm( ((m−2)/(m−1))*s^2 ) ]
    vc <- m/n * exp(2 * mean(log(pd))) *
      ( m/n * gm(0.5 * s^2)^2 -
          ((m - 1)/(n - 1) * gm((m - 2)/(m - 1) * s^2)) )
    
    # ----- Delta-lognormal population variance (not Var(mean)) -----
    p    <- m/n
    muL  <- mean(log(pd))
    sig2 <- s^2
    
    EY2 <- exp(2 * muL + 2 * sig2)
    varX <- p * EY2 - c^2
  }
  
  # Output
  return(c)
}









#sort points
#' Function to sort a set of points/coordinates in a clockwise or anti-clockwise
#' direction.
#' 
#' \code{sort_points} can be useful for creating spatial lines or polygons from 
#' points. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame containing y and x points. 
#' @param y Name of y variable in \code{df}. 
#' @param x Name of x variable in \code{df}. 
#' @param clockwise Should the points be arranged in a clockwise direction? If 
#' \code{FALSE}, the order will be an anti-clockwise direction. 
#'
#' @export
sort_points <- function(df, y = "latitude", x = "longitude", clockwise = TRUE) {
  
  # NA check, if NAs drop them
  if (any(is.na(c(df[, y], df[, x])))) {
    
    # Remove NAs
    df <- df[!(is.na(df[, y]) & is.na(df[, x])), ]
    
    # Raise warning
    warning("Missing coordinates were detected and have been removed.", 
            call. = FALSE)
    
    # Check 
    if (nrow(df) == 0) stop("There are no valid coordinates.", call. = FALSE)
    
  }
  
  # Get centre (-oid) point of points
  x_centre <- mean(df[, x])
  y_centre <- mean(df[, y])
  
  # Calculate deltas
  df$x_delta <- df[, x] - x_centre
  df$y_delta <- df[, y] - y_centre
  
  # Resolve angle, in radians
  df$angle <- atan2(df$y_delta, df$x_delta)
  # d$angle_degrees <- d$angle * 180 / pi
  
  # Arrange by angle
  if (clockwise) {
    
    df <- df[order(df$angle, decreasing = TRUE), ]
    
  } else {
    
    df <- df[order(df$angle, decreasing = FALSE), ]
    
  }
  
  # Drop intermediate variables
  df[, c("x_delta", "y_delta", "angle")] <- NULL
  
  # Return
  df
  
}


land.all <- st_read("Y:/Acoustic Index Review Files/NE_10M_Coastline/ne_10m_coastline.shp")
land.all<-st_cast(land.all$geometry, "POLYGON")
land.all


topline_broken = function(x, broken = TRUE){
if (broken){
  topline_dist <-      st_distance(st_point(c(topline$Lon_S, topline$Lat_S)), st_point(c(topline$Lon_E, topline$Lat_E)), crs = crs_use)
  x[x$Region_name == topline$Region_name,]$Dist_S <- 0
  x[x$Region_name == topline$Region_name,]$Dist_E <- topline_dist*1000
}
}

map_area_bufferedcell_SB <- function(x, transect_cell, transectEastWest = TRUE){
  defaultW <- getOption("warn") # turn off warnings so people don't freak out 
  options(warn = -1) #same as above 
  crs_use <- st_crs(land.all) # type of projection
  
  #add survey boxes
  ScotsBay_survey_box <-
    st_linestring(rbind(
      c(-65.202,45.012), 
      c(-65.202,45.174), 
      c(-64.834,45.315), 
      c(-64.672,45.218), 
      c(-65.202,45.012)
    ))
  ScotsBay_survey_box <- st_sfc(ScotsBay_survey_box, crs = crs_use)
  SB_attrib <- data.frame(name = "Scots Bay Survey Box")
  SB_sf <- st_sf(SB_attrib, geometry = ScotsBay_survey_box)
  
  Latitude  <- c(x$Lat_S, x$Lat_E) #brings in lat start and end for each transect
  Longitude  <- c(x$Lon_S, x$Lon_E) #brings in long start and end for each transect
  LatLonMat <- matrix(c( Latitude,Longitude), ncol = 2) #building polygon
  colnames(LatLonMat) <- c("latitude","longitude")
  LatLondf<-as.data.frame(LatLonMat)
  df<-sort_points(LatLondf)
  df_t<-rbind(df,df[1,]) #connecting last dot to first.
  
  #wonky polygon but used to find centroid
  transect_p <-st_polygon(list(matrix( c( df_t$longitude,df_t$latitude), nrow = length(df_t$longitude), ncol=2))) #setup to create polygon
  transect_p_poly <- st_sfc(transect_p, crs = crs_use) #adds the projection to appropriately calculate the area
  transect_p_attrib <- data.frame(name = paste0(surveydate)) #adds the attribute survey date
  transect_p_sf <- st_sf(transect_p_attrib, geometry = transect_p_poly, crs = crs_use) #creates sf polygon
  
  centy <- st_transform(transect_p_sf, crs_use) %>%  #find the center of the polygon
    st_centroid() %>% 
    # this is the crs from d, which has no EPSG code:
    st_transform(., "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
    # since you want the centroids in a second geometry col:
    st_geometry()
  
  centy<-unlist(centy) #give you the centroid of the polygon, so you can sort points E and W, or N and S
  
  
  # Arrange by angle
  if (transectEastWest) { # sorts the points to East and West
    
    West <- subset(df, longitude < centy[1])
    East <-subset(df, longitude >= centy[1])
    West<-West[order(West$latitude),]
    West$ordered <-West$latitude*West$longitude
    # West<-West[order(West$ordered, decreasing=TRUE),] # manually sort. Refer to example where we manually sort.
    
    East<-East[order(East$latitude, decreasing=TRUE),]
    East$ordered <-East$latitude*East$longitude
    East<-East[order(East$ordered, decreasing=FALSE),]
    
    
    East_point <- st_as_sf(East,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    West_point <- st_as_sf(West,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    East_point_distance <- East_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    East_point_distance<-East_point_distance$dist[!is.na( East_point_distance$dist)]
    West_point_distance <- West_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    West_point_distance<-West_point_distance$dist[!is.na( West_point_distance$dist)]
    distance_between_transects<-c(East_point_distance, West_point_distance)
    d <- (mean(as.numeric(distance_between_transects)))/2
    FBS_east <- finalBearing(as(East_point[1,], "Spatial"),  as(East_point[length(East_point$latitude),], "Spatial")) #find the buffer GIS point for SouthEast
    FBS_east_point <- destPoint(as(East_point[length(East_point$latitude),], "Spatial"),  FBS_east, d)
    FBN_east <- finalBearing( as(East_point[length(East_point$latitude),], "Spatial"), as(East_point[1,], "Spatial"))
    FBN_east_point <- destPoint(as(East_point[1,], "Spatial"),  FBN_east, d)
    
    FBN_west <- finalBearing(as(West_point[1,], "Spatial"),  as(West_point[length(West_point$latitude),], "Spatial"))
    FBN_west_point <- destPoint(as(West_point[length(West_point$latitude),], "Spatial"), FBN_west, d)
    FBS_west <- finalBearing( as(West_point[length(West_point$latitude),], "Spatial"), as(West_point[1,], "Spatial"))
    FBS_west_point <- destPoint(as(West_point[1,], "Spatial"),  FBS_west, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBS_west_point[1, 1],
        latitude = FBS_west_point[1, 2],
        ordered = FBS_west_point[1, 1] * FBS_west_point[1, 2]
      ),
      West,
      data.frame(
        longitude = FBN_west_point[1, 1],
        latitude = FBN_west_point[1, 2],
        ordered = FBN_west_point[1, 1] * FBN_west_point[1, 2]
      ),
      data.frame(
        longitude = FBN_east_point[1, 1],
        latitude = FBN_east_point[1, 2],
        ordered = FBN_east_point[1, 1] * FBN_east_point[1, 2]
      ),
      East,
      data.frame(
        longitude = FBS_east_point[1, 1],
        latitude = FBS_east_point[1, 2],
        ordered = FBS_east_point[1, 1] * FBS_east_point[1, 2]
      )
    ))
    
    df2 <- rbind(df2,df2[1,])
    
  } else {
    
    South <- subset(df, latitude < centy[2])
    North <-subset(df, latitude >= centy[2])
    South<-South[order(South$longitude),]
    North<-North[order(North$longitude, decreasing=TRUE),]
    df2<-rbind(South,North)
    df2 <- rbind(df2,df2[1,])
    
    South_point <- st_as_sf(South,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    North_point <- st_as_sf(North,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    
    South_point_distance <- South_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    South_point_distance<-South_point_distance$dist[!is.na( South_point_distance$dist)]
    North_point_distance <- North_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    North_point_distance<-North_point_distance$dist[!is.na( North_point_distance$dist)]
    
    distance_between_transects<-c(South_point_distance, North_point_distance)
    d <- (mean(as.numeric(distance_between_transects)))/2
    
    FBW_south <- finalBearing(as(South_point[1,], "Spatial"),  as(South_point[length(South_point$latitude),], "Spatial"))
    FBW_south_point <- destPoint(as(South_point[length(South_point$latitude),], "Spatial"),  FBW_south, d)
    FBE_south <- finalBearing( as(South_point[length(South_point$latitude),], "Spatial"), as(South_point[1,], "Spatial"))
    FBE_south_point <- destPoint(as(South_point[1,], "Spatial"),  FBE_south, d)
    
    FBE_north <- finalBearing(as(North_point[1,], "Spatial"),  as(North_point[length(North_point$latitude),], "Spatial"))
    FBE_north_point <- destPoint(as(North_point[length(North_point$latitude),], "Spatial"), FBE_north, d)
    FBW_north <- finalBearing( as(North_point[length(North_point$latitude),], "Spatial"), as(North_point[1,], "Spatial"))
    FBW_north_point <- destPoint(as(North_point[1,], "Spatial"),  FBW_north, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBE_south_point[1, 1],
        latitude = FBE_south_point[1, 2]
        
      ),
      South,
      data.frame(
        longitude = FBW_south_point[1, 1],
        latitude = FBW_south_point[1, 2]
        
      ),
      data.frame(
        longitude = FBW_north_point[1, 1],
        latitude = FBW_north_point[1, 2]
        
      ),
      North,
      data.frame(
        longitude = FBE_north_point[1, 1],
        latitude = FBE_north_point[1, 2]
        
      )
    ))
    df2 <- rbind(df2,df2[1,])
  }
  
  transect_p2 <-st_polygon(list(matrix( c( df2$longitude,df2$latitude), nrow = length(df2$longitude), ncol=2))) #builds polygon with buffer points
  transect_p_poly2 <- st_sfc(transect_p2, crs = crs_use)
  transect_p_attrib2 <- data.frame(name = paste0(surveydate))
  transect_p_sf2 <- st_sf(transect_p_attrib2, geometry = transect_p_poly2)
  
  transects_b<-list()
  for(i in 1:length(x$Region_name)){
    transects_b[[i]]<-matrix(c(x$Lon_S[i], x$Lon_E[i], x$Lat_S[i], x$Lat_E[i]), ncol=2) 
  }
  transect_l  <-st_multilinestring(transects_b)
  transect_l_line <- st_sfc(transect_l, crs = crs_use)
  transect_l_attrib <- data.frame(name = paste0(surveydate))
  transect_l_sf <- st_sf(transect_l_attrib, geometry = transect_l_line)
  polygon_bbox <-st_bbox(transect_p_sf2)
  
  theme_set(theme_bw())
  p <- ggplot(data = transect_p_sf2) +
    geom_sf( color = "black", alpha = 0.3, fill = "tomato") +
    geom_sf(data = transect_l_sf, color = "red") +
    geom_sf(data= land.all, color = "black") +
    labs(title = paste0(surveydate)) +
    coord_sf(xlim = c((polygon_bbox[1]-0.1), polygon_bbox[3]+0.1), ylim = c((polygon_bbox[2]-0.1),(polygon_bbox[4]+0.1))) 
  
  p2 <- ggplot(data = transect_p_sf2) +
    geom_sf(data= land.all, color = "black", fill = "grey") +
    geom_sf(data = SB_sf, color = "black") +
    geom_point(data = transect_cell,
               aes(
                 x = Lon_M,
                 y = Lat_M,
                 size = PRC_NASC,
                 color = Boat
               )) +
    scale_color_brewer(palette = "Set1") +  # Change the palette here
    guides(alpha = "none") +
    scale_size(
      breaks = c(0, 1000, 2500, 5000, 10000, 20000, 50000),
      limits = c(0, 100000),
      range = c(1, 8)
    ) +
    geom_sf( color = "black", alpha = 0.3, fill = "tomato") +
    # geom_sf(data = transect_l_sf, color = "red") +
    labs(title = paste0(surveydate)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis text
    )+
    coord_sf(xlim = c((polygon_bbox[1]-0.1), polygon_bbox[3]+0.1), ylim = c((polygon_bbox[2]-0.1),(polygon_bbox[4]+0.1))) 
  
  a<-st_area(transect_p_sf2)
  a<-set_units(a, km^2)
  a<-as.numeric(a)
  sf::sf_use_s2(FALSE)
  survey_box_land_substract <- st_intersection(transect_p_sf2, land.all)
  a_sub<-st_area(survey_box_land_substract)
  a_sub<-set_units(a_sub, km^2)
  a_sub<-as.numeric(a_sub)
  
  huate_check<-st_as_sfc(c("POINT(-64.996 45.2453333)"),crs = crs_use) %>%
    st_sf(ID = paste0("Haute Isle"))
  inside_poly <-st_intersection(huate_check, transect_p_sf2)
  
  if ( sjmisc::is_empty(inside_poly) ) {
    a<-a
  } else{ a<-(a- 1.61) }
  if ( sjmisc::is_empty(survey_box_land_substract) ) {
    a<-a
  } else{ a<-(a- a_sub) }
  
  
  return(list(p,as.numeric(a),d, transect_p_sf2, transect_l_sf, p2))
  
}




map_area_bufferedcell_GB <- function(x, transect_cell, transectEastWest = TRUE){
  defaultW <- getOption("warn") # turn off warnings so people don't freak out 
  options(warn = -1) #same as above 
  crs_use <- st_crs(land.all) # type of projection
  
  #add survey boxes
  German_survey_box <-
    st_linestring(rbind(
      c(-66.473, 43.233),
      c(-66.473, 43.567),
      c(-66.26, 43.567),
      c(-66.26, 43.233),
      c(-66.473, 43.233)
    ))
  German_survey_box <- st_sfc(German_survey_box, crs = crs_use)
  Ger_attrib <- data.frame(name = "German Survey Box")
  Ger_sf <- st_sf(Ger_attrib, geometry = German_survey_box)
  
  Latitude  <- c(x$Lat_S, x$Lat_E) #brings in lat start and end for each transect
  Longitude  <- c(x$Lon_S, x$Lon_E) #brings in long start and end for each transect
  LatLonMat <- matrix(c( Latitude,Longitude), ncol = 2) #building polygon
  colnames(LatLonMat) <- c("latitude","longitude")
  LatLondf<-as.data.frame(LatLonMat)
  df<-sort_points(LatLondf)
  df_t<-rbind(df,df[1,]) #connecting last dot to first.
  
  #wonky polygon but used to find centroid
  transect_p <-st_polygon(list(matrix( c( df_t$longitude,df_t$latitude), nrow = length(df_t$longitude), ncol=2))) #setup to create polygon
  transect_p_poly <- st_sfc(transect_p, crs = crs_use) #adds the projection to appropriately calculate the area
  transect_p_attrib <- data.frame(name = paste0(surveydate)) #adds the attribute survey date
  transect_p_sf <- st_sf(transect_p_attrib, geometry = transect_p_poly, crs = crs_use) #creates sf polygon
  
  centy <- st_transform(transect_p_sf, crs_use) %>%  #find the center of the polygon
    st_centroid() %>% 
    # this is the crs from d, which has no EPSG code:
    st_transform(., "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
    # since you want the centroids in a second geometry col:
    st_geometry()
  
  centy<-unlist(centy) #give you the centroid of the polygon, so you can sort points E and W, or N and S
  
  
  # Arrange by angle
  if (transectEastWest) { # sorts the points to East and West
    
    West <- subset(df, longitude < centy[1])
    East <-subset(df, longitude >= centy[1])
    West<-West[order(West$latitude),]
    West$ordered <-West$latitude*West$longitude
    # West<-West[order(West$ordered, decreasing=TRUE),] # manually sort. Refer to example where we manually sort.
    
    East<-East[order(East$latitude, decreasing=TRUE),]
    East$ordered <-East$latitude*East$longitude
    East<-East[order(East$ordered, decreasing=FALSE),]
    
    
    East_point <- st_as_sf(East,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    West_point <- st_as_sf(West,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    East_point_distance <- East_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    East_point_distance<-East_point_distance$dist[!is.na( East_point_distance$dist)]
    West_point_distance <- West_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    West_point_distance<-West_point_distance$dist[!is.na( West_point_distance$dist)]
    distance_between_transects<-c(East_point_distance, West_point_distance)
    d <- (mean(as.numeric(distance_between_transects)))/2
    FBS_east <- finalBearing(as(East_point[1,], "Spatial"),  as(East_point[length(East_point$latitude),], "Spatial")) #find the buffer GIS point for SouthEast
    FBS_east_point <- destPoint(as(East_point[length(East_point$latitude),], "Spatial"),  FBS_east, d)
    FBN_east <- finalBearing( as(East_point[length(East_point$latitude),], "Spatial"), as(East_point[1,], "Spatial"))
    FBN_east_point <- destPoint(as(East_point[1,], "Spatial"),  FBN_east, d)
    
    FBN_west <- finalBearing(as(West_point[1,], "Spatial"),  as(West_point[length(West_point$latitude),], "Spatial"))
    FBN_west_point <- destPoint(as(West_point[length(West_point$latitude),], "Spatial"), FBN_west, d)
    FBS_west <- finalBearing( as(West_point[length(West_point$latitude),], "Spatial"), as(West_point[1,], "Spatial"))
    FBS_west_point <- destPoint(as(West_point[1,], "Spatial"),  FBS_west, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBS_west_point[1, 1],
        latitude = FBS_west_point[1, 2],
        ordered = FBS_west_point[1, 1] * FBS_west_point[1, 2]
      ),
      West,
      data.frame(
        longitude = FBN_west_point[1, 1],
        latitude = FBN_west_point[1, 2],
        ordered = FBN_west_point[1, 1] * FBN_west_point[1, 2]
      ),
      data.frame(
        longitude = FBN_east_point[1, 1],
        latitude = FBN_east_point[1, 2],
        ordered = FBN_east_point[1, 1] * FBN_east_point[1, 2]
      ),
      East,
      data.frame(
        longitude = FBS_east_point[1, 1],
        latitude = FBS_east_point[1, 2],
        ordered = FBS_east_point[1, 1] * FBS_east_point[1, 2]
      )
    ))
    
    df2 <- rbind(df2,df2[1,])
    
  } else {
    
    South <- subset(df, latitude < centy[2])
    North <-subset(df, latitude >= centy[2])
    South<-South[order(South$longitude),]
    North<-North[order(North$longitude, decreasing=TRUE),]
    df2<-rbind(South,North)
    df2 <- rbind(df2,df2[1,])
    
    South_point <- st_as_sf(South,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    North_point <- st_as_sf(North,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    
    South_point_distance <- South_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    South_point_distance<-South_point_distance$dist[!is.na( South_point_distance$dist)]
    North_point_distance <- North_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    North_point_distance<-North_point_distance$dist[!is.na( North_point_distance$dist)]
    
    distance_between_transects<-c(South_point_distance, North_point_distance)
    d <- (mean(as.numeric(distance_between_transects)))/2
    
    FBW_south <- finalBearing(as(South_point[1,], "Spatial"),  as(South_point[length(South_point$latitude),], "Spatial"))
    FBW_south_point <- destPoint(as(South_point[length(South_point$latitude),], "Spatial"),  FBW_south, d)
    FBE_south <- finalBearing( as(South_point[length(South_point$latitude),], "Spatial"), as(South_point[1,], "Spatial"))
    FBE_south_point <- destPoint(as(South_point[1,], "Spatial"),  FBE_south, d)
    
    FBE_north <- finalBearing(as(North_point[1,], "Spatial"),  as(North_point[length(North_point$latitude),], "Spatial"))
    FBE_north_point <- destPoint(as(North_point[length(North_point$latitude),], "Spatial"), FBE_north, d)
    FBW_north <- finalBearing( as(North_point[length(North_point$latitude),], "Spatial"), as(North_point[1,], "Spatial"))
    FBW_north_point <- destPoint(as(North_point[1,], "Spatial"),  FBW_north, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBE_south_point[1, 1],
        latitude = FBE_south_point[1, 2]
        
      ),
      South,
      data.frame(
        longitude = FBW_south_point[1, 1],
        latitude = FBW_south_point[1, 2]
        
      ),
      data.frame(
        longitude = FBW_north_point[1, 1],
        latitude = FBW_north_point[1, 2]
        
      ),
      North,
      data.frame(
        longitude = FBE_north_point[1, 1],
        latitude = FBE_north_point[1, 2]
        
      )
    ))
    df2 <- rbind(df2,df2[1,])
  }
  
  transect_p2 <-st_polygon(list(matrix( c( df2$longitude,df2$latitude), nrow = length(df2$longitude), ncol=2))) #builds polygon with buffer points
  transect_p_poly2 <- st_sfc(transect_p2, crs = crs_use)
  transect_p_attrib2 <- data.frame(name = paste0(surveydate))
  transect_p_sf2 <- st_sf(transect_p_attrib2, geometry = transect_p_poly2)
  
  transects_b<-list()
  for(i in 1:length(x$Region_name)){
    transects_b[[i]]<-matrix(c(x$Lon_S[i], x$Lon_E[i], x$Lat_S[i], x$Lat_E[i]), ncol=2) 
  }
  transect_l  <-st_multilinestring(transects_b)
  transect_l_line <- st_sfc(transect_l, crs = crs_use)
  transect_l_attrib <- data.frame(name = paste0(surveydate))
  transect_l_sf <- st_sf(transect_l_attrib, geometry = transect_l_line)
  polygon_bbox <-st_bbox(transect_p_sf2)
  
  theme_set(theme_bw())
  p <- ggplot(data = transect_p_sf2) +
    geom_sf( color = "black", alpha = 0.3, fill = "tomato") +
    geom_sf(data = transect_l_sf, color = "red") +
    geom_sf(data= land.all, color = "black") +
    labs(title = paste0(surveydate)) +
    coord_sf(xlim = c((polygon_bbox[1]-0.1), polygon_bbox[3]+0.1), ylim = c((polygon_bbox[2]-0.1),(polygon_bbox[4]+0.1))) 
  
  p2 <- ggplot(data = transect_p_sf2) +
    geom_sf(data= land.all, color = "black", fill = "grey") +
    geom_sf(data = Ger_sf, color = "black") +
    geom_point(data = transect_cell,
               aes(
                 x = Lon_M,
                 y = Lat_M,
                 size = PRC_NASC,
                 color = Boat
               )) +
    scale_color_brewer(palette = "Set1") +  # Change the palette here
    guides(alpha = "none") +
    scale_size(
      breaks = c(0, 1000, 2500, 5000, 10000, 20000, 50000),
      limits = c(0, 100000),
      range = c(1, 8)
    ) +
    geom_sf( color = "black", alpha = 0.3, fill = "tomato") +
    # geom_sf(data = transect_l_sf, color = "red") +
    labs(title = paste0(surveydate)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis text
    )+
    coord_sf(xlim = c((polygon_bbox[1]-0.1), polygon_bbox[3]+0.1), ylim = c((polygon_bbox[2]-0.1),(polygon_bbox[4]+0.1))) 
  
  a<-st_area(transect_p_sf2)
  a<-set_units(a, km^2)
  a<-as.numeric(a)
  sf::sf_use_s2(FALSE)
  survey_box_land_substract <- st_intersection(transect_p_sf2, land.all)
  a_sub<-st_area(survey_box_land_substract)
  a_sub<-set_units(a_sub, km^2)
  a_sub<-as.numeric(a_sub)
  
  huate_check<-st_as_sfc(c("POINT(-64.996 45.2453333)"),crs = crs_use) %>%
    st_sf(ID = paste0("Haute Isle"))
  inside_poly <-st_intersection(huate_check, transect_p_sf2)
  
  if ( sjmisc::is_empty(inside_poly) ) {
    a<-a
  } else{ a<-(a- 1.61) }
  if ( sjmisc::is_empty(survey_box_land_substract) ) {
    a<-a
  } else{ a<-(a- a_sub) }
  
  
  return(list(p,as.numeric(a),d, transect_p_sf2, transect_l_sf, p2))
  
}






map_area_buffered = function(x, transectEastWest = TRUE){
  defaultW <- getOption("warn") # turn off warnings so people don't freak out 
  options(warn = -1) #same as above 
  crs_use <- st_crs(land.all) # type of projection
  
  Latitude  <- c(x$Lat_S, x$Lat_E) #brings in lat start and end for each transect
  Longitude  <- c(x$Lon_S, x$Lon_E) #brings in long start and end for each transect
  LatLonMat <- matrix(c( Latitude,Longitude), ncol = 2) #building polygon
  colnames(LatLonMat) <- c("latitude","longitude")
  LatLondf<-as.data.frame(LatLonMat)
  df<-sort_points(LatLondf)
  df_t<-rbind(df,df[1,]) #connecting last dot to first.
  
  #wonky polygon but used to find centroid
  transect_p <-st_polygon(list(matrix( c( df_t$longitude,df_t$latitude), nrow = length(df_t$longitude), ncol=2))) #setup to create polygon
  transect_p_poly <- st_sfc(transect_p, crs = crs_use) #adds the projection to appropriately calculate the area
  transect_p_attrib <- data.frame(name = paste0(surveydate)) #adds the attribute survey date
  transect_p_sf <- st_sf(transect_p_attrib, geometry = transect_p_poly, crs = crs_use) #creates sf polygon
  
  centy <- st_transform(transect_p_sf, crs_use) %>%  #find the center of the polygon
    st_centroid() %>% 
    # this is the crs from d, which has no EPSG code:
    st_transform(., "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
    # since you want the centroids in a second geometry col:
    st_geometry()
  
  centy<-unlist(centy) #give you the centroid of the polygon, so you can sort points E and W, or N and S
  
  
  # Arrange by angle
  if (transectEastWest) { # sorts the points to East and West
    
    West <- subset(df, longitude < centy[1])
    East <-subset(df, longitude >= centy[1])
    West<-West[order(West$latitude),]
    West$ordered <-West$latitude*West$longitude
    # West<-West[order(West$ordered, decreasing=TRUE),] # manually sort. Refer to example where we manually sort.
    
    East<-East[order(East$latitude, decreasing=TRUE),]
    East$ordered <-East$latitude*East$longitude
    East<-East[order(East$ordered, decreasing=FALSE),]
    
    
    East_point <- st_as_sf(East,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    West_point <- st_as_sf(West,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    East_point_distance <- East_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    East_point_distance<-East_point_distance$dist[!is.na( East_point_distance$dist)]
    West_point_distance <- West_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    West_point_distance<-West_point_distance$dist[!is.na( West_point_distance$dist)]
    distance_between_transects<-c(East_point_distance, West_point_distance)
    d <- (mean(as.numeric(distance_between_transects)))/2
    FBS_east <- finalBearing(as(East_point[1,], "Spatial"),  as(East_point[length(East_point$latitude),], "Spatial")) #find the buffer GIS point for SouthEast
    FBS_east_point <- destPoint(as(East_point[length(East_point$latitude),], "Spatial"),  FBS_east, d)
    FBN_east <- finalBearing( as(East_point[length(East_point$latitude),], "Spatial"), as(East_point[1,], "Spatial"))
    FBN_east_point <- destPoint(as(East_point[1,], "Spatial"),  FBN_east, d)
    
    FBN_west <- finalBearing(as(West_point[1,], "Spatial"),  as(West_point[length(West_point$latitude),], "Spatial"))
    FBN_west_point <- destPoint(as(West_point[length(West_point$latitude),], "Spatial"), FBN_west, d)
    FBS_west <- finalBearing( as(West_point[length(West_point$latitude),], "Spatial"), as(West_point[1,], "Spatial"))
    FBS_west_point <- destPoint(as(West_point[1,], "Spatial"),  FBS_west, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBS_west_point[1, 1],
        latitude = FBS_west_point[1, 2],
        ordered = FBS_west_point[1, 1] * FBS_west_point[1, 2]
      ),
      West,
      data.frame(
        longitude = FBN_west_point[1, 1],
        latitude = FBN_west_point[1, 2],
        ordered = FBN_west_point[1, 1] * FBN_west_point[1, 2]
      ),
      data.frame(
        longitude = FBN_east_point[1, 1],
        latitude = FBN_east_point[1, 2],
        ordered = FBN_east_point[1, 1] * FBN_east_point[1, 2]
      ),
      East,
      data.frame(
        longitude = FBS_east_point[1, 1],
        latitude = FBS_east_point[1, 2],
        ordered = FBS_east_point[1, 1] * FBS_east_point[1, 2]
      )
    ))
    
    df2 <- rbind(df2,df2[1,])
    
  } else {
    
    South <- subset(df, latitude < centy[2])
    North <-subset(df, latitude >= centy[2])
    South<-South[order(South$longitude),]
    North<-North[order(North$longitude, decreasing=TRUE),]
    df2<-rbind(South,North)
    df2 <- rbind(df2,df2[1,])
    
    South_point <- st_as_sf(South,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    North_point <- st_as_sf(North,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    
    South_point_distance <- South_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    South_point_distance<-South_point_distance$dist[!is.na( South_point_distance$dist)]
    North_point_distance <- North_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    North_point_distance<-North_point_distance$dist[!is.na( North_point_distance$dist)]
    
    distance_between_transects<-c(South_point_distance, North_point_distance)
    d <- (mean(as.numeric(distance_between_transects)))/2
    
    FBW_south <- finalBearing(as(South_point[1,], "Spatial"),  as(South_point[length(South_point$latitude),], "Spatial"))
    FBW_south_point <- destPoint(as(South_point[length(South_point$latitude),], "Spatial"),  FBW_south, d)
    FBE_south <- finalBearing( as(South_point[length(South_point$latitude),], "Spatial"), as(South_point[1,], "Spatial"))
    FBE_south_point <- destPoint(as(South_point[1,], "Spatial"),  FBE_south, d)
    
    FBE_north <- finalBearing(as(North_point[1,], "Spatial"),  as(North_point[length(North_point$latitude),], "Spatial"))
    FBE_north_point <- destPoint(as(North_point[length(North_point$latitude),], "Spatial"), FBE_north, d)
    FBW_north <- finalBearing( as(North_point[length(North_point$latitude),], "Spatial"), as(North_point[1,], "Spatial"))
    FBW_north_point <- destPoint(as(North_point[1,], "Spatial"),  FBW_north, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBE_south_point[1, 1],
        latitude = FBE_south_point[1, 2]
        
      ),
      South,
      data.frame(
        longitude = FBW_south_point[1, 1],
        latitude = FBW_south_point[1, 2]
        
      ),
      data.frame(
        longitude = FBW_north_point[1, 1],
        latitude = FBW_north_point[1, 2]
        
      ),
      North,
      data.frame(
        longitude = FBE_north_point[1, 1],
        latitude = FBE_north_point[1, 2]
        
      )
    ))
    df2 <- rbind(df2,df2[1,])
  }
  
  transect_p2 <-st_polygon(list(matrix( c( df2$longitude,df2$latitude), nrow = length(df2$longitude), ncol=2))) #builds polygon with buffer points
  transect_p_poly2 <- st_sfc(transect_p2, crs = crs_use)
  transect_p_attrib2 <- data.frame(name = paste0(surveydate))
  transect_p_sf2 <- st_sf(transect_p_attrib2, geometry = transect_p_poly2)
  
  transects_b<-list()
  for(i in 1:length(x$Region_name)){
    transects_b[[i]]<-matrix(c(x$Lon_S[i], x$Lon_E[i], x$Lat_S[i], x$Lat_E[i]), ncol=2) 
  }
  transect_l  <-st_multilinestring(transects_b)
  transect_l_line <- st_sfc(transect_l, crs = crs_use)
  transect_l_attrib <- data.frame(name = paste0(surveydate))
  transect_l_sf <- st_sf(transect_l_attrib, geometry = transect_l_line)
  polygon_bbox <-st_bbox(transect_p_sf2)
  
  theme_set(theme_bw())
  p <- ggplot(data = transect_p_sf2) +
    geom_sf( color = "black", alpha = 0.3, fill = "tomato") +
    geom_sf(data = transect_l_sf, color = "red") +
    geom_sf(data= land.all, color = "black") +
    labs(title = paste0(surveydate)) +
    coord_sf(xlim = c((polygon_bbox[1]-0.1), polygon_bbox[3]+0.1), ylim = c((polygon_bbox[2]-0.1),(polygon_bbox[4]+0.1))) 
  
  a<-st_area(transect_p_sf2)
  a<-set_units(a, km^2)
  a<-as.numeric(a)
  sf::sf_use_s2(FALSE)
  survey_box_land_substract <- st_intersection(transect_p_sf2, land.all)
  a_sub<-st_area(survey_box_land_substract)
  a_sub<-set_units(a_sub, km^2)
  a_sub<-as.numeric(a_sub)
  
  huate_check<-st_as_sfc(c("POINT(-64.996 45.2453333)"),crs = crs_use) %>%
    st_sf(ID = paste0("Haute Isle"))
  inside_poly <-st_intersection(huate_check, transect_p_sf2)
  
  if ( sjmisc::is_empty(inside_poly) ) {
    a<-a
  } else{ a<-(a- 1.61) }
  if ( sjmisc::is_empty(survey_box_land_substract) ) {
    a<-a
  } else{ a<-(a- a_sub) }
  

  return(list(p,as.numeric(a),d, transect_p_sf2, transect_l_sf))
  
}


map_area_buffered_gill = function(x, transectEastWest = TRUE){
  defaultW <- getOption("warn") # turn off warnings so people don't freak out 
  options(warn = -1) #same as above 
  crs_use <- st_crs(land.all) # type of projection
  
  Latitude  <- c(x$Lat_S, x$Lat_E) #brings in lat start and end for each transect
  Longitude  <- c(x$Lon_S, x$Lon_E) #brings in long start and end for each transect
  LatLonMat <- matrix(c( Latitude,Longitude), ncol = 2) #building polygon
  colnames(LatLonMat) <- c("latitude","longitude")
  LatLondf<-as.data.frame(LatLonMat)
  df<-sort_points(LatLondf)
  df_t<-rbind(df,df[1,]) #connecting last dot to first.
  
  #wonky polygon but used to find centroid
  transect_p <-st_polygon(list(matrix( c( df_t$longitude,df_t$latitude), nrow = length(df_t$longitude), ncol=2))) #setup to create polygon
  transect_p_poly <- st_sfc(transect_p, crs = crs_use) #adds the projection to appropriately calculate the area
  transect_p_attrib <- data.frame(name = paste0(surveydate)) #adds the attribute survey date
  transect_p_sf <- st_sf(transect_p_attrib, geometry = transect_p_poly, crs = crs_use) #creates sf polygon
  
  centy <- st_transform(transect_p_sf, crs_use) %>%  #find the center of the polygon
    st_centroid() %>% 
    # this is the crs from d, which has no EPSG code:
    st_transform(., "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
    # since you want the centroids in a second geometry col:
    st_geometry()
  
  centy<-unlist(centy) #give you the centroid of the polygon, so you can sort points E and W, or N and S
  
  
  # Arrange by angle
  if (transectEastWest) { # sorts the points to East and West
    
    West <- subset(df, longitude < centy[1])
    East <-subset(df, longitude >= centy[1])
    West<-West[order(West$latitude),]
    West$ordered <-West$latitude*West$longitude
    # West<-West[order(West$ordered, decreasing=TRUE),] # manually sort. Refer to example where we manually sort.
    
    East<-East[order(East$latitude, decreasing=TRUE),]
    East$ordered <-East$latitude*East$longitude
    East<-East[order(East$ordered, decreasing=FALSE),]
    
    
    East_point <- st_as_sf(East,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    West_point <- st_as_sf(West,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    East_point_distance <- East_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    East_point_distance<-East_point_distance$dist[!is.na( East_point_distance$dist)]
    West_point_distance <- West_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    West_point_distance<-West_point_distance$dist[!is.na( West_point_distance$dist)]
    distance_between_transects<-c(East_point_distance, West_point_distance)
    d <- (mean(as.numeric(distance_between_transects)))/2
    FBS_east <- finalBearing(as(East_point[1,], "Spatial"),  as(East_point[length(East_point$latitude),], "Spatial")) #find the buffer GIS point for SouthEast
    FBS_east_point <- destPoint(as(East_point[length(East_point$latitude),], "Spatial"),  FBS_east, d)
    FBN_east <- finalBearing( as(East_point[length(East_point$latitude),], "Spatial"), as(East_point[1,], "Spatial"))
    FBN_east_point <- destPoint(as(East_point[1,], "Spatial"),  FBN_east, d)
    
    FBN_west <- finalBearing(as(West_point[1,], "Spatial"),  as(West_point[length(West_point$latitude),], "Spatial"))
    FBN_west_point <- destPoint(as(West_point[length(West_point$latitude),], "Spatial"), FBN_west, d)
    FBS_west <- finalBearing( as(West_point[length(West_point$latitude),], "Spatial"), as(West_point[1,], "Spatial"))
    FBS_west_point <- destPoint(as(West_point[1,], "Spatial"),  FBS_west, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBS_west_point[1, 1],
        latitude = FBS_west_point[1, 2],
        ordered = FBS_west_point[1, 1] * FBS_west_point[1, 2]
      ),
      West,
      data.frame(
        longitude = FBN_west_point[1, 1],
        latitude = FBN_west_point[1, 2],
        ordered = FBN_west_point[1, 1] * FBN_west_point[1, 2]
      ),
      data.frame(
        longitude = FBN_east_point[1, 1],
        latitude = FBN_east_point[1, 2],
        ordered = FBN_east_point[1, 1] * FBN_east_point[1, 2]
      ),
      East,
      data.frame(
        longitude = FBS_east_point[1, 1],
        latitude = FBS_east_point[1, 2],
        ordered = FBS_east_point[1, 1] * FBS_east_point[1, 2]
      )
    ))
    
    df2 <- rbind(df2,df2[1,])
    
  } else {
    
    South <- subset(df, latitude < centy[2])
    North <-subset(df, latitude >= centy[2])
    South<-South[order(South$longitude),]
    North<-North[order(North$longitude, decreasing=TRUE),]
    df2<-rbind(South,North)
    df2 <- rbind(df2,df2[1,])
    
    South_point <- st_as_sf(South,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    North_point <- st_as_sf(North,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    
    South_point_distance <- South_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    South_point_distance<-South_point_distance$dist[!is.na( South_point_distance$dist)]
    North_point_distance <- North_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    North_point_distance<-North_point_distance$dist[!is.na( North_point_distance$dist)]
    
    distance_between_transects<-c(South_point_distance, North_point_distance)
    d <- (mean(as.numeric(distance_between_transects)))/2
    
    FBW_south <- finalBearing(as(South_point[1,], "Spatial"),  as(South_point[length(South_point$latitude),], "Spatial"))
    FBW_south_point <- destPoint(as(South_point[length(South_point$latitude),], "Spatial"),  FBW_south, d)
    FBE_south <- finalBearing( as(South_point[length(South_point$latitude),], "Spatial"), as(South_point[1,], "Spatial"))
    FBE_south_point <- destPoint(as(South_point[1,], "Spatial"),  FBE_south, d)
    
    FBE_north <- finalBearing(as(North_point[1,], "Spatial"),  as(North_point[length(North_point$latitude),], "Spatial"))
    FBE_north_point <- destPoint(as(North_point[length(North_point$latitude),], "Spatial"), FBE_north, d)
    FBW_north <- finalBearing( as(North_point[length(North_point$latitude),], "Spatial"), as(North_point[1,], "Spatial"))
    FBW_north_point <- destPoint(as(North_point[1,], "Spatial"),  FBW_north, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBE_south_point[1, 1],
        latitude = FBE_south_point[1, 2]
        
      ),
      South,
      data.frame(
        longitude = FBW_south_point[1, 1],
        latitude = FBW_south_point[1, 2]
        
      ),
      data.frame(
        longitude = FBW_north_point[1, 1],
        latitude = FBW_north_point[1, 2]
        
      ),
      North,
      data.frame(
        longitude = FBE_north_point[1, 1],
        latitude = FBE_north_point[1, 2]
        
      )
    ))
    df2 <- rbind(df2,df2[1,])
  }
  
  transect_p2 <-st_polygon(list(matrix( c( df2$longitude,df2$latitude), nrow = length(df2$longitude), ncol=2))) #builds polygon with buffer points
  transect_p_poly2 <- st_sfc(transect_p2, crs = crs_use)
  transect_p_attrib2 <- data.frame(name = paste0(surveydate))
  transect_p_sf2 <- st_sf(transect_p_attrib2, geometry = transect_p_poly2)
  
  transects_b<-list()
  for(i in 1:length(x$Region_name)){
    transects_b[[i]]<-matrix(c(x$Lon_S[i], x$Lon_E[i], x$Lat_S[i], x$Lat_E[i]), ncol=2) 
  }
  transect_l  <-st_multilinestring(transects_b)
  transect_l_line <- st_sfc(transect_l, crs = crs_use)
  transect_l_attrib <- data.frame(name = paste0(surveydate))
  transect_l_sf <- st_sf(transect_l_attrib, geometry = transect_l_line)
  polygon_bbox <-st_bbox(transect_p_sf2)
  
  theme_set(theme_bw())
  p <- ggplot(data = transect_p_sf2) +
    geom_sf( color = "black", alpha = 0.3, fill = "tomato") +
    geom_sf(data = transect_l_sf, color = "red") +
    geom_sf(data= land.all, color = "black") +
    labs(title = paste0(surveydate)) +
    coord_sf(xlim = c((polygon_bbox[1]-0.02), polygon_bbox[3]+0.02), ylim = c((polygon_bbox[2]-0.02),(polygon_bbox[4]+0.02))) 
  
  a<-st_area(transect_p_sf2)
  a<-set_units(a, km^2)
  a<-as.numeric(a)
  sf::sf_use_s2(FALSE)
  survey_box_land_substract <- st_intersection(transect_p_sf2, land.all)
  a_sub<-st_area(survey_box_land_substract)
  a_sub<-set_units(a_sub, km^2)
  a_sub<-as.numeric(a_sub)
  
  huate_check<-st_as_sfc(c("POINT(-64.996 45.2453333)"),crs = crs_use) %>%
    st_sf(ID = paste0("Haute Isle"))
  inside_poly <-st_intersection(huate_check, transect_p_sf2)
  
  if ( sjmisc::is_empty(inside_poly) ) {
    a<-a
  } else{ a<-(a- 1.61) }
  if ( sjmisc::is_empty(survey_box_land_substract) ) {
    a<-a
  } else{ a<-(a- a_sub) }
  

  return(list(p,as.numeric(a),d, transect_p_sf2, transect_l_sf))
  
}

map_area_buffered_standard = function(x, transectEastWest = TRUE, d = 1250){
  defaultW <- getOption("warn") # turn off warnings so people don't freak out 
  options(warn = -1) #same as above 
  crs_use <- st_crs(land.all) # type of projection
  
  Latitude  <- c(x$Lat_S, x$Lat_E) #brings in lat start and end for each transect
  Longitude  <- c(x$Lon_S, x$Lon_E) #brings in long start and end for each transect
  LatLonMat <- matrix(c( Latitude,Longitude), ncol = 2) #building polygon
  colnames(LatLonMat) <- c("latitude","longitude")
  LatLondf<-as.data.frame(LatLonMat)
  df<-sort_points(LatLondf)
  df_t<-rbind(df,df[1,]) #connecting last dot to first.
  
  #wonky polygon but used to find centroid
  transect_p <-st_polygon(list(matrix( c( df_t$longitude,df_t$latitude), nrow = length(df_t$longitude), ncol=2))) #setup to create polygon
  transect_p_poly <- st_sfc(transect_p, crs = crs_use) #adds the projection to appropriately calculate the area
  transect_p_attrib <- data.frame(name = paste0(surveydate)) #adds the attribute survey date
  transect_p_sf <- st_sf(transect_p_attrib, geometry = transect_p_poly, crs = crs_use) #creates sf polygon
  
  centy <- st_transform(transect_p_sf, crs_use) %>%  #find the center of the polygon
    st_centroid() %>% 
    # this is the crs from d, which has no EPSG code:
    st_transform(., "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
    # since you want the centroids in a second geometry col:
    st_geometry()
  
  centy<-unlist(centy) #give you the centroid of the polygon, so you can sort points E and W, or N and S
  
  
  # Arrange by angle
  if (transectEastWest) { # sorts the points to East and West
    
    West <- subset(df, longitude < centy[1])
    East <-subset(df, longitude >= centy[1])
    West<-West[order(West$latitude),]
    West$ordered <-West$latitude*West$longitude
    # West<-West[order(West$ordered, decreasing=TRUE),] # manually sort. Refer to example where we manually sort.
    
    East<-East[order(East$latitude, decreasing=TRUE),]
    East$ordered <-East$latitude*East$longitude
    East<-East[order(East$ordered, decreasing=FALSE),]
    
    
    East_point <- st_as_sf(East,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    West_point <- st_as_sf(West,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    
   
    FBS_east <- finalBearing(as(East_point[1,], "Spatial"),  as(East_point[length(East_point$latitude),], "Spatial")) #find the buffer GIS point for SouthEast
    FBS_east_point <- destPoint(as(East_point[length(East_point$latitude),], "Spatial"),  FBS_east, d)
    FBN_east <- finalBearing( as(East_point[length(East_point$latitude),], "Spatial"), as(East_point[1,], "Spatial"))
    FBN_east_point <- destPoint(as(East_point[1,], "Spatial"),  FBN_east, d)
    
    FBN_west <- finalBearing(as(West_point[1,], "Spatial"),  as(West_point[length(West_point$latitude),], "Spatial"))
    FBN_west_point <- destPoint(as(West_point[length(West_point$latitude),], "Spatial"), FBN_west, d)
    FBS_west <- finalBearing( as(West_point[length(West_point$latitude),], "Spatial"), as(West_point[1,], "Spatial"))
    FBS_west_point <- destPoint(as(West_point[1,], "Spatial"),  FBS_west, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBS_west_point[1, 1],
        latitude = FBS_west_point[1, 2],
        ordered = FBS_west_point[1, 1] * FBS_west_point[1, 2]
      ),
      West,
      data.frame(
        longitude = FBN_west_point[1, 1],
        latitude = FBN_west_point[1, 2],
        ordered = FBN_west_point[1, 1] * FBN_west_point[1, 2]
      ),
      data.frame(
        longitude = FBN_east_point[1, 1],
        latitude = FBN_east_point[1, 2],
        ordered = FBN_east_point[1, 1] * FBN_east_point[1, 2]
      ),
      East,
      data.frame(
        longitude = FBS_east_point[1, 1],
        latitude = FBS_east_point[1, 2],
        ordered = FBS_east_point[1, 1] * FBS_east_point[1, 2]
      )
    ))
    
    df2 <- rbind(df2,df2[1,])
    
  } else {
    
    South <- subset(df, latitude < centy[2])
    North <-subset(df, latitude >= centy[2])
    South<-South[order(South$longitude),]
    North<-North[order(North$longitude, decreasing=TRUE),]
    df2<-rbind(South,North)
    df2 <- rbind(df2,df2[1,])
    
    South_point <- st_as_sf(South,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    North_point <- st_as_sf(North,coords = c("longitude","latitude"),remove = F, crs = crs_use)
    
    South_point_distance <- South_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    South_point_distance<-South_point_distance$dist[!is.na( South_point_distance$dist)]
    North_point_distance <- North_point %>% dplyr::mutate(
      lead = geometry[row_number() + 1],
      dist = st_distance(geometry, lead, by_element = T),
    )
    North_point_distance<-North_point_distance$dist[!is.na( North_point_distance$dist)]
    
    distance_between_transects<-c(South_point_distance, North_point_distance)
    d <- (mean(as.numeric(distance_between_transects)))/2
    
    FBW_south <- finalBearing(as(South_point[1,], "Spatial"),  as(South_point[length(South_point$latitude),], "Spatial"))
    FBW_south_point <- destPoint(as(South_point[length(South_point$latitude),], "Spatial"),  FBW_south, d)
    FBE_south <- finalBearing( as(South_point[length(South_point$latitude),], "Spatial"), as(South_point[1,], "Spatial"))
    FBE_south_point <- destPoint(as(South_point[1,], "Spatial"),  FBE_south, d)
    
    FBE_north <- finalBearing(as(North_point[1,], "Spatial"),  as(North_point[length(North_point$latitude),], "Spatial"))
    FBE_north_point <- destPoint(as(North_point[length(North_point$latitude),], "Spatial"), FBE_north, d)
    FBW_north <- finalBearing( as(North_point[length(North_point$latitude),], "Spatial"), as(North_point[1,], "Spatial"))
    FBW_north_point <- destPoint(as(North_point[1,], "Spatial"),  FBW_north, d)
    
    df2<- do.call("rbind", list(
      data.frame(
        longitude = FBE_south_point[1, 1],
        latitude = FBE_south_point[1, 2]
        
      ),
      South,
      data.frame(
        longitude = FBW_south_point[1, 1],
        latitude = FBW_south_point[1, 2]
        
      ),
      data.frame(
        longitude = FBW_north_point[1, 1],
        latitude = FBW_north_point[1, 2]
        
      ),
      North,
      data.frame(
        longitude = FBE_north_point[1, 1],
        latitude = FBE_north_point[1, 2]
        
      )
    ))
    df2 <- rbind(df2,df2[1,])
  }
  
  transect_p2 <-st_polygon(list(matrix( c( df2$longitude,df2$latitude), nrow = length(df2$longitude), ncol=2))) #builds polygon with buffer points
  transect_p_poly2 <- st_sfc(transect_p2, crs = crs_use)
  transect_p_attrib2 <- data.frame(name = paste0(surveydate))
  transect_p_sf2 <- st_sf(transect_p_attrib2, geometry = transect_p_poly2)
  
  transects_b<-list()
  for(i in 1:length(x$Region_name)){
    transects_b[[i]]<-matrix(c(x$Lon_S[i], x$Lon_E[i], x$Lat_S[i], x$Lat_E[i]), ncol=2) 
  }
  transect_l  <-st_multilinestring(transects_b)
  transect_l_line <- st_sfc(transect_l, crs = crs_use)
  transect_l_attrib <- data.frame(name = paste0(surveydate))
  transect_l_sf <- st_sf(transect_l_attrib, geometry = transect_l_line)
  polygon_bbox <-st_bbox(transect_p_sf2)
  
  theme_set(theme_bw())
  p <- ggplot(data = transect_p_sf2) +
    geom_sf( color = "black", alpha = 0.3, fill = "tomato") +
    geom_sf(data = transect_l_sf, color = "red") +
    geom_sf(data= land.all, color = "black") +
    labs(title = paste0(surveydate)) +
    coord_sf(xlim = c((polygon_bbox[1]-0.1), polygon_bbox[3]+0.1), ylim = c((polygon_bbox[2]-0.1),(polygon_bbox[4]+0.1))) 
  
  a<-st_area(transect_p_sf2)
  a<-set_units(a, km^2)
  a<-as.numeric(a)
  survey_box_land_substract <- st_intersection(transect_p_sf2, land.all)
  sf::sf_use_s2(FALSE)
  a_sub<-st_area(survey_box_land_substract)
  a_sub<-set_units(a_sub, km^2)
  a_sub<-as.numeric(a_sub)
  
  huate_check<-st_as_sfc(c("POINT(-64.996 45.2453333)"),crs = crs_use) %>%
    st_sf(ID = paste0("Haute Isle"))
  inside_poly <-st_intersection(huate_check, transect_p_sf2)
  
  if ( sjmisc::is_empty(inside_poly) ) {
    a<-a
  } else{ a<-(a- 1.61) }
  if ( sjmisc::is_empty(survey_box_land_substract) ) {
    a<-a
  } else{ a<-(a- a_sub) }
  

  return(list(p,as.numeric(a),d, transect_p_sf2, transect_l_sf))
  
}




fix_distance_topline<-function(x, topline_broken = TRUE){
  if (topline_broken){
    land.all <-
      ne_countries(scale = "large",
                   returnclass = "sf",
                   continent = "North America")
    crs_use <- st_crs(land.all)
    topline <-x %>% filter( Lat_S == max(Lat_S) | Lat_E == max(Lat_S)) #determine which line is topline
    PT1 <- st_sfc(st_point(c(topline$Lon_S, topline$Lat_S)), crs = crs_use)
    PT2 <- st_sfc(st_point(c(topline$Lon_E, topline$Lat_E)), crs = crs_use)
    x[x$Region_name == topline$Region_name,]$Dist_S <- 0
    x[x$Region_name == topline$Region_name,]$Dist_E <- as.numeric(st_distance(PT1,PT2))
  }
  return(x)
}


fix_distance_customline<-function(x, topline_broken = TRUE, transectName){ #Where transectName needs to be as.character
  if (topline_broken){
    land.all <-
      ne_countries(scale = "large",
                   returnclass = "sf",
                   continent = "North America")
    crs_use <- st_crs(land.all)
    custom_line <-x %>% filter( Region_name == transectName) #determine which line is custom_line
    PT1 <- st_sfc(st_point(c(custom_line$Lon_S, custom_line$Lat_S)), crs = crs_use)
    PT2 <- st_sfc(st_point(c(custom_line$Lon_E, custom_line$Lat_E)), crs = crs_use)
    x[x$Region_name == custom_line$Region_name,]$Dist_S <- 0
    x[x$Region_name == custom_line$Region_name,]$Dist_E <- as.numeric(st_distance(PT1,PT2))
  }
  return(x)
}



map_area = function(x, transectEastWest = TRUE){ 
  land.all <-
    ne_countries(scale = "large",
                 returnclass = "sf",
                 continent = "North America")
  crs_use <- st_crs(land.all)
  
  Latitude  <- c(x$Lat_S, x$Lat_E)
  Longitude  <- c(x$Lon_S, x$Lon_E)
  LatLonMat <- matrix(c( Latitude,Longitude), ncol = 2)
  colnames(LatLonMat) <- c("latitude","longitude")
  LatLondf<-as.data.frame(LatLonMat)
  df<-sort_points(LatLondf)
  df_t<-rbind(df,df[1,])
  
  #wonky polygon but used to find centroid
  transect_p <-st_polygon(list(matrix( c( df_t$longitude,df_t$latitude), nrow = length(df_t$longitude), ncol=2)))
  transect_p_poly <- st_sfc(transect_p, crs = crs_use)
  transect_p_attrib <- data.frame(name = paste0(surveydate))
  transect_p_sf <- st_sf(transect_p_attrib, geometry = transect_p_poly)
  
  centy <- st_transform(transect_p_sf, crs_use) %>% 
    st_centroid() %>% 
    # this is the crs from d, which has no EPSG code:
    st_transform(., "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
    # since you want the centroids in a second geometry col:
    st_geometry()
  
  centy<-unlist(centy)
  
  # Arrange by angle
  if (transectEastWest) {
    
    West <- subset(df, longitude < centy[1])
    East <-subset(df, longitude >= centy[1])
    West<-West[order(West$latitude),]
    West$ordered <-West$latitude*West$longitude
    # West<-West[order(West$ordered, decreasing=TRUE),]
    
    East<-East[order(East$latitude, decreasing=TRUE),]
    East$ordered <-East$latitude*East$longitude
    East<-East[order(East$ordered, decreasing=FALSE),]
    
    
    df2<-rbind(West,East)
    df2 <- rbind(df2,df2[1,])
    
    
  } else {
    
    South <- subset(df, latitude < centy[2])
    North <-subset(df, latitude >= centy[2])
    South<-South[order(South$longitude),]
    North<-North[order(North$longitude, decreasing=TRUE),]
    df2<-rbind(South,North)
    df2 <- rbind(df2,df2[1,])
  }
  
  transect_p2 <-st_polygon(list(matrix( c( df2$longitude,df2$latitude), nrow = length(df2$longitude), ncol=2)))
  transect_p_poly2 <- st_sfc(transect_p2, crs = crs_use)
  transect_p_attrib2 <- data.frame(name = paste0(surveydate))
  transect_p_sf2 <- st_sf(transect_p_attrib2, geometry = transect_p_poly2)
  
  transects_b<-list()
  for(i in 1:length(x$Region_name)){
    transects_b[[i]]<-matrix(c(x$Lon_S[i], x$Lon_E[i], x$Lat_S[i], x$Lat_E[i]), ncol=2) 
  }
  transect_l  <-st_multilinestring(transects_b)
  transect_l_line <- st_sfc(transect_l, crs = crs_use)
  transect_l_attrib <- data.frame(name = paste0(surveydate))
  transect_l_sf <- st_sf(transect_l_attrib, geometry = transect_l_line)
  
  theme_set(theme_bw())
  p <- ggplot(data = transect_p_sf2) +
    geom_sf( color = "black", alpha = 0.3, fill = "tomato") +
    labs(title = paste0(surveydate)) +
    geom_sf(data = transect_l_sf, color = "red")
   
  p
  sf::sf_use_s2(FALSE)
  a<-st_area(transect_p_sf2)
  a<-set_units(a, km^2)
  a<-as.numeric(a)
  
  
  huate_check<-st_as_sfc(c("POINT(-64.996 45.2453333)"),crs = crs_use) %>%
    st_sf(ID = paste0("Haute Isle"))
  inside_poly <-st_intersection(huate_check, transect_p_sf2)
  
  if ( sjmisc::is_empty(inside_poly) ) {
    a<-a
  } else{ a<-(a- 1.61) }
  
  
  return(list(p,a))
}


Org_point = function(x){
  Latitude  <- c(x$Lat_S, x$Lat_E)
  Longitude  <- c(x$Lon_S, x$Lon_E)
  LatLonMat <- matrix(c( Latitude,Longitude), ncol = 2)
  colnames(LatLonMat) <- c("latitude","longitude")
  LatLondf<-as.data.frame(LatLonMat)
  return(LatLondf)
}


#calculate area of survey
area_calc = function(x) {
  Longitude  <- c(x$Lon_S, x$Lon_E)
  Latitude  <- c(x$Lat_S, x$Lat_E)
  LatLonMat <- matrix(c(Longitude, Latitude), ncol = 2)
  survey_coord <- SpatialPoints(LatLonMat)
  crs.geo <- CRS("+proj=utm +zone=20 +datum=WGS84")
  
  proj4string(survey_coord) <- crs.geo
  is.projected(survey_coord)
  summary(survey_coord)
  
  survey_area <- mcp(survey_coord, percent = 100)
  area <-
    survey_area$area * 100000000 #Converts to km squared. originally in Hectares, but not sure why it's million too small.
  return(area)
}



biomassCalc = function(x, area, TS38, TS50, TS75, TS120) {
  right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
  left = function (string,char){substr(string,1,char)}
  #x = transects_German
  x$areaKm = as.numeric(area)
   
  # Analysis
  x$distance = (x$Dist_E - x$Dist_S) / 1000
  x$sum_trans = sum(x$distance)
  x$Actual_Weighting = (x$distance) / (x$sum_trans)
  x$calc_actual_mean_sa = 10 ^ (x$Area_Backscatter_Strength / 10) * x$Actual_Weighting
  x$trans_meanSa = 10 * log10(x$calc_actual_mean_sa)
  x$TS <- as.numeric(ifelse(grepl(38, x$Frequency), TS38, ifelse(
    grepl(50, x$Frequency), TS50, ifelse(grepl(75, x$Frequency), TS75, TS120)
  )))
  
  x$meanSa = 10 * log10(sum(x$calc_actual_mean_sa))
  x$Date_S = min(x$Date_S)
  x$Vessel = left(x$Region_name, 2)
  x$Transect_No =  right(left(x$Region_name, 6),3)
  x$TransCount = length(x$calc_actual_mean_sa)
  x$biomass_density = 10 ^ ((x$Area_Backscatter_Strength - (x$TS)) / 10) # same as Jenna's Transects.R script calculation
  x$biomass_density[x$biomass_density < 0.0000001] <- 0
  
  x$density = (x$biomass_density) * (x$distance)
  x$weighted_mean_biomass_calc = (x$biomass_density) * (x$Actual_Weighting)
  x$trans_biomass = (x$weighted_mean_biomass_calc) * x$areaKm * 1000
  x$total_biomass = sum(x$trans_biomass)
  x$mean_biomass_density = mean(x$weighted_mean_biomass_calc)
  x$Variance = var(x$weighted_mean_biomass_calc)
  x$BiomassVariance = (x$areaKm * 1000)^2 *  var(x$weighted_mean_biomass_calc)
  x$standard_error_tonnes = sqrt(x$BiomassVariance) 
  x$standard_error_perc = x$standard_error_tonnes / x$total_biomass * 100
  x$SD = sqrt(x$BiomassVariance)
  x$cv =  (x$SD / x$total_biomass )*100
  x$ci =  1.96 * x$SD
    return(x)
}





abund_by_len_Calc = function(x, area, TS38_by_number, TS50_by_number, TS75_by_number, TS120_by_number, perc_no_meas) {
  x$areaKm = as.numeric(area)
   
  # Analysis
  x$distance = (x$Dist_E - x$Dist_S) / 1000
  x$sum_trans = sum(x$distance)
  x$Actual_Weighting = (x$distance) / (x$sum_trans)
  x$calc_actual_mean_sa = 10 ^ (x$Area_Backscatter_Strength / 10) * x$Actual_Weighting
  x$trans_meanSa = 10 * log10(x$calc_actual_mean_sa)  
  Prop_Sa_length<- list()
  Sa_prop_by_TS_no_meas<- list()
  Cal_dens_in_nos<- list()
  Nos_by_area <- list()
  for(i in 1:length(x$Region_name)){
    Prop_Sa_length[[i]] <- x$calc_actual_mean_sa[i]*perc_no_meas #convert TS weighted perc to prop of SA by length
    Sa_prop_by_TS_no_meas[[i]] <-10*log10(Prop_Sa_length[[i]]) #convert TS weighted perc to prop of SA by length
    Cal_dens_in_nos[[i]] <- 10 ^ (( Sa_prop_by_TS_no_meas[[i]] - if (grepl(38, x$Frequency[i])) { #Density by Frequency by transect
      TS38_by_number
    } else if (grepl(50, x$Frequency[i])) {
      TS50_by_number   # or NULL / NA / whatever you want
    } else if (grepl(75, x$Frequency[i])) {
      TS75_by_number
    } else {
      TS120_by_number
    } ) /10)
    Nos_by_area[[i]]<-Cal_dens_in_nos[[i]]*x$areaKm[i]
  }
  
  Abundundance_by_length_bin <- NULL
  for(j in 1:length(Nos_by_area[[1]])){
    Abundundance_by_length_bin[j]<- sum(sapply(Nos_by_area, function(df) df[[j]][1]))
  }
  return(list(x=x, Abundundance_by_length_bin=Abundundance_by_length_bin))
}



abund_bycell_len_Calc = function(x, area, TS38_by_number, TS50_by_number, TS75_by_number, TS120_by_number, perc_no_meas) {
  #x = transects_German
  x$areaKm = as.numeric(area)
  x$PRC_ABC[x$PRC_ABC < 1e-8] <- 0
  # Analysis
  x$distance = (x$Dist_E - x$Dist_S) / 1000
  x$sum_trans = sum(x$distance)
  x$Actual_Weighting = (x$distance) / (x$sum_trans)
  x$calc_actual_mean_sa = x$PRC_ABC  * x$Actual_Weighting
  x$trans_meanSa = 10 * log10(x$calc_actual_mean_sa)  
  Prop_Sa_length<- list()
  Sa_prop_by_TS_no_meas<- list()
  Cal_dens_in_nos<- list()
  Nos_by_area <- list()
  for(i in 1:length(x$Region_name)){
    Prop_Sa_length[[i]] <- x$calc_actual_mean_sa[i]*perc_no_meas #convert TS weighted perc to prop of SA by length
    Sa_prop_by_TS_no_meas[[i]] <-10*log10(Prop_Sa_length[[i]]) #convert TS weighted perc to prop of SA by length
    Cal_dens_in_nos[[i]] <- 10 ^ (( Sa_prop_by_TS_no_meas[[i]] - if (grepl(38, x$Frequency[i])) { #Density by Frequency by transect
      TS38_by_number
    } else if (grepl(50, x$Frequency[i])) {
      TS50_by_number   # or NULL / NA / whatever you want
    } else if (grepl(75, x$Frequency[i])) {
      TS75_by_number
    } else {
      TS120_by_number
    } ) /10)
    Nos_by_area[[i]]<-Cal_dens_in_nos[[i]]*x$areaKm[i]
  }
  
  Abundundance_by_length_bin <- NULL
  for(j in 1:length(Nos_by_area[[1]])){
    Abundundance_by_length_bin[j]<- sum(sapply(Nos_by_area, function(df) df[[j]][1]))
  }
  return(list(x=x, Abundundance_by_length_bin=Abundundance_by_length_bin))
}






abundCalc = function(x, area, TS38, TS50, TS75, TS120) {
  right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
  left = function (string,char){substr(string,1,char)}
  #x = transects_German
  x$areaKm = as.numeric(area)
   
  # Analysis
  x$distance = (x$Dist_E - x$Dist_S) / 1000
  x$sum_trans = sum(x$distance)
  x$Actual_Weighting = (x$distance) / (x$sum_trans)
  x$calc_actual_mean_sa = 10 ^ (x$Area_Backscatter_Strength / 10) * x$Actual_Weighting
  x$trans_meanSa = 10 * log10(x$calc_actual_mean_sa)
  x$TS <- as.numeric(ifelse(grepl(38, x$Frequency), TS38, ifelse(
    grepl(50, x$Frequency), TS50, ifelse(grepl(75, x$Frequency), TS75, TS120)
  )))
  x$abund_density = 10 ^ ((x$Area_Backscatter_Strength - (x$TS)) / 10) # same as Jenna's Transects.R script calculation
  x$abund_density[x$abund_density < 0.0000001] <- 0
  x$meanSa = 10 * log10(sum(x$calc_actual_mean_sa))
  x$Date_S = min(x$Date_S)
  x$Vessel = left(x$Region_name, 2)
  x$Transect_No =  right(left(x$Region_name, 6),3)
  x$TransCount = length(x$calc_actual_mean_sa)

  
  x$density = (x$abund_density) * (x$distance)
  x$weighted_mean_abund_calc = (x$abund_density) * (x$Actual_Weighting)
  x$trans_abund = (x$weighted_mean_abund_calc) * x$areaKm 
  x$total_abund = sum(x$trans_abund)
  x$mean_abund_density = mean(x$weighted_mean_abund_calc)
  x$Variance = var(x$weighted_mean_abund_calc)
  x$AbundVariance = (x$areaKm)^2 *  var(x$weighted_mean_abund_calc)
  x$standard_error_abund = sqrt(x$AbundVariance) 
  x$standard_error_perc = x$standard_error_abund / x$total_abund * 100
  x$SD = sqrt(x$AbundVariance)
  x$cv =  (x$SD / x$total_abund )*100
  x$ci =  1.96 * x$SD
  return(x)
}



abundCalc_bycell<-  function(x, area, TS38, TS50, TS75, TS120) {
  
  right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
  left = function (string,char){substr(string,1,char)}
  #x = transects_German
  x$areaKm = as.numeric(area)
  x$PRC_ABC[x$PRC_ABC < 1e-8] <- 0
  
  # Analysis
  x$distance = (x$Dist_E - x$Dist_S) / 1000
  x$sum_trans = sum(x$distance)
  x$Actual_Weighting = (x$distance) / (x$sum_trans)
  x$calc_actual_mean_sa = x$PRC_ABC * x$Actual_Weighting
  # x$trans_meanSa = 10 * log10(x$calc_actual_mean_sa)
  x$TS <- as.numeric(ifelse(grepl(38, x$Frequency), TS38, ifelse(
    grepl(50, x$Frequency), TS50, ifelse(grepl(75, x$Frequency), TS75, TS120)
  )))
  x$abund_density = 10 ^ (( (10*log10(x$PRC_ABC)) - (x$TS)) / 10) # To covert to ABS and then density.
  x$meanSa = 10 * log10(sum(x$calc_actual_mean_sa))
  x$Date_S = min(x$Date_S)
  x$Vessel = left(x$Region_name, 2)
  x$Transect_No =  right(left(x$Region_name, 6),3)
  x$TransCount = length(x$calc_actual_mean_sa)
  
  
  x$density = (x$abund_density) * (x$distance)
  x$weighted_mean_abund_calc = (x$abund_density) * (x$Actual_Weighting)
  x$trans_abund = (x$weighted_mean_abund_calc) * x$areaKm 
  x$total_abund = sum(x$trans_abund)
  x$mean_abund_density = mean(x$weighted_mean_abund_calc)
  x$Variance = var(x$weighted_mean_abund_calc)
  x$AbundVariance = (x$areaKm)^2 *  var(x$weighted_mean_abund_calc)
  x$standard_error_abund = sqrt(x$AbundVariance ) 
  x$standard_error_perc = x$standard_error_abund / x$total_abund * 100
  x$SD = sqrt(x$AbundVariance)
  x$cv =  (x$SD / x$total_abund )*100
  x$ci =  1.96 * x$SD
  return(x)
}


biomassCalc_bycell<-  function(x, area, TS38, TS50, TS75, TS120) {
  
  right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
  left = function (string,char){substr(string,1,char)}
  #x = transects_German
  x$areaKm = as.numeric(area)
  x$PRC_ABC[x$PRC_ABC < 1e-8] <- 0
  
  # Analysis
  x$distance = (x$Dist_E - x$Dist_S) / 1000
  x$sum_trans = sum(x$distance)
  x$Actual_Weighting = (x$distance) / (x$sum_trans)
  x$calc_actual_mean_sa = x$PRC_ABC * x$Actual_Weighting
  # x$trans_meanSa = 10 * log10(x$calc_actual_mean_sa)
  x$TS <- as.numeric(ifelse(grepl(38, x$Frequency), TS38, ifelse(
    grepl(50, x$Frequency), TS50, ifelse(grepl(75, x$Frequency), TS75, TS120)
  )))
  x$biomass_density = 10 ^ (( (10*log10(x$PRC_ABC)) - (x$TS)) / 10) # To covert to ABS and then density.
  x$meanSa = 10 * log10(sum(x$calc_actual_mean_sa))
  x$Date_S = min(x$Date_S)
  x$Vessel = left(x$Region_name, 2)
  x$Transect_No =  right(left(x$Region_name, 6),3)
  x$TransCount = length(x$calc_actual_mean_sa)
  
  x$density = (x$biomass_density) * (x$distance)
  x$weighted_mean_biomass_calc = (x$biomass_density) * (x$Actual_Weighting)
  x$trans_biomass = (x$weighted_mean_biomass_calc) * x$areaKm * 1000
  x$total_biomass = sum(x$trans_biomass)
  x$mean_biomass_density = mean(x$weighted_mean_biomass_calc)
  x$Variance = var(x$weighted_mean_biomass_calc)
  x$BiomassVariance = (x$areaKm * 1000)^2 *  var(x$weighted_mean_biomass_calc)
  x$standard_error_tonnes = sqrt(x$BiomassVariance) 
  x$standard_error_perc = x$standard_error_tonnes / x$total_biomass * 100
  x$SD = sqrt(x$BiomassVariance)
  x$cv =  (x$SD / x$total_biomass )*100
  x$ci =  1.96 * x$SD
  
  return(x)
}

# 
# x$delta_mean_biomass_density = as.numeric(delta_ln_ftn(x$weighted_mean_biomass_calc)[1])
# x$total_biomass = sum(x$delta_mean_biomass_density *x$areaKm *1000)
# x$delta_var_mean = as.numeric(delta_ln_ftn(x$weighted_mean_biomass_calc)[2])
# x$delta_var_mean_biomass = ((x$areaKm*1000)^2*x$delta_var_mean)
# x$delta_length <- length(x$weighted_mean_biomass_calc)
# x$delta_standard_error_tonnes = sqrt(x$delta_var_mean_biomass / x$delta_length)
# x$delta_standard_error_perc = x$delta_standard_error_tonnes / x$total_biomass * 100


#call files to determine calibration list - using string to determine vessel calibration
right = function (string, char) {
  substr(string, nchar(string) - (char - 1), nchar(string))
}
left = function (string, char) {
  substr(string, 1, char)
}
datediff = function(date2, date1) {
  if(
    as.numeric(gsub("S", "", gsub("d", "", left(
      as.character(days(date2) - days(date1)), 2
    )))) < 0
  ){
    0
  } else {
    as.numeric(gsub("S", "", gsub("d", "", left(
      as.character(days(date2) - days(date1)), 2
    ))))
  }
}

turnoverBio = function(y_intercept,
                       x_Var_1,
                       daysturnover,
                       Date,
                       Survey,
                       Biomass) {
  turnMat <-
    matrix(nrow = (length(Date) - 1), ncol = (length(Survey) -
                                                1))
  
  for (j in 1:(length(Survey) - 1)) {
    for (i in 1:(length(Date) - 1)) {
      turnMat[i, j] <-
        (yday(Date[i + 1]) - yday(Date[j]))
      if (turnMat[i, j] >= daysturnover) {
        turnMat[i, j] <- 0
      } else if (turnMat[i, j] < 0) {
        turnMat[i, j] <- 0
      }else if(turnMat[i, j] > 0){
        turnMat[i, j] <- (1 - (x_Var_1 * log10(turnMat[i, j]) + y_intercept))*Biomass[Survey[j]]
      }  
    }
  }
  finalbiomass <-
    Biomass[2:length(Biomass)] - rowSums(turnMat)
  finalbiomass[finalbiomass < 0] = 0
  totalbiomass <-
    as.integer(sum(c(Biomass[1], finalbiomass)))
  return(totalbiomass)
}



turnoverBio_adjustedBio = function(y_intercept,
                                   x_Var_1,
                                   daysturnover,
                                   Date,
                                   Survey,
                                   Biomass) {
  turnMat <-
    matrix(nrow = (length(Date) - 1), ncol = (length(Survey) -
                                                1))
  
  for (j in 1:(length(Survey) - 1)) {
    for (i in 1:(length(Date) - 1)) {
      turnMat[i, j] <-
        (yday(Date[i + 1]) - yday(Date[j]))
      if (turnMat[i, j] >= daysturnover) {
        turnMat[i, j] <- 0
      } else if (turnMat[i, j] < 0) {
        turnMat[i, j] <- 0
      }else if(turnMat[i, j] > 0){
        turnMat[i, j] <- (1 - (x_Var_1 * log10(turnMat[i, j]) + y_intercept))*Biomass[Survey[j]]
      }  
    }
  }
  finalbiomass <-
    Biomass[2:length(Biomass)] - rowSums(turnMat)
  finalbiomass[finalbiomass < 0] = 0
  totalbiomass <-
    (c(Biomass[1], finalbiomass))
  return(totalbiomass)
}



turnoverBio_emperical <- function(
    Date,
    Survey,
    Biomass,
    Ground,
    daysturnover
) {
  
  turnMat <-
    matrix(nrow = (length(Date) - 1), ncol = (length(Survey) -
                                                1))
  
  for (j in 1:(length(Survey) - 1)) {
    for (i in 1:(length(Date) - 1)) {
      turnMat[i, j] <-
        (yday(Date[i + 1]) - yday(Date[j]))
      if (turnMat[i, j] >= daysturnover) {
        turnMat[i, j] <- 0
      } else if (turnMat[i, j] < 0) {
        turnMat[i, j] <- 0
      }else if(turnMat[i, j] > 0){
        turnMat[i, j] <-  (1 - (min(Ground$Cum_Day_Large[Ground$Days == turnMat[i, j]], 1)))*Biomass[Survey[j]]
      }  
    }
  }
  finalbiomass <-
    Biomass[2:length(Biomass)] - rowSums(turnMat)
  finalbiomass[finalbiomass < 0] = 0
  totalbiomass <-
    as.integer(sum(c(Biomass[1], finalbiomass)))
  return(totalbiomass)
}


turnoverBio_adjustedBio_empirical<- function(
    Date,
    Survey,
    Biomass,
    Ground,
    daysturnover
){
  turnMat <-
    matrix(nrow = (length(Date) - 1), ncol = (length(Survey) -
                                                1))
  for (j in 1:(length(Survey) - 1)) {
    for (i in 1:(length(Date) - 1)) {
      turnMat[i, j] <-
        (yday(Date[i + 1]) - yday(Date[j]))
      if (turnMat[i, j] >= daysturnover) {
        turnMat[i, j] <- 0
      } else if (turnMat[i, j] <= 0) {
        turnMat[i, j] <- 0
      }
      else if(turnMat[i, j] > 0){
        turnMat[i, j] <-  (1 - (Ground$Cum_Day_Large[Ground$Days == turnMat[i, j]]))*Biomass[Survey[j]]  
      }  
    }
  }
  finalbiomass <-
    Biomass[2:length(Biomass)] - rowSums(turnMat)
  finalbiomass[finalbiomass < 0] = 0
  totalbiomass <-
    (c(Biomass[1], finalbiomass))
  return(totalbiomass)
}

