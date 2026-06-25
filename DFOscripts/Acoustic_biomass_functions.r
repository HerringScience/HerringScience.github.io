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
  df<-rbind(df,df[1,])
  #wonky polygon but used to find centroid
  transect_p <-st_polygon(list(matrix( c( df$longitude,df$latitude), nrow = length(df$longitude), ncol=2)))
  transect_p_poly <- st_sfc(transect_p, crs = crs_use)
  transect_p_attrib <- data.frame(name = paste0(surveydate))
  transect_p_sf <- st_sf(transect_p_attrib, geometry = transect_p_poly)
  
  centy <-st_coordinates(st_centroid(transect_p_sf))
  
  # Arrange by angle
  if (transectEastWest) {
    
    West <- subset(df, longitude < centy[1,1])
    East <-subset(df, longitude >= centy[1,1])
    West<-West[order(West$latitude),]
    West$ordered <-West$latitude*West$longitude
   # West<-West[order(West$ordered, decreasing=TRUE),]
    
    East<-East[order(East$latitude, decreasing=TRUE),]
    East$ordered <-East$latitude*East$longitude
    East<-East[order(East$ordered, decreasing=FALSE),]
    
    df2<-rbind(West,East)
    df2 <- rbind(df2,df2[1,])
    
    
  } else {
    
    South <- subset(df, latitude < centy[1,2])
    North <-subset(df, latitude >= centy[1,2])
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
    geom_sf(data = transect_l_sf, color = "red")
  sf::sf_use_s2(FALSE)
  a<-st_area(transect_p_sf2)
  a<-set_units(a, km^2)
  a<-as.numeric(a)
  
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
  x$areaKm = area
  
  # Analysis
  x$distance = (x$Dist_E - x$Dist_S) / 1000
  x$sum_trans = sum(x$distance)
  x$Actual_Weighting = (x$distance) / (x$sum_trans)
  x$calc_actual_mean_sa = 10 ^ (x$Area_Backscatter_Strength / 10) * x$Actual_Weighting
  x$trans_meanSa = 10 * log10(x$calc_actual_mean_sa)
  x$TS <- as.numeric(ifelse(grepl(38, x$Frequency), TS38, ifelse(
    grepl(50, x$Frequency), TS50, ifelse(grepl(75, x$Frequency), TS75, TS120)
  )))
  x$biomass_density = 10 ^ ((x$Area_Backscatter_Strength - (x$TS)) / 10) # same as Jenna's Transects.R script calculation
  x$density = (x$biomass_density) * (x$distance)
  x$weighted_mean_biomass_calc = (x$biomass_density) * (x$Actual_Weighting)
  x$trans_biomass = (x$weighted_mean_biomass_calc) * x$areaKm * 1000
  x$total_biomass = sum(x$trans_biomass)
  
  se <- function(x)
    sqrt(var(x) / length(x))
  x$se = se(x$biomass_density)
  x$standard_error_tonnes = x$se * x$areaKm * 1000
  x$standard_error_perc = x$standard_error_tonnes / x$total_biomass * 100
  x$COV_perc = x$standard_error_tonnes/x$total_biomass*100
  x$Variance = x$standard_error_tonnes^2
  x$mean_biomass_density = mean(x$biomass_density)
  x$meanSa = 10 * log10(sum(x$calc_actual_mean_sa))
  x$Date_S = min(x$Date_S)
  x$Vessel = left(x$Region_name, 2)
  x$Transect_No =  right(left(x$Region_name, 6),3)
  x$TransCount = length(x$biomass_density)
  x$SD = x$standard_error_tonnes*x$TransCount^0.5
  
  return(x)
}


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
        if (datediff(Date[i + 1], Date[j]) >= daysturnover) {
          turnMat[i, j] <- 0
        } else  if ((1 - (x_Var_1 * log10(
          datediff(Date[i + 1], Date[j])
        ) + y_intercept)) * Biomass[Survey[j]] < 0) {
          turnMat[i, j] <- 0
        } else {
          turnMat[i, j] <- (1 - (x_Var_1 * log10(
            datediff(Date[i + 1], Date[j])
          ) + y_intercept)) * Biomass[Survey[j]]
        }
    }
  }
    turnMat[turnMat == Inf] = 0
    finalbiomass <-
      Biomass[2:length(Biomass)] - rowSums(turnMat)
    finalbiomass[finalbiomass < 0] = 0
    totalbiomass <-
      as.integer(sum(c(Biomass[1], finalbiomass)))
return(totalbiomass)
}



turnoverBio_adjusted = function(y_intercept,
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
        if (datediff(Date[i + 1], Date[j]) >= daysturnover) {
          turnMat[i, j] <- 0
        } else  if ((1 - (x_Var_1 * log10(
          datediff(Date[i + 1], Date[j])
        ) + y_intercept)) * Biomass[Survey[j]] < 0) {
          turnMat[i, j] <- 0
        } else {
          turnMat[i, j] <- (1 - (x_Var_1 * log10(
            datediff(Date[i + 1], Date[j])
          ) + y_intercept)) * Biomass[Survey[j]]
        }
    }
  }
  turnMat[turnMat == Inf] = 0
  finalbiomass <-
    Biomass[2:length(Biomass)] - rowSums(turnMat)
  finalbiomass[finalbiomass < 0] = 0
  totalbiomass <-
   (c(Biomass[1], finalbiomass))
  return(totalbiomass)
}

