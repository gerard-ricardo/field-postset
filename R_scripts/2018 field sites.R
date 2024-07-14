#2017 Magnetic Island map
library(rgdal)
feat  <- readOGR(dsn="C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/R/Spatial/maps/GBRMPA/zipfolder",layer="Great_Barrier_Reef_Features")
library(ggplot2)
feat.f = fortify(feat)
mag <- ggplot() +
  geom_polygon(data = feat.f, aes(long, lat, group = group), fill = "grey83") +
  geom_path(data = feat.f, aes(long, lat,group=group), colour = "black") +
  coord_map(xlim = c(146.75, 146.9),ylim = c(-19.1, -19.25)) 
#mag = mag + theme_sleek()
mag = mag + theme_sleek1()
samp.site <- data.frame(Longitude= 146.881891 ,Latitude =-19.121135)  #Florence
samp.site2 <- data.frame(Longitude= 146.839533,Latitude =-19.186083)  #Picnic Bay
samp.site3 <- data.frame(Longitude= 146.864283,Latitude =-19.155650)  #Geoffrey Bay , 
samp.site4 <- data.frame(Longitude= 146.813950,Latitude =-19.196050)  #Middle Reef , 
samp.site5 <- data.frame(Longitude= 146.791683,Latitude =-19.211483)  #virago shoals  , 

mag = mag + geom_point(data=samp.site,aes(x=Longitude, y=Latitude), colour="steelblue1", size=4)
mag = mag + geom_point(data=samp.site2,aes(x=Longitude, y=Latitude), colour="steelblue1", size=4)
mag = mag + geom_point(data=samp.site3,aes(x=Longitude, y=Latitude), colour="steelblue1", size=4)
mag = mag + geom_point(data=samp.site4,aes(x=Longitude, y=Latitude), colour="steelblue1", size=4)
mag = mag + geom_point(data=samp.site5,aes(x=Longitude, y=Latitude), colour="steelblue1", size=4)
#mag = mag + geom_text(aes(label = y), position = "samp.site")
mag = mag + scale_y_continuous(name ="Latitude") 
mag = mag + scale_x_continuous(name ="Longitude") 

#Vola! Now scale bars is where R really fails. Luckily some guy has written some functions. It isn't really in package form yet. First we need to instal

#devtools::install_github("3wen/legendMap")  #http://egallic.fr/en/scale-bar-and-north-arrow-on-a-ggplot2-map/

#Don't be put of by the masses of code below, it only 3 functions that you can collapse later into just three lines and never have to touch. Deep breathes...

#1

create_scale_bar <- function(lon,lat,distance_lon,distance_lat,distance_legend, dist_units = "km"){
  # First rectangle
  bottom_right <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon, dist.units = dist_units, model = "WGS84")
  
  topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_lat, dist.units = dist_units, model = "WGS84")
  rectangle <- cbind(lon=c(lon, lon, bottom_right[1,"long"], bottom_right[1,"long"], lon),
                     lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  
  # Second rectangle t right of the first rectangle
  bottom_right2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon*2, dist.units = dist_units, model = "WGS84")
  rectangle2 <- cbind(lon = c(bottom_right[1,"long"], bottom_right[1,"long"], bottom_right2[1,"long"], bottom_right2[1,"long"], bottom_right[1,"long"]),
                      lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  
  # Now let's deal with the text
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_legend, dist.units = dist_units, model = "WGS84")
  on_top2 <- on_top3 <- on_top
  on_top2[1,"long"] <- bottom_right[1,"long"]
  on_top3[1,"long"] <- bottom_right2[1,"long"]
  
  legend <- rbind(on_top, on_top2, on_top3)
  legend <- data.frame(cbind(legend, text = c(0, distance_lon, distance_lon*2)), stringsAsFactors = FALSE, row.names = NULL)
  return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}

#Collapse the code and run number 2

create_orientation_arrow <- function(scale_bar, length, distance = 1, dist_units = "km"){
  lon <- scale_bar$rectangle2[1,1]
  lat <- scale_bar$rectangle2[1,2]
  
  # Bottom point of the arrow
  beg_point <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance, dist.units = dist_units, model = "WGS84")
  lon <- beg_point[1,"long"]
  lat <- beg_point[1,"lat"]
  
  # Let us create the endpoint
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = length, dist.units = dist_units, model = "WGS84")
  
  left_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 225, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  right_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 135, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  res <- rbind(
    cbind(x = lon, y = lat, xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = left_arrow[1,"long"], y = left_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = right_arrow[1,"long"], y = right_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]))
  
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  
  # Coordinates from which "N" will be plotted
  coords_n <- cbind(x = lon, y = (lat + on_top[1,"lat"])/2)
  
  return(list(res = res, coords_n = coords_n))
}

#Collapse. 3
scale_bar <- function(lon, lat, distance_lon, distance_lat, distance_legend, dist_unit = "km", rec_fill = "white", rec_colour = "black", rec2_fill = "black", rec2_colour = "black", legend_colour = "black", legend_size = 3, orientation = TRUE, arrow_length = 500, arrow_distance = 300, arrow_north_size = 6){
  the_scale_bar <- create_scale_bar(lon = lon, lat = lat, distance_lon = distance_lon, distance_lat = distance_lat, distance_legend = distance_legend, dist_unit = dist_unit)
  # First rectangle
  rectangle1 <- geom_polygon(data = the_scale_bar$rectangle, aes(x = lon, y = lat), fill = rec_fill, colour = rec_colour)
  
  # Second rectangle
  rectangle2 <- geom_polygon(data = the_scale_bar$rectangle2, aes(x = lon, y = lat), fill = rec2_fill, colour = rec2_colour)
  
  # Legend
  scale_bar_legend <- annotate("text", label = paste(the_scale_bar$legend[,"text"], dist_unit, sep=""), x = the_scale_bar$legend[,"long"], y = the_scale_bar$legend[,"lat"], size = legend_size, colour = legend_colour)
  
  res <- list(rectangle1, rectangle2, scale_bar_legend)
  
  if(orientation){# Add an arrow pointing North
    coords_arrow <- create_orientation_arrow(scale_bar = the_scale_bar, length = arrow_length, distance = arrow_distance, dist_unit = dist_unit)
    arrow <- list(geom_segment(data = coords_arrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coords_arrow$coords_n[1,"x"], y = coords_arrow$coords_n[1,"y"], size = arrow_north_size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res)
}

#You made it. Now we can add it on
library(maptools)
mag <- mag + scale_bar(lon = 146.855, lat = -19.23, 
                       distance_lon = 2, distance_lat = 0.4, distance_legend = 0.8, 
                       dist_unit = "km")  #change position of scale bar here

mag


# setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/6 Post-settlement/1 2018/2018 field/3 figures")
# setEPS()
# postscript("map1.eps")
# mag
# dev.off()
  

  