# load packages
require(rgeos)
require(sp)
require(rgdal)
require(tidyverse)
require(geosphere)
require(chron)
require(lubridate)
require(tmaptools)
require(extrafont)
require(reshape2)

# load more fonts for plot design (via extrafont package)
loadfonts(device = "win")

# import shapefiles of lakes and parks for map layout
import = ogrListLayers("binnenalster.kml")
alster = readOGR("binnenalster.kml",import[1])
alster = fortify(alster)

import = ogrListLayers("aussenalster.kml")
aussenalster = readOGR("aussenaussenalster.kml",import[1])
aussenalster = fortify(aussenalster)

import = ogrListLayers("stadtpark.kml")
stadtpark = readOGR("stadtpark.kml",import[1])
stadtpark = fortify(stadtpark)

# import route of marathon
import = ogrListLayers("marathon_route.kml")
marathon_route = readOGR("marathon_route.kml",import[1],require_geomType="wkbLineString")

# offset lines for wider running route
off1 = double_line(marathon_route, width = .0003, sides = "right")
off2 = double_line(marathon_route, width = .0006, sides = "right")
off3 = double_line(marathon_route, width = .0009, sides = "right")
off4 = double_line(marathon_route, width = .0012, sides = "right")

# convert sp-lines-object to dataframe
off0 = fortify(marathon_route)
off1 = fortify(off1)
off2 = fortify(off2)
off3 = fortify(off3)
off4 = fortify(off4)

# turn around order of dataframes
off1$seq = seq(1:nrow(off1))
off1 = off1[order(off1$seq, decreasing = TRUE), ]
off1$seq = NULL

off2$seq = seq(1:nrow(off2))
off2 = off2[order(off2$seq, decreasing = TRUE), ]
off2$seq = NULL

off3$seq = seq(1:nrow(off3))
off3 = off3[order(off3$seq, decreasing = TRUE), ]
off3$seq = NULL

off4$seq = seq(1:nrow(off4))
off4 = off4[order(off4$seq, decreasing = TRUE), ]
off4$seq = NULL

# loop over all dataframes
off_lines_list = list(off2,off3,off1,off0,off4)
datalistall = list()
counter_1 = 1

# loop for selected dataframe
for (counter_2 in off_lines_list) {
  
  # select dataframe
  selected_off = counter_2
  selected_off = select(selected_off, long, lat)
  print(nrow(selected_off))
  
  # calculate distances between coordinates of selected route and add to datafrane
  datalist = list()
  for (i in 1:nrow(selected_off)) {
    
    datalist[[i+1]] = distm(c(selected_off$long[i], selected_off$lat[i]), c(selected_off$long[i+1], selected_off$lat[i+1]), fun = distHaversine)
    
  }
  datalist[[1]] = 0
  distance_df = do.call(rbind, datalist)
  distance_df = distance_df[1:length(distance_df)-1]
  selected_off$dist = distance_df
  
  # add as many new coordinates to route until distances between coordinates become <= 10
  repeat{
    for (i in 2:nrow(selected_off)) {
      if(selected_off$dist[i] >= 10){
        
        new_coords = as.data.frame(midPoint(c(selected_off$long[i],selected_off$lat[i]),c(selected_off$long[i-1],selected_off$lat[i-1]))[1])
        new_coords$lat = midPoint(c(selected_off$long[i],selected_off$lat[i]),c(selected_off$long[i-1],selected_off$lat[i-1]))[2]
        colnames(new_coords) = c("long","lat")
        new_coords$dist = NA
        selected_off = rbind(selected_off[1:i-1,],new_coords,selected_off[-(1:i-1),])
        
        # calculate distances again with new coordinates
        datalist = list()
        for (i in 1:nrow(selected_off)) {
          datalist[[i+1]] = distm(c(selected_off$long[i], selected_off$lat[i]), c(selected_off$long[i+1], selected_off$lat[i+1]), fun = distHaversine)
        }
        datalist[[1]] = 0
        distance_df = do.call(rbind, datalist)
        distance_df = distance_df[1:length(distance_df)-1]
        selected_off$dist = distance_df
      }
    }
    if (max(selected_off$dist, na.rm = T) < 10) break
  }
  
  # add cumsum for distance and completed distance
  selected_off$dist_cum = cumsum(selected_off$dist)
  selected_off$dist_complete = selected_off$dist_cum/max(selected_off$dist_cum)
  
  # read in runners data (see scraping script)
  data = runners
  
  # select every 5th runner (because with offseted lines we have five routes)
  data = data[seq(counter_1,nrow(data),5),]
  
  # get position of every runner for every 20 seconds
  datalist = list()
  for (i in seq(1,max(runners$secondsFinish),20)) {
    
    # select one runner and calculate his position
    for (j in 1:nrow(data)) {
      
      length_run = 42.195
      
      data$current_pos[j] = ifelse(i <= data$seconds5[j],  ((i/data$seconds5[j])*(5/length_run)),
                            ifelse(i <= data$seconds10[j], (5/length_run)+(((i-data$seconds5[j])/(data$seconds10[j]-data$seconds5[j]))*(5/length_run)),
                            ifelse(i <= data$seconds15[j], (10/length_run)+(((i-data$seconds10[j])/(data$seconds15[j]-data$seconds10[j]))*(5/length_run)),
                            ifelse(i <= data$seconds20[j], (15/length_run)+(((i-data$seconds15[j])/(data$seconds20[j]-data$seconds15[j]))*(5/length_run)),
                            ifelse(i <= data$seconds25[j], (20/length_run)+(((i-data$seconds20[j])/(data$seconds25[j]-data$seconds20[j]))*(5/length_run)),
                            ifelse(i <= data$seconds30[j], (25/length_run)+(((i-data$seconds25[j])/(data$seconds30[j]-data$seconds25[j]))*(5/length_run)), 
                            ifelse(i <= data$seconds35[j], (30/length_run)+(((i-data$seconds30[j])/(data$seconds35[j]-data$seconds30[j]))*(5/length_run)),    
                            ifelse(i <= data$seconds40[j], (35/length_run)+(((i-data$seconds35[j])/(data$seconds40[j]-data$seconds35[j]))*(5/length_run)),         
                                                           (40/length_run)+(((i-data$seconds40[j])/(data$secondsFinish[j]-data$seconds40[j]))*(2.195/length_run))))))))))              
      
      data$lon[j] = selected_off$long[which.min(abs(selected_off$dist_complete - data$current_pos[j]))]
      data$lat[j] = selected_off$lat[which.min(abs(selected_off$dist_complete - data$current_pos[j]))]
      data$past_seconds = i
    }
    data = filter(data, current_pos <= 1)
    datalist[[i]] = data
    
  }
  
  runners_data = do.call(rbind, datalist)
  datalistall[[counter_1]] = runners_data
  counter_1 = counter_1+1
}
final_data = do.call(rbind, datalistall)

# plot function for different times
plot_function = function(i= 1, maxi = max(final_data$past_seconds)){
  
  final_data_save = filter(final_data, past_seconds == i)
  
  ggplot() +
    geom_polygon(data = stadtpark, aes(x = long, y = lat), fill = "#89ff8a", color = NA) +
    geom_polygon(data = alster, aes(x = long, y = lat), fill = "#89b8ff", color = NA) +
    geom_polygon(data = aussenalster, aes(x = long, y = lat), fill = "#89b8ff", color = NA) +
    geom_path(data = off2, aes(x = long, y = lat), color = "#cecece", size = 5) +
    geom_point(data = final_data_save, aes(x = lon, y = lat), colour = "black", size = 0.5) +
    theme(text=element_text(family = "Tw Cen MT"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust=0.5, size=35, face="bold",colour = "#2d2d2d"),
          plot.caption = element_text(hjust=0.5, size = 10, color = "#2d2d2d"),
          legend.position = "none",
          plot.subtitle = element_text(color = "#2d2d2d", hjust = 0.5, size = 23),
          panel.grid.major = element_line(color = "#f2f2f2", size = 0.2, linetype = "dashed"),
          panel.grid.minor = element_line(color = "#f2f2f2", size = 0.2, linetype = "dashed"),
          plot.background = element_rect(fill = "white", color = NA), 
          panel.background = element_rect(fill = "white", color = NA), 
          panel.border = element_blank()) +
    labs(caption = "Source: haspa-marathon-hamburg.de/ergebnisse") +
    labs(title = paste0("Hamburg Marathon 2018"), subtitle = "Time Lapse of all Runners") +
    geom_text(aes(x=10.0185, y=53.59607, label="Stadtpark"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=10.00708, y=53.56442, label="Alster"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=9.96168, y=53.55407, label="St. Pauli"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=9.91687, y=53.55224, label="Ottensen"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=10.02107, y=53.55244, label="St. Georg"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=9.97927, y=53.56865, label="Rotherbaum"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=10.02811, y=53.61267, label="Alsterdorf"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=9.99425, y=53.5911, label="Eppendorf"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=10.03026, y=53.57334, label="Uhlenhorst"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=9.93558, y=53.59617, label=paste0("Elapsed Time:\n", format(as.POSIXct(final_data_save$past_seconds[1], origin = "1970-01-01", tz = "UTC"), "%H:%M"))), size=9, colour = "#2d2d2d", family = "Tw Cen MT") +
    coord_fixed(ratio = 1.5/1)
}

# function for saving images
plot_save = function(i=1){
  file_path = paste0("YOUR_PATH", "/plot_",i ,".png")
  ggsave(filename=file_path, plot_function(i), dpi = 150, width = 7, height = 7)
}

# save all images
map(seq(1,max(runners$secondsFinish),20), plot_save)