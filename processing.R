# load packages
require(rgeos)
require(sp)
require(rgdal)
require(tidyverse)
require(geosphere)
require(chron)
require(lubridate)
require(tmaptools)
require(gganimate)
require(plotly)
require(extrafont)

# load more fonts for plotting (via extrafont package)
loadfonts(device = "win")

# import elbe
setwd("C:/Users/akruse/Downloads/")
import <- ogrListLayers("C:/Users/akruse/Downloads/elbe.kml")
elbe = readOGR("elbe.kml",import[1])
elbe = fortify(elbe)

# import alster
setwd("C:/Users/akruse/Downloads/")
import <- ogrListLayers("C:/Users/akruse/Downloads/alster.kml")
alster = readOGR("alster.kml",import[1])
alster = fortify(alster)

# import alster
setwd("C:/Users/akruse/Downloads/")
import <- ogrListLayers("C:/Users/akruse/Downloads/alsteraus.kml")
alsteraus = readOGR("alsteraus.kml",import[1])
alsteraus = fortify(alsteraus)

# import stadtpark
setwd("C:/Users/akruse/Downloads/")
import <- ogrListLayers("C:/Users/akruse/Downloads/spark.kml")
spark = readOGR("spark.kml",import[1])
spark = fortify(spark)

# import route
setwd("C:/Users/akruse/Downloads/")
import <- ogrListLayers("C:/Users/akruse/Downloads/mlayer.kml")
l2 = readOGR("mlayer.kml",import[1],require_geomType="wkbLineString")

# offset lines
res6 = double_line(l2, width = .0003, sides = "right")
res7 = double_line(l2, width = .0006, sides = "right")
res8 = double_line(l2, width = .0009, sides = "right")
res9 = double_line(l2, width = .0012, sides = "right")

# convert lines to dataframe
res1 = fortify(l2)
res6 = fortify(res6)
res7 = fortify(res7)
res8 = fortify(res8)
res9 = fortify(res9)

# sort dataframes
res6$seq = seq(1:nrow(res6))
res6 = res6[order(res6$seq, decreasing = TRUE), ]
res6$seq = NULL

res7$seq = seq(1:nrow(res7))
res7 = res7[order(res7$seq, decreasing = TRUE), ]
res7$seq = NULL

res8$seq = seq(1:nrow(res8))
res8 = res8[order(res8$seq, decreasing = TRUE), ]
res8$seq = NULL

res9$seq = seq(1:nrow(res9))
res9 = res9[order(res9$seq, decreasing = TRUE), ]
res9$seq = NULL

# loop over all offset line
them_res = list(res1,res6,res7,res8,res9)
datalistall = list()
its = 1
for (h in them_res) {
  
  # select dataframe
  res = h
  res = select(res, long, lat)
  
  # calculate distances between coordinates
  datalist = list()
  for (i in 1:nrow(res)) {
    
    datalist[[i+1]] <- distm(c(res$long[i], res$lat[i]), c(res$long[i+1], res$lat[i+1]), fun = distHaversine)
    
  }
  datalist[[1]] = 0
  big_data = do.call(rbind, datalist)
  big_data = big_data[1:length(big_data)-1]
  res$dist = big_data
  
  # add more coordinates for precision
  repeat{
    for (i in 2:nrow(res)) {
      if(res$dist[i] >= 500){
        
        savory = as.data.frame(midPoint(c(res$long[i],res$lat[i]),c(res$long[i-1],res$lat[i-1]))[1])
        savory$lat = midPoint(c(res$long[i],res$lat[i]),c(res$long[i-1],res$lat[i-1]))[2]
        colnames(savory) = c("long","lat")
        savory$dist = NA
        res <- rbind(res[1:i-1,],savory,res[-(1:i-1),])
        
        # calculate distances again with new coordinates
        datalist = list()
        for (i in 1:nrow(res)) {
          datalist[[i+1]] <- distm(c(res$long[i], res$lat[i]), c(res$long[i+1], res$lat[i+1]), fun = distHaversine)
        }
        datalist[[1]] = 0
        big_data = do.call(rbind, datalist)
        big_data = big_data[1:length(big_data)-1]
        res$dist = big_data
      }
    }
    if (max(res$dist, na.rm = T) < 500) break
  }
  
  # add cumsum and relative distance
  res$dist_cum = cumsum(res$dist)
  res$dist_max = res$dist_cum/max(res$dist_cum)
  
  # read in runners data and do some processing
  data = read.csv("Läufer_20180425_080444.csv")
  data1 = read.csv("Läufer_20180425_080433.csv")
  data = rbind(data,data1)
  data = data[seq(its,nrow(data),5),]
  data$Brutto = chron(times = as.character(data$Brutto))
  data$seconds = period_to_seconds(hms(data$Brutto))
  data = select(data, Name, Brutto, seconds)
  max_run = round(max(data$seconds)/60, 0)
  
  # get position of runners for different times
  datalist = list()
  for (i in seq(1,15000, 100)) {
    
    data$nach60 = i/data$seconds
    for (j in 1:nrow(data)) {
      data$lon[j] = res$long[which.min(abs(res$dist_max - data$nach60[j]))]
      data$lat[j] = res$lat[which.min(abs(res$dist_max - data$nach60[j]))]
      data$pasted_secs = i
    }
    datalist[[i]] <- data
  }
  
  big_data = do.call(rbind, datalist)
  datalistall[[its]] <- big_data
  its = its+1
}
all_data = do.call(rbind, datalistall)

# plot function for different times
p.progress = function(i= 1, maxi = max(all_data$pasted_secs)){
   all_data_save = filter(all_data, pasted_secs == i)
   ggplot() +
     geom_polygon(data = spark, aes(x = long, y = lat), fill = "#3f3f3f", color = NA) +
     geom_polygon(data = alster, aes(x = long, y = lat), fill = "#3f3f3f", color = NA) +
     geom_polygon(data = alsteraus, aes(x = long, y = lat), fill = "#3f3f3f", color = NA) +
     #geom_polygon(data = elbe, aes(x = long, y = lat), fill = "#3f3f3f", color = NA) +
     
     #geom_path(data = res7, aes(x = long, y = lat), color = "#505050", size = 2.5) +
     
     geom_path(data = res1, aes(x = long, y = lat), color = "#73C7D9", alpha = 0.2) +
     geom_path(data = res6, aes(x = long, y = lat), color = "#73C7D9", alpha = 0.4) +
     geom_path(data = res7, aes(x = long, y = lat), color = "#73C7D9", alpha = 0.6) +
     geom_path(data = res8, aes(x = long, y = lat), color = "#73C7D9", alpha = 0.4) +
     geom_path(data = res9, aes(x = long, y = lat), color = "#73C7D9", alpha = 0.2) +
     
     geom_point(data = all_data_save, aes(x = lon, y = lat), colour = "#FD7D60", size = 0.01) +
     
     theme(text=element_text(family = "Tw Cen MT"),
           axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank(),
           plot.title = element_text(hjust=0.5, size=30, face="bold",colour = "#FD7D60"),
           plot.caption = element_text(hjust=0.5, size = 9, color = "#73C7D9"),
           legend.position = "none",
           plot.subtitle = element_text(color = "#73C7D9", hjust = 0.5, size = 20),
           panel.grid.major = element_line(color = "#3a3a3a", size = 0.2, linetype = "dashed"),
           panel.grid.minor = element_line(color = "#3a3a3a", size = 0.2, linetype = "dashed"),
           plot.background = element_rect(fill = "#282828", color = NA), 
           panel.background = element_rect(fill = "#282828", color = NA), 
           legend.background = element_rect(fill = "#282828", color = NA),
           panel.border = element_blank()) +
     
     labs(caption = "Source: haspa-marathon-hamburg.de/ergebnisse") +
     labs(title = paste0("Hamburg Marathon 2018"), subtitle = "Time Lapse of all Runners") +
     
     geom_text(aes(x=10.0185, y=53.59607, label="Stadtpark"), size=3, colour = "#73C7D9", family = "Tw Cen MT") +
     geom_text(aes(x=10.00708, y=53.56442, label="Alster"), size=3, colour = "#73C7D9", family = "Tw Cen MT") +
     #geom_text(aes(x=9.92949, y=53.54209, label="Elbe"), size=3, colour = "#73C7D9", family = "Tw Cen MT") +
     geom_text(aes(x=9.96168, y=53.55407, label="St. Pauli"), size=3, colour = "#73C7D9", family = "Tw Cen MT") +
     geom_text(aes(x=9.91687, y=53.55224, label="Ottensen"), size=3, colour = "#73C7D9", family = "Tw Cen MT") +
     #geom_text(aes(x=9.88117, y=53.55101, label="Othmarschen"), size=3, colour = "#73C7D9", family = "Tw Cen MT") +
     geom_text(aes(x=10.01214, y=53.55305, label="St. Georg"), size=3, colour = "#73C7D9", family = "Tw Cen MT") +
     geom_text(aes(x=9.9876, y=53.56712, label="Rotherbaum"), size=3, colour = "#73C7D9", family = "Tw Cen MT") +
     geom_text(aes(x=10.01901, y=53.61022, label="Alsterdorf"), size=3, colour = "#73C7D9", family = "Tw Cen MT") +
     geom_text(aes(x=9.99099, y=53.5911, label="Eppendorf"), size=3, colour = "#73C7D9", family = "Tw Cen MT") +
     geom_text(aes(x=10.02322, y=53.57365, label="Uhlenhorst"), size=3, colour = "#73C7D9", family = "Tw Cen MT") +
     geom_text(aes(x=9.93558, y=53.59617, label=format(as.POSIXct(all_data_save$pasted_secs[1], origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")), size=20, colour = "#FD7D60", family = "Tw Cen MT") +
     coord_fixed(ratio = 1.5/1)
   
}

# function for saving images
plot.save = function(i=1){
  file_path = paste0("C:/Users/akruse/Documents/gif/marathon/", "/plot_",i ,".png")
  ggsave(filename=file_path, p.progress(i), dpi = 1000, width = 10, height = 8.97)
  
}

# save images
map(seq(1,15000, 100), plot.save)

# write gif
list.files(path = "C:/Users/akruse/Documents/gif/marathon", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("ndwi_aug_hgm.gif") # write to current dir
