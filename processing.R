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

# import alster
setwd("C:/Users/Alex/Downloads/")
import <- ogrListLayers("C:/Users/Alex/Downloads/alster.kml")
alster = readOGR("alster.kml",import[1])
alster = fortify(alster)

# import alster
setwd("C:/Users/Alex/Downloads/")
import <- ogrListLayers("C:/Users/Alex/Downloads/alsteraus.kml")
alsteraus = readOGR("alsteraus.kml",import[1])
alsteraus = fortify(alsteraus)

# import stadtpark
setwd("C:/Users/Alex/Downloads/")
import <- ogrListLayers("C:/Users/Alex/Downloads/spark.kml")
spark = readOGR("spark.kml",import[1])
spark = fortify(spark)

# import route
setwd("C:/Users/Alex/Downloads/")
import <- ogrListLayers("C:/Users/Alex/Downloads/mlayer.kml")
l2 = readOGR("mlayer.kml",import[1],require_geomType="wkbLineString")

# offset lines
res6 = double_line(l2, width = .0004, sides = "right")
res7 = double_line(l2, width = .0008, sides = "right")
res8 = double_line(l2, width = .0012, sides = "right")
res9 = double_line(l2, width = .0016, sides = "right")

# convert offset lines to dataframe
res1 = fortify(l2)
res6 = fortify(res6)
res7 = fortify(res7)
res8 = fortify(res8)
res9 = fortify(res9)

# sort dataframes
res1$seq = seq(1:nrow(res1))
res1 = res1[order(res1$seq, decreasing = TRUE), ]
res1$seq = NULL

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
  
  # select offset line dataframe
  res = h
  res = select(res, long, lat)
  
  # loop to get distance between coords
  datalist = list()
  for (i in 1:nrow(res)) {
    
    datalist[[i+1]] <- distm(c(res$long[i], res$lat[i]), c(res$long[i+1], res$lat[i+1]), fun = distHaversine)
    
  }
  datalist[[1]] = 0
  big_data = do.call(rbind, datalist)
  big_data = big_data[1:length(big_data)-1]
  res$dist = big_data
  
  # add more coords for precision and interpolation
  repeat{
    for (i in 2:nrow(res)) {
      if(res$dist[i] >= 500){
        
        savory = as.data.frame(midPoint(c(res$long[i],res$lat[i]),c(res$long[i-1],res$lat[i-1]))[1])
        savory$lat = midPoint(c(res$long[i],res$lat[i]),c(res$long[i-1],res$lat[i-1]))[2]
        colnames(savory) = c("long","lat")
        savory$dist = NA
        res <- rbind(res[1:i-1,],savory,res[-(1:i-1),])
        
        # calculate distance again with new coords
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
  
  # add cumsum to dataframe
  res$dist_cum = cumsum(res$dist)
  res$dist_max = res$dist_cum/max(res$dist_cum)
  
  # read in runners data
  data = read.csv("Läufer_20180424_160440.csv")
  data1 = read.csv("Läufer_20180424_160450.csv")
  data = rbind(data,data1)
  data = data[seq(its,nrow(data),5),]
  data$Brutto = chron(times = as.character(data$Brutto))
  data$seconds = period_to_seconds(hms(data$Brutto))
  data = select(data, Name, Brutto, seconds)
  max_run = round(max(data$seconds)/60, 0)
  
  # position of runners for every 100th second
  datalist = list()
  for (i in seq(1,15000, 1000)) {
    
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

p.progress = function(i= 1, maxi = max(all_data$pasted_secs)){
all_data_save = filter(all_data, pasted_secs == i)
ggplot() +
  geom_polygon(data = spark, aes(x = long, y = lat), fill = "#b1e2aa", color = NA) +
  geom_polygon(data = alster, aes(x = long, y = lat), fill = "#47b2ff", color = NA) +
  geom_polygon(data = alsteraus, aes(x = long, y = lat), fill = "#47b2ff", color = NA) +
  geom_path(data = res7, aes(x = long, y = lat), color = "#e5e5e5", size = 6) +
  geom_point(data = all_data_save, aes(x = lon, y = lat), colour = "red", size = 0.05) +
  theme(text=element_text(family = "Tw Cen MT"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust=0.5, size=18, face="bold",colour = "#4e4d47"),
        plot.caption = element_text(hjust=0.5, size = 8, color = "#4e4d47"),
        legend.position = "none",
        plot.subtitle = element_text(color = "red", hjust = 0.5),
        panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border = element_blank()) +
  labs(caption = "Source: haspa-marathon-hamburg.de") +
  labs(title = paste0("Haspa Marathon Hamburg 2018"), subtitle = "Time Lapse of all Runners") +
  geom_text(aes(x=10.01699, y=53.59601, label="Stadtpark"), size=2, colour = "white", family = "Tw Cen MT") +
  geom_text(aes(x=10.00708, y=53.56442, label="Alster"), size=2, colour = "white", family = "Tw Cen MT")
}

# use plot_grid to combine map and barplot
plotf = function(i=1){plot_grid(g.progress(i),p.progress(i), rel_heights=c(6,2),ncol=1)}

# function for saving images
plot.save = function(i=1){
  file_path = paste0("C:/Users/Alex/Documents/gif/", "/plot_",i ,".png")
  ggsave(filename=file_path, p.progress(i), dpi = 600, width = 4.75, height = 7)
  
}

# save images
map(seq(1,15000, 1000), plot.save)















ggplotly(ggplot() +
           geom_path(data = res7, aes(x = long, y = lat), size = 3) +
           geom_path(data = res1, aes(x = long, y = lat, colour = "red"), size = 0.3) +
           geom_path(data = res6, aes(x = long, y = lat, colour = "red"), size = 0.3) +
           geom_path(data = res7, aes(x = long, y = lat, colour = "red"), size = 0.3) +
           geom_path(data = res8, aes(x = long, y = lat, colour = "red"), size = 0.3) +
           geom_path(data = res9, aes(x = long, y = lat, colour = "red"), size = 0.3))


