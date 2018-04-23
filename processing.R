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

# import route
setwd("C:/Users/akruse/Downloads/")
import <- ogrListLayers("C:/Users/akruse/Downloads/testii.kml")
l2 = readOGR("testii.kml",import[1],require_geomType="wkbLineString")


# import finish line
setwd("C:/Users/akruse/Downloads/")
import <- ogrListLayers("C:/Users/akruse/Downloads/testoo.kml")
l3 = readOGR("testoo.kml",import[1],require_geomType="wkbLineString")
l3 = double_line(l3, width = .0003, sides = "left")


# combine routes
l2 = rbind(l2, l3)

start = 53.55843, 9.97466 53.55854, 9.97558
end = 53.5587, 9.97537

# create finish line
l1 <- cbind(c(9.97466, 9.97558), c(53.55843, 53.55854))
Sl1 <- Line(l1)
S1 <- Lines(list(Sl1), ID = "a")
Sl <- SpatialLines(list(S1))
df <- data.frame(len = sapply(1:length(Sl), function(i) gLength(Sl[i, ])))
rownames(df) <- sapply(1:length(Sl), function(i) Sl@lines[[i]]@ID)
Sldf <- SpatialLinesDataFrame(Sl, data = df)
oyoy =aggregate(rbind(Sldf,l2))

# offset lines
res2 = double_line(l2, width = .00003, sides = "left")
res3 = double_line(l2, width = .00004, sides = "left")
res4 = double_line(l2, width = .00005, sides = "left")
res5 = double_line(l2, width = .00006, sides = "left")
res6 = double_line(l2, width = .00007, sides = "left")
res7 = double_line(l2, width = .00008, sides = "left")
res8 = double_line(l2, width = .00009, sides = "left")
res9 = double_line(l2, width = .00010, sides = "left")

# convert offset lines to dataframe
res1 = fortify(l2)
res2 = fortify(res2)
res3 = fortify(res3)
res4 = fortify(res4)
res5 = fortify(res5)
res6 = fortify(res6)
res7 = fortify(res7)
res8 = fortify(res8)
res9 = fortify(res9)

# loop over all offset line
them_res = list(res1,res2,res3,res4,res5,res6,res7,res8,res9)
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
data = read.csv("Läufer_20180418_110452.csv")
data = data[seq(its,nrow(data),9),]
data$Brutto = chron(times = as.character(data$Brutto))
data$seconds = period_to_seconds(hms(data$Brutto))
data = select(data, Name, Brutto, seconds)
max_run = round(max(data$seconds)/60, 0)

# position of runners for every 100th second
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
datalistall[[i]] <- big_data
its = its+1
}
all_data = do.call(rbind, datalistall)





ggplotly(ggplot() +
           geom_path(data = res1, aes(x = long, y = lat), size = 3) +
           geom_path(data = res1, aes(x = long, y = lat, colour = "red"), size = 0.3) +
           geom_path(data = res2, aes(x = long, y = lat, colour = "red"), size = 0.3) +
           geom_path(data = res3, aes(x = long, y = lat, colour = "red"), size = 0.3) +
           geom_path(data = res4, aes(x = long, y = lat, colour = "red"), size = 0.3) +
           geom_path(data = res6, aes(x = long, y = lat, colour = "red"), size = 0.3) +
           geom_path(data = res7, aes(x = long, y = lat, colour = "red"), size = 0.3) +
           geom_path(data = res8, aes(x = long, y = lat, colour = "red"), size = 0.3) +
           geom_path(data = res9, aes(x = long, y = lat, colour = "red"), size = 0.3))








p = ggplot(res) +
  geom_path(aes(long,lat), color = "black", size = 3) +
  geom_point(data = all_data, aes(x = lon, y = lat, frame = pasted_secs), colour = "red", size = 0.2) +
  theme_void()

gganimate(p,"puit.mp4", interval = 1/10, nmax = 100000000)
