# import route
setwd("C:/Users/akruse/Downloads/")
import <- ogrListLayers("C:/Users/akruse/Downloads/mari.kml")
l2 = readOGR("mari.kml","Directions from KarolinenstraÃŸe 34-35, 20357 Hamburg, Germany to KarolinenstraÃŸe 35, 20357 Hamburg, Germany",require_geomType="wkbLineString")

# get coords from spatial file
res <- lapply(slot(l2, "lines"), function(x) lapply(slot(x, "Lines"),
                                                          function(y) slot(y, "coords")))
# format to dataframe
res = as.data.frame(res)

# loop to get distance between coords
datalist = list()
for (i in 1:nrow(res)) {
  
  datalist[[i+1]] <- distm(c(res$X1[i], res$X2[i]), c(res$X1[i+1], res$X2[i+1]), fun = distHaversine)

}
datalist[[1]] = 0
big_data = do.call(rbind, datalist)
big_data = big_data[1:length(big_data)-1]
res$dist = big_data

# add more coords for precision and interpolation
repeat{
for (i in 2:nrow(res)) {
  if(res$dist[i] >= 20){
    
    savory = as.data.frame(midPoint(c(res$X1[i],res$X2[i]),c(res$X1[i-1],res$X2[i-1]))[1])
    savory$lat = midPoint(c(res$X1[i],res$X2[i]),c(res$X1[i-1],res$X2[i-1]))[2]
    colnames(savory) = c("X1","X2")
    savory$dist = NA
    res <- rbind(res[1:i-1,],savory,res[-(1:i-1),])
    
    # calculate distance again with new coords
    datalist = list()
    for (i in 1:nrow(res)) {
      datalist[[i+1]] <- distm(c(res$X1[i], res$X2[i]), c(res$X1[i+1], res$X2[i+1]), fun = distHaversine)
    }
    datalist[[1]] = 0
    big_data = do.call(rbind, datalist)
    big_data = big_data[1:length(big_data)-1]
    res$dist = big_data
  }
}
  if (max(res$dist, na.rm = T) < 20) break
}

# add cumsum to dataframe
res$dist_cum = cumsum(res$dist)
res$dist_max = res$dist_cum/max(res$dist_cum)

# split route for offsetting
res1 = res[1:1180,]
res2 = res[1200:1335,]
res3 = res[1350:3214,]

rownames(res1) = 1:nrow(res1)
res1$ID = 1
res1$dist = NULL
res1 = dplyr::select(res1, X1, X2, ID)
colnames(res1) = c("x","y","id")

coordinates(res1) <- ~x+y
x <- lapply(split(res1, res1$id), function(x) Lines(list(Line(coordinates(x))), x$id[1L]))
lines <- SpatialLines(x)
data <- data.frame(id = unique(res1$id))
rownames(data) <- data$id
l <- SpatialLinesDataFrame(lines, data)

# offset line
res1_off = offset_line(l2, offset=c(.0005,0,-.0005,-.001))
res2 = double_line(l, width = .0005, sides = "right")

ggplot() +
  geom_path(data = df, aes(x = x, y = y)) +
  geom_path(data = res2, aes(x = X1, y = X2)) +
  geom_path(data = res3, aes(x = X1, y = X2))

  














# read csv
data = read.csv("Läufer_20180418_110452.csv")
data = data[sample(nrow(data), 100), ]
data$Brutto = chron(times = as.character(data$Brutto))
data$seconds = period_to_seconds(hms(data$Brutto))
data = select(data, Name, Brutto, seconds)
max_run = round(max(data$seconds)/60, 0)



datalist = list()
for (i in seq(1,15000, 10)) {

  data$nach60 = i/data$seconds
  for (j in 1:nrow(data)) {
    data$lon[j] = res$X1[which.min(abs(res$dist - data$nach60[j]))]
    data$lat[j] = res$X2[which.min(abs(res$dist - data$nach60[j]))]
    data$pasted_secs = i
  }
  datalist[[i]] <- data
}

big_data = do.call(rbind, datalist)



p = ggplot(res) +
  geom_path(aes(X1,X2), color = "grey") +
  geom_point(data = big_data, aes(x = lon, y = lat, frame = pasted_secs)) +
  theme_void()

gganimate(p,"puit.mp4", interval = 1/10, nmax = 100000000)


















require(rgeos)
require(sp)
require(rgdal)
require(tidyverse)

# import route
setwd("C:/Users/akruse/Downloads/")
import <- ogrListLayers("C:/Users/akruse/Downloads/mari.kml")
l2 = readOGR("mari.kml","Directions from KarolinenstraÃŸe 34-35, 20357 Hamburg, Germany to KarolinenstraÃŸe 35, 20357 Hamburg, Germany",require_geomType="wkbLineString")

# create polygon from lines with gbuffer
proj4string(l2) <- CRS("+proj=longlat")
l2.trans <- spTransform(l2, CRS("+proj=longlat"))
poly = gBuffer(l2.trans, width=.00025, quadsegs=5, capStyle="ROUND", joinStyle="ROUND", mitreLimit=0.01)

# extract outer lines from polygon
p.lines = fortify(poly)
p.lines.1 = p.lines[10:1380,]
p.lines.2 = p.lines[605:1240,]

# create polygon from lines with gbuffer
proj4string(l2) <- CRS("+proj=longlat")
l2.trans <- spTransform(l2, CRS("+proj=longlat"))
poly2 = gBuffer(l2.trans, width=.0005, quadsegs=5, capStyle="ROUND", joinStyle="ROUND", mitreLimit=0.01)

# extract outer lines from polygon
p.lines2 = fortify(poly2)
p.lines2.1 = p.lines2[5:650,]
p.lines2.2 = p.lines2[655:1390,]

ggplot() +
  geom_path(data = p.lines.1, aes(x = long, y = lat)) +
  #geom_path(data = p.lines.2, aes(x = long, y = lat)) +
  #geom_path(data = p.lines2.1, aes(x = long, y = lat)) +
  #geom_path(data = p.lines2.2, aes(x = long, y = lat)) +
  #geom_path(data = l2.fort, aes(x = long, y = lat)) +
  geom_point(data = p.lines, aes(x = long[2400], y = lat[2400]), color = "red")

