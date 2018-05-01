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
require(httr)

# load more fonts for plot design (via extrafont package)
loadfonts(device = "win")

# scrape data from website for men
datalistall = list()
for (j in 1:16) {
  
  url <- paste("http://hamburg.r.mikatiming.de/2018/?page=",j,"&event=HML&event_main_group=custom.meeting.marathon&num_results=500&pid=list&search[sex]=M&search[age_class]=%25", sep = "")
  doc <- read_html(url)
  check = as.data.frame(html_attr(html_nodes(doc, "a"), "href"))
  colnames(check) = "url"
  check = filter(check, grepl("?content",url))
  check$url = paste("http://hamburg.r.mikatiming.de/2018/",check$url,sep = "")
  
  datalist = list()
  for (i in 1:length(check$url)) {
    
    url <- check$url[i]
    population <- url %>%
      html() %>%
      html_nodes(xpath='//*[@class="table table-condensed table-striped"]') %>%
      html_table()
    population <- population[[1]]
    population$id = paste(j,i,sep = "_")
    population$gender = "m"
    datalist[[i]] <- population
  }
  big_data = do.call(rbind, datalist)
  datalistall[[j]] <- big_data
}
runners_men = do.call(rbind, datalistall)

# scrape data from website for women
datalistall = list()
for (j in 1:5) {
  
  url <- paste("http://hamburg.r.mikatiming.de/2018/?page=",j,"&event=HML&event_main_group=custom.meeting.marathon&num_results=500&pid=list&search[sex]=W&search[age_class]=%25", sep = "")
  doc <- read_html(url)
  check = as.data.frame(html_attr(html_nodes(doc, "a"), "href"))
  colnames(check) = "url"
  check = filter(check, grepl("?content",url))
  check$url = paste("http://hamburg.r.mikatiming.de/2018/",check$url,sep = "")
  
  datalist = list()
  for (i in 1:length(check$url)) {
    
    url <- check$url[i]
    population <- url %>%
      html() %>%
      html_nodes(xpath='//*[@class="table table-condensed table-striped"]') %>%
      html_table()
    population <- population[[1]]
    population$id = paste(j,i,sep = "_")
    population$gender = "w"
    datalist[[i]] <- population
  }
  big_data = do.call(rbind, datalist)
  datalistall[[j]] <- big_data
}

# combine data
runners_women = do.call(rbind, datalistall)
runners = rbind(runners_men,runners_women)
runners$id = paste(runners$gender,runners$id,sep = "")
runners = select(runners, id, Split, Zeit)
runners$Split = gsub(" \\*","",runners$Split)
runners = dcast(runners, id ~ Split, value.var="Zeit")
rm(i,j,url,runners_men,runners_women, population,datalist,datalistall,big_data,check,doc)

# format data
runners$`5km` = chron(times = as.character(runners$`5km`))
runners$seconds5 = period_to_seconds(hms(runners$`5km`))

runners$`10km` = chron(times = as.character(runners$`10km`))
runners$seconds10 = period_to_seconds(hms(runners$`10km`))

runners$`15km` = chron(times = as.character(runners$`15km`))
runners$seconds15 = period_to_seconds(hms(runners$`15km`))

runners$`20km` = chron(times = as.character(runners$`20km`))
runners$seconds20 = period_to_seconds(hms(runners$`20km`))

runners$`25km` = chron(times = as.character(runners$`25km`))
runners$seconds25 = period_to_seconds(hms(runners$`25km`))

runners$`30km` = chron(times = as.character(runners$`30km`))
runners$seconds30 = period_to_seconds(hms(runners$`30km`))

runners$`35km` = chron(times = as.character(runners$`35km`))
runners$seconds35 = period_to_seconds(hms(runners$`35km`))

runners$`40km` = chron(times = as.character(runners$`40km`))
runners$seconds40 = period_to_seconds(hms(runners$`40km`))

runners$Finish = chron(times = as.character(runners$Finish))
runners$secondsFinish = period_to_seconds(hms(runners$Finish))

runners = runners[complete.cases(runners$seconds5), ]
runners = runners[complete.cases(runners$seconds10), ]
runners = runners[complete.cases(runners$seconds15), ]
runners = runners[complete.cases(runners$secondsHalb), ]
runners = runners[complete.cases(runners$seconds20), ]
runners = runners[complete.cases(runners$seconds25), ]
runners = runners[complete.cases(runners$seconds30), ]
runners = runners[complete.cases(runners$seconds35), ]
runners = runners[complete.cases(runners$seconds40), ]
runners = runners[complete.cases(runners$secondsFinish), ]

# select
runners = runners[12:ncol(runners)]

# import shp alster
setwd("C:/Users/Alex/Downloads/")
import <- ogrListLayers("C:/Users/Alex/Downloads/alster.kml")
alster = readOGR("alster.kml",import[1])
alster = fortify(alster)

# import shp alster
setwd("C:/Users/Alex/Downloads/")
import <- ogrListLayers("C:/Users/Alex/Downloads/alsteraus.kml")
alsteraus = readOGR("alsteraus.kml",import[1])
alsteraus = fortify(alsteraus)

# import shp stadtpark
setwd("C:/Users/Alex/Downloads/")
import <- ogrListLayers("C:/Users/Alex/Downloads/spark.kml")
spark = readOGR("spark.kml",import[1])
spark = fortify(spark)

# import shp route
setwd("C:/Users/Alex/Downloads/")
import <- ogrListLayers("C:/Users/Alex/Downloads/mlayer.kml")
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

# loop over all lines
them_res = list(res7,res8,res6,res1,res9)
datalistall = list()
its = 1
for (h in them_res) {
  
  # select dataframe
  res = h
  res = select(res, long, lat)
  print(nrow(res))
  
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
      if(res$dist[i] >= 10){
        
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
    if (max(res$dist, na.rm = T) < 10) break
  }
  
  # add cumsum and relative distance
  res$dist_cum = cumsum(res$dist)
  res$dist_max = res$dist_cum/max(res$dist_cum)
  
  # read in runners data and do some processing
  data = runners
  data = data[seq(its,nrow(data),5),]
  
  # get position of runners for different times
  datalist = list()
  for (i in seq(1,max(runners$secondsFinish),20)) {
    
    
    for (j in 1:nrow(data)) {
      
      data$nach60[j] = ifelse(i <= data$seconds5[j], ((i/data$seconds5[j])*(5/42.195)),
                              ifelse(i <= data$seconds10[j], (5/42.195)+(((i-data$seconds5[j])/(data$seconds10[j]-data$seconds5[j]))*(5/42.195)),
                                     ifelse(i <= data$seconds15[j], (10/42.195)+(((i-data$seconds10[j])/(data$seconds15[j]-data$seconds10[j]))*(5/42.195)),
                                            ifelse(i <= data$seconds20[j], (15/42.195)+(((i-data$seconds15[j])/(data$seconds20[j]-data$seconds15[j]))*(5/42.195)),
                                                   ifelse(i <= data$seconds25[j], (20/42.195)+(((i-data$seconds20[j])/(data$seconds25[j]-data$seconds20[j]))*(5/42.195)),
                                                          ifelse(i <= data$seconds30[j], (25/42.195)+(((i-data$seconds25[j])/(data$seconds30[j]-data$seconds25[j]))*(5/42.195)), 
                                                                 ifelse(i <= data$seconds35[j], (30/42.195)+(((i-data$seconds30[j])/(data$seconds35[j]-data$seconds30[j]))*(5/42.195)),    
                                                                        ifelse(i <= data$seconds40[j], (35/42.195)+(((i-data$seconds35[j])/(data$seconds40[j]-data$seconds35[j]))*(5/42.195)),         
                                                                               (40/42.195)+(((i-data$seconds40[j])/(data$secondsFinish[j]-data$seconds40[j]))*(2.195/42.195))))))))))              
      
      
      data$lon[j] = res$long[which.min(abs(res$dist_max - data$nach60[j]))]
      data$lat[j] = res$lat[which.min(abs(res$dist_max - data$nach60[j]))]
      data$pasted_secs = i
    }
    data = filter(data, nach60 <= 1)
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
    geom_polygon(data = spark, aes(x = long, y = lat), fill = "#89ff8a", color = NA) +
    geom_polygon(data = alster, aes(x = long, y = lat), fill = "#89b8ff", color = NA) +
    geom_polygon(data = alsteraus, aes(x = long, y = lat), fill = "#89b8ff", color = NA) +
    #geom_polygon(data = elbe, aes(x = long, y = lat), fill = "#3f3f3f", color = NA) +
    
    geom_path(data = res7, aes(x = long, y = lat), color = "#cecece", size = 3) +
    
    geom_point(data = all_data_save, aes(x = lon, y = lat), colour = "black", size = 0.5) +
    
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
    #geom_text(aes(x=9.92949, y=53.54209, label="Elbe"), size=3, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=9.96168, y=53.55407, label="St. Pauli"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=9.91687, y=53.55224, label="Ottensen"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    #geom_text(aes(x=9.88117, y=53.55101, label="Othmarschen"), size=3, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=10.02107, y=53.55244, label="St. Georg"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=9.97927, y=53.56865, label="Rotherbaum"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=10.02811, y=53.61267, label="Alsterdorf"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=9.99425, y=53.5911, label="Eppendorf"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=10.03026, y=53.57334, label="Uhlenhorst"), size=4, colour = "#2d2d2d", family = "Tw Cen MT") +
    geom_text(aes(x=9.93558, y=53.59617, label=paste0("Elapsed Time:\n", format(as.POSIXct(all_data_save$pasted_secs[1], origin = "1970-01-01", tz = "UTC"), "%H:%M"))), size=9, colour = "#2d2d2d", family = "Tw Cen MT") +
    coord_fixed(ratio = 1.5/1)
  
}

# function for saving images
plot.save = function(i=1){
  file_path = paste0("C:/Users/Alex/Documents/gif/marathon/", "/plot_",i ,".png")
  ggsave(filename=file_path, p.progress(i), dpi = 500, width = 7, height = 7)
  
}

# save images
map(seq(1,max(runners$secondsFinish),20), plot.save)
