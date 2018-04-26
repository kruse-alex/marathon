require(tidyverse)
require(lubridate)
require(chron)
require(ggplot2)
require(gganimate)

setwd("C:/Users/akruse/Downloads/")
data = read.csv("Läufer_20180425_080444.csv")
data$gender = "w"
data1 = read.csv("Läufer_20180425_080433.csv")
data1$gender = "m"
data = rbind(data,data1)
rm(data1)
data$Brutto = chron(times = as.character(data$Brutto))
data$seconds = period_to_seconds(hms(data$Brutto))
data = select(data, Name, Brutto, gender, seconds, Jahrgang)

# get position of runners for different times
datalist = list()
for (i in seq(1,24447,150)) {
  
  savory = filter(data, seconds >= i)
  savory$way_done = i/savory$seconds
  savory$way_done = savory$way_done*42.195
  savory$time_done = paste(i)
  
  savory = savory %>% group_by(time_done, Jahrgang, gender) %>% summarise(min = min(way_done),
                                                                 q25 = quantile(way_done, 0.25),
                                                                 mean = mean(way_done),
                                                                 q75 = quantile(way_done, 0.75),
                                                                 max = max(way_done))

  datalist[[i]] <- savory
}

big_data = do.call(rbind, datalist)

# plot
g.progress= function(i=1,maxi = 24447){
data = filter(big_data, time_done == i)
ggplot(data, aes(y = mean, x = Jahrgang, color = Jahrgang)) +
  geom_crossbar(aes(ymin = min, ymax = max), fill = "aquamarine1",  color = "aquamarine1", width = 0.5) +
  geom_crossbar(aes(ymin = q25, ymax = q75), fill = "aquamarine4",  color = "aquamarine4", width = 0.5) +
  geom_crossbar(aes(ymin = mean, ymax = mean), fill = "black", color = "black", width = 0.2) +
  coord_flip() +
  theme_minimal() +
  ylab("Km") +
  xlab("Jahrgang") +
  scale_y_continuous(limits = c(0,43)) +
  scale_x_continuous(limits = c(1935,1999)) +
  facet_wrap(~gender, ncol = 1) 
  
}


g.progress(8551)









# function for saving images
plot.save = function(i=1){
  file_path = paste0("C:/Users/akruse/Documents/gif/marathon", "/plot_",i ,".png")
  ggsave(filename=file_path, g.progress(i))
  
}

# save plot
map(seq(1,24447,150), plot.save)


