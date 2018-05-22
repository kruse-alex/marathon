# load packages
require(tidyverse)
require(httr)

# scraping results of men from website
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

# scraping results of women from website
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
runners_women = do.call(rbind, datalistall)

# combine data of women and men
runners = rbind(runners_men,runners_women)

# create unique runners id
runners$id = paste(runners$gender,runners$id,sep = "")

# format data
runners = select(runners, id, Split, Zeit)
runners$Split = gsub(" \\*","",runners$Split)
runners = dcast(runners, id ~ Split, value.var="Zeit")

# remove useles things
rm(i,j,url,runners_men,runners_women, population,datalist,datalistall,big_data,check,doc)

# format character to timestamp
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

# remove rows with NAs
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

# remove useles columns
runners = runners[12:ncol(runners)]