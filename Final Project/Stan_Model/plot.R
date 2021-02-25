rm(list=ls())
library(ggplot2)
library(ggmap)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

register_google("AIzaSyD1u3tQHKMT2iPHpYJ_NW6DLw-GN6InCyc")
baseMap <- get_map(location = c(lon = 121.542902,lat = 25.027317), language="en", maptype="roadmap", zoom = 14)
station <- read.csv("stationLocation.csv", header=T)
netReturn <- read.csv("each_station_NetReturn_prediction_by_hour.csv", header=T)

for (i in 1:24) {
  data = subset(netReturn, Hour == i)
  if (i < 10) {
    filename = paste("net_return_0", as.character(i), ".png", sep="")
  } else {
    filename = paste("net_return_", as.character(i), ".png", sep="")
  }
  plot <- ggmap(baseMap)
  data$lon <- station$lon
  data$lat <- station$lat
  data$color <- ifelse(data$mu >= 0, "positive", "negative")
  data$size <- abs(data$mu)
  
  ggmap(baseMap) + 
    geom_point(aes(x = lon, y = lat, color = color, size=size), data = data) + 
    geom_text(aes(x = lon + 0.0002, y = lat + 0.0008, label=id, vjust=0, hjust=0.5), data=station, size=1) + 
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
    ggtitle(paste("Hour", as.character(i)))
  ggsave(filename, width=6, height=6, dpi=600)
}
