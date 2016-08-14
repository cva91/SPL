####### Clearing history and setting working directory

rm(list=ls(all=TRUE))

graphics.off()

# setwd("~/R/SPL")

####### Installing and loading required packages

install.packages("ggmap")

library(ggmap)

####### Reading and transforming dataset 

data_china = read.csv("data_china.csv")

data_final = data_china[,c(2,3,18,19,20)]

attach(data_final)


####### Map from Google-Maps without any additional information 

map_aux = get_map(location = "China", maptype = "roadmap", zoom = 4, source = "google")

map_raw = ggmap(map_aux, base_layer = ggplot(data_final))

print(map_raw)

####### Bubbles of the cities scaled by PM10

map_pm10 = map_raw + geom_point(aes(x = Longitude, y = Latitude, size = PM10), color = "red", alpha = 0.4)

map_pm10 = map_pm10 + labs(title = "Air Pollution in Chinese Cities", x = "Longitude", y = "Latitude", size = "PM10 ug/m3")

print(map_pm10)

####### Shanghai area marked with a blue rectangle

# size of the box: [longitude: 116-124, latitude: 28-34]

rect = data.frame(xmin = 116, xmax = 124, ymin = 28, ymax = 34)

map_rect = map_pm10 + geom_rect(data = rect, aes(xmin = 116, xmax = 124, ymin = 28, ymax = 34), color = "gray20", alpha = 0.4, inherit.aes = FALSE)

print(map_rect)
