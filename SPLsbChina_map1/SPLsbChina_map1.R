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

# Assigning colours to the clusters

data_final$Colour = ifelse(is.na(data_final$Cluster) == 1, "grey", ifelse(data_final$Cluster == 1, "darkgreen", ifelse(data_final$Cluster == 2, "blue", ifelse(data_final$Cluster == 3, "red", ifelse(data_final$Cluster == 4, "magenta","brown")))))

attach(data_final)

head(data_final)

####### Map from Google-Maps without any additional information 

map_aux = get_map(location = "China", maptype = "roadmap", zoom = 4, source = "google")

map_raw = ggmap(map_aux, base_layer = ggplot(data_final))

print(map_raw)

####### Map with cities by clusters

map_cities = map_raw + geom_point(color = Colour, size = 1, aes(x = Longitude, y = Latitude))

map_cities = map_cities + labs(title = "Cities by Cluster", x = "Longitude", y = "Latitude")

print(map_cities)
