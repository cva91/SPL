##########################################
##########################################
##########################################
#Extending regression by adding a variable capturing distance
#to coal power stations

install.packages("readxl")
install.packages("ggmap")
install.packages("sp")
install.packages("stargazer")
install.packages("ggplot2")

library("readxl")

#library for spDists
library("sp")

#package for map
library("ggmap")
libary("ggplot2")

#package for latex
library("stargazer")

setwd("C:/Users/TD/Documents/Uni/Dropbox/Air Pollution")

#load data
stations = read_excel("coal.xls")
stations = stations[,c(1,4,5,7)]

#split string containing both longitude and latitude into two variables at blank space
long.lat        = as.data.frame(do.call(rbind, strsplit(stations$Coordinates, ' ')))
names(long.lat) = c("lat","long")

#Convert coordinates in degrees minutes seconds to decimal degrees

dms          = do.call(rbind, strsplit(as.character(long.lat$lat), ":"))
long.lat$lat = as.numeric(dms[,1]) + 
(as.numeric(dms[,2]) + as.numeric(dms[,3])/60)/60
rm(dms)
   
dms           = do.call(rbind, strsplit(as.character(long.lat$long), ":"))
long.lat$long = as.numeric(dms[,1]) +
(as.numeric(dms[,2]) + as.numeric(dms[,3])/60)/60
rm(dms)

#Load Chinese cities
data_china=read.csv("data_chinadist.csv")

#check for missing coordinates and remove rows
is.na(data_china$Latitude)
data_china=data_china[which(is.na(data_china$Latitude) == FALSE),]

#Convert into matrix format
cit  = as.matrix(cbind(data_china$Latitude,data_china$Longitude))
stat = as.matrix(long.lat)

#calculate spherical distances
dist = spDists(cit, stat, longlat=TRUE)
dist = round(dist, digits = 3)

data_china$Name  = as.character(data_china$Name)
stations$Station = as.character(stations$Station)

rownames(dist) = c(data_china$Name)
colnames(dist) = c(stations$Station)
dist

#save closest station for each city and its corresponding distance 
#in a variable

for (i in 1:102) {
    data_china[i,"closest"] = stations$Station[which.min(dist[i,])]
   data_china[i,"Distance"] = min(dist[i,])
}
dist
#apply Benjamins code to stations to see where they are

data_final = data_china[,c(2,3,18,19,20)]

stations = cbind(stations,long.lat)

map_aux = get_map(location = "China", maptype = "roadmap", zoom = 4, source = "google")
map_raw = ggmap(map_aux, base_layer = ggplot(data_final))

#map with cities
map_cities = map_raw + geom_point(color = "red", size = 1, aes(x = Longitude, y = Latitude))
map_cities = map_cities + labs(title = "Cities", x = "Longitude", y = "Latitude")
print(map_cities)

#map with cities and stations
map_citiesstations = map_cities + geom_point(data = stations, aes(x = long,y = lat), colour = "black", size = 1) + 
  labs(title = "Cities and coal stations", x = "Longitude", y = "Latitude")
print(map_citiesstations)

#now regression with distance variable
attach(data_china)
lm = lm(PM10 ~ log(GDP) + SecondaryIndustry + PopulationDensity + log(Imports) + (Latitude >33) + Distance, data = data_china)
summary(lm)

#latex 
stargazer(lm, font.size = "small", no.space = T, intercept.bottom = T, single.row = T, keep.stat = c("n","adj.rsq"), digits = 2, align = T, report = "vc*")
