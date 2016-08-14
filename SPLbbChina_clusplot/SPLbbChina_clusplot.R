# Load and prepare data for our analysis
library(readxl)
library(maps)
setwd('C:/Users/Björn/Documents/Uni/MasterSem2/StatisticalProgramming/AirPolution') #Please enter working directory
data = read_excel("AAP_PM_database_May2014.xls", sheet = 2)
data_china = data.frame(data[which(data[, 3]=='China'),])
rm(data)
data_china = data_china[, c(4,5)]
names(data_china) = c('Name', 'PM10')

# Read in economic indicators
econ           = read_excel("economic_indicators.xls")
econ           = econ[-c(76,89,94),] #Delete cities without entries
names(econ)[1] = "Name"

# Merge
data_china = merge(data_china,econ,by='Name')
rm(econ)

# Convert PM variables to numeric
data_china$PM10 = as.numeric(data_china$PM10)

# Only consider values for population of econ data
data_china$Population = NULL

# Converte values for GDP in Euro
data_china$gdp = data_china$gdp * 0.136

# Calculate pop density
data_china$popDens = data_china$pop/data_china$area * 10^{6} 

# Calculate size of secondary industry
data_china$indus  = data_china$secondaryInd * data_china$gdp
names(data_china) = c('Name', 'PM10', 'Area', 'Population', 'GDP', 'PrimaryIndustry', 'SecondaryIndustry'
                    , 'TertiaryIndustry', 'Unemployment', 'FixedAssetInvestment', 'TotalExportsImports',
                    'Imports', 'Exports', 'SalesSocialConsumerGoods', 'PopulationDensity', 'TotalSecondaryIndustry')

# China Map with our selected cities get lon and lat out of map data
our_cities_map = world.cities[which(is.element(world.cities$name, data_china$Name) & world.cities$country.etc == "China"),]

# Delete small cities with the same name as big cities
our_cities_map = our_cities_map[-which(duplicated(our_cities_map$name)),] 

# Merge with our data
our_cities = our_cities_map[, c(1,4,5)]
rm(our_cities_map)
names(our_cities) = c("Name", "Latitude", "Longitude")



data_china = merge(data_china, our_cities, by = "Name", all=TRUE)
rm(our_cities)


######################################################################################
######################################################################################
######################################################################################
#Hierachical Cluster Analysis according to economic factors

library(cluster)

# Preperation for Cluster Analysis

# 1) Select Variables for our data frame
attach(data_china)
gdp_percap          = GDP / (Population * 10^{6}) * 10^{9}
data_cluster        = cbind.data.frame(Name, PopulationDensity, gdp_percap, SecondaryIndustry,
                              Unemployment, FixedAssetInvestment, SalesSocialConsumerGoods)
names(data_cluster) = c('Name', 'PopulationDensity', 'GDPperCapita', 'SecondaryIndustry',
                      'Unemployment', 'FixedAssetInvestment', 'SalesSocialConsumerGoods')
detach(data_china)

# 2) Search for outliers

attach(data_cluster)
# Get upper and lower bound 
pop_ex_min     = boxplot.stats(PopulationDensity)$stats[1] 
pop_ex_max     = boxplot.stats(PopulationDensity)$stats[5]
sec_ind_ex_min = boxplot.stats(SecondaryIndustry)$stats[1] * 4 / 5
sec_ind_ex_max = boxplot.stats(SecondaryIndustry)$stats[5] * 3 / 2
outlier_cities = which(PopulationDensity<pop_ex_min |PopulationDensity>pop_ex_max
                     |SecondaryIndustry<sec_ind_ex_min|SecondaryIndustry>sec_ind_ex_max)

# Delete outliers
data_cluster=data_cluster[-outlier_cities,]
detach(data_cluster)

# 3) Center Variables
cent_var = apply(X = as.matrix(data_cluster[, 2:7]), MARGIN = 2, FUN = scale)

# 4) Generate modified data frame for Cluster Analysis 
data_cluster        = cbind.data.frame(data_cluster$Name, cent_var)
data_cluster        = data_cluster[, c("data_cluster$Name", "PopulationDensity", "SecondaryIndustry")]
names(data_cluster) = c('Name', "PopulationDensity", "SecondaryIndustry")

######################################################################################
# Perform Hierachical Cluster Analysis using Euclidian Distance and 
# Ward Fusion Algorithm

# Generate Distance Matrix using Euclidean Metric
dist_mat = dist(data_cluster[, c(2,3)])

# Perform clustering algorithm
cluster_tree = hclust(dist_mat)

# Choose number of clusters
num_clus = 4
Cluster  = cutree(cluster_tree, k = num_clus)

# Add Cluster Information to data
data_cluster = cbind(data_cluster, Cluster)

# Visualize Clustering of Data
colors         = c('darkgreen','blue','red','magenta')
colors_cluster = colors[data_cluster$Cluster]
clusplot(x = cbind(-data_cluster[, 2], data_cluster[, 3]), clus = data_cluster[, 4]
         ,color = TRUE, lines = 0, labels = 4, plotchar = FALSE, 
         main = 'Visualization of the Clusters'
         , xlab = 'Population Density', ylab = 'Secondary Industry', sub = NULL,
         col.p=colors_cluster)

######################################################################################
# Remove all help variables
rm(gdp_percap, cent_var, Cluster, dist_mat, data_cluster, cluster_tree,
  num_clus,outlier_cities, pop_ex_max,pop_ex_min, sec_ind_ex_max,
  sec_ind_ex_min, data_china, colors_cluster, colors)











