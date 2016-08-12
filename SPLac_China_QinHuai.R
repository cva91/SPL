setwd("C:/Users/sgtpeppers/Dropbox/Statistical Programming Languages") # set working directory here
install.packages("readxl")
install.packages("maps")
install.packages("stargazer")

library(readxl)
library(maps)
library(stargazer)

# Loading and preparing data for our analysis
data              = read_excel("AAP_PM_database_May2014.xls", sheet = 2)
data_china        = data.frame(data[which(data[, 3] == "China"), ])
rm(data)
data_china        = data_china[, c(4, 5)]
names(data_china) = c("Name", "PM10")
# read in economic indicators
econ           = read_excel("economic_indicators.xls")
econ           = econ[-c(76, 89, 94), ]  # delete cities without entries
names(econ)[1] = "Name"

# merge
data_china     = merge(data_china, econ, by = "Name")
rm(econ)

# convert PM variables to numeric
data_china$PM10 = as.numeric(data_china$PM10)

# Only consider values for population of econ data
data_china$Population = NULL

# Converteing values for GDP in Euro
data_china$gdp = data_china$gdp * 0.136

# calculate pop density
data_china$popDens = data_china$pop/data_china$area * 10^6

# calculate size of secondary industry
data_china$indus  = data_china$secondaryInd * data_china$gdp
names(data_china) = c("Name", "PM10", "Area", "Population", "GDP", "PrimaryIndustry", "SecondaryIndustry", "TertiaryIndustry", "Unemployment", "FixedAssetInvestment", "TotalExportsImports", "Imports", "Exports", "SalesSocialConsumerGoods", "PopulationDensity", "TotalSecondaryIndustry")
# China Map with our selected cities get lon and lat out of map data
our_cities_map    = world.cities[which(is.element(world.cities$name, data_china$Name) & world.cities$country.etc == "China"),]

# merge with our data
our_cities        = our_cities_map[, c(1,4,5)]
rm(our_cities_map)
names(our_cities) = c("Name", "Latitude", "Longitude")
data_china        = merge(data_china, our_cities, by = "Name")
rm(our_cities)

png(filename = "SPLac_China_QinHuai1.png")
# Qin-Huai line
plot(data_china$Latitude, data_china$PM10, main = "PM10 pollution by Latitude", ylab = "PM10", xlab = "Latitude", cex.main=1.5, cex.axis=1.5, cex.lab = 1.5)
abline(v = 33)
text(32.5, 140, "Qin-Huai line", srt= 90)

# calculate means
meanSouth = mean(data_china$PM10[data_china$Latitude < 33])  #82.1
meanNorth = mean(data_china$PM10[data_china$Latitude > 33])  #95.2 --> they differ!

# draw means and legend
lines(x = c(0, 33), y  = c(meanSouth, meanSouth), lty = "dashed")
lines(x = c(33, 50), y = c(meanNorth, meanNorth), lty = "dashed")
legend("topleft", legend="means for south & north", col = "black", lty =2)

dev.off()

# ttest
t.test(data_china$PM10 ~ as.factor(data_china$Latitude > 33))

# run regressions
lm = lm(PM10 ~ log(GDP) + SecondaryIndustry + (Latitude > 33) + log(Imports) + Population, data = data_china)

summary(lm)

# create stargazer Latex-output
stargazer(lm, font.size = "small", no.space = T, intercept.bottom = T, single.row = T, keep.stat = c("n","adj.rsq"), digits = 2, align = T, report = "vc*")
