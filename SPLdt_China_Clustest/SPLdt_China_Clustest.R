#########################################################
#########################################################
#############Testing of clusters#########################
#############by Thomas Dengler###########################

install.packages("car")
install.packages("gplots")


#for plotmeans
library("gplots")

#for Leventest and ANOVA
library("car")

setwd("C:/Users/TD/backup")
data_china = read.csv("data_chinatest.csv")

attach(data_china)

#remove NAs
data_china = data_china[is.na(data_china$Cluster) == "FALSE",]

data_china$Cluster = as.factor(data_china$Cluster)

#meansplot
plotmeans(PM10 ~ Cluster, p = 0.95, bars = T, xlab = "Cluster", 
          ylab = "PM10 pollution (bars indicate 95% CI)", 
          main = "Error bar diagram",
       connect = FALSE)

#normality?
tapply(data_china$PM10, data_china$Cluster, shapiro.test)

#reject for cluster 2!
plot(density(PM10[Cluster == "2"], na.rm = TRUE))

#variances equal?
leveneTest(data_china$PM10, data_china$Cluster)

leveneTest(PM10, Cluster)
##Cannot reject at 5%. 


#nonparametric alternative, very conservative

mediantest  = function(x, z){
     median = median(x, na.rm = T)
     above  = (x > median)
mediantable = table(above, z)
  print(mediantable)
  chisq.test(mediantable)
  
}

mediantest(PM10, Cluster)

#do anova anyway
?anova
anova = aov(PM10 ~ Cluster)
summary(anova)

#post-hoc test
print(pairwise.t.test(PM10, Cluster, p.adjust.method = "bonferroni", pool.sd = TRUE))
print(pairwise.t.test(PM10, Cluster, p.adjust.method = "none"))

