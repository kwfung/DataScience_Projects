## read file 
happy = read.csv("SimpleData.csv")
View(happy)
summary(happy)

##data wrangling 
happy_clean <- as.data.frame(happy)

#remove NA and fill it in the means of the Country  
'happy_clean[is.na(happy_clean)] <- mean(happy_clean, na.rm= TRUE)'
happy_clean = happy_clean %>% group_by(Country.name) %>%
  mutate_each(funs(replace(., which(is.na(.)),
                           mean(., na.rm=TRUE))))
sum(is.na(happy_clean))
##there are still some NAs , fill those with mean of the full column (mean of all count)
library(zoo)
na.aggregate(happy_clean[,4:20])
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
#replace(happy_clean[,4:20], TRUE, lapply(happy_clean[,4:20], NA2mean))
happy_clean2 <- lapply(happy_clean, NA2mean)
happy_clean2<- as.data.frame(happy_clean2)

sum(is.na(happy_clean2))

#group by each country and mean of columns 
library(dplyr)
happy_group= happy_clean2 %>% group_by(Country.name) %>% summarise_all(funs(mean))

#remove year and row number
happy_group = happy_group[,-c(2,3)]

##preprocessing 
library(caret)
pp = preProcess(happy_group, method = c("center", "scale"))
happy.scaled = predict(pp, happy_group)
head(happy.scaled)

##data wrangling: change country name to row name
'happy.scaled2 <- happy.scaled[,-1]
rownames(happy.scaled2) <- happy.scaled[,1]
Countrynames = read.csv("SimpleData - Copy.csv")
Countrynames = Countrynames[1]'

happy.scaled2<- column_to_rownames(as.data.frame(happy.scaled), var="Country.name")
rownames(happy.scaled2)

##clustering
## kmeans cannot handle NA so have to omit NA for now (or use means of the column)
set.seed(144)
km <- kmeans(happy.scaled2, iter.max=100, 4)
names(km)
km

km$centers
km.clusters = km$cluster
km$tot.withinss
km$size

##visualize
library(cluster)
install.packages("fpc")
library(fpc)

plotcluster(happy.scaled2, km.clusters)

clusplot(happy.scaled2, km.clusters, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

###more visualization 
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

fviz_cluster(km, data = happy.scaled2)

# Scree plot for k-means
#######################################

# For k means, we literally try many value of k and look at their dissimilarity.
# here we test all k from 1 to 100
k.data <- data.frame(k = 1:100)
k.data$SS <- sapply(k.data$k, function(k) {
  kmeans(happy.scaled[,-1], iter.max=100, k)$tot.withinss
})

# Plot the scree plot.
plot(k.data$k, k.data$SS, type="l")
# zoom in
plot(k.data$k, k.data$SS, type="l", xlim=c(0, 40))

### analyzing each cluster group from happy.scaled2 
clusters_mean =    
  happy.scaled2 %>%
      mutate(Cluster = km.clusters) %>%
      group_by(Cluster) %>%
      summarise_all("mean")

