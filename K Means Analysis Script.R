library(ggplot2)
library(dplyr)
library(gather)
library(tidyr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
library(gridExtra)

#read in data
suicide_data <- read.csv(file = "/Users/mattdolan/Documents/DatasetsR/K Means Analysis/Suicide Data.csv", stringsAsFactors = F)
head(suicide_data)
dim(suicide_data)

#plotting missing value
options(repr.plot.width=6, repr.plot.height=6)
missing_data <- suicide_data %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +xlab('variables')+
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+coord_flip()+ theme_bw()

#removing hdi for year due to large number of missing values
suicide_data <- subset(suicide_data, select = -c(HDI.for.year))
head(suicide_data)

#removing categorical columns
suicide_data_cleaned <- subset(suicide_data, select = -c(year, sex, country, age, generation, country.year, gdp_for_year....))
head(suicide_data)

#scaling data for k means algorithm 
suicide_data_cleaned_scaled <- scale(suicide_data_cleaned)
head(suicide_data_cleaned_scaled)

#computes Euclidean distance matrix between the rows of a data matrix.
distance <- get_dist(suicide_data_cleaned_scaled)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

##trying out initial plot to see how data lies 
k2 <- kmeans(suicide_data_cleaned_scaled, centers = 2, nstart = 25)

#analysing statistical output of initial cluster configuration
k2

#visualising clusters from initial model 
fviz_cluster(k2, data = suicide_data_cleaned_scaled)

###trying out different number of cluster combinations
k3 <- kmeans(suicide_data_cleaned_scaled, centers = 3, nstart = 25)
k4 <- kmeans(suicide_data_cleaned_scaled, centers = 4, nstart = 25)
k5 <- kmeans(suicide_data_cleaned_scaled, centers = 5, nstart = 25)

#plotting different clustering configurations for comparison
p1 <- fviz_cluster(k2, geom = "point", data = suicide_data_cleaned_scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = suicide_data_cleaned_scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = suicide_data_cleaned_scaled) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = suicide_data_cleaned_scaled) + ggtitle("k = 5")
grid.arrange(p1, p2, p3, p4, nrow = 2)

#utilising the elbow method to identify the optimum number of clusters, after inconclusive analysis 
#from spotchecking cluster configurations visually
set.seed(123)
fviz_nbclust(suicide_data_cleaned_scaled, kmeans, method = "wss")

#elbow method suggests 4 clusters is the optimum
set.seed(123)
final <- kmeans(suicide_data_cleaned_scaled, 4, nstart = 25)
print(final)

#visualising results again
fviz_cluster(final, data = suicide_data_cleaned_scaled)

#producing descriptive statistics of clustering analysis - shows the mean for each variable in our scope of analysis, for each cluster
suicide_data_cleaned %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

