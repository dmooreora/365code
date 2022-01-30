################### ISM 645/IAF 601 Predictive Analytics #####################
#=============================================================================
#======================== Week 7. Cluster Analysis ===========================
#=============================================================================

############################ Hands-on Exercise ###############################

library(tidyverse)


## (1) Import mtcars.csv and inspect the data

my_mtcars <- read.csv("mtcars.csv")

str(my_mtcars)


## (2) Cluster the vehicles in the dataset (using either K-means or hierarchical clustering). 
##     You determine the number of clusters and the variables to be used for clustering.

# K-means clustering

summary(my_mtcars)

my_mtcars <- my_mtcars %>% 
  drop_na()

kmeans_result <- kmeans(my_mtcars[ , c(-1, -2)], centers = 4, iter.max = 25, nstart = 10)

kmeans_result$cluster

my_mtcars <- my_mtcars %>% 
  mutate(cluster_kmeans = kmeans_result$cluster)

str(my_mtcars)


# Hierarchical clustering

d_matrix <- dist(my_mtcars[ , c(-1, -2)], method = "euclidean")

hc_result <- hclust(d_matrix, method = "single")

plot(hc_result)

hc_cluster <- cutree(hc_result, k = 4)

hc_cluster

my_mtcars <- my_mtcars %>% 
  mutate(cluster_hc = hc_cluster)

str(my_mtcars)


## (3) Summarize the characteristics of each cluster.

my_mtcars %>% 
  group_by(cluster_kmeans) %>% 
  summarize(num_obs = n(), avg_mpg =mean(mpg), avg_hp=mean(hp))


## (4) Create a scatter plot with mpg on the x axis and hp on the y axis. 
##     Color the observations based on the clusters revealed in the previous question.

library(ggrepel)

ggplot(my_mtcars, aes(x=mpg, y=hp)) +
  geom_point(aes(color = as.factor(cluster_kmeans))) +
  labs(title = "K-Means Clustering", color = "Cluster") +
  geom_text_repel(aes(label = model), force = 5)




### Determining the optimal number of clusters

library(factoextra)

fviz_nbclust(my_mtcars[ , c(-1, -2)], kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")

fviz_nbclust(my_mtcars[ , c(-1, -2)], kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette Method")


kmeans_result2 <- kmeans(my_mtcars[ , c(-1, -2)], centers = 2, iter.max = 25, nstart = 10)

my_mtcars <- my_mtcars %>% 
  mutate(cluster_kmeans2 = kmeans_result2$cluster)

ggplot(my_mtcars, aes(x=mpg, y=hp)) +
  geom_point(aes(color = as.factor(cluster_kmeans2))) +
  labs(title = "K-Means Clustering", color = "Cluster") +
  geom_text_repel(aes(label = model), force = 5)
