library(tidyverse)
clus_df <- read_csv("https://raw.githubusercontent.com/vankesteren/dav_practicals/master/12_Unsupervised_learning_Clustering/data/clusterdata.csv")

#2
#Function for calculating Euclidean distance between 2 vectors
l2_dist <- function(x, y){
  distance_squared <- 0
  for (element in 1:length(x)){
    z <- x[element]-y[element]
    distance_squared <- distance_squared + z^2
  }
  distance <- sqrt(distance_squared)
  return(distance)
}

ggplot(data=clus_df) + geom_point(aes(x=x1, y=x2))

kmedians <- function(df, k){
  #randomly assign each row to a cluster
  df$cluster <- sample(1:k, 1)
  
  #calculate centroid for each cluster
  
}

