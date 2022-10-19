library(tidyverse)
clus_df <- read_csv("https://raw.githubusercontent.com/vankesteren/dav_practicals/master/12_Unsupervised_learning_Clustering/data/clusterdata.csv")

#Function for calculating Euclidean distance between 2 vectors
l2_dist <- function(x, y) {
  sqrt(sum((x - y)^2))
}

# Kmedians
kmedians <- function(X, K) {
  
  # Checking some error before starting the function
  stopifnot(is.data.frame(X))    # X must be data frame
  stopifnot(is.integer(as.integer(K))) # K must be convertible to int  
  
  # Randomly assign X dataset to K temporary clusters
  n_row  <- nrow(X)
  select_vec <- rep(1:K, length.out = n_row)
  X_temp <- X %>% mutate(clusters = sample(select_vec))
  
  # Calculate median every random clusters
  med_centroid <- X_temp %>% group_by(clusters) %>% summarize_all(funs(median))  
  
  clus_vec <- c() # vector for storing cluster number every points
  
  # Decide cluster by calculate Euclidean Distance and finding nearest with centroid
  for (i in 1:n_row) {
    
    vec <- c() # vector for storing 3 euc distance between a point with 3 med centroid
    for (j in 1:K) {
      euc_dist <- l2_dist(med_centroid[j,2:3], X[i,])   # Calculate Euclidean Distance
      vec <- append(vec, euc_dist) # Input distance to temporary vector
    }
    clust <- which.min(vec)
    clus_vec <- append(clus_vec, clust) 
  }

  return(clus_vec)
}

# Result Clustering with K=4
kmedians(clus_df, 4)
