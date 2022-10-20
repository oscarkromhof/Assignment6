library(tidyverse)

clus_df <- read_csv("https://raw.githubusercontent.com/vankesteren/dav_practicals/master/12_Unsupervised_learning_Clustering/data/clusterdata.csv")

#Function for calculating Euclidean distance between 2 vectors
l2_dist <- function(x, y) {
  sqrt(sum((x - y)^2))
}

# Kmedians
kmedians <- function(X, K, L = 3) {
  
  # Checking some error before starting the function
  stopifnot(is.data.frame(X))    # X must be data frame
  stopifnot(is.integer(as.integer(K))) # K must be convertible to int  
  
  # Randomly assign X dataset to K temporary clusters
  n_row  <- nrow(X)
  select_vec <- rep(1:K, length.out = n_row)
  X_temp <- X %>% mutate(clusters = sample(select_vec))
  
  clus_vec <- rep(0, length.out = n_row) # vector for storing cluster number every points
  
  
  #Default iteration 5 times, You can change from input function
  for (l in 1:L) {
    
    # Calculate median every clusters
    med_centroid <- X_temp %>% group_by(clusters) %>% summarize_all(list(med = median))  
    
    # Decide cluster by calculate Euclidean Distance and finding nearest with median centroid
    for (i in 1:n_row) {
      
      vec <- rep(0, length.out = K) # vector for storing 3 euc distance between a point with K med centroid
      
      for (j in 1:K) {
        euc_dist <- l2_dist(med_centroid[j,2:3], X_temp[i,1:2])   # Calculate Euclidean Distance
        vec[j] <- euc_dist # Input distance to temporary vector
      }
      clust <- which.min(vec)
      clus_vec[i] <- clust 
    }
    
    #If there are no change in assignment, break iteration
    if (all(X_temp$clusters == clus_vec) == TRUE){
      break
    }
    
    # Update cluster from result of iteration before
    X_temp <- X %>% mutate(clusters = clus_vec )
    
  }
  
  return(clus_vec)
}

# Result Clustering with K=3. Default iteration L = 3
clustID <- kmedians(clus_df, 4)
clustID
# Moreover, also can change the number iteration.
# For example iteration 5 times, add input like this : kmedians(clus_df, 4, 5)

# Visualization result from clustering
clus2_df <- clus_df
clus2_df$cluster <- as.factor(clustID)
clus_plot <- 
  ggplot(clus2_df, aes(x = x1, y = x2, color = cluster)) +
  geom_point()

clus_plot
