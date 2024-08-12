#' Equal-size K-Means Clustering
#' 
#' @description `kMeansEqual` Assigns `k` cluster identifiers to a data frame `kdat` whereby each cluster is of (nearly) equal size.
#' @param kdat A data frame to which we want to add a column with cluster identifiers.
#' @param k The number of clusters. Each of the `k` clusters will contain at least `floor(nrow(kdat)/k)` elements.
#' @param columns The columns of `kdat` that will be used for clustering.
#' @param centers An optional matrix with `k` rows and 2 columns indicating the centers of each of the `k` clusters. If `NULL` (default), the cluster centers will be calculated with the `kmeans` function.
#' @param add.centers A boolean indicating wheter new cluster centers need to be calculated. 
#' If `TRUE`, an attribute named `"centers"`  will be added to the output that contains the data frame with the new cluster centers. 
#' This attribute can immediately be plugged into the `centers` argument if this function is to be executed iteratively. Defaults to `FALSE`.
#' @param assigned.to A character vector indicating the name of the column that will contain the cluster identifier. Defaults to `"assigned"`.
#' @param seed A numeric value that can be set to generate a seed to make the clustering procedure reproducible.
#' @examples
#' data(mtcars)
#' # Cluster the mtcars dataset on `mpg` and `wt`
#' res <- kMeansEqual(kdat = mtcars, columns = c("mpg", "wt"), k = 3, add.centers = TRUE)
#' 
#' # Get the data frame with the assigned clusters
#' res
#' # View the cluster centers
#' attr(res, "centers")
#' 
#' # Re-iterate the procedure with the new cluster centers
#' res2 <- kMeansEqual(kdat = mtcars, columns = c("mpg", "wt"), centers = attr(res, "centers"), k = 3, add.centers = TRUE)
#' # View the new cluster centers
#' attr(res2, "centers")
#' @export
kMeansEqual <- function(kdat, k, columns = NULL, centers = NULL, add.centers = FALSE, assigned.to = "assigned", seed = NULL){
  
  if(is.null(columns)){
    columns <- 1:ncol(kdat)
  } else if(all(is.character(columns))){
    columns <- which(colnames(kdat) %in% columns)
  }
  
  if(is.null(centers)){
    if(!is.null(seed)){
      set.seed(seed)
    }
    kdat[, columns] %>% kmeans(k) -> kclust
    centers <- kclust$centers
  } else{
    centers <- as.matrix(centers)
    if(ncol(centers) != ncol(kdat[, columns])){stop("The number of columns in data frame `centers` needs to be equal to the length of `columns` or `ncol(kdat)` (if `columns` is NULL).")}
    if(nrow(centers) != k){stop("Data frame `centers` needs to have exactly `k` rows.")}
  }
  
  distances <- as.data.frame(matrix(NA, nrow = nrow(kdat), ncol = nrow(centers)))
  colnames(distances) <- paste0("D", 1:nrow(centers))
  
  for(i in 1:nrow(centers)){
    distances[, paste0("D", i)] <- sqrt(rowSums((sweep(kdat[, columns], 2, centers[i,]))^2))
  }
  
  kdat[, assigned.to] <- 0
  distances$index <- 1:nrow(kdat)
  FirstRound <- nrow(kdat) - (nrow(kdat) %% k)
  
  j.vector <- sapply(1:FirstRound, function(z){
    z <- if(z %% k == 0) k else (z %% k)
    return(z)
  })
  
  for(j in j.vector){
    # Cluster counts can be off by 1 due to uneven multiples of k. 
    itemloc <- which.min(distances[kdat[, assigned.to] == 0, (paste0("D", j))])[1]
    kdat[which(kdat[, assigned.to] == 0)[itemloc], assigned.to] <- j
    ### The sorting hat says... GRYFFINDOR!!! 
  }
  
  working <- distances[kdat[, assigned.to] == 0,]
  
  if(nrow(working) != 0){
    for(i in 1:nrow(working)){
      # These leftover points get assigned to whoever's closest, without regard to k
      kdat[distances$index == working$index[i], assigned.to] <- which(working[i, (1:k)] == min(working[i, (1:k)]))
    }
  }
  
  if(add.centers){
    NewCenters <- data.frame(x = rep(NA, k),
                             y = rep(NA, k))
    
    for(i in 1:nrow(NewCenters)){
      NewCenters[i, ] <- (kdat %>% filter(get(assigned.to) == i) %>% 
                            `[`(columns) %>%
                            kmeans(1))$centers
    }
    
    # Give original column names to NewCenters data frame
    colnames(NewCenters) <- colnames(kdat)[columns]
  }
  
  # Only keep the original columns and the "assigned" column
  # kdat <- kdat[, c(columns, which(colnames(kdat) == assigned.to))]
  
  # Add an attribute "centers" in case we want to apply this function iteratively
  if(add.centers){
    attr(kdat, "centers") <- as.matrix(NewCenters)
  }
  
  return(kdat)
}
