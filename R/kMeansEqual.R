#' Equal-size kmeans
#' 
#' @description `kMeansEqual` Assigns `k` cluster identifiers to a data frame `kdat` whereby each cluster is of (nearly) equal size.
#' @param kdat A data frame with 2 columns.
#' @param k The number of clusters. Each of the `k` clusters will contain at least `floor(nrow(kdat)/k)` elements.
#' @param centers An optional data frame with `k` rows and 2 columns indicating the centers of each of the `k` clusters. If `NULL` (default), the cluster centers will be calculated with the `kmeans` function.
#' @param add.centers A boolean indicating wheter new cluster centers need to be calculated. 
#' If `TRUE`, an attribute named `"centers"`  will be added to the output that contains the data frame with the new cluster centers. 
#' This attribute can immediately be plugged into the `centers` argument if this function is to be executed iteratively. Defaults to `FALSE`.
#' @param assigned.to A character vector indicating the name of the column that will contain the cluster identifier. Defaults to `"assigned"`.
kMeansEqual <- function(kdat, k, centers = NULL, add.centers = FALSE, assigned.to = "assigned"){
  
  if(ncol(kdat) != 2){
    stop("This function requires a data frame with 2 columns as input!")
  }
  kdat %>% kmeans(k) -> kclust
  
  if(is.null(centers)){
    centers <- kclust$centers
  } else{
    if(ncol(centers) != 2){stop("Data frame `centers` needs to have exactly two rows.")}
    if(nrow(centers) != k){stop("Data frame `centers` needs to have exactly `k` columns.")}
  }
  
  kdat[, paste0("D", 1:nrow(centers))] <- NA
  
  # seeds <- c(24778611, 99406234, 8194459, 51007850, 23271768, 59799526, 49374854, 38096642, 8162493, 26215659)
  for(i in 1:nrow(centers)){
    # set.seed(seeds[i])
    kdat[, paste0("D", i)] <- kdist(kdat[, 1], kdat[, 2], centers[i,1], centers[i,2])
  }
  
  kdat[, assigned.to] <- 0
  kdat$index <- 1:nrow(kdat)
  working <- kdat
  FirstRound <- nrow(kdat) - (nrow(kdat) %% k)
  
  j.vector <- sapply(1:FirstRound, function(z){
    z <- if(z %% k == 0) k else (z %% k)
    return(z)
  })
  
  for(j in j.vector){
    # Cluster counts can be off by 1 due to uneven multiples of k. 
    itemloc <- which.min(kdat[kdat[, assigned.to] == 0, (paste0("D", j))])[1]
    kdat[which(kdat[, assigned.to] == 0)[itemloc], assigned.to] <- j
    ### The sorting hat says... GRYFFINDOR!!! 
  }
  
  working <- kdat[kdat[, assigned.to] == 0,]
  for(i in 1:nrow(working)){
    # These leftover points get assigned to whoever's closest, without regard to k
    kdat[kdat$index == working$index[i], assigned.to] <- which(working[i, ((1:k)+2)] == min(working[i, ((1:k)+2)]))
  }
  
  if(add.centers){
    NewCenters <- data.frame(x = rep(NA, k),
                             y = rep(NA, k))
    
    for(i in 1:nrow(NewCenters)){
      NewCenters[i, ] <- (kdat %>% filter(get(assigned.to) == i) %>% 
                            `[`(c(1, 2)) %>%
                            kmeans(1))$centers
    }
    
    # Give original column names to NewCenters data frame
    colnames(NewCenters) <- colnames(kdat)[c(1, 2)]
  }
  
  # Only keep the original columns and the "assigned" column
  kdat <- kdat[, c(1, 2, which(colnames(kdat) == assigned.to))]
  
  # Add an attribute "centers" in case we want to apply this function iteratively
  if(add.centers){
    attr(kdat, "centers") <- NewCenters
  }
  
  return(kdat)
}

# Helper function
kdist <- function(x1, y1, x2, y2){
  return(sqrt((x1-x2)^2 + (y1-y2)^2))
}
