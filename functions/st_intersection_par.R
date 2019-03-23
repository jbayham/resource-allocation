#This script contains useful (somewhat) generic functions


st_intersection_par <- function(sf.x, sf.y, n.cores=4){
  #This function parallelizes sf's distance function by splitting up 
  #the query dataset into chunks and distributing the calculation
  require(sf)
  require(doSNOW)
  require(foreach)
  
  #Splitting up the query df
  split.df <- split(sf.x,
                    rep(1:n.cores, each = nrow(sf.x)/n.cores, length.out = nrow(sf.x)))
  
  #Performing st_distance in parallel default settings
  cl <- makeCluster(n.cores)
  registerDoSNOW(cl)
  df.intersected <- foreach(i=1:n.cores,.combine = 'rbind',.packages = "sf") %dopar% {  
    
    st_intersection(split.df[[i]],sf.y)
  }
  stopCluster(cl)
  
  return(df.intersected)
}
