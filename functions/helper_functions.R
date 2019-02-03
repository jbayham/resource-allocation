#This script contains functions used throughout the analysis


#caclulate.scale computes the maximum increase in home density within a 2 km radius
#of the fire ignition.  Note: radius is defined in ft so units of projection
#must be in ft (check st_crs(fire.pts)$units)
# calculate.scale.par <- function(fire.pts,wui.shp,radius=6561.68, n.cores=4){  #6561.68 ft=2km
#   require(sf)
#   require(doSNOW)
#   require(foreach)
#   
#   #Splitting up the query df
#   split.df <- split(fire.pts,
#                     rep(1:n.cores, each = nrow(fire.pts)/n.cores, length.out = nrow(fire.pts)))
#   
#   #Performing st_distance in parallel default settings
#   cl <- makeCluster(n.cores)
#   registerDoSNOW(cl)
#   foreach(i=1:n.cores,
#           .combine = 'rbind',
#           .packages = c("sf","dplyr"),
#           .inorder = T) %dopar% { 
#             pt.buffered <- st_buffer(split.df[[i]],dist = radius)
#             pt.intersection <- suppressWarnings(st_intersection(pt.buffered,wui.shp)) 
#             pts.df <- pt.intersection %>%
#               group_by(ics_209_incident_number)
#               #dplyr::summarise(scale=stats::weighted.mean(scale,st_area(pt.intersection),na.rm=T))
#               dplyr::summarise(scale=max(scale,na.rm=T))
#             return(pts.df)
#           }
#   stopCluster(cl)
# }

calculate.scale <- function(fire.pt,wui.shp,radius=6561.68){  #6561.68 ft=2km
  require(sf)
  require(dplyr)
  pt.buffered <- st_buffer(fire.pt,dist = radius)
  pt.intersection <- suppressWarnings(st_intersection(pt.buffered,wui.shp)) 
  scale <- pt.intersection %>%
    #dplyr::summarise(scale=stats::weighted.mean(scale,st_area(pt.intersection),na.rm=T))
    dplyr::summarise(scale=max(scale,na.rm=T))
  st_geometry(scale) <- NULL
  return(scale)
}

#####
#unit test
#test <- calculate.scale(fire.pts[1,],wui.shp)

