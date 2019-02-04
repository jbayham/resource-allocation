#This script calculates the scale factor used to project the number of threatened
#homes in 2030 based on housing density projections from Hammer 2004


###########################################################
#Merging short data with 209 
fire.dat <- inner_join(g.short,
                       ics.dat,
                       by=c("ics_209_incident_number"="im_incident_number")) %>%
  st_transform(plot.proj)

save(fire.dat,file = "cache/fire_dat.Rdata")

fire.pts <- fire.dat %>% 
  dplyr::select(ics_209_incident_number,fire_name,latitude,longitude,fire_size) %>%
  distinct(.keep_all=T)

#################################################################
#Running this in parallel improves efficiency considerably  #dim(fire.pts)[1]
#I may be able to do this faster with raster overlay

cl <- makeCluster(4)
registerDoSNOW(cl)
loop.out <- foreach(i=1:dim(fire.pts)[1],
                    .combine = 'rbind',
                    .packages = c("sf","dplyr"),
                    .inorder = T) %dopar% {  
                      calculate.scale(fire.pt = fire.pts[i,],
                                      wui.shp = wui.shp %>% dplyr::select(scale))
                    }
stopCluster(cl)

#associating the scale factor with the fire
fire.pts$scale <- unlist(loop.out)
fire.pts$scale[is.nan(fire.pts$scale) | fire.pts$scale<1] <- 1

st_geometry(fire.pts) <- NULL


save(fire.pts,file = "cache/fire_pts.Rdata")