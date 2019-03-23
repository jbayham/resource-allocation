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

#Setting up progress bar
pb <- txtProgressBar(max=dim(fire.pts)[1], style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

loop.out <- foreach(i=1:dim(fire.pts)[1],
                    .combine = 'rbind',
                    .packages = c("sf","dplyr"),
                    .inorder = T,
                    .options.snow=opts) %dopar% {  
                      calculate.scale(fire.pt = fire.pts[i,],
                                      wui.shp = wui.poly)
                    }
stopCluster(cl)
close(pb)


#associating the scale factor with the fire
fire.pts <- bind_cols(fire.pts,loop.out) %>%
  mutate_at(vars(hden00:scale),~ifelse(is.nan(.)|is.infinite(.),1,.))


st_geometry(fire.pts) <- NULL


save(fire.pts,file = "cache/fire_pts.Rdata")
