#This script conducts the simulated impact of increased threatened assets based on projected
#housing growth through 2030.



###########################################################
#Merging short data with 209 
fire.dat <- inner_join(g.short,ics.dat,by=c("ics_209_incident_number"="im_incident_number"))



fire.pts <- dplyr::distinct(fire.dat %>% dplyr::select(ics_209_incident_number,latitude,longitude,fire_size),.keep_all=T)

#################################################################
#Using buffer
fire.pt <- fire.dat[1,]
# wui.shp <- wui.shp
#radius=5280
calculate.scale <- function(fire.pt,wui.shp,radius=6561.68){  #6561.68 ft=2km
  require(sf)
  pt.buffered <- st_buffer(fire.pt,dist = radius)
  pt.intersection <- suppressWarnings(st_intersection(pt.buffered,wui.shp)) 
  scale <- pt.intersection %>%
    #dplyr::summarise(scale=stats::weighted.mean(scale,st_area(pt.intersection),na.rm=T))
    dplyr::summarise(scale=max(scale,na.rm=T))
  st_geometry(scale) <- NULL
  return(scale)
}

#test <- calculate.scale(fire.pts[i,],wui.shp)
ggplot(wui.shp %>% dplyr::filter(scale>2.5)) +
  geom_sf(aes(fill=scale),color=NA)

#Running this in parallel improves efficiency considerably  #dim(fire.pts)[1]
cl <- makeCluster(4)
registerDoSNOW(cl)
loop.out <- foreach(i=1:dim(fire.pts)[1],.combine = 'c') %dopar% {  
  calculate.scale(fire.pt = fire.pts[i,],
                  wui.shp = wui.shp %>% dplyr::select(scale))
}
stopCluster(cl)
save(loop.out,file = "scale_calc.Rdata")
################################################################
load("scale_calc.Rdata")
fire.pts$scale <- unlist(loop.out)
fire.pts$scale[is.nan(fire.pts$scale)] <- 1

#st_geometry(fire.dat) <- NULL
#st_geometry(fire.pts) <- NULL




#Additional threatened homes times the coefficient divide 20 members in crew times 14 days times daily costs of 10,400
#Cost and resource parameters type 1 crew and engine
r.est <- c(0.015,0.006) #regression estimates c(0.0145774,0.0057903)
r.est.upper <- c(0.028327297,0.010654072) #regression estimates -- upper CI
r.est.lower <- c(0.000827503,0.000926529) #regression estimates -- lower CI
crew.size <- c(16.573,4.373)   #c(18.24,3.5)  c(20,5)
daily.cost <- c(10400,4411)

#Calculating the increased cost of increased # of threatened homes per fire
#break out into parts
fire.dat.new <- inner_join(fire.dat,fire.pts,by = c("ics_209_incident_number", "latitude", "longitude", "fire_size")) %>%
  mutate(tres=threatened,
         tres_30=tres*scale-tres, #
         add_res=(tres_30*sum(r.est/crew.size)),
         #add_res_upper=(tres_30*sum(r.est.upper/crew.size)),
         #add_res_lower=(tres_30*sum(r.est.lower/crew.size)),
         add_res_crew=(tres_30*sum(r.est[1]/crew.size[1])),
         add_res_crew_upper=(tres_30*sum(r.est.upper[1]/crew.size[1])),
         add_res_crew_lower=(tres_30*sum(r.est.lower[1]/crew.size[1])),
         add_res_engine=(tres_30*sum(r.est[2]/crew.size[2])),
         add_res_engine_upper=(tres_30*sum(r.est.upper[2]/crew.size[2])),
         add_res_engine_lower=(tres_30*sum(r.est.lower[2]/crew.size[2])),
         add_cost=tres_30*sum(daily.cost*r.est/crew.size),
         add_cost_upper=tres_30*sum(daily.cost*r.est.upper/crew.size),
         add_cost_lower=tres_30*sum(daily.cost*r.est.lower/crew.size),
         add_cost_crew=daily.cost[1]*add_res_crew,
         add_cost_crew_upper=daily.cost[1]*add_res_crew_upper,
         add_cost_crew_lower=daily.cost[1]*add_res_crew_lower,
         add_cost_engine=daily.cost[2]*add_res_engine,
         add_cost_engine_upper=daily.cost[2]*add_res_engine_upper,
         add_cost_engine_lower=daily.cost[2]*add_res_engine_lower         
         )

fire.dat.new %>% 
  group_by(ics_209_incident_number) %>%
  summarize(max_scale=max(scale)) %>%
  ungroup() %>%
  summarise(mean_scale=mean(max_scale))


#####
#
# fire.dat.new %>% 
#   group_by(year(discovery_date)) %>%
#   summarize(total_cost=sum(add_cost,na.rm = T),
#             total_cost_upper=sum(add_cost_upper,na.rm = T),
#             total_cost_lower=sum(add_cost_lower,na.rm = T)) 
fire.dat.new %>% 
  group_by(year(discovery_date)) %>%
  summarize_at(vars(matches("(cost)")),funs(sum(.,na.rm=T))) 

#annual average
fire.dat.new %>% 
  group_by(year(discovery_date)) %>%
  summarize_at(vars(matches("(cost)")),funs(sum(.,na.rm=T))) %>%
  ungroup() %>%
  summarize_at(vars(matches("(cost)")),funs(mean(.,na.rm=T)))




#we calculate a lower bound of between (use confidence intervals on regression estimates) $5.8 million
#The recently suspended CA prevention fee collected $117 or $151 for each habitable structure if a residence was located in a state responsibility area.  
#There are roughly 800,000 homes located in the SRA and subject to the fee.  The state collects roughly $93 million each year


##############################################
#Rasterizing the wui data
ca.counties <- us_counties(resolution = "low",states = "california") %>% st_transform(plot.proj)
#wui.plot <- st_intersection(wui.shp,ca.counties %>% dplyr::filter(name %in% c("Los Angeles","Santa Barbara","Ventura","Kern","San Luis Obispo"))) %>%
wui.plot <- st_intersection(wui.shp,ca.counties) %>%
  mutate(scalar=hden30/hden00-1,
         scalar=if_else(is.na(scalar),1,scalar)*100) %>%
  st_transform('+proj=longlat +datum=WGS84')

wui.plot.raster <- wui.plot %>%
  st_transform(3857)

r.raster <- raster()
extent(r.raster) <- extent(wui.plot.raster)
res(r.raster) <- 2000 #Setting cell size in meters (resolution)
wui.raster <- rasterize(wui.plot.raster,r.raster,field="scalar",fun=max)

#############################################
#Plotting with leaflet raster
library(maps)
library(leaflet)
library(viridis)
library(mapview)
library(htmlwidgets)
#mapStates = map("county","california", fill = TRUE, plot = FALSE)
#pal <- colorNumeric(c('#f7f7f7','#cccccc','#969696','#525252'),values(wui.raster),na.color = "transparent")
#pal <- colorNumeric(c('#fee5d9','#fcae91','#fb6a4a','#cb181d'),values(wui.raster),na.color = "transparent")
pal <- colorNumeric(palette = "viridis",values(wui.raster),na.color = "transparent",reverse = T)

m <- leaflet() %>% setView(lng = -119.665899, lat = 34.540063, zoom = 9)
map.tosave <- m %>% 
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addRasterImage(wui.raster, colors = pal, opacity = .35) %>%
  addLegend(pal=pal,
            values = values(wui.raster),
            title = "Housing Density Change <br>2000-2030",
            position = "bottomleft",
            labFormat = labelFormat(suffix = "%")) %>%
  addCircles(lng = fire.pts$longitude, lat = fire.pts$latitude, radius=100,
             fill = TRUE, fillColor = "black", fillOpacity = 1, color = "black", stroke=F) %>%
  addCircles(lng = fire.pts$longitude, lat = fire.pts$latitude,
             radius = 1609, stroke = T, weight = 1, opacity = 1, color = "black", fill = F) %>%
  addScaleBar(position = c("bottomleft")) #%>%
  # addLogo(img="Compass-rose-basic-thin-letters-400.png", alpha = 1, src = c("local"),
  #         position = c("topright"), width = 80, height = 80)

saveWidget(map.tosave,file="CA_Growth_Fire.html",selfcontained = T)
#Can use mapshot but I just displayed it and used windows snipping tool
#mapview::mapshot(map.tosave,file="CA_Growth_Fire.pdf")

##################
# #Plotting with leaflet polygon
# library(maps)
# library(leaflet)
# # this is modified from 
# # https://github.com/rstudio/leaflet/blob/master/inst/examples/icons.R#L24
# pchIcons = function(pch = 1, width = 30, height = 30, bg = "transparent", col = "black", ...) {
#   n = length(pch)
#   files = character(n)
#   # create a sequence of png images
#   for (i in seq_len(n)) {
#     f = tempfile(fileext = '.png')
#     png(f, width = width, height = height, bg = bg)
#     par(mar = c(0, 0, 0, 0))
#     plot.new()
#     points(0.5, 0.5, pch = pch[i], col = col[i], cex = min(width, height) / 8, ...)
#     dev.off()
#     files[i] = f
#   }
#   files
# }
# 
# iconFiles = pchIcons(pch=17,width = 20,height=20)
# 
# mapStates = map("county","california", fill = TRUE, plot = FALSE)
# #pal <- colorNumeric(c('#f0f0f0','#bdbdbd','#636363'),domain = wui.plot$scalar,na.color = "transparent")
# pal <- colorNumeric(palette = "viridis",domain = wui.plot$scalar,na.color = "transparent",reverse = T)
# 
# m <- leaflet(wui.plot) %>% setView(lng = -119.665899, lat = 34.540063, zoom = 9)
# map.tosave <- m %>% 
#   addProviderTiles(providers$Stamen.Toner) %>%
#   addPolygons(fillColor = ~pal(scalar),
#               fillOpacity = 0.5,
#               stroke = F) %>%
#   addCircles(lng = fire.pts$longitude, lat = fire.pts$latitude, radius=100,
#              fill = TRUE, fillColor = "black", fillOpacity = 1, color = "black", stroke=F) %>%
#   addCircles(lng = fire.pts$longitude, lat = fire.pts$latitude,
#              radius = 1609, stroke = T, weight = 1, opacity = 1, color = "black", fill = F)
# 
# mapview::mapshot(map.tosave,file="CA_Growth_Fire.pdf")
# ########################
# #Plot
# ca.counties <- us_counties(resolution = "low",states = "california") %>% st_transform(plot.proj)
# wui.gg <- wui.shp[ca.counties %>% dplyr::filter(name=="Santa Barbara"),] %>%
#   mutate(scalar=hden30/hden00,
#          scalar=if_else(is.na(scalar),1,scalar))
# ggplot() + 
#   geom_sf(data=wui.gg, aes(fill=scalar), color=NA) +
# #   scale_fill_distiller(limits=c(0,100)) + 
# #   geom_sf(data=ca.counties %>% dplyr::filter(name=="Butte"), fill=NA) +
# #   geom_sf(data=fire.temp.buffer[ca.counties %>% dplyr::filter(name=="Butte"),], color="red", fill=NA)
# 
# #Map with scale and fire pt overlays
# ca.state <- us_states(resolution = "high",states = "california") %>% st_transform(plot.proj)
# #wui.plot <- wui.shp[ca.counties %>% dplyr::filter(name=="Butte"),]
# ggplot() +
#   #geom_sf(data=wui.shp %>% sample_frac(size = .05), aes(fill=scale), color=NA) +
#   #scale_fill_distiller(limits=c(1,3)) +
#   geom_sf(data=ca.state, fill=NA) +
#   geom_sf(data=g.short, color="red", fill=NA, aes(size=fire_size))

