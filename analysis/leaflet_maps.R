#This script creates the leaflet map


##############################################
#Rasterizing the wui data
western.states <- us_states(resolution = "low",
                            states = c("washington","oregon","california","arizona","nevada","utah","idaho","montana","wyoming","colorado","new mexico")) %>%
  st_transform(plot.proj)

wui.plot <- st_intersection(wui.shp,western.states) %>%
  mutate(scale=(scale-1)*100,
         scale=ifelse(scale<0,0,scale)) %>%
  st_transform(3857)

r.raster <- raster()   #Creating raster object
extent(r.raster) <- extent(wui.plot)  #Setting extent to match sf object
res(r.raster) <- 2000 #Setting cell size in meters (resolution)
wui.raster <- rasterize(wui.plot,r.raster,field="scale",fun=max)

save(wui.raster,file = "cache/wui_plot.Rdata")

#############################################
#Plotting with leaflet raster


#Define color pallete
pal <- colorNumeric(palette = "viridis",values(wui.raster),na.color = "transparent",reverse = T)

fire.pts.homes <- fire.pts.homes %>%
  mutate_at(vars(add_homes:add_cost_upper),~round(.,digits = 2)) %>%
  mutate_at(vars(add_cost:add_cost_upper),dollar) 


#Define fire labels
fire.labels <- sprintf(
  "<strong> Fire Name %s </strong> <br/>  Homes Threatened: %g <br/> Additional Crews: %g  <br/> Additional Engines: %g  <br/>  Additional Cost: %s [%s, %s] ",
  fire.pts.homes$ics_209_incident_number,
  fire.pts.homes$add_homes,
  fire.pts.homes$add_crew,
  fire.pts.homes$add_engine,
  fire.pts.homes$add_cost,
  fire.pts.homes$add_cost_lower,
  fire.pts.homes$add_cost_upper) %>%
  lapply(htmltools::HTML)

#Create map
m <- leaflet() %>% setView(lng = -113.758744, lat = 39.873757, zoom = 5)
map.tosave <- m %>% 
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addRasterImage(wui.raster, colors = pal, opacity = .35) %>%
  addLegend(pal=pal,
            values = values(wui.raster),
            title = "Housing Density Change <br>2000-2030",
            position = "bottomleft",
            labFormat = labelFormat(suffix = "%")) %>%
  addCircles(lng = fire.pts.homes$longitude, lat = fire.pts.homes$latitude, radius=100,
             fill = TRUE, fillColor = "black", fillOpacity = 1, color = "black", stroke=F) %>%
  addCircles(lng = fire.pts.homes$longitude, lat = fire.pts.homes$latitude,
             radius = 1609, stroke = T, weight = 1, opacity = 1, color = "black", fill = F,
             label = fire.labels) %>%
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
