#This script reads in the WUI layer and calculates the coefficient
#to rescale the threatened homes variable.

#Loading wui layer in each state and binding them together
shp.files <- str_subset(dir("data/pbg",full.names = T),".shp$")

wui.poly <- map(shp.files,
               function(x){
                st_read(x) %>%
                   rename_all(str_to_lower) %>%
                   transmute(scale=hden30/hden00,
                             scale=if_else(is.na(scale),1,scale)) %>%
                   st_transform(plot.proj)
}  )


wui.poly <- purrr::reduce(wui.poly,rbind)

save(wui.poly,file = "cache/wui_poly.Rdata")

# ggplot() +
#  geom_sf(data = wui.poly %>% sample_n(500))

##############################################
#Rasterizing the wui data
western.states <- us_states(resolution = "low",
                            states = c("washington","oregon","california","arizona","nevada","utah","idaho","montana","wyoming","colorado","new mexico")) %>%
  st_transform(plot.proj)

wui.poly <- st_intersection(wui.poly,western.states) %>%
  mutate(scale=(scale-1)*100,
         scale=ifelse(scale<0,0,scale)) %>%
  st_transform(3857)

r.raster <- raster()   #Creating raster object
extent(r.raster) <- extent(wui.poly)  #Setting extent to match sf object
res(r.raster) <- 2000 #Setting cell size in meters (resolution)
wui.raster <- rasterize(wui.poly,r.raster,field="scale",fun=max)

save(wui.raster,file = "cache/wui_raster.Rdata")

