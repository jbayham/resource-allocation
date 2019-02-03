#This script reads in the WUI layer and calculates the coefficient
#to rescale the threatened homes variable.

#Loading wui layer in each state and binding them together
shp.files <- str_subset(dir("data/pbg",full.names = T),".shp$")

wui.shp <- map(shp.files,
               function(x){
                st_read(x) %>%
                   rename_all(str_to_lower) %>%
                   transmute(scale=hden30/hden00,
                             scale=if_else(is.na(scale),1,scale)) %>%
                   st_transform(plot.proj)
}  )


wui.shp <- purrr::reduce(wui.shp,rbind)

# ggplot() +
#  geom_sf(data = wui.shp %>% sample_n(500))

save(wui.shp,file = "cache/housing_sf.Rdata")
