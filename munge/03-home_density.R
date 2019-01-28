#This script reads in the WUI layer and calculates the coefficient
#to rescale the threatened homes variable.

#Loading wui layer
wui.shp <- st_read("data/ca_wui/ca_pbg00_source.shp") %>%
  dplyr::rename_all(str_to_lower) %>%
  mutate(scale=hden30/hden00,
         scale=if_else(is.na(scale),1,scale)) %>%
  st_transform(plot.proj)
