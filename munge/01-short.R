#This script reads in Karen Short's data


#Short Data
g.short <- st_read(dsn="data/FPA_FOD_20150323.gdb",layer="Fires") %>% 
  st_crs(plot.proj) %>%
  rename_all(str_to_lower) %>%
  select(ics_209_incident_number,latitude,longitude,discovery_date,cont_date,state,fire_size) %>%
  mutate_if(base::is.factor,as.character) %>%  
  mutate(discovery_date=ymd_hms(discovery_date),
                cont_date=ymd_hms(cont_date)) %>%
  filter(state=="CA",
                !is.na(ics_209_incident_number)) %>% 
  st_transform(plot.proj)

# save(g.short,file="data/short_sf.Rdata")

