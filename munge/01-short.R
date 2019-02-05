#This script reads in Karen Short's data


#Short Data
g.short <- st_read(dsn="data/FPA_FOD_20150323.gdb",
                   layer="Fires",
                   stringsAsFactors = F)  %>% 
  st_transform(readin.proj) %>%
  rename_all(str_to_lower) %>%
  select(ics_209_incident_number,fire_name,latitude,longitude,discovery_date,cont_date,state,fire_size) %>% 
  filter(state %in% c("AZ","CA","CO","ID","MT","NM","NV","OR","UT","WA","WY"),
         !is.na(ics_209_incident_number)) 

save(g.short,file="cache/g_short.Rdata")

