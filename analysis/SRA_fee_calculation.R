#This script calculates the number of additional homes in CA
#expected to pay the fire prevention fee when reinstated in 2031. 
#This is a response to reviewer comments.

#Load the whole CA partial block group file with housing density
CA.pbg <- st_read("data/pbg/ca_pbg00_source.shp",stringsAsFactors=F) %>%
  rename_all(str_to_lower) %>% 
  st_transform(plot.proj) %>%
  st_buffer(dist = 0.0) %>%
  select(pbg00,hden00,hden30)

#Load the CA state responsibility area subject to the fire prevention fee
CA.SRA <- st_read("data/CA_SRA/SRA16_1.shp", stringsAsFactors=F) %>%
  rename_all(str_to_lower) %>%
  filter(ruleid==3) %>%    #3=state responsibility area
  st_transform(plot.proj) %>%
  st_buffer(dist = 0.0) %>%   #set a 0 buffer as a hack to fix self-intersection error (invalid geometry)
  select(geometry)

#Plot to make sure grabbing only SRA
# ggplot(CA.SRA) +
#   geom_sf(fill=NA)

#Check validity of geometries so we can compute intersection
any(!st_is_valid(CA.pbg))
any(!st_is_valid(CA.SRA))  #any not valid should equal false
#CA.SRA <- mutate(CA.SRA,valid=st_is_valid(CA.SRA,reason = T))




#Cut CA.pgb based on the SRA

CA.fee <- st_intersection_par(CA.pbg,CA.SRA)
save(CA.fee,file="cache/CA_fee.Rdata")

################
load("cache/CA_fee.Rdata")


CA.fee.calc <- CA.fee %>%
  mutate(area=st_area(geometry)/1000000,
         homes=as.vector(area*(hden30-hden00)),
         fees_low=homes*117,
         fees_high=homes*150,
         fees_mean=(fees_high+fees_low)/2)

st_geometry(CA.fee.calc) <- NULL

CA.fee.calc %>% 
  summarise_at(vars(homes:fees_mean),~sum(.))
  

summary(CA.pgb$hden30*CA.pgb$area)
