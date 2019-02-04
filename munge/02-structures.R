#This script reads in 209 data on structures threatened

#update this with structures from pull for Caggiano
load("data/structures.Rdata")
ics.dat <- structures %>%
  filter(str_detect(structure_type,"Residence")) %>%
  mutate(im_report_date=as_date(im_report_date)) %>%
  group_by(im_incident_number,im_report_date) %>%
  summarise(threatened=max(threatened)) %>%
  ungroup() 

save(ics.dat,file = "cache/ics_dat.Rdata")
