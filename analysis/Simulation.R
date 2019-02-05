#This script conducts the simulated impact of increased threatened assets based on projected
#housing growth through 2030.


############################################################
#Additional threatened homes times the coefficient divide 20 members in crew times 14 days times daily costs of 10,400
#Cost and resource parameters type 1 crew and engine
r.est <- c(0.016,0.007) #regression estimates c(0.0145774,0.0057903)
r.est.upper <- c(0.028327297,0.010654072) #regression estimates -- upper CI
r.est.lower <- c(0.000827503,0.000926529) #regression estimates -- lower CI
crew.size <- c(16.573,4.373)   #c(18.24,3.5)  c(20,5)
daily.cost <- c(10400,4411)


#Calculating the increased cost of increased # of threatened homes per fire
#break out into parts
fire.dat.homes <- left_join(fire.dat,fire.pts) %>%
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
         #add_cost=tres_30*sum(daily.cost*r.est/crew.size),
         #add_cost_upper=tres_30*sum(daily.cost*r.est.upper/crew.size),
         #add_cost_lower=tres_30*sum(daily.cost*r.est.lower/crew.size),
         add_cost_crew=daily.cost[1]*add_res_crew,
         add_cost_crew_upper=daily.cost[1]*add_res_crew_upper,
         add_cost_crew_lower=daily.cost[1]*add_res_crew_lower,
         add_cost_engine=daily.cost[2]*add_res_engine,
         add_cost_engine_upper=daily.cost[2]*add_res_engine_upper,
         add_cost_engine_lower=daily.cost[2]*add_res_engine_lower,
         add_cost=add_cost_crew+add_cost_engine,
         add_cost_upper=add_cost_crew_upper+add_cost_engine_upper,
         add_cost_lower=add_cost_crew_lower+add_cost_engine_lower
         )

fire.pts.homes <- fire.dat.homes %>% 
  group_by(ics_209_incident_number) %>%
  summarize(fire_name=first(fire_name),
            latitude=first(latitude),
            longitude=first(longitude),
            max_scale=max(scale),
            year=first(year(discovery_date)),
            state=first(state),
            add_homes=sum(tres_30,na.rm = T),
            add_crew=sum(add_res_crew,na.rm = T),
            add_engine=sum(add_res_engine,na.rm = T),
            add_cost=sum(add_cost,na.rm = T),
            add_cost_lower=sum(add_cost_lower,na.rm = T),
            add_cost_upper=sum(add_cost_upper,na.rm = T)) %>%
  ungroup() 

save(fire.dat.homes,fire.pts.homes,file = "cache/fire_dat_homes.Rdata")

summary(fire.pts.homes$max_scale)
###################
fire.pts.homes %>%
  group_by(year,state) %>%
  summarize_at(vars(matches("(cost)")),funs(sum(.,na.rm=T))) %>%
  mutate_at(vars(matches("(cost)")),~round(.,digits = 2)) %>%
  mutate_at(vars(matches("(cost)")),dollar) %>% 
  View()

fire.pts.homes %>%
  group_by(year,state) %>%
  summarize_at(vars(matches("(cost)")),funs(sum(.,na.rm=T))) %>%
  ungroup() %>%
  group_by(state) %>%
  summarize_at(vars(matches("(cost)")),funs(max(.,na.rm=T))) %>%
  ungroup() %>%
  ggplot(aes(x=reorder(state, add_cost),y=add_cost)) +
  geom_col() +
  geom_errorbar(aes(ymin=add_cost_lower,ymax=add_cost_upper),width=0) +
  coord_flip() +
  scale_y_continuous(name="Projected Cost",labels = dollar) +
  scale_x_discrete(name="") +
  theme_bw()

ggsave(filename = "figures/cost_by_state.pdf",width = 5,height = 4,units = "in")

#Try to add error bars to 
  

####################

#
# fire.dat.new %>% 
#   group_by(year(discovery_date)) %>%
#   summarize(total_cost=sum(add_cost,na.rm = T),
#             total_cost_upper=sum(add_cost_upper,na.rm = T),
#             total_cost_lower=sum(add_cost_lower,na.rm = T)) 
fire.dat.homes %>% 
  group_by(year(discovery_date)) %>%
  summarize_at(vars(matches("(cost)")),funs(sum(.,na.rm=T))) %>%
  mutate_at(vars(matches("(cost)")),~round(.,digits = 2)) %>%
  mutate_at(vars(matches("(cost)")),dollar) %>% View()

#annual average
fire.dat.homes %>% 
  group_by(year(discovery_date)) %>%
  summarize_at(vars(matches("(cost)")),funs(sum(.,na.rm=T))) %>%
  ungroup() %>%
  summarize_at(vars(matches("(cost)")),funs(mean(.,na.rm=T)))




#we calculate a lower bound of between (use confidence intervals on regression estimates) $5.8 million
#The recently suspended CA prevention fee collected $117 or $151 for each habitable structure if a residence was located in a state responsibility area.  
#There are roughly 800,000 homes located in the SRA and subject to the fee.  The state collects roughly $93 million each year



