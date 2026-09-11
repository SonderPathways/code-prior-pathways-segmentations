
### READ SPATIAL DATA 
bound <- read_sf("Data/sdr_subnational_boundaries/shps/sdr_subnational_boundaries2.shp")
table(bound$DHSREGEN)
table(bound$REGNAME)
table(bound$OTHREGNA)
bound <- subset(bound, OTHREGNA=="Uttar Pradesh")
# capital city 
capital <- c("Lucknow")
bound$capital_REGNAME <- ifelse(bound$REGNAME %in% capital, "Capital", "Non-Capital")

#urbanization 
UPIR$urban <- ifelse(UPIR$v025=="urban", 1, 0)
urban <- UPIR %>% select(sdist, urban) 
length(unique(urban$sdist)) #75 districts 

urban_region <- urban %>% 
  group_by(sdist) %>%
  summarise(urban = mean(urban, na.rm = T))
summary(urban_region$urban) #mean=20%
table(urban_region$sdist)
table(bound$REGNAME)
bound$REGNAME <- tolower(bound$REGNAME)
urban_region$REGNAME <- urban_region$sdist
urban_region$REGNAME[urban_region$sdist=="balrampur_(182)"]              <- "balrampur"
urban_region$REGNAME[urban_region$sdist=="hamirpur_(168)"]               <- "hamirpur"
urban_region$REGNAME[urban_region$sdist=="pratapgarh_(173)"]             <- "pratapgarh"
urban_region$REGNAME[urban_region$sdist=="balrampur_(182)"]              <- "balrampur"
urban_region$REGNAME[urban_region$sdist=="sant ravidas nagar (bhadohi)"] <- "sant ravidas nagar"
urban_region$REGNAME[urban_region$sdist=="mahrajganj"]                   <- "maharajganj"
urban_region <- urban_region%>%select(urban, REGNAME)
urban_region_bound <- merge(urban_region, bound, by="REGNAME")

ggplot(bound) + 
  geom_sf(aes(fill = capital_REGNAME)) +
  scale_fill_manual(values = c("red2", "white")) +
  ggtitle("India: Uttar Pradesh (2020 DHS)") +
  coord_sf() +
  theme_void() +
  theme(legend.position="none")

#### Palettes 
palette <- colorRampPalette(brewer.pal(9,"PuBu"))
cols    <- palette(4)
# "#023858" "#2585BA" "#B3C3DE" "#FFF7FB"

ggplot(data = urban_region_bound, aes(geometry = geometry, fill=urban)) + 
  geom_sf() +
  ggtitle("Urbanization of Uttar Pradesh, India (2020 DHS)") +
  scale_fill_gradientn(limits = c(0,1), colours=cols) + 
  theme_void()
