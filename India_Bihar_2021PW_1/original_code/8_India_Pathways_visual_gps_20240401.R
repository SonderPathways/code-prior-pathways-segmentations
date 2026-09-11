rm(list = ls())
pacman::p_load(RODBC, dplyr, tidyverse, openxlsx, tidyselect, expss, zscorer, survey, foreign, tidyr, data.table, psych, haven, sf)
pacman::p_load("foreign", "stringr", "magrittr", "reshape2", "data.table", "psych", "vtable", "corrplot", "haven", 
               "survey", "ggplot2", "dplyr", "tidyr", "sf", "ggplot2", "RColorBrewer")
conn <- odbcConnect("IDMAzureDatabricks_DSN") 
options(max.print=1000000)

setwd("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2021 Pathways/Data/Bihar_constructed_data")
load("vuloutcomes.RData")

# woman <- sqlQuery(conn, "SELECT * FROM idm_general.pathways.pathways_bihar_ityp_1_clean") 
bihar <- sqlQuery(conn, "SELECT * FROM idm_general.pathways.pathways_bihar_clean") 
class(bihar$ITYP)
woman <- bihar %>% filter(ITYP == 1)

#get a file for Lang Gao for html 
vuloutcomes.raw <- merge(vuloutcomes, woman, by="caseid")
class.vul.outcomes.fp <- vuloutcomes.raw %>% dplyr::select(LCA5_class, 2:45, 82:107, 
                                                 starts_with("C36_") & !C36_99, 
                                                 starts_with("C37_") & !C37_99)
#a 3 categorical variable: never use, ever use (but not now), current use. 
class.vul.outcomes.fp <- class.vul.outcomes.fp %>%
  mutate(fp.use=case_when(
    fp.ever.any==0 & fp.current.any==0  ~ 0, #never use
    fp.ever.any==0 & preg.current==1 ~ 0, #never use
    fp.ever.any==1 & fp.current.any==0 ~ 1, #ever use (but not now)
    fp.ever.any==1 & preg.current==1 ~ 1, #ever use (but not now)
    fp.ever.any==0 & fp.current.any==1 ~ 2, #current use
    fp.ever.any==1 & fp.current.any==1 ~ 2 #current use
  ))
table(class.vul.outcomes.fp$fp.use)
sum(is.na(class.vul.outcomes.fp$fp.use))
write.csv(class.vul.outcomes.fp, "class_vul_outcomes_fp.csv")


gps <- woman %>% select(caseid, QX1_Longitude, QX1_Latitude, QX1_Accuracy, R_district, R_block)
length(unique(gps$R_district)) #N=38
summary(gps$QX1_Accuracy) #median=4km? 

### READ SPATIAL DATA 
bound <- read_sf("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2020 DHS/Data/sdr_subnational_boundaries/shps/sdr_subnational_boundaries2.shp")
table(bound$DHSREGEN)
table(bound$REGNAME)
table(bound$OTHREGNA)
bound <- subset(bound, OTHREGNA=="Bihar")
# capital city 
capital <- c("Patna")
bound$capital_REGNAME <- ifelse(bound$REGNAME %in% capital, "Capital", "Non-Capital")

ggplot(bound) + 
  geom_sf(aes(fill = capital_REGNAME)) +
  scale_fill_manual(values = c("red2", "white")) +
  ggtitle("India: Bihar (2020 DHS)") +
  coord_sf() +
  theme_void() +
  theme(legend.position="none")

#plot GPS data 
ggplot() + 
  geom_sf(data = bound, size = 1.5, color = "black", fill = "transparent") + 
  geom_point(data = gps, aes( x = QX1_Longitude, y = QX1_Latitude), size=1) +
  coord_sf() +
  theme_void() +
  theme(legend.position="none")
  

#merge GPS with class membership 
gps_vuloutcomes <- merge(gps, vuloutcomes, by="caseid")
gps_vuloutcomes$LCA5_class <- as.character(gps_vuloutcomes$LCA5_class)

gps_vuloutcomes1 <- gps_vuloutcomes %>% filter(LCA5_class==1)
gps_vuloutcomes2 <- gps_vuloutcomes %>% filter(LCA5_class==2)
gps_vuloutcomes3 <- gps_vuloutcomes %>% filter(LCA5_class==3)
gps_vuloutcomes4 <- gps_vuloutcomes %>% filter(LCA5_class==4)
gps_vuloutcomes5 <- gps_vuloutcomes %>% filter(LCA5_class==5)

ggplot(gps_vuloutcomes) + 
  geom_sf(data = bound, size = 1.5, color = "black", fill = "transparent") + 
  geom_point(data = gps_vuloutcomes, aes( x = QX1_Longitude, y = QX1_Latitude, color=LCA5_class), size=1) +
  scale_color_manual(values = c("blue", "darkgreen", "purple", "red", "orange")) + 
  coord_sf() +
  theme_void() 
  
ggplot(gps_vuloutcomes1) + 
  geom_sf(data = bound, size = 1.5, color = "black", fill = "transparent") + 
  geom_point(data = gps_vuloutcomes1, aes( x = QX1_Longitude, y = QX1_Latitude, color=LCA5_class), size=1) +
  scale_color_manual(values = c("blue")) + 
  coord_sf() +
  theme_void() 

ggplot(gps_vuloutcomes2) + 
  geom_sf(data = bound, size = 1.5, color = "black", fill = "transparent") + 
  geom_point(data = gps_vuloutcomes2, aes( x = QX1_Longitude, y = QX1_Latitude, color=LCA5_class), size=1) +
  scale_color_manual(values = c("darkgreen")) + 
  coord_sf() +
  theme_void()   
  
ggplot(gps_vuloutcomes3) + 
  geom_sf(data = bound, size = 1.5, color = "black", fill = "transparent") + 
  geom_point(data = gps_vuloutcomes3, aes( x = QX1_Longitude, y = QX1_Latitude, color=LCA5_class), size=1) +
  scale_color_manual(values = c("purple")) + 
  coord_sf() +
  theme_void()   

ggplot(gps_vuloutcomes4) + 
  geom_sf(data = bound, size = 1.5, color = "black", fill = "transparent") + 
  geom_point(data = gps_vuloutcomes4, aes( x = QX1_Longitude, y = QX1_Latitude, color=LCA5_class), size=2) +
  scale_color_manual(values = c("red")) + 
  coord_sf() +
  theme_void()   

ggplot(gps_vuloutcomes5) + 
  geom_sf(data = bound, size = 1.5, color = "black", fill = "transparent") + 
  geom_point(data = gps_vuloutcomes5, aes( x = QX1_Longitude, y = QX1_Latitude, color=LCA5_class), size=2) +
  scale_color_manual(values = c("orange")) + 
  coord_sf() +
  theme_void()   

table(woman$R_district)
gps4 <- gps %>% filter(R_district=="D03")

ggplot() + 
  geom_sf(data = bound, size = 1.5, color = "black", fill = "transparent") + 
#  geom_point(data = gps, aes( x = QX1_Longitude, y = QX1_Latitude), size=1) +
  coord_sf() +
  theme_void() +
  theme(legend.position="none")

ggplot() + 
  geom_sf(data = bound, size = 1.5, color = "black", fill = "transparent") + 
  geom_point(data = gps %>% filter(R_district=="D38"), aes( x = QX1_Longitude, y = QX1_Latitude), size=1) +
  coord_sf() +
  theme_void() +
  theme(legend.position="none")

table(bound$REGNAME)

gps_vuloutcomes <- gps_vuloutcomes %>% mutate(
  REGNAME =  case_when(
    R_district=="D01" ~ "Araria",
    R_district=="D02" ~ "Arwal",
    R_district=="D03" ~ "Aurangabad",
    R_district=="D04" ~ "Banka",
    R_district=="D05" ~ "Begusarai",
    R_district=="D06" ~ "Bhagalpur",
    R_district=="D07" ~ "Bhojpur",
    R_district=="D08" ~ "Buxer",
    R_district=="D09" ~ "Darbhanga",
    R_district=="D10" ~ "Purba Champaran",
    R_district=="D11" ~ "Gaya",
    R_district=="D12" ~ "Gopalganj",
    R_district=="D13" ~ "Jamui",
    R_district=="D14" ~ "Jehanabad",
    R_district=="D15" ~ "Kaimur (Bhabua)",
    R_district=="D16" ~ "Katihar",
    R_district=="D17" ~ "Khagaria",
    R_district=="D18" ~ "Kishanganj",
    R_district=="D19" ~ "Lakhisarai",
    R_district=="D20" ~ "Madhepura",
    R_district=="D21" ~ "Madhubani",
    R_district=="D22" ~ "Munger",
    R_district=="D23" ~ "Muzaffarpur",
    R_district=="D24" ~ "Nalanda",
    R_district=="D25" ~ "Nawada",
    R_district=="D26" ~ "Patna",
    R_district=="D27" ~ "Purnia",
    R_district=="D28" ~ "Rohtas",
    R_district=="D29" ~ "Saharsa",
    R_district=="D30" ~ "Samastipur",
    R_district=="D31" ~ "Saran",
    R_district=="D32" ~ "Sheikhpura",
    R_district=="D33" ~ "Sheohar",
    R_district=="D34" ~ "Sitamarhi",
    R_district=="D35" ~ "Siwan",
    R_district=="D36" ~ "Supaul",
    R_district=="D37" ~ "Vaishali",
    R_district=="D38" ~ "Pashchim Champaran"
  ))

length(unique(gps_vuloutcomes$R_district)) #N=38 districts 
length(unique(gps_vuloutcomes$REGNAME)) #N=38 districts 
#sample size for each district (2000/38, min and max)
woman <- woman %>% 
  group_by(R_district) %>% 
  mutate(count = n())
summary(woman$count) #mean=200 women per district (range: 15-328)

#for each district: the highest share of class 
highest_class <- gps_vuloutcomes %>%
  group_by(REGNAME) %>%
  summarise(LCA5_class = names(which.max(table(LCA5_class))))

highest_class_bound <- merge(bound , highest_class, by="REGNAME")
class(highest_class_bound$LCA5_class)



ggplot() + 
  geom_sf(data=highest_class_bound) + 
  geom_sf_text(data=highest_class_bound, aes(label = REGNAME, geometry = geometry), size=3)+
  theme_void()


ggplot() +
  geom_sf(data=highest_class_bound, aes(fill = factor(LCA5_class)), alpha=0.4) + 
  scale_fill_manual(values = c("blue", "darkgreen", "purple", "red", "orange")) + 
  coord_sf() +
  theme_void() + 
  theme(legend.position = "none")


  
odbcCloseAll()
