rm(list=ls())
pacman::p_load("poLCA", "dplyr", "tidyverse", "tidyLPA", "networkD3", "data.table", "ggpubr", "forcats", "readxl", "tidyr",
               "ggalluvial", "ggsankey", "openxlsx")

setwd("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2020 DHS")
load("Data/IR.RData")
v025 <- IR %>% select(caseid, v025)


rural_LCA2 <- readRDS("India_constructed_data/rural_LCA2.rds")
rural_LCA3 <- readRDS("India_constructed_data/rural_LCA3.rds")
rural_LCA4 <- readRDS("India_constructed_data/rural_LCA4.rds")
rural_LCA5 <- readRDS("India_constructed_data/rural_LCA5.rds")
rural_LCA6 <- readRDS("India_constructed_data/rural_LCA6.rds")
rural_LCA7 <- readRDS("India_constructed_data/rural_LCA7.rds")
rural_LCA8 <- readRDS("India_constructed_data/rural_LCA8.rds")
rural_LCA9 <- readRDS("India_constructed_data/rural_LCA9.rds")
rural_LCA10 <- readRDS("India_constructed_data/rural_LCA10.rds")
rural_LCA11 <- readRDS("India_constructed_data/rural_LCA11.rds")
rural_LCA12 <- readRDS("India_constructed_data/rural_LCA12.rds")
prop.table(table(rural_LCA5$predclass))

urban_LCA2 <- readRDS("India_constructed_data/urban_LCA2.rds")
urban_LCA3 <- readRDS("India_constructed_data/urban_LCA3.rds")
urban_LCA4 <- readRDS("India_constructed_data/urban_LCA4.rds")
urban_LCA5 <- readRDS("India_constructed_data/urban_LCA5.rds")
urban_LCA6 <- readRDS("India_constructed_data/urban_LCA6.rds")
urban_LCA7 <- readRDS("India_constructed_data/urban_LCA7.rds")
prop.table(table(urban_LCA5$predclass))
prop.table(table(urban_LCA6$predclass))

#a file for class only 
urban_class <- cbind(urban_LCA2$predclass, 
                     urban_LCA3$predclass,
                     urban_LCA4$predclass,
                     urban_LCA5$predclass,
                     urban_LCA6$predclass,
                     urban_LCA7$predclass)
colnames(urban_class) <- c("cluster_k2", "cluster_k3", "cluster_k4", "cluster_k5", "cluster_k6", "cluster_k7")
urban_class <- as.data.table(urban_class)
prop.table(table(urban_class$cluster_k2))
prop.table(table(urban_class$cluster_k3))
prop.table(table(urban_class$cluster_k4))
prop.table(table(urban_class$cluster_k5))
prop.table(table(urban_class$cluster_k6))
prop.table(table(urban_class$cluster_k7))

rural_class <- cbind(rural_LCA2$predclass, 
                     rural_LCA3$predclass,
                     rural_LCA4$predclass,
                     rural_LCA5$predclass,
                     rural_LCA6$predclass,
                     rural_LCA7$predclass,
                     rural_LCA8$predclass,
                     rural_LCA9$predclass,
                     rural_LCA10$predclass,
                     rural_LCA11$predclass,
                     rural_LCA12$predclass)
colnames(rural_class) <- c("cluster_k2", "cluster_k3", "cluster_k4", "cluster_k5", "cluster_k6", "cluster_k7",
                           "cluster_k8", "cluster_k9", "cluster_k10", "cluster_k11", "cluster_k12")
rural_class <- as.data.table(rural_class)

prop.table(table(rural_LCA5$predclass))

prop.table(table(urban_LCA5$predclass))


#make sankey plots 
make_sankey <- function(data){
  links <- lapply(2:6, function(i){
    link.i<-  data[,.(value = .N),.(source = paste0("k", i, "_", get(paste0("cluster_k", i))), target = paste0("k", i+1, "_",get(paste0("cluster_k", i+1))))]
    return(link.i)
  }) %>% rbindlist(use.names = T, fill = T)
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(
    name=c(as.character(links$source), 
           as.character(links$target)) %>% unique()
  )
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  # Make the Network
  p <- sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", fontSize = 14, 
                     sinksRight=FALSE)
  return(p)
}

sankey_urban <- make_sankey(urban_class)
sankey_urban

#make sankey plots 
make_sankey <- function(data){
  links <- lapply(2:9, function(i){
    link.i<-  data[,.(value = .N),.(source = paste0("k", i, "_", get(paste0("cluster_k", i))), target = paste0("k", i+1, "_",get(paste0("cluster_k", i+1))))]
    return(link.i)
  }) %>% rbindlist(use.names = T, fill = T)
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(
    name=c(as.character(links$source), 
           as.character(links$target)) %>% unique()
  )
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  # Make the Network
  p <- sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name",  fontSize = 14, 
                     sinksRight=FALSE)
  return(p)
}

sankey_rural <- make_sankey(rural_class)
sankey_rural




######################################################## DESCRIPTIVES 
vulnerability <- readRDS("India_constructed_data/2_UP_DHS_vulpredictors.RData")
df <- vulnerability
##make all vars into binary: higher percentage means more vulnerable, low=disadvantaged 
df$caste <- ifelse(df$caste=="1: Schedule caste/tribe", 1, 0)
df$ed.level <- ifelse(df$ed.level=="0: no education" | df$ed.level=="1: incomplete primary", 1, 0)
df$ed.any <- 1 - df$ed.any
df$cohab.birth.months <- ifelse(df$cohab.birth.months=="3: 2-3 years" | df$cohab.birth.months=="4: >3 years", 0, 1)
df$num.births.cat <- ifelse(df$num.births.cat=="4" | df$num.births.cat=="5+", 1, 0)
df$married.10years <- 1- df$married.10years
df$num.under5.cat <- ifelse(df$num.under5.cat=="2+", 1, 0) #higher fertility
df$hh.members.cat <- ifelse(df$hh.members.cat>=3, 1, 0)
df$num.kids.house.cat <- ifelse(df$num.kids.house.cat=="3+", 1, 0)
df$ed.lev.his <- ifelse(df$ed.lev.his=="0: no education" | df$ed.lev.his=="1: incomplete primary", 1, 0)
df$medical.permit <- 1- df$medical.permit
df$say.no.sex <- 1- df$say.no.sex
df$say.no.sex.sti <- 1- df$say.no.sex.sti
df$say.no.sex.otherwomen <- 1- df$say.no.sex.otherwomen
df$joint.health <- 1- df$joint.health
df$joint.large <- 1- df$joint.large
df$joint.visit <- 1- df$joint.visit
df$joint.his.money <- 1- df$joint.his.money
df$her.own.house <- 1- df$her.own.house
df$her.own.land <- 1- df$her.own.land
df$her.own.money <- 1- df$her.own.money
df$her.own.phone <- 1- df$her.own.phone
df$work.type <- ifelse(df$work.type=="2: others", 0, 1)
df$work.current <- 1- df$work.current
df$work.12mo <- 1- df$work.12mo
df$work.seasonal <- ifelse(df$work.seasonal=="2: All year", 0, 1)
df$work.type.his <- ifelse(df$work.type.his=="1: agricultural", 1, 0)
df$work.12mo.his <- ifelse(df$work.12mo.his=="0: Didn't work last 12 months", 1, 0)
df$health.insurance <- 1- df$health.insurance
df$heard.hiv <- 1- df$heard.hiv
df$heard.hiv.media <- ifelse(df$heard.hiv.media=="2: media", 0, 1)
df$know.hiv.reduce <- ifelse(df$know.hiv.reduce=="2: Yes", 0, 1)
df$know.hiv.misinfo <- ifelse(df$know.hiv.misinfo=="2: Yes", 1, 0)
df$know.hiv.stigma <- ifelse(df$know.hiv.stigma=="2: Yes", 1, 0)
df$medical.transport <- 1- df$medical.transport
df$medical.female.provider <- 1- df$medical.female.provider
df$medical.no.drug <- 1 - df$medical.no.drug
df$know.womanloan.program <- 1- df$know.womanloan.program
df$heard.hiv.community <- ifelse(df$heard.hiv.community=="2: community", 0, 1)
df$source.fp.friend <- 1 - df$source.fp.friend
df$her.use.internet <- 1- df$her.use.internet
df$her.read.text <- 1- df$her.read.text
df$edu.hh.highest <- ifelse(df$edu.hh.highest=="1: no/primary education" | df$edu.hh.highest=="2: incomplete secondary", 1, 0)
df$hh.electricity <- 1 - df$hh.electricity 
df$hh.tv <- 1- df$hh.tv
df$hh.refrig <- 1- df$hh.refrig
df$hh.motor <- 1- df$hh.motor
df$hh.internet <- 1- df$hh.internet
df$hh.ac <- 1- df$hh.ac
df$hh.wash.machine <- 1- df$hh.wash.machine
df$hh.land <- 1- df$hh.land
df$hh.bedrooms <- ifelse(df$hh.bedrooms=="0-1", 1, 0)
df$hh.WASH.toilet <- 1- df$hh.WASH.toilet
df$hh.water.premise <- 1- df$hh.water.premise
df$partner.absent <- ifelse(df$partner.absent=="Yes", 1, 0)
df$hh.cook.fuel.nosmoke <- 1 - df$hh.cook.fuel.nosmoke
df$media <- 1 - df$media

################################################################ URBAN SOLUTIONS 
df <- merge(df, v025, by="caseid")
df <- df %>% filter(v025=="urban")

lca_vars <- read_excel("India_constructed_data/UP_DHS_LCAplusPCAresults.xlsx", sheet="map")

lca_vars <- lca_vars %>% filter(is.na(urban_univarite_drop) & is.na(urban_PCA_drop) & is.na(urban_LCA))
lca_vars <- names(df)[(names(df) %in% lca_vars$urban_factor)]
# lca_vars <- lca_vars %>% filter(is.na(rural_univarite_drop) & is.na(rural_PCA_drop) & is.na(rural_LCA))
# lca_vars <- names(vulnerability)[(names(vulnerability) %in% lca_vars$rural_factor)]
df <- df %>% dplyr::select(all_of(lca_vars))
df <- df %>% drop_na() 
lapply(df, table)
lapply(df, class)
# urban_class <- cbind(df, LCA5$predclass)
# names(urban_class)[length(names(urban_class))] <-"class" 
# saveRDS(urban_class, file="urban_class_LCA5.RData")

### re-order these variables to the order of the list of vars shown in descriptives 
df <- df %>% select(
  caste, 
  ed.level	,
  ed.any	,
  edu.hh.highest	,
  early.sex.18	,
  cohab.birth.months	,
  dv.accept	,
  ipv_physical	,
  num.under5.cat	,
  hh.kid.wom.rat1	,
  hh.members.cat	,
  num.kids.house.cat	,
  num.births.cat	,
  married.10years	,
  his.older	,
  ed.lev.his	,
  medical.permit	,
  say.no.sex.sti	,
  her.own.money	,
  her.own.phone	,
  joint.large	,
  work.current	,
  work.12mo	,
  work.type	,
  work.seasonal	,
  work.type.his	,
  hh.electricity	,
  hh.tv	,
  hh.refrig	,
  hh.motor	,
  hh.internet	,
  hh.ac	,
  hh.wash.machine	,
  hh.land	,
  medical.transport	,
  medical.female.provider	,
  medical.no.drug	,
  heard.hiv	,
  heard.hiv.media	,
  know.hiv.reduce	,
  know.hiv.misinfo	,
  know.hiv.stigma	,
  source.fp.friend	,
  heard.hiv.community	,
  hh.cook.fuel.nosmoke	,
  hh.bedrooms	,
  media	,
  bedroom.slum	
)

# df5 <- cbind(df, urban_LCA5$predclass)
# df6 <- cbind(df, urban_LCA6$predclass)
# df7 <- cbind(df, urban_LCA7$predclass)


### VARS NEED TO BE IN THE SAME ORDER AS THE MAP
df2 <- cbind(df, urban_LCA2$predclass)
table_urban_LCA2 <- 
  df %>%
  group_by(urban_LCA2$predclass) %>%
  summarise_all("mean")
table_urban_LCA2 = t(table_urban_LCA2)
table_urban_LCA2 <- as.data.frame(table_urban_LCA2)
table_urban_LCA2 = table_urban_LCA2[-1,]
names(table_urban_LCA2) <- c('k2_1', 'k2_2')

df3 <- cbind(df, urban_LCA3$predclass)
table_urban_LCA3 <- 
  df %>%
  group_by(urban_LCA3$predclass) %>%
  summarise_all("mean")
table_urban_LCA3 = t(table_urban_LCA3)
table_urban_LCA3 <- as.data.frame(table_urban_LCA3)
table_urban_LCA3 = table_urban_LCA3[-1,]
names(table_urban_LCA3) <- c('k3_1', 'k3_2', 'k3_3')

df4 <- cbind(df, urban_LCA4$predclass)
table_urban_LCA4 <- 
  df %>%
  group_by(urban_LCA4$predclass) %>%
  summarise_all("mean")
table_urban_LCA4 = t(table_urban_LCA4)
table_urban_LCA4 <- as.data.frame(table_urban_LCA4)
table_urban_LCA4 = table_urban_LCA4[-1,]
names(table_urban_LCA4) <- c('k4_1', 'k4_2', 'k4_3', 'k4_4')

df5 <- cbind(df, urban_LCA5$predclass)
table_urban_LCA5 <- 
  df5 %>%
  group_by(urban_LCA5$predclass) %>%
  summarise_all("mean")
table_urban_LCA5 = t(table_urban_LCA5)
table_urban_LCA5 <- as.data.frame(table_urban_LCA5)
table_urban_LCA5 = table_urban_LCA5[-1,]
names(table_urban_LCA5) <- c('k5_1', 'k5_2', 'k5_3', 'k5_4', "k5_5")

df6 <- cbind(df, urban_LCA6$predclass)
table_urban_LCA6 <- 
  df6 %>%
  group_by(urban_LCA6$predclass) %>%
  summarise_all("mean")
table_urban_LCA6 = t(table_urban_LCA6)
table_urban_LCA6 <- as.data.frame(table_urban_LCA6)
table_urban_LCA6 = table_urban_LCA6[-1,]
names(table_urban_LCA6) <- c('k6_1', 'k6_2', 'k6_3', 'k6_4', "k6_5", "k6_6")

df7 <- cbind(df, urban_LCA7$predclass)
table_urban_LCA7 <- 
  df7 %>%
  group_by(urban_LCA7$predclass) %>%
  summarise_all("mean")
table_urban_LCA7 = t(table_urban_LCA7)
table_urban_LCA7 <- as.data.frame(table_urban_LCA7)
table_urban_LCA7 = table_urban_LCA7[-1,]
names(table_urban_LCA7) <- c('k7_1', 'k7_2', 'k7_3', 'k7_4', "k7_5", "k7_6", "k7_7")

table_urban_class <- cbind(table_urban_LCA2, table_urban_LCA3, table_urban_LCA4, table_urban_LCA5, table_urban_LCA6, table_urban_LCA7)
table_urban_class$factor <- rownames(table_urban_class)
table_urban_class <- table_urban_class %>%
  select(factor, everything())
write.xlsx(table_urban_class, "India_constructed_data/table_urban_class_.xlsx")


################################################################ RURAL SOLUTIONS 
df <- merge(df, v025, by="caseid")
df <- df %>% filter(v025=="rural")

lca_vars <- read_excel("India_constructed_data/UP_DHS_LCAplusPCAresults.xlsx", sheet="map")

# lca_vars <- lca_vars %>% filter(is.na(urban_univarite_drop) & is.na(urban_PCA_drop) & is.na(urban_LCA))
# lca_vars <- names(df)[(names(df) %in% lca_vars$urban_factor)]
lca_vars <- lca_vars %>% filter(is.na(rural_univarite_drop) & is.na(rural_PCA_drop) & is.na(rural_LCA))
lca_vars <- names(vulnerability)[(names(vulnerability) %in% lca_vars$rural_factor)]
df <- df %>% dplyr::select(all_of(lca_vars))
df <- df %>% drop_na() 
lapply(df, table)
lapply(df, class)
# urban_class <- cbind(df, LCA5$predclass)
# names(urban_class)[length(names(urban_class))] <-"class" 
# saveRDS(urban_class, file="urban_class_LCA5.RData")

### re-order these variables to the order of the list of vars shown in descriptives 
df <- df %>% select(
  caste	,
  ed.level	,
  ed.any	,
  edu.hh.highest	,
  early.sex.18	,
  cohab.birth.months	,
  dv.negkid	,
  dv.argue	,
  dv.disrespect	,
  dv.accept	,
  ipv_physical	,
  ipv.any	,
  num.under5.cat	,
  hh.kid.wom.rat1	,
  hh.members.cat	,
  num.kids.house.cat	,
  num.births.cat	,
  partner.absent	,
  married.10years	,
  ed.lev.his	,
  say.no.sex	,
  say.no.sex.sti	,
  say.no.sex.otherwomen	,
  her.own.house	,
  her.own.land	,
  joint.health	,
  joint.large	,
  joint.visit	,
  joint.his.money	,
  work.current	,
  work.12mo	,
  work.seasonal	,
  work.type.his	,
  work.12mo.his	,
  hh.tv	,
  hh.refrig	,
  hh.motor	,
  hh.internet	,
  hh.ac	,
  hh.wash.machine	,
  hh.land	,
  health.insurance	,
  medical.transport	,
  medical.female.provider	,
  heard.hiv	,
  heard.hiv.media	,
  know.hiv.reduce	,
  know.hiv.misinfo	,
  know.hiv.stigma	,
  know.womanloan.program	,
  heard.hiv.community	,
  hh.cook.inside	,
  hh.bedrooms	,
  hh.WASH.toilet	,
  hh.water.premise	,
  her.use.internet	,
  her.read.text	,
  floor.slum	,
  bedroom.slum	,
  latrine	
)


### VARS NEED TO BE IN THE SAME ORDER AS THE MAP
df2 <- cbind(df, rural_LCA2$predclass)
table_rural_LCA2 <- 
  df %>%
  group_by(rural_LCA2$predclass) %>%
  summarise_all("mean")
table_rural_LCA2 = t(table_rural_LCA2)
table_rural_LCA2 <- as.data.frame(table_rural_LCA2)
table_rural_LCA2 = table_rural_LCA2[-1,]
names(table_rural_LCA2) <- c('k2_1', 'k2_2')

df3 <- cbind(df, rural_LCA3$predclass)
table_rural_LCA3 <- 
  df %>%
  group_by(rural_LCA3$predclass) %>%
  summarise_all("mean")
table_rural_LCA3 = t(table_rural_LCA3)
table_rural_LCA3 <- as.data.frame(table_rural_LCA3)
table_rural_LCA3 = table_rural_LCA3[-1,]
names(table_rural_LCA3) <- c('k3_1', 'k3_2', 'k3_3')

df4 <- cbind(df, rural_LCA4$predclass)
table_rural_LCA4 <- 
  df %>%
  group_by(rural_LCA4$predclass) %>%
  summarise_all("mean")
table_rural_LCA4 = t(table_rural_LCA4)
table_rural_LCA4 <- as.data.frame(table_rural_LCA4)
table_rural_LCA4 = table_rural_LCA4[-1,]
names(table_rural_LCA4) <- c('k4_1', 'k4_2', 'k4_3', 'k4_4')

df5 <- cbind(df, rural_LCA5$predclass)
table_rural_LCA5 <- 
  df5 %>%
  group_by(rural_LCA5$predclass) %>%
  summarise_all("mean")
table_rural_LCA5 = t(table_rural_LCA5)
table_rural_LCA5 <- as.data.frame(table_rural_LCA5)
table_rural_LCA5 = table_rural_LCA5[-1,]
names(table_rural_LCA5) <- c('k5_1', 'k5_2', 'k5_3', 'k5_4', "k5_5")

df6 <- cbind(df, rural_LCA6$predclass)
table_rural_LCA6 <- 
  df6 %>%
  group_by(rural_LCA6$predclass) %>%
  summarise_all("mean")
table_rural_LCA6 = t(table_rural_LCA6)
table_rural_LCA6 <- as.data.frame(table_rural_LCA6)
table_rural_LCA6 = table_rural_LCA6[-1,]
names(table_rural_LCA6) <- c('k6_1', 'k6_2', 'k6_3', 'k6_4', "k6_5", "k6_6")

df7 <- cbind(df, rural_LCA7$predclass)
table_rural_LCA7 <- 
  df7 %>%
  group_by(rural_LCA7$predclass) %>%
  summarise_all("mean")
table_rural_LCA7 = t(table_rural_LCA7)
table_rural_LCA7 <- as.data.frame(table_rural_LCA7)
table_rural_LCA7 = table_rural_LCA7[-1,]
names(table_rural_LCA7) <- c('k7_1', 'k7_2', 'k7_3', 'k7_4', "k7_5", "k7_6", "k7_7")

df8 <- cbind(df, rural_LCA8$predclass)
table_rural_LCA8 <- 
  df8 %>%
  group_by(rural_LCA8$predclass) %>%
  summarise_all("mean")
table_rural_LCA8 = t(table_rural_LCA8)
table_rural_LCA8 <- as.data.frame(table_rural_LCA8)
table_rural_LCA8 = table_rural_LCA8[-1,]
names(table_rural_LCA8) <- c('k8_1', 'k8_2', 'k8_3', 'k8_4', "k8_5", "k8_6", "k8_7", "k8_8")

df9 <- cbind(df, rural_LCA9$predclass)
table_rural_LCA9 <- 
  df9 %>%
  group_by(rural_LCA9$predclass) %>%
  summarise_all("mean")
table_rural_LCA9 = t(table_rural_LCA9)
table_rural_LCA9 <- as.data.frame(table_rural_LCA9)
table_rural_LCA9 = table_rural_LCA9[-1,]
names(table_rural_LCA9) <- c('k9_1', 'k9_2', 'k9_3', 'k9_4', "k9_5", "k9_6", "k9_7", "k9_8", "k9_9")

table_rural_class <- cbind(table_rural_LCA2, table_rural_LCA3, table_rural_LCA4, table_rural_LCA5, table_rural_LCA6, table_rural_LCA7, table_rural_LCA8, table_rural_LCA9)
table_rural_class$factor <- rownames(table_rural_class)
table_rural_class <- table_rural_class %>%
  select(factor, everything())
write.xlsx(table_rural_class, "India_constructed_data/table_rural_class_.xlsx")

