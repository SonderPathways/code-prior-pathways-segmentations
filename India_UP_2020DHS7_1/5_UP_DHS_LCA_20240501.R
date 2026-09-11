rm(list=ls())
pacman::p_load("poLCA", "dplyr", "tidyverse", "tidyLPA", "networkD3", "data.table", "ggpubr", "forcats", "readxl")

setwd("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2020 DHS")
vulnerability <- readRDS("India_constructed_data/2_UP_DHS_vulpredictors.RData")
load("India_constructed_data/1_UP_DHS_outcomes.RData")

load("Data/IR.RData")
v025 <- IR %>% select(caseid, v025, v001)


vulnerability <- readRDS("India_constructed_data/2_UP_DHS_vulpredictors.RData")
#merge v025 with vul files 
vulnerability <- merge(vulnerability, v025, by="caseid")
# vulnerability <- vulnerability %>% filter(v025=="urban")
vulnerability <- vulnerability %>% filter(v025=="rural")
lca_vars <- read_excel("India_constructed_data/UP_DHS_LCAplusPCAresults.xlsx", sheet="map")
# 
# lca_vars <- lca_vars %>% filter(is.na(urban_univarite_drop) & is.na(urban_PCA_drop) & is.na(urban_LCA))
# lca_vars <- names(vulnerability)[(names(vulnerability) %in% lca_vars$urban_factor)]
lca_vars <- lca_vars %>% filter(is.na(rural_univarite_drop) & is.na(rural_PCA_drop) & is.na(rural_LCA))
lca_vars <- names(vulnerability)[(names(vulnerability) %in% lca_vars$rural_factor)]

lca_vars

df <- vulnerability %>% dplyr::select(all_of(lca_vars))
colSums(is.na(df))
df <- df %>% drop_na() 
lapply(df, class)
lapply(df, table)

cat.var <- names(which(sapply(df, is.character)))
num.vars <- names(which(sapply(df, is.numeric)))

for (i in 1:length(num.vars)){
  df[,num.vars[i]] <- df[,num.vars[i]]+1
  print(table(df[,num.vars[i]]))
}

## setting nominal to be coded as 1,2,...
for(i in 1:length(cat.var)){
  var.class <- class(df[,cat.var[i]])
  if(var.class=='factor'){
    df[,cat.var[i]] <- as.numeric(df[,cat.var[i]])
  }
  if(var.class=='character'){
    df[,cat.var[i]] <- as.numeric(as.factor(df[,cat.var[i]]))
  }
  print(table(df[,cat.var[i]]))
}
colnames(df)

# set.seed(1234)
set.seed(99)
formula <- as.formula(paste("cbind(",paste(names(df),collapse=","),")~1",sep=""))

### fitting 2-7 classes 
for(n in 2:12){
  name <- paste0('LCA', n)
  data <- poLCA(formula, data=df, nclass=n, maxiter=100, nrep=10)
  assign(name, data)
}

# saveRDS(LCA2, file = "India_constructed_data/urban_LCA2.rds")
# saveRDS(LCA3, file = "India_constructed_data/urban_LCA3.rds")
# saveRDS(LCA4, file = "India_constructed_data/urban_LCA4.rds")
# saveRDS(LCA5, file = "India_constructed_data/urban_LCA5.rds")
# saveRDS(LCA6, file = "India_constructed_data/urban_LCA6.rds")
# saveRDS(LCA7, file = "India_constructed_data/urban_LCA7.rds")

saveRDS(LCA2, file = "India_constructed_data/rural_LCA2.rds")
saveRDS(LCA3, file = "India_constructed_data/rural_LCA3.rds")
saveRDS(LCA4, file = "India_constructed_data/rural_LCA4.rds")
saveRDS(LCA5, file = "India_constructed_data/rural_LCA5.rds")
saveRDS(LCA6, file = "India_constructed_data/rural_LCA6.rds")
saveRDS(LCA7, file = "India_constructed_data/rural_LCA7.rds")
saveRDS(LCA8, file = "India_constructed_data/rural_LCA8.rds")
saveRDS(LCA9, file = "India_constructed_data/rural_LCA9.rds")
saveRDS(LCA10, file = "India_constructed_data/rural_LCA10.rds")
saveRDS(LCA11, file = "India_constructed_data/rural_LCA11.rds")
saveRDS(LCA12, file = "India_constructed_data/rural_LCA12.rds")


### BIC 
myds <- c(LCA2$bic,
          LCA3$bic,
          LCA4$bic,
          LCA5$bic,
          LCA6$bic,
          LCA7$bic,
          LCA8$bic,
          LCA9$bic,
          LCA10$bic,
          LCA11$bic,
          LCA12$bic)
x <- (1:length(myds))
#make the main plot
plot(myds, xaxt = "n", type="b", 
     xlab="Solutions", ylab="BIC", main="Bayesian Information Criterion (BIC)")
axis(1, at=1:11, labels=c("2-class",
                         "3-class",
                         "4-class", 
                         "5-class", 
                         "6-class", 
                         "7-class",
                         "8-class",
                         "9-class",
                         "10-class",
                         "11-class",
                         "12-class"))

prop.table(table(LCA2$predclass))
prop.table(table(LCA3$predclass))
prop.table(table(LCA4$predclass)) 
prop.table(table(LCA5$predclass))
prop.table(table(LCA6$predclass)) 
prop.table(table(LCA7$predclass)) 
prop.table(table(LCA8$predclass)) 
prop.table(table(LCA9$predclass)) 
prop.table(table(LCA10$predclass)) #lowest BIC, smallest class=6%
prop.table(table(LCA11$predclass)) 
prop.table(table(LCA12$predclass)) 



###merge class membership back with original raw data 
vulnerability <- readRDS("India_constructed_data/2_UP_DHS_vulpredictors.RData")
#merge v025 with vul files 
vulnerability <- merge(vulnerability, v025, by="caseid")
# vulnerability <- vulnerability %>% filter(v025=="urban")
vulnerability <- vulnerability %>% filter(v025=="rural")
lca_vars <- read_excel("India_constructed_data/UP_DHS_LCAplusPCAresults.xlsx", sheet="map")

# lca_vars <- lca_vars %>% filter(is.na(urban_univarite_drop) & is.na(urban_PCA_drop) & is.na(urban_LCA))
# lca_vars <- names(vulnerability)[(names(vulnerability) %in% lca_vars$urban_factor)]
lca_vars <- lca_vars %>% filter(is.na(rural_univarite_drop) & is.na(rural_PCA_drop) & is.na(rural_LCA))
lca_vars <- names(vulnerability)[(names(vulnerability) %in% lca_vars$rural_factor)]
df <- vulnerability %>% dplyr::select(all_of(lca_vars))
df <- df %>% drop_na() 
lapply(df, table)
lapply(df, class)
# urban_class <- cbind(df, LCA5$predclass)
# names(urban_class)[length(names(urban_class))] <-"class" 
# saveRDS(urban_class, file="urban_class_LCA5.RData")



rural_class <- cbind(df, LCA10$predclass)
names(rural_class)[length(names(rural_class))] <-"class" 
saveRDS(rural_class, file="India_constructed_data/rural_class_LCA10.RData")


df <- readRDS("India_constructed_data/rural_class_LCA10.RData")

##make all vars into binary: higher percentage means better, low=disadvantaged 
df$caste <- ifelse(df$caste=="1: Schedule caste/tribe", 1, 0)
df$ed.level <- ifelse(df$ed.level=="0: no education" | df$ed.level=="1: incomplete primary", 1, 0)
df$ed.any <- 1 - df$ed.any
df$cohab.birth.months <- ifelse(df$cohab.birth.months=="3: 2-3 years" | df$cohab.birth.months=="4: >3 years", 0, 1)
df$num.births.cat <- ifelse(df$num.births.cat=="4" | df$num.births.cat=="5+", 1, 0)
df$married.10years <- 1- df$married.10years
df$num.under5.cat <- ifelse(df$num.under5.cat=="2+", 1, 0)
df$hh.members.cat <- ifelse(df$hh.members.cat>=3, 1, 0)
df$num.kids.house.cat <- ifelse(df$num.kids.house.cat=="3+", 1, 0)
df$ed.lev.his <- ifelse(df$ed.lev.his=="0: no education" | df$ed.lev.his=="1: incomplete primary", 1, 0)
df$say.no.sex <- 1- df$say.no.sex
df$say.no.sex.sti <- 1- df$say.no.sex.sti
df$say.no.sex.otherwomen <- 1- df$say.no.sex.otherwomen
df$joint.health <- 1- df$joint.health
df$joint.large <- 1- df$joint.large
df$joint.visit <- 1- df$joint.visit
df$joint.his.money <- 1- df$joint.his.money
df$her.own.house <- 1- df$her.own.house
df$her.own.land <- 1- df$her.own.land
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
df$know.womanloan.program <- 1- df$know.womanloan.program
df$heard.hiv.community <- ifelse(df$heard.hiv.community=="2: community", 0, 1)
df$her.use.internet <- 1- df$her.use.internet
df$her.read.text <- 1- df$her.read.text
df$edu.hh.highest <- ifelse(df$edu.hh.highest=="1: no/primary education" | df$edu.hh.highest=="2: incomplete secondary", 1, 0)
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

lapply(df, table)
lapply(df, class)
df <- df %>% select(
  class,
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
  latrine	)

### VARS NEED TO BE IN THE SAME ORDER AS THE MAP

table_rural_LCA10 <- 
  df %>%
  group_by(class) %>%
  summarise_all("mean")
table_rural_LCA10 = t(table_rural_LCA10)
table_rural_LCA10 <- as.data.frame(table_rural_LCA10)
table_rural_LCA10 = table_rural_LCA10[-1,]
names(table_rural_LCA10) <- c("class1", "class2", 'class3', 'class4', 'class5',
                              "class6", "class7", 'class8', 'class9', 'class10')
write.csv(table_rural_LCA10, "India_constructed_data/table_rural_LCA10.csv")

## plot sankey diagram 

