############## 
# 10a2_Ethiopia_DHS_cluster_profiles
# Purpose: Ethiopia segment profiling
# Constructed by ER
# Data source: Ethiopia DHS 2016, Recode 7 and gadm41_ETH.gpkg
# Purpose: Describe the segments and create plots to visualize profiles, also merges back with IR and HH dhS data 
# input data: dhs_urban_noage.rds, dhs_rural_noage.rds, ETIR71FL.DTA, ETHR71FL.DTA, 7_Construct_mapping_external_final.xlsx",sheet=1 and final_var_list, gadm41_ETH.gpkg
# called script: Code/10a1_Ethiopia_DHS_cluster_plots_DR.R
### edits by AG April 15: pastoralnoagro.yn instead of anypastoral.yn, they are not the same - imputed here with errors. Also changed name of script to a2 and name of ##called script. Change excel used to 7_Construct_mapping_external_final from 7_Construct mapping, note this has final variable decisions marked by hand 
#note: each user needs to change the working directory
############## 

############## Setup ####

rm(list=ls())
pacman::p_load(foreign, ggplot2, dplyr, tidyr, data.table, psych, vtable, corrplot, haven, survey, naniar, UpSetR, readxl, sf, 
               openxlsx, srvyr, hrbrthemes, scales, forcats, ggsn, here, readr, sf, colorspace)

options(max.print = 10000)

# Set directory - change for a different user
setwd("/Users/darcyrao/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Pathways - GE Data & Insights/Modeling") # for use from Darcy's Mac
setwd("C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways - GE Data & Insights\\Modeling") # for use from Elisabeth's PC

# Load data
dhs.seg.urban <- readRDS("Data/Ethiopia_constructed_data/LCA_urban_noage_data/dhs_urban_noage.rds")
dhs.seg.rural <- readRDS("Data/Ethiopia_constructed_data/LCA_rural_noage_data/dhs_rural_noage.rds")

# Merge urban and rural
dhs.seg.comb <- merge(dhs.seg.rural, dhs.seg.urban, all = TRUE)

# Bring in other variables from the original data frame
IR <- data.table(read.dta("Data/Ethiopia DHS/ET_2016/ETIR71DT/ETIR71FL.DTA")) 
HH <- data.table(read.dta("Data/Ethiopia DHS/ET_2016/ETHR71DT/ETHR71FL.DTA")) 

HH$region <- HH$hv024

IR <- IR %>% mutate(internet.ever = case_when(
  v171a %in% c("never") ~ 0,
  v171a %in% c("yes, last 12 months", "yes, before last 12 months", "yes, can't establish when")  ~ 1
))

IR <- IR %>% mutate(internet.pastyr = case_when(
  v171a %in% c("never", "yes, before last 12 months") ~ 0,
  v171a %in% c("yes, last 12 months")  ~ 1,
  v171a %in% c("yes, can't establish when") ~ 98
))

IR$internet.freq.pastmo <- IR$v171b

IR$mobile.own <- ifelse(IR$v169a == "yes", 1, 0)
IR$mobile.finance <- ifelse(IR$v169b == "yes", 1, 0)

IR$bankaccount.personal <- IR$v170

HH$v001<-HH$hv001
HH$v002<-HH$hv002
IR$wt<-IR$v005/1000000
  IR.merge <- IR %>% select(v001, v002, caseid, internet.ever, internet.pastyr, internet.freq.pastmo, mobile.own, mobile.finance, bankaccount.personal)
HH.merge <- HH %>% select(v001, v002, region)

newmerge <-merge(IR.merge, HH.merge, by=c("v001", "v002"), all.x=T)
dhs.seg.comb <- merge(dhs.seg.comb, newmerge, by="caseid", all.x=T)
  
## Make new binary vars
dhs.seg.comb$med.index.4plus <- ifelse(dhs.seg.comb$med.index>=4,1,0)

dhs.seg.comb$not.working <- ifelse(dhs.seg.comb$work.seasonal %in% c("No work in the past 12 months"), 1,
                                                                       ifelse(dhs.seg.comb$work.seasonal %in% c("All  year", "Occassional", "Seasonal"), 0, NA))

dhs.seg.comb$wealth.index.ur.poorest <- ifelse(dhs.seg.comb$wealth.index.ur %in% c("poorest"), 1,
                                        ifelse(dhs.seg.comb$wealth.index.ur %in% c("poorer", "middle", "richer", "richest"), 0, NA))

dhs.seg.comb$jdwd.index.under5 <- ifelse(dhs.seg.comb$jdwd.index.cat %in% c("None", "One to four"), 1,0)

dhs.seg.comb$dv.index.5 <- ifelse(dhs.seg.comb$dv.index>=5,1,0)

#AG already now defined in vunerability file 
#dhs.seg.comb$conflict_incidents_26plus <- ifelse(dhs.seg.comb$conflict_incidents %in% c("26-50", "50+"), 1,
#                                                  ifelse(dhs.seg.comb$conflict_incidents %in% c("0-25"), 0, NA))

dhs.seg.comb$HHedyrs_under7 <- ifelse(dhs.seg.comb$highestyearsedinHH.yrs %in% c("0-7"), 1,
                                       ifelse(dhs.seg.comb$highestyearsedinHH.yrs %in% c("7+"), 0, NA))


## Reverse some binary measures to make them an indicator of more vulnerability
dhs.seg.comb$no.media <- ifelse(dhs.seg.comb$any.media.yn == 0, 1, 0)
dhs.seg.comb$no.educ <- ifelse(dhs.seg.comb$anyed.yn == 0, 1, 0)
dhs.seg.comb$no.electricity <- ifelse(dhs.seg.comb$hh.electricity == 0, 1, 0)
dhs.seg.comb$partner.not.working <- ifelse(dhs.seg.comb$partner.working %in% c("Didn't work last 12 months"), 1, 0)
dhs.seg.comb$water.unimproved <- dhs.seg.comb$water


## Set the cluster variable to use
dhs.seg.comb$cluster <- dhs.seg.comb$cluster_k5_rename_short
# dhs.seg.comb <- dhs.seg.comb %>% mutate(cluster = case_when (
#                 cluster_k5_rename_short == "RM.1" ~ "RM.3",
#                 cluster_k5_rename_short == "RM.2" ~ "RM.2",
#                 cluster_k5_rename_short == "RM.3" ~ "RM.1",
#                 cluster_k5_rename_short == "RU.1" ~ "RU.1",
#                 cluster_k5_rename_short == "RU.2" ~ "RU.2",
#                 cluster_k5_rename_short == "UM.1" ~ "UM.3",
#                 cluster_k5_rename_short == "UM.2" ~ "UM.2",
#                 cluster_k5_rename_short == "UM.3" ~ "UM.1",
#                 cluster_k5_rename_short == "UU.1" ~ "UU.1",
#                 cluster_k5_rename_short == "UU.2" ~ "UU.2",
#               )
# )

# Rename regions 
dhs.seg.comb <- dhs.seg.comb %>% mutate(region.1 = case_when(
  region == "tigray" ~ "Tigray",
  region == "afar" ~ "Afar",
  region == "amhara" ~ "Amhara",
  region == "oromia" ~ "Oromia",
  region == "somali" ~ "Somali",
  region == "benishangul" ~ "Benishangul-Gumuz",
  region == "snnpr" ~ "SNNPR",
  region == "gambela" ~ "Gambela",
  region == "harari" ~ "Harari",
  region == "addis adaba" ~ "Addis Ababa",
  region == "dire dawa" ~ "Dire Dawa"))
  

## Create the survey design objects
dhs.seg.comb.des <-svydesign(id=~v021, strata=~v023, weights=~wt, data=dhs.seg.comb)


## read in construct mapping
constr <- readxl::read_excel("Data/Ethiopia_constructed_data/Archive/7_Construct_mapping_external_final.xlsx",sheet="Sheet1")
constr <- constr[!duplicated(constr$f.var.name),]
constr$category[constr$f.var.name=='early.birth.15'] <-  "Woman and her past experiences"
constr <- constr %>% select(f.var.name, category)

## Add other variables re-categorized as binary below to the construct mapping
constr_add <- data.frame(f.var.name = c("med.index.4plus", "not.working", "wealth.index.ur.poorest", "muslim", "jdwd.index.under5", "dv.index.5", "conflict_incidents_26plus", "HHedyrs_under7",
                                        "no.media", "no.educ", "no.electricity", "partner.not.working", "water.unimproved"),
                         category = c("Healthcare and mental models", "Household economics", "Household economics", "Woman and her past experiences", "Household relationships", "Woman and her past experiences",  "Larger environment", "Household relationships",
                                      "Woman and her past experiences", "Woman and her past experiences", "Household economics", "Household economics", "Larger environment"))
constr <- rbind.data.frame(constr, constr_add)
constr$f.var.name[constr$f.var.name=='pastoralnoagro.yn'] <- 'pastoralnoagro.yn'  #AG edit April 15 this had anypastoral.yn, which is different


## Add labels for the plot for the included vars
constr$var.label <- NA
constr$var.label[constr$f.var.name == "med.permis"] <- "Med barrier: permission"
constr$var.label[constr$f.var.name == "early.sex.15"] <- "Early sex (<15)"
constr$var.label[constr$f.var.name == "pastoralnoagro.yn"] <- "Pastoral"
constr$var.label[constr$f.var.name == "med.index.4plus"] <- "Med barriers: 4 or more"
constr$var.label[constr$f.var.name == "not.working"] <- "Woman not working"
constr$var.label[constr$f.var.name == "wealth.index.ur.poorest"] <- "Lowest wealth quintile"
constr$var.label[constr$f.var.name == "muslim"] <- "Muslim"
constr$var.label[constr$f.var.name == "wealth.index.ur.poorest"] <- "Lowest wealth quintile"
constr$var.label[constr$f.var.name == "wealth.index.ur.poorest"] <- "Lowest wealth quintile"
constr$var.label[constr$f.var.name == "wealth.index.ur.poorest"] <- "Lowest wealth quintile"
constr$var.label[constr$f.var.name == "jdwd.index.under5"] <- "Lower decision-making"
constr$var.label[constr$f.var.name == "dv.index.5"] <- "Higher acceptance of DV"
constr$var.label[constr$f.var.name == "conflict_incidents_26plus"] <- "26+ conflict incidents"
constr$var.label[constr$f.var.name == "HHedyrs_under7"] <- "Lower household education (<7 yrs)"
constr$var.label[constr$f.var.name == "no.media"] <- "No media"
constr$var.label[constr$f.var.name == "no.educ"] <- "No education"
constr$var.label[constr$f.var.name == "no.electricity"] <- "No electricity"
constr$var.label[constr$f.var.name == "partner.not.working"] <- "Partner not working"
constr$var.label[constr$f.var.name == "water.unimproved"] <- "Water unimproved"

## categorize variables as binary/categorical/continuous
var <- readxl::read_xlsx("Data/Ethiopia_constructed_data/Archive/7_Construct_mapping_external_final.xlsx",sheet = "final_var_list")
var <- var[is.na(var$Cut),]
var <- var[!duplicated(var$f.var.name),]
var$f.var.name[var$f.var.name=='anypastoral.yn'] <- 'pastoralnoagro.yn'

con.var <- c(var$f.var.name[var$variable_type=='continuous'])
con.var <- con.var[-5]
con.var <- c(con.var, "age.yrs")
cat.var <- c(var$f.var.name[var$variable_type=='categorical' | var$variable_type=='ordinal'],'age.adj','conflict_incidents')
bin.var <- c(var$f.var.name[var$variable_type=='binary'], constr_add$f.var.name)
bin.var <- bin.var[-1] # remove working y.n in favor of the not.working var below
bin.var <- bin.var[-c(1,2,4,5,6)] # remove original versions in favor of the flipped binary versions made below


############## Population breakdown #### 

pop.by.cluster <- as.data.frame(prop.table(svytable(~cluster, dhs.seg.comb.des)))
pop.by.cluster$total <- svytotal(~cluster, dhs.seg.comb.des)
pop.by.cluster$urban <- ifelse(grepl('^U', pop.by.cluster$cluster), "Urban", "Rural")
pop.by.cluster

# Stacked bar charts
pcolors <- c(RColorBrewer::brewer.pal(5, "BuPu"), RColorBrewer::brewer.pal(5, "OrRd"))

ggplot(pop.by.cluster, aes(fill = cluster, y=Freq, x = urban, width = 0.5)) + 
  geom_bar(position = "stack", stat = "identity", color = "grey80") + 
  scale_fill_manual(values = pcolors) + 
  xlab("") + ylab("Percent of total") + 
  theme_ipsum(axis_title_size = 14, axis_text_size = 12)  +  theme(legend.position = "none") 


  # ## Highlight each cluster at one time
  #   # UM1  
  #   ggplot(pop.by.cluster, aes(fill = cluster, color = cluster, y=Freq, x = urban, width = 0.5)) + 
  #       geom_bar(position = "stack", stat = "identity") + 
  #       scale_fill_manual(values = c(rep("grey90", 5), pcolors[6], rep("grey90", 4))) + 
  #       scale_color_manual(values = c(rep("grey90", 5), "grey20", rep("grey90", 4))) +
  #       xlab("") + ylab("Percent of total") + 
  #       theme_ipsum()  +  theme(legend.position = "none") 
  #   
  #   # UM2
  #   ggplot(pop.by.cluster, aes(fill = cluster, color = cluster, y=Freq, x = urban, width = 0.5)) + 
  #     geom_bar(position = "stack", stat = "identity") + 
  #     scale_fill_manual(values = c(rep("grey90", 6), pcolors[7], rep("grey90", 3))) + 
  #     scale_color_manual(values = c(rep("grey90", 6), "grey20", rep("grey90", 3))) +
  #     xlab("") + ylab("Percent of total") + 
  #     theme_ipsum()  +  theme(legend.position = "none") 
  #   
  # 
  # 
 
############## Profiles ####

source("Code/10a1_Ethiopia_DHS_cluster_plots_DR.R")

# Heatmaps - main vulnerability vars
heatmap.bin(dhs.seg.comb, urban = TRUE, plot_text= TRUE)
heatmap.bin(dhs.seg.comb, urban = FALSE, plot_text = TRUE)


#### Deep dive - plots ####
## Age

agegpXclust <- data.frame(svytable(~age.adj+cluster, dhs.seg.comb.des))
#i <- 1 # set the number for the cluster, in order "RM.1" "RM.2" "RM.3" "RU.1" "RU.2" "UM.1" "UM.2" "UM.3" "UU.1" "UU.2"
for(i in 1:10){
agegpXclust_p <- agegpXclust %>%
                  filter(cluster == levels(agegpXclust$cluster)[i]) %>%
                  mutate(percent = Freq/sum(Freq),
                  label = percent(percent, round(2)))
ymax <- max(agegpXclust_p$percent + 0.02)
clust <- levels(agegpXclust$cluster)[i]
agegpXclust_p %>%
  ggplot(aes(x=age.adj, y = percent)) +
  geom_col(fill = pcolors[i], colour = "grey20") +
  geom_text(aes(label = label), vjust = -0.5, size = 5) +
  xlab("") + ylab("") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16)) + ylim(c(0,ymax)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p_filepath <- paste0("Presentations/Profile_plots/","age_",clust,".png")
ggsave(p_filepath, width = 20, height = 10, units = "cm")
}

## Religion
religionXclust <- data.frame(svytable(~religion+cluster, dhs.seg.comb.des)) 
religionXclust <- religionXclust %>% mutate(religion_lab = case_when(
  religion == "orthodox" ~ "Orthodox",
  religion == "catholic" ~ "Catholic",
  religion == "protestant" ~ "Protestant",
  religion == "muslin" ~ "Muslim",
  religion == "traditional" ~ "Traditional",
  religion == "other" ~ "Other",
))
#i <- 1 # set the number for the cluster, in order "RM.1" "RM.2" "RM.3" "RU.1" "RU.2" "UM.1" "UM.2" "UM.3" "UU.1" "UU.2"
for(i in 1:10){
  religionXclust_p <- religionXclust %>%
    filter(cluster == levels(religionXclust$cluster)[i]) %>%
    mutate(percent = Freq/sum(Freq),
           label = percent(percent, round(2)))
  ymax <- max(religionXclust_p$percent + 0.02)
  clust <- levels(religionXclust$cluster)[i]
  religionXclust_p %>%
    ggplot(aes(x=religion_lab, y = percent)) +
    geom_col(fill = pcolors[i], colour = "grey20") +
    geom_text(aes(label = label), vjust = -0.5, size = 5) +
    xlab("") + ylab("") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16)) + ylim(c(0,ymax)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  p_filepath <- paste0("Presentations/Profile_plots/","religion_",clust,".png")
  ggsave(p_filepath, width = 20, height = 10, units = "cm")
}

## Wealth index
wealthXclust <- data.frame(svytable(~wealth.index.ur+cluster, dhs.seg.comb.des)) 
wealthXclust <- wealthXclust %>% mutate(wealth_lab = case_when(
  wealth.index.ur == "poorest" ~ "Poorest",
  wealth.index.ur == "poorer" ~ "Poorer",
  wealth.index.ur == "middle" ~ "Middle",
  wealth.index.ur == "richer" ~ "Richer",
  wealth.index.ur == "richest" ~ "Richest")) 
wealthXclust$wealth_lab = factor(wealthXclust$wealth_lab, levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))
#i <- 1 # set the number for the cluster, in order "RM.1" "RM.2" "RM.3" "RU.1" "RU.2" "UM.1" "UM.2" "UM.3" "UU.1" "UU.2"
for(i in 1:10){
  wealthXclust_p <- wealthXclust %>%
    filter(cluster == levels(wealthXclust$cluster)[i]) %>%
    mutate(percent = Freq/sum(Freq),
           label = percent(percent, round(2)))
  ymax <- max(wealthXclust_p$percent + 0.02)
  clust <- levels(wealthXclust$cluster)[i]
  wealthXclust_p %>%
    ggplot(aes(x=wealth_lab, y = percent)) +
    geom_col(fill = pcolors[i], colour = "grey20") +
    geom_text(aes(label = label), vjust = -0.5, size = 5) +
    xlab("") + ylab("") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16)) + ylim(c(0,ymax)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  p_filepath <- paste0("Presentations/Profile_plots/","wealth_",clust,".png")
  ggsave(p_filepath, width = 20, height = 10, units = "cm")
}


#### Deep dive - stats ####
# set cluster
clust <- "RU.2" # set the number for the cluster, in order "RM.1" "RM.2" "RM.3" "RU.1" "RU.2" "UM.1" "UM.2" "UM.3" "UU.1" "UU.2"

## Region
data.frame(svytable(~region+cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>%
  filter(cluster == clust) 

## Age
svymean(~age.yrs, subset(dhs.seg.comb.des, cluster == clust))
svyquantile(~age.yrs,  subset(dhs.seg.comb.des, cluster == clust), quantiles = c(0.25, 0.5, 0.75))

## Partnership status
data.frame(svytable(~pship.status + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svymean(~age.1stcohab, subset(dhs.seg.comb.des, cluster == clust), na.rm = T)) 
svyquantile(~age.1stcohab,  subset(dhs.seg.comb.des, cluster == clust), quantiles = c(0.25, 0.5, 0.75))

## Male headed
data.frame(svytable(~head.sex+cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>%
  filter(head.sex == "male") %>%
  filter(cluster == clust) 

## Household structure

data.frame(svytable(~polygamy + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~partner.absent + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~num.15up.cat + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf) 

data.frame(svytable(~num.under5 + cluster, dhs.seg.comb.des)) %>%  #Number of kids in the household #AG: under age 5
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)
data.frame(svymean(~num.under5, subset(dhs.seg.comb.des, cluster == clust))) 

data.frame(svymean(~hh.kidwom.rat, subset(dhs.seg.comb.des, cluster == clust)))
svyquantile(~hh.kidwom.rat,  subset(dhs.seg.comb.des, cluster == clust), quantiles = c(0.25, 0.5, 0.75))

## Education
data.frame(svytable(~ed.lev + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2),
         cum_pct = cumsum(percent)/sum(percent),
         cum_pct_inv = 1-cum_pct) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~highestyearsedinHH.yrs + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

## Work

data.frame(svytable(~work.seasonal + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~work.type + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~work.type.prof + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~partner.not.working + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

## Economics
data.frame(svytable(~wealth.index.ur + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~hh.motor + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~hh.car + cluster, dhs.seg.comb.des)) %>%  
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~hh.bike + cluster, dhs.seg.comb.des)) %>%  
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~bankaccount.personal + cluster, dhs.seg.comb.des)) %>%  
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

## Digital connectivity

data.frame(svytable(~mobile.own + cluster, dhs.seg.comb.des)) %>%  
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~mobile.finance + cluster, dhs.seg.comb.des)) %>%  
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~internet.ever + cluster, dhs.seg.comb.des)) %>%  
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~internet.pastyr + cluster, dhs.seg.comb.des)) %>%  
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~internet.freq.pastmo + cluster, dhs.seg.comb.des)) %>%  
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)


## Reproductive history
data.frame(svytable(~num.preg + cluster, dhs.seg.comb.des)) %>%  
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)
data.frame(svymean(~num.preg, subset(dhs.seg.comb.des, cluster == clust))) 
svyquantile(~num.preg,  subset(dhs.seg.comb.des, cluster == clust), quantiles = c(0.25, 0.5, 0.75))

data.frame(svymean(~age.1stbrth, subset(dhs.seg.comb.des, cluster == clust), na.rm = T)) 
svyquantile(~age.1stbrth,  subset(dhs.seg.comb.des, cluster == clust), quantiles = c(0.25, 0.5, 0.75), na.rm=T)

## Decision-making
data.frame(svytable(~jdwd.index + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2),
         cum_pct = cumsum(percent)/sum(percent),
         cum_pct_inv = 1-cum_pct) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

## Med index
data.frame(svytable(~med.index + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2),
         cum_pct = cumsum(percent)/sum(percent)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~med.cost + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~med.permis + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~med.dist + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~med.alone + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

## Domestic violence
data.frame(svytable(~dv.physical + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~dv.emotional + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~dv.sexual + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~violence_other + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)


## Larger environment
data.frame(svytable(~hh.land + cluster, dhs.seg.comb.des)) %>% 
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~pastoralnoagro.yn + cluster, dhs.seg.comb.des)) %>%  
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~hh.electricity + cluster, dhs.seg.comb.des)) %>%  
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~latrine + cluster, dhs.seg.comb.des)) %>%  
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)

data.frame(svytable(~water.unimproved + cluster, dhs.seg.comb.des)) %>%  
  group_by(cluster) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  filter(Freq>0) %>%
  filter(cluster == clust) %>%
  print(n=Inf)


#### Map of clusters by region ####

#remotes::install_github("rspatial/terra")
#eth <- gadm(country="ETH", level=1, path=tempdir())

# Remote file information
u_remote <- "https://geodata.ucdavis.edu/"
p_remote <- "gadm/gadm4.1/gpkg/"
f_name <- "gadm41_ETH.gpkg"

# Local file location to save to
ethiopia_rds <- file.path(tempdir(), "gadm41_ETH.gpkg")

# Download data
if (toupper(Sys.info()["sysname"]) == "WINDOWS") {
  download.file(
    url = paste0(u_remote, p_remote, f_name),
    destfile = ethiopia_rds,
    method = "wininet",
    mode = "wb"
  )
} else {
  download.file(
    url = paste0(u_remote, p_remote, f_name),
    destfile = ethiopia_rds,
    method = "auto"
  )
}

st_layers(ethiopia_rds)
ethiopia_sf <- st_read(ethiopia_rds, layer = "ADM_ADM_3")

# Plot regions

ethiopia_regions_sf <-
  ethiopia_sf %>%
  mutate(NAME_1b = gsub("Addis Abeba", "Addis Ababa",
                       NAME_1)) %>%
  mutate(NAME_1b = gsub("Benshangul-Gumaz", "Benishangul-Gumuz",
                        NAME_1b)) %>%
  mutate(NAME_1b = gsub("Gambela Peoples", "Gambela",
                        NAME_1b)) %>%
  mutate(NAME_1b = gsub("Harari People", "Harari",
                        NAME_1b)) %>%
  mutate(NAME_1b = gsub("Southern Nations, Nationalities", "SNNPR",
                        NAME_1b)) %>%
  group_by(NAME_1b) %>%
  summarise() %>%
  ungroup() %>%
  mutate(NAME_1b_lab = gsub("Benishangul-Gumuz", "Benishangul-\nGumuz", NAME_1b)) %>%
  st_as_sf()

ethiopia_regions_ggplot <- 
  ggplot(ethiopia_regions_sf) +
  geom_sf(fill = "white") +
  geom_sf_text(
    aes(label = NAME_1b_lab),
    size = 5,
    hjust = 1
  )

# Second layer, regions in the cluster
pcolors <- c(RColorBrewer::brewer.pal(5, "BuPu"), RColorBrewer::brewer.pal(5, "OrRd"))
pcolors_map <- darken(pcolors, 0.3, method = "relative", space = "HLS")

clust.vec <- c("RM.1", "RM.2", "RM.3", "RU.1", "RU.2", "UM.1", "UM.2", "UM.3", "UU.1", "UU.2")

for(i in 1:10){
  clust <- clust.vec[i]
  regionsXclust <- data.frame(svytable(~region.1 + cluster, subset(dhs.seg.comb.des, cluster == clust))) %>%
                      mutate(percent = Freq/sum(Freq)) %>% 
                      filter(Freq>0) 
  
  regions <- as.vector(regionsXclust$region.1)
  reg1_sf <- filter(ethiopia_regions_sf,
                           NAME_1b %in% regions)
  
  ethiopia_regions_ggplot +
    geom_sf(data = reg1_sf,
            aes(linetype = NAME_1b,
                alpha = NAME_1b), color = "grey20", fill = pcolors_map[i]) +
    scale_alpha_manual(breaks = regions, values = regionsXclust$percent) +
    xlab("") + ylab("") + theme(axis.text=element_blank(), axis.ticks=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = 'transparent', color = NA),
          plot.background = element_rect(fill='transparent', color=NA),
          legend.position = "none")    

  p_filepath <- paste0("Presentations/Profile_plots/","map_",clust,".png")
  ggsave(p_filepath, width = 20, height = 19.83, units = "cm")
}


# ## Pship by working
# UM.1 <- data.frame(svyby(~work.seasonal, ~pship.status, subset(dhs.seg.comb.des, cluster=="UM.1"), svytotal), row.names=NULL)
# UM.1 <- UM.1[,c(1:5)]
# names(UM.1) <- c("Partnership_status", levels(dhs.seg.comb$work.seasonal))
# UM.1[,c(2:5)] <- UM.1[,c(2:5)]/sum(UM.1[,c(2:5)])
# 
# UM.1 %>%
#   gather(work, prop, `All  year`:`Seasonal`, factor_key = TRUE) %>%
#   mutate(work = fct_recode(work, "None" = "No work in the past 12 months", "All year" = "All  year"),
#          Partnership_status = fct_recode(Partnership_status, "Living with partner" = "living with partner", "Married" = "married"),
#          work.ord = fct_relevel(work, "All year", after = 3), 
#          label = percent(prop, round(2))) %>%
#   ggplot(aes(work.ord, Partnership_status, fill= prop)) + 
#   geom_tile(color = "grey") + 
#   geom_text(aes(label = label), color = "grey50", size = 5) +
#   scale_fill_gradient2(low = "#FFFFCC", mid = "#FD8D3C", high = "#800026", midpoint = 0.5,breaks=seq(0,1,0.1)) +
#   xlab('Work (past 12 months)') +ylab('Partnership status') + 
#   theme_ipsum(axis_title_size = 20, axis_text_size = 18, axis_title_just = "rt") + theme(legend.position = "none")
# 
# 
# UM.2 <- data.frame(svyby(~work.seasonal, ~pship.status, subset(dhs.seg.comb.des, cluster=="UM.2"), svytotal), row.names=NULL)
# UM.2 <- UM.2[,c(1:5)]
# names(UM.2) <- c("Partnership_status", levels(dhs.seg.comb$work.seasonal))
# UM.2[,c(2:5)] <- UM.2[,c(2:5)]/sum(UM.2[,c(2:5)])
# 
# UM.2 %>%
#   gather(work, prop, `All  year`:`Seasonal`, factor_key = TRUE) %>%
#   mutate(work = fct_recode(work, "None" = "No work in the past 12 months", "All year" = "All  year"),
#          Partnership_status = fct_recode(Partnership_status, "Living with partner" = "living with partner", "Married" = "married"),
#          work.ord = fct_relevel(work, "All year", after = 3), 
#          label = percent(prop, round(2))) %>%
#   ggplot(aes(work.ord, Partnership_status, fill= prop)) + 
#   geom_tile(color = "grey") + 
#   geom_text(aes(label = label), color = "grey50") +
#   scale_fill_gradient2(low = "#FFFFCC", mid = "#FD8D3C", high = "#800026", midpoint = 0.5,breaks=seq(0,1,0.1)) +
#   xlab('Work (past 12 months)') +ylab('Partnership status') + 
#   theme_ipsum(axis_title_size = 13, axis_text_size = 14) + theme(legend.position = "none")
# 
# 
# UM.3 <- data.frame(svyby(~work.seasonal, ~pship.status, subset(dhs.seg.comb.des, cluster=="UM.3"), svytotal), row.names=NULL)
# UM.3 <- UM.3[,c(1:5)]
# names(UM.3) <- c("Partnership_status", levels(dhs.seg.comb$work.seasonal))
# UM.3[,c(2:5)] <- UM.3[,c(2:5)]/sum(UM.3[,c(2:5)])
# 
# UM.3 %>%
#   gather(work, prop, `All  year`:`Seasonal`, factor_key = TRUE) %>%
#   mutate(work = fct_recode(work, "None" = "No work in the past 12 months", "All year" = "All  year"),
#          Partnership_status = fct_recode(Partnership_status, "Living with partner" = "living with partner", "Married" = "married"),
#          work.ord = fct_relevel(work, "All year", after = 3), 
#          label = percent(prop, round(2))) %>%
#   ggplot(aes(work.ord, Partnership_status, fill= prop)) + 
#   geom_tile(color = "grey") + 
#   geom_text(aes(label = label), color = "grey50") +
#   scale_fill_gradient2(low = "#FFFFCC", mid = "#FD8D3C", high = "#800026", midpoint = 0.5,breaks=seq(0,1,0.1)) +
#   xlab('Work (past 12 months)') +ylab('Partnership status') + 
#   theme_ipsum(axis_title_size = 13, axis_text_size = 14) + theme(legend.position = "none")
# 
# 
# UU.1 <- data.frame(svyby(~work.seasonal, ~pship.status, subset(dhs.seg.comb.des, cluster=="UU.1"), svytotal), row.names=NULL)
# UU.1 <- UU.1[,c(1:5)]
# names(UU.1) <- c("Partnership_status", levels(dhs.seg.comb$work.seasonal))
# UU.1[,c(2:5)] <- UU.1[,c(2:5)]/sum(UU.1[,c(2:5)])
# 
# UU.1 %>%
#   gather(work, prop, `All  year`:`Seasonal`, factor_key = TRUE) %>%
#   mutate(work = fct_recode(work, "None" = "No work in the past 12 months", "All year" = "All  year"),
#          Partnership_status = fct_recode(Partnership_status, "Living apart/separated" = "no longer living together/separated", 
#                                          "Divorced" = "divorced", "Widowed" = "widowed", "Never in union" = "never in union"),
#          work.ord = fct_relevel(work, "All year", after = 3), 
#          label = percent(prop, round(2))) %>%
#   ggplot(aes(work.ord, Partnership_status, fill= prop)) + 
#   geom_tile(color = "grey") + 
#   geom_text(aes(label = label), color = "grey50") +
#   scale_fill_gradient2(low = "#FFFFCC", mid = "#FD8D3C", high = "#800026", midpoint = 0.5,breaks=seq(0,1,0.1)) +
#   xlab('Work (past 12 months)') +ylab('Partnership status') + 
#   theme_ipsum(axis_title_size = 13, axis_text_size = 14) + theme(legend.position = "none")
# 
# 
# UU.2 <- data.frame(svyby(~work.seasonal, ~pship.status, subset(dhs.seg.comb.des, cluster=="UU.2"), svytotal), row.names=NULL)
# UU.2 <- UU.2[,c(1:5)]
# names(UU.2) <- c("Partnership_status", levels(dhs.seg.comb$work.seasonal))
# UU.2[,c(2:5)] <- UU.2[,c(2:5)]/sum(UU.2[,c(2:5)])
# 
# UU.2 %>%
#   gather(work, prop, `All  year`:`Seasonal`, factor_key = TRUE) %>%
#   mutate(work = fct_recode(work, "None" = "No work in the past 12 months", "All year" = "All  year"),
#          Partnership_status = Partnership_status = fct_recode(Partnership_status, "Living apart/separated" = "no longer living together/separated", 
#                                                               "Divorced" = "divorced", "Widowed" = "widowed", "Never in union" = "never in union"),
#          work.ord = fct_relevel(work, "All year", after = 3), 
#          label = percent(prop, round(2))) %>%
#   ggplot(aes(work.ord, Partnership_status, fill= prop)) + 
#   geom_tile(color = "grey") + 
#   geom_text(aes(label = label), color = "grey50") +
#   scale_fill_gradient2(low = "#FFFFCC", mid = "#FD8D3C", high = "#800026", midpoint = 0.5,breaks=seq(0,1,0.1)) +
#   xlab('Work (past 12 months)') +ylab('Partnership status') + 
#   theme_ipsum(axis_title_size = 13, axis_text_size = 14) + theme(legend.position = "none")
# 


