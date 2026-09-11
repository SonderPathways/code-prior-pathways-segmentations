###################################################################
########             3_Ethiopia_DHS_pastoralism   ##############
########              Ethiopia DHS 2016              ##############
########              Author: Adi Greif              ##############
########          Last update: Feb 24, 2023 AM changed pathfiles for her PC   ##############
########           input files: Data/Ethiopia DHS/ET_2016/iPUMS livelihood data/livelihood_legend_web.csv ###########
########               output files: none.            ##############
########               ancestor scripts: none.      ##############
######## called by script: 4_Ethiopia_DHS_vulnerability ##############
######## note: need to change working directory ################
###################################################################

rm(list=ls())
pacman::p_load(ipumsr, tidyverse, sf, geonames)
#library('ipumsr')
#library(tidyverse)
#library('sf')
#library('geonames')



############## Set directory - change for a different user
setwd("/Users/darcyrao/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Pathways - GE Data & Insights/Modeling") # for use from Darcy's Mac
#setwd("/Users/adigreif/Desktop/Gates Ethiopia/EthiopiaSegmentation/") # for use from my Mac
#setwd("/Users/adigreif/Desktop/Gates Ethiopia/EthiopiaSegmentation/")
#setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/")

############ Read in data

#ddi <- read_ipums_ddi("idhs_00001.xml")
ddi <- read_ipums_ddi('Data/Ethiopia DHS/ET_2016/iPUMS livelihood data/idhs_00001.xml')
data <- read_ipums_micro(ddi)
ipums_conditions()
livelihood <- read.csv("Data/Ethiopia DHS/ET_2016/iPUMS livelihood data/livelihood_legend_web.csv")

pastoralism <- left_join(data[,!names(data) %in% 'COUNTRY'],livelihood,by=c('LIVELIHOOD'='IPUMS.DHS.code'))  #removing country from data because it is same variable as in livelihood but coded as a havel level attribute

####Notes
#Pastoralists mostly live in Afar, Somali, South Omo Zone of SNNP and Borena Zone of Oromia
#NA livelihood zones:
#PSU or clusterno missing LZs: 2, 9, 16, 42, 48, 72, 96, 131, 641 625 561 540 530 525 379 367 356 328 323 301 250 have livelihood zones = -998
#missingness are in strata 9,11 (oromia rural), 13 (somali), 20 (gambela), 25 (dire dawa), assuming that strata are the same as the hv022 and hv023 variables in ETHH16. They seem to be.

#Pastoralist and agro-pastoralist zones:
# pastoralists: LZCODE=ET04 ET05 ET07 ET08 ET09 ET55 ET57 ET82 ET83 ET84 ET137 ET138 ET146 ET149 ET150 ET152 ET156 ET158 ET159 ET162 Pastoral
# agropastoralists:  ET02 ET03 ET06 ET39 ET49 ET56 ET86 ET90 ET107 ET132 ET135 ET147 ET151 ET153 ET155 ET157 ET161 Agro-Pastoral or agropastoral

pastoralism$anypastoralstring <-str_detect(pastoralism$LZNAMEEN, regex("Pastoral", ignore_case = TRUE)) #anypastoral includes agropastoral
pastoralism$agropastoralonlystring <-str_detect(pastoralism$LZNAMEEN, regex("agro", ignore_case = TRUE)) # only agropastoral

pastoralism$anypastoral.yn <-ifelse(pastoralism$anypastoralstring=="TRUE",1,0)
pastoralism$agropastoral.yn <-ifelse(pastoralism$agropastoralonlystring=="TRUE",1,0)
pastoralism$pastoralnoagro.yn <-ifelse(pastoralism$anypastoralstring=="TRUE" & pastoralism$agropastoralonlystring=="FALSE",1,0) #excludes agropastoral. Selected this.

#selecting variables 
pastoralism <- pastoralism %>% select(-c("AGE", "AGE5YEAR", "RESIDENT", "RELIGION", "MARSTAT", "CHEB", "CURRWORK", "WEALTHQ", "WEALTHS", "EDUCLVL", "EDYRTOTAL", "INTERVIEWERIDIR", "INTERVIEWERIDHH", "INTRESULT"))

#############crosstabs to see how pastoralists differ. selected variables used to impute missing pastoralism values############

#Notes on checking that pastoralism data seems reasonable:
#AG: did rough check by 1. googling some of the agropastoralist zones listed and seeing that they seem to exist in the regions located according to various websites found 
#2. seeing that the pastoralists are in the regions we would expect based on where we know they live, roughly (Afar, Somali, South Omo Zone of SNNP and Borena Zone of Oromia). 
#looked at the places where livelihood zones are missing. There are some missing in Oromia rural and in Somali, so it is possible we are missing some pastoralist zones. That said, we have pastoralist zones marked elsewhere in those regions so I don't think we are in danger of systematically missing pastoralists within an entire region.

#pastoralists have far more goats on average, 60 vs 20% or so
#crosstabtest <- dhs.seg %>% filter(URBAN_RURA=="R") %>% group_by(anypastoral.yn, hh.kidwom.rat) %>%   summarise(n = n()) %>%   mutate(     totalN = (cumsum(n)),     percent = round((n / sum(n)), 3),     cumuPer = round(cumsum(freq = n / sum(n)), 3))
#View(crosstabtest)

#crosstabtest <- dhs.seg %>% filter(URBAN_RURA=="R") %>% group_by(anypastoral.yn, asset.goats) %>% summarise(n = n()) %>%   mutate(     totalN = (cumsum(n)),     percent = round((n / sum(n)), 3),     cumuPer = round(cumsum(freq = n / sum(n)), 3))
#View(crosstabtest)

#crosstabtest <- dhs.seg %>%   group_by(dhs.seg$anypastoral.yn, asset.goats) %>%   summarise(n = n()) %>%   mutate(     totalN = (cumsum(n)),     percent = round((n / sum(n)), 3),     cumuPer = round(cumsum(freq = n / sum(n)), 3))
#View(crosstabtest)

#checking that these variables do seem to differ by pastoralist
#meantest <- dhs.seg %>% filter(URBAN_RURA=="R") %>% group_by(anypastoral.yn) %>%
#  summarize_at(c("early.sex.15", "dv.index.yn", "muslim", "ed.lev.ord", "hh.land", "asset.goats", "asset.sheep", "asset.chickens", "toilet.facility", "water", "wealth.index.ur.ord", "hh.cook.inside.yn", "hh.rooms.2plus","hh.kidwom.rat", "hv022"),mean)
#View(meantest)

#now lets look at average goats in missing livelihood zones
#crosstabtest <- dhs.seg %>%   group_by(CLUSTERNO, asset.goats) %>%   summarise(n = n()) %>%   mutate(     totalN = (cumsum(n)),     percent = round((n / sum(n)), 3),     cumuPer = round(cumsum(freq = n / sum(n)), 3))
#View(crosstabtest)

#crosstabtest <- dhs.seg %>%   group_by(anO, mean(hh.kidwom.rat)) %>%   summarise(n = n()) %>%   mutate(     totalN = (cumsum(n)),     percent = round((n / sum(n)), 3),     cumuPer = round(cumsum(freq = n / sum(n)), 3))
#View(crosstabtest)






