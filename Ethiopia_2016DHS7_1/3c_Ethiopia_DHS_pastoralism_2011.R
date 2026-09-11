###################################################################
########             3c_Ethiopia_DHS_pastoralism_2011   ##############
########              Ethiopia DHS 2011              ##############
########              Author: Adi Greif              ##############
########           purpose: help validate cluster results 2016 ########
######## input files: Data/Ethiopia DHS/ET_2011/iPUMS livelihood data/livelihood_stata_labels_women.csv ###########
########               output files: none.            ##############
########               ancestor scripts: none,       ##############
######## descendant scripts: 4b_Ethiopia_DHS_vulnerability ##############
######### ###### # Note: the working directory needs to be changed when using script  #######
###################################################################

rm(list=ls())

pacman::p_load(ipumsr, tidyverse, sf, geonames)
#library('ipumsr')
#library(tidyverse)
#library('sf')
#library('geonames')


############## Set directory - change for a different user
#setwd("/Users/adigreif/Desktop/Gates Ethiopia/EthiopiaSegmentation/") 
#setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/")

############ Read in data
#pastoralism <- data.table(read_dta("idhs_00002_livelihood.dta")) 
pastoralism_2011 <-  read.csv('Data/Ethiopia DHS/ET_2011/iPUMS livelihood data/livelihood_stata_labels_women.csv')

#ddi <- read_ipums_ddi('Data/Ethiopia DHS/ET_2016/iPUMS livelihood data/idhs_00001.xml')
#data <- read_ipums_micro(ddi)
ipums_conditions()
#livelihood <- read.csv("livelihood_legend_web.csv")

#pastoralism <- left_join(data[,(!names(data) %in% 'COUNTRY')],livelihood,by=c('LIVELIHOOD'='IPUMS.DHS.code'))  #removing country from data because it is same variable as in livelihood but coded as a havel level attribute

####Notes
#Pastoralists mostly live in Afar, Somali, South Omo Zone of SNNP and Borena Zone of Oromia
#NA livelihood zones:
#PSU or clusterno missing LZs: 2, 9, 16, 42, 48, 72, 96, 131, 641 625 561 540 530 525 379 367 356 328 323 301 250 have livelihood zones = -998
#missingness are in strata 9,11 (oromia rural), 13 (somali), 20 (gambela), 25 (dire dawa), assuming that strata are the same as the hv022 and hv023 variables in ETHH16. They seem to be.

#Pastoralist and agro-pastoralist zones:
# pastoralists: LZCODE=ET04 ET05 ET07 ET08 ET09 ET55 ET57 ET82 ET83 ET84 ET137 ET138 ET146 ET149 ET150 ET152 ET156 ET158 ET159 ET162 Pastoral
# agropastoralists:  ET02 ET03 ET06 ET39 ET49 ET56 ET86 ET90 ET107 ET132 ET135 ET147 ET151 ET153 ET155 ET157 ET161 Agro-Pastoral or agropastoral

pastoralism_2011$anypastoralstring <-str_detect(pastoralism_2011$livelihood, regex("Pastoral", ignore_case = TRUE))
pastoralism_2011$agropastoralonlystring <-str_detect(pastoralism_2011$livelihood, regex("agro", ignore_case = TRUE))

pastoralism_2011$anypastoral.yn <-ifelse(pastoralism_2011$anypastoralstring=="TRUE",1,0)
pastoralism_2011$agropastoral.yn <-ifelse(pastoralism_2011$agropastoralonlystring=="TRUE",1,0)
pastoralism_2011$pastoralnoagro.yn <-ifelse(pastoralism_2011$anypastoralstring=="TRUE" & pastoralism_2011$agropastoralonlystring=="FALSE",1,0)

#remove 2016
pastoralism_2011 <- filter(pastoralism_2011, year==2011)
#selecting variables 
#pastoralism <- pastoralism %>% select(-c("AGE", "AGE5YEAR", "RESIDENT", "RELIGION", "MARSTAT", "CHEB", "CURRWORK", "WEALTHQ", "WEALTHS", "EDUCLVL", "EDYRTOTAL", "INTERVIEWERIDIR", "INTERVIEWERIDHH", "INTRESULT"))

#note line numbers don't match since this is men's data


#pastoralism_2011$matchcaseid <-gsub('.{3}$', '', pastoralism_2011$caseid) #remove last number, which is the line number
#pastoralism_2011$matchcaseid <-gsub("[[:space:]]*$","", pastoralism_2011$matchcaseid) #remove last number, which is the line number
pastoralism_2011$LINENO <-sub(".* ", "", pastoralism_2011$caseid)
pastoralism_2011 <- pastoralism_2011 %>% 
  rename(CLUSTERNO = clusterno)  %>%
  rename(HHNUM = hhnum)

saveRDS(pastoralism_2011, file = "3_Ethiopia_DHS_pastoralism_2011.rds")

##crosstabs to see how pastoralists differ     
#Notes on checking that pastoralism data seems reasonable:
#AG: did rough check by 1. googling some of the agropastoralist zones listed and seeing that they seem to exist in the regions located according to various websites found 
#2. seeing that the pastoralists are in the regions we would expect based on where we know they live, roughly (Afar, Somali, South Omo Zone of SNNP and Borena Zone of Oromia). 
#looked at the places where livelihood zones are missing. There are some missing in Oromia rural and in Somali, so it is possible we are missing some pastoralist zones. That said, we have pastoralist zones marked elsewhere in those regions so I don't think we are in danger of systematically missing pastoralists within an entire region.







