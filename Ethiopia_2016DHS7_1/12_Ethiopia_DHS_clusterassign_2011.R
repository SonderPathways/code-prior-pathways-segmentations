####################################### 
# 12_Ethiopia_DHS_clusterassign_2011
# Ethiopia DHS 2011, Recode 6
# input data: vulnerability_2011
# ancestor code: 4b_Ethiopia_DHS_vulnerability_measures_2011
# descendent code: 13_Ethiopia_DHS_2011typingtool_test
# purpose: typing tool
# Note:need to set working directory for each user 
#Note Urban
#UM.1. (married, half working, low income)
#UM.2. (married, working)
#UM.3. (married, not working)
#UU1. unmarried, working
#UU.2. (unmarried, not working)
#
#urban variables used: work.seasonal, pship.cat, religionrecode.cat, any.media.yn, wealth.index.ur.ord, hh.electricity, anyed.yn
#
#Rural
#RM1. married, not working, low income
#RM2. married, not working, higher income
#RM3. married, working
#RU2. unmarried, not working, younger
#RU1. unmarried, working, older
#
#rural variables used: work.seasonal, pship.cat, religionrecode.cat, wealth.index.ur.ord, anyed.yn, conflict_incidents, pastoralnoagro.yn, partner.working.yn, jdwd.index.cat, med.index
##############################################

################# install packages ############

#install.packages('pacman') -- only needs to be done once
rm(list=ls())
pacman::p_load(foreign, ggplot2, dplyr, tidyr, data.table, psych, vtable, corrplot, haven, survey, naniar, UpSetR, readxl, sf, openxlsx, gtools, mice, DescTools)
#library('mice')
#library('vegan')
#library("DescTools")
############## Set directory - change for a different user

#setwd("/Users/darcyrao/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Pathways - GE Data & Insights") # for use from Darcy's Mac
#setwd("C:/Users/darcyra/OneDrive - Bill & Melinda Gates Foundation/Pathways - GE Data & Insights/Modeling") # For use from Darcy's PC
#setwd("/Users/adigreif/Desktop/Gates Ethiopia/EthiopiaSegmentation/")
vulnerability_2011 <- readRDS("Data/Ethiopia_constructed_data/ETH_2011_constructed_data/4c_Ethiopia_DHS_vulnerability_2011.rds")

############ Urban ############
  #getting response options
  #vulnerability_2011  %>% group_by(pship.cat) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))
  #vulnerability_2011  %>% group_by(work.seasonal, working.yn) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))
  #vulnerability_2011  %>% group_by(religionrecode.cat) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))
  #vulnerability_2011  %>% group_by(any.media.yn) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))
  #vulnerability_2011  %>% group_by(wealth.index.ur.ord) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))
  #vulnerability_2011  %>% group_by(work.seasonal) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

#starting on the left most part of the tree and working toward the right 
vulnerability_2011$UM.1 <- if_else(vulnerability_2011$pship.cat == "married/cohab" & (vulnerability_2011$work.seasonal=="All  year" | vulnerability_2011$work.seasonal=="Occasional" | vulnerability_2011$work.seasonal=="Seasonal") & vulnerability_2011$any.media.yn==0 & vulnerability_2011$wealth.index.ur.ord<3  , 1, 0)
  #vulnerability_2011  %>% group_by(UM.1) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$UM.2 <- if_else(vulnerability_2011$pship.cat == "married/cohab" & (vulnerability_2011$work.seasonal=="All  year" | vulnerability_2011$work.seasonal=="Occasional" | vulnerability_2011$work.seasonal=="Seasonal") & vulnerability_2011$any.media.yn==0 & vulnerability_2011$wealth.index.ur.ord>=3  , 1, 0)
  #vulnerability_2011  %>% group_by(UM.2) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$UM.1.1 <- if_else(vulnerability_2011$pship.cat == "married/cohab" & (vulnerability_2011$work.seasonal=="All  year" | vulnerability_2011$work.seasonal=="Occasional" | vulnerability_2011$work.seasonal=="Seasonal") & vulnerability_2011$any.media.yn==1 & vulnerability_2011$wealth.index.ur.ord<2 & (vulnerability_2011$religionrecode.cat=="muslim" | vulnerability_2011$religionrecode.cat=="protestant")  , 1, 0)
  #vulnerability_2011  %>% group_by(UM.1.1) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$UM.2.1 <- if_else(vulnerability_2011$pship.cat == "married/cohab" & (vulnerability_2011$work.seasonal=="All  year" | vulnerability_2011$work.seasonal=="Occasional" | vulnerability_2011$work.seasonal=="Seasonal") & vulnerability_2011$any.media.yn==1 & vulnerability_2011$wealth.index.ur.ord<2 & (vulnerability_2011$religionrecode.cat=="orthodox" | vulnerability_2011$religionrecode.cat=="other")  , 1, 0)

vulnerability_2011$UM.2.2 <- if_else(vulnerability_2011$pship.cat == "married/cohab" & (vulnerability_2011$work.seasonal=="All  year" | vulnerability_2011$work.seasonal=="Occasional" | vulnerability_2011$work.seasonal=="Seasonal") & vulnerability_2011$any.media.yn==1 & vulnerability_2011$wealth.index.ur.ord>=2  , 1, 0)
  #vulnerability_2011  %>% group_by(UM.2.2) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$UM.1.2 <- if_else(vulnerability_2011$pship.cat == "married/cohab" & vulnerability_2011$work.seasonal=="No work in the past 12 months"  & vulnerability_2011$any.media.yn==0 & vulnerability_2011$wealth.index.ur.ord<3  , 1, 0)
  #vulnerability_2011  %>% group_by(UM.1.2) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$UM.1.3 <- if_else(vulnerability_2011$pship.cat == "married/cohab" & vulnerability_2011$work.seasonal=="No work in the past 12 months"  & vulnerability_2011$any.media.yn==0 & vulnerability_2011$wealth.index.ur.ord>=3 & (vulnerability_2011$religionrecode.cat=="muslim" | vulnerability_2011$religionrecode.cat=="protestant")   , 1, 0)
  #vulnerability_2011  %>% group_by(UM.1.3) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$UM.3 <- if_else(vulnerability_2011$pship.cat == "married/cohab" & vulnerability_2011$work.seasonal=="No work in the past 12 months"  & vulnerability_2011$any.media.yn==0 & vulnerability_2011$wealth.index.ur.ord>=3 & (vulnerability_2011$religionrecode.cat=="orthodox" | vulnerability_2011$religionrecode.cat=="other")   , 1, 0)
  #vulnerability_2011  %>% group_by(UM.3) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$UM.1.4 <- if_else(vulnerability_2011$pship.cat == "married/cohab" & vulnerability_2011$work.seasonal=="No work in the past 12 months"  & vulnerability_2011$any.media.yn==1 & vulnerability_2011$hh.electricity==0    , 1, 0)
  #vulnerability_2011  %>% group_by(UM.1.4) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$UM.1.5 <- if_else(vulnerability_2011$pship.cat == "married/cohab" & vulnerability_2011$work.seasonal=="No work in the past 12 months"  & vulnerability_2011$any.media.yn==1 & vulnerability_2011$hh.electricity==1 & vulnerability_2011$anyed.yn==0 & vulnerability_2011$wealth.index.ur.ord<3 & (vulnerability_2011$religionrecode.cat=="muslim" | vulnerability_2011$religionrecode.cat=="protestant")   , 1, 0)
  #vulnerability_2011  %>% group_by(UM.1.5) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$UM.3.1 <- if_else(vulnerability_2011$pship.cat == "married/cohab" & vulnerability_2011$work.seasonal=="No work in the past 12 months"  & vulnerability_2011$any.media.yn==1 & vulnerability_2011$hh.electricity==1 & vulnerability_2011$anyed.yn==0 & vulnerability_2011$wealth.index.ur.ord<3 & (vulnerability_2011$religionrecode.cat=="orthodox")   , 1, 0)
  #vulnerability_2011  %>% group_by(UM.3.1) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$UM.3.2 <- if_else(vulnerability_2011$pship.cat == "married/cohab" & vulnerability_2011$work.seasonal=="No work in the past 12 months"  & vulnerability_2011$any.media.yn==1 & vulnerability_2011$hh.electricity==1 & vulnerability_2011$anyed.yn==0 & vulnerability_2011$wealth.index.ur.ord>=3  , 1, 0)
  #vulnerability_2011  %>% group_by(UM.3.2) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$UM.3.3 <- if_else(vulnerability_2011$pship.cat == "married/cohab" & vulnerability_2011$work.seasonal=="No work in the past 12 months"  & vulnerability_2011$any.media.yn==1 & vulnerability_2011$hh.electricity==1 & vulnerability_2011$anyed.yn==1  , 1, 0)
  #vulnerability_2011  %>% group_by(UM.3.3) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$UM.1.final <- if_else((vulnerability_2011$UM.1 == 1 | vulnerability_2011$UM.1.1 == 1 | vulnerability_2011$UM.1.2 == 1 | vulnerability_2011$UM.1.3 == 1 | vulnerability_2011$UM.1.4 == 1 | vulnerability_2011$UM.1.5 == 1) , 1, 0)
  #vulnerability_2011 %>% select(UM.1.final, UM.1, UM.1.1, UM.1.2, UM.1.3, UM.1.4, UM.1.5)  %>% View

vulnerability_2011$UM.2.final <- if_else((vulnerability_2011$UM.2 == 1 | vulnerability_2011$UM.2.1 == 1 | vulnerability_2011$UM.2.2 == 1 ) , 1, 0)

vulnerability_2011$UM.3.final <- if_else((vulnerability_2011$UM.3 == 1 | vulnerability_2011$UM.3.1 == 1 | vulnerability_2011$UM.3.2 == 1 | vulnerability_2011$UM.3.3 == 1) , 1, 0)

vulnerability_2011$UU.1.final <- if_else((vulnerability_2011$pship.cat == "never" | vulnerability_2011$pship.cat == "no partner now") &  (vulnerability_2011$work.seasonal=="All  year" | vulnerability_2011$work.seasonal=="Occasional" | vulnerability_2011$work.seasonal=="Seasonal")  , 1, 0)
#vulnerability_2011  %>% group_by(UU.1) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$UU.2.final <- if_else((vulnerability_2011$pship.cat == "never" | vulnerability_2011$pship.cat == "no partner now") &  (vulnerability_2011$work.seasonal=="No work in the past 12 months")  , 1, 0)
  #vulnerability_2011  %>% group_by(UU.2) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))
  #vulnerability_2011 %>% select(UM.1.final, UM.2.final, UM.3.final, UU.1.final, UU.2.final)  %>% View
  #checking no overlap
    #vulnerability_2011$sum <-vulnerability_2011$UM.1.final+ vulnerability_2011$UM.2.final+ vulnerability_2011$UM.3.final+ vulnerability_2011$UU.1.final+ vulnerability_2011$UU.2.final
    #vulnerability_2011  %>% group_by(sum) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))


################### RURAL ###########################################################################################################################################################################################################################
#Note the below code follows the tree exactly, but this could be made more parsimonious by specifying the wealth index only once each

  #starting on the left most branches and working to the right ofthe image
vulnerability_2011$RM.1 <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & vulnerability_2011$pship.cat=="married/cohab" & vulnerability_2011$wealth.index.ur.ord<2 & (vulnerability_2011$religionrecode.cat=="muslim" | vulnerability_2011$religionrecode.cat=="other")  , 1, 0)
  #vulnerability_2011  %>% group_by(RM.1) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$RM.1.1 <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & vulnerability_2011$pship.cat=="married/cohab" & vulnerability_2011$wealth.index.ur.ord<2 & (vulnerability_2011$religionrecode.cat=="orthodox" | vulnerability_2011$religionrecode.cat=="protestant") & vulnerability_2011$pastoralnoagro.yn==1 & vulnerability_2011$anyed.yn==0  , 1, 0)
#vulnerability_2011  %>% group_by(RM.1.1) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))
vulnerability_2011$RM.2 <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & vulnerability_2011$pship.cat=="married/cohab" & vulnerability_2011$wealth.index.ur.ord<2 & (vulnerability_2011$religionrecode.cat=="orthodox" | vulnerability_2011$religionrecode.cat=="protestant") & vulnerability_2011$pastoralnoagro.yn==1 & vulnerability_2011$anyed.yn==1, 1, 0)
#vulnerability_2011  %>% group_by(RM.1.1) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))
vulnerability_2011$RM.2.1 <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & vulnerability_2011$pship.cat=="married/cohab" & vulnerability_2011$wealth.index.ur.ord<2 & (vulnerability_2011$religionrecode.cat=="orthodox" | vulnerability_2011$religionrecode.cat=="protestant") & vulnerability_2011$pastoralnoagro.yn==0, 1, 0)
#vulnerability_2011  %>% group_by(RM.2.1) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$RU.2.final <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & (vulnerability_2011$pship.cat == "never" | vulnerability_2011$pship.cat == "no partner now")  , 1, 0)


vulnerability_2011$RM.1.2 <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & vulnerability_2011$pship.cat=="married/cohab" & vulnerability_2011$wealth.index.ur.ord>=2 & vulnerability_2011$pastoralnoagro.yn==1 & vulnerability_2011$wealth.index.ur.ord<4 & vulnerability_2011$anyed.yn==0, 1, 0)
#vulnerability_2011  %>% group_by(RM.1.2) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))
vulnerability_2011$RM.2.2 <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & vulnerability_2011$pship.cat=="married/cohab" & vulnerability_2011$wealth.index.ur.ord>=2 & vulnerability_2011$pastoralnoagro.yn==1 & vulnerability_2011$wealth.index.ur.ord<4 & vulnerability_2011$anyed.yn==1, 1, 0)
#vulnerability_2011  %>% group_by(RM.2.2) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))
vulnerability_2011$RM.2.3 <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & vulnerability_2011$pship.cat=="married/cohab" & vulnerability_2011$wealth.index.ur.ord>=2 & vulnerability_2011$pastoralnoagro.yn==1 & vulnerability_2011$wealth.index.ur.ord>=4, 1, 0)
#vulnerability_2011  %>% group_by(RM.2.3) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

vulnerability_2011$RM.1.3 <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & vulnerability_2011$pship.cat=="married/cohab" & vulnerability_2011$wealth.index.ur.ord>=2 & vulnerability_2011$pastoralnoagro.yn==0 & vulnerability_2011$wealth.index.ur.ord<3 & vulnerability_2011$religionrecode.cat=="muslim" & vulnerability_2011$anyed.yn==0 & vulnerability_2011$med.index>=3 & vulnerability_2011$conflict_incidents<=2, 1, 0)
#vulnerability_2011  %>% group_by(RM.1.3) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))
  #no people in this group. There are around 100 without the final condition on partners not working
vulnerability_2011$RM.1.4 <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & vulnerability_2011$pship.cat=="married/cohab" & vulnerability_2011$wealth.index.ur.ord>=2 & vulnerability_2011$pastoralnoagro.yn==0 & vulnerability_2011$wealth.index.ur.ord<3 & vulnerability_2011$religionrecode.cat=="muslim"  & vulnerability_2011$anyed.yn==0 & vulnerability_2011$med.index>=3 & vulnerability_2011$conflict_incidents>=25 & vulnerability_2011$partner.working.yn==0, 1, 0)
  #26 people in this group
vulnerability_2011$RM.1.5 <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & vulnerability_2011$pship.cat=="married/cohab" & vulnerability_2011$wealth.index.ur.ord>=2 & vulnerability_2011$pastoralnoagro.yn==0 & vulnerability_2011$wealth.index.ur.ord<3 & vulnerability_2011$religionrecode.cat=="muslim"  & vulnerability_2011$anyed.yn==0 & vulnerability_2011$med.index>=3 & vulnerability_2011$conflict_incidents>=25 & vulnerability_2011$partner.working.yn==1 & vulnerability_2011$jdwd.index.cat=="None", 1, 0)
vulnerability_2011$RM.2.4 <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & vulnerability_2011$pship.cat=="married/cohab" & vulnerability_2011$wealth.index.ur.ord>=2 & vulnerability_2011$pastoralnoagro.yn==0 & vulnerability_2011$wealth.index.ur.ord<3 & vulnerability_2011$religionrecode.cat=="muslim"  & vulnerability_2011$anyed.yn==0 & vulnerability_2011$med.index>=3 & vulnerability_2011$conflict_incidents>=25 & vulnerability_2011$partner.working.yn==1 &  (vulnerability_2011$jdwd.index.cat=="One to four" | vulnerability_2011$jdwd.index.cat=="Five to six") , 1, 0)
  #going back up the farthest right branch.
  #28 people
vulnerability_2011$RM.2.5 <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & vulnerability_2011$pship.cat=="married/cohab" & vulnerability_2011$wealth.index.ur.ord>=2 & vulnerability_2011$pastoralnoagro.yn==0 & vulnerability_2011$wealth.index.ur.ord<3 & vulnerability_2011$religionrecode.cat=="muslim"  & vulnerability_2011$anyed.yn==0 & vulnerability_2011$med.index<3, 1, 0)
vulnerability_2011$RM.2.6 <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & vulnerability_2011$pship.cat=="married/cohab" & vulnerability_2011$wealth.index.ur.ord>=2 & vulnerability_2011$pastoralnoagro.yn==0 & vulnerability_2011$wealth.index.ur.ord<3 & vulnerability_2011$religionrecode.cat=="muslim"  & vulnerability_2011$anyed.yn==1, 1, 0)
vulnerability_2011$RM.2.7 <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & vulnerability_2011$pship.cat=="married/cohab" & vulnerability_2011$wealth.index.ur.ord>=2 & vulnerability_2011$pastoralnoagro.yn==0 & vulnerability_2011$wealth.index.ur.ord<3 & ( vulnerability_2011$religionrecode.cat=="orthodox" | vulnerability_2011$religionrecode.cat=="other" | vulnerability_2011$religionrecode.cat=="protestant") , 1, 0)
#vulnerability_2011  %>% group_by(RM.2.7) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))
vulnerability_2011$RM.2.8 <- if_else((vulnerability_2011$work.seasonal=="No work in the past 12 months") & vulnerability_2011$pship.cat=="married/cohab" & vulnerability_2011$wealth.index.ur.ord>=2 & vulnerability_2011$pastoralnoagro.yn==0 & vulnerability_2011$wealth.index.ur.ord>=3  , 1, 0)


vulnerability_2011$RM.3.final <- if_else((vulnerability_2011$work.seasonal=="All  year" | vulnerability_2011$work.seasonal=="Occasional" | vulnerability_2011$work.seasonal=="Seasonal") & vulnerability_2011$pship.cat == "married/cohab" , 1, 0)
vulnerability_2011$RU.1.final <- if_else((vulnerability_2011$work.seasonal=="All  year" | vulnerability_2011$work.seasonal=="Occasional" | vulnerability_2011$work.seasonal=="Seasonal") & (vulnerability_2011$pship.cat == "never" | vulnerability_2011$pship.cat == "no partner now")  , 1, 0)

vulnerability_2011$RM.1.final <- if_else((vulnerability_2011$RM.1 == 1 | vulnerability_2011$RM.1.1 == 1 | vulnerability_2011$RM.1.2 == 1 | vulnerability_2011$RM.1.3 == 1 | vulnerability_2011$RM.1.4 == 1 | vulnerability_2011$RM.1.5 == 1  ) , 1, 0)

vulnerability_2011$RM.2.final <- if_else((vulnerability_2011$RM.2 == 1 | vulnerability_2011$RM.2.1 == 1 | vulnerability_2011$RM.2.2 == 1 | vulnerability_2011$RM.2.3 == 1 | vulnerability_2011$RM.2.4 == 1 | vulnerability_2011$RM.2.5 == 1 | vulnerability_2011$RM.2.6 == 1 | vulnerability_2011$RM.2.7 == 1 | vulnerability_2011$RM.2.8 == 1) , 1, 0)

##checking no overlap in the resulting clusters
#vulnerability_2011$sum_rural <-vulnerability_2011$RM.1.final+ vulnerability_2011$RM.2.final+ vulnerability_2011$RM.3.final+ vulnerability_2011$RU.1.final+ vulnerability_2011$RU.2.final
#vulnerability_2011  %>% group_by(sum_rural) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))
#vulnerability_2011 %>% filter(sum_rural==2) %>% select(sum_rural, RM.1.final, RM.1, RM.1.1, RM.1.2, RM.1.3, RM.1.4, RM.1.5, RM.1.6, RM.2.final, work.seasonal, pship.cat, wealth.index.ur.ord, religionrecode.cat, pastoralnoagro.yn, anyed.yn, med.index, conflict_incidents, partner.working.yn, jdwd.index.cat ) %>% View 


saveRDS(vulnerability_2011, file = "Data/Ethiopia_constructed_data/ETH_2011_constructed_data/4c_Ethiopia_DHS_vulnerability_2011_posttypingtool.rds")

##vulnerability_2011  %>% group_by(conflict_incidents) %>%   summarise(n = n()) %>%   mutate(totalN = (cumsum(n)),   percent = round((n/sum(n)), 3),     cumuPer = round(cumsum(freq = n/ sum(n)), 3))

#conflict_incidents     n totalN percent cumuPer
#<dbl> <int>  <int>   <dbl>   <dbl>
#  1                  0  1259   1259   0.076   0.076
#2                  5  1130   2389   0.068   0.145
#3                  6  3487   5876   0.211   0.356
#4                 11  1728   7604   0.105   0.46 
#5                 23  2034   9638   0.123   0.584
#6                 48  1741  11379   0.105   0.689
#7                 62  2087  13466   0.126   0.815
#8                125  2135  15601   0.129   0.945
#9                323   914  16515   0.055   1    
