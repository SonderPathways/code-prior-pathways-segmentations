############## 
# 13_Ethiopia_DHS_2011typingtool_test
# Ethiopia DHS 2011, Recode 6
# input data: vulnerability_2011_posttypingtool
# ancestor code: 12_Ethiopia_DHS_clusterassign_2011
# descendent code: none
# purpose: test clusters resulting from typing tool
### Note:need to set working directory for each user ######
############## 

#Note Urban
#UM.1. (married, half working, low income)
#UM.2. (married, working)
#UM.3. (married, not working)
#UU1. unmarried, working
#UU.2. (unmarried, not working)

#Rural
#RM1. married, not working, low income
#RM2. married, not working, higher income
#RM3. married, working
#RU2. unmarried, not working, younger
#RU1. unmarried, working, older

#install.packages('pacman') -- only needs to be done once
rm(list=ls())
pacman::p_load(foreign, ggplot2, dplyr, tidyr, data.table, haven, survey, readxl, sf, openxlsx, gtools, cluster, readr, Rtsne, NbClust, vegan, ade4, cluster)


############## Set directory - change for a different user

#setwd("/Users/darcyrao/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Pathways - GE Data & Insights") # for use from Darcy's Mac
#setwd("C:/Users/darcyra/OneDrive - Bill & Melinda Gates Foundation/Pathways - GE Data & Insights/Modeling") # For use from Darcy's PC
#setwd("/Users/adigreif/Desktop/Gates Ethiopia/EthiopiaSegmentation/")
setwd("/Users/adigreif/Desktop/Gates Ethiopia/EthiopiaSegmentation/")

############## Load Ethiopia DHS data
vulnerability_2011 <- readRDS("Data/Ethiopia_constructed_data/ETH_2011_constructed_data/4c_Ethiopia_DHS_vulnerability_2011_posttypingtool.rds")


################ URBAN ############################################

vulnerability_2011 <- vulnerability_2011 %>%  mutate(typology_urban=case_when(
  UU.1.final==1 ~ "UU.1.final",
  UU.2.final==1 ~ "UU.2.final",
  UM.1.final==1 ~ "UM.1.final",
  UM.2.final==1 ~ "UM.2.final",
  UM.3.final==1 ~ "UM.3.final"))
vulnerability_2011$typology_urban <- as.factor(vulnerability_2011$typology_urban)

vulnerability_2011_urban <- vulnerability_2011 %>% filter(hh.urban==1 ) 

#############  urban - selected chi squared tests ######################

#can do chisq directly on data, or as a wrapper around a contingency table
chisq.test(vulnerability_2011_urban$typology_urban, vulnerability_2011_urban$pship.cat)
  #X-squared = 5676.3, df = 8, p-value < 2.2e-16
chisq.test(table(vulnerability_2011_urban$typology_urban, vulnerability_2011_urban$pship.cat))
  #same result: X-squared = 5676.3, df = 8, p-value < 2.2e-16

GTest(vulnerability_2011_urban$typology_urban, vulnerability_2011_urban$religionrecode.cat)
#suggested test when chisq distribution fits poorly and sample size is fairly small: https://www.datacamp.com/tutorial/contingency-tables-r
chisq.test(vulnerability_2011_urban$typology_urban, vulnerability_2011_urban$work.seasonal)
chisq.test(vulnerability_2011_urban$typology_urban, vulnerability_2011_urban$any.media.yn)
chisq.test(vulnerability_2011_urban$typology_urban, vulnerability_2011_urban$hh.electricity)
chisq.test(vulnerability_2011_urban$typology_urban, vulnerability_2011_urban$anyed.yn)
chisq.test(vulnerability_2011_urban$typology_urban, vulnerability_2011_urban$wealth.index.ur.ord)

#nice possible plots here: https://statsandr.com/blog/chi-square-test-of-independence-in-r/
#example of corrections: https://alanarnholt.github.io/PDS-Bookdown2/post-hoc-tests-1.html

#############  urban - anosim tests ######################

###first create distance matrix, using PAM as used before###

#urban variables used: work.seasonal, pship.cat, religionrecode.cat, any.media.yn, wealth.index.ur.ord, hh.electricity, anyed.yn

  vulnerability_2011_urban_subset <- vulnerability_2011_urban %>% 
  select(CLUSTERNO, HHNUM, LINENO, working.yn, hh.electricity, 
         work.seasonal, wealth.index.ur.ord, pship.cat,  
         any.media.yn, anyed.yn, religionrecode.cat, typology_urban
  )

  #saving two versions, one with only what is needed for the distance matrix, and the same df (complete cases only) that can be merged back and has the typing tool results
vulnerability_2011_urban_subset <- vulnerability_2011_urban_subset[complete.cases(vulnerability_2011_urban_subset),]
vulnerability_2011_urban_test <- vulnerability_2011_urban_subset %>% select(-c("CLUSTERNO", "HHNUM", "LINENO", "typology_urban"))

## changing character to factor variables
vulnerability_2011_urban_test$work.seasonal <- as.factor(vulnerability_2011_urban_test$work.seasonal)
vulnerability_2011_urban_test$pship.cat <- as.factor(vulnerability_2011_urban_test$pship.cat)
vulnerability_2011_urban_test$religionrecode.cat <- as.factor(vulnerability_2011_urban_test$religionrecode.cat)

##setting ordinal variables
#note: not setting conflict_incidents as ordered since in theory it can range continuously
vulnerability_2011_urban_test$wealth.index.ur.ord <- as.ordered(vulnerability_2011_urban_test$wealth.index.ur.ord)  

  ## getting dissimilarity matrix and running anosim
gdist_urban_2011 <- daisy(vulnerability_2011_urban_test, metric = "gower")
##Warning messages state which columsn are asymmetric binary and which interval scaled
gdist_urban_mat_2011 <- as.matrix(gdist_urban_2011)

  #should also consider PERMANOVA or non parametric NPMANOVA)
  #https://rpubs.com/hafezahmad/948799
  #anosim requires dissimilarity matrix, I believe it ranks it
  #see https://cran.r-project.org/web/packages/vegan/vegan.pdf
  #see adonis2, more robust?
  #mrpp similar?
  #mantel test less appropriate, it's for two matrices
  #0.5-0.75 is a pretty good ratio between group over within group ratios
  #https://www.researchgate.net/post/Which-R-value-is-considered-to-show-a-strong-difference-between-the-groups-in-ANOSIM
  #could do two way tests or PRIMER to follow up on finding out specifics
  #https://www.researchgate.net/post/Why-do-I-obtain-different-results-for-MDS-and-PERMANOVA-in-PRIMER
anosim(gdist_urban_mat_2011, vulnerability_2011_urban_subset$typology_urban,  distance = "gower", permutations = 30)

#################### RURAL ######################################

vulnerability_2011 <- vulnerability_2011 %>%  mutate(typology_rural=case_when(
  RU.1.final==1 ~ "RU.1.final",
  RU.2.final==1 ~ "RU.2.final",
  RM.1.final==1 ~ "RM.1.final",
  RM.2.final==1 ~ "RM.2.final",
  RM.3.final==1 ~ "RM.3.final"))
#select(vulnerability_2011,typology_rural) %>% View
vulnerability_2011$typology_rural <- as.factor(vulnerability_2011$typology_rural)
vulnerability_2011_rural <- vulnerability_2011 %>% filter(hh.urban==0 ) 

############## rural chi squared tests ########################
chisq.test(vulnerability_2011_rural$typology_rural, vulnerability_2011_rural$pship.cat)
GTest(vulnerability_2011_rural$typology_rural, vulnerability_2011_rural$religionrecode.cat)
#suggested test when chisq distribution fits poorly and sample size is fairly small: https://www.datacamp.com/tutorial/contingency-tables-r
chisq.test(vulnerability_2011_rural$typology_rural, vulnerability_2011_rural$work.seasonal)
chisq.test(vulnerability_2011_rural$typology_rural, vulnerability_2011_rural$med.index)
chisq.test(vulnerability_2011_rural$typology_rural, vulnerability_2011_rural$conflict_incidents)
chisq.test(vulnerability_2011_rural$typology_rural, vulnerability_2011_rural$anyed.yn)
chisq.test(vulnerability_2011_rural$typology_rural, vulnerability_2011_rural$wealth.index.ur.ord)
chisq.test(vulnerability_2011_rural$typology_rural, vulnerability_2011_rural$jdwd.index.cat)
chisq.test(vulnerability_2011_rural$typology_rural, vulnerability_2011_rural$pastoralnoagro.yn)

############### rural anosim test ##################################

###first create distance matrix, using PAM as used before###
vulnerability_2011_rural_subset <- vulnerability_2011_rural %>% 
  select(CLUSTERNO, HHNUM, LINENO, med.index, partner.working.yn, 
         work.seasonal, wealth.index.ur.ord, jdwd.index.cat, 
         pship.cat, anyed.yn, religionrecode.cat, conflict_incidents, 
         pastoralnoagro.yn, typology_rural)
vulnerability_2011_rural_subset <- vulnerability_2011_rural_subset[complete.cases(vulnerability_2011_rural_subset),]
vulnerability_2011_rural_test <- vulnerability_2011_rural_subset %>% select(-c("CLUSTERNO", "HHNUM", "LINENO", "typology_rural"))

## changing character to factor variables
vulnerability_2011_rural_test$work.seasonal <- as.factor(vulnerability_2011_rural_test$work.seasonal)
vulnerability_2011_rural_test$pship.cat <- as.factor(vulnerability_2011_rural_test$pship.cat)
vulnerability_2011_rural_test$religionrecode.cat <- as.factor(vulnerability_2011_rural_test$religionrecode.cat)
vulnerability_2011_rural_test$jdwd.index.cat <- as.factor(vulnerability_2011_rural_test$jdwd.index.cat)

##setting ordinal variables
#note: not setting conflict_incidents as ordered since in theory it can range continuously
vulnerability_2011_rural_test$wealth.index.ur.ord <- as.ordered(vulnerability_2011_rural_test$wealth.index.ur.ord)  
  #could also rename this med.index.ord and drop med.index
vulnerability_2011_rural_test$med.index <- as.ordered(vulnerability_2011_rural_test$med.index)  

## getting dissimilarity matrix and running anosim
gdist_rural_2011 <- daisy(vulnerability_2011_rural_test, metric = "gower")
##Warning messages state which columsn are asymmetric binary and which interval scaled
gdist_rural_mat_2011 <- as.matrix(gdist_rural_2011)

anosim(gdist_rural_mat_2011, vulnerability_2011_rural_subset$typology_rural,  distance = "gower", permutations = 30)

rural_mrpp <- mrpp(gdist_rural_mat_2011, vulnerability_2011_rural_subset$typology_rural, permutations=1)
