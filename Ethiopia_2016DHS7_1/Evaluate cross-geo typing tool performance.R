#### XX: Identify/test for "core" set of variables
####purpose: Explores the performance of alternative variable sets 
### ancestor: NA
### descendent: none. 
### input: 9a2_dhs_urban_noage.rds, 9a2_dhs_rural_noage.rds, Pathways Kenya and Nigeria datasets
### output: pictures of trees 
### note: working directory needs to be set for each user 

rm(list=ls())

pacman::p_load(rpart, rpart.plot, caret, randomForest)
pacman::p_load(rattle, rpart.plot, RColorBrewer, foreign, ggplot2, dplyr, tidyr, data.table, survey, srvyr, psych) # Darcy added
#library(rpart)
#library(rpart.plot)
#library(caret)

#### setup ####

##set working directory - need to change for each user
setwd("/Users/darcyrao/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Pathways - GE Data & Insights/Modeling") # Darcy
# setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/") # Anu
# setwd('C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways - GE Data & Insights\\Modeling') #Elisabeth

## load data
# Ethiopia DHS 2016
  eth_urban <- readRDS("Data/Ethiopia_constructed_data/LCA_urban_noage_data/9a2_dhs_urban_noage.rds")
  eth_rural <- readRDS("Data/Ethiopia_constructed_data/LCA_rural_noage_data/9a2_dhs_rural_noage.rds")
  eth_dhs <- merge(eth_urban, eth_rural, all = TRUE)
  eth_IR_orig <- data.table(read.dta("Data/Ethiopia DHS/ET_2016/ETIR71DT/ETIR71FL.DTA"))  # to bring in other variables
  eth_HH_orig <- data.table(read.dta("Data/Ethiopia DHS/ET_2016/ETHR71DT/ETHR71FL.DTA")) 
  

# Kenya Pathways
  ken_path <- read.spss("Data/Pathways/Pathways main survey tool - Kenya_Jan 3 ALL MERGED ITYPES.sav")
  ken_path<-as.data.frame(ken_path)
  ken_path <-subset(ken_path, ken_path$ITYPclean==1)

# Nigeria Pathways
  nig_path <- read.spss("Data/Pathways/Pathways main survey tool_Nigeria_2022_11_04_14_02 FULL DATA - MERGED.sav")
  nig_path<-as.data.frame(nig_path)
  nig_path$ITYP<-as.numeric(nig_path$ITYP)
  # Unique ID
  nig_path$hhid<-paste(nig_path$PSUclean,nig_path$HHIDclean)
  nig_path$hhid<-gsub(" ", "", nig_path$hhid, fixed=F)
  nig_path$caseid<-paste(nig_path$PSUclean,nig_path$HHIDclean,nig_path$ITYP)
  nig_path$caseid<-gsub(" ", "", nig_path$caseid, fixed=F)
  # Create a dataset of only women
  nig_path<-subset(nig_path, nig_path$ITYP==1)


### List variables needed for typing tools
  # all variables: urban, head.sex, religionrecode.cat, pship.cat, ed.cat, part.ed.cat, working.yn, partner.occup, 
  #           freq.rad, any.media.yn, freq.tv, wealth.quint, asset.index, hh.electricity, hh.watpur.appr, toilet.type,
  #           dec.money, , med.index, med.permis, fp.mod.now, parity, anc.4plus, hb.ever
  #       *Vars only for Pathways datasets: hbsafe, helpless
  # 
  # union: urban, ed.cat
  #
  
#### Data cleaning ####

## Ethiopia
  # Urban
    eth_dhs$urban <- ifelse(eth_dhs$URBAN == 1, 1, 0)
  
  # Household head sex
    #eth_dhs$head.sex
    
  # Religion
    #eth_dhs$religionrecode.cat
  
  # Partnership status
    #eth_dhs$pship.cat
  
  # Education
    eth_dhs <- eth_dhs %>% mutate(ed.cat=case_when(
      ed.lev %in% c("no education") ~ "None",
      ed.lev %in% c("incomplete primary") ~ "Incomplete primary",
      ed.lev %in% c("complete primary") ~ "Complete primary",
      ed.lev %in% c("incomplete secondary","complete secondary", "higher") ~ "Higher"
    ))
  
  # Partner's education
    eth_dhs <- eth_dhs %>% mutate(part.ed.cat=case_when(
      husb.ed.grp %in% c("Not in union") ~ "No partner",
      husb.ed.grp %in% c("no education") ~ "None",
      husb.ed.grp %in% c("incomplete primary") ~ "Incomplete primary",
      husb.ed.grp %in% c("complete primary") ~ "Complete primary", 
      husb.ed.grp %in% c("incomplete secondary", "complete secondary", "higher") ~ "Higher"
    ))
    
  # Worked in the past 12 months (includes on leave/ absent)
    #eth_dhs$working.yn
    
  # Partner occupation
    eth_IR_orig$partner.occup <- ifelse(eth_IR_orig$v705 %in% c("professional/technical/managerial"), "Professionaltechnical/managerial",
                                        ifelse(eth_IR_orig$v705 %in% c("did not work"), "No work p12 months",
                                               ifelse(eth_IR_orig$v501 %in% c("never in union", "widowed", "divorced", "no longer living together/separated"), "No partner",
                                                      "Other occupation")))
    
  # Radio and TV freq
    eth_IR_orig$freq.rad <- eth_IR_orig$v158
    eth_IR_orig$freq.tv <- eth_IR_orig$v159
    
    #eth_dhs$any.media.yn # Any tv, radio, newspapers

  # Wealth index
    eth_dhs$wealth.quint <- eth_dhs$wealth.index.ur.ord
    
  # Asset ownership index (motorcycle/scooter, car/truck, cellphone, bank account )
    eth_dhs <- eth_dhs %>% 
      rowwise() %>% mutate(asset.index = as.numeric(sum(hh.motor,hh.car, hh.phone, hh.bank.acct, na.rm=TRUE)))
    eth_dhs$asset.index<-ifelse(is.na(eth_dhs$hh.motor) & is.na(eth_dhs$hh.car) & is.na(eth_dhs$hh.phone) & is.na(eth_dhs$hh.bank.acct), NA, eth_dhs$asset.index)
    
  # household electricity
    #eth_dhs$hh.electricity
    
  # Water treatment
    #eth_dhs$hh.watpur
    
  # toilet facility (No facility, bush, field / bucket in house / pit latrine, flush toilet)
    eth_HH_orig$toilet.type <- ifelse(eth_HH_orig$hv205 %in% c("no facility", "no facility/bush/field"), "No facility/bush/field",
                                      ifelse(eth_HH_orig$hv205 %in% c("bucket toilet"), "Bucket","Latrine or toilet"))
    
    
  # Belief about gender control over money -- in this case it is about who makes decisions
    eth_IR_orig$dec.money <- ifelse(eth_IR_orig$v739 %in% c("husband/partner alone"), 1, 0)
    
  # Index of barriers to seeking med
    #eth_dhs$med.index
    
  # Modern contraception now
    # eth_dhs$fp.mod.now
    
  # Parity (total children ever born)
    eth_IR_orig$parity <- ifelse(eth_IR_orig$v201 %in% c(0), "0",
                                       ifelse(eth_IR_orig$v201 %in% c(1,2,3), "1-3",
                                              ifelse(eth_IR_orig$v201 %in% c(4), "4",
                                                     ifelse(eth_IR_orig$v201 %in% c(5:20), "5+", NA))))
 
  # ANC count - include never pregnant / dk in 0 
    eth_dhs$anc.4plus <- ifelse(eth_dhs$anc.num %in% c(4:20), 1, 0)
    
  # Ever had a home birth
    eth_IR_orig[,hb.1:= ifelse(m15_1== "home" | m15_1== "respondent's home" | m15_1== "other home" | m15_1== "other",1,0)]
    eth_IR_orig[,hb.2:= ifelse(m15_2== "home" | m15_2== "respondent's home" | m15_2== "other home" | m15_2== "other",1,0)]
    eth_IR_orig[,hb.3:= ifelse(m15_3== "home" | m15_3== "respondent's home" | m15_3== "other home" | m15_3== "other",1,0)]
    eth_IR_orig[,hb.4:= ifelse(m15_4== "home" | m15_4== "respondent's home" | m15_4== "other home" | m15_4== "other",1,0)]
    eth_IR_orig[,hb.5:= ifelse(m15_5== "home" | m15_5== "respondent's home" | m15_5== "other home" | m15_5== "other",1,0)]
    
    eth_IR_orig$hb.ever[(eth_IR_orig$hb.1==1 | eth_IR_orig$hb.2==1 | eth_IR_orig$hb.3==1 | eth_IR_orig$hb.4==1 | eth_IR_orig$hb.5==1)]<- 1
    eth_IR_orig$hb.ever[is.na(eth_IR_orig$hb.ever)] <- 0
    
  # Pull in additional vars from the woman level dataset to explore
    ## Region
    eth_IR_orig$region <- eth_IR_orig$v139 # dejure region of usual residence
    
    ## pull in new wealth variables
    eth_IR_orig$hh.cook_notwood <- ifelse(eth_IR_orig$v161 == "wood", 0, 1) # does not cook with wood as fuel
    eth_IR_orig$hh.floor_notearthdung <- ifelse(eth_IR_orig$v127 %in% c("earth/sand", "dung"), 0, 1) # Floor is *not* earth/sand or dung (the latter I added)
    eth_IR_orig$hh.floor_notearth <- ifelse(eth_IR_orig$v127 %in% c("earth/sand"), 0, 1) # Floor is *not* earth/sand
    eth_IR_orig$hh.cook_elect <- ifelse(eth_IR_orig$v161 == "electricity", 1, 0) # cooks with electricity
    eth_IR_orig$water.piped <- ifelse(eth_IR_orig$v113=="piped to yard/plot", 1,0)
    eth_IR_orig$water.notpublic <-  ifelse(eth_IR_orig$v113 == "public tap/standpipe", 0, 1)
    eth_IR_orig$roof.notsod <- ifelse(eth_IR_orig$v129 == "sod", 0, 1)
    eth_IR_orig$walls.notbamboo <- ifelse(eth_IR_orig$v128 == "bamboo with mud", 0, 1)

    
    eth_HH_orig$v001<-eth_HH_orig$hv001
    eth_HH_orig$v002<-eth_HH_orig$hv002
    eth_IR_orig$wt<-eth_IR_orig$v005/1000000
    eth_IR_orig.merge <- eth_IR_orig %>% select(v001, v002, caseid, partner.occup:walls.notbamboo)
    eth_HH_orig.merge <- eth_HH_orig %>% select(v001, v002, toilet.type)
    
    newmerge <-merge(eth_IR_orig.merge, eth_HH_orig.merge, by=c("v001", "v002"), all.x=T)
    eth_dhs <- merge(eth_dhs, newmerge, by="caseid", all.x=T)
    
  # Set cluster variable
    eth_dhs$cluster <- factor(eth_dhs$cluster_k5_rename_short)
    
  # Define alternative wealth index
    ## full
    eth_dhs<-eth_dhs %>% mutate(wealth.index.alt = hh.tv + hh.refrig + hh.cook_notwood + hh.floor_notearth +
                                            hh.cook_elect + hh.electricity + hh.bank.acct +
                                            water.piped + hh.computer + toilet.facility +
                                            hh.table + hh.radio + hh.watch + hh.chair +
                                            hh.phone + water.notpublic + roof.notsod + walls.notbamboo +
                                            hh.car)
    nlab<-c(1,2,3,4,5)
    eth_dhs<-mutate(eth_dhs,asset.quint=as.factor(cut_number(eth_dhs$wealth.index.alt,n=5,labels=nlab)))
    
    ## with paired down list to top 5 based on SD within urban/rural - don't break down by quintiles, just use the index
    eth_dhs<-eth_dhs %>% mutate(wealth.index.alt.urban = hh.tv + hh.cook_notwood + water.piped + hh.bank.acct + 
                                            hh.land,
                                          wealth.index.alt.rural = toilet.facility + hh.table + hh.chair + hh.floor_notearth +
                                            asset.goats)
    
  
  # Subset by urban/rural #
    eth_dhs.urban <- eth_dhs %>% filter(URBAN == 1)
    eth_dhs.urban$cluster <- droplevels(eth_dhs.urban$cluster)
    eth_dhs.rural <- eth_dhs %>% filter(URBAN == 2)
    eth_dhs.rural$cluster <- droplevels(eth_dhs.rural$cluster)
    
  
## Kenya
    # Urban
    ken_path$urban <- ifelse(ken_path$URBAN_RURA == "U  ", 1, 0)
    
    # Household head sex
    ken_path$head.sex <- ifelse(grepl("female", ken_path$S07, ignore.case = TRUE), "female",
                                ifelse(grepl("brother", ken_path$S07, ignore.case = TRUE),  "male", 
                                       ifelse(grepl("father", ken_path$S07, ignore.case = TRUE), "male", 
                                              ifelse(grepl("nephew", ken_path$S07, ignore.case = TRUE), "male",
                                                     ifelse(grepl("son", ken_path$S07, ignore.case = TRUE), "male",
                                                            ifelse(grepl("husband", ken_path$S07, ignore.case = TRUE),"male",
                                                                   ifelse(grepl("uncle", ken_path$S07, ignore.case = TRUE),"male",
                                                                   ifelse(grepl("boyfriend", ken_path$S07, ignore.case = TRUE), "male",
                                                                          ifelse(grepl("male", ken_path$S07, ignore.case = TRUE),"male",
                                                                                 "female")))))))))
    
    # Religion
    ken_path$religionrecode.cat <- ifelse(ken_path$E39_O1 %in% c("Muslim"), "Muslim",
                                          ifelse(ken_path$E39_O1 %in% c("Christian - Protestant"), "Christian - Protestant",
                                                 ifelse(ken_path$E39_O1 %in% c("Christian - Catholic"), "Christian - Catholic",
                                                                               "Other")))
    
    # Partnership status
    ken_path <- ken_path %>% mutate(pship.cat = case_when(
      A01 %in% c("Yes -  currently married", "Yes - have a boyfriend or partner") ~ "married/cohab",
      A01 %in% c("No -  widowed / partner died", "No -  divorced / separated") ~ "no partner now",
      A01 %in% c("Never married / never in union") ~ "never",
      S03_O1 %in% c("Husband", "Partner or boyfriend") ~ "married/cohab"
                                                     ))
    
    # Education
    ken_path$ed.cat<-ifelse(ken_path$E07a %in% c("No school"), "None",
                                ifelse(ken_path$E07a %in% c("Primary"), "Primary",
                                       ifelse(ken_path$E07a %in% c("Secondary", "Higher"), "Higher", NA)))
    
    # Patner education
    ken_path <- ken_path %>% mutate(part.ed.cat = case_when(
      E07c == "No school" ~ "None",
      E07c %in% c("Primary") ~ "Primary",
      E07c %in% c("Secondary", "Higher") ~ "Higher",
      E07c %in% c("Don't know") ~ "Don't know",
      pship.cat %in% c("never", "no partner now") ~ "No partner"
    ))
      
    # Working y/n
    E08.set = grep("E08_", names(ken_path), value=TRUE)
    ken_path$working.yn = 0
    for (i in 1:length(E08.set)) {
      ken_path$working.yn[ken_path[,E08.set[i]] == "Self"] = 1
    }
    
    # Partner occupation

    
    # Radio and TV freq
    ken_path$freq.rad <- ken_path$T_E34_2
    ken_path$freq.tv <- ken_path$T_E34_3
    ken_path$newsp.freq <- ken_path$T_E34_4
    
    ken_path$any.media.yn <- ifelse((ken_path$freq.rad %in% c("Not at all") | is.na(ken_path$freq.rad)) & 
                                      (ken_path$freq.tv %in% c("Not at all") | is.na(ken_path$freq.tv)) &
                                       (ken_path$newsp.freq %in% c("Not at all") | is.na(ken_path$newsp.freq)), 0, 1)
    
    # Wealth index
    ken_path$hh.phone <- ifelse(ken_path$IO19 %in% c("Owns cell phone alone ", "Owns shared cell phone", 
                                                     "Owns both shared and personal cell phones", "Someone else in household owns, but not her"), 1, 
                                ifelse(ken_path$IO19 == "No one in household", 0, NA))
    ken_path$land<-ifelse(ken_path$E21=="No one in household",0,1)
    ken_path$hh.electricity<-ifelse(ken_path$E23=="Yes",1,0)
    ken_path$bike<-ifelse(ken_path$T_E35_1=="Yes",1,0)
    ken_path$scoot<-ifelse(ken_path$T_E35_2=="Yes",1,0)
    ken_path$car<-ifelse(ken_path$T_E35_3=="Yes",1,0)
    ken_path$boat<-ifelse(ken_path$T_E35_4=="Yes",1,0)
    ken_path$rooms<-ifelse(ken_path$IO16=="1 room",1,
                     ifelse(ken_path$IO16=="2 rooms",2,
                            ifelse(ken_path$IO16=="3 rooms",3,
                                   ifelse(ken_path$IO16=="4 rooms",4,5))))
    ken_path$flusht<-ifelse(ken_path$IO21=="Flush toilet",1,0)
    ken_path$pittoi<-ifelse(ken_path$IO21=="Pit latrine",1,0)
    ken_path$notoi<-ifelse(ken_path$IO21=="No facility/bush/field",1,0)
    ken_path$bucktoi<-ifelse(ken_path$IO21=="Bucket in house",1,0)
    ken_path$toishare<-ifelse(ken_path$IO22=="Yes",1,0); ken_path$toishare[is.na(ken_path$toishare)]<-0
    ken_path$watpipe<-ifelse(ken_path$IO26=="Yes, piped water available",1,0)
    ken_path$roofno<-ifelse(ken_path$IO5=="No roof",1,0)
    ken_path$roofnat<-ifelse(ken_path$IO5=="Thatch/palm leaf"|ken_path$IO5=="Sod"|ken_path$IO5=="Other, specify:",1,0)
    ken_path$roofrud<-ifelse(ken_path$IO5=="Rustic mat"|ken_path$IO5=="Palm/bamboo"|ken_path$IO5=="Wood planks"|ken_path$IO5=="Cardboard",1,0)
    ken_path$rooffin<-ifelse(ken_path$IO5=="Metal / iron sheets"|ken_path$IO5=="Wood"|ken_path$IO5=="Calamine/cement fiber"|ken_path$IO5=="Ceramic tiles"|ken_path$IO5=="Cement"|ken_path$IO5=="Roofing shingles",1,0)
    ken_path$wallno<-ifelse(ken_path$IO6=="No walls",1,0)
    ken_path$wallnat<-ifelse(ken_path$IO6=="Cane/Palm/Trunks"|ken_path$IO6=="Dirt / mud"|ken_path$IO6=="Other, specify:",1,0)
    ken_path$wallrud<-ifelse(ken_path$IO6=="Bamboo with mud"|ken_path$IO6=="Stone with mud"|ken_path$IO6=="Uncovered adobe"|ken_path$IO6=="Plywood"|ken_path$IO6=="Cardboard"|ken_path$IO6=="Reused wood",1,0)
    ken_path$wallfin<-ifelse(ken_path$IO6=="Cement"|ken_path$IO6=="Stone with lime/cement"|ken_path$IO6=="Bricks"|ken_path$IO6=="Cement blocks"|ken_path$IO6=="Covered adobe"|ken_path$IO6=="Wood plank/shingles"|ken_path$IO6=="Metal/ iron sheets",1,0)
    ken_path$sewerno<-ifelse(ken_path$IO9=="No drainage",1,0)
    ken_path$seweropu<-ifelse(ken_path$IO9=="Open, but uncemented",1,0)
    ken_path$seweropc<-ifelse(ken_path$IO9=="Open, but cemented",1,0)
    ken_path$sewercl<-ifelse(ken_path$IO9=="Closed sewer",1,0)
    ken_path$ownhome<-ifelse(ken_path$E16=="Own home",1,0)
    ken_path$hungry<-ifelse(ken_path$T_A11_1=="Yes",1,0)
    ken_path$reloc<-ifelse(ken_path$T_A11_3=="Yes",1,0)
    ken_path$kidwrk<-ifelse(ken_path$T_A11_5=="Yes",1,0)
    
    wealth<-ken_path %>% select(c("land","hh.electricity","hh.phone","bike","scoot","car","boat","rooms","flusht","pittoi","notoi","bucktoi","toishare","watpipe","roofno","roofnat","roofrud","rooffin",
                            "wallno","wallnat","wallrud","wallfin","sewerno","seweropu","seweropc","sewercl","ownhome","hungry","reloc","kidwrk"))
    
    prn<-principal(wealth, rotate="varimax", nfactors=5,covar=T, scores=TRUE)
    ken_path$wealth.index<-prn$scores[,1]
    nlab<-c(1,2,3,4,5)
    ken_path<-mutate(ken_path,wealth.quint=as.factor(cut(ken_path$wealth.index,breaks=5,labels=nlab)))
    
    
    # Asset ownership index (motorcycle/scooter, car/truck, cellphone, bank account )
   
    ken_path$hh.bank.acct <- ifelse(ken_path$E25 %in% c("Has own bank account alone ", "Has joint account", 
                                                         "Has both joint and separate accounts", "Someone else in household has bank account"), 1, 
                                    ifelse(ken_path$E25 %in% c("No one in household has bank account", "Don't know") , 0, NA))
    ken_path$hh.motor<-ifelse(ken_path$T_E35_2=="Yes",1,0)
    ken_path$hh.car<-ifelse(ken_path$T_E35_3=="Yes",1,0)
    
    ken_path <- ken_path %>% 
      rowwise() %>% mutate(asset.index = as.numeric(sum(hh.motor,hh.car, hh.phone, hh.bank.acct, na.rm=TRUE)))
    ken_path$asset.index<-ifelse(is.na(ken_path$hh.motor) & is.na(ken_path$hh.car) & is.na(ken_path$hh.phone) & is.na(ken_path$hh.bank.acct), NA, ken_path$asset.index)
    
    # household electricity
    ken_path$elec<-ifelse(ken_path$E23=="Yes",1,0)
    
    # Water treatment
    D03.set = grep("D03_", names(ken_path), value=TRUE)
    ken_path$hh.watpur.appr = 0
    for (i in 1:length(D03.set)) {
      ken_path$hh.watpur.appr[ken_path[,D03.set[i]] == "Boil"] = 1
      ken_path$hh.watpur.appr[ken_path[,D03.set[i]] == "Add bleach/chlorine"] = 1
      ken_path$hh.watpur.appr[ken_path[,D03.set[i]] == "Strain through a cloth"] = 1
      ken_path$hh.watpur.appr[ken_path[,D03.set[i]] == "Use water filter (ceramic/ sand/ composite/ etc.]"] = 1
      ken_path$hh.watpur.appr[ken_path[,D03.set[i]] == "Let it stand and settle"] = 1
      ken_path$hh.watpur.appr[ken_path[,D03.set[i]] == "Solar disinfection"] = 1
    }

    # toilet facility (No facility, bush, field / bucket in house / pit latrine, flush toilet)
    ken_path$toilet.type <- ifelse(ken_path$IO21 %in% c("No facility/bush/field"), "No facility/bush/field",
                                      ifelse(ken_path$IO21 %in% c("Bucket in house"), "Bucket","Latrine or toilet"))
    
    # Belief about gender control over money
    ken_path$dec.money <- ifelse(ken_path$T_B01_7 %in% c("Completely agree", "Agree"), 1, 0)
    
    # Home birth safety
    ken_path$hbsafe<-ifelse(ken_path$T_B02_5 %in% c("Completely agree", "Agree"), 1, 0)
    
    # Index of barriers to seeking med
    
    ken_path$med.permis<-ifelse(ken_path$T_C32_1=="Big problem ",1,0)
    ken_path$med.cost<-ifelse(ken_path$T_C32_2=="Big problem ",1,0)
    ken_path$med.dist<-ifelse(ken_path$T_C32_3=="Big problem ",1,0)
    ken_path$med.alone<-ifelse(ken_path$T_C32_4=="Big problem ",1,0)
    
    ken_path$med.nacnt<-ken_path  %>% 
      rowwise() %>% summarise(med.nacnt = sum(is.na(med.permis),is.na(med.cost),is.na(med.dist),is.na(med.alone)))
    ken_path<-ken_path %>% 
      rowwise() %>% mutate(med.sum = sum(med.permis, med.cost, med.dist, med.alone, na.rm=TRUE))
    ken_path$med.index<-ifelse(ken_path$med.nacnt == 9, NA, ken_path$med.sum)
    ken_path$med.index<-as.numeric(ken_path$med.index)
    ken_path <- ken_path %>% select(-c(med.sum, med.nacnt))
    
    
    # Modern contraception now
    ken_path$C361<-ifelse(ken_path$C36_O1=="No method"|ken_path$C36_O1=="Standard days/Rhythm method"|ken_path$C36_O1=="Lactational amenorrhoea method"|ken_path$C36_O1=="Withdrawal method"|ken_path$C36_O1=="Other method, specify:",0,1)
    ken_path$C362<-ifelse(ken_path$C36_O2=="No method"|ken_path$C36_O2=="Standard days/Rhythm method"|ken_path$C36_O2=="Lactational amenorrhoea method"|ken_path$C36_O2=="Withdrawal method"|ken_path$C36_O2=="Other method, specify:",0,1)
    ken_path$C363<-ifelse(ken_path$C36_O3=="No method"|ken_path$C36_O3=="Standard days/Rhythm method"|ken_path$C36_O3=="Lactational amenorrhoea method"|ken_path$C36_O3=="Withdrawal method"|ken_path$C36_O3=="Other method, specify:",0,1)
    ken_path$C364<-ifelse(ken_path$C36_O4=="No method"|ken_path$C36_O4=="Standard days/Rhythm method"|ken_path$C36_O4=="Lactational amenorrhoea method"|ken_path$C36_O4=="Withdrawal method"|ken_path$C36_O4=="Other method, specify:",0,1)
    ken_path$C365<-ifelse(ken_path$C36_O5=="No method"|ken_path$C36_O5=="Standard days/Rhythm method"|ken_path$C36_O5=="Lactational amenorrhoea method"|ken_path$C36_O5=="Withdrawal method"|ken_path$C36_O5=="Other method, specify:",0,1)
    ken_path$C366<-ifelse(ken_path$C36_O6=="No method"|ken_path$C36_O6=="Standard days/Rhythm method"|ken_path$C36_O6=="Lactational amenorrhoea method"|ken_path$C36_O6=="Withdrawal method"|ken_path$C36_O6=="Other method, specify:",0,1)
    ken_path$fp.mod.now<-ifelse(ken_path$C361==1|ken_path$C362==1|ken_path$C363==1|ken_path$C364==1|ken_path$C365==1|ken_path$C366==1,"Yes","No")
    ken_path$fp.mod.now<-ifelse(ken_path$C35=="Currently pregnant", "Currently Pregnant",ken_path$fp.mod.now)
    ken_path$fp.mod.now[is.na(ken_path$fp.mod.now)]<-"No"
    ken_path <- ken_path %>% select(-c(C361:C366))
    
    # Parity (total children ever born)
    ken_path$parity <- ifelse(is.na(ken_path$T_C13_1), "0",
                                       ifelse(ken_path$T_C13_1 %in% c(1,2,3), "1-3",
                                              ifelse(ken_path$T_C13_1 %in% c(4), "4",
                                                     ifelse(ken_path$T_C13_1 %in% c(5:100), "5+", NA))))
    
    # ANC count - include never pregnant / dk in 0 
    ken_path$anc.4plus<-ifelse(ken_path$C17=="4 or more",1,0)
    
    # Ever had a home birth
    ken_path$brth.700s<-ifelse(ken_path$C21_S_T_C21_700=="home                                                                                                                    "|
                           ken_path$C21_S_T_C21_700=="Home                                                                                                                    "|
                           ken_path$C21_S_T_C21_700=="kwa barabara                                                                                                            "|
                           ken_path$C21_S_T_C21_700=="kwa nyumba                                                                                                              "|
                           ken_path$C21_S_T_C21_700=="nyumba                                                                                                                  "|
                           ken_path$C21_S_T_C21_700=="nyumbani                                                                                                                ",1,0)
    ken_path$brth.701s<-ifelse(ken_path$C21_S_T_C21_701=="home                                                                                                                    "|
                           ken_path$C21_S_T_C21_701=="Home                                                                                                                    "|
                           ken_path$C21_S_T_C21_701=="by the road side                                                                                                        "|
                           ken_path$C21_S_T_C21_701=="Nyumbani                                                                                                                "|
                           ken_path$C21_S_T_C21_701=="nyumbani                                                                                                                ",1,0)
    ken_path$brth.702s<-ifelse(ken_path$C21_S_T_C21_702=="home                                                                                                                    "|
                           ken_path$C21_S_T_C21_702=="nyumbani                                                                                                                ",1,0)
    ken_path$brth.703s<-ifelse(ken_path$C21_S_T_C21_703=="home                                                                                                                    "|
                           ken_path$C21_S_T_C21_703=="nyumbani                                                                                                                ",1,0)
    ken_path$brth.704s<-ifelse(ken_path$C21_S_T_C21_704=="home                                                                                                                    ",1,0)
    ken_path$brth.705s<-ifelse(ken_path$C21_S_T_C21_705=="home                                                                                                                    ",1,0)
    
    ken_path$brth.800s<-ifelse(ken_path$C21_S_T_C21_800=="home                                                                                                                    "|
                           ken_path$C21_S_T_C21_800=="At home                                                                                                                 "|
                           ken_path$C21_S_T_C21_800=="Home                                                                                                                    "|
                           ken_path$C21_S_T_C21_800=="barabarani                                                                                                              "|
                           ken_path$C21_S_T_C21_800=="kwa nyumbani                                                                                                            "|
                           ken_path$C21_S_T_C21_800=="nyumba                                                                                                                  "|
                           ken_path$C21_S_T_C21_800=="nyumbani                                                                                                                "|
                           ken_path$C21_S_T_C21_800=="njiani                                                                                                                  ",1,0)
    ken_path$brth.801s<-ifelse(ken_path$C21_S_T_C21_801=="home                                                                                                                    "|
                           ken_path$C21_S_T_C21_801=="At home                                                                                                                 "|
                           ken_path$C21_S_T_C21_801=="Home                                                                                                                    "|
                           ken_path$C21_S_T_C21_801=="nyumbani                                                                                                                ",1,0)
    ken_path$brth.802s<-ifelse(ken_path$C21_S_T_C21_802=="nyumbani                                                                                                                ",1,0)
    ken_path$brth.803s<-ifelse(ken_path$C21_S_T_C21_803=="nyumbani                                                                                                                ",1,0)
    
    ken_path$hb.700<-ifelse(ken_path$T_C21_700=="Home"|ken_path$T_C21_700=="On the way to the hospital"|ken_path$brth.700s==1,1,0); ken_path$hb.700[is.na(ken_path$hb.700)]<-0
    ken_path$hb.701<-ifelse(ken_path$T_C21_701=="Home"|ken_path$T_C21_701=="On the way to the hospital"|ken_path$brth.701s==1,1,0); ken_path$hb.701[is.na(ken_path$hb.701)]<-0
    ken_path$hb.702<-ifelse(ken_path$T_C21_702=="Home"|ken_path$T_C21_702=="On the way to the hospital"|ken_path$brth.702s==1,1,0); ken_path$hb.702[is.na(ken_path$hb.702)]<-0
    ken_path$hb.703<-ifelse(ken_path$T_C21_703=="Home"|ken_path$T_C21_703=="On the way to the hospital"|ken_path$brth.703s==1,1,0); ken_path$hb.703[is.na(ken_path$hb.703)]<-0
    ken_path$hb.704<-ifelse(ken_path$T_C21_704=="Home"|ken_path$T_C21_704=="On the way to the hospital"|ken_path$brth.704s==1,1,0); ken_path$hb.704[is.na(ken_path$hb.704)]<-0
    ken_path$hb.705<-ifelse(ken_path$T_C21_705=="Home"|ken_path$T_C21_705=="On the way to the hospital"|ken_path$brth.705s==1,1,0); ken_path$hb.705[is.na(ken_path$hb.705)]<-0
    ken_path$hb.706<-ifelse(ken_path$T_C21_706=="Home"|ken_path$T_C21_706=="On the way to the hospital",1,0); ken_path$hb.706[is.na(ken_path$hb.706)]<-0
    ken_path$hb.800<-ifelse(ken_path$T_C21_800=="Home"|ken_path$T_C21_800=="On the way to the hospital"|ken_path$brth.800s,1,0); ken_path$hb.800[is.na(ken_path$hb.800)]<-0
    ken_path$hb.801<-ifelse(ken_path$T_C21_801=="Home"|ken_path$T_C21_801=="On the way to the hospital"|ken_path$brth.801s,1,0); ken_path$hb.801[is.na(ken_path$hb.801)]<-0
    ken_path$hb.802<-ifelse(ken_path$T_C21_802=="Home"|ken_path$T_C21_802=="On the way to the hospital"|ken_path$brth.802s,1,0); ken_path$hb.802[is.na(ken_path$hb.802)]<-0
    ken_path$hb.803<-ifelse(ken_path$T_C21_803=="Home"|ken_path$T_C21_803=="On the way to the hospital"|ken_path$brth.803s,1,0); ken_path$hb.803[is.na(ken_path$hb.803)]<-0
    ken_path$hb.804<-ifelse(ken_path$T_C21_804=="Home"|ken_path$T_C21_804=="On the way to the hospital",1,0); ken_path$hb.804[is.na(ken_path$hb.804)]<-0
    ken_path$hb.805<-ifelse(ken_path$T_C21_805=="Home"|ken_path$T_C21_805=="On the way to the hospital",1,0); ken_path$hb.805[is.na(ken_path$hb.805)]<-0
    ken_path$hb.806<-ifelse(ken_path$T_C21_806=="Home"|ken_path$T_C21_806=="On the way to the hospital",1,0); ken_path$hb.806[is.na(ken_path$hb.806)]<-0
    ken_path$hb.807<-ifelse(ken_path$T_C21_807=="Home"|ken_path$T_C21_807=="On the way to the hospital",1,0); ken_path$hb.807[is.na(ken_path$hb.807)]<-0
    ken_path$hb.808<-ifelse(ken_path$T_C21_808=="Home"|ken_path$T_C21_808=="On the way to the hospital",1,0); ken_path$hb.808[is.na(ken_path$hb.808)]<-0
    
    ken_path$hb.ever<-ifelse(ken_path$hb.700==1|ken_path$hb.701==1|ken_path$hb.702==1|ken_path$hb.703==1|ken_path$hb.704==1|ken_path$hb.705==1|ken_path$hb.706==1|
                         ken_path$hb.800==1|ken_path$hb.801==1|ken_path$hb.802==1|ken_path$hb.803==1|ken_path$hb.804==1|ken_path$hb.805==1|ken_path$hb.806==1|ken_path$hb.807==1|ken_path$hb.808==1,1,0)
    ken_path$hb.ever[is.na(ken_path$hb.ever)]<-0
    ken_path <- ken_path %>% select(-c(brth.700s:hb.808))
    
    
    # New seg
    old<-c("RF4",
           "RF1",
           "RM1",
           "RM3",
           "RM2",
           "UF3",
           "UF1",
           "UM5",
           "UM1")
    new<-c("RF2",
           "RF4",
           "RM3",
           "RM2",
           "RM4",
           "UF2",
           "UF4",
           "UM1",
           "UM4")
    new.seg<-data.frame("urfm.seg"=old, "cluster"=new)
    new.seg$cluster <- factor(new.seg$cluster)
    
    # Unique ID
    ken_path$caseid<-paste(ken_path$PSUclean,ken_path$HHIDclean,ken_path$ITYPclean)
    
    # Select vars to keep
    ken_path_keep <- ken_path %>% select("HHIDclean","PSUclean","caseid", "urfm.seg","base.wt", "urban":"hb.ever")
    
    ken_path <-merge(ken_path_keep, new.seg, by.x="urfm.seg", by.y="urfm.seg",all.x=T)
    
        
#### Perform CART ####
    set.seed(123)
    
 # 1. Ethiopia ####
    
    #### URBAN ####
    
    #A# wealth only (original index)
    ## Grow tree - set cp to negative to get all branches of weatlh
    tree_urban <- rpart(cluster ~ wealth.quint, 
                        data=eth_dhs.urban, control=rpart.control(cp=-0.001, xval=5), method="class")
    
    plotcp(tree_urban); printcp(tree_urban)

    # plot
    plot(tree_urban, uniform=TRUE, main="Classification Tree for Urban LCA")
    text(tree_urban, use.n=TRUE, all=TRUE, cex=.7)
    prp(tree_urban, type=3, varlen=0, cex=1)
    # fancyRpartPlot(tree_urban, type = 3, cex = 0.7, caption = NULL, uniform=T)
    
    PredictCART_train_urban <- predict(tree_urban, data=eth_dhs.urban, type="class")
    confusionMatrix(data=PredictCART_train_urban, eth_dhs.urban$cluster)
    
    #B# Full list of variables
    ## Grow tree
    tree_urban <- rpart(cluster ~ head.sex + wealth.quint + religionrecode.cat + pship.cat + ed.cat + part.ed.cat + working.yn + partner.occup + 
                        freq.rad + any.media.yn + freq.tv + wealth.quint + asset.index + hh.electricity + hh.watpur.appr + toilet.type +
                        dec.money + med.index + med.permis + fp.mod.now + parity + anc.4plus + hb.ever,
                        data=eth_dhs.urban, control=rpart.control(cp=0.0001, xval=5), method="class")

    plotcp(tree_urban); printcp(tree_urban)
    
    ## prune tree
    #tree_urban_prune<-prune(tree_urban, cp=tree_urban$cptable[which.min(tree_urban$cptable[,"xerror"]),"CP"])
    tree_urban_prune<-prune(tree_urban, cp=.001)
    plotcp(tree_urban_prune); printcp(tree_urban_prune)
    
    
    ## plot
    plot(tree_urban_prune, uniform=TRUE, main="Classification Tree for Urban LCA")
    text(tree_urban_prune, use.n=TRUE, all=TRUE, cex=.7)
    prp(tree_urban_prune, type=3, varlen=0, cex=1)
    # fancyRpartPlot(tree_urban_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)
    
    PredictCART_train_urban <- predict(tree_urban_prune, data=eth_dhs.urban, type="class")
    confusionMatrix(data=PredictCART_train_urban, eth_dhs.urban$cluster)
    
    
    #### RURAL ####
    
    #A# wealth only (original index)
    ## Grow tree - set cp to negative to get all branches of weatlh
    tree_rural <- rpart(cluster ~ wealth.quint, 
                        data=eth_dhs.rural, control=rpart.control(cp=-0.001, xval=5), method="class")
    
    plotcp(tree_rural); printcp(tree_rural)
    
    # plot
    plot(tree_rural, uniform=TRUE, main="Classification Tree for rural LCA")
    text(tree_rural, use.n=TRUE, all=TRUE, cex=.7)
    prp(tree_rural, type=3, varlen=0, cex=1)
    # fancyRpartPlot(tree_rural, type = 3, cex = 0.7, caption = NULL, uniform=T)
    
    PredictCART_train_rural <- predict(tree_rural, data=eth_dhs.rural, type="class")
    confusionMatrix(data=PredictCART_train_rural, eth_dhs.rural$cluster)
  
    
    #B# Full list of variables (TRY WITH AND WITHOUT PASTORAL)
    ## Grow tree 
    tree_rural <- rpart(cluster ~ head.sex + wealth.quint + religionrecode.cat + pship.cat + ed.cat + part.ed.cat + working.yn + partner.occup + 
                          freq.rad + any.media.yn + freq.tv + wealth.quint + asset.index + hh.electricity + hh.watpur.appr + toilet.type +
                          #pastoralnoagro +
                          dec.money + med.index + med.permis + fp.mod.now + parity + anc.4plus + hb.ever,
                        data=eth_dhs.rural, control=rpart.control(cp=0.0001, xval=5), method="class")
    
    plotcp(tree_rural); printcp(tree_rural)
    
    ## prune tree
    #tree_rural_prune<-prune(tree_rural, cp=tree_rural$cptable[which.min(tree_rural$cptable[,"xerror"]),"CP"])
    tree_rural_prune<-prune(tree_rural, cp=.001)
    plotcp(tree_rural_prune); printcp(tree_rural_prune)
    
    
    ## plot
    plot(tree_rural_prune, uniform=TRUE, main="Classification Tree for rural LCA")
    text(tree_rural_prune, use.n=TRUE, all=TRUE, cex=.7)
    prp(tree_rural_prune, type=3, varlen=0, cex=1)
    # fancyRpartPlot(tree_rural_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)
    
    PredictCART_train_rural <- predict(tree_rural_prune, data=eth_dhs.rural, type="class")
    confusionMatrix(data=PredictCART_train_rural, eth_dhs.rural$cluster)
    
  # 2. Kenya ####
    
    #A# wealth only (original index)
    ## Grow tree - set cp to negative to get all branches of weatlh
    tree_ken <- rpart(cluster ~ wealth.quint, 
                        data=ken_path, control=rpart.control(cp=-0.001, xval=5), method="class")
    
    plotcp(tree_ken); printcp(tree_ken)
    
    # plot
    plot(tree_ken, uniform=TRUE, main="Classification Tree for Urban LCA")
    text(tree_ken, use.n=TRUE, all=TRUE, cex=.7)
    prp(tree_ken, type=3, varlen=0, cex=1)
    # fancyRpartPlot(tree_ken, type = 3, cex = 0.7, caption = NULL, uniform=T)
    
    PredictCART_train_ken <- predict(tree_ken, data=ken_path, type="class")
    confusionMatrix(data=PredictCART_train_ken, ken_path$cluster)
    
    #B# Full list of variables 
    ## Grow tree - add urban, hbsafe, helpless
    tree_ken <- rpart(cluster ~ urban + head.sex + wealth.quint + religionrecode.cat + pship.cat + ed.cat + part.ed.cat + working.yn + 
                        #partner.occup + 
                          freq.rad + any.media.yn + freq.tv + wealth.quint + asset.index + hh.electricity + hh.watpur.appr + toilet.type +
                          dec.money + med.index + med.permis + hbsafe + 
                        #helpless + 
                        fp.mod.now + parity + anc.4plus + hb.ever,
                        data=ken_path, control=rpart.control(cp=0.0001, xval=5), method="class")
    
    plotcp(tree_ken); printcp(tree_ken)
    
    ## prune tree
    #tree_ken_prune<-prune(tree_ken, cp=tree_ken$cptable[which.min(tree_ken$cptable[,"xerror"]),"CP"])
    tree_ken_prune<-prune(tree_ken, cp=.002)
    plotcp(tree_ken_prune); printcp(tree_ken_prune)
    
    
    ## plot
    plot(tree_ken_prune, uniform=TRUE, main="Classification Tree for rural LCA")
    text(tree_ken_prune, use.n=TRUE, all=TRUE, cex=.7)
    prp(tree_ken_prune, type=3, varlen=0, cex=1)
    # fancyRpartPlot(tree_ken_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)
    
    PredictCART_train_rural <- predict(tree_ken_prune, data=ken_path, type="class")
    confusionMatrix(data=PredictCART_train_rural, ken_path$cluster)
    
    
    #C# Original typing tool list 
    ## Grow tree - add urban, hbsafe, helpless
    tree_ken <- rpart(cluster ~ urban + head.sex + religionrecode.cat + ed.cat + part.ed.cat + 
                       freq.rad + freq.tv + asset.index + hh.watpur.appr + toilet.type +
                        fp.mod.now + parity,
                      data=ken_path, control=rpart.control(cp=-0.0001, xval=5), method="class")
    
    plotcp(tree_ken); printcp(tree_ken)
    
    ## prune tree
    #tree_ken_prune<-prune(tree_ken, cp=tree_ken$cptable[which.min(tree_ken$cptable[,"xerror"]),"CP"])
    # tree_ken_prune<-prune(tree_ken, cp=.002)
    # plotcp(tree_ken_prune); printcp(tree_ken_prune)
    # 
    
    ## plot
    plot(tree_ken, uniform=TRUE, main="Classification Tree for rural LCA")
    text(tree_ken, use.n=F, all=TRUE, cex=.7)
    prp(tree_ken, type=3, varlen=0, cex=0.4)
    # fancyRpartPlot(tree_ken, type = 3, cex = 0.7, caption = NULL, uniform=T)
    
    PredictCART_train <- predict(tree_ken, data=ken_path, type="class")
    confusionMatrix(data=PredictCART_train, ken_path$cluster)
    
    
