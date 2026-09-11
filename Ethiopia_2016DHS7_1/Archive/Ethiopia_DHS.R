rm(list=ls())
library(foreign)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(psych)
library(vtable)
library(corrplot)
library(haven)
library(survey)


############## Load Ethiopia DHS data
#ETBR <- data.table(read.dta("C:/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways/rawdata/ETH_DHS/ET_2016/ETBR71DT/ETBR71FL.DTA")) 
ETHH <- data.table(read.dta("C:/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways/rawdata/ETH_DHS/ET_2016/ETHR71DT/ETHR71FL.DTA")) 
ETIR <- data.table(read.dta("C:/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways/rawdata/ETH_DHS/ET_2016/ETIR71DT/ETIR71FL.DTA")) 

# New seg
old<-c("RF4","RF1","RM1","RM3","RM2","UF3","UF1","UM5","UM1")
new<-c("RF2","RF4","RM3","RM2","RM4","UF2","UF4","UM1","UM4")
new.seg<-data.frame("seg1"=old, "new.seg"=new)


# Recode all variables needed for segmentation

# Recode urban/rural
ETIR$urban<-ETIR$v102

# Recode head of household sex
ETIR$head.sex<-ETIR$v151

# Recode vehicle use
ETIR[,own.car:=ifelse(v125=="yes",1,0)]
ETIR$own.car <- ifelse((is.na(ETIR$v125)|(ETIR$v125=="not a dejure resident")),NA, ETIR$own.car)

ETIR[,own.motor:=ifelse(v124== "yes",1,0)]
ETIR$own.motor <- ifelse((is.na(ETIR$v124) | (ETIR$v124=="not a dejure resident")),NA, ETIR$own.motor)

# Recode radio and TV exposure
# Radio is 158
ETIR<-ETIR %>% mutate(freq.rad = case_when(
                (v158== "not at all" | is.na(v158)) ~ 0,
                v158=="at least once a week" ~2,
                v158=="less than once a week" ~1))

ETIR$rad.yn<-ifelse((ETIR$v158== "not at all" | is.na(ETIR$v158)),0,1)

ETIR<-ETIR %>% mutate(freq.tv = case_when(
  (v159=="not at all" | is.na(v159)) ~ 0,
   v159 == "at least once a week" ~2,
   v159 == "less than once a week" ~1))

ETIR$tv.yn<-ifelse((ETIR$v159=="not at all" | is.na(ETIR$v159)),0,1)

ETIR[,table(v133, useNA="always")]

# Recode education
ETIR$ed.yrs<-ETIR$v133
ETIR$ed.grp<-ETIR$v106
ETIR$ed.grp.seg<-ETIR$v149 #one used for segmentation

ETIR<-ETIR %>% replace_na(list(v155= "blind/visually impaired"))
ETIR$ed.yrs.lit <- ifelse(ETIR$v155 == "cannot read at all",0, ETIR$v133)

# 0=no partner, 1=no education, 2=incomplete primary, 3=complete primary, 4=incomplete secondary
# 5=complete secondary, 6=higher, 7=don't known 
ETIR[,husb.ed.grp.seg:=v729]
ETIR$no.partner<-ifelse((ETIR$v502== "never in union"| ETIR$v502=="formerly in union/living with a man"),1,0)
ETIR$husb.ed.grp.seg<-ifelse(ETIR$no.partner == 1,0,ETIR$husb.ed.grp.seg)

# Recode FP
ETIR$fp.mod.now <- ifelse(ETIR$v313 == "modern method",1,0)
ETIR$fp.dis.5yr<-ifelse((ETIR$v359=="pill"|ETIR$v359=="iud"|ETIR$v359=="injections"|ETIR$v359=="male condom"|
                           ETIR$v359=="implants/norplant"|ETIR$v359=="female condom"|ETIR$v359=="emergency contraception"|
                           ETIR$v359=="other modern method"),1,0)
ETIR$fp.dis.5yr[is.na(ETIR$fp.dis.5yr)] <- 0
ETIR$fp.mod.ever<-ifelse((ETIR$fp.mod.now==1 | ETIR$fp.dis.5yr==1),1,0)


# Recode HH size
ETIR[,num.hhmem:=v136]
ETIR[,num.bio.child:=v201] #used for segmentation
ETIR$num.child.die <- ETIR$v206 + ETIR$v207

# Recode religion
ETIR$muslim<-ifelse(ETIR$v130 == "muslin",1,0)

# Recode age
ETIR[,woman.age:=v012]
ETIR[,woman.agegrp:=v013]

# Recode age at first sex, marriage, birth
ETIR$age.1stsex <- ifelse(ETIR$v531 == 97,17, 
                             ifelse(ETIR$v531 == 98, 17, 
                                    ifelse(ETIR$v531 == 0, NA, ETIR$v531)))
ETIR<- ETIR %>% mutate(age.1stsex.cat = case_when(
  is.na(age.1stsex) ~ "never",
  (age.1stsex > 0 & age.1stsex < 15) ~ "5-14",
  (age.1stsex >= age.1stsex & age.1stsex < 20) ~ "15-19",
  (age.1stsex >= 20 & age.1stsex < 50) ~ "20+"))
  
ETIR$age.1stcohab<-ETIR$v511
ETIR<- ETIR %>% mutate(age.1stcohab.cat = case_when(
  is.na(v511) ~ "never",
  (v511 > 0 & v511 < 15) ~ "5-14",
  (v511 >= 15 & v511 < 20) ~ "15-19",
  (v511 >= 20) ~ "20+"))

ETIR$age.1stbrth<-ETIR$v212
ETIR<- ETIR %>% mutate(age.1stbrth.cat = case_when(
  is.na(v212) ~ "no births",
  (v212 > 0 & v212 < 15) ~ "5-14",
  (v212 >= 15 & v212 < 20) ~ "15-19",
  (v212 >= 20 & v212 < 50) ~ "20+"))

# Recode abortion/miscarriage
ETIR<-ETIR %>% replace_na(list(v228= "no"))
ETIR$preg.term <- ifelse(ETIR$v228 == "yes",1,0)

# Recode ANC
ETIR$anc.1stvisit <- ifelse(ETIR$m13_1 == 98,5,ETIR$m13_1) # Impute DNK to the mean of 5
ETIR$anc.1stvisit[ETIR$m14_1 == 0] <-10 # if had no visits set to 10 (birth)
ETIR$anc.num <- ifelse(ETIR$m14_1 == 98,3,ETIR$m14_1) # Impute DNK to the mean of 3
ETIR$anc.4mo <- ifelse(ETIR$anc.1stvisit>=4,1,0)
ETIR$anc.4plus <-ifelse(ETIR$anc.num>=4,1,0)
ETIR$anc.month<-ifelse(ETIR$anc.1stvisit<4,"Month 1-3",
                        ifelse(ETIR$anc.1stvisit>3 & ETIR$anc.1stvisit<7,"Month 4-6",
                               ifelse(ETIR$anc.1stvisit>6 & ETIR$anc.1stvisit<10, "Month 7-9", "No ANC")))
ETIR$anc.month <- factor(ETIR$anc.month, c("No ANC","Month 1-3","Month 4-6", "Month 7-9"))
ETIR$anyanc<-ifelse(ETIR$m14_1 == 0,0,1)
ETIR$anc.1sttri<-ifelse(ETIR$anc.month=="Month 1-3", 1,0)

# Recode used health services
ETIR$hc.chw <- ifelse(ETIR$v393 == "yes",1,0)
ETIR$hc.hfac <- ifelse(ETIR$v394 == "yes",1,0)

# Wealth index
ETIR$wealth<-ETIR$v190

# Marital status
ETIR$marr.cohab<-ifelse(ETIR$v501 == "married" | ETIR$v501 == "living with partner",1,0)

# Recode home births
ETIR[,hb_1:= ifelse(m15_1== "home" | m15_1== "respondent's home" | m15_1== "other home" | m15_1== "other",1,0)]
ETIR[,hb_2:= ifelse(m15_2== "home" | m15_2== "respondent's home" | m15_2== "other home" | m15_2== "other",1,0)]
ETIR[,hb_3:= ifelse(m15_3== "home" | m15_3== "respondent's home" | m15_3== "other home" | m15_3== "other",1,0)]
ETIR[,hb_4:= ifelse(m15_4== "home" | m15_4== "respondent's home" | m15_4== "other home" | m15_4== "other",1,0)]
ETIR[,hb_5:= ifelse(m15_5== "home" | m15_5== "respondent's home" | m15_5== "other home" | m15_5== "other",1,0)]

ETIR$hb_ever[(ETIR$hb_1==1 | ETIR$hb_2==1 | ETIR$hb_3==1 | ETIR$hb_4==1 | ETIR$hb_5==1)]<- 1 
ETIR$hb_ever[((ETIR$hb_1==0 | is.na(ETIR$hb_1)) & (ETIR$hb_2==0 | is.na(ETIR$hb_2)) & (ETIR$hb_3==0 | is.na(ETIR$hb_3)) & 
                (ETIR$hb_4==0 | is.na(ETIR$hb_4)) & (ETIR$hb_5==0 | is.na(ETIR$hb_5)))]<-0
ETIR$hb_ever <- ifelse((is.na(ETIR$hb_1))&(is.na(ETIR$hb_2))&(is.na(ETIR$hb_3))&(is.na(ETIR$hb_4))&(is.na(ETIR$hb_5)),NA,ETIR$hb_ever)

ETIR[,table(hb_ever, useNA="always")]
ETIR[,table(hb_1, useNA="always")]

# DV - higher is worse
ETIR<- ETIR %>% mutate(dv.out = case_when(
  v744a =="no" ~0,
  v744a == "yes" ~1))
ETIR<- ETIR %>% mutate(dv.negkid = case_when(
  v744b =="no" ~0,
  v744b == "yes" ~1))
ETIR<- ETIR %>% mutate(dv.argue = case_when(
  v744c =="no" ~0,
  v744c == "yes" ~1))
ETIR<- ETIR %>% mutate(dv.nosex = case_when(
  v744d =="no" ~0,
  v744d == "yes" ~1))
ETIR<- ETIR %>% mutate(dv.burnfd = case_when(
  v744e =="no" ~0,
  v744e == "yes" ~1))

ETIR$dv.nacnt<-ETIR  %>% 
  rowwise() %>% summarise(dv.nacnt = sum(is.na(dv.out),is.na(dv.negkid),is.na(dv.argue),is.na(dv.nosex),is.na(dv.burnfd)))
ETIR<-ETIR %>% 
  rowwise() %>% mutate(dv.sum = sum(dv.out,dv.negkid,dv.argue,dv.nosex,dv.burnfd, na.rm=TRUE))
ETIR$dv.index<-ifelse(ETIR$dv.nacnt == 5, NA, ETIR$dv.sum)
ETIR$dv.index<-as.numeric(ETIR$dv.index)

# Barriers to HC
ETIR<- ETIR %>% mutate(med.dnk = case_when(
  v467a =="not a big problem" ~0,
  v467a == "big problem" ~1))
ETIR<- ETIR %>% mutate(med.permis = case_when(
  v467b =="not a big problem" ~0,
  v467b == "big problem" ~1))
ETIR<- ETIR %>% mutate(med.cost = case_when(
  v467c =="not a big problem" ~0,
  v467c == "big problem" ~1))
ETIR<- ETIR %>% mutate(med.dist = case_when(
  v467d =="not a big problem" ~0,
  v467d == "big problem" ~1))
ETIR<- ETIR %>% mutate(med.transp = case_when(
  v467e =="not a big problem" ~0,
  v467e == "big problem" ~1))
ETIR<- ETIR %>% mutate(med.alone = case_when(
  v467f =="not a big problem" ~0,
  v467f == "big problem" ~1))
ETIR<- ETIR %>% mutate(med.fempro = case_when(
  v467g =="not a big problem" ~0,
  v467g == "big problem" ~1))
ETIR<- ETIR %>% mutate(med.nopro = case_when(
  v467h =="not a big problem" ~0,
  v467h == "big problem" ~1))
ETIR<- ETIR %>% mutate(med.nomeds = case_when(
  v467i =="not a big problem" ~0,
  v467i == "big problem" ~1))

ETIR$med.nacnt<-ETIR  %>% 
  rowwise() %>% summarise(med.nacnt = sum(is.na(med.dnk),is.na(med.permis),is.na(med.cost),is.na(med.dist),is.na(med.transp),is.na(med.alone),
                                         is.na(med.fempro),is.na(med.nopro),is.na(med.nomeds)))
ETIR<-ETIR %>% 
  rowwise() %>% mutate(med.sum = sum(med.dnk,med.permis,med.cost,med.dist,med.transp,med.alone,med.fempro,med.nopro,med.nomeds, na.rm=TRUE))
ETIR$med.index<-ifelse(ETIR$med.nacnt == 9, NA, ETIR$med.sum)
ETIR$med.index<-as.numeric(ETIR$med.index)

# Joint decision making
ETIR$jd.hlth<- ifelse(ETIR$v743a== "respondent and husband/partner",1,0)
ETIR$jd.lrgpur<- ifelse(ETIR$v743b== "respondent and husband/partner",1,0)
ETIR$jd.hhpur<- ifelse(ETIR$v743c== "respondent and husband/partner",1,0)
ETIR$jd.visit<- ifelse(ETIR$v743d== "respondent and husband/partner",1,0)
ETIR$jd.food<- ifelse(ETIR$v743e== "respondent and husband/partner",1,0)
ETIR$jd.money<- ifelse(ETIR$v743f== "respondent and husband/partner",1,0)

ETIR$jd.nacnt<-ETIR  %>% 
  rowwise() %>% summarise(jd.nacnt = sum(is.na(jd.hlth),is.na(jd.lrgpur),is.na(jd.hhpur),is.na(jd.visit),is.na(jd.food),is.na(jd.money)))
ETIR<-ETIR %>% 
  rowwise() %>% mutate(jd.sum = sum(jd.hlth,jd.lrgpur,jd.hhpur,jd.visit,jd.food,jd.money, na.rm=TRUE))
ETIR$jd.index<-ifelse(ETIR$jd.nacnt == 6, NA, ETIR$jd.sum)
ETIR$jd.index<-as.numeric(ETIR$jd.index)

# Woman decision making
ETIR$wd.hlth<- ifelse(ETIR$v743a== "respondent alone",1,0)
ETIR$wd.lrgpur<- ifelse(ETIR$v743b== "respondent alone",1,0)
ETIR$wd.hhpur<- ifelse(ETIR$v743c== "respondent alone",1,0)
ETIR$wd.visit<- ifelse(ETIR$v743d== "respondent alone",1,0)
ETIR$wd.food<- ifelse(ETIR$v743e== "respondent alone",1,0)
ETIR$wd.money<- ifelse(ETIR$v743f== "respondent alone",1,0)

ETIR$wd.nacnt<-ETIR  %>% 
  rowwise() %>% summarise(wd.nacnt = sum(is.na(wd.hlth),is.na(wd.lrgpur),is.na(wd.hhpur),is.na(wd.visit),is.na(wd.food),is.na(wd.money)))
ETIR<-ETIR %>% 
  rowwise() %>% mutate(wd.sum = sum(wd.hlth,wd.lrgpur,wd.hhpur,wd.visit,wd.food,wd.money, na.rm=TRUE))
ETIR$wd.index<-ifelse(ETIR$wd.nacnt == 6, NA, ETIR$wd.sum)
ETIR$wd.index<-as.numeric(ETIR$wd.index)

# Women's empowerment index
SWPR<-read.dta("C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways\\rawdata\\Kenya_DHS\\KEIR72DT\\KEIR72FL.dta")
source("C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways\\Adolescents\\Code\\dhs-swper.R")
SWPR<-cbind(SWPR, computeSWPER(SWPR))

SWPR <- SWPR %>% select(c("caseid", "SWPER.SCORE.1", "SWPER.SCORE.2", "SWPER.SCORE.3", "SWPER.EMPOWERMENT.1", "SWPER.EMPOWERMENT.2", "SWPER.EMPOWERMENT.3"))

# Household variables
ETHH<-ETHH %>% mutate(hh.bank.acct = case_when(
  hv247=="no" | hv247 == "don't know" ~0,
  hv247 == "yes" ~1))
ETHH<-ETHH %>% mutate(hh.land = case_when(
  hv244=="no" ~0,
  hv244 == "yes" ~1))
ETHH<-ETHH %>% mutate(hh.animal = case_when(
  hv246=="no" ~0,
  hv246 == "yes" ~1))
ETHH<-ETHH %>% mutate(hh.urban = case_when(
  hv025=="rural" ~0,
  hv025== "urban" ~1))
ETHH<-ETHH %>% mutate(hh.phone = case_when(
  hv243a=="no" ~0,
  hv243a== "yes" ~1))
ETHH<-ETHH %>% mutate(hh.car = case_when(
  hv212=="no" ~0,
  hv212== "yes" ~1))
ETHH<-ETHH %>% mutate(hh.motor = case_when(
  hv211=="no" ~0,
  hv211== "yes" ~1))
ETHH$hh.index<-ETHH$hh.car+ETHH$hh.motor+ETHH$hh.phone+ETHH$hh.bank.acct

ETHH$hh.woman15ov<-ETHH$hv010
ETHH$hh.kidwom.rat<-ifelse(ETHH$hv010 == 0, 0, (ETHH$hv014/ETHH$hv010))

ETHH$hh.watpur<-ETHH$hv237

# Urban slum - UN definition 
ETHH[,floor:= ifelse(hv213 == "earth, sand" | hv213== "dung" | hv213== "wood planks" | hv213== "palm, bamboo",1,0)]
ETHH[,wall:= ifelse(hv214 == "no walls" | hv214== "cane / palm / trunks" | hv214== "dung / mud / sod" | hv214== "grass" |
                      hv214 == "bamboo with mud" | hv214 == "stone with mud" | hv214 == "uncovered adobe" |
                      hv214 == "cardboard" | hv214 == "reused wood" | hv214 == "iron sheets",1,0)]
ETHH[,water:= ifelse(hv201 == "piped water" | hv201== "piped into dwelling" | hv201== "piped to yard/plot" | hv201== "piped to neighbor" | hv201== "public tap/standpipe" |
                     hv201 == "tube well water" | hv201 == "tube well or borehole" | hv201 == "protected well" | hv201== "tanker truck" |
                       hv201== "cart with small tank" | hv201== "bottled water", 0,1)]
ETHH[,latrine:= ifelse(hv205 == "pit toilet latrine" | hv205== "ventilated improved pit latrine (vip)" | hv205== "pit latrine without slab/open pit" | hv205== "no facility/bush/field" |
                       hv205 == "composting toilet" | hv205 == "bucket toilet" | hv205 == "hanging toilet/latrine" | hv205 == "other", 1,0)]
ETHH$hv012<-ifelse(ETHH$hv012 == 0, ETHH$hv013, ETHH$hv012)
ETHH$memsleep <- ifelse(ETHH$hv216 == 0,ETHH$hv012,ETHH$hv012/ETHH$hv216)
ETHH[,living:= ifelse(ETHH$memsleep > 3,1,0)]

ETHH<-ETHH %>% 
  rowwise() %>% mutate(slum.sum = sum(floor,water,latrine,living, na.rm=T))
ETHH<-ETHH %>% mutate(slum1 = case_when(
  hv025=="rural" ~ "rural",
  (hv025 == "urban" & slum.sum >= 2) ~ "urban slum",
  (hv025 == "urban" & slum.sum < 2) ~ "urban non-slum"))

ETHH$v001<-ETHH$hv001
ETHH$v002<-ETHH$hv002
ETIR$wt<-ETIR$v005/1000000

# Urban slum 2 - Zulu et al, 2002
ETHH$elec<- ifelse(ETHH$hv206 == "no",1,0)
ETHH$piped<- ifelse(ETHH$hv201 == "piped water" | ETHH$hv201== "piped into dwelling" | ETHH$hv201== "piped to yard/plot" | ETHH$hv201== "public tap/standpipe", 0,1)
ETHH$slum2 <- ifelse(ETHH$hh.urban == 1 & ETHH$elec == 1 & ETHH$latrine == 1 & ETHH$piped == 1, 1, 0)

#Create segmentation dataset
ETIR.seg <- ETIR %>% select(c("v001","v002","v005","v021","v023", "wt", "caseid","freq.tv", "ed.grp.seg",
                              "urban","husb.ed.grp.seg","muslim","head.sex","freq.rad", "no.partner",
                              "rad.yn", "tv.yn","fp.mod.ever", "fp.mod.now", "num.bio.child", "hb_1", "hb_ever",
                              "ed.yrs","ed.grp", "ed.yrs.lit", "num.hhmem", "num.child.die", "woman.age",
                              "woman.agegrp", "age.1stsex", "age.1stsex.cat", "age.1stcohab", "age.1stcohab.cat",
                              "age.1stbrth", "age.1stbrth.cat", "preg.term", "anc.1stvisit", "anc.num", "anc.4mo",
                              "anc.4plus","anc.month", "anyanc", "anc.1sttri", "hc.chw", "hc.hfac", "wealth",
                              "marr.cohab", 'hb_1', "hb_ever", "dv.index", "med.permis","med.cost","med.dist",
                              "med.alone", "med.index", "jd.index", "wd.index"))

ETHH.seg <- ETHH %>% select(c("v001", "v002", "hh.bank.acct","hh.phone","water", "latrine",
                              "hh.index","hh.car","hh.motor","hh.watpur", "hh.woman15ov", "hh.kidwom.rat",
                              "slum.sum", "slum1", "slum2"))

######################## Merge IR and HH
eth.seg<-merge(ETIR.seg, ETHH.seg, by=c("v001", "v002"), all.x=T)
eth.seg<-merge(eth.seg, SWPR, by="caseid", all.x=T)

########## Impute missings
#gg_miss_var(kenya, show_pct = TRUE)

################################################
## Kenyan Urban Typing Tool Logic (each end point gets one line of code)
################################################

####################################################
### Urban Kenyan Male Headed Households UM1 and UM5
####################################################

# UM1 ##############################
eth.seg$seg1[(eth.seg$urban == "urban" &
                     eth.seg$head.sex == "male" & 
                     eth.seg$ed.grp.seg == "no education")] <- "UM1"
# UM5 ##############################
eth.seg$seg1[(eth.seg$urban == "urban" & 
                     eth.seg$head.sex == "male" & 
                      eth.seg$ed.grp.seg %in% c("complete primary", "incomplete secondary", "complete secondary", "higher"))] <- "UM5"
# UM1 ##############################
eth.seg$seg1[(eth.seg$urban == "urban" 
                   & eth.seg$head.sex == "male" & 
                      eth.seg$ed.grp.seg %in% c("incomplete primary") &
                      eth.seg$num.bio.child < 4 &
                      eth.seg$freq.tv == 0)] <- "UM1"
# UM5 ##############################
eth.seg$seg1[(eth.seg$urban == "urban" 
                   & eth.seg$head.sex == "male" & 
                      eth.seg$ed.grp.seg %in% c("incomplete primary") &
                      eth.seg$num.bio.child < 4 &
                      eth.seg$freq.tv > 0)] <- "UM5"
# UM1 ##############################
eth.seg$seg1[(eth.seg$urban == "urban" &
                   eth.seg$head.sex == "male" & 
                      eth.seg$ed.grp.seg %in% c("incomplete primary") &
                      eth.seg$num.bio.child > 3 &
                      eth.seg$freq.rad == 0)] <- "UM1"
# UM5 ##############################
eth.seg$seg1[(eth.seg$urban == "urban" & 
                     eth.seg$head.sex == "male" & 
                      eth.seg$ed.grp.seg %in% c("incomplete primary") &
                      eth.seg$num.bio.child > 3 &
                      eth.seg$freq.rad > 0)] <- "UM5"

#####################################################
### Urban Kenyan Female Headed Households UF1 and UF3
#####################################################

# UF1 ##############################
eth.seg$seg1[(eth.seg$urban == "urban" & 
                     eth.seg$head.sex == "female" & 
                      eth.seg$hh.index < 2 &
                      eth.seg$num.bio.child < 4 &
                      eth.seg$freq.rad == 0)] <- "UF1"
# UF3 ##############################
eth.seg$seg1[(eth.seg$urban == "urban" & 
                     eth.seg$head.sex == "female" & 
                      eth.seg$hh.index < 2 &
                      eth.seg$num.bio.child < 4 &
                      eth.seg$freq.rad != 0)] <- "UF3"
# UF1 ##############################
eth.seg$seg1[(eth.seg$urban == "urban" & 
                     eth.seg$head.sex == "female" & 
                      eth.seg$hh.index < 2 &
                      eth.seg$num.bio.child > 3)] <- "UF1"
# UF3 ##############################
eth.seg$seg1[(eth.seg$urban == "urban" & 
                     eth.seg$head.sex == "female" & 
                      eth.seg$hh.index > 1 &
                      eth.seg$num.bio.child < 4)] <- "UF3"
# UF3 ##############################
eth.seg$seg1[(eth.seg$urban == "urban" & 
                     eth.seg$head.sex == "female" & 
                      eth.seg$hh.index > 1 &
                      eth.seg$num.bio.child < 4)] <- "UF3"
# UF1 ##############################
eth.seg$seg1[(eth.seg$urban == "urban" & 
                     eth.seg$head.sex == "female" & 
                      eth.seg$hh.index > 1 &
                      eth.seg$num.bio.child > 3 &
                      eth.seg$freq.rad == 0)] <- "UF1"
# UF3 ##############################
eth.seg$seg1[(eth.seg$urban == "urban" & 
                     eth.seg$head.sex == "female" & 
                      eth.seg$hh.index > 1 &
                      eth.seg$num.bio.child > 3 &
                      eth.seg$freq.rad > 0)] <- "UF3"

####################################################
### Rural Kenyan Male Headed Households
####################################################

# RM2 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "male" & 
                     eth.seg$ed.grp.seg %in% c("no education") &
                     eth.seg$muslim == 0 &
                     eth.seg$husb.ed.grp.seg < 3)] <- "RM2"
# RM1 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "male" & 
                     eth.seg$ed.grp.seg %in% c("no education") &
                     eth.seg$muslim == 0 &
                     eth.seg$husb.ed.grp.seg > 2)] <- "RM1"
# RM2 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "male" & 
                     eth.seg$ed.grp.seg %in% c("no education") &
                     eth.seg$muslim == 1)] <- "RM2"
# RM3 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "male" & 
                     eth.seg$ed.grp.seg %in% c("incomplete primary", "complete primary", "incomplete secondary", "complete secondary", "higher") &
                     eth.seg$hh.watpur == "yes" &
                     eth.seg$latrine == 1)] <- "RM3"
# RM1 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "male" & 
                     eth.seg$ed.grp.seg %in% c("incomplete primary", "complete primary", "incomplete secondary", "complete secondary", "higher") &
                     eth.seg$hh.watpur == "yes" &
                     eth.seg$latrine == 0)] <- "RM1"
# RM3 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "male" & 
                     eth.seg$ed.grp.seg %in% c("incomplete secondary", "complete secondary", "higher") &
                     eth.seg$hh.watpur %in% c("no", "don't know"))] <- "RM3"
# RM3 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "male" & 
                     eth.seg$ed.grp.seg %in% c("incomplete primary", "complete primary") &
                     eth.seg$hh.watpur %in% c("no", "don't know") &
                     eth.seg$freq.rad == 2)] <- "RM3"
# RM3 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "male" & 
                     eth.seg$ed.grp.seg %in% c("incomplete primary", "complete primary") &
                     eth.seg$hh.watpur %in% c("no", "don't know") &
                     eth.seg$freq.rad < 2 &
                     eth.seg$husb.ed.grp.seg > 3)] <- "RM3"
# RM1 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "male" & 
                     eth.seg$ed.grp.seg %in% c("incomplete primary", "complete primary") &
                     eth.seg$hh.watpur %in% c("no", "don't know") &
                     eth.seg$freq.rad < 2 &
                     eth.seg$husb.ed.grp.seg < 4)] <- "RM1"

####################################################
### Rural Kenyan Female Headed Households
####################################################

# RF1 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "female" & 
                     eth.seg$ed.grp.seg %in% c("no education"))] <- "RF1"
# RF4 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "female" & 
                     eth.seg$ed.grp.seg %in% c("complete primary", "incomplete secondary", "complete secondary", "higher"))] <- "RF4"
# RF1 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "female" & 
                     eth.seg$ed.grp.seg %in% c("incomplete primary") &
                     eth.seg$hh.index == 0)] <- "RF1"
# RF4 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "female" & 
                     eth.seg$ed.grp.seg %in% c("incomplete primary") &
                     eth.seg$hh.index > 0 &
                     eth.seg$fp.mod.ever == 1)] <- "RF4"
# RF4 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "female" & 
                     eth.seg$ed.grp.seg %in% c("incomplete primary") &
                     eth.seg$hh.index > 0 &
                     eth.seg$fp.mod.ever == 0 &
                     eth.seg$hh.watpur == "yes")] <- "RF4"
# RF1 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "female" & 
                     eth.seg$ed.grp.seg %in% c("incomplete primary") &
                     eth.seg$hh.index > 0 &
                     eth.seg$fp.mod.ever == 0 &
                     eth.seg$hh.watpur %in% c("no", "don't know") &
                     eth.seg$num.bio.child > 3)] <- "RF1"
# RF4 ##############################
eth.seg$seg1[(eth.seg$urban == "rural" &
                     eth.seg$head.sex == "female" & 
                     eth.seg$ed.grp.seg %in% c("incomplete primary") &
                     eth.seg$hh.index > 0 &
                     eth.seg$fp.mod.ever == 0 &
                     eth.seg$hh.watpur %in% c("no", "don't know") &
                     eth.seg$num.bio.child < 4)] <- "RF4"

eth.seg<-merge(eth.seg, new.seg, by.x="seg1", by.y="seg1",all.x=T)


# Set survey design matrix
DHSdes<-svydesign(id=~v021, strata=~v023, weights=~wt, data=eth.seg)

# Pie chart of segs
pop.seg<-svytotal(~new.seg, DHSdes)
pop.seg<-as.data.frame(pop.seg)
pop.seg$seg<-c("RF2","RF4","RM2","RM3","RM4","UF2","UF4","UM1","UM4")

# Compute percentages
pop.seg$fraction <- pop.seg$total / sum(pop.seg$total)
pop.seg$fraction <- round(pop.seg$fraction*100,1)

# Compute the cumulative percentages (top of each rectangle)
pop.seg$ymax <- cumsum(pop.seg$fraction)

# Compute the bottom of each rectangle
pop.seg$ymin <- c(0, head(pop.seg$ymax, n=-1))

# Compute label position
pop.seg$labelPosition <- (pop.seg$ymax + pop.seg$ymin) / 2

# Compute a good label
pop.seg$label <- paste0(pop.seg$seg, "\n", pop.seg$fraction, "%")

# Make the plot
ggplot(pop.seg, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=seg)) +
  geom_rect() +
  geom_label(x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

######################## plotting 
tot <- melt(svytable(~new.seg, DHSdes))
hb <- melt(svytable(~hb_ever, DHSdes))

tot <-data.table(tot) 
setkey(tot,value)

####### 
hbseg <- melt(svytable(~new.seg+hb_ever, DHSdes))
perc <- merge(hbseg,tot,by="new.seg")
perc$perct <- 100*perc$value.x/perc$value.y
#perc <- perc[perc$value.y>=50,]

theme_set(theme_bw(30))
ggplot(aes(x=new.seg),data=perc)+  geom_col(aes(y=perct, fill=as.factor(hb_ever))) + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Segments")+ ylab("% of Women in each Segment") +ggtitle("") + 
  guides(fill=guide_legend(title="Home Birth"))+ scale_fill_manual(values=c("grey", "darkcyan"), labels=c("No", "Yes"))

## Ladder plot home birth
svytotal(~new.seg, DHSdes)
svyciprop(~hb_ever, DHSdes, na.rm=T)

hb<-svyby(~hb_ever, ~new.seg, DHSdes, svymean,ci=TRUE,vartype="ci", na.rm=T); as.data.frame(hb)
hb$hb_ever<-hb$hb_ever*100
hb$ci_l<-hb$ci_l*100
hb$ci_u<-hb$ci_u*100

ggplot(hb) + 
  geom_col(aes(x=reorder(new.seg, desc(new.seg)), y=hb_ever), alpha=0.9, size=5, fill="darkcyan", show.legend = FALSE) +
  geom_errorbar(aes(x=reorder(new.seg, desc(new.seg)), y=hb_ever, ymin=ci_l, ymax=ci_u), size=1, width=.5, show.legend = FALSE) +
  geom_text(aes(y=hb_ever, x=new.seg, label=round(hb_ever,1)), vjust=-1, hjust=.5)+
  coord_flip()+
  xlab("") +
  scale_y_continuous(name = "Percent of Births at Home", breaks=seq(0, 100, 10)) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

#Ladder plot 2
svy.glm<-svyglm(hb_ever~new.seg,family=quasibinomial,design=DHSdes, data=eth.seg)

pred.data<-data.frame("new.seg"=new.seg$new.seg)
pred.data3<-data.frame(type=c("RF" ,"RF", "RM" ,"RM" ,"RM" , "UF" ,"UF", "UM" ,"UM"))

# Plot extended segments
probabilities <- predict(svy.glm, pred.data, type = "response", se.fit=T)
prob1<-as.data.frame(probabilities)
prob1$lower<- prob1$response - (1.96*prob1$SE)
prob1$upper<- prob1$response + (1.96*prob1$SE)
prob1$segX4<- pred.data$new.seg
prob1$type<-pred.data3$type

cbPalette <- c("#A6611A", "#DFC27D", "#80CDC1", "#018571")
ggplot(prob1, aes(x=reorder(segX4, desc(segX4)), y=response)) + 
  geom_point(aes(colour=type), alpha=0.9, size=5, show.legend = FALSE) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=type), size=1, width=.5, show.legend = FALSE) +
  geom_text(aes(y=response, x=segX4, label=round(response,2)), vjust=-1.2, hjust=.5) +
  coord_flip()+
  xlab("") +
  ylab("Probability of Home Birth") +
  scale_colour_manual(values=cbPalette) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

## Ladder plot FP
svytotal(~new.seg, DHSdes)
svyciprop(~fp.mod.ever, DHSdes, na.rm=T)

hb<-svyby(~fp.mod.ever, ~new.seg, DHSdes, svymean,ci=TRUE,vartype="ci", na.rm=T); as.data.frame(hb)
hb$fp.mod.ever<-hb$fp.mod.ever*100
hb$ci_l<-hb$ci_l*100
hb$ci_u<-hb$ci_u*100

ggplot(hb) + 
  geom_col(aes(x=reorder(new.seg, desc(new.seg)), y=fp.mod.ever), alpha=0.9, size=5, fill="darkcyan", show.legend = FALSE) +
  geom_errorbar(aes(x=reorder(new.seg, desc(new.seg)), y=fp.mod.ever, ymin=ci_l, ymax=ci_u), size=1, width=.5, show.legend = FALSE) +
  geom_text(aes(y=fp.mod.ever, x=new.seg, label=round(fp.mod.ever,1)), vjust=-1, hjust=.5)+
  coord_flip()+
  xlab("") +
  scale_y_continuous(name = "Percent Ever Use of Modern Contraception", breaks=seq(0, 100, 10)) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))
