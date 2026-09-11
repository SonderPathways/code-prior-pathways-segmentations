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


############## Load Kenya DHS data
#KEBR <- data.table(read.dta("C:/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways/rawdata/Kenya_DHS/KEBR72DT/KEBR72FL.dta")) 
KEHH <- data.table(read.dta("C:/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways/rawdata/Kenya_DHS/KEHR72DT/KEHR72FL.dta")) 
#KEPR <- data.table(read.dta("C:/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways/rawdata/Kenya_DHS/KEPR72DT/KEPR72FL.dta")) 
#KEMR <- data.table(read.dta("C:/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways/rawdata/Kenya_DHS/KEMR72DT/KEMR72FL.dta")) 
KEIR <- data.table(read.dta("C:/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways/rawdata/Kenya_DHS/KEIR72DT/KEIR72FL.dta")) 

############## Read pathways Rdat file contianing segment designation for DHS caseids
path <- load("C:/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways - Gender Equality Division/Modeling/Data/DHS Pathways interim segments & factors.RDat")
path2 <- load("C:/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways - Gender Equality Division/Modeling/Data/Broad Cut KE.Rdata")

dhs.seg <- data.table(dhs.seg)
unique(dhs.seg$cv.regX5)

# Only keep data for Kenya DHS crosswaling
dhs.seg <- dhs.seg[cv.regX5=="Kenya",]

#The household segment ID is column “segX4”
#The country / state indicator is column “cv.regX5”
#The sample weight is column "cmb.wt"

dhs.seg$caseid <- substr(as.character(dhs.seg$IDHSPID),9,24)

# This is the larger crosswalk for Kenya
broad.cut <- data.table(broad.cut)
broad.cut$caseid <- substr(as.character(broad.cut$IDHSPID),9,24)
broad.cut$segX4<-broad.cut$seg.drv

KEIRcut<-KEIR[,1:3215]

#test<-KEIR[caseid %in% c("     0102053 02", "     0001006 02", "     0010070 02")]
#t_test <- transpose(test); t_test$var<-colnames(test)

dhs.seg[,table(dlvhm_01, anc.visit.lbrth, useNA="always")]

######################## Home Births

# Recode home births
KEIRcut[,hb_1:= ifelse(m15_1== "home" | m15_1== "respondent's home" | m15_1== "other home" | m15_1== "other",1,0)]
KEIRcut[,hb_2:= ifelse(m15_2== "home" | m15_2== "respondent's home" | m15_2== "other home" | m15_2== "other",1,0)]
KEIRcut[,hb_3:= ifelse(m15_3== "home" | m15_3== "respondent's home" | m15_3== "other home" | m15_3== "other",1,0)]
KEIRcut[,hb_4:= ifelse(m15_4== "home" | m15_4== "respondent's home" | m15_4== "other home" | m15_4== "other",1,0)]
KEIRcut[,hb_5:= ifelse(m15_5== "home" | m15_5== "respondent's home" | m15_5== "other home" | m15_5== "other",1,0)]

KEIRcut$hb_ever[(KEIRcut$hb_1==1 | KEIRcut$hb_2==1 | KEIRcut$hb_3==1 | KEIRcut$hb_4==1 | KEIRcut$hb_5==1)]<- 1 
KEIRcut$hb_ever[((KEIRcut$hb_1==0 | is.na(KEIRcut$hb_1)) & (KEIRcut$hb_2==0 | is.na(KEIRcut$hb_2)) & (KEIRcut$hb_3==0 | is.na(KEIRcut$hb_3)) & 
                (KEIRcut$hb_4==0 | is.na(KEIRcut$hb_4)) & (KEIRcut$hb_5==0 | is.na(KEIRcut$hb_5)))]<-0
KEIRcut$hb_ever <- ifelse((is.na(KEIRcut$hb_1))&(is.na(KEIRcut$hb_2))&(is.na(KEIRcut$hb_3))&(is.na(KEIRcut$hb_4))&(is.na(KEIRcut$hb_5)),NA,KEIRcut$hb_ever)

KEIRcut[,table(hb_ever, useNA="always")]
KEIRcut[,table(hb_1, useNA="always")]

# Recode vehicle use
KEIRcut[,own.vehic:=ifelse(v124== "yes" | v125=="yes",1,0)]
KEIRcut$own.vehic <- ifelse((is.na(KEIRcut$v124) & (is.na(KEIRcut$v125))| (KEIRcut$v124=="not a dejure resident") & (KEIRcut$v125=="not a dejure resident")),NA, KEIRcut$own.vehic)

# Recode radio and TV exposure
KEIRcut<-KEIRcut %>% mutate(freq.radtv = case_when(
                (v158== "not at all" | is.na(v158)) & (v159=="not at all" | is.na(v159)) ~ 0,
                v158=="at least once a week" | v159 == "at least once a week" ~2,
                v158=="less than once a week" | v159 == "less than once a week" ~1))

KEIRcut$radtv.yn<-ifelse((KEIRcut$v158== "not at all" | is.na(KEIRcut$v158)) & (KEIRcut$v159=="not at all" | is.na(KEIRcut$v159)),0,1)

KEIRcut[,table(v133, useNA="always")]

# Recode education
KEIRcut$ed.yrs<-KEIRcut$v133
KEIRcut$ed.grp<-KEIRcut$v106

KEIRcut<-KEIRcut %>% replace_na(list(v155= "blind/visually impaired"))
KEIRcut$ed.lit <- ifelse(KEIRcut$v155 == "cannot read at all",0, KEIRcut$v133)

KEIRcut[,husb.ed.grp:=v701]
KEIRcut$husb.ed.grp<-ifelse((KEIRcut$v701 == "don't know" & (KEIRcut$v705 == "professional/technical/managerial"  | KEIRcut$v705 == "clerical")),4,KEIRcut$husb.ed.grp)
KEIRcut$husb.ed.grp<-ifelse(KEIRcut$husb.ed.grp == 5,2,KEIRcut$husb.ed.grp) # Impute DNK to the mean of 5

# Recode age
KEIRcut[,woman.age:=v012]
KEIRcut[,woman.agegrp:=v013]

# Recode HH size
KEIRcut[,num.hhmem:=v136]
KEIRcut[,num.child:=v137]
KEIRcut[,num.child.ever:=v201]
KEIRcut$num.child.die <- KEIRcut$v206 + KEIRcut$v207

# Recode religion
KEIRcut<-KEIRcut %>% replace_na(list(v130= "no religion"))
KEIRcut$muslim<-ifelse(KEIRcut$v130 == "muslim",1,0)

# Recode age at first sex, marriage, birth
KEIRcut$age.1stsex <- ifelse(KEIRcut$v531 == 97,17, 
                             ifelse(KEIRcut$v531 == 98, 17, 
                                    ifelse(KEIRcut$v531 == 0, NA, KEIRcut$v531)))
KEIRcut<- KEIRcut %>% mutate(age.1stsex.cat = case_when(
  is.na(age.1stsex) ~ "never",
  (age.1stsex > 0 & age.1stsex < 15) ~ "5-14",
  (age.1stsex >= age.1stsex & age.1stsex < 20) ~ "15-19",
  (age.1stsex >= 20 & age.1stsex < 50) ~ "20+"))
  
KEIRcut$age.1stcohab<-KEIRcut$v511
KEIRcut<- KEIRcut %>% mutate(age.1stcohab.cat = case_when(
  is.na(v511) ~ "never",
  (v511 > 0 & v511 < 15) ~ "5-14",
  (v511 >= 15 & v511 < 20) ~ "15-19",
  (v511 >= 20) ~ "20+"))

KEIRcut$age.1stbrth<-KEIRcut$v212
KEIRcut<- KEIRcut %>% mutate(age.1stbrth.cat = case_when(
  is.na(v212) ~ "no births",
  (v212 > 0 & v212 < 15) ~ "5-14",
  (v212 >= 15 & v212 < 20) ~ "15-19",
  (v212 >= 20 & v212 < 50) ~ "20+"))

# Recode abortion/miscarriage
KEIRcut<-KEIRcut %>% replace_na(list(v228= "no"))
KEIRcut$preg.term <- ifelse(KEIRcut$v228 == "yes",1,0)

# Recode ANC
KEIRcut$anc.1stvisit <- ifelse(KEIRcut$m13_1 == 98,5,KEIRcut$m13_1) # Impute DNK to the mean of 5
KEIRcut$anc.1stvisit[KEIRcut$m14_1 == 0 & is.na(KEIRcut$m13_1)] <-10 # set to 10 (birth)
KEIRcut$anc.num <- ifelse(KEIRcut$m14_1 == 98,4,KEIRcut$m14_1) # Impute DNK to the mean of 4
KEIRcut$anc.num[is.na(KEIRcut$m14_1) & KEIRcut$m13_1 > 0] <- 4 # set to 4 
KEIRcut$anc.4mo <- ifelse(KEIRcut$anc.1stvisit>=4,1,0)

# Recode FP
KEIRcut$fp.mod.now <- ifelse(KEIRcut$v313 == "modern method",1,0)
#KEIRcut$fp.mod.ever <- ifelse(KEIRcut$v302 == "used modern method",1,0)

# Recode used health services
KEIRcut$hc.chw <- ifelse(KEIRcut$v393 == "yes",1,0)
KEIRcut$hc.hfac <- ifelse(KEIRcut$v394 == "yes",1,0)

# Wealth index
KEIRcut$wealth<-KEIRcut$v190

# Marital status
KEIRcut$marr.cohab<-ifelse(KEIRcut$v501 == "married" | KEIRcut$v501 == "living with partner",1,0)

# Woman as HH head
KEIRcut$woman.hoh<-ifelse(KEIRcut$v150 == "head",1,0)

# DV
KEIRcut<- KEIRcut %>% mutate(dv.out = case_when(
  v744a =="no" ~0,
  v744a == "yes" ~1))
KEIRcut<- KEIRcut %>% mutate(dv.negkid = case_when(
  v744b =="no" ~0,
  v744b == "yes" ~1))
KEIRcut<- KEIRcut %>% mutate(dv.argue = case_when(
  v744c =="no" ~0,
  v744c == "yes" ~1))
KEIRcut<- KEIRcut %>% mutate(dv.nosex = case_when(
  v744d =="no" ~0,
  v744d == "yes" ~1))
KEIRcut<- KEIRcut %>% mutate(dv.burnfd = case_when(
  v744e =="no" ~0,
  v744e == "yes" ~1))

KEIRcut$dv.nacnt<-KEIRcut  %>% 
  rowwise() %>% summarise(dv.nacnt = sum(is.na(dv.out),is.na(dv.negkid),is.na(dv.argue),is.na(dv.nosex),is.na(dv.burnfd)))
KEIRcut<-KEIRcut %>% 
  rowwise() %>% mutate(dv.sum = sum(dv.out,dv.negkid,dv.argue,dv.nosex,dv.burnfd, na.rm=TRUE))
KEIRcut$dv.index<-ifelse(KEIRcut$dv.nacnt == 5, NA, KEIRcut$dv.sum)
KEIRcut$dv.index<-as.numeric(KEIRcut$dv.index)

# Barriers to HC
KEIRcut<- KEIRcut %>% mutate(med.dnk = case_when(
  v467a =="not a big problem" ~0,
  v467a == "big problem" ~1))
KEIRcut<- KEIRcut %>% mutate(med.permis = case_when(
  v467b =="not a big problem" ~0,
  v467b == "big problem" ~1))
KEIRcut<- KEIRcut %>% mutate(med.cost = case_when(
  v467c =="not a big problem" ~0,
  v467c == "big problem" ~1))
KEIRcut<- KEIRcut %>% mutate(med.dist = case_when(
  v467d =="not a big problem" ~0,
  v467d == "big problem" ~1))
KEIRcut<- KEIRcut %>% mutate(med.transp = case_when(
  v467e =="not a big problem" ~0,
  v467e == "big problem" ~1))
KEIRcut<- KEIRcut %>% mutate(med.alone = case_when(
  v467f =="not a big problem" ~0,
  v467f == "big problem" ~1))
KEIRcut<- KEIRcut %>% mutate(med.fempro = case_when(
  v467g =="not a big problem" ~0,
  v467g == "big problem" ~1))
KEIRcut<- KEIRcut %>% mutate(med.nopro = case_when(
  v467h =="not a big problem" ~0,
  v467h == "big problem" ~1))
KEIRcut<- KEIRcut %>% mutate(med.nomeds = case_when(
  v467i =="not a big problem" ~0,
  v467i == "big problem" ~1))

KEIRcut$med.nacnt<-KEIRcut  %>% 
  rowwise() %>% summarise(med.nacnt = sum(is.na(med.dnk),is.na(med.permis),is.na(med.cost),is.na(med.dist),is.na(med.transp),is.na(med.alone),
                                         is.na(med.fempro),is.na(med.nopro),is.na(med.nomeds)))
KEIRcut<-KEIRcut %>% 
  rowwise() %>% mutate(med.sum = sum(med.dnk,med.permis,med.cost,med.dist,med.transp,med.alone,med.fempro,med.nopro,med.nomeds, na.rm=TRUE))
KEIRcut$med.index<-ifelse(KEIRcut$med.nacnt == 9, NA, KEIRcut$med.sum)
KEIRcut$med.index<-as.numeric(KEIRcut$med.index)

# Joint decision making
KEIRcut$jd.hlth<- ifelse(KEIRcut$v743a== "respondent and husband/partner",1,0)
KEIRcut$jd.lrgpur<- ifelse(KEIRcut$v743b== "respondent and husband/partner",1,0)
KEIRcut$jd.hhpur<- ifelse(KEIRcut$v743c== "respondent and husband/partner",1,0)
KEIRcut$jd.visit<- ifelse(KEIRcut$v743d== "respondent and husband/partner",1,0)
KEIRcut$jd.food<- ifelse(KEIRcut$v743e== "respondent and husband/partner",1,0)
KEIRcut$jd.money<- ifelse(KEIRcut$v743f== "respondent and husband/partner",1,0)

KEIRcut$jd.nacnt<-KEIRcut  %>% 
  rowwise() %>% summarise(jd.nacnt = sum(is.na(jd.hlth),is.na(jd.lrgpur),is.na(jd.hhpur),is.na(jd.visit),is.na(jd.food),is.na(jd.money)))
KEIRcut<-KEIRcut %>% 
  rowwise() %>% mutate(jd.sum = sum(jd.hlth,jd.lrgpur,jd.hhpur,jd.visit,jd.food,jd.money, na.rm=TRUE))
KEIRcut$jd.index<-ifelse(KEIRcut$jd.nacnt == 6, NA, KEIRcut$jd.sum)
KEIRcut$jd.index<-as.numeric(KEIRcut$jd.index)

# Woman decision making
KEIRcut$wd.hlth<- ifelse(KEIRcut$v743a== "respondent alone",1,0)
KEIRcut$wd.lrgpur<- ifelse(KEIRcut$v743b== "respondent alone",1,0)
KEIRcut$wd.hhpur<- ifelse(KEIRcut$v743c== "respondent alone",1,0)
KEIRcut$wd.visit<- ifelse(KEIRcut$v743d== "respondent alone",1,0)
KEIRcut$wd.food<- ifelse(KEIRcut$v743e== "respondent alone",1,0)
KEIRcut$wd.money<- ifelse(KEIRcut$v743f== "respondent alone",1,0)

KEIRcut$wd.nacnt<-KEIRcut  %>% 
  rowwise() %>% summarise(wd.nacnt = sum(is.na(wd.hlth),is.na(wd.lrgpur),is.na(wd.hhpur),is.na(wd.visit),is.na(wd.food),is.na(wd.money)))
KEIRcut<-KEIRcut %>% 
  rowwise() %>% mutate(wd.sum = sum(wd.hlth,wd.lrgpur,wd.hhpur,wd.visit,wd.food,wd.money, na.rm=TRUE))
KEIRcut$wd.index<-ifelse(KEIRcut$wd.nacnt == 6, NA, KEIRcut$wd.sum)
KEIRcut$wd.index<-as.numeric(KEIRcut$wd.index)

# Women's empowerment index
# Imports Kenya IR code again, uses the dhs-swper R code and computes the SWPR codes, then creates a new dataset of the three
# indices - Score.1 = 
SWPR<-read.dta("C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways\\rawdata\\Kenya_DHS\\KEIR72DT\\KEIR72FL.dta")
source("C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways\\Adolescents\\Code\\dhs-swper.R")
SWPR<-cbind(SWPR, computeSWPER(SWPR))

SWPR <- SWPR %>% select(c("caseid", "SWPER.SCORE.1", "SWPER.SCORE.2", "SWPER.SCORE.3", "SWPER.EMPOWERMENT.1", "SWPER.EMPOWERMENT.2", "SWPER.EMPOWERMENT.3"))

# Household variables
KEHH<-KEHH %>% mutate(hh.bank.acct = case_when(
  hv247=="no" | hv247 == "don't know" ~0,
  hv247 == "yes" ~1))
KEHH<-KEHH %>% mutate(hh.land = case_when(
  hv244=="no" ~0,
  hv244 == "yes" ~1))
KEHH<-KEHH %>% mutate(hh.animal = case_when(
  hv246=="no" ~0,
  hv246 == "yes" ~1))
KEHH<-KEHH %>% mutate(hh.urban = case_when(
  hv025=="rural" ~0,
  hv025== "urban" ~1))
KEHH<-KEHH %>% mutate(hh.phone = case_when(
  hv243a=="no" ~0,
  hv243a== "yes" ~1))

KEHH$head.ed.grp<-KEHH$hv106_01
KEHH$head.ed.yrs <- ifelse((KEHH$hv108_01 == 98 & KEHH$hv106_01 == "don't know"),7,
                           ifelse(((KEHH$hv108_01 == 98 | is.na(KEHH$hv108_01)) & KEHH$hv106_01 == "primary"),7,
                                  ifelse((KEHH$hv108_01 == 98 & KEHH$hv106_01 == "higher"),14,KEHH$hv108_01)))

KEHH$hh.woman15ov<-KEHH$hv010
KEHH$hh.kidwom.rat<-ifelse(KEHH$hv010 == 0, 0, (KEHH$hv014/KEHH$hv010))

# Urban slum - UN definition 
KEHH[,floor:= ifelse(hv213 == "earth, sand" | hv213== "dung" | hv213== "wood planks" | hv213== "palm, bamboo",1,0)]
KEHH[,wall:= ifelse(hv214 == "no walls" | hv214== "cane / palm / trunks" | hv214== "dung / mud / sod" | hv214== "grass" |
                      hv214 == "bamboo with mud" | hv214 == "stone with mud" | hv214 == "uncovered adobe" |
                      hv214 == "cardboard" | hv214 == "reused wood" | hv214 == "iron sheets",1,0)]
KEHH[,water:= ifelse(hv201 == "piped water" | hv201== "piped into dwelling" | hv201== "piped to yard/plot" | hv201== "public tap/standpipe" |
                     hv201 == "tube well water" | hv201 == "tube well or borehole" | hv201 == "protected well", 0,1)]
KEHH[,latrine:= ifelse(hv205 == "pit toilet latrine" | hv205== "ventilated improved pit latrine (vip)" | hv205== "pit latrine without slab/open pit" | hv205== "no facility/bush/field" |
                       hv205 == "composting toilet" | hv205 == "bucket toilet" | hv205 == "hanging toilet/latrine" | hv205 == "other", 1,0)]
KEHH$hv012<-ifelse(KEHH$hv012 == 0, KEHH$hv013, KEHH$hv012)
KEHH$memsleep <- ifelse(KEHH$hv216 == 0,KEHH$hv012,KEHH$hv012/KEHH$hv216)
KEHH[,living:= ifelse(KEHH$memsleep > 3,1,0)]

KEHH<-KEHH %>% 
  rowwise() %>% mutate(slum.sum = sum(floor,water,latrine,living, na.rm=T))
KEHH<-KEHH %>% mutate(slum1 = case_when(
  hv025=="rural" ~ "rural",
  (hv025 == "urban" & slum.sum >= 2) ~ "urban slum",
  (hv025 == "urban" & slum.sum < 2) ~ "urban non-slum"))

KEHH$v001<-KEHH$hv001
KEHH$v002<-KEHH$hv002

# Urban slum 2 - Zulu et al, 2002
KEHH$elec<- ifelse(KEHH$hv206 == "no",1,0)
KEHH$piped<- ifelse(KEHH$hv201 == "piped water" | KEHH$hv201== "piped into dwelling" | KEHH$hv201== "piped to yard/plot" | KEHH$hv201== "public tap/standpipe", 0,1)
KEHH$slum2 <- ifelse(KEHH$hh.urban == 1 & KEHH$elec == 1 & KEHH$latrine == 1 & KEHH$piped == 1, 1, 0)

######################## Immunization
# This code was written by Navideh and I don't think it quite works with this dataset I created, needs some work

#Similar to Cata-Preta et al, 2021, prevalence of zero-dose children was defined as those who had not received any doses of the 
#four vaccines (four basic vaccines: BCG, polio (OPV and IPV, doses given at birth were not considered in the analyses), DPT, and MCV). 
#The immunization cascade was generated as a score ranging from 0 to 4. Each type of vaccine accounts for one point in the cascade regardless of the number of doses. 
#For example, a child was coded as “1" if either one, two or three doses of DPT/polio vaccine had been received. 

KEBR[,ChildAge:= (v008-b3)] # in months
summary(KEBR[is.na(h2),]$ChildAge)
summary(KEBR[is.na(h4),]$b8)
summary(KEBR[is.na(h4) & b5=="yes",]$ChildAge)

KEBR[,ZeroDose:= ifelse(h2== "no" & h3== "no" & h4== "no" & h5== "no" & h6== "no" & h7== "no" & h8== "no" & h9== "no",1,0)]
KEBR$ZeroDose <- ifelse((is.na(KEBR$h2))|(is.na(KEBR$h3))|(is.na(KEBR$h4))|(is.na(KEBR$h5))|(is.na(KEBR$h6))|(is.na(KEBR$h7))|(is.na(KEBR$h8))|(is.na(KEBR$h9)),NA,KEBR$ZeroDose)
KEBR$ZeroDose <- ifelse(KEBR$h2=="don't know" |KEBR$h3=="don't know"|KEBR$h4=="don't know" |KEBR$h5=="don't know" |KEBR$h6=="don't know" |KEBR$h7=="don't know" |KEBR$h8=="don't know" |KEBR$h9=="don't know",NA,KEBR$ZeroDose)

######################## 
KEBR[!(is.na(h2))& h2!="don't know",bcg:=ifelse(h2 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
KEBR[!(is.na(h3))& h3!="don't know",dpt1:=ifelse(h3 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
KEBR[!(is.na(h4))& h4!="don't know",opv1:=ifelse(h4 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
KEBR[!(is.na(h5))& h5!="don't know",dpt2:=ifelse(h5 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
KEBR[!(is.na(h6))& h6!="don't know",opv2:=ifelse(h6 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
KEBR[!(is.na(h7))& h7!="don't know",dpt3:=ifelse(h7 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
KEBR[!(is.na(h8))& h8!="don't know",opv3:=ifelse(h8 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
KEBR[!(is.na(h9))& h9!="don't know",mcv1:=ifelse(h9 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]

KEBR[,dpt:= ifelse(dpt1==1 |dpt2==1 | dpt3==1, 1, 0)]
KEBR[,opv:= ifelse(opv1==1 |opv2==1 | opv3==1, 1, 0)]

KEBR[,score:= (bcg + opv + dpt + mcv1)]
KEBR[,score2:= (bcg + opv1 + opv2 + opv3 + dpt1 + dpt2 + dpt3 + mcv1)]

KEBR[,table(ZeroDose)]

KEBR[,table(ZeroDose,b5)]

KEBR$Agegrp <- ifelse(KEBR$ChildAge < 12, "<12",ifelse(KEBR$ChildAge >=12 & KEBR$ChildAge <=23, "12-23",ifelse(KEBR$ChildAge>23 &KEBR$ChildAge<=35,"24-35",
                                                                                                               ifelse(KEBR$ChildAge>35 &KEBR$ChildAge<=59,"36-59","60+"))))

KEBR$Agegrp <- factor(KEBR$Agegrp, levels = c("<12","12-23", "24-35","36-59","60+"))

KEBR[,table(ZeroDose,Agegrp)]
KEBR[,table(score,Agegrp)]
KEBR[,table(dpt1,Agegrp)]

KEBR[!(is.na(ZeroDose)),list(count=length(unique(caseid))),keyby=c("ZeroDose","Agegrp")]
KEBR[!(is.na(ZeroDose)),list(count=length(unique(caseid))),keyby=c("score","Agegrp")]
KEBR[!(is.na(dpt1)),list(count=length(unique(caseid))),keyby=c("dpt1","Agegrp")]

KEBR[,dpt12mcv1:= ifelse(dpt1==1 & dpt2==1 & mcv1==1 & dpt3==0, 1, 0)]

KEZD<-subset(KEBR, ChildAge<60)

######################## Matching Pathways with DHS
KEZD<-KEBR %>% group_by(caseid) %>% summarise(ZDkids = sum(ZeroDose), kids=n())

# Gather up data into new dataset
KEIR2 <- KEIRcut %>% select(c("v001","v002","v005","v021","v023","caseid","hb_1", "hb_ever", "own.vehic", "freq.radtv", "ed.yrs", "ed.grp", "marr.cohab", "woman.hoh", "age.1stsex",
                              "radtv.yn", "age.1stsex.cat", "age.1stcohab", "age.1stcohab.cat", "age.1stbrth", "age.1stbrth.cat",
                              "ed.lit","woman.age","woman.agegrp","num.hhmem","num.child","num.child.ever","num.child.die","preg.term", "husb.ed.grp", "jd.index","wd.index",
                              "anc.1stvisit","anc.num","fp.mod.now","hc.chw","hc.hfac", "wealth","dv.index","med.alone","med.permis", "med.cost","med.dist","med.index","muslim","anc.4mo"))

# not in Kenya dataset "med.dnk""med.transp", "med.fempro","med.nopro","med.nomeds",
                              

KEHH2 <- KEHH %>% select(c("v001", "v002", "head.ed.grp","head.ed.yrs","hh.bank.acct","hh.land","hh.animal","hh.phone", "hh.urban", "floor", "wall", "water", "latrine", "living", 
                           "hh.woman15ov", 'hh.kidwom.rat', "slum.sum", "slum1", "slum2"))

KEBR2<-KEBR %>% select(c("ZeroDose","score","Agegrp","caseid"))

#seg<- dhs.seg %>% select(c("caseid", "segX4","jntdm.X4", "husband.X4","hcbar.X4", "finsay.X4", "dv.justX5","popden.delta.20"))

######################## Merge IR and HH
kenya<-merge(KEIR2, KEHH2, by=c("v001", "v002"), all.x=T)
kenya<-merge(kenya, SWPR, by="caseid", all.x=T)
kenya<-merge(kenya, KEBR2, by="caseid", all.x=T)

########## Impute missings
gg_miss_var(kenya, show_pct = TRUE)

######################## Matching Pathways with DHS
Merg <- merge(kenya,seg,by="caseid", all.y = T)
Merg <- merge(kenya,broad.cut,by="caseid", all.y = T)
length(unique(Merg$caseid))

Merg[,table(v302a, useNA="always")]
Merg[,table(hb_1, useNA="always")]
Merg[,table(hb_1, segX4, useNA="always")]
Merg[,table(dlvhm_01, useNA="always")]
Merg[,table(dlvhm_01, hb_1, useNA="always")]
Merg[,table(dlvhm_ever, hb_ever, useNA="always")]
