###################################################################
########          1_Ethiopia_DHS_Outcomes          ###############
######## Defining outcomes for vulnerability analysis ##############
########              Ethiopia DHS 2016              ##############
########              Author: Elisabeth Root?             ##############
######## Last update:   Feb 28 AG changed file directory names and put in pacman script ###
######## input files: ETBR71FL.DTA, ETKR71FL.DTA, ETHR71FL.DTA, ETIR71FL.DTA ##############
######## output files: "1_Ethiopia_DHS_outcomes.RData"    ###########
######## ancestor scripts:  none                      #################
######## descendant scripts: 5_Ethiopia_DHS_exploratory #####################
########                                             ################
####### # Feb 28: AG edited to change file paths and names
###### # Note: the working directory needs to be changed when using scripts (and any scripts called upon)
##################################################################################

rm(list=ls())
pacman::p_load("foreign", "stringr", "magrittr", "reshape2", "data.table", "psych", "vtable", "corrplot", "haven", "survey", "ggplot2", "dplyr", "tidyr")

############# Set working directory
#setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/") #filepath for Anu
#setwd("/Users/adigreif/Desktop/Gates Ethiopia/EthiopiaSegmentation/")

############## Load Ethiopia DHS data
#ETBR <- data.table(read.dta("C:/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways/rawdata/ETH_DHS/ET_2016/ETBR71DT/ETBR71FL.DTA"))
#ETKR <- data.table(read.dta("C:/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways/rawdata/ETH_DHS/ET_2016/ETKR71DT/ETKR71FL.DTA"))
#ETHH <- data.table(read.dta("C:/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways/rawdata/ETH_DHS/ET_2016/ETHR71DT/ETHR71FL.DTA")) 
#ETIR <- data.table(read.dta("C:/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways/rawdata/ETH_DHS/ET_2016/ETIR71DT/ETIR71FL.DTA")) 
   #ETBR <- data.table(read.dta("Modeling/Data/Ethiopia DHS/ET_2016/ETBR71DT/ETBR71FL.DTA")) #filepath for Anu

ETBR <- data.table(read.dta("Data/Ethiopia DHS/ET_2016/ETBR71DT/ETBR71FL.DTA"))
ETKR <- data.table(read.dta("Data/Ethiopia DHS/ET_2016/ETKR71DT/ETKR71FL.DTA"))
ETHH <- data.table(read.dta("Data/Ethiopia DHS/ET_2016/ETHR71DT/ETHR71FL.DTA"))
ETIR <- data.table(read.dta("Data/Ethiopia DHS/ET_2016/ETIR71DT/ETIR71FL.DTA")) 

# For merging HH data to Individual data - "v001", "v002" is cluster, HHID need both to merge by HH

# Still birth
####################
getStillbirths<-function(individ,recode=7)
{
  require(data.table)
  setDT(individ)
  
  #tmp=individ[v228 %in% 1,some,with=F]}
  #tmp[v233>6,] # v233: months of pregnancy. 
  
  tmp=data.table::melt(individ,measure.vars=patterns('^bord[_]','^b0[_]','^b3[_]','^b4[_]','^b5[_]','^b6[_]','^b7[_]','^b8[_]','^b11[_]','^m14[_]','^m15[_]','^m17[_]'),
                       value.name=c('bord','b0','dob',"sex",'alive',"deathAge",'deathAgeMo','CurrAge',"b11","m14","m15","m17"),variable.name='ReverseOrder',
                       id.vars=some)
}

###################
ETIR[,str:=substr(vcal_1,v018,v018+59)]
ETIR[,STL:=ifelse(grepl('TPPPPPP',str),1,NA)]
ETIR[,LB:=ifelse(grepl('BPPPPPPP',str),1,NA)]

dim(ETIR)
some <- names(ETIR)[c(1:38,5900:5905)]

stl <- getStillbirths(ETIR)
stl[,id:=1:dim(stl)[1]]
stl[,start:=str_locate_all(str,"TPPPPPP")[[1]][1],by=c("id")]
stl[,MomAgeSTL:=ceiling((v012 - (start/12))),]
stl[,MomAge:=ceiling((dob-v011)/12),]

stl$period = 60
stl$tu <- stl$v008
stl$tl <- stl$v008 - stl$period 
stl <- stl[tl <= dob & dob < tu,]  #5  years, due to limitation of stillbirth data

stl[,index:= paste(caseid,v000,v007)]
stl[,diff:=MomAgeSTL-MomAge]

sub <- stl[STL==1,]
setkey(sub,index)

sub$rep<- NA
sub$rep[1] <- 1
for (i in 2:dim(sub)[1]){
  sub$rep[i] <- ifelse(sub$index[i]==sub$index[i-1],0,1)
}
setkey(sub,index)

sub <- sub[rep==1,]
sub[,MomAge:=MomAgeSTL]
sub[,deathAge:=NA]
sub[,CurrAge:=NA]
sub[,deathAgeMo:=NA]
sub[,sex:=NA]
sub[,dob:=NA]
sub[,alive:="STL"]

dim(sub)
stl <- rbind(stl,sub[,-67])

setkey(stl,index)

stl[alive=="STL",stl:=1]
stl[is.na(stl),stl:=0]

stl.out <- stl %>% group_by(caseid) %>% summarize(stl.cnt=sum(stl, na.rm=T))

# Recode FP
ETIR$fp.mod.now <- ifelse(ETIR$v313 == "modern method",1,0)
ETIR$fp.dis.5yr<-ifelse((ETIR$v359=="pill"|ETIR$v359=="iud"|ETIR$v359=="injections"|ETIR$v359=="male condom"|
                           ETIR$v359=="implants/norplant"|ETIR$v359=="female condom"|ETIR$v359=="emergency contraception"|
                           ETIR$v359=="other modern method"),1,0)
ETIR$fp.dis.5yr[is.na(ETIR$fp.dis.5yr)] <- 0
ETIR$fp.mod.ever<-ifelse((ETIR$fp.mod.now==1 | ETIR$fp.dis.5yr==1),1,0)

# Recode home births
ETIR[,hb.1:= ifelse(m15_1== "home" | m15_1== "respondent's home" | m15_1== "other home" | m15_1== "other",1,0)]
ETIR[,hb.2:= ifelse(m15_2== "home" | m15_2== "respondent's home" | m15_2== "other home" | m15_2== "other",1,0)]
ETIR[,hb.3:= ifelse(m15_3== "home" | m15_3== "respondent's home" | m15_3== "other home" | m15_3== "other",1,0)]
ETIR[,hb.4:= ifelse(m15_4== "home" | m15_4== "respondent's home" | m15_4== "other home" | m15_4== "other",1,0)]
ETIR[,hb.5:= ifelse(m15_5== "home" | m15_5== "respondent's home" | m15_5== "other home" | m15_5== "other",1,0)]

ETIR$hb.ever[(ETIR$hb.1==1 | ETIR$hb.2==1 | ETIR$hb.3==1 | ETIR$hb.4==1 | ETIR$hb.5==1)]<- 1 
ETIR$hb.ever[(ETIR$hb.1==0 & ETIR$hb.2==0 & ETIR$hb.3==0 & ETIR$hb.4==0 & ETIR$hb.5==0)]<-0
ETIR$hb.ever <- ifelse((is.na(ETIR$hb.1))&(is.na(ETIR$hb.2))&(is.na(ETIR$hb.3))&(is.na(ETIR$hb.4))&(is.na(ETIR$hb.5)),NA,ETIR$hb.ever)

# Women's weight
ETIR$bmi<-ETIR$v445/100
ETIR$bmi<-ifelse(ETIR$bmi>90,NA,ETIR$bmi)
ETIR<- ETIR %>% mutate(bmi.cat1 = case_when(
  (bmi < 16) ~ "Severely thin",
  (bmi >= 16 & bmi < 17) ~ "Moderately thin",
  (bmi >= 17 & bmi < 18.5) ~ "Mildly thin",
  (bmi >= 18.5 & bmi < 25) ~ "Normal",
  (bmi >= 25 & bmi < 30) ~ "Overweight",
  (bmi >= 30) ~ "Obese"))
ETIR$bmi.cat1<-factor(ETIR$bmi.cat1, c("Severely thin", "Moderately thin", "Mildly thin","Normal", "Overweight","Obese"))
ETIR<- ETIR %>% mutate(bmi.cat2 = case_when(
  (bmi < 18.5) ~ "Underweight",
  (bmi >= 18.5 & bmi < 25) ~ "Normal",
  (bmi >= 25 & bmi < 30) ~ "Overweight",
  (bmi >= 30) ~ "Obese"))
ETIR$bmi.cat2<-factor(ETIR$bmi.cat2, c("Underweight","Normal", "Overweight","Obese"))

# Number of ANC visits
ETIR$anc.1stvisit <- ifelse(ETIR$m13_1 == 98,5,ETIR$m13_1) # Impute DNK to the mean of 5
ETIR$anc.1stvisit[ETIR$m14_1 == 0] <-10 # if had no visits set to 10 (birth)
ETIR$anc.num <- ifelse(ETIR$m14_1 == 98,3,ETIR$m14_1) # Impute DNK to the mean of 3
ETIR$anc.4plus <-ifelse(ETIR$anc.num>=4,1,0)
ETIR$anc.month<-ifelse(ETIR$anc.1stvisit<4,"Month 1-3",
                       ifelse(ETIR$anc.1stvisit>3 & ETIR$anc.1stvisit<7,"Month 4-6",
                              ifelse(ETIR$anc.1stvisit>6 & ETIR$anc.1stvisit<10, "Month 7-9", "No ANC")))
ETIR$anc.month <- factor(ETIR$anc.month, c("No ANC","Month 1-3","Month 4-6", "Month 7-9"))
ETIR$anyanc<-ifelse(ETIR$m14_1 == 0,0,1)
ETIR$anc.1sttri<-ifelse(ETIR$anc.month=="Month 1-3", 1,0)

# Woman had health check after birth
temp1<-ifelse(ETIR$m66_1=="yes" & !is.na(ETIR$m66_1),1,0)
temp2<-ifelse(ETIR$m62_1=="yes" & !is.na(ETIR$m62_1),1,0)
ETIR$wom.hlthck<-ifelse((temp1==1|temp2==1),1,0)
ETIR$wom.hlthck<-ifelse((is.na(ETIR$m62_1) & is.na(ETIR$m66_1)),NA,ETIR$wom.hlthck)

# Baby had health check after birth
temp3<-ifelse(ETIR$m70_1=="yes" & !is.na(ETIR$m70_1),1,0)
temp4<-ifelse(ETIR$m74_1=="yes" & !is.na(ETIR$m74_1),1,0)
ETIR$baby.hlthck<-ifelse((temp3==1|temp4==1),1,0)
ETIR$baby.hlthck<-ifelse((is.na(ETIR$m70_1) & is.na(ETIR$m74_1)),NA,ETIR$baby.hlthck)

# Wasting, stunting, underweight for last child
ETIR$haz.last<-ETIR$hw5_1/100; ETIR$haz.last<-ifelse(ETIR$haz.last>90,NA,ETIR$haz.last) #height/age standard deviation (AG 2/28)
ETIR$waz.last<-ETIR$hw8_1/100; ETIR$waz.last<-ifelse(ETIR$waz.last>90,NA,ETIR$waz.last) #weight/age standard deviation (AG 2/28)
ETIR$whz.last<-ETIR$hw11_1/100; ETIR$whz.last<-ifelse(ETIR$whz.last>90,NA,ETIR$whz.last) #weight/height standard deviation (AG 2/28)

ETIR<- ETIR %>% mutate(stunt.cat1.last =  case_when(
  haz.last < -3 ~ "Severely stunted",
  (haz.last >= -3 & haz.last < -2) ~ "Moderately stunted",
  haz.last >= -2 ~ "Not stunted"))
ETIR$stunt.cat1.last<-factor(ETIR$stunt.cat1, c("Severely stunted","Moderately stunted", "Not stunted"))
ETIR$stunt.cat2.last<-ifelse(ETIR$haz.last < -2,1,0)

ETIR<- ETIR %>% mutate(waste.cat1.last =  case_when(
  waz.last < -3 ~ "Severely wasted",
  (waz.last >= -3 & waz.last < -2) ~ "Moderately wasted",
  waz.last >= -2 ~ "Not wasted"))
ETIR$waste.cat1.last<-factor(ETIR$waste.cat1, c("Severely wasted","Moderately wasted", "Not wasted"))
ETIR$waste.cat2.last<- ifelse(ETIR$waz.last < -2,1,0)

ETIR<- ETIR %>% mutate(unwgt.cat1.last =  case_when(
  waz.last < -3 ~ "Severely underweight",
  (waz.last >= -3 & waz.last < -2) ~ "Moderately underweight",
  waz.last >= -2 ~ "Not underweight"))
ETIR$unwgt.cat1.last<-factor(ETIR$unwgt.cat1, c("Severely underweight","Moderately underweight", "Not underweight"))
ETIR$undwgt.last<- ifelse(ETIR$waz.last < -2,1,0)
ETIR$ovrwgt.last<-ifelse(ETIR$waz.last >2,1,0)

# Breastfeeding last child
ETIR$breastfed<-ifelse(ETIR$m4_1 == "ever breastfed, not currently breastfeeding" | ETIR$m4_1=="still breastfeeding",1,0)
ETIR$breastfed2<-ifelse((ETIR$b19_01 < 24 & ETIR$b9_01 == "respondent"),ETIR$breastfed, NA) #child is <24 months, child lives with respondent (AG added 2/28)

ETIR <- ETIR %>%
  mutate(comp.feed = case_when(ETIR$v409 == "yes" | ETIR$v410 == "yes" | ETIR$v411 == "yes" | ETIR$v411 == "yes" | ETIR$v412a == "yes" |
                                 ETIR$v412c == "yes" | ETIR$v413 == "yes" | ETIR$v414e == "yes"  | ETIR$v414f == "yes" |
                                 ETIR$v414g == "yes" | ETIR$v414h == "yes" | ETIR$v414i == "yes" | ETIR$v414j == "yes" |
                                 ETIR$v414k == "yes" | ETIR$v414l == "yes" | ETIR$v414m == "yes" | ETIR$v414n == "yes" |
                                 ETIR$v414o == "yes" | ETIR$v414p == "yes" | ETIR$v414s == "yes" | ETIR$v414v == "yes" ~1,
                               TRUE ~ 0))
ETIR$comp.feed<-ifelse((ETIR$b19_01 < 24 & ETIR$b9_01 == "respondent"),ETIR$comp.feed, NA) ##these are questions on other feed types, like water or solids

ETIR$breastfed2.excl<-ifelse((ETIR$breastfed2==1 & ETIR$comp.feed==0),1,0)

ir.out<-ETIR %>% select(c("v001","v002","caseid","baby.hlthck","wom.hlthck","bmi","bmi.cat1","bmi.cat2","hb.1","fp.mod.now",
                          "fp.mod.ever","anc.1stvisit","anc.num","anc.4plus","anc.month","anyanc","anc.1sttri","haz.last","waz.last","whz.last",
                          "stunt.cat1.last","stunt.cat2.last","waste.cat1.last","waste.cat2.last","unwgt.cat1.last","undwgt.last","ovrwgt.last",
                          "breastfed","breastfed2","breastfed2.excl"))

########## KR FILE
# Wasting, stunting, underweight for all children
ETKR$hhid<-paste(ETKR$v001,ETKR$v002)
ETKR$haz<-ETKR$hw5/100; ETKR$haz<-ifelse(ETKR$haz>90,NA,ETKR$haz)
ETKR$waz<-ETKR$hw8/100; ETKR$waz<-ifelse(ETKR$waz>90,NA,ETKR$waz)
ETKR$whz<-ETKR$hw11/100; ETKR$whz<-ifelse(ETKR$whz>90,NA,ETKR$whz)

ETKR<- ETKR %>% mutate(stunt.cat1 =  case_when(
  haz < -3 ~ "Severely stunted",
  (haz >= -3 & haz < -2) ~ "Moderately stunted",
  haz >= -2 ~ "Not stunted"))
ETKR$stunt.cat1<-factor(ETKR$stunt.cat1, c("Severely stunted","Moderately stunted", "Not stunted"))
ETKR$stunt.cat2<-ifelse(ETKR$haz < -2,1,0)

ETKR<- ETKR %>% mutate(waste.cat1 =  case_when(
  whz < -3 ~ "Severely wasted",
  (whz >= -3 & whz < -2) ~ "Moderately wasted",
  whz >= -2 ~ "Not wasted"))
ETKR$waste.cat1<-factor(ETKR$waste.cat1, c("Severely wasted","Moderately wasted", "Not wasted"))
ETKR$waste.cat2<- ifelse(ETKR$whz < -2,1,0)

ETKR<- ETKR %>% mutate(unwgt.cat1 =  case_when(
  waz < -3 ~ "Severely underweight",
  (waz >= -3 & waz < -2) ~ "Moderately underweight",
  waz >= -2 ~ "Not underweight"))
ETKR$unwgt.cat1<-factor(ETKR$unwgt.cat1, c("Severely underweight","Moderately underweight", "Not underweight"))
ETKR$undwgt<- ifelse(ETKR$waz < -2,1,0)
ETKR$ovrwgt<-ifelse(ETKR$waz >2,1,0)

# Breastfeeding
ETKR$breastfeed<-ifelse(ETKR$m4 == "ever breastfed, not currently breastfeeding" | ETKR$m4=="still breastfeeding",1,0)
ETKR$breastfeed<-ifelse((ETKR$b19 < 24 & ETKR$b9 == "respondent"),ETKR$breastfeed, NA)

ETKR <- ETKR %>%
  mutate(comp.feed = case_when(ETKR$v409 == "yes" | ETKR$v410 == "yes" | ETKR$v411 == "yes" | ETKR$v411 == "yes" | ETKR$v412a == "yes" |
                                 ETKR$v412c == "yes" | ETKR$v413 == "yes" | ETKR$v414e == "yes"  | ETKR$v414f == "yes" |
                                 ETKR$v414g == "yes" | ETKR$v414h == "yes" | ETKR$v414i == "yes" | ETKR$v414j == "yes" |
                                 ETKR$v414k == "yes" | ETKR$v414l == "yes" | ETKR$v414m == "yes" | ETKR$v414n == "yes" |
                                 ETKR$v414o == "yes" | ETKR$v414p == "yes" | ETKR$v414s == "yes" | ETKR$v414v == "yes" ~1,
                               TRUE ~ 0))
ETKR$comp.feed<-ifelse((ETKR$b19 < 24 & ETKR$b9 == "respondent"),ETKR$comp.feed, NA)

ETKR$breastfeed.excl<-ifelse((ETKR$breastfeed==1 & ETKR$comp.feed==0),1,0)

ETKR$breastfeed.n<-ifelse((ETKR$b19 < 24 & ETKR$b9 == "respondent"),1,0)

kr.mom<- ETKR %>% group_by(caseid) %>% summarize(k.rost=length(caseid), k.meas=length(caseid[(!is.na(whz)&!is.na(haz)&!is.na(waz))]), k.breast=sum(breastfeed.n, na.rm=T), 
                                              breast.cnt=sum(breastfeed, na.rm=T), breastex.cnt=sum(breastfeed.excl, na.rm=T),
                                              stunt.cnt=sum(stunt.cat2, na.rm=T), waste.cnt = sum(waste.cat2, na.rm=T), unwgt.cnt = sum(undwgt, na.rm=T),
                                              ovrwgt.cnt = sum(ovrwgt, na.rm=T), haz.mean=mean(haz, na.rm=T), waz.mean=mean(waz, na.rm=T), 
                                              whz.mean=mean(whz, na.rm=T))
kr.out<- ETKR %>% select(c("caseid", "haz","waz","whz","stunt.cat1","stunt.cat2","waste.cat1", "waste.cat2","unwgt.cat1","undwgt","ovrwgt",
                           "breastfeed","breastfeed.excl"))

# Immunization - zero-dose
#Similar to Cata-Preta et al, 2021, prevalence of zero-dose children was defined as those who had not received any doses of the 
#four vaccines (four basic vaccines: BCG, polio (OPV and IPV, doses given at birth were not considered in the analyses), DPT, and MCV). 
#The immunization cascade was generated as a score ranging from 0 to 4. Each type of vaccine accounts for one point in the cascade regardless of the number of doses. 
#For example, a child was coded as "1" if either one, two or three doses of DPT/polio vaccine had been received. 

ETBR[,ChildAge:= (v008-b3)] # in months
summary(ETBR[is.na(h2),]$ChildAge)
summary(ETBR[is.na(h4),]$b8)
summary(ETBR[is.na(h4) & b5=="yes",]$ChildAge)

ETBR[,ZeroDose:= ifelse(h2== "no" & h3== "no" & h4== "no" & h5== "no" & h6== "no" & h7== "no" & h8== "no" & h9== "no",1,0)]
ETBR$ZeroDose <- ifelse((is.na(ETBR$h2))|(is.na(ETBR$h3))|(is.na(ETBR$h4))|(is.na(ETBR$h5))|(is.na(ETBR$h6))|(is.na(ETBR$h7))|(is.na(ETBR$h8))|(is.na(ETBR$h9)),NA,ETBR$ZeroDose)
ETBR$ZeroDose <- ifelse(ETBR$h2=="don't know" |ETBR$h3=="don't know"|ETBR$h4=="don't know" |ETBR$h5=="don't know" |ETBR$h6=="don't know" |ETBR$h7=="don't know" |ETBR$h8=="don't know" |ETBR$h9=="don't know",NA,ETBR$ZeroDose)

######################## 
ETBR[!(is.na(h2))& h2!="don't know",bcg:=ifelse(h2 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
ETBR[!(is.na(h3))& h3!="don't know",dpt1:=ifelse(h3 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
ETBR[!(is.na(h4))& h4!="don't know",opv1:=ifelse(h4 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
ETBR[!(is.na(h5))& h5!="don't know",dpt2:=ifelse(h5 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
ETBR[!(is.na(h6))& h6!="don't know",opv2:=ifelse(h6 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
ETBR[!(is.na(h7))& h7!="don't know",dpt3:=ifelse(h7 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
ETBR[!(is.na(h8))& h8!="don't know",opv3:=ifelse(h8 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
ETBR[!(is.na(h9))& h9!="don't know",mcv1:=ifelse(h9 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]

ETBR[,dpt:= ifelse(dpt1==1 |dpt2==1 | dpt3==1, 1, 0)]
ETBR[,opv:= ifelse(opv1==1 |opv2==1 | opv3==1, 1, 0)]

ETBR[,score:= (bcg + opv + dpt + mcv1)]
ETBR[,score2:= (bcg + opv1 + opv2 + opv3 + dpt1 + dpt2 + dpt3 + mcv1)]

#ETBR[!(is.na(ZeroDose)),list(count=length(unique(caseid))),keyby=c("ZeroDose","Agegrp")]
#ETBR[!(is.na(ZeroDose)),list(count=length(unique(caseid))),keyby=c("score","Agegrp")]
#ETBR[!(is.na(dpt1)),list(count=length(unique(caseid))),keyby=c("dpt1","Agegrp")]

ETBR[,dpt12mcv1:= ifelse(dpt1==1 & dpt2==1 & mcv1==1 & dpt3==0, 1, 0)]

# Child death U5, U1
#b3 - Date of birth of child (CMC)
#b5 - Child is alive (1 = Yes, 0 = No)
#b7 - Age at death in months (imputed)
ETBR$hhid<-paste(ETBR$v001,ETBR$v002)
ETBR$u1mort<-ifelse(ETBR$b7<13 & ETBR$b5=="no",1,0)
ETBR$u5mort<-ifelse(ETBR$b5=="no",1,0)

br.mom <- ETBR %>% group_by(caseid) %>% summarize(b.rost=length(caseid), zerodosecnt = sum(ZeroDose, na.rm=T), dpt1cnt = sum(dpt1, na.rm=T),
                                               avgscore=mean(score, na.rm=T), u1mortcnt = sum(u1mort, na.rm=T), u5mortcnt = sum(u5mort, na.rm=T))
br.out<- ETBR %>% select(c("caseid", "v001","v002","ZeroDose","dpt1","score", "u1mort","u5mort"))

outcomes <- merge(ir.out, kr.mom, by="caseid", all.x = TRUE)
outcomes <- merge(outcomes, br.mom, by="caseid", all.x=T)
outcomes <- merge(outcomes, stl.out, by="caseid", all.x=T)

#### Define and group outcomes for use in age exploration file ####

## Make binary versions of some outcomes 
# Underweight
outcomes$underweight <- ifelse(outcomes$bmi.cat2 == "Underweight", 1, 
                              ifelse(!is.na(outcomes$bmi.cat2), 0, NA))

# Zero dose
outcomes$zerodose.yn <- ifelse(outcomes$zerodosecnt > 0 & !is.na(outcomes$zerodosecnt), 1, 
                             ifelse(outcomes$zerodosecnt == 0, 0, NA))

# DPT1
outcomes$dpt1.yn <- ifelse(outcomes$dpt1cnt > 0 & !is.na(outcomes$dpt1cnt), 1, 
                          ifelse(outcomes$dpt1cnt == 0, 0, NA))

# Any deaths of kids under 1
outcomes$u1mort.yn <- ifelse(outcomes$u1mortcnt > 0 & !is.na(outcomes$u1mortcnt), 1, 
                            ifelse(outcomes$u1mortcnt == 0, 0, NA))

# Any deaths of kids under 5
outcomes$u5mort.yn <- ifelse(outcomes$u5mortcnt > 0 & !is.na(outcomes$u5mortcnt), 1, 
                             ifelse(outcomes$u5mortcnt == 0, 0, NA))


##Adi update on file path:
save(outcomes, file = "Data/Ethiopia_constructed_data/1_Ethiopia_DHS_outcomes.RData")

#save(outcomes, file = "C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways - Gender Equality #Division\\Modeling\\Code\\Ethiopia_DHS_outcomes.RData")

