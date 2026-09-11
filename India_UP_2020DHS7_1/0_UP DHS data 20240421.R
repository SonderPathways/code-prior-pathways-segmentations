rm(list=ls())
pacman::p_load(RODBC, dplyr, tidyverse, openxlsx, tidyselect)
pacman::p_load("foreign", "stringr", "magrittr", "reshape2", "data.table", "psych", "vtable", "corrplot", "haven", 
               "survey", "ggplot2", "dplyr", "tidyr", "sf", "ggplot", "RColorBrewer")
conn <- odbcConnect("IDMAzureDatabricks_DSN")
options(max.print=1000000)
setwd("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2020 DHS")


# 
# UPIR <- sqlQuery(conn, "SELECT * FROM idm_dhs.ind.iair7efl WHERE v024 = 'uttar pradesh'")
# save(UPIR, file = "Data/UPIR.RData")
# UPHR <- sqlQuery(conn, "SELECT * FROM idm_dhs.ind.iahr7efl WHERE hv024 = 'uttar pradesh'")
# save(UPHR, file = "Data/UPHR.RData")

# UPBR <- sqlQuery(conn, "SELECT * FROM idm_dhs.ind.iabr7efl WHERE v024 = 'uttar pradesh'") 
# save(UPBR, file = "Data/UPBR.RData")
# UPKR <- sqlQuery(conn, "SELECT * FROM idm_dhs.ind.iakr7efl WHERE v024 = 'uttar pradesh'") 
# save(UPKR, file = "Data/UPKR.RData")
# UPPR <- sqlQuery(conn, "SELECT * FROM idm_dhs.ind.iapr7efl WHERE hv024 = 'uttar pradesh'") 
# save(UPPR, file = "Data/UPPR.RData")

### READ-IN TRUNCATED UP DATASET
load("Data/UPBR.RData")
load("Data/UPKR.RData")
load("Data/UPPR.RData")
load("Data/UPIR.RData")
load("Data/UPHR.RData")

##further truncated to DV sample
#one woman has weird calendar data 
UPIR <- UPIR %>% filter(caseid!="  0914701783 01")
UPIR <- UPIR %>% filter(v501=="married" & v044=="woman selected and interviewed")
UPBR <- UPBR %>% filter(v501=="married" & v044=="woman selected and interviewed")
UPKR <- UPKR %>% filter(v501=="married" & v044=="woman selected and interviewed")

HH <- UPHR
IR <- UPIR
BR <- UPBR
KR <- UPKR
PR <- UPPR
save(HH, file = "Data/HH.RData")
save(IR, file = "Data/IR.RData")
save(BR, file = "Data/BR.RData")
save(KR, file = "Data/KR.RData")
save(PR, file = "Data/PR.RData")


### READ SPATIAL DATA 
bound <- read_sf("Data/sdr_subnational_boundaries/shps/sdr_subnational_boundaries2.shp")
table(bound$DHSREGEN)
table(bound$REGNAME)
table(bound$OTHREGNA)
bound <- subset(bound, OTHREGNA=="Uttar Pradesh")
# capital city 
capital <- c("Lucknow")
bound$capital_REGNAME <- ifelse(bound$REGNAME %in% capital, "Capital", "Non-Capital")

#urbanization 
UPIR$urban <- ifelse(UPIR$v025=="urban", 1, 0)
urban <- UPIR %>% select(sdist, urban) 
length(unique(urban$sdist)) #75 districts 

urban_region <- urban %>% 
  group_by(sdist) %>%
  summarise(urban = mean(urban, na.rm = T))
summary(urban_region$urban) #mean=20%
table(urban_region$sdist)
table(bound$REGNAME)
bound$REGNAME <- tolower(bound$REGNAME)
urban_region$REGNAME <- urban_region$sdist
urban_region$REGNAME[urban_region$sdist=="balrampur_(182)"]              <- "balrampur"
urban_region$REGNAME[urban_region$sdist=="hamirpur_(168)"]               <- "hamirpur"
urban_region$REGNAME[urban_region$sdist=="pratapgarh_(173)"]             <- "pratapgarh"
urban_region$REGNAME[urban_region$sdist=="balrampur_(182)"]              <- "balrampur"
urban_region$REGNAME[urban_region$sdist=="sant ravidas nagar (bhadohi)"] <- "sant ravidas nagar"
urban_region$REGNAME[urban_region$sdist=="mahrajganj"]                   <- "maharajganj"
urban_region <- urban_region%>%select(urban, REGNAME)
urban_region_bound <- merge(urban_region, bound, by="REGNAME")

ggplot(bound) + 
  geom_sf(aes(fill = capital_REGNAME)) +
  scale_fill_manual(values = c("red2", "white")) +
  ggtitle("India: Uttar Pradesh (2020 DHS)") +
  coord_sf() +
  theme_void() +
  theme(legend.position="none")

#### Palettes 
palette <- colorRampPalette(brewer.pal(9,"PuBu"))
cols    <- palette(4)
# "#023858" "#2585BA" "#B3C3DE" "#FFF7FB"

ggplot(data = urban_region_bound, aes(geometry = geometry, fill=urban)) + 
  geom_sf() +
  ggtitle("Urbanization of Uttar Pradesh, India (2020 DHS)") +
  scale_fill_gradientn(limits = c(0,1), colours=cols) + 
  theme_void()


###checking sample 
summary(UPIR$v012)
table(UPIR$v025)
prop.table(table(UPIR$v025)) #20% urban -- make a map here 
table(UPIR$v501)
prop.table(table(UPIR$v501))
table(UPIR$v201) #number of children everborn 
prop.table(table(UPIR$v201)) #36% have never had children 

### urban/rural by household surveys 
prop.table(table(UPHR$hv025)) #20% urban: not much different than 
table(UPIR$sdist)
length(unique(UPIR$sdist)) #75 districts 

########## KR FILE
# Wasting, stunting, underweight for all children
UPKR$hhid<-paste(UPKR$v001,UPKR$v002)
UPKR$haz<-UPKR$hw5/100; UPKR$haz<-ifelse(UPKR$haz>90,NA,UPKR$haz)
UPKR$waz<-UPKR$hw8/100; UPKR$waz<-ifelse(UPKR$waz>90,NA,UPKR$waz)
UPKR$whz<-UPKR$hw11/100; UPKR$whz<-ifelse(UPKR$whz>90,NA,UPKR$whz)

UPKR<- UPKR %>% mutate(stunt.cat1 =  case_when(
  haz < -3 ~ "Severely stunted",
  (haz >= -3 & haz < -2) ~ "Moderately stunted",
  haz >= -2 ~ "Not stunted"))
UPKR$stunt.cat1 <- factor(UPKR$stunt.cat1, c("Severely stunted","Moderately stunted", "Not stunted"))
UPKR$stunt.cat2 <- ifelse(UPKR$haz < -2,1,0)
table(UPKR$stunt.cat1)
table(UPKR$stunt.cat2)

UPKR<- UPKR %>% mutate(waste.cat1 =  case_when(
  whz < -3 ~ "Severely wasted",
  (whz >= -3 & whz < -2) ~ "Moderately wasted",
  whz >= -2 ~ "Not wasted"))
UPKR$waste.cat1<-factor(UPKR$waste.cat1, c("Severely wasted","Moderately wasted", "Not wasted"))
UPKR$waste.cat2<- ifelse(UPKR$whz < -2,1,0)
table(UPKR$waste.cat1)
table(UPKR$waste.cat2)

UPKR<- UPKR %>% mutate(unwgt.cat1 =  case_when(
  waz < -3 ~ "Severely underweight",
  (waz >= -3 & waz < -2) ~ "Moderately underweight",
  waz >= -2 ~ "Not underweight"))
UPKR$unwgt.cat1 <-factor(UPKR$unwgt.cat1, c("Severely underweight","Moderately underweight", "Not underweight"))
UPKR$unwgt.cat2 <- ifelse(UPKR$waz < -2,1,0)
table(UPKR$unwgt.cat1)
table(UPKR$unwgt.cat2)

# Breastfeeding
# label define M4      
# 93 "Ever breastfed, not currently breastfeeding"
# 94 "Never breastfed"
# 95 "Still breastfeeding"
# 96 "Breastfed until died"
# 97 "Inconsistent"
# 98 "Don't know"
UPKR$breastfeed <- ifelse(UPKR$m4 <94 | UPKR$m4==95, 1, 0)
UPKR$breastfeed <-ifelse((UPKR$b19 < 24 & UPKR$b9 == "respondent"), UPKR$breastfeed, NA)
UPKR$breastfeed[UPKR$m4==98] <- NA
table(UPKR$breastfeed)
prop.table(table(UPKR$breastfeed))

v41 <- UPKR %>% dplyr:: select(starts_with("v41"))
lapply(v41, table)
UPKR <- UPKR %>%
  mutate(comp.feed = case_when(UPKR$v409 == "yes" | UPKR$v410 == "yes" | UPKR$v411 == "yes" | UPKR$v411a == "yes" | 
                                 UPKR$v412a == "yes" | UPKR$v412c == "yes" | UPKR$v413 == "yes"  | UPKR$v414a == "yes" |
                                 UPKR$v414e == "yes" | UPKR$v414f == "yes" | UPKR$v414g == "yes" | UPKR$v414i == "yes" | 
                                 UPKR$v414j == "yes" | UPKR$v414k == "yes" | UPKR$v414l == "yes" | UPKR$v414m == "yes" | 
                                 UPKR$v414n == "yes" | UPKR$v414o == "yes" | UPKR$v414p == "yes" | UPKR$v414s == "yes" | 
                                 UPKR$v414t == "yes" | UPKR$v414v == "yes" ~1, TRUE ~ 0))
UPKR$comp.feed<-ifelse((UPKR$b19 < 24 & UPKR$b9 == "respondent"),UPKR$comp.feed, NA)
table(UPKR$comp.feed)

UPKR$breastfeed.excl<-ifelse((UPKR$breastfeed==1 & UPKR$comp.feed==0),1,0)
table(UPKR$breastfeed.excl)

UPKR$breastfeed.n<-ifelse((UPKR$b19 < 24 & UPKR$b9 == "respondent"),1,0)
table(UPKR$breastfeed.n)

kr.mom<- UPKR %>% group_by(caseid) %>% summarize(k.rost=length(caseid), 
                                                 k.meas=length(caseid[(!is.na(whz)&!is.na(haz)&!is.na(waz))]), 
                                                 k.breast=sum(breastfeed.n, na.rm=T), 
                                                 breast.cnt=sum(breastfeed, na.rm=T), 
                                                 breastex.cnt=sum(breastfeed.excl, na.rm=T),
                                                 stunt.cnt=sum(stunt.cat2, na.rm=T), 
                                                 waste.cnt = sum(waste.cat2, na.rm=T), 
                                                 unwgt.cnt = sum(unwgt.cat2, na.rm=T),
                                                 haz.mean=mean(haz, na.rm=T), 
                                                 waz.mean=mean(waz, na.rm=T), 
                                                 whz.mean=mean(whz, na.rm=T))
kr.out<- UPKR %>% select(c("caseid", "haz","waz","whz","stunt.cat1","stunt.cat2","waste.cat1", "waste.cat2","unwgt.cat1","unwgt.cat2",
                           "breastfeed","breastfeed.excl"))



########## BR FILE
# Immunization - zero-dose
#Similar to Cata-Preta et al, 2021, prevalence of zero-dose children was defined as those who had not received any doses of the 
#four vaccines (four basic vaccines: BCG, polio (OPV and IPV, doses given at birth were not considered in the analyses), DPT, and MCV). 
#The immunization cascade was generated as a score ranging from 0 to 4. Each type of vaccine accounts for one point in the cascade regardless of the number of doses. 
#For example, a child was coded as "1" if either one, two or three doses of DPT/polio vaccine had been received. 
UPBR[,ChildAge:= (v008-b3)] # in months
summary(UPBR[is.na(h2),]$ChildAge)
summary(UPBR[is.na(h4),]$b8)
summary(UPBR[is.na(h4) & b5=="yes",]$ChildAge)

UPBR[,ZeroDose:= ifelse(h2== "no" & h3== "no" & h4== "no" & h5== "no" & h6== "no" & h7== "no" & h8== "no" & h9== "no", 1, 0)]
UPBR$ZeroDose <- ifelse(UPBR$h2=="don't know" |UPBR$h3=="don't know"|UPBR$h4=="don't know" |UPBR$h5=="don't know" |UPBR$h6=="don't know" |UPBR$h7=="don't know" |UPBR$h8=="don't know" |UPBR$h9=="don't know",NA,UPBR$ZeroDose)
table(UPBR$ZeroDose)
prop.table(table(UPBR$ZeroDose)) #6% very low 

######################## 
UPBR[!(is.na(h2))& h2!="don't know",bcg:=ifelse(h2 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
UPBR[!(is.na(h3))& h3!="don't know",dpt1:=ifelse(h3 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
UPBR[!(is.na(h4))& h4!="don't know",opv1:=ifelse(h4 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
UPBR[!(is.na(h5))& h5!="don't know",dpt2:=ifelse(h5 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
UPBR[!(is.na(h6))& h6!="don't know",opv2:=ifelse(h6 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
UPBR[!(is.na(h7))& h7!="don't know",dpt3:=ifelse(h7 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
UPBR[!(is.na(h8))& h8!="don't know",opv3:=ifelse(h8 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
UPBR[!(is.na(h9))& h9!="don't know",mcv1:=ifelse(h9 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
UPBR[,dpt:= ifelse(dpt1==1 |dpt2==1 | dpt3==1, 1, 0)]
UPBR[,opv:= ifelse(opv1==1 |opv2==1 | opv3==1, 1, 0)]
UPBR[,score:= (bcg + opv + dpt + mcv1)]
UPBR[,score2:= (bcg + opv1 + opv2 + opv3 + dpt1 + dpt2 + dpt3 + mcv1)]
UPBR[,dpt12mcv1:= ifelse(dpt1==1 & dpt2==1 & mcv1==1 & dpt3==0, 1, 0)]

# Child death U5, U1
#b3 - Date of birth of child (CMC)
#b5 - Child is alive (1 = Yes, 0 = No)
#b7 - Age at death in months (imputed)
UPBR$hhid<-paste(UPBR$v001,UPBR$v002)
UPBR$u1mort<-ifelse(UPBR$b7<13 & UPBR$b5=="no",1,0)
UPBR$u5mort<-ifelse(UPBR$b5=="no",1,0)

length(unique(UPBR$caseid))
br.mom <- UPBR %>% group_by(caseid) %>% summarize(b.rost=length(caseid), 
                                                  zerodosecnt = sum(ZeroDose, na.rm=T), 
                                                  dpt1cnt = sum(dpt1, na.rm=T),
                                                  avgscore=mean(score, na.rm=T), 
                                                  u1mortcnt = sum(u1mort, na.rm=T), 
                                                  u5mortcnt = sum(u5mort, na.rm=T))
br.out <- UPBR %>% select(c("caseid", "v001","v002","ZeroDose","dpt1","score", "u1mort","u5mort"))
table(br.mom$avgscore[br.mom$ZeroDose==0])


################### IR file
#Pregnancy loss: 
# v234: last pregnancy: need to find a denominator among those who had a last pregnancy??? 
table(UPIR$s234)
prop.table(table(UPIR$s234))
# past five years: vcal_7
UPIR$cal <- substr(UPIR$vcal_7, UPIR$v018, UPIR$v018+59)
#still birth: A baby who dies after 28 weeks of pregnancy, but before or during birth,

x <- UPIR$cal
UPIR$preg.live.cnt   <- lengths(regmatches(x, gregexpr("B", x)))
UPIR$preg.miscar.cnt <- lengths(regmatches(x, gregexpr("M", x)))
UPIR$preg.abort.cnt  <- lengths(regmatches(x, gregexpr("A", x)))
UPIR$preg.still.cnt  <- lengths(regmatches(x, gregexpr("S", x)))
table(UPIR$preg.live.cnt)
table(UPIR$preg.miscar.cnt)
table(UPIR$preg.abort.cnt)
table(UPIR$preg.still.cnt)

#ever experience preg loss
UPIR$preg.loss <- ifelse(UPIR$preg.miscar.cnt  >0 |
                           UPIR$preg.abort.cnt >0 |
                           UPIR$preg.still.cnt >0, 1, 0)
UPIR$preg.loss[UPIR$preg.live.cnt==0] <- NA
table(UPIR$preg.loss)
prop.table(table(UPIR$preg.loss))

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

odbcCloseAll()

