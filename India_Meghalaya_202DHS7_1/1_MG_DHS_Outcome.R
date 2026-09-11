

################################################################################
# GENERATE OUTCOME VARIABLES
################################################################################

# PROGRAM:
# PURPOSE:
# DATA INPUTS:
# DATA OUTPUTS:
# AUTHOR:
# DATA LAST MODIFIED:
# NOTES:


###################################
# RUN SETUP
# IR <- IR1
# KR <- KR1
# BR <- BR1


###################################
# TABLE OF CONTENTS
# 1 | GENERAL
# 2 | ANC/PNC
# 3 | BMI
# 4 | BREASTFEEDING
# 5 | CHILD MORTALITY
# 6 | FAMILY PLANNING
# 7 | HOME BIRTHS
# 8 | IMMUNIZATION
# 9 | MALNOURISHMENT
# 10 | MENSTRUAL HEALTH
# 11 | SUMMARIZE
rm(list=ls())
pacman::p_load(RODBC, dplyr, tidyverse, openxlsx, tidyselect)
pacman::p_load("foreign", "stringr", "magrittr", "reshape2", "data.table", "psych", "vtable", "corrplot", "haven",
               "survey", "ggplot2", "dplyr", "tidyr", "sf", "RColorBrewer", "readxl")



BR <- readRDS("data/BR_meghalaya.rds")
KR <- readRDS("data/KR_meghalaya.rds")
IR <- readRDS("data/IR_meghalaya.rds")

IR <- IR %>%
  filter(v501 %in% c("married", "living with partner"))


  ######################################################################
  # 1 | GENERAL

  # INDIVIDUAL
  IR[,str:=substr(vcal_1, v018, v018+59)]
  IR[,STL:=ifelse(grepl('TPPPPPP',str),1,NA)]
  IR[,LB:=ifelse(grepl('BPPPPPPP',str),1,NA)]


  # BIRTH
  BR$hhid <- paste(BR$v001, BR$v002)
  BR[,ChildAge:= (v008-b3)]


  # KID
  KR$hhid<-paste(KR$v001, KR$v002)

  ######################################################################
  # 2 | ANC/PNC

  # CALCULATE THE MEAN NUMBER OF ANC VISITS AND IMPUTE AS DNK IN ANC OUTCOME VARIABLES
  anc.mean <- as.integer(mean(IR$m14_1, na.rm=TRUE))


  # NUMBER OF ANC VISITS
  IR$anc.num <- ifelse(IR$m14_1 == 98, anc.mean, IR$m14_1)


  # BINARY OUTCOME FOR ANC.NUM >= 4
  IR$anc.4plus <- ifelse(IR$anc.num >= 4, 0, 1) # made_change reversed order
  table(IR$anc.4plus, useNA = "always")


  # CALCULATE THE MEAN MONTH OF 1ST ANC VISIT AND IMPUTE AS DNK IN ANC OUTCOME VARIABLES
  anc.1st.mean <- as.integer(mean(IR$m13_1, na.rm=TRUE))

  # MONTH OF FIRST ANC VISIT
  IR$anc.1stvisit <- ifelse(IR$m13_1 == 98, anc.1st.mean, IR$m13_1)
  IR$anc.1stvisit[IR$m14_1 == 0] <- 10

  # CATEGORICAL OUTCOME FOR ANC 1ST VISIT # made_change did not include because was not binary
  # IR$anc.month <- ifelse(IR$anc.1stvisit < 4, "Month 1-3",
  #                      ifelse(IR$anc.1stvisit > 3 & IR$anc.1stvisit < 7,"Month 4-6",
  #                             ifelse(IR$anc.1stvisit > 6 & IR$anc.1stvisit < 10, "Month 7-9", "No ANC")))
  # IR$anc.month <- factor(IR$anc.month, c("No ANC","Month 1-3","Month 4-6", "Month 7-9"))


  # WOMAN HAD HEALTH CHECK AFTER BIRTH made_change reverse order
  temp1 <- ifelse(IR$m66_1=="yes" & !is.na(IR$m66_1), 1, 0)
  temp2 <- ifelse(IR$m62_1=="yes" & !is.na(IR$m62_1), 1, 0)
  IR$wom.hlthck <- ifelse((temp1==1|temp2==1), 0, 1)
  IR$wom.hlthck <- ifelse((is.na(IR$m62_1) & is.na(IR$m66_1)), NA, IR$wom.hlthck)
  table(IR$wom.hlthck, useNA = "always")

  # BABY HAD HEALTH CHECK AFTER BIRTH made_change reverse order
  temp3<-ifelse(IR$m70_1=="yes" & !is.na(IR$m70_1), 1,0)
  temp4<-ifelse(IR$m74_1=="yes" & !is.na(IR$m74_1), 1,0)
  IR$baby.hlthck<-ifelse((temp3==1|temp4==1),0,1)
  IR$baby.hlthck<-ifelse((is.na(IR$m70_1) & is.na(IR$m74_1)), NA, IR$baby.hlthck)
  table(IR$baby.hlthck, useNA = "always")

  ######################################################################
  # 3 | BMI

  # BMI
  IR$bmi<-as.numeric(IR$v445)/100
  IR$bmi<-ifelse(IR$bmi>90, NA, IR$bmi)

  #
  IR$underweight <- NA

  #
  IR$overweight.obese <- NA

  #
  IR$obese <- NA



  ######################################################################
  # 4 | BREASTFEEDING
  
  # 93 "Ever breastfed, not currently breastfeeding"
  # 94 "Never breastfed"
  # 95 "Still breastfeeding"
  # 96 "Breastfed until died"
  # 97 "Inconsistent"
  # 98 "Don't know"

  # CATEGORICAL OUTCOME FOR LATEST CHILD BREASTFED.  # made change. swapped the order
  IR$breastfed <- ifelse(IR$m4_1 == 93 | IR$m4_1== 95, 0, 1)


  # ALTERNATE DEFINITION FOR LAST CHILD BREASTFED
  # IR$breastfed2 <- ifelse((IR$b19_01 < 24 & IR$b9_01 == "respondent"), IR$breastfed, NA)


  #
  # KR$breastfeed.n <- ifelse((KR$b19 < 24 & KR$b9 == "respondent"), 1, 0)



  ######################################################################
  # 5 | CHILD MORTALITY

  # CHILD UNDER 1 DIED
  BR$u1mort <- ifelse(BR$b7<13 & BR$b5=="no", 1, 0)


  # CHILD UNDER 5 DIED
  BR$u5mort<-ifelse(BR$b5=="no", 1, 0)


  # STILLBIRTHS
  getStillbirths <- function(individ,id_vars,recode=7){
    setDT(individ)

    #tmp=individ[v228 %in% 1,some,with=F]}
    #tmp[v233>6,] # v233: months of pregnancy.

    tmp=data.table::melt(individ, measure.vars=patterns('^bord[_]','^b0[_]','^b3[_]','^b4[_]','^b5[_]','^b6[_]','^b7[_]','^b8[_]','^b11[_]','^m14[_]','^m15[_]','^m17[_]'),
                         value.name=c('bord','b0','dob',"sex",'alive',"deathAge",'deathAgeMo','CurrAge',"b11","m14","m15","m17"),variable.name='ReverseOrder',
                         id.vars=id_vars)
  }


  stl_var_list = c("caseid", "v000", "v001", "v002", "v003", "v004", "v005", "v006", "v007", "v008", "v008a", "v009", "v010", "v011", "v012", "v013", "v014", "v015", "v016", "v017",
                   "v018", "v019", "v019a", "v020", "v021", "v022", "v023", "v024", "v025", "v026", "v027", "v028", "v029", "v030", "v031", "v032", "v034", "v040",
                   "sm508va_4", "sm508va_5", "sm508va_6", "str", "STL", "LB")

  some <- names(IR)[(names(IR) %in% stl_var_list)]

  stl <- getStillbirths(IR, id_vars=some)

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
  # stl <- rbind(stl,sub[,-67])  # "rep"
  stl <- rbind(stl, sub[,-c("rep")])

  setkey(stl,index)

  stl[alive=="STL",stl:=1]
  stl[is.na(stl),stl:=0]

  stl.out <- stl %>% group_by(caseid) %>% summarize(stl.cnt=sum(stl, na.rm=T))
  stl.out$stl.yn<-ifelse(stl.out$stl.cnt > 0,1,0)


  ######################################################################
  # 6 | FAMILY PLANNING

  # CURRENTLY USING MODERN FAMILY PLANNING METHODS
  IR$fp.mod.now <- ifelse(IR$v313 == "modern method", 0, 1) # made_change reversed order
  table(IR$fp.mod.now, useNA= "always")

  # EVER USED MODERN FAMILY PLANNING METHODS
  IR$fp.dis.5yr <- ifelse((IR$v359=="pill"|IR$v359=="iud"|IR$v359=="injections"|IR$v359=="male condom"|
                             IR$v359=="implants/norplant"|IR$v359=="female condom"|IR$v359=="emergency contraception"|
                             IR$v359=="other modern method"),1,0)


  
  table(IR$fp.dis.5yr, useNA = "always")


  IR$fp.mod.ever <- ifelse((IR$fp.mod.now==1 | IR$fp.dis.5yr==1), 0, 1) # made_change reversed order
  table(IR$fp.mod.ever, useNA = "always")


  ######################################################################
  # 7 | HOME BIRTHS

  # LATEST BIRTH WAS HOME BIRTH
  IR[,hb.1:= ifelse(m15_1== "home" | m15_1== "respondent's home" | m15_1== "other home" | m15_1== "other", 1, 0)]

  ######################################################################
  # 8 | IMMUNIZATION

  # *** Pentavalent ***
  # DPT 1, 2, 3 either source
  KR <- KR %>%
    mutate(dpt1 = case_when(h3%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h3%in%c("no","don't know") ~ 0  )) %>%
    mutate(dpt2 = case_when(h5%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h5%in%c("no","don't know") ~ 0  )) %>%
    mutate(dpt3 = case_when(h7%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h7%in%c("no","don't know") ~ 0  )) %>%
    mutate(dptsum = dpt1 + dpt2 + dpt3) %>%
    mutate(dpt.full = case_when(is.na(dptsum) ~ NA, dptsum == 3 ~ 0, TRUE ~ 1)) %>%
    mutate(zero.dose = case_when(is.na(dptsum) ~ NA, dptsum == 0 ~ 1, TRUE ~ 0))


  # *** Measles ***
  # Measles either source
  KR <- KR %>%
    mutate(measles1 = case_when(h9%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h9%in%c("no","don't know")  ~ 0  )) %>%
    mutate(measles2 = case_when(h9a%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h9a%in%c("no","don't know")  ~ 0  )) %>%
    mutate(measlessum = measles1 + measles2) %>%
    mutate(measles.full = case_when(is.na(measlessum) ~ NA, measlessum == 2 ~ 0, TRUE ~ 1))


  # *** Polio ***
  # Polio 0, 1, 2, 3 either source
  KR <- KR %>%
    mutate(polio1 = case_when(h4%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h4%in%c("no","don't know") ~ 0  )) %>%
    mutate(polio2 = case_when(h6%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h6%in%c("no","don't know") ~ 0  )) %>%
    mutate(polio3 = case_when(h8%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h8%in%c("no","don't know") ~ 0  )) %>%
    mutate(poliosum=polio1 + polio2 + polio3) %>%
    mutate(polio.full = case_when(is.na(poliosum) ~ NA, poliosum == 3 ~ 0, TRUE ~ 1))


  
  ######################################################################
  # 9 | MALNOURISHMENT

  # STANDARD MALNOURISHMENT VARIABLES IN KID FILE
  KR$haz<-KR$hw5/100; KR$haz <- ifelse(KR$haz>90, NA, KR$haz)
  KR$waz<-KR$hw8/100; KR$waz <- ifelse(KR$waz>90, NA, KR$waz)
  KR$whz<-KR$hw11/100; KR$whz <- ifelse(KR$whz>90, NA, KR$whz)


  # STANDARD MALNOURISHMENT VARIABELS IN WOMEN'S FILE
  IR$haz.last <- IR$hw5_1/100; IR$haz.last<-ifelse(IR$haz.last>90, NA, IR$haz.last) #height/age standard deviation (AG 2/28)
  IR$waz.last <- IR$hw8_1/100; IR$waz.last<-ifelse(IR$waz.last>90, NA, IR$waz.last) #weight/age standard deviation (AG 2/28)
  IR$whz.last <- IR$hw11_1/100; IR$whz.last<-ifelse(IR$whz.last>90, NA, IR$whz.last) #weight/height standard deviation (AG 2/28)


  # KID IS UNDERWEIGHT
  KR$undwgt <- ifelse(KR$waz < -2, 1, 0)
  table(KR$undwgt)

  # LAST KID IS UNDERWEIGHT
  IR$undwgt.last<- ifelse(IR$waz.last < -2, 1, 0)
  table(IR$undwgt.last)

  # CHILD IS OVERWEIGHT
  # KR$ovrwgt <- ifelse(KR$waz > 2, 1, 0)
  # table(KR$ovrwgt)

  # LAST CHILD IS OVERWEIGHT
  # IR$ovrwgt.last <- ifelse(IR$waz.last >2, 1, 0)
  # table(IR$ovrwgt.last)

  # CATEGORICAL OUTCOME FOR KID STUNTING
  KR <- KR %>% mutate(stunt.cat1 =  case_when(
    haz < -3 ~ "Severely stunted",
    (haz >= -3 & haz < -2) ~ "Moderately stunted",
    haz >= -2 ~ "Not stunted"))
  KR$stunt.cat1 <- factor(KR$stunt.cat1, c("Severely stunted", "Moderately stunted", "Not stunted"))


  # BINARY OUTCOME FOR KID STUNTING
  KR$stunt.cat2 <- ifelse(KR$haz < -2, 1, 0)
  table(KR$stunt.cat2, useNA = "always")

  # CATEGORICAL OUTCOME FOR LAST CHILD STUNTING
  IR <- IR %>% mutate(stunt.cat1.last =  case_when(
    haz.last < -3 ~ "Severely stunted",
    (haz.last >= -3 & haz.last < -2) ~ "Moderately stunted",
    haz.last >= -2 ~ "Not stunted"))
  IR$stunt.cat1.last <- factor(IR$stunt.cat1, c("Severely stunted", "Moderately stunted", "Not stunted"))


  # BINARY OUTCOME FOR LAST CHILD STUNTING
  IR$stunt.cat2.last <- ifelse(IR$haz.last < -2, 1, 0)
  table(IR$stunt.cat2.last, useNA = "always")

  # CATEGORICAL OUTCOME FOR CHILD WASTING
  KR<- KR %>% mutate(waste.cat1 =  case_when(
    whz < -3 ~ "Severely wasted",
    (whz >= -3 & whz < -2) ~ "Moderately wasted",
    whz >= -2 ~ "Not wasted"))
  KR$waste.cat1 <- factor(KR$waste.cat1, c("Severely wasted", "Moderately wasted", "Not wasted"))


  # BINARY OUTCOME FOR CHILD WASTING
  KR$waste.cat2 <- ifelse(KR$whz < -2, 1, 0)
  table(KR$waste.cat2, useNA = "always")



  ######################################################################
  # 10 | MENSTRUAL HEALTH

  #
  # IR$mens.priv <- ifelse(IR$s239c=="non",1,0)
  
  IR$no.mens.wash <- ifelse(IR$s264=="no",1,0)
  table(IR$no.mens.wash, useNA = "always")
  
  ######################################################################
  # 11  STI (self-reported)
  table(IR$v763a)
  table(IR$v763b)
  table(IR$v763c)
  IR$sti <- ifelse(IR$v763a=="yes" | IR$v763b =="yes" | IR$v763c=="yes", 1, 0)
  table(IR$sti, useNA = "always")

  ######################################################################
  # 12  Other health
  
  ##hypertension
  table(IR$s728b)
  prop.table(table(IR$s728b))
  table(IR$sb18s)
  table(IR$sb18d)
  sum(is.na(IR$sb18d))
  IR$sb18s <- as.numeric(IR$sb18s)
  IR$sb25s <- as.numeric(IR$sb25s)
  IR$sb29s <- as.numeric(IR$sb29s)
  IR <- as.data.frame(IR)
  IR$mean_systolic <- rowMeans(IR[c('sb18s', 'sb25s', 'sb29s')], na.rm = TRUE)
  summary(IR$mean_systolic)
  IR$sb18d <- as.numeric(IR$sb18d)
  IR$sb25d <- as.numeric(IR$sb25d)
  IR$sb29d <- as.numeric(IR$sb29d)
  IR$mean_diastolic <- rowMeans(IR[c('sb18d', 'sb25d', 'sb29d')], na.rm = TRUE)
  summary(IR$mean_diastolic)
  
  #The new guidelines define hypertension as a systolic blood pressure of =130 mmHg
  #or diastolic blood pressure of =80 mmHg or treatment
  IR$hypertension <- ifelse(IR$mean_systolic>=140 | IR$mean_diastolic>=90, 1, 0)
  IR$hypertension[is.na(IR$mean_systolic) & is.na(IR$mean_diastolic)] <- NA
  table(IR$hypertension)
  prop.table(table(IR$hypertension))
  
  
  #blood glucose
  table(IR$sb56)
  table(IR$sb57)
  table(IR$sb74)
  sum(is.na(IR$sb74))
  sum(is.na(IR$sb56))
  sum(is.na(IR$sb74))
  IR$sb74 <- as.numeric(IR$sb74)
  summary(IR$sb74)
  table(IR$sb74)
  IR$blood_glucose <- ifelse(IR$sb56=="yes" | IR$sb57=="yes", 1, 0)
  IR$blood_glucose[IR$sb74>=140] <- 1
  table(IR$blood_glucose)
  prop.table(table(IR$blood_glucose, useNA = "always"))
  
  
  ######################################################################
  # 11 | SUMMARIZE

  # INDIVIDUAL
  ir.out <- IR %>%  dplyr::select(all_of(c("caseid", "v001" , "v021" ,"v022", "v023",  "v025",
                                    "anc.4plus", "wom.hlthck","baby.hlthck", "breastfed",
                                    "hb.1", "fp.mod.now","undwgt.last",
                                    "stunt.cat2.last","no.mens.wash", "sti", "hypertension","blood_glucose")))

  # BIRTH
  br.out <- BR %>%
    group_by(caseid) %>%
    dplyr::summarize(b.rost = length(caseid),
                     u1mortcnt = sum(u1mort, na.rm=TRUE),
                     u5mortcnt = sum(u5mort, na.rm=TRUE)) %>%
    mutate(u1mort.yn = ifelse(u1mortcnt > 0, 1, 0),
           u5mort.yn = ifelse(u5mortcnt > 0, 1, 0)) %>%
    dplyr::select(caseid, u1mort.yn, u5mort.yn)

  # KID
  kr.out <- KR %>%
    group_by(caseid) %>%
    dplyr::summarize(k.rost = length(caseid),
                     undwgt.cnt = sum(undwgt, na.rm=TRUE),
                     stunt.cat2.cnt = sum(stunt.cat2, na.rm=TRUE),
                     waste.cat2.cnt = sum(waste.cat2, na.rm=TRUE),
                     meas.full.cnt = ifelse(all(is.na(measles.full)), NA, sum(measles.full, na.rm=TRUE)),
                     polio.full.cnt = ifelse(all(is.na(polio.full)), NA, sum(polio.full, na.rm=TRUE)),
                     dpt.full.cnt = ifelse(all(is.na(dpt.full)), NA, sum(dpt.full, na.rm=TRUE)),
                     zerodose.cnt = ifelse(all(is.na(zero.dose)), NA, sum(zero.dose, na.rm=TRUE))) %>%
    mutate(
           
           undwgt.yn = ifelse(undwgt.cnt > 0, 1, 0),
           stunt.cat2.yn = ifelse(stunt.cat2.cnt > 0, 1, 0),
           waste.cat2.yn = ifelse(waste.cat2.cnt > 0, 1, 0),
           meas.full.yn = ifelse(meas.full.cnt > 0, 1, 0),
           polio.full.yn = ifelse(polio.full.cnt > 0, 1, 0),
           dpt.full.yn = ifelse(dpt.full.cnt > 0, 1, 0),
           zerodose.yn = ifelse(zerodose.cnt > 0, 1, 0)) %>%
    dplyr::select(caseid,
           undwgt.yn, stunt.cat2.yn, waste.cat2.yn, meas.full.yn, dpt.full.yn, polio.full.yn, zerodose.yn)


  # JOIN TOGETHER
  outcomes <- ir.out %>%
    base::merge(kr.out, by=c("caseid"), all.x=TRUE) %>%
    base::merge(br.out, by=c("caseid"), all.x=TRUE) 


  # look at fillrates
  fill_rates <- sapply(outcomes, function(x) sum(!is.na(x)) / length(x))
  
  fill_keep <- names(fill_rates[which(fill_rates > .5)])
  
  outcomes <- outcomes %>%  dplyr::select(all_of(fill_keep))
  
  # save data
  saveRDS(outcomes, file = "dataOut/1_MG_DHS_outcomes.rds")

