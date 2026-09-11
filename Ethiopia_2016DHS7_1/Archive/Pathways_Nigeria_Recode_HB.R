##################################################################################
## Importing and recoding Pathways Nigeria dataset                              ##
## Original dataset in SPSS with some formatting of variables                   ##
## Full dataset imports, but not all variables are recoded and used             ##
## This is a woman's recode file                                                ##
##                                                                              ##
## Last updated 10/12/2022 by E.D. Root                                         ##
##################################################################################

library(foreign)
library(dplyr)
library(tidyr)
library(data.table)
library(psych)
library(survey)
library(haven)

# Import dataset
# There are some errors thrown, but I've looked at the data and it doesn't seem to affect the data we're using
data<-read.spss("C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways\\rawdata\\Pathways Cache\\Final Data Sets\\Pathways main survey tool_Nigeria_2022_11_04_14_02 FULL DATA - MERGED.sav")
data<-as.data.frame(data)

data$ITYP<-as.numeric(data$ITYP)

# Unique ID
data$hhid<-paste(data$PSUclean,data$HHIDclean)
data$hhid<-gsub(" ", "", data$hhid, fixed=F)
data$caseid<-paste(data$PSUclean,data$HHIDclean,data$ITYP)
data$caseid<-gsub(" ", "", data$caseid, fixed=F)

# Create a dataset of only women
woman<-subset(data, data$ITYP==1)

# Creating an crosswalk for the old and new, recoded segments names
old<-c("RM2.1", "RM2.2", "RF3", "RM3", "RF4", "UM1", "UF2", "UM4")
new<-c("RM4.1-NN", "RM4.2-NN", "RF4-NN", "RM3-NN", "RF1-NN", "UM4-NN", "UF3-NN", "UM1-NN")
new2<-c("RM4-NN", "RM4-NN", "RF4-NN", "RM3-NN", "RF1-NN", "UM4-NN", "UF3-NN", "UM1-NN")
new.seg<-data.frame("cmb.seg"=old, "new.seg"=new, "new.comb"=new2)

# Create an index of of gender equality questions, make it into a z-score
woman$b011.num<-ifelse(woman$T_B01_1=="Completely disagree",5,
                    ifelse(woman$T_B01_1=="Disagree",4,
                           ifelse(woman$T_B01_1=="Neither agree nor disagree",3,
                                  ifelse(woman$T_B01_1=="Agree",2,1))))
woman$b011.num[is.na(woman$b011.num)] <- 3.5
woman$b013.num<-ifelse(woman$T_B01_3=="Completely disagree",5,
                    ifelse(woman$T_B01_3=="Disagree",4,
                           ifelse(woman$T_B01_3=="Neither agree nor disagree",3,
                                  ifelse(woman$T_B01_3=="Agree",2,1))))
woman$b015.num<-ifelse(woman$T_B01_5=="Completely disagree",5,
                    ifelse(woman$T_B01_5=="Disagree",4,
                           ifelse(woman$T_B01_5=="Neither agree nor disagree",3,
                                  ifelse(woman$T_B01_5=="Agree",2,1))))
woman$b016.num<-ifelse(woman$T_B01_6=="Completely disagree",5,
                    ifelse(woman$T_B01_6=="Disagree",4,
                           ifelse(woman$T_B01_6=="Neither agree nor disagree",3,
                                  ifelse(woman$T_B01_6=="Agree",2,1))))
woman$b017.num<-ifelse(woman$T_B01_7=="Completely disagree",5,
                    ifelse(woman$T_B01_7=="Disagree",4,
                           ifelse(woman$T_B01_7=="Neither agree nor disagree",3,
                                  ifelse(woman$T_B01_7=="Agree",2,1))))
woman$gen.role<-woman$b011.num+woman$b013.num+woman$b015.num+woman$b016.num+woman$b017.num
woman$gen.rolez<-as.numeric(scale(woman$gen.role))

woman$wom.money.dec<-ifelse(woman$b017.num > 3,1,0)
woman$wom.money.manjob<-ifelse(woman$b015.num >3,1,0)

# Make an index for barriers to health care, make it into a z-score
# This index creates a 3 tiered variable for barriers based on "big", "small" or "no" problem response
woman$c321.num<-ifelse(woman$T_C32_1=="Big problem ",3,
                    ifelse(woman$T_C32_1=="Small problem",2,1))
woman$c322.num<-ifelse(woman$T_C32_2=="Big problem ",3,
                    ifelse(woman$T_C32_2=="Small problem",2,1))
woman$c323.num<-ifelse(woman$T_C32_3=="Big problem ",3,
                    ifelse(woman$T_C32_3=="Small problem",2,1))
woman$c324.num<-ifelse(woman$T_C32_4=="Big problem ",3,
                    ifelse(woman$T_C32_4=="Small problem",2,1))
woman$bar.hc<-woman$c321.num+woman$c322.num+woman$c323.num+woman$c324.num
woman$bar.hcz<-as.numeric(scale(woman$bar.hc))

# Make an index for barriers to health care, make it into a z-score
# This index only counts a barrier when respondent says access is a "big problem"
woman$hc.permis<-woman$T_C32_1
woman$hc.permis.cat<-ifelse(woman$T_C32_1=="Big problem ",1,0)
woman$hc.cost<-woman$T_C32_2
woman$hc.cost.cat<-ifelse(woman$T_C32_2=="Big problem ",1,0)
woman$hc.cost.num<-ifelse(woman$T_C32_2=="Not a problem",0,
                          ifelse(woman$T_C32_2=="Small problem",1,2))
woman$hc.dist<-woman$T_C32_3
woman$hc.dist.cat<-ifelse(woman$T_C32_3=="Big problem ",1,0)
woman$hc.dist.num<-ifelse(woman$T_C32_3=="Not a problem",0,
                          ifelse(woman$T_C32_3=="Small problem",1,2))
woman$hc.alone<-woman$T_C32_4
woman$hc.alone.cat<-ifelse(woman$T_C32_4=="Big problem ",1,0)
woman$bar.hc2<-woman$hc.permis.cat+woman$hc.cost.cat+woman$hc.dist.cat+woman$hc.alone.cat
woman$bar.hcz2<-as.numeric(scale(woman$bar.hc))

# Create a travel time variable
woman$hc.travtime<-woman$C30
woman$hc.travtime.cat<-ifelse(woman$C30=="2 to 3 hours"|woman$C30==">3 hours",1,0)

# Index of birthing norms and z-score
# Adds 3 variables which ask about beliefs around birthing in a hosptial setting
woman$brth.med<-ifelse(woman$T_B02_3=="Completely disagree",5,
                    ifelse(woman$T_B02_3=="Disagree",4,
                           ifelse(woman$T_B02_3=="Neither agree nor disagree",3,
                                  ifelse(woman$T_B02_3=="Agree",2,1))))
woman$brth.hbsafe<-ifelse(woman$T_B02_5=="Completely disagree",5,
                       ifelse(woman$T_B02_5=="Disagree",4,
                              ifelse(woman$T_B02_5=="Neither agree nor disagree",3,
                                     ifelse(woman$T_B02_5=="Agree",2,1))))
woman$brth.hosp<-ifelse(woman$T_B02_6=="Completely disagree",5,
                     ifelse(woman$T_B02_6=="Disagree",4,
                            ifelse(woman$T_B02_6=="Neither agree nor disagree",3,
                                   ifelse(woman$T_B02_6=="Agree",2,1))))
woman$brth.norm<-woman$brth.med+woman$brth.hbsafe+woman$brth.hosp
woman$brth.normz<-as.numeric(scale(woman$brth.norm))

# Single, categorical indicator for beliefs about birthing in a hospital
woman$brth.hbsafe.cat<-ifelse(woman$T_B02_3=="Agree" | woman$T_B02_3=="Completely agree",1,0)
woman$brth.hosp.cat<-ifelse(woman$T_B02_6=="Agree" | woman$T_B02_3=="Completely agree",1,0)

# Categorical variable for whether a woman knew about pregnancy before becoming pregnant for the first time
woman$preg.know<-ifelse(woman$C14=="Yes",1,0)

# Index and z-score using questions about whether a woman was told she might need a higher level of intrapartum care
# Index 1 does not include ANC, index 2 does (rational is that needing more ANC isn't exclusively about high risk pregnancy)
woman$c191.num<-ifelse(woman$T_C19_1=="Yes",1,0); woman$c191.num[is.na(woman$c191.num)] <- 0
woman$told.brth.hosp<-ifelse(woman$T_C19_2=="Yes",1,0); woman$told.brth.hosp[is.na(woman$told.brth.hosp)] <- 0
woman$c193.num<-ifelse(woman$T_C19_3=="Yes",1,0); woman$c193.num[is.na(woman$c193.num)] <- 0
woman$risky.brth<-woman$c191.num+woman$told.brth.hosp+woman$c193.num
woman$risky.brthz<-as.numeric(scale(woman$risky.brth))

woman$c191.num<-ifelse(woman$T_C19_1=="Yes",1,0); woman$c191.num[is.na(woman$c191.num)] <- 0
woman$told.brth.hosp<-ifelse(woman$T_C19_2=="Yes",1,0); woman$told.brth.hosp[is.na(woman$told.brth.hosp)] <- 0
woman$c193.num<-ifelse(woman$T_C19_3=="Yes",1,0); woman$c193.num[is.na(woman$c193.num)] <- 0
woman$more.anc<-ifelse(woman$T_C19_4=="Yes",1,0); woman$more.anc[is.na(woman$more.anc)] <- 0
woman$risky.brth2<-woman$c191.num+woman$told.brth.hosp+woman$c193.num+woman$more.anc
woman$risky.brth2z<-as.numeric(scale(woman$risky.brth2))

# Preferred site of birth (created both a numeric and factor version)
# Two categorical variables since one option was "no plan"
woman$pref.home<-ifelse(woman$C22=="Home",1,0)
woman$pref.home2<-ifelse(woman$pref.home==0, "Facility","Home")
woman$no.plan<-ifelse(woman$C22=="Not planned",1,0)

# Trust in doctor, CHW, or either
# Could have an alternative coding for this with "Somewhat" and "A lot"
woman$trust.doc<-ifelse(woman$C33=="A lot",1,0)
woman$trust.chw<-ifelse(woman$C34=="A lot","Yes","No"); woman$trust.chw[is.na(woman$trust.chw)] <- "No CHW"; woman$trust.chw<-factor(woman$trust.chw, c("No","Yes","No CHW"))
woman$trust.prov<-ifelse(woman$C33=="A lot"|woman$C34=="A lot",1,0)

# Create a variable for ever delivered at home
# Version 1 is LinChiat's code
# S03 contain household roster variables; "7"=sons, "8"=daughters
# C21 variables ask place of delivery in a loop for each child in the HH roster
C21.set = c("T_C21_700", "T_C21_701"  , "T_C21_702"  , "T_C21_703", "T_C21_704"  , "T_C21_705", 
            "T_C21_706"  , "T_C21_707", "T_C21_708" , "T_C21_709",
            "T_C21_800", "T_C21_801"  , "T_C21_802"  , "T_C21_803", "T_C21_804"  , "T_C21_805", 
            "T_C21_806"  , "T_C21_807", "T_C21_808" , "T_C21_809")

#Initialize variable to 0, loop through and reassign to a 1 if any of C21 variables ="Home"
woman$dlvhm.ever = 0

for (i in 1:20) {
  woman$dlvhm.ever[woman[,C21.set[i]] == "Home"] = 1
}

#describe(woman$dlvhm.ever)

#Much uglier code that also codes write in (text) responses to place of delivery
woman$brth.700s<-ifelse(woman$C21_S_T_C21_700=="at home                                                                                                                 "|
                       woman$C21_S_T_C21_700=="house doctor                                                                                                            ",1,0)
woman$brth.701s<-ifelse(woman$C21_S_T_C21_701=="home                                                                                                                    "|
                       woman$C21_S_T_C21_701=="Herbal Home                                                                                                             "|
                       woman$C21_S_T_C21_701=="doctors home                                                                                                            ",1,0)
woman$brth.702s<-ifelse(woman$C21_S_T_C21_702=="home                                                                                                                    ",1,0)

woman$brth.800s<-ifelse(woman$C21_S_T_C21_800=="home                                                                                                                    "|
                       woman$C21_S_T_C21_800=="farm                                                                                                                    "|
                       woman$C21_S_T_C21_800=="in the car                                                                                                              "|
                       woman$C21_S_T_C21_800=="Iya Abiye                                                                                                               "|
                       woman$C21_S_T_C21_800=="nurse help                                                                                                              ",1,0)
woman$brth.801s<-ifelse(woman$C21_S_T_C21_801=="family doctors house                                                                                                    ",1,0)
woman$brth.802s<-ifelse(woman$C21_S_T_C21_802=="in a car                                                                                                                ",1,0)

woman$hb.700<-ifelse(woman$T_C21_700=="Home"|woman$T_C21_700=="On the way to the hospital"|woman$brth.700s==1,1,0); woman$hb.700[is.na(woman$hb.700)]<-0
woman$hb.701<-ifelse(woman$T_C21_701=="Home"|woman$T_C21_701=="On the way to the hospital"|woman$brth.701s==1,1,0); woman$hb.701[is.na(woman$hb.701)]<-0
woman$hb.702<-ifelse(woman$T_C21_702=="Home"|woman$T_C21_702=="On the way to the hospital"|woman$brth.702s==1,1,0); woman$hb.702[is.na(woman$hb.702)]<-0
woman$hb.703<-ifelse(woman$T_C21_703=="Home"|woman$T_C21_703=="On the way to the hospital",1,0); woman$hb.703[is.na(woman$hb.703)]<-0
woman$hb.704<-ifelse(woman$T_C21_704=="Home"|woman$T_C21_704=="On the way to the hospital",1,0); woman$hb.704[is.na(woman$hb.704)]<-0
woman$hb.705<-ifelse(woman$T_C21_705=="Home"|woman$T_C21_705=="On the way to the hospital",1,0); woman$hb.705[is.na(woman$hb.705)]<-0
woman$hb.706<-ifelse(woman$T_C21_706=="Home"|woman$T_C21_706=="On the way to the hospital",1,0); woman$hb.706[is.na(woman$hb.706)]<-0
woman$hb.800<-ifelse(woman$T_C21_800=="Home"|woman$T_C21_800=="On the way to the hospital"|woman$brth.800s,1,0); woman$hb.800[is.na(woman$hb.800)]<-0
woman$hb.801<-ifelse(woman$T_C21_801=="Home"|woman$T_C21_801=="On the way to the hospital"|woman$brth.801s,1,0); woman$hb.801[is.na(woman$hb.801)]<-0
woman$hb.802<-ifelse(woman$T_C21_802=="Home"|woman$T_C21_802=="On the way to the hospital"|woman$brth.802s,1,0); woman$hb.802[is.na(woman$hb.802)]<-0
woman$hb.803<-ifelse(woman$T_C21_803=="Home"|woman$T_C21_803=="On the way to the hospital",1,0); woman$hb.803[is.na(woman$hb.803)]<-0
woman$hb.804<-ifelse(woman$T_C21_804=="Home"|woman$T_C21_804=="On the way to the hospital",1,0); woman$hb.804[is.na(woman$hb.804)]<-0
woman$hb.805<-ifelse(woman$T_C21_805=="Home"|woman$T_C21_805=="On the way to the hospital",1,0); woman$hb.805[is.na(woman$hb.805)]<-0
woman$hb.806<-ifelse(woman$T_C21_806=="Home"|woman$T_C21_806=="On the way to the hospital",1,0); woman$hb.806[is.na(woman$hb.806)]<-0
woman$hb.807<-ifelse(woman$T_C21_807=="Home"|woman$T_C21_807=="On the way to the hospital",1,0); woman$hb.807[is.na(woman$hb.807)]<-0
woman$hb.808<-ifelse(woman$T_C21_808=="Home"|woman$T_C21_808=="On the way to the hospital",1,0); woman$hb.808[is.na(woman$hb.808)]<-0

woman$hb.ever<-ifelse(woman$hb.700==1|woman$hb.701==1|woman$hb.702==1|woman$hb.703==1|woman$hb.704==1|woman$hb.705==1|woman$hb.706==1|
                     woman$hb.800==1|woman$hb.801==1|woman$hb.802==1|woman$hb.803==1|woman$hb.804==1|woman$hb.805==1|woman$hb.806==1|woman$hb.807==1|woman$hb.808==1,1,0)
woman$hb.ever[is.na(woman$hb.ever)]<-0

# Location of delivery of youngest child:
# AGE OF CHILDREN
age.set = c("T_S03a_700", "T_S03a_701", "T_S03a_702", "T_S03a_703", "T_S03a_704", "T_S03a_705", "T_S03a_706", "T_S03a_707", "T_S03a_708",
            "T_S03a_800", "T_S03a_801", "T_S03a_802", "T_S03a_803", "T_S03a_804", "T_S03a_805", "T_S03a_806", "T_S03a_807", "T_S03a_808")

describe(woman[age.set])

tdat = woman[c("SbjNum", age.set, C21.set)]

# find age of children columns
names(tdat)

age.mat = tdat [2:19]
loc.mat = tdat [20:37]

# recode missing to 999 to match 
age.mat[is.na(age.mat)] = 999 

# find column with youngest child
id.kid = apply(age.mat, MARGIN = 1, FUN = which.min)
# create location of birth of youngest child:
tdat$C21.youngest.child = 99
tdat$C21.youngest.child = loc.mat[cbind(1:nrow(loc.mat), id.kid)]
 
woman$dlvhm.young = tdat$C21.youngest.child

woman$hb.young<-ifelse((woman$dlvhm.young=="Private hospital" | woman$dlvhm.young=="Public hospital"),0,1)

# Recoding birth outcomes - child death, sillbirth or miscarriage, abortion, and live births
woman$child.death<-ifelse(woman$C13a=="Yes",1,0)
woman$still.mis<-ifelse(woman$T_C13_2>0|woman$T_C13_4>0,1,0)
woman$still<-ifelse(woman$T_C13_2>0,1,0)
woman$abort<-ifelse(woman$T_C13_3>0,1,0)
woman$live.brth<-woman$T_C13_1; woman$live.brth<-replace(woman$live.brth, woman$live.brth==60,4) #Set an impossible value to the mean

# Variable for only having one child
woman$onechild<-ifelse(woman$live.brth==1,0,1)

#Age at major events - marriage/cohabitation, 1st birth and 1st pregnancy
woman$age.cohab<-woman$I_1_C13m_recode
woman$age.1stbrth<-woman$I_5_C13m_recode
woman$age.1stpreg<-woman$I_4_C13m_recode
woman$age.1stbirth.young<-ifelse(woman$age.1stbrth<18,1,0)

# Categorize these variables so that you don't drop Don't know or Never responses
# Also set reference categories
woman<- woman %>% mutate(age.cohab.cat = case_when(
  (I_1_C13m == 998) ~ "Never",
  (I_1_C13m > 0 & I_1_C13m < 15) ~ "5-14",
  (I_1_C13m >= 15 & I_1_C13m < 20) ~ "15-19",
  (I_1_C13m >= 20 & I_1_C13m < 990) ~ "20+",
  (I_1_C13m == 999) ~ "DNK"))
woman$age.cohab.cat<-factor(woman$age.cohab.cat, c("5-14", "15-19", "20+","Never", "DNK"))

woman<- woman %>% mutate(age.1stbrth.cat = case_when(
  (I_5_C13m == 998) ~ "Never",
  (I_5_C13m > 0 & I_5_C13m < 20) ~ "5-19",
  (I_5_C13m >= 20 & I_5_C13m < 30) ~ "20-29",
  (I_5_C13m >= 30 & I_5_C13m < 50) ~ "30+",
  (I_5_C13m > 990) ~ "DNK"))
woman$age.1stbrth.cat<-factor(woman$age.1stbrth.cat, c("5-19", "20-29", "30+","Never", "DNK"))

woman$age.bin.1brth<-ifelse(woman$I_5_C13m <15, "LT 15",
                         ifelse(woman$I_5_C13m >= 15 & woman$I_5_C13m <20, "15-19",
                                ifelse(woman$I_5_C13m >= 20 & woman$I_5_C13m <25, "20-24",
                                       ifelse(woman$I_5_C13m >= 25 & woman$I_5_C13m <30,"25-29",
                                              ifelse(woman$I_5_C13m >= 30 & woman$I_5_C13m <50,"30+",
                                                     ifelse(woman$I_5_C13m >=998,"Never", "DNK"))))))
woman$age.bin.1brth<-factor(woman$age.bin.1brth, c("20-24", "LT 15", "15-19", "25-29", "30+", "Never", "DNK"))

woman<- woman %>% mutate(age.1stpreg.cat = case_when(
  (I_4_C13m == 998) ~ "Never",
  (I_4_C13m > 0 & I_4_C13m < 15) ~ "5-14",
  (I_4_C13m >= 15 & I_4_C13m < 20) ~ "15-19",
  (I_4_C13m >= 20 & I_4_C13m < 50) ~ "20+",
  (I_4_C13m == 999) ~"DNK"))
woman$age.1stpreg.cat<-factor(woman$age.1stpreg.cat, c("5-14", "15-19", "20+","Never", "DNK"))

#Marital status
# Need to use 2 variables because if S03_01 or _02 were filled in, skipped marriage question
woman$mc1<-ifelse(woman$S03_O1=="Husband"|woman$S03_O1=="Partner or boyfriend"|woman$S03_O2=="Partner or boyfriend",1,0)
woman$mc2<-ifelse(woman$A01=="Yes -  currently married"|woman$A01=="Yes - have a boyfriend or partner",1,0)
woman$marrcohab<-ifelse(woman$mc1==1|woman$mc2==1,1,0)

#Education - categorized several different ways
woman$wom.ed.cat<-woman$E07a
woman$wom.ed.cat<-factor(woman$wom.ed.cat, c("No school","Primary","Secondary","Higher"))

woman$wom.ed.cat2<-ifelse((woman$E07a=="No school" | woman$E07a=="Primary"),"Low","High")
woman$wom.ed.cat2<-factor(woman$wom.ed.cat2, c("Low","High"))

# Actual number of years of education
woman$wom.ed.yrs<-woman$E07b; woman$wom.ed.yrs[is.na(woman$wom.ed.yrs)]<-0
# Number of years of partner education - lots of missings here
woman$part.ed.cat<-woman$E07c

# ANC care - categorized several different ways
# How many visits
woman$anc.visit<-woman$C17
woman$anc.visit.cat<-ifelse(woman$C17=="0 ","None",
                         ifelse(woman$C17=="1" |woman$C17=="2" |woman$C17=="3","1 to 3",
                                ifelse(woman$C17=="4 or more","4 or more","Don't know")))
woman$anc.visit.cat <- factor(woman$anc.visit.cat, c("None","1 to 3","4 or more","Don't know"))

# Month ANC began
woman$anc.month<-ifelse(woman$C18<4,"Month 1-3",
                     ifelse(woman$C18>3 & woman$C18<7,"Month 4-6","Month 7-9"))
woman$anc.month2<-ifelse((woman$C17=="0 "), "No ANC", woman$anc.month); woman$anc.month2<-ifelse((woman$anc.visit=="Don't know"), "DNK", woman$anc.month2)
woman$anc.month2 <- factor(woman$anc.month2, c("No ANC","Month 1-3","Month 4-6", "Month 7-9", "DNK"))

# If had 4+ visits or not
woman$anc.4plus<-ifelse(woman$C17=="4 or more",1,0)

# Had any ANC at all
woman$anyanc<-ifelse(woman$C17=="0 ","No","Yes")

# Received ANC in the first trimester
woman$anc.1sttri<-ifelse(woman$anc.month2=="Month 1-3", "Yes",
                      ifelse(woman$anc.month2=="DNK", "DNK","No"))
woman$anc.1sttri<-factor(woman$anc.1sttri, c("Yes","No","DNK"))

# Index and z-score for exposure to multi-media sources (TV, radio, computer, etc) 
woman$media.all<-woman$T_E34_1_recode+woman$T_E34_2_recode+woman$T_E34_3_recode+woman$T_E34_4_recode
woman$media.allz<-as.numeric(scale(woman$media.all))

# Smart phone ownership
# Code DNK as "no" because probably don't use the smartphone options
woman$smartphone<-ifelse(woman$IO20=="Smartphone",1,0); woman$smartphone[is.na(woman$smartphone)]<-0

# Index and z-score using 4 questions on community trust - higher number=more trust
woman$a101.num<-ifelse(woman$T_A10_1=="Never",1,
                    ifelse(woman$T_A10_1=="Rarely",2,
                           ifelse(woman$T_A10_1=="Half the time",3,
                                  ifelse(woman$T_A10_1=="Most of the time",4,5))))
woman$a102.num<-ifelse(woman$T_A10_2=="Never",1,
                    ifelse(woman$T_A10_2=="Rarely",2,
                           ifelse(woman$T_A10_2=="Half the time",3,
                                  ifelse(woman$T_A10_2=="Most of the time",4,5))))
woman$a103.num<-ifelse(woman$T_A10_3=="Never",5,
                    ifelse(woman$T_A10_3=="Rarely",4,
                           ifelse(woman$T_A10_3=="Half the time",3,
                                  ifelse(woman$T_A10_3=="Most of the time",2,1))))
woman$a104.num<-ifelse(woman$T_A10_4=="Never",5,
                    ifelse(woman$T_A10_4=="Rarely",4,
                           ifelse(woman$T_A10_4=="Half the time",3,
                                  ifelse(woman$T_A10_4=="Most of the time",2,1))))
woman$a105.num<-ifelse(woman$T_A10_5=="Never",5,
                    ifelse(woman$T_A10_5=="Rarely",4,
                           ifelse(woman$T_A10_5=="Half the time",3,
                                  ifelse(woman$T_A10_5=="Most of the time",2,1))))
woman$comm.trust<-woman$a101.num+woman$a102.num+woman$a103.num+woman$a104.num+woman$a105.num
woman$comm.trustz<-as.numeric(scale(woman$comm.trust))

# Index and z-score using 10 questions measuring trust in her partner - higher number=more trust
woman$a91.num<-ifelse(woman$T_A09_1_1=="Never",1,
                   ifelse(woman$T_A09_1_1=="Rarely",2,
                          ifelse(woman$T_A09_1_1=="Half the time",3,
                                 ifelse(woman$T_A09_1_1=="Most of the time",4,5))))
woman$a92.num<-ifelse(woman$T_A09_1_2=="Never",1,
                   ifelse(woman$T_A09_1_2=="Rarely",2,
                          ifelse(woman$T_A09_1_2=="Half the time",3,
                                 ifelse(woman$T_A09_1_2=="Most of the time",4,5))))
woman$a93.num<-ifelse(woman$T_A09_1_3=="Never",5,
                   ifelse(woman$T_A09_1_3=="Rarely",4,
                          ifelse(woman$T_A09_1_3=="Half the time",3,
                                 ifelse(woman$T_A09_1_3=="Most of the time",2,1))))
woman$a94.num<-ifelse(woman$T_A09_1_4=="Never",5,
                   ifelse(woman$T_A09_1_4=="Rarely",4,
                          ifelse(woman$T_A09_1_4=="Half the time",3,
                                 ifelse(woman$T_A09_1_4=="Most of the time",2,1))))
woman$a95.num<-ifelse(woman$T_A09_1_5=="Never",1,
                   ifelse(woman$T_A09_1_5=="Rarely",2,
                          ifelse(woman$T_A09_1_5=="Half the time",3,
                                 ifelse(woman$T_A09_1_5=="Most of the time",4,5))))
woman$a96.num<-ifelse(woman$T_A09_1_6=="Never",5,
                   ifelse(woman$T_A09_1_6=="Rarely",4,
                          ifelse(woman$T_A09_1_6=="Half the time",3,
                                 ifelse(woman$T_A09_1_6=="Most of the time",2,1))))
woman$a97.num<-ifelse(woman$T_A09_1_7=="Never",5,
                   ifelse(woman$T_A09_1_7=="Rarely",4,
                          ifelse(woman$T_A09_1_7=="Half the time",3,
                                 ifelse(woman$T_A09_1_7=="Most of the time",2,1))))
woman$a98.num<-ifelse(woman$T_A09_1_8=="Never",1,
                   ifelse(woman$T_A09_1_8=="Rarely",2,
                          ifelse(woman$T_A09_1_8=="Half the time",3,
                                 ifelse(woman$T_A09_1_8=="Most of the time",4,5))))
woman$a99.num<-ifelse(woman$T_A09_1_9=="Never",1,
                   ifelse(woman$T_A09_1_9=="Rarely",2,
                          ifelse(woman$T_A09_1_9=="Half the time",3,
                                 ifelse(woman$T_A09_1_9=="Most of the time",4,5))))
woman$a910.num<-ifelse(woman$T_A09_1_10=="Never",5,
                    ifelse(woman$T_A09_1_10=="Rarely",4,
                           ifelse(woman$T_A09_1_10=="Half the time",3,
                                  ifelse(woman$T_A09_1_10=="Most of the time",2,1))))
woman$part.trust<-woman$a91.num+woman$a92.num+woman$a93.num+woman$a94.num+woman$a95.num+woman$a96.num+woman$a97.num+woman$a98.num+woman$a99.num+woman$a910.num
woman$part.trustz<-as.numeric(scale(woman$part.trust))

# Made partner trust into qunitles and added a "no partner" category to minimize dropped observations
nlab<-c("Low","Low2","Mid","Mid-High","High")
woman$part.trust.quint<-cut(woman$part.trustz,breaks=5,labels=nlab); woman$part.trust.quint[woman$part.trust.quint=="Low2"]<-"Low"
woman$part.trust.quint<-as.character(woman$part.trust.quint)
woman$part.trust.quint[is.na(woman$part.trust.quint)]<-"No partner"
woman$part.trust.quint<-factor(woman$part.trust.quint, c("Mid","Low","Mid-High","High","No partner"))

# Women's decision making - who makes health care decisions and big housheold decisions 
woman$wom.dec.hlth<-ifelse(woman$A02a_O1=="Self",1,0); woman$wom.dec.hlth[is.na(woman$wom.dec.hlth)]<-0
woman$wom.dec.big<-ifelse(woman$A03b=="Self",1,0) # Lots of NAs in this, not sure why

# Does the woman have a partner living in the household
woman$part.in.hh<-ifelse(woman$Partner_in_HH=="Partner in HH",1,0); woman$part.in.hh[is.na(woman$part.in.hh)]<-0

# Does the woman's partner have multiple wives?
woman$mult.wives<-ifelse(woman$C10=="1 or 2" | woman$C10=="3 or 4",1,0); woman$mult.wives[is.na(woman$mult.wives)]<-0

# Does the woman say she has another form of social support aside from her partner?
woman$supp.out.hh<-ifelse(woman$A04a_O1=="Friend"|woman$A04a_O1=="Relative"|woman$A04a_O2=="Relative",1,0); woman$supp.out.hh[is.na(woman$supp.out.hh)]<-0

# Identified religion
woman$muslim<-ifelse(woman$E39_O1=="Muslim",1,0)
woman$catholic<-ifelse(woman$E39_O1=="Christian - Catholic",1,0)
woman$christian<-ifelse(woman$E39_O1=="Christian - Catholic" | woman$E39_O1=="Christian - Protestant" | woman$E39_O1=="Christian - Other",1,0)

# Index and z-score for domestic violence; higher=less acceptable
# This version codes a "don't know" answer as a middle category - e.g., ambivalent
woman$a131.num<-ifelse(woman$T_A13_1=="No",3,
                    ifelse(woman$T_A13_1=="Don't know",2,1))
woman$a132.num<-ifelse(woman$T_A13_2=="No",3,
                    ifelse(woman$T_A13_2=="Don't know",2,1))
woman$a133.num<-ifelse(woman$T_A13_3=="No",3,
                    ifelse(woman$T_A13_3=="Don't know",2,1))
woman$a134.num<-ifelse(woman$T_A13_4=="No",3,
                    ifelse(woman$T_A13_4=="Don't know",2,1))
woman$dv.notok<-woman$a131.num+woman$a132.num+woman$a133.num+woman$a134.num
woman$dv.notokz<-as.numeric(scale(woman$dv.notok))

# Modern contraceptive use ever
# Only asked of women <45 years old, 221 NAs
woman$c371<-ifelse(woman$C37_O1=="None of the above"|woman$C37_O1=="Standard days/Rhythm method"|woman$C37_O1=="Lactational amenorrhoea method"|woman$C37_O1=="Withdrawal method"|woman$C37_O1=="Other method, specify:",0,1)
woman$c372<-ifelse(woman$C37_O2=="None of the above"|woman$C37_O2=="Standard days/Rhythm method"|woman$C37_O2=="Lactational amenorrhoea method"|woman$C37_O2=="Withdrawal method"|woman$C37_O2=="Other method, specify:",0,1)
woman$c373<-ifelse(woman$C37_O3=="None of the above"|woman$C37_O3=="Standard days/Rhythm method"|woman$C37_O3=="Lactational amenorrhoea method"|woman$C37_O3=="Withdrawal method"|woman$C37_O3=="Other method, specify:",0,1)
woman$c374<-ifelse(woman$C37_O4=="None of the above"|woman$C37_O4=="Standard days/Rhythm method"|woman$C37_O4=="Lactational amenorrhoea method"|woman$C37_O4=="Withdrawal method"|woman$C37_O4=="Other method, specify:",0,1)
woman$fp.use.ever<-ifelse(woman$c371==1|woman$c372==1|woman$c373==1|woman$c374==1,1,0); woman$fp.use.ever[is.na(woman$fp.use.ever)]<-0

# Current use of modern contraception
# Need to create a category for currently pregnant so don't drop records
woman$C361<-ifelse(woman$C36_O1=="No method"|woman$C36_O1=="Standard days/Rhythm method"|woman$C36_O1=="Lactational amenorrhoea method"|woman$C36_O1=="Withdrawal method"|woman$C36_O1=="Other method, specify:",0,1)
woman$C362<-ifelse(woman$C36_O2=="No method"|woman$C36_O2=="Standard days/Rhythm method"|woman$C36_O2=="Lactational amenorrhoea method"|woman$C36_O2=="Withdrawal method"|woman$C36_O2=="Other method, specify:",0,1)
woman$C363<-ifelse(woman$C36_O3=="No method"|woman$C36_O3=="Standard days/Rhythm method"|woman$C36_O3=="Lactational amenorrhoea method"|woman$C36_O3=="Withdrawal method"|woman$C36_O3=="Other method, specify:",0,1)
woman$fp.use.now<-ifelse(woman$C361==1|woman$C362==1|woman$C363==1,"Yes","No")
woman$fp.use.now<-ifelse(woman$C35=="Currently pregnant", "Currently Pregnant",woman$fp.use.now); woman$fp.use.now[is.na(woman$fp.use.now)]<-"No"
woman$fp.use.now<-factor(woman$fp.use.now, c("No","Yes","Currently Pregnant"))
# Create a simple yes/no version
woman$fp.use.now2<-ifelse(woman$fp.use.now=="Yes","Yes","No")

# LARC use
woman$larc1<-ifelse(woman$C37_O1=="PPIUCD{0}"|woman$C37_O1=="Injectables"|woman$C37_O1=="Implants",1,0)
woman$larc2<-ifelse(woman$C37_O2=="PPIUCD{0}"|woman$C37_O2=="Injectables"|woman$C37_O2=="Implants",1,0)
woman$larc3<-ifelse(woman$C37_O3=="PPIUCD{0}"|woman$C37_O3=="Injectables"|woman$C37_O3=="Implants",1,0)
woman$larc4<-ifelse(woman$C37_O4=="PPIUCD{0}"|woman$C37_O4=="Injectables"|woman$C37_O4=="Implants",1,0)
woman$fp.larc.ever<-ifelse(woman$larc1==1|woman$larc2==1|woman$larc3==1|woman$larc4==1,1,0); woman$fp.larc.ever[is.na(woman$fp.larc.ever)]<-0

# Index and z-score for 4 variables that ask about family planning perceptions
woman$fp.preg<-ifelse(woman$T_B02_1=="Completely disagree",5,
                   ifelse(woman$T_B02_3=="Disagree",4,
                          ifelse(woman$T_B02_3=="Neither agree nor disagree",3,
                                 ifelse(woman$T_B02_3=="Agree",2,1))))
woman$fp.cond<-ifelse(woman$T_B02_2=="Completely disagree",5,
                   ifelse(woman$T_B02_5=="Disagree",4,
                          ifelse(woman$T_B02_5=="Neither agree nor disagree",3,
                                 ifelse(woman$T_B02_5=="Agree",2,1))))
woman$fp.nosex<-ifelse(woman$T_B02_7=="Completely disagree",5,
                    ifelse(woman$T_B02_6=="Disagree",4,
                           ifelse(woman$T_B02_6=="Neither agree nor disagree",3,
                                  ifelse(woman$T_B02_6=="Agree",2,1))))
woman$fp.probs<-ifelse(woman$T_B02_8=="Completely disagree",5,
                    ifelse(woman$T_B02_6=="Disagree",4,
                           ifelse(woman$T_B02_6=="Neither agree nor disagree",3,
                                  ifelse(woman$T_B02_6=="Agree",2,1))))
woman$fp.norm<-woman$fp.preg+woman$fp.cond+woman$fp.nosex+woman$fp.probs
woman$fp.normz<-as.numeric(scale(woman$brth.norm))

# Avoid pregnancy
woman$avoid.preg<-ifelse(woman$C35=="Yes",1,0); woman$avoid.preg[is.na(woman$avoid.preg)]<-0

# Use formal HC system when ill
# First version looks at both doctors and health facility
# Second version only looks at health facilities
woman$c281<-ifelse(woman$C28_1_recode=="Private doctor",1,0)
woman$c282<-ifelse(woman$C28_2_recode=="Government Health facility",1,0)
woman$use.hc.sick<-ifelse(woman$c281==1|woman$c282==1,1,0); woman$use.hc.sick[is.na(woman$use.hc.sick)]<-0
woman$use.hc.sick2<-ifelse(woman$c282==1,1,0); woman$use.hc.sick[is.na(woman$use.hc.sick)]<-0

# Use of formal HC system for child when ill
woman$c251<-ifelse(woman$C25_1_recode=="Private doctor",1,0)
woman$c252<-ifelse(woman$C25_2_recode=="Government Health Facility",1,0)
woman$use.hc.child<-ifelse(woman$c251==1|woman$c252==1,1,0); woman$use.hc.child[is.na(woman$use.hc.child)]<-0

#HC Utilization index, similar to Navideh's
woman$anyanc2<-ifelse(woman$anyanc=="No",0,1); woman$anyanc2[is.na(woman$anyanc2)]<-0
woman$hc.utilize<-woman$anyanc2+woman$use.hc.sick+woman$use.hc.child+woman$fp.use.ever

# Maybe(?) a measure of willingness to delay care seeking
woman$use.hc.hisick<-ifelse(woman$B06=="{0} will wait for more symptoms to show up before going to the doctor as cough and cold is not severe enough.",1,0)

# Did the woman live with her biological parents
woman$lived.bio.par<-ifelse(woman$E01_recode=="Someone else (NOT bio parent)",0,1)

# How safe did she feel when she was a child?
woman$feel.safe<-ifelse(woman$E04=="Always"|woman$E04=="Most of the time",1,0)

# Economics
woman$wom.savgrp<-ifelse(woman$E28=="Yes",1,0); woman$wom.savgrp[is.na(woman$wom.savgrp)]<-0
woman$hh.trans<-ifelse(woman$T_E35_2=="Yes"|woman$T_E35_3=="Yes"|woman$T_E35_4=="Yes",1,0)
woman$rural<-ifelse(woman$URBAN_RURA=="R  ",1,0)

# Digital
woman$hh.phone<-ifelse(woman$IO19=="No one in household",0,1)
woman <- woman %>% mutate(hh.phonetype = case_when(
  IO20_IO19_recode=="smartphone" ~ "Smartphone",
  IO20_IO19_recode=="cell phone" ~ "Mobile only",
  IO20_IO19_recode=="Don't know" | IO20_IO19_recode=="No phone" | IO20_IO19_recode=="99" ~ "No phone"))
woman$wom.phone<-ifelse(woman$IO19=="Owns cell phone alone "|woman$IO19=="Owns both shared and personal cell phones",1,0)
woman<- woman %>% mutate(wom.phonetype = case_when(
  wom.phone==1 & IO20=="Smartphone" ~ "Smartphone",
  wom.phone==1 & IO20=="Cellular phone only" ~ "Mobile only",
  wom.phone==0 | (wom.phone==1 & IO20=="Don't know")~ "No phone"))

woman$hh.bank.acct<-ifelse(woman$E25=="No one in household has bank account" | woman$E25=="Don't know",0,1)
woman$wom.bank.acct<-ifelse(woman$E25=="Has own bank account alone " | woman$E25=="Has both joint and separate accounts",1,0)

woman$hh.mobbank<-ifelse(woman$E25a=="No one in household has account" | woman$E25a=="Don't know",0,1)
woman$wom.mobbank<-ifelse(woman$E25a=="Has own account" | woman$E25a=="Has both joint and separate accounts",1,0)

woman$use.internet<-ifelse(woman$T_E34_1=="Not at all",0,1)
woman$use.internet.freq<-woman$T_E34_1

E08.set = grep("E08_", names(woman), value=TRUE)
woman$wom.work = 0
for (i in 1:230) {
  woman$wom.work[woman[,E08.set[i]] == "Self"] = 1
}

# 0=does not work, 1=thoughout year, 2=seasonally, 3=when available
woman$wom.work.type<-ifelse(woman$wom.work==0, 0, woman$T_E09_100)
woman$wom.work.full<-ifelse(woman$wom.work.type==1,1,0)

# Climate loss
woman$climate.loss<-ifelse(woman$D10=="Lost everything" | woman$D10=="Lost almost everything",3,
                        ifelse(woman$D10=="Lost half",2,
                               ifelse(woman$D10=="Lost less than half",1,0)))

#Wealth index based on assets, following DHS welth index method
woman$land<-ifelse(woman$E21=="No one in household",0,1)
woman$elec<-ifelse(woman$E23=="Yes",1,0)
woman$bing<-ifelse(woman$T_E35_1=="Yes",1,0)
woman$scoot<-ifelse(woman$T_E35_2=="Yes",1,0)
woman$car<-ifelse(woman$T_E35_3=="Yes",1,0)
woman$boat<-ifelse(woman$T_E35_4=="Yes",1,0)
woman$rooms<-ifelse(woman$IO16=="1 room",1,
                 ifelse(woman$IO16=="2 rooms",2,
                        ifelse(woman$IO16=="3 rooms",3,
                               ifelse(woman$IO16=="4 rooms",4,5))))
woman$flusht<-ifelse(woman$IO21=="Flush toilet",1,0)
woman$pittoi<-ifelse(woman$IO21=="Pit latrine",1,0)
woman$notoi<-ifelse(woman$IO21=="No facility/bush/field",1,0)
woman$bucktoi<-ifelse(woman$IO21=="Bucket in house",1,0)
woman$toishare<-ifelse(woman$IO22=="Yes",1,0); woman$toishare[is.na(woman$toishare)]<-0
woman$watpipe<-ifelse(woman$IO26=="Yes, piped water available",1,0)
woman$roofno<-ifelse(woman$IO5=="No roof",1,0)
woman$roofnat<-ifelse(woman$IO5=="Thatch/palm leaf"|woman$IO5=="Sod"|woman$IO5=="Other, specify:",1,0)
woman$roofrud<-ifelse(woman$IO5=="Rustic mat"|woman$IO5=="Palm/bamboo"|woman$IO5=="Wood planks"|woman$IO5=="Cardboard",1,0)
woman$rooffin<-ifelse(woman$IO5=="Metal / iron sheets"|woman$IO5=="Wood"|woman$IO5=="Calamine/cement fiber"|woman$IO5=="Ceramic tiles"|woman$IO5=="Cement"|woman$IO5=="Roofing shingles",1,0)
woman$wallno<-ifelse(woman$IO6=="No walls",1,0)
woman$wallnat<-ifelse(woman$IO6=="Cane/Palm/Trunks"|woman$IO6=="Dirt / mud"|woman$IO6=="Other, specify:",1,0)
woman$wallrud<-ifelse(woman$IO6=="Bamboo with mud"|woman$IO6=="Stone with mud"|woman$IO6=="Uncovered adobe"|woman$IO6=="Plywood"|woman$IO6=="Cardboard"|woman$IO6=="Reused wood",1,0)
woman$wallfin<-ifelse(woman$IO6=="Cement"|woman$IO6=="Stone with lime/cement"|woman$IO6=="Bricks"|woman$IO6=="Cement blocks"|woman$IO6=="Covered adobe"|woman$IO6=="Wood plank/shingles"|woman$IO6=="Metal/ iron sheets",1,0)
woman$sewerno<-ifelse(woman$IO9=="No drainage",1,0)
woman$seweropu<-ifelse(woman$IO9=="Open, but uncemented",1,0)
woman$seweropc<-ifelse(woman$IO9=="Open, but cemented",1,0)
woman$sewercl<-ifelse(woman$IO9=="Closed sewer",1,0)
woman$ownhome<-ifelse(woman$E16=="Own home",1,0)
woman$hungry<-ifelse(woman$T_A11_1=="Yes",1,0)
woman$reloc<-ifelse(woman$T_A11_3=="Yes",1,0)
woman$kidwrk<-ifelse(woman$T_A11_5=="Yes",1,0)

wealth<-woman %>% select(c("land","elec","hh.phone","bing","scoot","car","boat","rooms","flusht","pittoi","notoi","bucktoi","toishare","watpipe","roofno","roofnat","roofrud","rooffin",
                        "wallno","wallnat","wallrud","wallfin","sewerno","seweropu","seweropc","sewercl","ownhome"))
#gg_miss_var(wealth, show_pct = TRUE)
which(colSums(is.na(wealth))>0)

prn<-principal(wealth, rotate="varimax", nfactors=3,covar=T, eps=1e-14, scores=TRUE)
tab<-as.data.frame(prn$scores)
woman$wealth.index<-tab$RC1
nlab<-c("Low","Low-Mid","Mid","Mid-High","High")
woman$wealth.quint<-cut(woman$wealth.index,breaks=5,labels=nlab)

hist(woman$wealth.index)

woman$chronic.ill<-ifelse(woman$T_A11_4=="Yes",1,0)
woman<- woman %>% mutate(hc.travtime.num = case_when(
  C30 == "<15 minutes" ~ 1,
  C30 == "16 to 30 minutes" ~ 2,
  C30 == "31 to 60 minutes" ~ 3,
  C30 == "61 to 120 minutes" ~ 4,
  C30 == "2 to 3 hours" ~ 5,
  C30 == ">3 hours" ~ 6,
  C30 == "Don't know" ~ NA))


# Create clean dataset with newly recoded variables and segment, survey weights, etc.
nigeria <- woman %>% select(c("HHIDclean","PSUclean","caseid", "STATE", "cmb.seg","base.wt", "dlvhm.ever","hb.ever","dlvhm.young","gen.role","gen.rolez", "hc.permis","hc.cost","hc.dist","hc.alone",
                           "hc.travtime","bar.hc","bar.hcz","brth.hbsafe","brth.norm","brth.normz","told.brth.hosp","hc.travtime.cat", "part.in.hh", "fp.use.now", "fp.use.now2","age.bin.1brth",
                           "risky.brth","risky.brthz","risky.brth2", "risky.brth2z", "more.anc","pref.home","no.plan", "trust.doc","trust.prov","child.death","still.mis","abort","live.brth","preg.know","num.hh.members",
                           "num.hh.females","total_sons_daughters","TotalchildrenU5","age.cohab","age.1stbrth","age.1stpreg","age.cohab.cat","age.1stbrth.cat","age.1stpreg.cat","wom.ed.cat","wom.ed.yrs",
                           "part.ed.cat","marrcohab","age.resp","anc.visit","anc.month","anc.4plus", "media.all", "media.allz", "supp.out.hh", "anc.month2", "avoid.preg", "fp.use.now", "use.hc.sick",
                           "comm.trust","comm.trustz","part.trust","part.trustz","Partner_in_HH","C41_preg_rej_partner","wom.dec.hlth","wom.dec.big","muslim","dv.notok","dv.notokz","wom.work",
                           "hh.bank.acct","hh.phone","wom.phone","wom.savgrp","hh.trans","rural","feel.safe","lived.bio.par","fp.use.ever","wealth.index","wealth.quint", "pref.home2",
                           "hc.cost.cat","hc.alone.cat","hc.permis.cat","hc.dist.cat","anc.visit.cat","onechild","bar.hcz2","num.hh.females","trust.chw","mult.wives","anyanc","part.trust.quint",
                           "inhh.mil","inhh.mother","brth.hbsafe.cat","brth.hosp.cat","use.hc.child","anc.1sttri","use.hc.hisick","use.hc.sick2","fp.normz", "wom.ed.cat2", "fp.larc.ever", "wom.work.type",
                           "wom.money.manjob", "wom.money.dec", "wom.work.full", "hb.young", "hh.phonetype","wom.phonetype", "elec", "age.1stbirth.young", "still",
                           "wom.bank.acct", "hh.mobbank","wom.mobbank","use.internet","use.internet.freq", "IO20", "hungry", "reloc",
                           "chronic.ill", "hc.travtime.num", "climate.loss", "supp.out.hh","kidwrk", "hc.cost.num","hc.dist.num","hc.utilize"))

# Merge in the new segment recodes created at the beginning
nigeria$cmb.seg<-gsub(" ", "", woman$cmb.seg, fixed = TRUE)
nigeria<-merge(nigeria, new.seg, by="cmb.seg",all.x=T)

#nigeria<-subset(nigeria, STATE %in% c("BAUCHI   ",	"BORNO    ",	"GOMBE    ",	"KADUNA   ",	
#                                      "KANO     ",	"NASARAWA ",	"NIGER    ",	"SOKOTO   ",	"YOBE     "))

options(survey.lonely.psu="adjust")

# "RF2","RF4","RM3","RM2","RM4","UF2","UF4","UM1","UM4"
clust="UM4-NN"

prop.table(svytable(~hb.young+new.comb, Pathdesign), margin=2)*100

svyquantile(~age.resp,  subset(Pathdesign, new.seg == clust), quantiles = c(0.25, 0.5, 0.75))

# Distance REACH
data.frame(svytable(~hc.dist.cat + new.seg, Pathdesign)) %>%  
  group_by(new.seg) %>%
  mutate(percent = round(Freq/sum(Freq)*100,2)) %>% 
  #  filter(Freq>0) %>%
  print(n=Inf)

#Ladder plot  for predicted probability of home births
options(survey.lonely.psu="adjust")
svy.glm<-svyglm(hb.ever~new.seg,family=quasibinomial,design=Pathdesign, data=woman)

pred.data<-data.frame(new.seg=c("RF4-NN", "RM4.1-NN", "RM4.2-NN","UM4-NN", "RM3-NN", "UF3-NN","RF1-NN", "UM1-NN"))
pred.data3<-data.frame(type=c("most" ,"most", "most" ,"most", "more" ,"more", "least", "least"))

# Plot extended segments
probabilities <- predict(svy.glm, pred.data, type = "response", se.fit=T)
prob1<-as.data.frame(probabilities)
prob1$lower<- prob1$response - (1.96*prob1$SE)
prob1$upper<- prob1$response + (1.96*prob1$SE)
prob1$segX4<- pred.data$new.seg
prob1$type<-pred.data3$type

prob1 <- prob1 %>%
  mutate(segX4 = fct_relevel(segX4, 
                             "RF4-NN", "RM4.1-NN", "RM4.2-NN","UM4-NN", "RM3-NN", "UF3-NN","RF1-NN", "UM1-NN"))

cbPalette <- c("#14BDCC", "#6F55F2", "#F26185")
ggplot(prob1, aes(x=segX4, y=response)) + 
  geom_point(aes(colour=type), alpha=0.9, size=5, show.legend = FALSE) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=type), size=1, width=.5, show.legend = FALSE) +
  geom_text(aes(y=response, x=segX4, label=round(response,2)), vjust=-1.2, hjust=.5) +
  coord_flip()+
  xlab("") +
  ylab("Probability of Home Birth") +
  scale_y_continuous(limits = c(.2, 1), breaks = seq(.2, 1, .2))+
  scale_colour_manual(values=cbPalette) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

options(survey.lonely.psu="adjust")
proportions(svytable(~wealth.quint+new.seg, Pathdesign), margin=2)
svyby(~wealth.index, ~new.seg, Pathdesign, svymean, na.rm=T)


svy.glm1 <-svyglm(hb.ever~new.comb, family=quasibinomial,design=Pathdesign, data=nigeria)
svy.glm2 <-svyglm(hb.ever~new.comb+hc.dist.num+hc.cost.num+wom.dec.hlth+hc.utilize,family=quasibinomial,design=Pathdesign,data=nigeria) 

pred.data1<-data.frame(new.comb=c("RM4-NN",
                              "RF4-NN",
                              "UM4-NN",
                              "RM3-NN",
                              "UF3-NN",
                              "RF1-NN",
                              "UM1-NN"))

pred.data2<-data.frame(new.comb=c("RM4-NN",
                               "RF4-NN",
                               "UM4-NN",
                               "RM3-NN",
                               "UF3-NN",
                               "RF1-NN",
                               "UM1-NN"),
                       hc.dist.num=0, hc.cost.num=2, hc.utilize=3, wom.dec.hlth=0)
pred.data3<-data.frame(type=c("most", "most", "most","more","more","least","least"))
                  
# Plot extended segments
probabilities1 <- predict(svy.glm1, pred.data1, type = "response", se.fit=T)
prob1<-as.data.frame(probabilities1)
prob1$lower<- prob1$response - (1.96*prob1$SE)
prob1$upper<- prob1$response + (1.96*prob1$SE)
prob1$segX4<- pred.data1$new.comb
prob1$type<-pred.data3$type

prob1$segX4 <- factor(prob1$segX4, levels=c("RM4-NN","RF4-NN","UM4-NN","RM3-NN","UF3-NN","RF1-NN","UM1-NN"))

# Plot extended segments
probabilities2 <- predict(svy.glm2, pred.data2, type = "response", se.fit=T)
prob2<-as.data.frame(probabilities2)
prob2$lower<- prob2$response - (1.96*prob2$SE)
prob2$upper<- prob2$response + (1.96*prob2$SE)
prob2$segX4<- pred.data2$new.comb
prob2$type<-pred.data3$type

prob <- rbind(prob1,prob2)
prob$segX4 <- factor(prob$segX4, levels=c("RM4-NN","RF4-NN","UM4-NN","RM3-NN","UF3-NN","RF1-NN","UM1-NN"))
prob$type2 <- c(c(rep("Unadjusted",7)),c(rep("Adjusted",7)))
prob$type2 <- factor(prob$type2, levels=c("Unadjusted","Adjusted"))

theme_set(theme_bw(30))
cbPalette <- c("#14BDCC", "#6F55F2", "#F26185")
ggplot(prob, aes(x=segX4, y=response,group=type2)) + 
  geom_point(aes(colour=type,shape=type2),position=position_dodge(width=0.3), alpha=0.9, size=5, show.legend = FALSE) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=type,linetype =type2), position=position_dodge(width=0.3),size=1, width=.5) +
  geom_text(aes(y=response, x=segX4, label=signif(response,2)), position=position_dodge(width=0.6), vjust=-1.2, hjust=.5,size=6) +
  coord_flip()+
  xlab("") +
  scale_y_continuous(name = "Probability of Stunting")+
  scale_colour_manual(values=cbPalette) +
  ggtitle("")+
  guides(linetype=guide_legend(title=NULL),colour=FALSE,fill=FALSE)+
  theme(legend.position=c(0.8,0.8))
