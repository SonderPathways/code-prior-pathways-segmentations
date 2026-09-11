#######################       KENYA_DHS22_Outcomes       #######################

# Purpose: Define outcomes for vulnerability analysis
# Authors: Helena, Sarah, Amanda, Andrea, Bih
# Input files: KEBR8BFL.DTA, KEHR8BFL.DTA, KEIR8BFL.DTA (Update with rds files)
# Last update:  Oct 22: SS merged updates from review of variables and response options
# Output files: "1_Kenya_DHS_outcomes_2Nov2023.rds", "1_Kenya_DHS_outcomes_2Nov2023.RData"
# Ancestor scripts: None
# Descendant scripts: "5_Kenya_DHS_exploratory.R"
# Note: the working directory needs to be changed when using scripts (and any scripts called upon)


#_______________________________________________________________________________

# Clearing the workspace -------------------------------------------------------


rm(list=ls())

# Load libraries ---------------------------------------------------------------
pacman::p_load("foreign", "stringr", "magrittr", "reshape2", "data.table", "psych", "vtable", "corrplot", "haven", "survey", "ggplot2", "dplyr", "tidyr")

# Set working directory---------------------------------------------------------

# Change username, e.g. type "Andrea"

user = "Andrea"


if (tolower(user) == "helena") {
  rootdir <- "C:/Users/hmang/UW START Dropbox/Helena Manguerra/"
} else if (tolower(user) == "mac") {
  rootdir <- "~/UW START Dropbox/Helena Manguerra/"
} else if (tolower(user) == "amanda") {
  rootdir <- "C:/Users/manda/UW START Dropbox/Amanda Brumwell/"
} else if (tolower(user) == "andrea") {
  rootdir <- "C:/Users/Andrea Rivas/UW START Dropbox/Andrea Rivas/"
} else if (tolower(user) == "bih") {
  rootdir <- "C:/Users/bihmo/UW START Dropbox/Bih Epse Cuba Moki Suh/"
} else if (tolower(user) == "sarah") {
  rootdir <- "C:/Users/sshaw/UW START Dropbox/Sarah Shaw/"
} else {
  print("No filepath")
}

setwd(file.path(rootdir, "START_229_Gender Equity/Working Files"))

# Welcome

message("Hola", user, "!")

# Check if the working directory is the correct one

message("Your working is:", getwd())

#_______________________________________________________________________________

#Keep in case the last section does not work

#setwd("C:/Users/Andrea Rivas/UW START Dropbox/Andrea Rivas/START_229_Gender Equity/Working Files")
#setwd("C:/Users/Admin/UW START Dropbox/Helena Manguerra/START_229_Gender Equity/Working Files/")
#setwd("C:/Users/sshaw/UW START Dropbox/Sarah Shaw/START_229_Gender Equity/Working Files/") # for Sarah's use

#_______________________________________________________________________________

# Load Kenya DHS data ----------------------------------------------------------

BR <- readRDS("Data/Kenya DHS/KE_2022/KEBR8BDT/KEBR8BFL.rds")
KR <- readRDS("Data/Kenya DHS/KE_2022/KEKR8BDT/KEKR8BFL.rds")
# HH <- readRDS("Data/Kenya DHS/KE_2022/KEHR8BDT/KEHR8BFL.rds") #note the household survey is not used 
IR <- readRDS("Data/Kenya DHS/KE_2022/KEIR8BDT/KEIR8BFL.rds") 


# Potential header for variables -----------------------------------------------

#################################### IR FILE ###################################

# Still birth ------------------------------------------------------------------

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

IR[,str:=substr(vcal_1,v018,v018+59)]
IR[,STL:=ifelse(grepl('TPPPPPP',str),1,NA)]
IR[,LB:=ifelse(grepl('BPPPPPPP',str),1,NA)]

dim(IR)

# some <- names(IR)[c(1:38,5900:5905)]

# Note: None of these variables are included later in the analysis - not sure what this code is for?
some = c(
  "caseid",    "v000",      "v001",     "v002",      "v003",      "v004",     
  "v005",      "v006",      "v007",      "v008",      "v008a",     "v009",     
  "v010",      "v011",      "v012",      "v013",      "v014",      "v015",     
  "v016",      "v017",      "v018",      "v019",      "v019a",     "v020",     
  "v021",      "v022",      "v023",      "v024",      "v025",      "v026",     
  "v027",     "v028",      "v029",     "v030",      "v031",      "v032",   
  "v034",      "v040",      
  # "sm508va_4", "sm508va_5", "sm508va_6", ## HPM: These vars used in Ethiopia are missing in Kenya, don't include 
  "str",      "STL",       "LB"  
  
)

# Check if variables exist in the dataset --------------------------------------

some %in% names(IR)

stl <- getStillbirths(IR)
stl[,id:=1:dim(stl)[1]]
stl[,start:=str_locate_all(str,"TPPPPPP")[[1]][1],by=c("id")]


# MomAgeSTL (Adapted from Ethiopia code)

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
stl <- rbind(stl,sub[,-c("rep")])

setkey(stl,index)

stl[alive=="STL",stl:=1]
stl[is.na(stl),stl:=0]

stl.out <- stl %>% group_by(caseid) %>% dplyr::summarize(stl.cnt=sum(stl, na.rm=T))


# Recode family planning -------------------------------------------------------

  #* Currently using a modern method -------------------------------------------
  
IR$fp.mod.now <- ifelse(IR$v313 == "modern method",1,0) 

  #* Last method discontinued in last 5 years ----------------------------------

IR$fp.dis.5yr<-ifelse((IR$v359=="pill"|IR$v359=="iud"|IR$v359=="injections"|IR$v359=="male condom"|
                           IR$v359=="implants/norplant"|IR$v359=="female condom"|IR$v359=="emergency contraception"|
                           IR$v359=="other modern method"),1,0) 
IR$fp.dis.5yr[is.na(IR$fp.dis.5yr)] <- 0

  #* Ever used a modern method -------------------------------------------------

IR$fp.mod.ever<-ifelse((IR$fp.mod.now==1 | IR$fp.dis.5yr==1),1,0) #included in data dictionary 


  #* Never used a modern method ------------------------------------------------

IR$fp.mod.never<-ifelse((IR$fp.mod.now==1 | IR$fp.dis.5yr==1),0,1)


# Recode home births -----------------------------------------------------------

IR[,hb.1:= ifelse(m15_1== "home" | m15_1== "respondent's home" | m15_1== "other home" | m15_1== "other",1,0)]
IR[,hb.2:= ifelse(m15_2== "home" | m15_2== "respondent's home" | m15_2== "other home" | m15_2== "other",1,0)]
IR[,hb.3:= ifelse(m15_3== "home" | m15_3== "respondent's home" | m15_3== "other home" | m15_3== "other",1,0)]
IR[,hb.4:= ifelse(m15_4== "home" | m15_4== "respondent's home" | m15_4== "other home" | m15_4== "other",1,0)]
IR[,hb.5:= ifelse(m15_5== "home" | m15_5== "respondent's home" | m15_5== "other home" | m15_5== "other",1,0)]

IR$hb.ever[(IR$hb.1==1 | IR$hb.2==1 | IR$hb.3==1 | IR$hb.4==1 | IR$hb.5==1)]<- 1 
IR$hb.ever[(IR$hb.1==0 & IR$hb.2==0 & IR$hb.3==0 & IR$hb.4==0 & IR$hb.5==0)]<-0
IR$hb.ever <- ifelse((is.na(IR$hb.1))&(is.na(IR$hb.2))&(is.na(IR$hb.3))&(is.na(IR$hb.4))&(is.na(IR$hb.5)),NA,IR$hb.ever)

# Women's weight ---------------------------------------------------------------

IR$bmi<-IR$v445/100
IR$bmi<-ifelse(IR$bmi>90,NA,IR$bmi)
IR<- IR %>% mutate(bmi.cat1 = case_when(
  (bmi < 16) ~ "Severely thin",
  (bmi >= 16 & bmi < 17) ~ "Moderately thin",
  (bmi >= 17 & bmi < 18.5) ~ "Mildly thin",
  (bmi >= 18.5 & bmi < 25) ~ "Normal",
  (bmi >= 25 & bmi < 30) ~ "Overweight",
  (bmi >= 30) ~ "Obese"))
IR$bmi.cat1<-factor(IR$bmi.cat1, c("Severely thin", "Moderately thin", "Mildly thin","Normal", "Overweight","Obese"))
IR<- IR %>% mutate(bmi.cat2 = case_when(
  (bmi < 18.5) ~ "Underweight",
  (bmi >= 18.5 & bmi < 25) ~ "Normal",
  (bmi >= 25 & bmi < 30) ~ "Overweight",
  (bmi >= 30) ~ "Obese"))
IR$bmi.cat2<-factor(IR$bmi.cat2, c("Underweight","Normal", "Overweight","Obese"))

# Number of ANC visits ---------------------------------------------------------

  #* Timing of 1st antenatal check (months) ------------------------------------

IR$anc.1stvisit <- ifelse(IR$m13_1 == 98,5,IR$m13_1) # Impute DNK to the mean of 5
IR$anc.1stvisit[IR$m14_1 == 0] <-10 # if had no visits set to 10 (birth)

  #* Number of antenatal controls ----------------------------------------------

IR$anc.num <- ifelse(IR$m14_1 == 98,4,IR$m14_1) # Impute DNK to the mean of 4

  #* Four or more antenatal controls -------------------------------------------

IR$anc.4plus <-ifelse(IR$anc.num>=4,1,0)

  #* Less than 4 antenatal controls --------------------------------------------

#Recoded
IR$anc.less4 <-ifelse(IR$anc.num>=4,0,1)

  #* Month of first antenatal control ------------------------------------------

IR$anc.month<-ifelse(IR$anc.1stvisit<4,"Month 1-3",
                       ifelse(IR$anc.1stvisit>3 & IR$anc.1stvisit<7,"Month 4-6",
                              ifelse(IR$anc.1stvisit>6 & IR$anc.1stvisit<10, "Month 7-9", "No ANC")))
IR$anc.month <- factor(IR$anc.month, c("No ANC","Month 1-3","Month 4-6", "Month 7-9"))

  #* Had at least 1 antenatal control ------------------------------------------

IR$anyanc<-ifelse(IR$m14_1 == 0,0,1)

  #* Had an antenatal control during the 1st trimester -------------------------

IR$anc.1sttri<-ifelse(IR$anc.month=="Month 1-3", 1,0)

  #* Had no antenatal control during the 1st trimester -------------------------

# Recoded
IR$noanc.1sttri<-ifelse(IR$anc.month=="Month 1-3", 0,1)

  
# Health check after birth -----------------------------------------------------

  #* Woman had health check after birth ----------------------------------------


temp1<-ifelse(IR$m66_1=="yes" & !is.na(IR$m66_1),1,0) # After birth at home
temp2<-ifelse(IR$m62_1=="yes" & !is.na(IR$m62_1),1,0) # After birth at facility

IR$wom.hlthck<-ifelse((temp1==1|temp2==1),1,0)
IR$wom.hlthck<-ifelse((is.na(IR$m62_1) & is.na(IR$m66_1)),NA,IR$wom.hlthck)

  #* Woman had no health check after birth -------------------------------------

# Recoded 
IR$wom.nohlthck<-ifelse((temp1==1|temp2==1),0,1)
IR$wom.nohlthck<-ifelse((is.na(IR$m62_1) & is.na(IR$m66_1)),NA,IR$wom.nohlthck)


  #* Baby had health check after birth -----------------------------------------

temp3<-ifelse(IR$m70_1=="yes" & !is.na(IR$m70_1),1,0) # After birth at home
temp4<-ifelse(IR$m74_1=="yes" & !is.na(IR$m74_1),1,0) # After birth in facility
IR$baby.hlthck<-ifelse((temp3==1|temp4==1),1,0)
IR$baby.hlthck<-ifelse((is.na(IR$m70_1) & is.na(IR$m74_1)),NA,IR$baby.hlthck)

  #* Baby had no health check after birth --------------------------------------

# Recoded 
IR$baby.nohlthck<-ifelse((temp3==1|temp4==1),0,1)
IR$baby.nohlthck<-ifelse((is.na(IR$m70_1) & is.na(IR$m74_1)),NA,IR$baby.nohlthck)

# Wasting, stunting, underweight for last child --------------------------------

# Wasting

IR$haz.last<-IR$hw70_1/100; IR$haz.last<-ifelse(IR$haz.last>90,NA,IR$haz.last) #height/age standard deviation
IR$waz.last<-IR$hw71_1/100; IR$waz.last<-ifelse(IR$waz.last>90,NA,IR$waz.last) #weight/age standard deviation
IR$whz.last<-IR$hw72_1/100; IR$whz.last<-ifelse(IR$whz.last>90,NA,IR$whz.last) #weight/height standard deviation

# Stunting (categorical version)

IR<- IR %>% mutate(stunt.cat1.last =  case_when(
  haz.last < -3 ~ "Severely stunted",
  (haz.last >= -3 & haz.last < -2) ~ "Moderately stunted",
  haz.last >= -2 ~ "Not stunted"))
IR$stunt.cat1.last<-factor(IR$stunt.cat1, c("Severely stunted","Moderately stunted", "Not stunted"))

IR$stunt.cat2.last<-ifelse(IR$haz.last < -2,1,0)


# Wasting (categorical)

IR<- IR %>% mutate(waste.cat1.last =  case_when(
  waz.last < -3 ~ "Severely wasted",
  (waz.last >= -3 & waz.last < -2) ~ "Moderately wasted",
  waz.last >= -2 ~ "Not wasted"))
IR$waste.cat1.last<-factor(IR$waste.cat1, c("Severely wasted","Moderately wasted", "Not wasted"))

IR$waste.cat2.last<- ifelse(IR$waz.last < -2,1,0)

# Underweight

IR<- IR %>% mutate(unwgt.cat1.last =  case_when(
  waz.last < -3 ~ "Severely underweight",
  (waz.last >= -3 & waz.last < -2) ~ "Moderately underweight",
  waz.last >= -2 ~ "Not underweight"))
IR$unwgt.cat1.last<-factor(IR$unwgt.cat1, c("Severely underweight","Moderately underweight", "Not underweight"))

IR$undwgt.last<- ifelse(IR$waz.last < -2,1,0)

# Overweight

IR$ovrwgt.last<-ifelse(IR$waz.last >2,1,0)

# Breastfeeding last child -----------------------------------------------------

  #* Woman ever breastfed (no restrictions) ------------------------------------

IR$breastfed<-ifelse(IR$m4_1 == "ever breastfed, not currently breastfeeding" | IR$m4_1=="still breastfeeding",1,0)

  #* Woman never breastfed (no restrictions) -----------------------------------

#Recoded
IR$nobreastfed<-ifelse(IR$m4_1 == "ever breastfed, not currently breastfeeding" | IR$m4_1=="still breastfeeding",0,1)

  #* Ever breastfed (child less than 24mo and lives with the respondent) -------

IR$breastfed2<-ifelse((IR$b19_01 < 24 & IR$b9_01 == "respondent"), IR$breastfed, NA) #child is <24 months, child lives with respondent

  #* Ever breastfed (child less than 6mo and lives with the respondent) -------

IR$breastfed6<-ifelse((IR$b19_01 < 6 & IR$b9_01 == "respondent"), IR$breastfed, NA) #child is <6 months, child lives with respondent

  #* Never breastfed (child less than 24mo and lives with the respondent) ------

#Recoded
IR$nobreastfed2<-ifelse((IR$b19_01 < 24 & IR$b9_01 == "respondent"), IR$nobreastfed, NA) #child is <24 months, child lives with respondent


  #* Child received other types of food ----------------------------------------

IR <- IR %>%
  mutate(comp.feed = case_when(IR$v409 == "yes" | IR$v410 == "yes" | IR$v411 == "yes" | IR$v411 == "yes" | # HM: IR$v412a == "yes" |
                                 IR$v412c == "yes" | IR$v413 == "yes" | IR$v414e == "yes"  | IR$v414f == "yes" |
                                 IR$v414g == "yes" | IR$v414h == "yes" | IR$v414i == "yes" | IR$v414j == "yes" |
                                 IR$v414k == "yes" | IR$v414l == "yes" | IR$v414m == "yes" | IR$v414n == "yes" |
                                 IR$v414o == "yes" | IR$v414p == "yes" | IR$v414s == "yes" | IR$v414v == "yes" ~1,
                               TRUE ~ 0))
IR$comp.feed<-ifelse((IR$b19_01 < 24 & IR$b9_01 == "respondent"),IR$comp.feed, NA) ##these are questions on other feed types, like water or solids


  #* Child received exclusive breastfeeding ------------------------------------

IR$breastfed2.excl<-ifelse((IR$breastfed2==1 & IR$comp.feed==0),1,0)

  #* Child did not receive exclusive breastfeeding -----------------------------

# Recoded
IR$breastfed2.noexcl<-ifelse((IR$breastfed2==1 & IR$comp.feed==0),0,1)

#* Child under 6m did receive exclusive breastfeeding --------------------------

IR$breastfed6.excl<-ifelse((IR$breastfed6==1 & IR$comp.feed==0),1,0)

############## Adding new outcomes for Kenya 2022 DHS  #####################

# Current pregnancy wanted (creation of categorical and binary variable) -------


IR <- IR %>% mutate(cur.preg.want.cat = case_when(v225 == "then" ~ "wanted then", 
                                                  v225 == "later" ~ "wanted later",
                                                  v225 == "not at all" ~ "not wanted", 
                                                  v213 == "no or unsure" ~ "not currently pregnant"))

IR <- IR %>% mutate(cur.preg.want.yn = case_when(v225 == "then" ~ 1, 
                                                 v225 == "later" ~ 1,
                                                 v225 == "not at all" ~ 0))

# Current pregnancy NOT wanted -------------------------------------------------

IR <- IR %>% mutate(cur.preg.notwant = case_when(v225 == "then" ~ 0, 
                                                 v225 == "later" ~ 0,
                                                 v225 == "not at all" ~ 1))



# Most recent child had diarrhea in last 2 weeks -------------------------------
# Potential data quality issues here - data reported when also report 0 children in household]  - may need to run this again to check

IR <- IR %>% mutate(diarrhea.last.yn = ifelse(h11_1=="yes, last 24 hours" | h11_1 == "yes, last two weeks", 1, 0)) # coded don't know as zero/no 


IR <- IR %>% mutate(diarrhea.last.cat = case_when(h11_1 == "yes, last 24 hours" | h11_1 == "yes, last two weeks" ~ "yes",
                                                  h11_1 == "no" ~ "no",
                                                  h11_1 == "don't know" ~ "don't know",
                                                  v137 == 0 ~ "no children"))
IR <- IR %>% mutate(diarrhea.last.cat3 = case_when(h11_1 == "yes, last 24 hours" | h11_1 == "yes, last two weeks" ~ 1,
                                                  h11_1 == "no" ~ 0,
                                                  h11_1 == "don't know" ~ NA_real_,
                                                  v137 == 0 ~ NA_real_
                                                  ))



# Most recent child delivered by skilled birth attendance ----------------------

# ______________________________________________________________________________
#Type of attendance - (we can see if we need this section)

{ 
  # a = doctor
  # b = nurse/midwife/clinical officer
  # Only a and b considered skilled birth attendant
  
  # g = traditional birth attendant
  # h = relative/friend 
  # k = other
  # n = no one 
}

#_______________________________________________________________________________

  #* Binary --------------------------------------------------------------------

IR <- IR %>% mutate(birth.assist.skill.yn = ifelse(m3a_1 == "yes" | m3b_1 == "yes", 1, 0))

  #* Categorical ---------------------------------------------------------------

IR <- IR %>% mutate(birth.assist.skill.cat = case_when(birth.assist.skill.yn == 1 ~ "yes",
                                                       birth.assist.skill.yn == 0 ~ "no",
                                                       v137 == 0 ~ "no children"))


# Most recent child NOT delivered by skilled birth attendance (Binary) ---------

#Recoded
IR <- IR %>% mutate(birth.assist.noskill = ifelse(m3a_1 == "yes" | m3b_1 == "yes", 0, 1))



# Most recent child was put on mother's chest/bare skin after birth ------------

  #* Binary for now - focused on whether there was contact on bare skin] -------

IR <- IR %>% mutate(bareskin.yn = ifelse(m77_1 == "put on chest, touching bare skin", 1, 0))


# Most recent child was not put on mother's chest/bare skin after birth --------

#Recoded
IR <- IR %>% mutate(nobareskin = ifelse(m77_1 == "put on chest, touching bare skin", 0, 1))

# Most recent child's birth weight ---------------------------------------------

IR <- IR %>% mutate(birthwgt.last = m19_1) # continuous - note many NAs
IR <- IR %>% mutate(birthwgt.last = ifelse(m19_1 == 9998 | m19_1 == 9996, NA, birthwgt.last)) # continuous - convert 9996 and 9998 to NAs 

IR <- IR %>% mutate(birthwgt.last.cat = ifelse(is.na(m19_1) | m19_1==9998, "no birth record", ifelse(m19_1==9996, "Not weighed at birth", m19_1)))
IR <- IR %>% mutate(birthwgt.last.cat2 = case_when(m19_1 == 9996 ~ "not weighed at birth",
                                                   m19_1 == 9998 | is.na(m19_1) ~ "no birth record",
                                                   m19_1 < 2500 ~ "low birth weight",
                                                   m19_1 == 2500 | m19_1 >2500 ~ "not low birth weight"
                                                   ))

  #* Make into 0,1 indicator variable ------------------------------------------
  
IR <- IR %>% mutate(birthwgt.last.cat3 = ifelse(m19_1 >= 2500 & m19_1 < 9000, 1, 0))
IR <- IR %>% mutate(birthwgt.last.cat3 = ifelse(m19_1 == 9998 | m19_1 == 9996, NA, birthwgt.last.cat3))

# Most recent child's birth weight was low (<2500) -----------------------------

#Recoded
IR <- IR %>% mutate(low.birthwgt = ifelse(m19_1 >= 2500 & m19_1 < 9000, 0, 1))
IR <- IR %>% mutate(low.birthwgt = ifelse(m19_1 == 9998 | m19_1 == 9996, NA, low.birthwgt))


# Length of time between discharge/delivery and postnatal checkup for most recent child ----

IR <- IR %>% mutate(postnatal.chck.last.time = m71_1) # actual continuous time of health check post discharge/delivery - review codebook to interpret numbers [note some data quality issues]

  #* Estimating the mean length of time in days to capture those coded as 299 (days but missing specific #) and 399 (weeks but missing specific #) ----
  
subdata <- IR %>% filter((postnatal.chck.last.time > 200 & postnatal.chck.last.time < 299) | (postnatal.chck.last.time > 300 & postnatal.chck.last.time < 399)) 
subdata <- subdata %>% 
  mutate(postnatal.chck.days = ifelse(postnatal.chck.last.time > 200 & postnatal.chck.last.time < 299, postnatal.chck.last.time - 200, (postnatal.chck.last.time - 300)*7))
meandays <- mean(subdata$postnatal.chck.days)

  #* Create continuous variable in days [can break it into categories if needed] ----

IR <- IR %>% mutate(postnatal.chck.last.days = case_when(postnatal.chck.last.time < 200 ~ 0, #0 represents anything less than 24 hours
                                                         postnatal.chck.last.time > 200 & postnatal.chck.last.time < 299 ~ (postnatal.chck.last.time - 200), 
                                                         postnatal.chck.last.time > 300 & postnatal.chck.last.time < 399 ~ ((postnatal.chck.last.time - 300)*7),
                                                         postnatal.chck.last.time == 299 | postnatal.chck.last.time == 399 ~ meandays, #299 and 399 are days/weeks of an unknown number so setting as mean for now
                                                         postnatal.chck.last.time == 998 ~ as.double(NA)))

# Most recent child has had a fever in the last two weeks ----------------------

IR <- IR %>% mutate(fever.2wks.last = h22_1)
IR <- IR %>% mutate(fever.2wks.last2 = case_when(h22_1 == "yes" ~ "fever last 2 weeks",
                                                 h22_1 == "no" ~ "no fever last 2 weeks",
                                                 h22_1 == "don't know" ~ "don't know",
                                                 v137 == 0 ~ "no children"))
IR <- IR %>% mutate(fever.2wks.last3 = case_when(h22_1 == "yes" ~ 1,
                                                 h22_1 == "no" ~ 0,
                                                 h22_1 == "don't know" ~ NA_real_,
                                                 v137 == 0 ~ NA_real_))


# Most recent child has had malaria in the last two weeks ----------------------

IR <- IR %>% mutate(malaria.2wks.last = h71_1)
IR <- IR %>% mutate(malaria.2wks.last2 = case_when(h71_1 == "yes" ~ "malaria last 2 weeks",
                                                   h71_1 == "no" ~ "no malaria last 2 weeks",
                                                   h71_1 == "don't know" ~ "don't know",
                                                   v137 == 0 ~ "no children"))
IR <- IR %>% mutate(malaria.2wks.last3 = case_when(h71_1 == "yes" ~ 1,
                                                   h71_1 == "no" ~ 0,
                                                   h71_1 == "don't know" ~ NA_real_,
                                                 v137 == 0 ~ NA_real_))

# Most recent child has had cough in the last two weeks ------------------------

IR <- IR %>% mutate(cough.last = h31_1)
IR <- IR %>% mutate(cough.last2 = case_when(h31_1 == "yes, last 24 hours" ~ "cough in last 24 hours",
                                            h31_1 == "yes, last two weeks" ~ "cough in last 2 weeks",
                                            h31_1 == "no" ~ "no cough last 2 weeks",
                                            h31_1 == "don't know" ~ "don't know",
                                            v137 == 0 ~ "no children"))
IR <- IR %>% mutate(cough.last3 = case_when(h31_1 == "yes, last 24 hours" ~ 1,
                                            h31_1 == "yes, last two weeks" ~ 1,
                                            h31_1 == "no" ~ 0,
                                            h31_1 == "don't know" ~ NA_real_,
                                            v137 == 0 ~ NA_real_))


# Most recent child had short rapid breaths/difficulty breathing ---------------

IR <- IR %>% mutate(diffbreath.last = h31b_1)
IR <- IR %>% mutate(diffbreath.last2 = case_when(h31b_1 == "yes" ~ "yes",
                                                 h31b_1 == "no" ~ "no",
                                                 h31b_1 == "don't know" ~ "don't know",
                                                 v137 == 0 ~ "no children"))
IR <- IR %>% mutate(diffbreath.last3 = case_when(h31b_1 == "yes" ~ 1,
                                                 h31b_1 == "no" ~ 0,
                                                 h31b_1 == "don't know" ~ NA_real_,
                                                 v137 == 0 ~ NA_real_))

# Most recent child has health card and/or other vaccination document ----------

IR <- IR %>% mutate(vac.doc = h1a_1)
IR <- IR %>% mutate(vac.doc.yn = ifelse(vac.doc == "does not have either card or other document", 0, 1))


# Most recent child does not have health card and/or other vaccination document ----

#Recoded
IR <- IR %>% mutate(vac.nodoc = ifelse(vac.doc == "does not have either card or other document", 1, 0))


# Most recent child received Vitamin A in past 6 months ------------------------

IR <- IR %>% mutate(vit.a.6m = h34_1)
IR <- IR %>% mutate(vit.a.6m.cat = case_when(h34_1 == "yes" ~ "yes",
                                             h34_1 == "no" ~ "no",
                                             h34_1 == "don't know" ~ "don't know",
                                             v137 == 0 ~ "no children"))
IR <- IR %>% mutate(vit.a.6m.cat3 = case_when(h34_1 == "yes" ~ 1,
                                             h34_1 == "no" ~ 0,
                                             h34_1 == "don't know" ~ NA_real_,
                                             v137 == 0 ~ NA_real_))

# Most recent child did not receive Vitamin A in past 6 months -----------------

#Recoded
IR <- IR %>% mutate(novita.6m = case_when(h34_1 == "yes" ~ 0,
                                             h34_1 == "no" ~ 1,
                                             h34_1 == "don't know" ~ NA_real_,
                                             v137 == 0 ~ NA_real_))

# Most recent child received tx for intestinal worms in past 6 months ----------

IR <- IR %>% mutate(tx.worms.6m = h43_1)
IR <- IR %>% mutate(tx.worms.6m.cat = case_when(h43_1 == "yes" ~ "yes",
                                                h43_1 == "no" ~ "no",
                                                h43_1 == "don't know" ~ "don't know",
                                                v137 == 0 ~ "no children"))
IR <- IR %>% mutate(tx.worms.6m.cat3 = case_when(h43_1 == "yes" ~ 1,
                                                h43_1 == "no" ~ 0,
                                                h43_1 == "don't know" ~ NA_real_,
                                                v137 == 0 ~ NA_real_))

# Most recent child did not receive tx for intestinal worms in past 6 months ----

#Recoded

IR <- IR %>% mutate(notx.worms.6m = case_when(h43_1 == "yes" ~ 0,
                                              h43_1 == "no" ~ 1,
                                              h43_1 == "don't know" ~ NA_real_,
                                              v137 == 0 ~ NA_real_))

# ADDED FEB 13th ---------------------------------------------------------------

  #* HIV testing - Never tested for HIV Feb 13th, AR ---------------------------

# Just renamed, code remains the same

IR<-IR %>% mutate(hiv.never.tested = case_when(
  v781=="no"~ 1,
  v781 == "yes"~0,
  is.na(v781)~NA_integer_))

  #* STI - Had STI 12 months back vs Had genital sore/discharge ----------------

IR<- IR %>% mutate(sti.any = case_when(
    v763a=="yes"~1,
    v763b=="yes"~1, 
    v763c=="yes"~1,
    v763a=="no" & v763b=="no" & v763c=="no"~0,
   
  
  is.na(v763a)~ NA_integer_,
  is.na(v763b)~  NA_integer_,
  is.na(v763c)~ NA_integer_))
                                       
                                         
                                       
                                         
                                          
                                                                      
                                                                                               
#_______________________________________________________________________________

# Creating "ir.out" that contains all the relevant variables -------------------

# Adding sshort is required for plotting long/short questionnaire --------------

# Approx 50% of the sample completed the long questionnaire

ir.out<-IR %>% select(c("sshort", "v001","v002","caseid","baby.hlthck","wom.hlthck","bmi","bmi.cat1","bmi.cat2","hb.1","fp.mod.now",
                          "fp.mod.ever","anc.1stvisit","anc.num","anc.4plus", "anc.month","anyanc","anc.1sttri","haz.last","waz.last","whz.last",
                          "stunt.cat1.last","stunt.cat2.last","waste.cat1.last","waste.cat2.last","unwgt.cat1.last","undwgt.last","ovrwgt.last",
                          "breastfed","breastfed2","breastfed2.excl", 
                        # New for Kenya
                        "cur.preg.want.yn", "diarrhea.last.cat3", "birth.assist.skill.yn", "bareskin.yn",
                        "birthwgt.last", "birthwgt.last.cat3", "postnatal.chck.last.days", "fever.2wks.last3",
                        "malaria.2wks.last3", "cough.last3", "diffbreath.last3", 
                        "vac.doc","vac.doc.yn", "vit.a.6m.cat3", "tx.worms.6m.cat3", "sti.any", 
                        "hiv.never.tested","breastfed6.excl", 
                        #reversed
                        "fp.mod.never", "anc.less4", "noanc.1sttri", "wom.nohlthck", "baby.nohlthck", "nobreastfed2", 
                        "breastfed2.noexcl", "cur.preg.notwant", "birth.assist.noskill", "nobareskin", "low.birthwgt",
                        "vac.nodoc", "novita.6m", "notx.worms.6m"))



################################### KR FILE ####################################

# Wasting, stunting, underweight for all children ------------------------------


# For merging HH data to Individual data - "v001", "v002" is cluster, HHID need both to merge by HH

# Wasting

KR$hhid<-paste(KR$v001,KR$v002)
KR$haz<-KR$hw70/100; KR$haz<-ifelse(KR$haz>90,NA,KR$haz)
KR$waz<-KR$hw71/100; KR$waz<-ifelse(KR$waz>90,NA,KR$waz)
KR$whz<-KR$hw72/100; KR$whz<-ifelse(KR$whz>90,NA,KR$whz)

# Stunting 

KR<- KR %>% mutate(stunt.cat1 =  case_when(
  haz < -3 ~ "Severely stunted",
  (haz >= -3 & haz < -2) ~ "Moderately stunted",
  haz >= -2 ~ "Not stunted"))
KR$stunt.cat1<-factor(KR$stunt.cat1, c("Severely stunted","Moderately stunted", "Not stunted"))
KR$stunt.cat2<-ifelse(KR$haz < -2,1,0)

# Wasting (categorical)

KR<- KR %>% mutate(waste.cat1 =  case_when(
  whz < -3 ~ "Severely wasted",
  (whz >= -3 & whz < -2) ~ "Moderately wasted",
  whz >= -2 ~ "Not wasted"))
KR$waste.cat1<-factor(KR$waste.cat1, c("Severely wasted","Moderately wasted", "Not wasted"))
KR$waste.cat2<- ifelse(KR$whz < -2,1,0)

# Underweight
# CHECK if under weight is cat/not cat #Ask Helena/Sarah ----

KR<- KR %>% mutate(unwgt.cat1 =  case_when(
  waz < -3 ~ "Severely underweight",
  (waz >= -3 & waz < -2) ~ "Moderately underweight",
  waz >= -2 ~ "Not underweight"))
KR$unwgt.cat1<-factor(KR$unwgt.cat1, c("Severely underweight","Moderately underweight", "Not underweight"))
KR$undwgt<- ifelse(KR$waz < -2,1,0)

# Overweight

KR$ovrwgt<-ifelse(KR$waz >2,1,0)

# Breastfeeding ----------------------------------------------------------------

KR$breastfeed<-ifelse(KR$m4 == "ever breastfed, not currently breastfeeding" | KR$m4=="still breastfeeding",1,0)
KR$breastfeed<-ifelse((KR$b19 < 24 & KR$b9 == "respondent"),KR$breastfeed, NA)

KR <- KR %>%
  mutate(comp.feed = case_when(KR$v409 == "yes" | KR$v410 == "yes" | KR$v411 == "yes" | KR$v411 == "yes" | KR$v412a == "yes" |
                                 KR$v412c == "yes" | KR$v413 == "yes" | KR$v414e == "yes"  | KR$v414f == "yes" |
                                 KR$v414g == "yes" | KR$v414h == "yes" | KR$v414i == "yes" | KR$v414j == "yes" |
                                 KR$v414k == "yes" | KR$v414l == "yes" | KR$v414m == "yes" | KR$v414n == "yes" |
                                 KR$v414o == "yes" | KR$v414p == "yes" | KR$v414s == "yes" | KR$v414v == "yes" ~1,
                               TRUE ~ 0))
KR$comp.feed<-ifelse((KR$b19 < 24 & KR$b9 == "respondent"),KR$comp.feed, NA)

KR$breastfeed.excl<-ifelse((KR$breastfeed==1 & KR$comp.feed==0),1,0)

# NEED to check----
# KR$breastfeed.excl<-ifelse((KR$breastfeed==1 & KR$comp.feed==0),0,1)

KR$breastfeed.n<-ifelse((KR$b19 < 24 & KR$b9 == "respondent"),1,0)

kr.mom<- KR %>% group_by(caseid) %>% dplyr::summarize(k.rost=length(caseid), k.meas=length(caseid[(!is.na(whz)&!is.na(haz)&!is.na(waz))]), k.breast=sum(breastfeed.n, na.rm=T), 
                                              breast.cnt=sum(breastfeed, na.rm=T), breastex.cnt=sum(breastfeed.excl, na.rm=T),
                                              stunt.cnt=sum(stunt.cat2, na.rm=T), waste.cnt = sum(waste.cat2, na.rm=T), unwgt.cnt = sum(undwgt, na.rm=T),
                                              ovrwgt.cnt = sum(ovrwgt, na.rm=T), haz.mean=mean(haz, na.rm=T), waz.mean=mean(waz, na.rm=T), 
                                              whz.mean=mean(whz, na.rm=T))
kr.out<- KR %>% select(c("caseid", "haz","waz","whz","stunt.cat1","stunt.cat2","waste.cat1", "waste.cat2","unwgt.cat1","undwgt","ovrwgt",
                           "breastfeed","breastfeed.excl"))

#################################### BR FILE ###################################

# Immunization - zero-dose -----------------------------------------------------

#Similar to Cata-Preta et al, 2021, prevalence of zero-dose children was defined as those who had not received any doses of the 
#four vaccines (four basic vaccines: BCG, polio -OPV and IPV, doses given at birth were not considered in the analyses-, DPT, and MCV). 
#The immunization cascade was generated as a score ranging from 0 to 4. Each type of vaccine accounts for one point in the cascade regardless of the number of doses. 
#For example, a child was coded as "1" if either one, two or three doses of DPT/polio vaccine had been received. 

BR[,ChildAge:= (v008-b3)] # in months
summary(BR[is.na(h2),]$ChildAge)
summary(BR[is.na(h4),]$b8)
summary(BR[is.na(h4) & b5=="yes",]$ChildAge)

BR[,ZeroDose:= ifelse(h2== "no" & h3== "no" & h4== "no" & h5== "no" & h6== "no" & h7== "no" & h8== "no" & h9== "no",1,0)]
BR$ZeroDose <- ifelse((is.na(BR$h2))|(is.na(BR$h3))|(is.na(BR$h4))|(is.na(BR$h5))|(is.na(BR$h6))|(is.na(BR$h7))|(is.na(BR$h8))|(is.na(BR$h9)),NA,BR$ZeroDose)
BR$ZeroDose <- ifelse(BR$h2=="don't know" |BR$h3=="don't know"|BR$h4=="don't know" |BR$h5=="don't know" |BR$h6=="don't know" |BR$h7=="don't know" |BR$h8=="don't know" |BR$h9=="don't know",NA,BR$ZeroDose)

#_______________________________________________________________________________

BR[!(is.na(h2))& h2!="don't know",bcg:=ifelse(h2 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
BR[!(is.na(h3))& h3!="don't know",dpt1:=ifelse(h3 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
BR[!(is.na(h4))& h4!="don't know",opv1:=ifelse(h4 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
BR[!(is.na(h5))& h5!="don't know",dpt2:=ifelse(h5 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
BR[!(is.na(h6))& h6!="don't know",opv2:=ifelse(h6 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
BR[!(is.na(h7))& h7!="don't know",dpt3:=ifelse(h7 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
BR[!(is.na(h8))& h8!="don't know",opv3:=ifelse(h8 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
BR[!(is.na(h9))& h9!="don't know",mcv1:=ifelse(h9 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]

BR[,dpt:= ifelse(dpt1==1 |dpt2==1 | dpt3==1, 1, 0)]
BR[,opv:= ifelse(opv1==1 |opv2==1 | opv3==1, 1, 0)]

BR[,score:= (bcg + opv + dpt + mcv1)]
BR[,score2:= (bcg + opv1 + opv2 + opv3 + dpt1 + dpt2 + dpt3 + mcv1)]


#Recoded

#BR[!(is.na(h2))& h2!="don't know",bcg:=ifelse(h2 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),0,1)]
#BR[!(is.na(h3))& h3!="don't know",dpt1:=ifelse(h3 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),0,1)]
#BR[!(is.na(h4))& h4!="don't know",opv1:=ifelse(h4 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),0,1)]
#BR[!(is.na(h5))& h5!="don't know",dpt2:=ifelse(h5 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),0,1)]
#BR[!(is.na(h6))& h6!="don't know",opv2:=ifelse(h6 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),0,1)]
#BR[!(is.na(h7))& h7!="don't know",dpt3:=ifelse(h7 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),0,1)]
#BR[!(is.na(h8))& h8!="don't know",opv3:=ifelse(h8 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),0,1)]
#BR[!(is.na(h9))& h9!="don't know",mcv1:=ifelse(h9 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),0,1)]

#BR[,dpt:= ifelse(dpt1==1 |dpt2==1 | dpt3==1, 0,1)]
#BR[,opv:= ifelse(opv1==1 |opv2==1 | opv3==1, 0,1)]

# Additional code adapted from Ethiopia ----------------------------------------

#BR[!(is.na(ZeroDose)),list(count=length(unique(caseid))),keyby=c("ZeroDose","Agegrp")]
#BR[!(is.na(ZeroDose)),list(count=length(unique(caseid))),keyby=c("score","Agegrp")]
#BR[!(is.na(dpt1)),list(count=length(unique(caseid))),keyby=c("dpt1","Agegrp")]

BR[,dpt12mcv1:= ifelse(dpt1==1 & dpt2==1 & mcv1==1 & dpt3==0, 1, 0)]

# Child death under 5, under 1 -------------------------------------------------

#b3 - Date of birth of child (CMC)
#b5 - Child is alive (1 = Yes, 0 = No)
#b7 - Age at death in months (imputed)

BR$hhid<-paste(BR$v001,BR$v002)
BR$u1mort<-ifelse(BR$b7<13 & BR$b5=="no",1,0)
BR$u5mort<-ifelse(BR$b5=="no",1,0)



br.mom <- BR %>% group_by(caseid) %>% dplyr::summarize(b.rost=length(caseid), zerodosecnt = sum(ZeroDose, na.rm=T), dpt1cnt = sum(dpt1, na.rm=T),
                                               avgscore=mean(score, na.rm=T), u1mortcnt = sum(u1mort, na.rm=T), u5mortcnt = sum(u5mort, na.rm=T))
br.out<- BR %>% select(c("sshort", "caseid", "v001","v002","ZeroDose","dpt1","score", "u1mort","u5mort"))

outcomes <- merge(ir.out, kr.mom, by="caseid", all.x = TRUE)
outcomes <- merge(outcomes, br.mom, by="caseid", all.x=T)
outcomes <- merge(outcomes, stl.out, by="caseid", all.x=T)




# Define and group outcomes for use in age exploration file --------------------

  #*  Make binary versions of some outcomes ------------------------------------

    #** Underweight (woman, not child) -----------------------------------------
    
outcomes$underweight <- ifelse(outcomes$bmi.cat2 == "Underweight", 1, 
                              ifelse(!is.na(outcomes$bmi.cat2), 0, NA))

    #** Zero dose --------------------------------------------------------------
    
outcomes$zerodose.yn <- ifelse(outcomes$zerodosecnt > 0 & !is.na(outcomes$zerodosecnt), 1, 
                             ifelse(outcomes$zerodosecnt == 0, 0, NA))

   #** Received DPT1 -----------------------------------------------------------
  
outcomes$dpt1.yn <- ifelse(outcomes$dpt1cnt > 0 & !is.na(outcomes$dpt1cnt), 1, 
                          ifelse(outcomes$dpt1cnt == 0, 0, NA))

    #** Did not receive DPT1 ---------------------------------------------------

outcomes$nodpt1 <- ifelse(outcomes$dpt1cnt > 0 & !is.na(outcomes$dpt1cnt), 0, 
                           ifelse(outcomes$dpt1cnt == 0, 1, NA))

    #** Any deaths of kids under 1 ---------------------------------------------
outcomes$u1mort.yn <- ifelse(outcomes$u1mortcnt > 0 & !is.na(outcomes$u1mortcnt), 1, 
                            ifelse(outcomes$u1mortcnt == 0, 0, NA))

    #** Any deaths of kids under 5 ---------------------------------------------
    
outcomes$u5mort.yn <- ifelse(outcomes$u5mortcnt > 0 & !is.na(outcomes$u5mortcnt), 1, 
                             ifelse(outcomes$u5mortcnt == 0, 0, NA))



# Save constructed data --------------------------------------------------------


saveRDS(outcomes, "Data/KE_2022_constructed_data/1_Kenya_DHS_outcomes.rds")

