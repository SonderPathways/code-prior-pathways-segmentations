


################################################################################
# GENERATE OUTCOME VARIABLES | DHS
################################################################################

# PROGRAM: North_Nigeria_2024DHS8_1.0
# PURPOSE: Segmentation
# DATA INPUTS: 2024DHS8
# DATA OUTPUTS:
# AUTHOR: Lang Gao
# DATA LAST MODIFIED: Nov 30, 2025
# NOTES:


###################################
# RUN SETUP


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
# 11 | CHILD HEALTH
# 12 | MATERNAL HEALTH
# 13 | SUMMARIZE

gen_outcome_variables_dhs <- function(IR=NULL, KR=NULL, BR=NULL, dhs = 8) {


  ######################################################################
  # 1 | GENERAL

  # INDIVIDUAL
  IR[,str:=substr(vcal_1, v018, v018+59)]
  IR[,STL:=ifelse(grepl('TPPPPPP',str),1,NA)]
  IR[,LB:=ifelse(grepl('BPPPPPPP',str),1,NA)]

  # BIRTH
  BR$hhid <- paste(BR$v001, BR$v002)
  BR[,child.age:= (v008-b3)] #MONTHS
  KR[,child.age:= (v008-b3)] #MONTHS

  # KID
  KR$hhid<-paste(KR$v001, KR$v002)

  # FILTER TO ONLY CHILDREN <= 5 IN KR FILE (DATA QUALITY)
  # table(KR$child.age)
  KR <- KR %>%
    dplyr::filter(child.age < 61)


  ######################################################################
  # 2 | ANC/PNC ----

  ## ANC VISITS ----
  # Number of antenatal visits during last pregnancy ()
  # table(IR$m14_1, useNA = "always") #13452 NAs (this question applies to births in the last three/five years)
  # table(IR$v531, useNA = "always") #4538 have not had sex (v531=0, age at 1st sex). At this point we have not yet place any filter for sampling inclusion.
  # table(IR$survey, useNA = "always") #all comes from survey==NG8ADT. The line dplyr::group_by(survey) is useful when combining 2 IR files like Senegal.
  # Assign the original 98 (not all NAs) to mean (excluding NAs)
  IR <- IR %>%
    dplyr::group_by(survey) %>%
    dplyr::mutate(anc.total = case_when((m14_1 == 98 | m14_1 == "don't know") ~ NA,
                                        (m14_1 == "no antenatal visits" | m14_1 == 0) ~ 0,
                                        is.na(m14_1) ~ NA,
                                        TRUE ~ as.integer(m14_1)),
                  anc.mean = as.integer(mean(as.integer(anc.total), na.rm=TRUE)),
                  anc.total = ifelse((m14_1 == 98 | m14_1 == "don't know"), anc.mean, anc.total),
                  anc.less4.last = ifelse(anc.total >= 4, 0, 1)) %>%
    ungroup()
  # table(IR$anc.total, useNA = "always")

  ## 1st ANC ----
  # Timing of 1st antenatal check (months)
  IR <- IR %>%
    dplyr::group_by(survey) %>%
    dplyr::mutate(anc.1stvisit = case_when((m13_1 == 98 | m13_1 == "don't know") ~ NA,
                                           is.na(m13_1) ~ NA,
                                           TRUE ~ as.integer(m13_1)),
                  anc.1st.mean = as.integer(mean(as.integer(anc.1stvisit), na.rm=TRUE)),
                  anc.1stvisit = ifelse((m13_1 == 98 | m13_1 == "don't know"), anc.1st.mean, anc.1stvisit),
                  no.anc.1st.tri = ifelse(anc.1stvisit < 4, 0, 1)) %>%
    ungroup()
  # table(IR$anc.1stvisit, useNA = "always")
  table(IR$m14_1, useNA = "always")

  ## CATEGORICAL OUTCOME FOR ANC 1ST VISIT ----
  # IR$anc.month <- ifelse(IR$anc.1stvisit < 4, "Month 1-3",
  #                        ifelse(IR$anc.1stvisit > 3 & IR$anc.1stvisit < 7,"Month 4-6",
  #                               ifelse(IR$anc.1stvisit > 6 & IR$anc.1stvisit < 10, "Month 7-9", "No ANC")))
  IR <- IR %>%
    dplyr::mutate(anc.month = case_when(
      anc.1stvisit < 4 ~ "Month 1-3",
      (anc.1stvisit > 3 & IR$anc.1stvisit < 7) ~ "Month 4-6",
      (anc.1stvisit > 6 & IR$anc.1stvisit < 10) ~ "Month 7-9",
      m14_1==0 ~ "No ANC"))
  IR$anc.month <- factor(IR$anc.month, c("No ANC","Month 1-3","Month 4-6", "Month 7-9"))
  # table(IR$anc.month, useNA = "always")

  ## WOMAN HAD HEALTH CHECK AFTER BIRTH ----
  # table(IR$m66_1, useNA = "always") #13452 NAs
  temp1 <- ifelse(IR$m66_1=="yes" & !is.na(IR$m66_1), 1, 0)
  temp2 <- ifelse(IR$m62_1=="yes" & !is.na(IR$m62_1), 1, 0)
  IR$wom.nohlthck <- ifelse((temp1==1|temp2==1), 0, 1)
  IR$wom.nohlthck <- ifelse((is.na(IR$m62_1) & is.na(IR$m66_1)), NA, IR$wom.nohlthck)
  # table(IR$wom.nohlthck, useNA = "always") #13452 NAs

  ## BABY HAD HEALTH CHECK AFTER BIRTH ----
  # Baby postnatal check within 2 months, or Whether the child’s health was checked before discharge
  temp3<-ifelse(IR$m70_1=="yes" & !is.na(IR$m70_1), 1,0)
  temp4<-ifelse(IR$m74_1=="yes" & !is.na(IR$m74_1), 1,0)
  IR$baby.nohlthck<-ifelse((temp3==1|temp4==1),0, 1)
  IR$baby.nohlthck<-ifelse((is.na(IR$m70_1) & is.na(IR$m74_1)), NA, IR$baby.nohlthck)
  # table(IR$baby.nohlthck, useNA = "always") #13610 NAs
  # table(IR$m70_1, useNA = "always") #13610 NAs


  ######################################################################
  # 3 | BMI ----
  ## BMI ----
  IR$bmi <- as.numeric(IR$v445)/100
  # summary(IR$bmi, useNA = "always") #15041 NAs
  IR$bmi <- ifelse(IR$bmi > 90, NA, IR$bmi)

  ## WOMAN UNDERWEIGHT ----
  IR <- IR %>%
    dplyr::mutate(woman.underweight = case_when(bmi < 18.5 ~ 1,
                                                is.na(bmi) ~ NA,
                                                TRUE ~ 0))
  # table(IR$woman.underweight, useNA = "always") #15050 NAs

  ## WOMAN OVERWEIGHT ----

  ######################################################################
  # 4 | BREASTFEEDING ----

  ## LATEST CHILD BREASTFED ----
  # table(IR$m4_1, useNA = "always") #13610 NAs
  IR <- IR %>%
    dplyr::mutate(no.breastfeed.last = case_when(m4_1 %in% c("never breastfed") ~ 1,
                                                 is.na(m4_1) ~ NA,
                                                 TRUE ~ 0))
  # table(IR$no.breastfeed.last, useNA = "always")
  # table(KR$m4, useNA = "always") #NAs
  KR <- KR %>%
    dplyr::mutate(no.breastfeed.n = case_when(m4 %in% c("never breastfed") ~ 1,
                                              is.na(m4) ~ NA,
                                              TRUE ~ 0))

  # ALTERNATE DEFINITION FOR LAST CHILD BREASTFED ----
  IR <- IR %>%
    dplyr::mutate(no.breastfeed2.last = ifelse((b19_01 < 24 & b9_01 == "respondent"), no.breastfeed.last, NA))
  # table(IR$no.breastfeed2.last, useNA = "always")
  KR <- KR %>%
    dplyr::mutate(no.breastfeed2.n = ifelse((b19 < 24 & b9 == "respondent"), no.breastfeed.n, NA))

  ######################################################################
  # 5 | CHILD MORTALITY ----

  # BIRTH COMPLICATIONS (ADDED BY SONDER ATTN BROKEN)
  # *question 428A asked women if they had birth complications for last birth and next-to-last birth. 0=="no", 1=="1",8=="don't know"
  # IR[,bc.1:= ifelse(s428aa_1== 1 | s428ab_1== 1 | s428ac_1== 1 | s428ad_1== 1 | s428ae_1== 1 | s428af_1== 1 | s428ag_1 == 1 | s428ah_1== 1,1,0)]
  # IR[,bc.2:= ifelse(s428aa_2== 1 | s428ab_2== 1 | s428ac_2== 1 | s428ad_2== 1 | s428ae_2== 1 | s428af_2== 1 | s428ag_2 == 1 | s428ah_2== 1,1,0)]
  #
  # IR$bc.ever[(IR$bc.1==1 | IR$bc.2==1)]<- 1
  # IR$bc.ever[(IR$bc.1==0 & IR$bc.2==0)]<-0
  # IR$bc.ever <- ifelse((is.na(IR$bc.1))&(is.na(IR$bc.2)),NA,IR$bc.ever)
  #

  ## CHILD UNDER 1 DIED ----
  BR$u1mort <- ifelse(BR$b7<12 & BR$b5=="no", 1, 0)

  ## CHILD UNDER 5 DIED ----
  BR$u5mort<-ifelse(BR$b7<60 & BR$b5=="no", 1, 0)
  # table(BR$u5mort, useNA = "always") #9284 when no condition on b7; 8364 when added b7<61

  ## STILLBIRTHS ----
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
  stl.out$stl.yn <- ifelse(stl.out$stl.cnt > 0, 1, 0)


  ######################################################################
  # 6 | FAMILY PLANNING ----

  # NOT CURRENTLY USING MODERN FAMILY PLANNING METHOD
  ## Current use by method type ----
  IR <- IR %>%
    dplyr::mutate(nofp.mod.now = ifelse(v313 == "modern method", 0, 1))
 # table(IR$v359, useNA = "always") #0 NA

  # Last method discontinued in last 5 years
  IR <- IR %>%
    dplyr::mutate(nofp.dis.5yr = case_when(v359 %in% c("pill", "iud", "injections", "male condom", "implants/norplant", "female condom", "emergency contraception", "other modern method", "lactational amenorrhea (lam)", "standard days method (sdm)") ~ 0,
                                           is.na(v359) ~ NA,
                                           TRUE ~ 1))
  #v359: Last method discontinued in the last five years
  # Decision to interpret NA as "Did not discontinue a modern method in last 5yrs"
  IR$nofp.dis.5yr[is.na(IR$nofp.dis.5yr)] <- 1

  # NEVER USE OF MODERN FAMILY PLANNING METHOD
  # Per input by Darcy, let's say if you're currently using or discontinued a modern method in last 5yrs, then 0, otherwise we assume you have never used a modern method
  IR <- IR %>%
    # dplyr::mutate(nofp.mod.ever = ifelse((nofp.mod.now==1 & nofp.dis.5yr==1), 1, 0))
    dplyr::mutate(nofp.mod.ever = ifelse((nofp.mod.now==0 | nofp.dis.5yr==0), 0, 1))
  # IR <- IR %>%
  #   dplyr::mutate(nofp.mod.ever = ifelse((nofp.mod.now==0 & nofp.dis.5yr==0), 0, 1))
  # IR <- IR %>%
  #   dplyr::mutate(nofp.mod.ever = ifelse((nofp.mod.now==1 | nofp.dis.5yr==1), 1, 0))
  # table(IR$nofp.mod.ever, useNA = "always")

table(IR$nofp.dis.5yr, useNA = "always")
  ######################################################################
  # 7 | HOME BIRTHS ----

  # LATEST BIRTH WAS HOME BIRTH
  IR <- IR %>%
    dplyr::mutate(home.birth.last = case_when(m15_1 %in% c("home", "parents' home", "her home", "other home") ~ 1,
                                              is.na(m15_1) ~ NA,
                                              TRUE ~ 0))
  # table(IR$m15_1, useNA = "always") #13452 NAs
  IR <- IR %>%
    dplyr::mutate(home.birth.2 = case_when(m15_2 %in% c("her home", "other home") ~ 1,
                                              is.na(m15_2) ~ NA,
                                              TRUE ~ 0))
  IR <- IR %>%
    dplyr::mutate(home.birth.3 = case_when(m15_3 %in% c("her home", "other home") ~ 1,
                                           is.na(m15_3) ~ NA,
                                           TRUE ~ 0))
  IR <- IR %>%
    dplyr::mutate(home.birth.4 = case_when(m15_4 %in% c("her home", "other home") ~ 1,
                                           is.na(m15_4) ~ NA,
                                           TRUE ~ 0))
  IR <- IR %>%
    dplyr::mutate(home.birth.5 = case_when(m15_5 %in% c("her home", "other home") ~ 1,
                                           is.na(m15_5) ~ NA,
                                           TRUE ~ 0))
  IR <- IR %>%
    mutate(home.birth.ever = case_when((home.birth.last==1|home.birth.2==1|home.birth.3==1|home.birth.4==1|home.birth.5==1) ~ 1,
                                       is.na(home.birth.last) & is.na(home.birth.2) & is.na(home.birth.3) & is.na(home.birth.4) & is.na(home.birth.5) ~ NA,
                                       TRUE ~ 0))


  # ANY BIRTH WAS A HOME BIRTH
  BR <- BR %>%
    dplyr::mutate(home.birth = case_when(m15 %in% c("home", "parents' home", "her home", "other home") ~ 1,
                                         is.na(m15) ~ NA,
                                         TRUE ~ 0))
  # table(BR$m15, useNA = "always")

  ######################################################################
  # 8 | IMMUNIZATION ----

  # MOST RECENT CHILD HAS A HEALTH CARD AND OR OTHER VACCINATION DOCUMENT (ADDED BY SONDER)
  IR <- IR %>% mutate(vac.doc = h1a_1) #13924 NAs
  IR <- IR %>% mutate(vac.doc.yn = ifelse(vac.doc == "does not have either card or other document", 0, 1))
  # table(IR$vac.doc.yn, useNA = "always")

  # table(KR$h10, useNA = "always")

  # ANY ROUTINE VACCINE COUNT (ADDED BY SONDER ATTN BROKEN)
  # any.routine.vac.cnt

  # ANY ROUTINE VACCINE (ADDED BY SONDER ATTN BROKEN)
  # any.routine.vac.yn (needs to be custom coded) We may need to create this? Define which ones are considered as “routine”? I think we discussed this is country specific / needs input from expert panel?


  # *** Pentavalent ***
  ## DPT 1, 2, 3 either source (correction?) ----
  KR <- KR %>%
    mutate(dpt1 = case_when(h3%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h3%in%c("no","don't know") ~ 0)) %>%
    mutate(dpt2 = case_when(h5%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h5%in%c("no","don't know") ~ 0)) %>%
    mutate(dpt3 = case_when(h7%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h7%in%c("no","don't know") ~ 0)) %>%
    mutate(dptsum = dpt1 + dpt2 + dpt3) %>%
    mutate(dpt.full = case_when(is.na(dptsum) ~ NA, dptsum == 3 ~ 0, TRUE ~ 1)) %>%
    mutate(zero.dose = case_when(is.na(dptsum) ~ NA, dptsum == 0 ~ 1, TRUE ~ 0))
  # table(KR$h3)

  # *** Measles ***
  ## Measles either source ----
  KR <- KR %>%
    mutate(measles1 = case_when(h9 %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, h9 %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(measles2 = case_when(h9a %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, h9a %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(measlessum = measles1 + measles2) %>%
    mutate(measles.full = case_when(is.na(measlessum) ~ NA, measlessum == 2 ~ 0, TRUE ~ 1))

  # *** Polio ***
  ## Polio 0, 1, 2, 3 either source ----
  KR <- KR %>%
    mutate(polio1 = case_when(h4%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h4%in%c("no","don't know") ~ 0  )) %>%
    mutate(polio2 = case_when(h6%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h6%in%c("no","don't know") ~ 0  )) %>%
    mutate(polio3 = case_when(h8%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h8%in%c("no","don't know") ~ 0  )) %>%
    mutate(poliosum=polio1 + polio2 + polio3) %>%
    mutate(polio.full = case_when(is.na(poliosum) ~ NA, poliosum == 3 ~ 0, TRUE ~ 1))



  ##Individual vaccines (NEW) ----
  #Basis for questions: All children 0–59 months
  ### Measles either source ----
  KR <- KR %>%
    mutate(measles1 = case_when(h9 %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, h9 %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(measles2 = case_when(h9a %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, h9a %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(measlessum = measles1 + measles2) %>%
    mutate(measles.all = case_when(is.na(measlessum) ~ NA, measlessum == 2 ~ 1, TRUE ~ 0))
  # table(KR$measles.all, useNA = "ifany")


  ### DPT 1, 2, 3 either source ----
  KR <- KR %>%
    mutate(dpt1 = case_when(h3%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h3%in%c("no","don't know") ~ 0  )) %>%
    mutate(dpt2 = case_when(h5%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h5%in%c("no","don't know") ~ 0  )) %>%
    mutate(dpt3 = case_when(h7%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h7%in%c("no","don't know") ~ 0  )) %>%
    mutate(dptsum = dpt1 + dpt2 + dpt3) %>%
    mutate(dpt.all = case_when(is.na(dptsum) ~ NA, dptsum == 3 ~ 1, TRUE ~ 0))
  # table(KR$dpt.all, useNA = "ifany")

  ### Polio ----
  # Polio 0, 1, 2, 3 either source
  KR <- KR %>%
    mutate(polio1 = case_when(h4%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h4%in%c("no","don't know") ~ 0  )) %>%
    mutate(polio2 = case_when(h6%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h6%in%c("no","don't know") ~ 0  )) %>%
    mutate(polio3 = case_when(h8%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h8%in%c("no","don't know") ~ 0  )) %>%
    mutate(poliosum=polio1 + polio2 + polio3) %>%
    mutate(polio.all = case_when(is.na(poliosum) ~ NA, poliosum == 3 ~ 0, TRUE ~ 1))
  # table(KR$polio.all, useNA = "ifany")

  ### BCG ----
  KR <- KR %>%
    mutate(bcg1 = case_when(h2 %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, h2 %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(bcg.all = case_when(is.na(bcg1) ~ NA, bcg1 == 1 ~ 1, TRUE ~ 0))
  # table(KR$bcg.all, useNA = "ifany")

  ### Hep B ----
  # 1 birth dose
  KR <- KR %>%
    mutate(hepb1 = case_when(h50 %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, h50 %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(hepb.all = case_when(is.na(hepb1) ~ NA, hepb1 == 1 ~ 1, TRUE ~ 0))

  # table(KR$hepb.all, useNA = "ifany")

  ### Pentavalent ----
  # 0, 1, 2, 3
  # This contains DPT (plus Hib and Hep B) so the # is the same as DPT - it's the same injection.
  KR <- KR %>%
    mutate(pentavalent1 = case_when(h51%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h51%in%c("no","don't know") ~ 0  )) %>%
    mutate(pentavalent2 = case_when(h52%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h52%in%c("no","don't know") ~ 0  )) %>%
    mutate(pentavalent3 = case_when(h53%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h53%in%c("no","don't know") ~ 0  )) %>%
    mutate(pentavalentsum=pentavalent1 + pentavalent2 + pentavalent3) %>%
    mutate(pentavalent.all = case_when(is.na(pentavalentsum) ~ NA, pentavalentsum == 3 ~ 1, TRUE ~ 0))

  # table(KR$pentavalent.all, useNA = "ifany")

  ### Pneumococcal ----
  # 0, 1, 2, 3
  KR <- KR %>%
    mutate(pneumo1 = case_when(h54%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h54%in%c("no","don't know") ~ 0  )) %>%
    mutate(pneumo2 = case_when(h55%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h55%in%c("no","don't know") ~ 0  )) %>%
    mutate(pneumo3 = case_when(h56%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h56%in%c("no","don't know") ~ 0  )) %>%
    mutate(pneumosum=pneumo1 + pneumo2 + pneumo3) %>%
    mutate(pneumo.all = case_when(is.na(pneumosum) ~ NA, pneumosum == 3 ~ 1, TRUE ~ 0))

  # table(KR$pneumo.all, useNA = "ifany")

  ### Rotavirus ----
  # 0, 1, 2, 3
  KR <- KR %>%
    mutate(rota1 = case_when(h57%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h57%in%c("no","don't know") ~ 0  )) %>%
    mutate(rota2 = case_when(h58%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h58%in%c("no","don't know") ~ 0  )) %>%
    mutate(rota3 = case_when(h59%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h59%in%c("no","don't know") ~ 0  )) %>%
    mutate(rotasum=rota1 + rota2 + rota3) %>%
    mutate(rota.all = case_when(is.na(rotasum) ~ NA, rotasum == 3 ~ 1, TRUE ~ 0))
  # table(KR$rota.all, useNA = "ifany")

  ### Yellow fever ----
  #1 Due to an error in the Woman’s Questionnaire, information on yellow fever and meningitis vaccinations is missing
  #for 337 children age 12–23 months.
  KR <- KR %>%
    mutate(yellowfever1 = case_when(syf %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, syf %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(yellowfever.all = case_when(is.na(yellowfever1) ~ NA, yellowfever1 == 1 ~ 1, TRUE ~ 0))
  # table(KR$yellowfever.all, useNA = "ifany")

  ### Meningitis ----
  #1 Due to an error in the Woman’s Questionnaire, information on yellow fever and meningitis vaccinations is missing
  #for 337 children age 12–23 months.
  KR <- KR %>%
    mutate(meningitis1 = case_when(smg %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, smg %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(meningitis.all = case_when(is.na(meningitis1) ~ NA, meningitis1 == 1 ~ 1, TRUE ~ 0))
  # table(KR$meningitis.all, useNA = "ifany")

  ## SUMMARY VAX MEASURES (New) ----

  ### Basic antigen coverage ----
  #one dose BCG
  #3 dose polio
  #3 dose DPT
  #1 dose measles
  #children age 12 months+

  KR <- KR %>%
    mutate(
      basic.antigen.full.12m = case_when(
        b19 < 12 ~ NA_real_,
        b19 >= 12 &  bcg.all == 1 & dpt.all == 1 & polio.all == 1 & measles1 == 1 ~ 1,
        b19 >= 12 ~ 0))
  # table(KR$basic.antigen.full.12m, useNA = "ifany")

  ### Fully vaccinated 12 months ----
  # One dose of BCG vaccine
  # Hepatitis B vaccine (birth dose)
  # Three doses of DPT-HepB-Hib (pentavalent)
  # Four doses of OPV
  # Two doses of IPV
  # Three doses of pneumococcal vaccine
  # Three doses of rotavirus vaccine*
  # One dose of measles vaccine
  # One dose of yellow fever vaccine
  # One dose of meningitis vaccine
  #children age 12 months+

  KR <- KR %>%
    mutate(
      full.vax.schedule.12m = case_when(
        b19 < 12 ~ NA_real_,
        b19 >= 12 &  bcg.all == 1 & hepb.all == 1 & pentavalent.all == 1 & polio.all == 1 &
          pneumo.all == 1 & measles1 == 1 & yellowfever.all == 1 & meningitis.all == 1 ~ 1,
        b19 >= 12 ~ 0))
   # table(KR$full.vax.schedule.12m, useNA = "ifany")

  KR <- KR %>%
    mutate(
      not.full.vax.schedule.12m = case_when(
        is.na(h1) ~ NA_real_,
        b19 < 12 | b19 > 24 ~ NA_real_,
        b19 >= 12 & b19 <= 24 &
          (bcg.all == 0 | hepb.all == 0 | pentavalent.all == 0 | polio.all == 0 | pneumo.all == 0 | measles1 == 0 | yellowfever.all == 0 | meningitis.all == 0) ~ 1,
        b19 >= 12 & b19 <= 24 ~ 0))
  table(KR$not.full.vax.schedule.12m, useNA = "ifany")

  ### Fully vaccinated  24 months ----
  # second dose of measles
  KR <- KR %>%
    mutate(
      full.vax.schedule.24m = case_when(
        b19 < 24 ~ NA_real_,
        b19 >= 24 &  bcg.all == 1 & hepb.all == 1 & pentavalent.all == 1 & polio.all == 1 &
          pneumo.all == 1 & measles.all == 1 & yellowfever.all == 1 & meningitis.all == 1 ~ 1,
        b19 >= 24 ~ 0))
  # table(KR$full.vax.schedule.24m, useNA = "ifany")

  # Negatively coded fully vaccinated 24 months
  KR <- KR %>%
    mutate(
      not.full.vax.schedule.24m = case_when(
        is.na(h1) ~ NA_real_,
        b19 < 24 | b19 > 36 ~ NA_real_,
        b19 >= 24 &  bcg.all == 0 | hepb.all == 0 | pentavalent.all == 0 | polio.all == 0 |
          pneumo.all == 0 | measles.all == 0 | yellowfever.all == 0 | meningitis.all == 0 ~ 1,
        b19 >= 24 & b19 <= 36 ~ 0))
  # table(KR$not.full.vax.schedule.24m, useNA = "ifany")


  ### Global Zero Dose definition ----
  # no dose (BCG, polio, DPT and MCV)

  KR <- KR %>%
    mutate(
      zero.dose.4vax.12m = case_when(
        b19 < 12 ~ NA_real_,
        b19 >= 12 &  bcg1 == 0 & dpt1 == 0 & dpt2 == 0 & dpt3 ==0 &
          polio1 == 0 & polio2 == 0 &polio3 == 0 & measles1 == 0 ~ 1,
        b19 >= 12 ~ 0))
  # table(KR$zero.dose.4vax.12m, useNA = "ifany")

  KR <- KR %>%
    mutate(
      zero.dose.operational = case_when(
        b19 < 12 | b19 > 24 ~ NA_real_,
        b19 >= 12 &  dpt1 == 0 ~ 1,
        b19 >= 12 & b19 <= 24 ~ 0))
  # table(KR$zero.dose.operational, useNA = "always")


  ######################################################################
  # 9 | MALNOURISHMENT ----
  # STANDARD MALNOURISHMENT VARIABLES IN IR FILE

  ## NO VITAMIN A SUPPLIMENTS (ADDED BY SONDER) ----
  # *NOTE (only available for children less than 60 months, alive)
  IR <- IR %>% mutate(novita.6m = case_when(h34_1 == "yes" ~ 0,
                                            h34_1 == "no" ~ 1,
                                            h34_1 == "don't know" ~ NA_real_,
                                            v137 == 0 ~ NA_real_))
  # table(IR$novita.6m, useNA = "always")

  ## IN THE LAST 12 MONTHS GIVEN MICRONUTRIENTS: LOCAL NAME FOR MULTIPLE MICRONUTRIENT POWDERS (ADDED BY SONDER) ----
  # * micronutrient.12m  - confirmed in NG data (only available for children less than 60 months, alive)
  BR <- BR %>%
    dplyr::mutate(micronutrient.12m = case_when(
      h80a %in% c("no","don't know") ~ 0,
      h80a== "yes" ~ 1))
  # table(BR$h80a, useNA = "always")

  ## STANDARD MALNOURISHMENT VARIABLES IN IR FILE ----
  ### STUNTING AND WASTING ----
  if (dhs == 7){

    # STUNTING
    IR <- IR %>%
      dplyr::mutate(haz.last = case_when(as.numeric(hw5_1)/100 > 90 ~ NA,
                                         is.na(hw5_1) ~ NA,
                                         TRUE ~ as.numeric(hw5_1)/100)) %>%
      dplyr::mutate(stunt.cat2.last = ifelse(haz.last < -2, 1, 0))

    # WASTING
    IR <- IR %>%
      dplyr::mutate(whz.last = case_when(as.numeric(hw11_1)/100 > 90 ~ NA,
                                         is.na(hw11_1) ~ NA,
                                         TRUE ~ as.numeric(hw11_1)/100)) %>%
      dplyr::mutate(waste.cat2.last = ifelse(whz.last < -2, 1, 0))

  } else if (dhs == 8){

    # STUNTING
    IR <- IR %>%
      dplyr::mutate(haz.last = case_when(as.numeric(hw70_1)/100 > 90 ~ NA,
                                         is.na(hw70_1) ~ NA,
                                         TRUE ~ as.numeric(hw70_1)/100)) %>%
      dplyr::mutate(stunt.cat2.last = ifelse(haz.last < -2, 1, 0))

    # WASTING
    IR <- IR %>%
      dplyr::mutate(whz.last = case_when(as.numeric(hw72_1)/100 > 90 ~ NA,
                                         is.na(hw72_1) ~ NA,
                                         TRUE ~ as.numeric(hw72_1)/100)) %>%
      dplyr::mutate(waste.cat2.last = ifelse(whz.last < -2, 1, 0))

  }

# table(IR$stunt.cat2.last, useNA = "always")

  ### UNDERWEIGHT/OVERWEIGHT ----
  IR <- IR %>%
    dplyr::mutate(waz.last = case_when(as.numeric(hw71_1)/100 > 90 ~ NA,
                                       is.na(hw71_1) ~ NA,
                                       TRUE ~ as.numeric(hw71_1)/100)) %>%
    dplyr::mutate(undwgt.last = ifelse(waz.last < -2, 1, 0),
                  ovrwgt.last = ifelse(waz.last > 2, 1, 0))


  ## STANDARD MALNOURISHMENT VARIABLES IN KR FILE ----
  ### STUNTING AND WASTING ----
  if (dhs == 7){

    # STUNTING
    KR <- KR %>%
      dplyr::mutate(haz = case_when(as.numeric(hw5)/100 > 90 ~ NA,
                                    is.na(hw5) ~ NA,
                                    TRUE ~ as.numeric(hw5)/100)) %>%
      dplyr::mutate(stunt.cat2 = ifelse(haz < -2, 1, 0))

    # WASTING
    KR <- KR %>%
      dplyr::mutate(whz = case_when(as.numeric(hw11)/100 > 90 ~ NA,
                                    is.na(hw11) ~ NA,
                                    TRUE ~ as.numeric(hw11)/100)) %>%
      dplyr::mutate(waste.cat2 = ifelse(whz < -2, 1, 0))

  } else if (dhs == 8){

    # STUNTING
    KR <- KR %>%
      dplyr::mutate(haz = case_when(as.numeric(hw70)/100 > 90 ~ NA,
                                    is.na(hw70) ~ NA,
                                    TRUE ~ as.numeric(hw70)/100)) %>%
      dplyr::mutate(stunt.cat2 = ifelse(haz < -2, 1, 0))

    # WASTING
    KR <- KR %>%
      dplyr::mutate(whz = case_when(as.numeric(hw72)/100 > 90 ~ NA,
                                    is.na(hw72) ~ NA,
                                    TRUE ~ as.numeric(hw72)/100)) %>%
      dplyr::mutate(waste.cat2 = ifelse(whz < -2, 1, 0))

  }


  # table(KR$stunt.cat2, useNA = "always")
  ### UNDERWEIGHT/OVERWEIGHT ----
  KR <- KR %>%
    dplyr::mutate(waz = case_when(as.numeric(hw71)/100 > 90 ~ NA,
                                  is.na(hw71) ~ NA,
                                  TRUE ~ as.numeric(hw71)/100)) %>%
    dplyr::mutate(undwgt = ifelse(waz < -2, 1, 0),
                  ovrwgt = ifelse(waz > 2, 1, 0))
  # table(KR$ovrwgt, useNA = "always")

  ######################################################################
  # 10 | MENSTRUAL HEALTH ----

  if(dhs == 8)
  # Able to change in privacy during last menstrual cycle
  IR$mens.nopriv <- ifelse(IR$v248=="no",1,0)
  # table(IR$v248, useNA = "always") #4494 NAs

  # Proper product
  IR <- IR %>%
    mutate(mens.product = ifelse(v247a=="yes"|v247b=="yes"|v247c=="yes"|v247d=="yes", 1, 0))

  IR$mens.noproduct = ifelse(IR$mens.product==0, 1, 0)
  # table(IR$mens.noproduct, useNA = "always")


  # Either
  IR <- IR %>%
    mutate(mens.noeither = ifelse(mens.noproduct==1 |mens.nopriv==1, 1, 0))


  ######################################################################
  # 11 | CHILD HEALTH ----

  # NO EXCLUSIVE BREASTFEEDING (ADDED BY SONDER ATTN BROKEN)
  KR <- KR %>%
    mutate(bf_curr = case_when(m4 == "still breastfeeding" ~ 1,                       # still breastfeeding
                               m4 %in% c("ever breastfed, not currently breastfeeding", "never breastfed", "inconsistent", "don't know") ~ 0))     # not currently breastfeeding

  # table(KR$bf_curr, useNA = "always")

  # Consuming other foods
  KR <- KR %>%
    mutate(
      water = if_else(v409 == "yes", 1, 0),
      liquids = if_else(v409a == "yes" | v410 == "yes" | v410a == "yes" | v412c == "yes" | v413 == "yes" | v413a == "yes" | v413b == "yes" | v413c == "yes" | v413d == "yes",1, 0),
      milk = if_else(v411 == "yes" | v411a == "yes", 1, 0),
      solids = if_else(v414a == "yes" | v414b == "yes" | v414c == "yes" | v414d == "yes" | v414e == "yes" | v414f == "yes" | v414g == "yes" | v414h == "yes" | v414i == "yes" | v414j == "yes" | v414k == "yes" | v414l == "yes" | v414m == "yes" | v414n == "yes" | v414o == "yes" |
                         v414p == "yes" | v414r == "yes" | v414s == "yes" | v414t == "yes" | v414u == "yes" | v414v == "yes" | v414w == "yes" | m39a == "yes", 1, 0))

  # table(KR$solids, useNA = "always")

  # Breast feeding status
  KR <- KR %>%
    mutate(
      bf_status = case_when(
        bf_curr == 0 ~ 0,    # not currently breastfeeding
        solids == 1    ~ 5,  # children eating solids
        milk == 1      ~ 4,  # given animal milk
        liquids == 1   ~ 3,  # given other liquids
        water == 1     ~ 2,  # given water
        TRUE ~ 1 ))          # exclusively breastfed
  # table(KR$bf_status, useNA = "always")


  KR <- KR %>%
    mutate(no.ebf.n = case_when(b19 < 6 & b9 == "respondent" & bf_status==1  ~ 0 , # exclusively bf
                                b19 < 6 & b9 == "respondent" & bf_status!=1 ~ 1))  # not exclusively bf
  # table(KR$no.ebf.n, useNA = "always")



  # Add this code to kr.out
  # no.ebf.cnt = ifelse(all(is.na(no.ebf.n)), NA, sum(no.ebf.n, na.rm=TRUE))


  # Add this code to kr.out
  # no.ebf.yn = ifelse(no.ebf.cnt<0, 1, 0)

  ## NO SKIN TO SKIN CONTACT (ADDED BY SONDER) ----
  IR <- IR %>% mutate(nobareskin = ifelse(m77_1 == "put on chest, touching bare skin", 0, 1))
  # table(IR$nobareskin, useNA = "always")

  ## CHILD GIVEN COMPLEMENTARY FEEDING (ADDED BY SONDER) ----
  IR <- IR %>%
    mutate(comp.feed = case_when(IR$v409 == "yes" | IR$v410 == "yes" | IR$v411 == "yes" | IR$v411 == "yes" | # HM: IR$v412a == "yes" |
                                   IR$v412c == "yes" | IR$v413 == "yes" | IR$v414e == "yes"  | IR$v414f == "yes" |
                                   IR$v414g == "yes" | IR$v414h == "yes" | IR$v414i == "yes" | IR$v414j == "yes" |
                                   IR$v414k == "yes" | IR$v414l == "yes" | IR$v414m == "yes" | IR$v414n == "yes" |
                                   IR$v414o == "yes" | IR$v414p == "yes" | IR$v414s == "yes" | IR$v414v == "yes" ~1,
                                 TRUE ~ 0))

  ## CHILD DID NOT RECIEVE EXCLUSIVE BREASTFEEDING (ADDED BY SONDER) ----
  IR <- IR %>%
    dplyr::mutate(breastfeed.last = case_when(m4_1 %in% c("never breastfed") ~ 0,
                                              is.na(m4_1) ~ NA,
                                              TRUE ~ 1))
  IR <- IR %>%
    dplyr::mutate(breastfeed2.last = ifelse((b19_01 < 24 & b9_01 == "respondent"), breastfeed.last, NA))
  # b19: Current age of child in months

  IR$breastfed2.noexcl<-ifelse((IR$breastfeed2.last==1 & IR$comp.feed==0),0,1)
  # table(IR$breastfed2.noexcl, useNA = "always")



  # ANY CHILD IMMEDIATLEY BREASTFEAD (ADDED BY SONDER ATTN BROKEN)
  # *Note in IR file any children <36 months were immediately breastfed
  # IR <- IR %>% mutate(no.imm.breastfeed.yn = case_when(
  #   rowSums(c_across(c(m34_1, m34_2, m34_3, m34_5, m34_6)) %in%
  #             c("Immediately", "Within first hour"), na.rm = TRUE) > 0 ~ 0,
  #   is.na(m34_1) & is.na(m34_2) & is.na(m34_3) & is.na(m34_5) & is.na(m34_6) ~ NA_integer_,
  #   TRUE ~ 1))

  # LAST CHILD IMMEDIATLEY BREASTFEAD (ADDED BY SONDER)
  # *Note in IR file any children <36 months were immediately breastfed
  IR <- IR %>% mutate(no.imm.breastfeed.last.yn = case_when(
    m34_1 %in% c("0", "100") ~ 0,
    is.na(m34_1) ~ NA_integer_,
    TRUE ~ 1))
  # table(IR$no.imm.breastfeed.last.yn, useNA = "always")


  # NUMBER CHILD IMMEDIATLEY BREASTFEAD (ADDED BY SONDER ATTN BROKEN)
  # *Note in IR file any children <36 months were immediately breastfed
  # IR <- IR %>% mutate(no.imm.breastfeed.cnt = rowSums(
  #   c_across(c(m34_1, m34_2, m34_3, m34_5, m34_6)) %in% c("Immediately", "Within first hour"), na.rm = TRUE))


  ## LOW BIRTHWEIGHT LAST CHILD (ADDED BY SONDER) ----
  # * [From Kenya DHS2022 WRA] - confirmed in NG data
  IR <- IR %>% mutate(low.birthwgt = ifelse(m19_1 >= 2500 & m19_1 < 9995, 0, 1))
  IR <- IR %>% mutate(low.birthwgt = ifelse(m19_1 == 9998 | m19_1 == 9996, NA, low.birthwgt))

  ## NOT TREATED FOR WORMS (ADDED BY SONDER) ----
  IR <- IR %>% mutate(notx.worms.6m = case_when(h43_1 == "yes" ~ 0,
                                                h43_1 == "no" ~ 1,
                                                h43_1 == "don't know" ~ NA_real_,
                                                v137 == 0 ~ NA_real_))

  ## DIARRHEAL ILLNESS ----
  # Had diarrhea recently (LAST CHILD)
  IR <- IR %>%
    dplyr::mutate(diarrhea.2wks.last = case_when(h11_1 == "yes, last 24 hours" ~ 1,
                                                 h11_1 == "yes, last two weeks" ~ 1,
                                                 h11_1 == "no" ~ 0,
                                                 h11_1 == "don't know" ~ NA,
                                                 is.na(h11_1) ~ NA,
                                                 v137 == 0 ~ NA))

  # Had diarrhea recently (ANY CHILD UNDER 5)
  KR <- KR %>%
    dplyr::mutate(diarrhea.2wks = case_when(h11 == "yes, last 24 hours" ~ 1,
                                            h11 == "yes, last two weeks" ~ 1,
                                            h11 == "no" ~ 0,
                                            h11 == "don't know" ~ NA,
                                            is.na(h11) ~ NA,
                                            v137 == 0 ~ NA))


  ## FEBRILE ILLNESS ----
  # Had fever in last two weeks (LAST CHILD)
  IR <- IR %>%
    dplyr::mutate(fever.2wks.last = case_when(h22_1 == "yes" ~ 1,
                                              h22_1 == "no" ~ 0,
                                              h22_1 == "don't know" ~ NA,
                                              is.na(h22_1) ~ NA,
                                              v137 == 0 ~ NA))

  # Had fever in last two weeks (ANY CHILD UNDER 5)
  KR <- KR %>%
    dplyr::mutate(fever.2wks = case_when(h22 == "yes" ~ 1,
                                         h22 == "no" ~ 0,
                                         h22 == "don't know" ~ NA,
                                         is.na(h22) ~ NA,
                                         v137 == 0 ~ NA))

  ## COUGH ----
  # Had cough in last two weeks (LAST CHILD UNDER 5)
  IR <- IR %>%
    dplyr::mutate(cough.2wks.last = case_when(h31_1 == "yes, last 24 hours" ~ 1,
                                              h31_1 == "yes, last two weeks" ~ 1,
                                              h31_1 == "no" ~ 0,
                                              h31_1 == "don't know" ~ NA,
                                              is.na(h31_1) ~ NA,
                                              v137 == 0 ~ NA))
  #v137: Number of children resident in the household and aged 5 and under. Visiting children are not included.

  # Had cough in last two weeks (ANY CHILD UNDER 5)
  KR <- KR %>%
    dplyr::mutate(cough.2wks = case_when(h31 == "yes, last 24 hours" ~ 1,
                                         h31 == "yes, last two weeks" ~ 1,
                                         h31 == "no" ~ 0,
                                         h31 == "don't know" ~ NA,
                                         is.na(h31) ~ NA,
                                         v137 == 0 ~ NA))
  # table(IR$cough.2wks.last, useNA = "always")
  # table(KR$cough.2wks, useNA = "always")


  ## FEVER OR COUGH (ALIGNS WITH TREATMENT QUESTION) ----
  IR <- IR %>%
    dplyr::mutate(fever.cough.2wks.last = ifelse(fever.2wks.last == 1 | cough.2wks.last == 1, 1, 0))

  KR <- KR %>%
    dplyr::mutate(fever.cough.2wks = ifelse(fever.2wks == 1 | cough.2wks == 1, 1, 0))


  ## PROBLEM IN CHEST ----
  # Problem in the chest or blocked or running nose
  IR <- IR %>%
    dplyr::mutate(chest.prob.last = case_when(h31c_1 == "both" ~ 1,
                                              h31c_1 == "chest only" ~ 1,
                                              h31c_1 == "nose only" ~ 0,
                                              h31c_1 == "other" ~ 0,
                                              h31c_1 == "don't know" ~ NA,
                                              is.na(h31c_1) ~ NA,
                                              v137 == 0 ~ NA))

  KR <- KR %>%
    dplyr::mutate(chest.prob = case_when(h31c == "both" ~ 1,
                                         h31c == "chest only" ~ 1,
                                         h31c == "nose only" ~ 0,
                                         h31c == "other" ~ 0,
                                         h31c == "don't know" ~ NA,
                                         is.na(h31c) ~ NA,
                                         v137 == 0 ~ NA))


  ## DIFFICULTY BREATHING ----
  # Short, rapid breaths
  IR <- IR %>%
    dplyr::mutate(diff.breath.last = case_when(h31b_1 == "yes" ~ 1,
                                               h31b_1 == "no" ~ 0,
                                               h31b_1 == "don't know" ~ NA,
                                               is.na(h31b_1) ~ NA,
                                               v137 == 0 ~ NA))

  KR <- KR %>%
    dplyr::mutate(diff.breath = case_when(h31b == "yes" ~ 1,
                                          h31b == "no" ~ 0,
                                          h31b == "don't know" ~ NA,
                                          is.na(h31b) ~ NA,
                                          v137 == 0 ~ NA))


  ## ARI SYMPTOMS ----
  IR <- IR %>%
    dplyr::mutate(ari.last = ifelse(chest.prob.last == 1 & diff.breath.last == 1, 1, 0))

  KR <- KR %>%
    dplyr::mutate(ari = ifelse(chest.prob == 1 & diff.breath == 1, 1, 0))


  ## HEALTH SEEKING FOR ILLNESS: TREATMENT FOR FEVER/COUGH ----
  # Fever/cough: (treatment sought at Xi)
  kr_var <- KR %>%
    dplyr::select(survey, caseid, starts_with("h32")) %>%
    reshape2::melt(id.vars=c("survey", "caseid")) %>%
    dplyr::mutate(care = case_when((variable == "h32a" & value == "yes") ~ 1, # PUBLIC HOSPITAL = Teaching hospital
                                   (variable == "h32b" & value == "yes") ~ 1, # FEDERAL MEDICAL CENTER
                                   (variable == "h32c" & value == "yes") ~ 1, # GENERAL HOSPITAL
                                   (variable == "h32d" & value == "yes") ~ 1, # PRIMARY HEALTHCARE CENTER
                                   (variable == "h32e" & value == "yes") ~ 1, # PRIMARY HEALTH CLINIC
                                   (variable == "h32f" & value == "yes") ~ 1, # HEALTH POST
                                   (variable == "h32g" & value == "yes") ~ 1, # OTHER PUBLIC SECTOR
                                   (variable == "h32h" & value == "yes") ~ 1, # CS PUBLIC SECTOR
                                   (variable == "h32i" & value == "yes") ~ 1, # CS PUBLIC SECTOR
                                   (variable == "h32j" & value == "yes") ~ 1, # PRIVATE HOSPITAL
                                   (variable == "h32k" & value == "yes") ~ 1, # PHARMACY
                                   (variable == "h32l" & value == "yes") ~ 1, # PRIVATE DOCTOR
                                   (variable == "h32m" & value == "yes") ~ 1, # PRIVATE CLINIC
                                   (variable == "h32n" & value == "yes") ~ 1, # Patent proprietary medicine vendor
                                   (variable == "h32o" & value == "yes") ~ 1, # MOBILE CLINIC
                                   (variable == "h32p" & value == "yes") ~ 1, # Maternity center
                                   (variable == "h32q" & value == "yes") ~ 1, # PRIVATE OTHER
                                   (variable == "h32nb" & value == "yes") ~ 1, # Faith-based clinic
                                   (variable == "h32nc" & value == "yes") ~ 1, # OTHER NGO SECTOR
                                   is.na(value) ~ NA,
                                   TRUE ~ 0)) %>%
    dplyr::group_by(survey, caseid) %>%
    dplyr::mutate(care.cnt = ifelse(all(is.na(care)), NA, sum(care, na.rm=TRUE)),
                  no.fever.cough.care.yn = ifelse(care.cnt == 0, 1, 0)) %>%
    dplyr::select(survey, caseid, care.cnt, no.fever.cough.care.yn) %>%
    distinct()

  # table(KR$h32y, useNA = "always")

  ## Child Minimum Dietary Diversity ----
  KR <- KR %>%
    mutate(
      across(c(v469e, v469f, v469x), ~ na_if(., 8), .names = "{.col}_"),
      across(c(v469e_, v469f_, v469x_), ~ coalesce(., 0)),
      totmilkf = v469e_ + v469f_ + v469x_,
      nt_fed_milk = case_when(
        (totmilkf >= 2 | m4 == "still breastfeeding") & (b19>=6 & b19<=23) ~ 1,
        (totmilkf < 2 | m4 != "still breastfeeding") & (b19>=6 & b19<=23) ~ 0,
        TRUE ~ NA_real_))
  # table(KR$nt_fed_milk, useNA = "always") #note that lots of NAs here

  ### Country Specific Food ----
  KR <- KR %>%
    # country specific foods. These can be added to the foods below based on the survey. See example for nt_root & nt_meatfish below
    mutate(food1  = case_when(v414a=="yes"  ~ 1 , v414a!="yes" ~ 0)) %>%
    mutate(food2  = case_when(v414b=="yes"  ~ 1 , v414b!="yes" ~ 0)) %>%
    mutate(food3  = case_when(v414c=="yes"  ~ 1 , v414c!="yes" ~ 0)) %>%
    mutate(food4  = case_when(v414d=="yes"  ~ 1 , v414d!="yes" ~ 0)) %>%
    mutate(nt_formula  = case_when(v411a=="yes"  ~ 1 , v411a!="yes"~ 0)) %>% # Given formula
    mutate(nt_milk  = case_when(v411=="yes"  ~ 1 , v411!="yes"~ 0)) %>% # Given other milk
    mutate(nt_liquids= case_when(v410=="yes" | v412c=="yes" | v413=="yes"  ~ 1 , v410!="yes" | v412c!="yes" | v413!="yes"  ~ 0)) %>% # Given other liquids
    mutate(nt_bbyfood  = case_when(v412a=="yes"  ~ 1 , v412a!="yes"~ 0)) %>% # Given fortified baby food
    mutate(nt_grains  = case_when(v412a=="yes" | v414e=="yes" ~ 1 , v412a!="yes" | v414e!="yes" ~ 0)) %>% # Given grains
    mutate(nt_vita = case_when(v414i=="yes" | v414j=="yes" | v414k=="yes" ~ 1 , v414i!="yes" | v414j!="yes" | v414k!="yes" ~ 0)) %>% # Given Vit A rich foods
    mutate(nt_frtveg  = case_when(v414l=="yes"  ~ 1 , v414l!="yes"~ 0)) %>% # Given other fruits or vegetables
    mutate(nt_root  = case_when(v414f=="yes" ~ 1, v414f!="yes" ~ 0)) %>% # Given tubers
    mutate(nt_nuts  = case_when(v414o=="yes"  ~ 1 , v414o!="yes"~ 0)) %>% # Given nuts or legumes
    mutate(nt_meatfish  = case_when((v414h=="yes" | v414m=="yes" | v414n=="yes") ~ 1, !(v414h=="yes" | v414m=="yes" | v414n=="yes") ~ 0)) %>% # Given meat, fish, shellfish, or poultry
    mutate(nt_eggs  = case_when(v414g=="yes"  ~ 1 , v414g!="yes"~ 0)) %>% # Given eggs
    mutate(nt_dairy  = case_when(v414p=="yes" | v414v=="yes" ~ 1 , v414p!="yes" | v414v!="yes" ~ 0)) %>% # Given dairy
    mutate(nt_solids = case_when( nt_bbyfood=="yes" | nt_grains=="yes" | nt_vita=="yes" | nt_frtveg=="yes" | nt_root=="yes" | nt_nuts=="yes" | nt_meatfish=="yes" |
                                    nt_eggs=="yes" | nt_dairy=="yes" | v414s=="yes" ~ 1 ,
                                  nt_bbyfood!="yes" | nt_grains!="yes" | nt_vita!="yes" | nt_frtveg!="yes" | nt_root!="yes" | nt_nuts!="yes" | nt_meatfish!="yes" |
                                    nt_eggs!="yes" | nt_dairy!="yes" | v414s!="yes" ~ 0) )

  # table(KR$food1, useNA = "always")

  KR <- KR %>%
    # 1. breastmilk
    mutate(group1 = case_when(m4=="still breastfeeding"  ~ 1 ,
                              m4!="still breastfeeding" ~ 0)) %>%
    #2. infant formula, milk other than breast milk, cheese or yogurt or other milk products
    mutate(group2 = case_when(nt_formula==1 | nt_milk==1 | nt_dairy==1  ~ 1 ,
                              nt_formula!=1 | nt_milk!=1 | nt_dairy!=1 ~ 0)) %>%
    #3. foods made from grains, roots, tubers, and bananas/plantains, including porridge and fortified baby food from grains
    mutate(group3  = case_when(nt_grains==1 | nt_root==1 | nt_bbyfood==1 ~ 1 , nt_grains!=1 | nt_root!=1 | nt_bbyfood!=1 ~ 0)) %>%
    #4. vitamin A-rich fruits and vegetables
    mutate(group4  = case_when(nt_vita==1  ~ 1 , nt_vita!=1 ~ 0)) %>%
    #5. other fruits and vegetables
    mutate(group5  = case_when(nt_frtveg==1 ~ 1 , nt_frtveg!=1~ 0)) %>%
    #6. eggs
    mutate(group6  = case_when(nt_eggs==1 ~ 1 , nt_eggs!=1~ 0)) %>%
    #7. meat, poultry, fish, and shellfish (and organ meats)
    mutate(group7  = case_when(nt_meatfish==1 ~ 1 , nt_meatfish!=1~ 0)) %>%
    #8. legumes and nuts
    mutate(group8  = case_when(nt_nuts==1 ~ 1 , nt_nuts!=1~ 0)) %>%
    #add the food groups
    mutate(food_nonmiss = rowSums(!is.na(select(., group1:group8))),
           foodsum = case_when(
             food_nonmiss == 0 ~ NA_real_,
             TRUE ~ rowSums(dplyr::select(., group1:group8), na.rm = TRUE))) %>%
    mutate(nt_mdd  = case_when((b19>=6 & b19<24) & foodsum<5 ~ 0 ,
                               (b19>=6 & b19<24) & foodsum>=5~ 1))
  #older surveys are 4 out of 7 food groups so the foodsum would add group2-group8 and the recode the sum for 4+ as yes

  # table(KR$foodsum, useNA = "always")

  ### Min meal frequency ----
  KR <- KR %>%
    mutate(feedings = case_when(m39 > 0 & m39 < 8 ~ totmilkf + m39, TRUE ~ NA_real_)) %>%
    mutate(nt_mmf = case_when(b19 < 6 ~ NA_real_,
                              # Breastfed children
                              (m4 == "still breastfeeding" & b19 >= 6 & b19 <= 8 & m39 >= 2 & m39 <= 7) |
                                (m4 == "still breastfeeding" & b19 >= 9 & b19 <= 23 & m39 >= 3 & m39 <= 7) ~ 1,
                              # Non-breastfed children
                              (m4 != "still breastfeeding" & b19 >= 6 & b19 <= 23 & feedings >= 4) ~ 1, TRUE ~ 0))

  # table(KR$feedings, useNA = "always") #feedings has a lot of NAs coming from m39 (BASE: Youngest children under 24 (36) months living with their mother (B5(1) = 1 & B9(1) = 0 & B19(1) < 24))

  ### Min acceptable diet ----
  KR <- KR %>%
    # Food group count
    mutate(foodsum2 = nt_grains + nt_root + nt_nuts + nt_meatfish + nt_vita + nt_frtveg + nt_eggs) %>%
    # Minimum acceptable diet
    mutate(nt_mad = case_when(b19 < 6 ~ NA_real_,
                              # Breastfed children
                              (m4 == "still breastfeeding" & nt_mdd == 1 & nt_mmf == 1) ~ 1,
                              # Non-breastfed children
                              (m4 != "still breastfeeding" & foodsum2 >= 4 & nt_mmf == 1 & totmilkf >= 2) ~ 1, TRUE ~ 0)) %>%
    mutate(no.mad = ifelse(nt_mad == 1, 0, 1))

  # table(KR$no.mad, useNA = "always")

  ######################################################################
  # 12 | MATERNAL HEALTH ----

  ## WOMAN IS OVERWEIGHT OR OBESE (ADDED BY SONDER) ----
  IR$bmi<-IR$v445/100
  IR$bmi<-ifelse(IR$bmi>90,NA,IR$bmi)
  IR<- IR %>% mutate(bmi.cat1 = case_when(
    (bmi < 16) ~ "Severely thin",
    (bmi >= 16 & bmi < 17) ~ "Moderately thin",
    (bmi >= 17 & bmi < 18.5) ~ "Mildly thin",
    (bmi >= 18.5 & bmi < 25) ~ "Normal",
    (bmi >= 25 & bmi < 30) ~ "Overweight",
    (bmi >= 30) ~ "Obese"))

  IR <- IR %>%mutate(overweight.obese = if_else(bmi.cat1 %in% c("Overweight", "Obese"), 1, 0))
  # table(IR$overweight.obese, useNA = "always")

  ## EVER TERMINATED A PREGNANCY (ADDED BY SONDER) ----
  IR$preg.termin <- IR$v228
  # table(IR$v228, useNA = "always")

  ## ANY STI'S IN THE LAST 12 MONTHS (ADDED BY SONDER) ----
  # IR<- IR %>% mutate(sti.any = case_when(
  #   v763a=="yes"~1,
  #   v763b=="yes"~1,
  #   v763c=="yes"~1,
  #   v763a=="no" & v763b=="no" & v763c=="no"~0,
  #   is.na(v763a)~ NA_integer_,
  #   is.na(v763b)~  NA_integer_,
  #   is.na(v763c)~ NA_integer_))
  # # table(IR$sti.any, useNA = "always")

  # STI'S YES NO (ADDED BY SONDER)
  IR <- IR %>% mutate(sti.yn = case_when(
    v763a == "yes" | v763b == "yes" | v763c == "yes" | v763d == "yes" ~ 1,
    is.na(v763a) & is.na(v763b) & is.na(v763c) & is.na(v763d) ~ NA_integer_,
    TRUE ~ 0 ))

  ## NEVER  TESTED FOR HIV (ADDED BY SONDER) ----
  IR<-IR %>% mutate(hiv.never.tested = case_when(
    v781=="no"~ 1,
    v781 == "yes"~0,
    is.na(v781)~NA_integer_))
  # table(IR$hiv.never.tested, useNA = "always")


  ## WHO ASSITED DELIVERY ----
  BR <- BR %>%
    mutate(across(m3a:m3n, ~ ifelse(.x == "yes", 1,
                                    ifelse(.x == "no", 0, NA_integer_))))
  BR <- BR %>%
    mutate(
      who.assisted.del = case_when(
        m3a == 1 ~ "Doctor",
        m3b == 1 ~ "Nurse/midwife",
        m3c == 1 ~ "Auxilary midwife",
        m3d == 1 | m3e == 1 | m3f == 1 ~ "Community health extension worker",
        m3g == 1 ~ "Traditional birth attendant",
        m3h == 1 | m3i == 1 | m3j == 1 | m3k == 1 | m3l == 1 | m3m == 1 ~ "Relative/other",
        m3n == 1 ~ "No one",
        m3a == NA ~ "NA - last birth >5 yrs",
        is.na(m3a) ~ "NA - last birth >5 yrs",
        TRUE ~ NA_character_))


  ## TETANUS TOXOID ----
  #Coded as none during pregnancy, to match the wording in Pathways survey.
  BR <- BR %>%
    dplyr::mutate(no.prenatal.tetnus = case_when(m1 == 0 ~ "1",
                                                 m1 %in% c(1:8) ~ "0"))

  table(BR$no.prenatal.tetnus, useNA = "ifany")

  # # FOOD INSECURITY SCALE - NEED TO F/U
  # table(HH$hfs_mod, useNA = "ifany")
  # table(HH$hfs_sev, useNA = "ifany")


  ## UNMET CONTRACEPTIVE NEED ----
  # table(IR$v626a, useNA = "always")
  IR<-IR %>%
    dplyr::mutate(unmet.fp.need = case_when(
      v626a=="unmet need for limiting" ~ "Unmet need for limiting",
      v626a=="unmet need for spacing" ~ "Unmet need for spacing",
      v626a=="no unmet need" ~ "No unmet need",
      v626a=="using for limiting" | v626a=="using for spacing" ~ "Using contraception",
      v626a=="never had sex" | v626a=="not married and no sex in last 30 days" | v626a=="infecund, menopausal"  ~ "NA (infecund, menopausal, no sex)"))
  # table(IR$unmet.fp.need, useNA = "always")

  ## UNMET NEED - BINARY ----

  IR<-IR %>%
    dplyr::mutate(unmet.fp.need_yn = case_when(
      v626a=="unmet need for limiting" | v626a=="unmet need for spacing" ~ "Yes",
      v626a=="no unmet need" | v626a=="using for limiting" | v626a=="using for spacing" ~ "No",
      v626a=="never had sex" | v626a=="not married and no sex in last 30 days" | v626a=="infecund, menopausal"  ~ NA))
  # table(IR$unmet.fp.need_yn, useNA = "ifany") #2122 NAs


  ######################################################################
  # 13 | SUMMARIZE

  # INDIVIDUAL
  ir.out <- IR

  # BIRTH
  br.out <- BR %>%
    group_by(survey, caseid) %>%
    dplyr::summarize(b.rost = length(caseid),
                     u1mort.cnt = ifelse(all(is.na(u1mort)), NA, sum(u1mort, na.rm=TRUE)),
                     u5mort.cnt = ifelse(all(is.na(u5mort)), NA, sum(u5mort, na.rm=TRUE)),
                     micronutrient.12m.cnt = ifelse(all(is.na(micronutrient.12m)), NA, sum(micronutrient.12m, na.rm=TRUE)),
                     home.birth.cnt = ifelse(all(is.na(home.birth)), NA, sum(home.birth, na.rm=TRUE))) %>%
    mutate(u1mort.yn = ifelse(u1mort.cnt > 0, 1, 0),
           u5mort.yn = ifelse(u5mort.cnt > 0, 1, 0),
           micronutrient.12m = ifelse(micronutrient.12m.cnt > 0, 1, 0),
           home.birth.yn = ifelse(home.birth.cnt > 0, 1, 0)) %>%
    dplyr::select(survey, caseid, u1mort.cnt, u5mort.cnt, home.birth.cnt, u1mort.yn, u5mort.yn, home.birth.yn, micronutrient.12m) %>%
    dplyr::mutate(micronutrient.12m = case_when(
      micronutrient.12m == 1 ~ "Yes",
      micronutrient.12m == 0 ~ "No",
      is.na(micronutrient.12m) ~ "No response"
    ))
  # table(br.out$micronutrient.12m)

  # KID
  kr.out <- KR %>%
    group_by(survey, caseid) %>%
    dplyr::summarize(k.rost = length(caseid),
                     no.breastfeed.cnt = ifelse(all(is.na(no.breastfeed.n)), NA, sum(no.breastfeed.n, na.rm=TRUE)),
                     no.breastfeed2.cnt = ifelse(all(is.na(no.breastfeed2.n)), NA, sum(no.breastfeed2.n, na.rm=TRUE)),
                     ovrwgt.cnt = ifelse(all(is.na(ovrwgt)), NA, sum(ovrwgt, na.rm=TRUE)),
                     undwgt.cnt = ifelse(all(is.na(undwgt)), NA, sum(undwgt, na.rm=TRUE)),
                     stunt.cat2.cnt = ifelse(all(is.na(stunt.cat2)), NA, sum(stunt.cat2, na.rm=TRUE)),
                     waste.cat2.cnt = ifelse(all(is.na(waste.cat2)), NA, sum(waste.cat2, na.rm=TRUE)),
                     meas.full.cnt = ifelse(all(is.na(measles.full)), NA, sum(measles.full, na.rm=TRUE)),
                     polio.full.cnt = ifelse(all(is.na(polio.full)), NA, sum(polio.full, na.rm=TRUE)),
                     dpt.full.cnt = ifelse(all(is.na(dpt.full)), NA, sum(dpt.full, na.rm=TRUE)),
                     zerodose.cnt = ifelse(all(is.na(zero.dose)), NA, sum(zero.dose, na.rm=TRUE)),
                     diarrhea.cnt = ifelse(all(is.na(diarrhea.2wks)), NA, sum(diarrhea.2wks, na.rm=TRUE)),
                     fever.cnt = ifelse(all(is.na(fever.2wks)), NA, sum(fever.2wks, na.rm=TRUE)),
                     cough.cnt = ifelse(all(is.na(cough.2wks)), NA, sum(cough.2wks, na.rm=TRUE)),
                     chest.prob.cnt = ifelse(all(is.na(chest.prob)), NA, sum(chest.prob, na.rm=TRUE)),
                     diff.breath.cnt = ifelse(all(is.na(diff.breath)), NA, sum(diff.breath, na.rm=TRUE)),
                     ari.cnt = ifelse(all(is.na(ari)), NA, sum(ari, na.rm=TRUE)),
                     no.ebf.cnt = ifelse(all(is.na(no.ebf.n)), NA, sum(no.ebf.n, na.rm=TRUE)),
                     zero.dose.operational.cnt = ifelse(all(is.na(zero.dose.operational)), NA, sum(zero.dose.operational, na.rm=TRUE)),
                     zero.dose.4vax.12m.cnt = ifelse(all(is.na(zero.dose.4vax.12m)), NA, sum(zero.dose.4vax.12m, na.rm = TRUE)),
                     not.full.vax.schedule.24m.cnt = ifelse(all(is.na(not.full.vax.schedule.24m)), NA, sum(not.full.vax.schedule.24m, na.rm=TRUE)),
                     not.full.vax.schedule.12m.cnt = ifelse(all(is.na(not.full.vax.schedule.12m)), NA, sum(not.full.vax.schedule.12m, na.rm=TRUE)),
                     full.vax.schedule.12m.cnt = ifelse(all(is.na(full.vax.schedule.12m)), NA, sum(full.vax.schedule.12m, na.rm = TRUE)),
                     full.vax.schedule.24m.cnt = ifelse(all(is.na(full.vax.schedule.24m)), NA, sum(full.vax.schedule.24m, na.rm = TRUE)),
                     measles.all.cnt = ifelse(all(is.na(measles.all)), NA, sum(measles.all, na.rm=TRUE)),
                     dpt.all.cnt = ifelse(all(is.na(dpt.all)), NA, sum(dpt.all, na.rm=TRUE)),
                     polio.all.cnt = ifelse(all(is.na(polio.all)), NA, sum(polio.all, na.rm=TRUE)),
                     bcg.all.cnt = ifelse(all(is.na(bcg.all)), NA, sum(bcg.all, na.rm=TRUE)),
                     hepb.all.cnt = ifelse(all(is.na(hepb.all)), NA, sum(hepb.all, na.rm = TRUE)),
                     pentavalent.all.cnt = ifelse(all(is.na(pentavalent.all)), NA, sum(pentavalent.all, na.rm = TRUE)),
                     pneumo.all.cnt = ifelse(all(is.na(pneumo.all)), NA, sum(pneumo.all, na.rm = TRUE)),
                     rota.all.cnt = ifelse(all(is.na(rota.all)), NA, sum(rota.all, na.rm = TRUE)),
                     yellowfever.all.cnt = ifelse(all(is.na(yellowfever.all)), NA, sum(yellowfever.all, na.rm = TRUE)),
                     meningitis.all.cnt = ifelse(all(is.na(meningitis.all)), NA, sum(meningitis.all, na.rm = TRUE)),
                     basic.antigen.full.12m.cnt = ifelse(all(is.na(basic.antigen.full.12m)), NA, sum(basic.antigen.full.12m, na.rm = TRUE)),
                     no.mad.cnt = ifelse(all(is.na(no.mad)), NA, sum(no.mad, na.rm = TRUE))) %>%

    dplyr::mutate(no.breastfeed.yn = ifelse(no.breastfeed.cnt > 0, 1, 0),
                  no.breastfeed2.yn = ifelse(no.breastfeed2.cnt > 0, 1, 0),
                  ovrwgt.yn = ifelse(ovrwgt.cnt > 0, 1, 0),
                  undwgt.yn = ifelse(undwgt.cnt > 0, 1, 0),
                  stunt.cat2.yn = ifelse(stunt.cat2.cnt > 0, 1, 0),
                  waste.cat2.yn = ifelse(waste.cat2.cnt > 0, 1, 0),
                  meas.full.yn = ifelse(meas.full.cnt > 0, 1, 0),
                  polio.full.yn = ifelse(polio.full.cnt > 0, 1, 0),
                  dpt.full.yn = ifelse(dpt.full.cnt > 0, 1, 0),
                  zerodose.yn = ifelse(zerodose.cnt > 0, 1, 0),
                  fever.yn = ifelse(fever.cnt > 0, 1, 0),
                  cough.yn = ifelse(cough.cnt > 0, 1, 0),
                  diff.breath.yn = ifelse(diff.breath.cnt > 0, 1, 0),
                  chest.prob.yn = ifelse(chest.prob.cnt > 0, 1, 0),
                  zero.dose.operational.yn = ifelse(zero.dose.operational.cnt > 0, 1, 0),
                  ari.yn = ifelse(ari.cnt > 0, 1, 0),
                  no.ebf.yn = ifelse(no.ebf.cnt>0, 1, 0),
                  pos.measles.all.yn = ifelse(measles.all.cnt > 0, 1, 0),
                  pos.dpt.all.yn = ifelse(dpt.all.cnt > 0, 1, 0),
                  pos.polio.all.yn = ifelse(polio.all.cnt > 0, 1, 0),
                  pos.bcg.all.yn = ifelse(bcg.all.cnt > 0, 1, 0),
                  pos.hepb.all.yn = ifelse(hepb.all.cnt > 0, 1, 0),
                  pos.pentavalent.all.yn = ifelse(pentavalent.all.cnt > 0, 1, 0),
                  pos.pneumo.all.yn = ifelse(pneumo.all.cnt > 0, 1, 0),
                  pos.rota.all.yn = ifelse(rota.all.cnt > 0, 1, 0),
                  pos.yellowfever.all.yn = ifelse(yellowfever.all.cnt > 0, 1, 0),
                  pos.meningitis.all.yn = ifelse(meningitis.all.cnt > 0, 1, 0),
                  pos.basic.antigen.full.12m.yn = ifelse(basic.antigen.full.12m.cnt > 0, 1, 0),
                  not.full.vax.schedule.24m.yn = ifelse(not.full.vax.schedule.24m.cnt > 0, 1, 0),
                  not.full.vax.schedule.12m.yn = ifelse(not.full.vax.schedule.12m.cnt > 0, 1, 0),
                  pos.full.vax.schedule.12m.yn = ifelse(full.vax.schedule.12m.cnt > 0, 1, 0),
                  pos.full.vax.schedule.24m.yn = ifelse(full.vax.schedule.24m.cnt > 0, 1, 0),
                  zero.dose.4vax.12m.yn = ifelse(zero.dose.4vax.12m.cnt > 0, 1, 0),
                  no.mad.yn = ifelse(no.mad.cnt > 0, 1, 0)) %>%
    dplyr::select(survey, caseid,
                  no.breastfeed.cnt, no.breastfeed2.cnt, ovrwgt.cnt, undwgt.cnt, stunt.cat2.cnt, waste.cat2.cnt, meas.full.cnt, dpt.full.cnt, polio.full.cnt, zerodose.cnt, diarrhea.cnt, fever.cnt, cough.cnt, diff.breath.cnt, chest.prob.cnt, ari.cnt, no.ebf.cnt,
                  no.breastfeed.yn, no.breastfeed2.yn, ovrwgt.yn, undwgt.yn, stunt.cat2.yn, waste.cat2.yn, meas.full.yn, dpt.full.yn, polio.full.yn, zerodose.yn, fever.yn, cough.yn, diff.breath.yn, chest.prob.yn, ari.yn, no.ebf.yn,
                  pos.measles.all.yn, pos.dpt.all.yn, pos.polio.all.yn, pos.bcg.all.yn, pos.hepb.all.yn, pos.pentavalent.all.yn, pos.pneumo.all.yn, pos.rota.all.yn, pos.yellowfever.all.yn, pos.meningitis.all.yn, pos.basic.antigen.full.12m.yn, pos.full.vax.schedule.12m.yn,
                  pos.full.vax.schedule.24m.yn, zero.dose.4vax.12m.yn, zero.dose.operational.yn, not.full.vax.schedule.24m.yn, not.full.vax.schedule.12m.yn, measles.all.cnt, dpt.all.cnt, polio.all.cnt, bcg.all.cnt, hepb.all.cnt, pentavalent.all.cnt, pneumo.all.cnt, rota.all.cnt, yellowfever.all.cnt,
                  meningitis.all.cnt, basic.antigen.full.12m.cnt, full.vax.schedule.12m.cnt, full.vax.schedule.24m.cnt, zero.dose.4vax.12m.cnt, no.mad.yn, no.mad.cnt) %>%
    base::merge(kr_var, by=c("survey", "caseid"), all.x=TRUE)

  # JOIN TOGETHER
  outcomes <- ir.out %>%
    base::merge(kr.out, by=c("survey", "caseid"), all.x=TRUE) %>%
    base::merge(br.out, by=c("survey", "caseid"), all.x=TRUE) %>%
    base::merge(stl.out, by=c("caseid"), all.x=TRUE)


  # DROP SOME INTERMEDIARY VARIABLES
  drop_vars <- c("anc.1st.mean", "anc.mean")
  outcomes <- outcomes %>% dplyr::select(-all_of(drop_vars))

  return(outcomes)

}




