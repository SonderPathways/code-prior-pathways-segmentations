pacman::p_load(RODBC, dplyr, tidyverse, openxlsx, tidyselect)
conn <- odbcConnect("IDMAzureDatabricks_DSN") 
options(max.print=1000000)

df1 <- sqlQuery(conn, "SELECT * FROM idm_general.pathways.pathways_bihar_ityp_1_clean") 
df1_ <- sqlQuery(conn, "SELECT * FROM idm_general.pathways.pathways_bihar_raw") 
#woman <- subset(df1, df1$ITYP==1)
table(df1_$ITYP)
woman <- subset(df1_, df1_$ITYP==1)
colnames(woman)

#NOTE: within each constructs, variables are derived based on alphabetical order of original varnames 


###################################
### HH ROSTER-RELATED VARIABLES ### 
###################################

### FAMILY SIZE: TOTAL HH MEMBERS 
S03 <- woman %>%dplyr::select(starts_with("S03_"))
apply(S03, 2, table) #quick tab to see if any column has no response
woman$family.size <- rowSums(cbind(woman$S03_1 , woman$S03_3 , woman$S03_4 , woman$S03_5 , woman$S03_6 ,
                                   woman$S03_7_SP , woman$S03_8_SP , 
                                   woman$S03_9 , woman$S03_10 , 
                                   woman$S03_11_SP , woman$S03_12_SP , woman$S03_13_SP , woman$S03_14_SP , 
                                   woman$S03_15 , woman$S03_16 , 
                                   woman$S03_17_SP , woman$S03_18_SP , woman$S03_19_SP , woman$S03_20_SP , woman$S03_21_SP , woman$S03_22_SP ,
                                   woman$S03_23 , woman$S03_24 ,
                                   woman$S03_25_SP , woman$S03_26_SP , woman$S03_27_SP , woman$S03_28_SP), na.rm=T)
table(woman$family.size)
#re-categorize to <=4, 5, 6, 7, 8, 8+
woman$family.size[woman$family.size<4] <- 4 
woman$family.size[woman$family.size>8] <- 9
table(woman$family.size)

### 1. COHABITING ADULTS & CHILDREN
table(woman$S03)
S03a <- woman %>%dplyr::select(starts_with("S03a_"))
apply(S03a, 2, table) #quick view about what hh memebers' age look like 
#decide whether each column is an adult (anyone who is >=18 years); if yes, count the sum
adult_count <- function(row) {
  # Count the number of elements >=18
  result <- sum(row >=18, na.rm = T)
  return(result)
}
woman$cohab.adultsum <-  apply(dplyr::select(woman, (is.numeric) & starts_with("S03a_")), 1, adult_count)
table(woman$cohab.adultsum)
#recategorize: <=1, 3, 4, 5+
woman$cohab.adultsum[woman$cohab.adultsum==1] <- 2
woman$cohab.adultsum[woman$cohab.adultsum>5] <- 5
table(woman$cohab.adultsum)
#decide whether each column is a child (anyone who is <12 years); if yes, count the sum
child_count <- function(row) {
  # Count the number of elements <12
  result <- sum(row <12, na.rm = T)
  return(result)
}
woman$cohab.childsum <-  apply(dplyr::select(woman, (is.numeric) & starts_with("S03a_")), 1, child_count)
table(woman$cohab.childsum)
#recategorize: 1, 2, 3, 4, 5+
woman$cohab.childsum[woman$cohab.childsum>5] <- 5
table(woman$cohab.childsum)
#decide whether each column is her son/daughter (regardless of age)
table(woman$S03_7) #sons 
table(woman$S03_8) #daughters 
#woman$test <- woman$S03_7 + woman$S03_8 #all women have a bio kid 
woman$son1 <- ifelse(( !is.na(woman$S03a_7_1)),1,0)
woman$son2 <- ifelse(( !is.na(woman$S03a_7_2)),1,0)
woman$son3 <- ifelse(( !is.na(woman$S03a_7_3)),1,0)
woman$son4 <- ifelse(( !is.na(woman$S03a_7_4)),1,0)
woman$son5 <- ifelse(( !is.na(woman$S03a_7_5)),1,0)
woman$dau1 <- ifelse(( !is.na(woman$S03a_8_1)),1,0)
woman$dau2 <- ifelse(( !is.na(woman$S03a_8_2)),1,0)
woman$dau3 <- ifelse(( !is.na(woman$S03a_8_3)),1,0)
woman$dau4 <- ifelse(( !is.na(woman$S03a_8_4)),1,0)
woman$dau5 <- ifelse(( !is.na(woman$S03a_8_5)),1,0)
##cohabiting adults/children
woman <- woman %>% mutate(cohab.sonsum = rowSums(across(starts_with('son')), na.rm = TRUE))
woman <- woman %>% mutate(cohab.dausum = rowSums(across(starts_with('dau')), na.rm = TRUE))
woman$cohab.biochildsum <- woman$cohab.sonsum + woman$cohab.dausum
table(woman$cohab.biochildsum)
sum(is.na(woman$cohab.biochildsum))
##descriptives: make sense? 
table(woman$cohab.adultsum)
table(woman$cohab.childsum)
table(woman$cohab.sonsum)
table(woman$cohab.dausum)
table(woman$cohab.biochildsum)
summary(woman$cohab.adultsum)
summary(woman$cohab.childsum)
summary(woman$cohab.sonsum)
summary(woman$cohab.dausum)
summary(woman$cohab.biochildsum)
sd(woman$cohab.adultsum)
sd(woman$cohab.childsum)
sd(woman$cohab.sonsum)
sd(woman$cohab.dausum)
sd(woman$cohab.biochildsum)

### 2. WOMEN BEING PRIMARY CAREGIVER FOR NON_BIOLOGICAL CHILDREN 
#check missing data 
S05 <- tidyselect::vars_select(names(woman), starts_with('S05_', ignore.case = TRUE)) 
for (i in S05) {
  print(i)
  print(table(woman[[i]]))
}
##primary caregiver to other non-biological children 
#If any S05 column==1 then yes; denominator: woman who have a non-biological child living with her 
woman <- woman %>% 
  mutate(S05any = case_when(if_any(starts_with("S05_"), ~ .x == 1) ~ 1))
table(woman$S05any)
# for the purpose of analysis, I make the denominator as all women 
woman <- woman %>% 
  mutate(care.nobiochild = case_when( S05any==1 ~ 1,
                                     is.na(S05any) ~ 0))
table(woman$care.nobiochild)
prop.table(table(woman$care.nobiochild))

### 3. WORK  
#3.1 anyone working in the household? 
E08 <- woman %>%dplyr::select(starts_with("E08"))
apply(E08, 2, table)
woman$her.work <- ifelse((woman$E08_29==1),1,0) 
table(woman$her.work)
#total other hh members who has work
work_count <- function(row) {
  # Count the number of elements <12
  result <- sum(row ==1, na.rm = T)
  return(result)
}
woman$other.work <-  apply(dplyr::select(woman, (is.numeric) & starts_with("E08_") & !E08_29), 1, work_count)
table(woman$other.work)
#re-categorize to <=1, 2, 3, 3+
woman$other.work[woman$other.work==0] <- 1 
woman$other.work[woman$other.work>=4] <- 4
table(woman$other.work)

#3.2 work stability: Total household members having a job that is throughout the year?
E09 <- tidyselect::vars_select(names(woman), starts_with('E09', ignore.case = TRUE)) 
woman %>% dplyr::select(tidyselect::any_of(E09))
# for each i (E09_ variable), create a 0/1 binary to indicate whether that person is working regularly
for (i in E09) {
  print(i)
  print(table(woman[[i]]))
  woman[[paste("_", i, sep = "")]] <- ifelse((woman[[i]]==1),1,0)  
}
#summing all the _E09 variables to calculate the sum of working people in the household 
woman <- woman %>%
  mutate(house.worksum = rowSums(dplyr::select(., starts_with("_E09")), na.rm=T) * NA ^ (rowSums(!is.na(dplyr::select(., starts_with("_E09")))) == 0))
#see this to make sure that sum=0 does not pick up all NA obs: https://stackoverflow.com/questions/33672059/rowsums-with-all-na 
sum(is.na(woman$house.worksum)) #there are 7 woman who did not gave any answer to the E09 questions
table(woman$house.worksum)
#regroup to 0, 1, 2, 3
woman$house.worksum[woman$house.worksum > 3] <- 3
table(woman$house.worksum)
prop.table(table(woman$house.worksum))

#woman herself working throughout the year? 
table(woman$E09_29)
sum(is.na(woman$E09_29))
woman$house.workher <- ifelse((woman$E09_29==1),1,0) 
woman$house.workher[is.na(woman$E09_29)] <- NA
table(woman$house.workher)

#3.3 type of work in the household? will come back in the future to recode this!!!
#[1]=cultivator 
E10_1 <- woman %>%dplyr::select(starts_with("E10_1_"))
apply(E10_1, 2, table)
#[2] Agricultrual labourer
E10_2 <- woman %>%dplyr::select(starts_with("E10_2_"))
apply(E10_2, 2, table)
#[3] Non-agricultural labourer (Skilled)
E10_3 <- woman %>%dplyr::select(starts_with("E10_3_"))
apply(E10_3, 2, table)
#[4] Non-agricultural labourer (Unskilled)
E10_4 <- woman %>%dplyr::select(starts_with("E10_4_"))
apply(E10_4, 2, table)
#[5] Business / Petty trader / Self employed
E10_5 <- woman %>%dplyr::select(starts_with("E10_5_"))
apply(E10_5, 2, table)
#[6] Salaried Employee Government - VERY SMALL CATEGORY
E10_6 <- woman %>%dplyr::select(starts_with("E10_6_"))
apply(E10_6, 2, table)
#[7] Salaried Employee Private
E10_7 <- woman %>%dplyr::select(starts_with("E10_7_"))
apply(E10_7, 2, table)
#[98] Other, specify
E10_98 <- woman %>%dplyr::select(starts_with("E10_98_"))
apply(E10_98, 2, table)
#woman herself: what kind of job? agricultural versus others? 
table(E10_1$E10_1_29)
table(E10_2$E10_2_29)
table(E10_3$E10_3_29)
table(E10_4$E10_4_29)
table(E10_5$E10_5_29)
table(E10_6$E10_6_29)
table(E10_7$E10_7_29)



### 4. HOUSEHOLD HEAD
table(woman$S07)
woman$head.female <- ifelse((woman$S07==29 | woman$S07==3 | woman$S07==4),1,0)
table(woman$head.female)
table(woman$female_head_household) # consistent with the pre-constructed variable 
prop.table(table(woman$head.female))

### 5. FEMALE INFLUENCER? (who can help when child is sick?) 
table(woman$S03_3[!is.na(woman$S08)]) #mother
table(woman$S03_4[!is.na(woman$S08)]) #mother-in-law
table(woman$S03_13[!is.na(woman$S08)]) #sister
table(woman$S03_14[!is.na(woman$S08)]) #sister-in-law
table(woman$S03_18[!is.na(woman$S08)]) #grandma
table(woman$S03_22[!is.na(woman$S08)]) #aunt - no 'yes' response
table(woman$S03_24[!is.na(woman$S08)]) #female cousin - no 'yes' response
table(woman$S03_28[!is.na(woman$S08)]) #other adult female
table(woman$S09)
#create a binary variable 
#0=no female in HH/there is female, but no female influencer
#1=yes (MOTSLY mother-in-law, n=1303, >95%)
woman <- woman %>% 
  mutate(female.help = case_when(
    S08 == 2 | is.na(S08) ~ 0,
    S09 == "99" ~ 0,
    S08 == 1 ~ 1,
    S09 == "14_1" | S09 == "14_3" | S09 == "18_1" | S09 == "22_1" | S09 == "28_1" |
    S09 == "3" | S09 == "4"  ~ 1
      ))
table(woman$female.help)
prop.table(table(woman$female.help))

### 6. CHILD HEALTH DECISIONS (which household member can decide?)
A02a <- woman %>%dplyr::select(starts_with("A02a_"), A02b)
apply(A02a, 2, table)
#first, can woman herself decide? 
woman$decide.child_her <- ifelse((woman$A02a_29==1),1,0)
table(woman$decide.child_her)
prop.table(table(woman$decide.child_her))
#how many adults can decide, EXCEPT woman herself 
woman <- woman %>% 
  mutate(
    decide.child_hh = dplyr::select(., starts_with("A02a_") & !A02a_29) %>% rowSums()
  )
table(woman$decide.child_hh)
#recategorize to 0, 1, 2, 3, 3+
woman$decide.child_hh[woman$decide.child_hh>4] <- 4
table(woman$decide.child_hh)
prop.table(table(woman$decide.child_hh)) # the NO category is very small
#who decide the MOST? 
table(woman$A02b)
A02a1 <- subset(A02a, is.na(A02b))
 #missing N=473, when only one A02a is selected 
#see among these 473, who is in charge of child health?
apply(A02a1, 2, table) #most is A02a_1, husband 
#so let's create a 5 categorical variable decide.child_most: husband, woman herself, mom-in-law, dad-in law, other adults 
woman <- woman %>% 
  mutate(decide.child_most = case_when(
    A02a_1 == 1 & is.na(A02b) ~ 1, #only one hh member is selected, husband can decide
    A02a_29 == 1 & is.na(A02b) ~ 2,  #only one hh member is selected, woman herself can decide
    A02a_4 == 1 & is.na(A02b) ~ 3, #only one hh member is selected, mom-in-law can decide
    A02a_6 == 1 & is.na(A02b) ~ 4,  #only one hh member is selected, dad-in-law can decide   
    A02b == "1" ~ 1,
    A02b == "29" ~ 2,
    A02b == "4" ~ 3,
    A02b == "6" ~ 4,
  ))
table(woman$decide.child_most)
prop.table(table(woman$decide.child_most))
sum(is.na(woman$decide.child_most))
woman$decide.child_most[is.na(woman$decide.child_most)] <- 5
table(woman$decide.child_most)
prop.table(table(woman$decide.child_most))

### 7. HH BIG DECISIONS (which household member can decide?)
A03a <- woman %>%dplyr::select(starts_with("A03a_"), A03b)
apply(A03a, 2, table)
#first, can woman herself decide? 
woman$decide.big_her <- ifelse((woman$A03a_29==1),1,0)
table(woman$decide.big_her)
prop.table(table(woman$decide.big_her))
#how many adults can decide, EXCEPT woman herself 
woman <- woman %>% 
  mutate(
    decide.big_hh = dplyr::select(., starts_with("A03a_") & !A03a_29) %>% rowSums()
  )
table(woman$decide.big_hh)
#re-categorize to 0, 1, 2, 3+
woman$decide.big_hh[woman$decide.big_hh>3] <- 3
table(woman$decide.big_hh)
#who decide the MOST? 
table(woman$A03b)
A03a1 <- subset(A03a, is.na(A03b))
#missing N=473, when only one A02a is selected 
#see among these 473, who is in charge of child health?
apply(A03a1, 2, table) #most is A02a_1, husband 
#so let's create a 5 categorical variable decide.child_most: husband, woman herself, mom-in-law, dad-in law, other adults 
woman <- woman %>% 
  mutate(decide.big_most = case_when(
    A03a_1 == 1 & is.na(A03b) ~ 1, #only one hh member is selected, husband can decide
    A03a_29 == 1 & is.na(A03b) ~ 2,  #only one hh member is selected, woman herself can decide
    A03a_4 == 1 & is.na(A03b) ~ 3, #only one hh member is selected, mom-in-law can decide
    A03a_6 == 1 & is.na(A03b) ~ 4,  #only one hh member is selected, dad-in-law can decide    
    A03b == "1" ~ 1,
    A03b == "29" ~ 2,
    A03b == "4" ~ 3,
    A03b == "6" ~ 4,
  ))
table(woman$decide.big_most)
prop.table(table(woman$decide.big_most))
sum(is.na(woman$decide.big_most))
woman$decide.big_most[is.na(woman$decide.big_most)] <- 5
table(woman$decide.big_most)
prop.table(table(woman$decide.big_most))

### 8. SOCIAL NETWORK SUPPORT: WITHIN & BEYOND THE HOUSEHOLD 
table(woman$A04_add)
table(woman$A04_add_R)
table(woman$A04_add_F)
table(woman$A04a)
table(woman$A04a_1)
table(woman$A04a_2)
#view(woman$Label_A04_s)
#quick tab to see what these variables look like 
A04 <- woman %>%dplyr::select(starts_with("A04_") & !A04_add & !A04_add_R & !A04_add_F)
apply(A04, 2, table)
#decide whether each A04 column is an adult in the FIRST circle; if yes, count the sum
first_count <- function(row) {
  # Count the number of elements >=18
  result <- sum(row == 1, na.rm = T)
  return(result)
}
woman$support.sum <-  apply(dplyr::select(woman, (is.numeric) 
                                          & starts_with("A04_") 
                                          & !A04_add & !A04_add_R & !A04_add_F), 1, first_count)
table(woman$support.sum)
sum(is.na(woman$support.sum))
#a binary categorization <=1 versus >1
woman$support.sum[woman$support.sum==0] <- 1
woman$support.sum[woman$support.sum>1] <- 2
table(woman$support.sum)
#sum of supports outside of hh 
woman$support.nonhh_s <-  apply(dplyr::select(woman, (is.numeric) & starts_with("A04_s")), 1, first_count)
woman$support.nonhh_1 <-  apply(dplyr::select(woman, (is.numeric) & starts_with("A04_1_")), 1, first_count)
table(woman$support.nonhh_s)
sum(is.na(woman$support.nonhh_s))
table(woman$support.nonhh_1)
sum(is.na(woman$support.nonhh_1))
woman$support.nonhh <-  woman$support.nonhh_s + woman$support.nonhh_1
table(woman$support.nonhh)
woman$support.nonhh[woman$support.nonhh==2 | woman$support.nonhh==3 
                     | woman$support.nonhh==4 | woman$support.nonhh==5 ] <- 1
table(woman$support.nonhh)
#sum of supports within hh 
woman$support.hh <-  apply(dplyr::select(woman, A04_1: A04_28_1), 1, first_count)
table(woman$support.hh)
sum(is.na(woman$support.hh))
#a binary categorization <=1 versus >1
woman$support.hh[woman$support.hh==0] <- 1
woman$support.hh[woman$support.hh>1] <- 2
table(woman$support.hh)

### 9. PLACE OF DELIVERY OF HER CHILDREN 
C21 <- woman %>%dplyr::select(starts_with("C21_7_") | starts_with("C21_8_"))
apply(C21, 2, table)
#just generate a simple binary to see if woman has EVER deliver at home? 
woman$deliver.home <- ifelse((woman$C21_7_1==1 |woman$ C21_7_2==1 | woman$C21_7_3==1 | woman$C21_7_4==1 | woman$C21_7_5==1 | 
                              woman$C21_8_1==1 | woman$C21_8_2==1 | woman$C21_8_3==1 | woman$C21_8_4==1 | woman$C21_8_5==1),1,0) 
table(woman$deliver.home)
woman$deliver.home[is.na(woman$deliver.home)] <- 0
woman$deliver.home[woman$C21_8_5_SP=="Home" | woman$C21_8_1_SP=="AT HOME"] <- 0
table(woman$deliver.home)

### 10. ANY COHAB CHILD (including non-bio ones) HAS BEEN HOSPITALIZED FOR THE PAST YEAR? 
C26 <- woman %>%dplyr::select(starts_with("C26"))
apply(C26, 2, table)
apply(C26, 2, class)
hospital_count <- function(row) {
  # Count the number of elements >=18
  result <- sum(row == 1, na.rm = T)
  return(result)
}
woman$child.hosp_sum <-  apply(dplyr::select(woman, where(is.numeric) & starts_with("C26")), 1, hospital_count)
table(woman$child.hosp_sum)
sum(is.na(woman$child.hosp_sum))
#ANY child ever hospitalized? 
woman$child.hosp <- ifelse((woman$child.hosp_sum ==0),0,1) 
table(woman$child.hosp)
#her biological children 
woman$son.hosp_sum <-  apply(dplyr::select(woman, starts_with("C26_7")), 1, hospital_count)
woman$dau.hosp_sum <-  apply(dplyr::select(woman, starts_with("C26_8")), 1, hospital_count)
table(woman$son.hosp_sum)
table(woman$dau.hosp_sum)
woman$biochild.hosp <- ifelse((woman$son.hosp_sum >0 | woman$dau.hosp_sum >0),1,0) 
table(woman$biochild.hosp)


######################
### SOCIAL SUPPORT ###
######################

###COMMUNITY HEALTH WORKER? 
table(woman$A05)
table(woman$A06)
sum(is.na(woman$A06)) #correct: missing=29 (who dont have chw living around)
woman <- woman %>% 
  mutate(support.chw = case_when(
    A06==1 ~ 1,
    A06==2 ~ 2,
    A06==3 ~ 3,
    A06==4 | A05==2 ~ 4 #no chw, or outside of the trust circle
  ))
table(woman$support.chw)

### Anganwadi Worker/AWW
table(woman$A07)
table(woman$A08)
sum(is.na(woman$A08)) #correct: missing=52 (who dont have AWW living around)
woman <- woman %>% 
  mutate(support.aww = case_when(
    A08==1 ~ 1,
    A08==2 ~ 2,
    A08==3 ~ 3,
    A08==4 | A07==2 ~ 4 #no aww, or outside of the trust circle
  ))
table(woman$support.aww)

###HUSBAND SUPPORT 
woman <- woman %>% mutate(support.his = rowMeans(across(starts_with('A091')), na.rm = TRUE))
summary(woman$support.his)
woman$support.his_2 <- as.numeric(cut_number(woman$support.his,2))
table(woman$support.his_2)

#missing is truly missing 
# A09 <- woman %>%select(starts_with("A09"), support.his)
# support.his <- subset(A09, is.na(support.his))

###NON-HUSBAND SUPPORT (CHOOSE A RANDOM "First circle" response)
woman <- woman %>% mutate(support.other = rowMeans(across(starts_with('A092')), na.rm = TRUE))
summary(woman$support.other)

### COMMUNITY BELONGING
woman <- woman %>% mutate(support.comm = rowMeans(across(starts_with('A10')), na.rm = TRUE))
summary(woman$support.comm)
woman$support.comm_2 <- as.numeric(cut_number(woman$support.comm,2))
table(woman$support.comm_2)

#################
### Education ###
#################
table(woman$E07a)
table(woman$E07b)
table(woman$E07c)
table(woman$E07d)
woman$edulevel.her <- woman$E07a 
woman$eduyrs.her <- woman$E07b
woman$edulevel.his <- woman$E07c
woman$eduyrs.his <- woman$E07d
table(woman$edulevel.her)
prop.table(table(woman$edulevel.her))
table(woman$edulevel.his)
prop.table(table(woman$edulevel.his))
table(woman$eduyrs.her)
table(woman$eduyrs.his)
woman$eduyrs.his[woman$eduyrs.his == 97] <- NA
table(woman$eduyrs.his)
summary(woman$eduyrs.her)
sd(woman$eduyrs.her, na.rm=TRUE)
summary(woman$eduyrs.his)
sd(woman$eduyrs.his, na.rm=TRUE)
#woman education higher than husband? 
woman$edu.higher <- ifelse((woman$eduyrs.her > woman$eduyrs.his),1,0) 
table(woman$edu.higher)
prop.table(table(woman$edu.higher))


####################
### Child health ###
####################


###RECOGNITION OF CHILD ILLNESS
woman$child.sick <- woman$B061
table(woman$child.sick)
prop.table(table(woman$child.sick))



########################
### MNCH EXPERIENCES ###
########################

### PREGNANCY PREARENESS
table(woman$C14)
woman$preg.know <- ifelse((woman$C14==1),1,0)
table(woman$preg.know)
prop.table(table(woman$preg.know))

### ANC VISITS 
#total number of visits during last preg 
table(woman$C17)
woman$anc.total <- woman$C17
woman$anc.total[woman$C17==97] <- NA
table(woman$anc.total)
#the start of ANC visits
table(woman$C18)
woman$anc.start <- woman$C18

###LAST PREG HIGH RISK? 
table(woman$C19_1)
table(woman$C19_2)
table(woman$C19_3)
table(woman$C19_4)
#yes=either 1, no=all no, don't know= all don't know 
woman <- woman %>% 
  mutate(preg.risk = case_when(
    C19_1 == 1 | C19_2 == 1 | C19_3 == 1 | C19_4 == 1 ~ 1,
    C19_1 == 2 & C19_2 == 2 & C19_3 == 2 & C19_4 == 2 ~ 2,
    C19_1 == 97 & C19_2 == 97 & C19_3 == 97 & C19_4 == 97 ~ NA
  ))
table(woman$preg.risk)
sum(is.na(woman$preg.risk))
prop.table(table(woman$preg.risk))

### PREFERED SITE OF BIRTH DELIVERY
#1=home, 2=private hospital, 3=public hospital 
sum(is.na(woman$C22))
table(woman$C22)
class(woman$C22)
table(woman$C22_SP)
#view(woman$C22_SP[woman$C22==98])
#anywhere safe
#PAC
#Mother's house -> recode to "home"
#I thought Patna
#thought of having a baby in my mother's house  -> recode to "home"
#Mumbai city
#Mother house  -> recode to "home"
woman$deliver.site <- woman$C22
woman$deliver.site[woman$deliver.site==98] <- NA #N=7 answered "Others"
woman$deliver.site[woman$deliver.site==99] <- NA
#recode mother's house to "home"
woman$deliver.site[woman$C22_SP=="Mother's house"] <- 1
woman$deliver.site[woman$C22_SP=="thought of having a baby in my mother's house"] <- 1
woman$deliver.site[woman$C22_SP=="Mother house"] <- 1
table(woman$deliver.site)
prop.table(table(woman$deliver.site))

### BREASTFEEDING 
table(woman$C20)
table(woman$C20_week)
table(woman$C20_month)
table(woman$C20_year)
class(woman$C20_year)
woman$C20_year[woman$C20_year > 2017] <- NA #recode 2018, 2019, 2020, 2021 to missing 
#for those who answered EVER breastfed, replace NA with 0 
woman$C20_week[woman$C20==1 & is.na(woman$C20_week)] <- 0
woman$C20_month[woman$C20==1 & is.na(woman$C20_month)] <- 0
woman$C20_year[woman$C20==1 & is.na(woman$C20_year)] <- 0
table(woman$C20_week)
table(woman$C20_month)
table(woman$C20_year)
class(woman$C20_year)
woman$breastfed.mo <- woman$C20_week/4 + woman$C20_year + woman$C20_year*12
summary(woman$breastfed.mo)
#cutoff, one year=12 months 
woman$breastfed.mo[woman$breastfed.mo<=12] <-0 
woman$breastfed.mo[woman$breastfed.mo>12] <-1
table(woman$breastfed.mo)
sum(is.na(woman$breastfed.mo))


###AWARENESS OF GOV INCENTIVES 
sum(is.na(woman$C23))
table(woman$C23)
class(woman$C23)
woman <- woman %>% 
  mutate(know.hbincent = case_when(
    !is.na(C23) & C23!=97 ~ 1,
    C23 ==97 ~ 0
  ))
table(woman$know.hbincent)
prop.table(table(woman$know.hbincent))

### RECENT CHILD ILLNESS 
C24 <- woman %>%dplyr::select(starts_with("C24"))
apply(C24, 2, table)
woman$child.ill_3mo <- ifelse((woman$C241==1 | woman$C242==1 | woman$C243==1 |
                               woman$C244==1 | woman$C245==1 | woman$C246==1 |
                               woman$C247==1 | woman$C2498==1),1,0)
table(woman$child.ill_3mo)
#total number of symptoms 
sick_count <- function(row) {
  # Count the number of elements >=18
  result <- sum(row ==1, na.rm = T)
  return(result)
}
woman$child.ill_sum <-  apply(dplyr::select(woman, where(is.numeric) & starts_with("C24")), 1, sick_count)
table(woman$child.ill_sum)

### PREFERED SITE FOR CHILD ILLNESS 
table(woman$C25)
table(woman$C25_SP)
C25 <- woman %>%dplyr::select(starts_with("C25"))
apply(C25, 2, table)
#create a binary variable indicating whether she would take the child for ANY modern/traditional medicine 
woman$child.ill_tramed <- ifelse((woman$C25_3==1 | woman$C25_5==1 | woman$C25_6==1),1,0)
table(woman$child.ill_tramed)
woman$child.ill_modern <- ifelse((woman$C25_1==1 | woman$C25_2==1 | woman$C25_4==1),1,0)
table(woman$child.ill_modern) # traditional medicine is more distinguishable than ever used modern med

### PREFERED SITE FOR HER OWN ILLNESS 
table(woman$C28)
table(woman$C28_SP)
C28 <- woman %>%dplyr::select(starts_with("C28"))
apply(C28, 2, table)
#create a binary variable indicating whether she would take the child for ANY modern/traditional medicine 
woman$her.ill_tramed <- ifelse((woman$C28_3==1 | woman$C28_5==1 | woman$C28_6==1),1,0)
table(woman$her.ill_tramed)
woman$her.ill_modern <- ifelse((woman$C28_1==1 | woman$C28_2==1 | woman$C28_4==1),1,0)
table(woman$her.ill_modern) # traditional medicine is more distinguishable than ever used modern med

### DISTANCE TO DOCTORS 
table(woman$C30)
woman$doctor.distance <- woman$C30
woman$doctor.distance[woman$C30==97] <- NA
table(woman$doctor.distance)
#collapse categories: 4+5+6 -> greater than one hour 
woman$doctor.distance[woman$C30==5] <- 4
woman$doctor.distance[woman$C30==6] <- 4
table(woman$doctor.distance)

### BARRIERS TO ACCESS HEALTH FACILITIES 
table(woman$C321)
table(woman$C322)
table(woman$C323)
table(woman$C324)
#create a binary variable indicating whether woman has ANY BIG PROBLEM in accessing healthcare
woman$doctor.barrier <- ifelse((woman$C321 ==1 | woman$C322 ==1 | woman$C323 ==1 | woman$C324 ==1 ),0,1) 
table(woman$doctor.barrier)

### TRUST IN DOCTORS/ASHA
table(woman$C33)
table(woman$C34)
woman$trust.doctor <- woman$C33
woman$trust.doctor[woman$C33==99] <- NA
woman$trust.asha <- woman$C34

### CURRENT USE OF FP
#currently avoiding preg? 
table(woman$C35)
woman$preg.avoid <- ifelse((woman$C35 ==2),0,1) 
table(woman$preg.avoid)
woman$preg.avoid[woman$C35==3] <- NA #recode currently preg woman to missing 
table(woman$preg.avoid)
#current use of methods: a binary indicating whether using a modern method
#NOT modern method: SDM & withdrawal 
table(woman$C36)
C36 <- woman %>%dplyr::select(starts_with("C36"))
apply(C36, 2, table)
woman$fp.current.modern <- ifelse((woman$C36_1 ==1 | woman$C36_2==1 | woman$C36_3==1 |
                                     woman$C36_4==1 | woman$C36_5==1 | woman$C36_6==1|
                                     woman$C36_7==1 | woman$C36_9==1 | woman$C36_11==1),1,0) 
table(woman$fp.current.modern)
#correct denominator to those who (1) is not preg & (2) does not try to avoid preg
#also recode the "not applicable" categories: menopause, etc. 
woman$fp.current.modern[woman$preg.avoid==0] <- NA
woman$fp.current.modern[woman$C36_SP=="Mensturation period not happened."] <- NA
woman$fp.current.modern[woman$C36_SP=="Menstruation is not happening yet."] <- NA
woman$fp.current.modern[woman$C36_SP=="Since 2.5 yrs. Menstruation period not happened."] <- NA
woman$fp.current.modern[woman$C36_SP=="Not Menstruation period not happened after last child birth."] <- NA
table(woman$fp.current.modern)
#ever use of methods: a binary indicating whether using a modern method
#NOT modern method: SDM & withdrawal 
table(woman$C37)
C37 <- woman %>%dplyr::select(starts_with("C37"))
apply(C37, 2, table)
woman$fp.ever.modern <- ifelse((woman$C37_1 ==1 | woman$C37_2==1 | woman$C37_3==1 |
                                     woman$C37_4==1 | woman$C37_5==1 | woman$C37_6==1|
                                     woman$C37_7==1 | woman$C37_9==1 | woman$C37_11==1),1,0) 
table(woman$fp.ever.modern)
woman$fp.ever.modern[woman$C37_SP=="Sterlization"] <- 1
table(woman$fp.ever.modern)

#menstruation 
table(woman$C01)
table(woman$C02) #miss label meanings here 
woman$know.menst <- woman$C01
table(woman$know.menst)
prop.table(table(woman$know.menst))
#re-categorize to 1=knew something, 0=know nothing 
woman$know.menst[woman$know.menst==2] <- 1
woman$know.menst[woman$know.menst==3] <- 0
table(woman$know.menst)
#sexual debut 
table(woman$C05)
woman$sex.timing <- woman$C05
table(woman$sex.timing)
prop.table(table(woman$sex.timing))
##recategorize to two categories: 2=on time, 1=not on time 
woman$sex.timing[woman$sex.timing==3] <- 1
table(woman$sex.timing)

###PERCEPTION ABOUT SRH (e.g., contraception, birth delivery)
table(woman$B021)
table(woman$B022)
table(woman$B023)
table(woman$B024)
table(woman$B025)
table(woman$B026)
table(woman$B027)
table(woman$B028)
sum(is.na(woman$B021))
sum(is.na(woman$B022))
sum(is.na(woman$B023))
sum(is.na(woman$B024))
sum(is.na(woman$B025))
sum(is.na(woman$B026))
sum(is.na(woman$B027))
sum(is.na(woman$B028))
woman <- woman %>% mutate(know.srh = rowMeans(across(starts_with('B02')), na.rm = TRUE))
summary(woman$know.srh)
sd(woman$know.srh)
woman$know.srh_2 <- as.numeric(cut_number(woman$know.srh,2))
table(woman$know.srh_2)
woman$know.fpinfert   <- woman$B021
woman$know.condom     <- woman$B022
woman$know.medspreg   <- woman$B023
woman$know.fpabort    <- woman$B024
woman$know.hbsafe     <- woman$B025
woman$know.brthsurg   <- woman$B026
woman$know.fpsex      <- woman$B027
woman$know.fpside     <- woman$B028


###############################
### Marriage & Relationship ###
###############################

###REASONS FOR MARRIAGE: not sure how to code? "right age" is a top, then 'good offer' 
table(woman$C08)
table(woman$C08_1)
table(woman$C08_2)
table(woman$C08_3)
table(woman$C08_4)
table(woman$C08_5)
table(woman$C08_6)
table(woman$C08_7)
table(woman$C08_8)
table(woman$C08_98)
table(woman$C08_SP)

### PARTNET MARRIED PREVIOUSLY? 
table(woman$C09)
woman$his.1stmarry <- ifelse((woman$C09 ==1),0,1) 
woman$his.1stmarry[woman$C09==97] <-NA
table(woman$his.1stmarry)
prop.table(table(woman$his.1stmarry))



##############################
### Natal family structure ###
##############################

### WHO RAISED THE WOMAN RESPONDENT? 
table(woman$E01)
E01 <- woman %>%dplyr::select(starts_with("E01"))
apply(E01, 2, table)
#raised by single mom? 
woman$single.mom <- ifelse((woman$E01_1 ==1 & woman$E01_2==0),1,0) 
table(woman$single.mom)
#raised by BOTH biological parents? 
woman$bio.parent <- ifelse((woman$E01_1 ==1 & woman$E01_2==1),1,0) 
table(woman$bio.parent)

### HER CHILDHOOD HOME ENVIRONMENT 
#feeling safe? 
table(woman$E04)
woman$childhood.home.safe <- ifelse((woman$E04 ==5),1,0) 
table(woman$childhood.home.safe)
#ANY household stressors? - create two variables: woman herself, someone else 
E05 <- woman %>%dplyr::select(starts_with("E05"))
apply(E05, 2, table)
woman$childhood.home.stress_her <- ifelse((woman$E05n11==1 | woman$E05n21==1 | woman$E05n31==1 
                                           | woman$E05n41==1 | woman$E05n51==1  ),1,0) 
woman$childhood.home.stress_other <- ifelse((woman$E05n12==1 | woman$E05n22==1 | woman$E05n32==1 
                                           | woman$E05n42==1 | woman$E05n52==1  ),1,0) 
table(woman$childhood.home.stress_her)
table(woman$childhood.home.stress_other)

### DISTANCE TO NATAL/PARENT HOME 
woman$dist.natal <- woman$E06
table(woman$dist.natal)
prop.table(table(woman$dist.natal))
woman$dist.natal[woman$dist.natal==98] <-NA
woman$dist.natal[woman$dist.natal==3 | woman$dist.natal==4 | woman$dist.natal==5] <-2
table(woman$dist.natal)
  



###########
### Age ###
###########

### WOMEN'S OWN AGE AT KEY MILESTONES
sum(is.na(woman$S02))
table(woman$S02)
woman$age.her <- woman$S02
#### Define categorical age variable for adjustment ####
summary(woman$age.her)
table(woman$age.her)
woman$herage.adj <- cut(woman$age.her,
                        breaks = c(17, 19, 24, 29, 47), 
                        labels = c("18-19", "20-24", "25-29", "30-47"))
table(woman$herage.adj)
class(woman$herage.adj)
table(woman$S03)
# Husband's age 
sum(is.na(woman$S03a_1))
table(woman$S03a_1)
woman$age.his <- woman$S03a_1
#### Define categorical age variable for adjustment ####
summary(woman$age.his)
table(woman$age.his)
woman$hisage.adj <- cut(woman$age.his,
                        breaks = c(18, 23, 29, 35, 52), 
                        labels = c("19-23", "24-29", "30-35", "36-52"))
table(woman$hisage.adj)
class(woman$hisage.adj)
sum(is.na(woman$hisage.adj))
#age diff between women and her husband 
woman$age.diff <- woman$age.his - woman$age.her
sum(is.na(woman$age.diff))
table(woman$age.diff)
#if husband is older 
woman$age.older <- ifelse((woman$age.diff>1),1,0)
table(woman$age.older)
prop.table(table(woman$age.older))
##age at major life events 
sum(is.na(woman$C13m_1))
sum(is.na(woman$C13m_2))
sum(is.na(woman$C13m_3))
sum(is.na(woman$C13m_4))
class(woman$C13m_1)
table(woman$C13m_1)
table(woman$C13m_2)
table(woman$C13m_3)
table(woman$C13m_4)
woman$age.1stcohab.her <- ifelse((woman$C13m_1<18),1,0)
woman$age.1stsex.her   <- ifelse((woman$C13m_2<18),1,0)
woman$age.1stpreg.her  <- ifelse((woman$C13m_3<18),1,0)
woman$age.1stbrth.her <- ifelse((woman$C13m_4<18),1,0)
woman$age.1stcohab.her[woman$C13m_1==3] <- NA
woman$age.1stsex.her[woman$C13m_2==3] <- NA 
woman$age.1stpreg.her[woman$C13m_3==3] <- NA
woman$age.1stbrth.her[woman$C13m_4==3] <- NA
table(woman$age.1stcohab.her)
table(woman$age.1stsex.her)
table(woman$age.1stpreg.her)
table(woman$age.1stbrth.her)
prop.table(table(woman$age.1stcohab.her))
prop.table(table(woman$age.1stsex.her))
prop.table(table(woman$age.1stpreg.her))
prop.table(table(woman$age.1stbrth.her))
### PARTNER'S AGE AT KEY MILESTONES
sum(is.na(woman$C13p_1))
sum(is.na(woman$C13p_2))
sum(is.na(woman$C13p_3))
sum(is.na(woman$C13p_4))
table(woman$C13p_1)
table(woman$C13p_2)
table(woman$C13p_3)
table(woman$C13p_4)
woman$age.1stcohab.his <- ifelse((woman$C13p_1<18),0,1)
woman$age.1stsex.his   <- ifelse((woman$C13p_2<18),0,1)
woman$age.1stpreg.his  <- ifelse((woman$C13p_3<18),0,1)
woman$age.1stbrth.his <- ifelse((woman$C13p_4<18),0,1)
woman$age.1stcohab.his[woman$C13p_1==3] <- NA
woman$age.1stsex.his[woman$C13p_2==3] <- NA 
woman$age.1stpreg.his[woman$C13p_3==3] <- NA
woman$age.1stbrth.his[woman$C13p_4==3] <- NA
#view(woman$C13m_1)








###########################
### Natal family health ###
###########################


# child illness






#parent death 
table(woman$E02_1) #mother 
table(woman$E02_2) #father 
table(woman$E03_1) #mother 
table(woman$E03_2) #father 
#don't know -> missing 
woman <- woman %>% 
  mutate(death.parent = case_when(
    E02_1 == 2 | E02_2 ==2 ~ 1,
    E02_1 == 1 & E02_2 ==1 ~ 0
  ))
woman <- woman %>% 
  mutate(death.mom = case_when(
    E02_1 == 2  ~ 1,
    E02_1 == 1  ~ 0
  ))
woman <- woman %>% 
  mutate(death.dad = case_when(
    E02_2 ==2 ~ 1,
    E02_2 ==1 ~ 0
  ))
table(woman$death.parent)
table(woman$death.mom)
table(woman$death.dad)
prop.table(table(woman$death.parent))
prop.table(table(woman$death.mom))
prop.table(table(woman$death.dad))
#one-in-five women have experienced parental death 

# Orphan: both parents died before the woman was age of 12
table(woman$E03_1)
table(woman$E03_2)
woman$birthyear <- 2021 - woman$S02
sum(is.na(woman$birthyear))
woman$orphan <- ifelse((woman$E03_1<(woman$birthyear+12) & woman$E03_2<(woman$birthyear+12) 
                        & !is.na(woman$E03_1) & !is.na(woman$E03_2)),1,0)
table(woman$orphan)
prop.table(table(woman$orphan))



#####################
### SES CONDITION ###
#####################

##household stressor/instability 
sum(is.na(woman$A111))
sum(is.na(woman$A112))
sum(is.na(woman$A113))
sum(is.na(woman$A114))
sum(is.na(woman$A115))
table(woman$A111)
table(woman$A112)
table(woman$A113)
table(woman$A114)
table(woman$A115)
woman$house.stress <- ifelse((woman$A111==1 |  woman$A112==1 |  woman$A113==1
                              | woman$A114==1 |  woman$A115==1),1,0)
table(woman$house.stress)
prop.table(table(woman$house.stress))

### MIGRATION
### PARTNER AWAY FOR A MONTH DURING THE PAST YEAR? 
table(woman$E15)
woman$migrant.his <- ifelse((woman$E15 ==1),1,0) 
table(woman$E15a)
woman$migrant.her <- ifelse((woman$E15a ==1),1,0) 
table(woman$migrant.his)
prop.table(table(woman$migrant.his))
table(woman$migrant.her)
prop.table(table(woman$migrant.her))
### How many years living in this place 
table(woman$E17)
woman$home.years <- woman$E17
summary(woman$home.years)
#cut-off, 7 years 
woman$home.years[woman$home.years<=7] <-0
woman$home.years[woman$home.years>7] <-1
table(woman$home.years)
table(woman$E18) # NO VARIATION 
table(woman$E19)

###HOUSEHOLD CONFLICT for any reason: NOTE-many missing here!!!
table(woman$A14)
table(woman$A15)
woman$house.conflict <- ifelse((woman$A14==1),0,1)
table(woman$house.conflict)
prop.table(table(woman$house.conflict))
A15 <- woman %>%dplyr::select(starts_with("A15_"))
apply(A15, 2, table)
#reasons for conflict: e.g., money 
#but since the %conflict is not very big, we might not need to disaggregate by reasons 
#also, I am a bit confused about the difference between A15 and A16
#seems like A161 was only asked when NO is selected in A15_1 (but the denominator seems werid?)
table(woman$A15_1)
sum(is.na(woman$A161)) #missing exactly 200 
table(woman$A161)

###LAND OWNERSHIP: own or jointly
table(woman$E21)
woman <- woman %>% 
  mutate(land.own = case_when(
    E21 == 2  ~ 1,
    E21 == 2 | E21 ==3 ~ 1,
    E21 == 4 | E21==5 ~0
  ))
table(woman$land.own)
prop.table(table(woman$land.own))

### ELECTRICITY 
table(woman$E23) #NO VARIATION 
table(woman$E24) #1=very rare, 2=sometimes, 3=everyday 
woman <- woman %>% 
  mutate(elec.lose = case_when(
    E24 == 1  ~ 3,
    E24 == 2 | E24 ==3 | E24==4 ~ 2,
    E24 == 5  ~1
  ))
table(woman$elec.lose)
prop.table(table(woman$elec.lose))

###Woman owns a cellphone? 1=own, 2=shared, 0=No
table(woman$IO19)
woman <- woman %>% 
  mutate(cell.own = case_when(
    IO19 == 1  ~ 1,
    IO19 == 3  ~ 1,    
    IO19 == 2  ~ 2, 
    IO19 == 4 | IO19==5  ~ 0,
  ))
table(woman$cell.own)
prop.table(table(woman$cell.own))
table(woman$IO20)
woman$cell.smart <- ifelse((woman$IO20==1),1,0)
table(woman$cell.smart)
prop.table(table(woman$cell.smart))

### BANK 
table(woman$E25)
woman <- woman %>% 
  mutate(bank.own = case_when(
    E25 == 1  ~ 1,
    E25 == 3  ~ 1,    
    E25 == 2 |  E25 == 4 | E25==5  ~ 0,
    E25 == 97 ~ NA
  ))
table(woman$bank.own)
prop.table(table(woman$bank.own))
#mobile bank account: 0=no one in the HH, 1=own, 2=others in the HH 
table(woman$E25a)
woman <- woman %>% 
  mutate(bank.mobile = case_when(
    E25a == 1  ~ 1,
    E25a == 4  ~ 2,    
    E25a == 5  ~ 0,
    E25a == 97 ~ NA
  ))
table(woman$bank.mobile)
prop.table(table(woman$bank.mobile))
#last use of bank account: 1=within 3 months, 2=more than 3 months, 97=don't know) 
table(woman$E26)
table(woman$E26a)
woman <- woman %>% 
  mutate(bank.last = case_when(
    E26 == 1 | E26==2 | E26==3  ~ 1,
    E26 == 4  ~ 2,    
    E26 == 97 ~ 97
  ))
table(woman$bank.last)
prop.table(table(woman$bank.last))

### WOMEN SELF-HELP GROUP SAVINGS:1=yes & join, 2= yes & don't join, 0=NO
table(woman$E27)
table(woman$E28)
table(woman$E29)
woman <- woman %>% 
  mutate(self.help = case_when(
    E27 == 1 & E28==1  ~ 1,
    E27 == 1 & E28==2  ~ 2,
    E27 == 2 ~ 0
  ))
table(woman$self.help)
prop.table(table(woman$self.help))

### LOAN 
table(woman$E30)
table(woman$E31)
table(woman$E32)
table(woman$E33)
woman$loan <- woman$E30
table(woman$loan)
prop.table(table(woman$loan))

### MEDIA EXPOSURE 
table(woman$E34_1) #internet: majority of media exposure 
table(woman$E34_2) #radio: very little variation 
table(woman$E34_3)
table(woman$E34_4) #newspaper: very little variation 
woman <- woman %>% mutate(media = rowMeans(across(starts_with('E34_')), na.rm = TRUE))
summary(woman$media)
sd(woman$media)
woman$media_2 <- as.numeric(cut_number(woman$media,2))
table(woman$media_2)

### TRANSPORTATION 
table(woman$E351)
table(woman$E352)
table(woman$E353) #car, very little variation 
table(woman$E354) #motor boat, very little variation 
woman$transport <- ifelse((woman$E351==1 | woman$E352==1 | woman$E353==1 | woman$E354==1),1,0)
table(woman$transport)
prop.table(table(woman$transport))

### CASTE
table(woman$E38)
sum(is.na(woman$E38))
table(woman$E38a)
#1=The Scheduled Castes and Scheduled Tribes are officially designated groups and the most disadvantaged SES
woman <- woman %>% 
  mutate(caste = case_when(
    E38 == 1 | E38==2  ~ 1,
    E38==3  ~ 2,
    E38 == 4 ~ 3
  ))
table(woman$caste)
prop.table(table(woman$caste))

### RELIGION: hindu or not 
table(woman$E39)
table(woman$E39_SP)
woman$hindu <- ifelse((woman$E39==1),1,0)
table(woman$hindu)
prop.table(table(woman$hindu))


#################################
### Internalized gender norms ###
#################################

###BEATING WIFE JUSTIFIED?
sum(is.na(woman$A131))
sum(is.na(woman$A132))
sum(is.na(woman$A133))
sum(is.na(woman$A134))
table(woman$A131)
table(woman$A132)
table(woman$A133)
table(woman$A134)
woman$ipv.accept <- ifelse((woman$A131==1 |  woman$A132==1 |  woman$A133==1 | woman$A134==1 ),1,0)
table(woman$ipv.accept)
prop.table(table(woman$ipv.accept))

###GENDER ROLE: WOMEN SHOULD DO X, Y, Z?
sum(is.na(woman$B011))
sum(is.na(woman$B012))
sum(is.na(woman$B013))
sum(is.na(woman$B014))
sum(is.na(woman$B015))
sum(is.na(woman$B016))
sum(is.na(woman$B017))
sum(is.na(woman$B018))
table(woman$B011)
table(woman$B012)
table(woman$B013)
table(woman$B014)
table(woman$B015)
table(woman$B016)
table(woman$B017)
table(woman$B018)
woman <- woman %>% mutate(gender.role = rowMeans(across(starts_with('B01')), na.rm = TRUE))
summary(woman$gender.role)
sd(woman$gender.role)
woman$gender.role_2 <- as.numeric(cut_number(woman$gender.role,2))
table(woman$gender.role_2)
###WOMEN DOES NOT WANT A CHILD 
table(woman$B071)
woman$negotiate.preg <- woman$B071
table(woman$negotiate.preg)
prop.table(table(woman$negotiate.preg))



##########################
### LARGER ENVIRONMENT ###
##########################

### WATER ACCESS
table(woman$D01)
#create a 3-cat: 1=in house, 2=yard, 3=5 min walk
woman$water <- woman$D01
woman$water[woman$D01==4 | woman$D01==5] <- 3
table(woman$water)
table(woman$D02)
#is water free? 
woman$water.free <- ifelse((woman$D02==0 ),1,0)
table(woman$water.free)
table(woman$D03)
#is water purified? 
woman$water.puri <- ifelse((woman$D03==99 ),1,0)
table(woman$water.puri)

### FOOD 
table(woman$D06)
table(woman$D06_01) #no response 
woman$processfood.freq <- woman$D06
#recategorize to two categories: 1=everyday/more, 0=< everyday 
woman$processfood.freq[woman$processfood.freq==7] <- 0
woman$processfood.freq[woman$processfood.freq<5] <- 0
woman$processfood.freq[woman$processfood.freq==5 | woman$processfood.freq==6] <- 1
table(woman$processfood.freq)
table(woman$D08)
woman$processfood.percept <- ifelse((woman$D08==3 ),1,0)
table(woman$processfood.percept)

### CLIMATE 
table(woman$D091)
table(woman$D092)
table(woman$D093)
table(woman$D094)
#any YES to the above questions?
woman$climate.impact <- ifelse((woman$D091==1 | woman$D092==1 | woman$D093==1 | woman$D094==1 ),1,0)
table(woman$climate.impact)
#impact on money? 
table(woman$D10)
woman$climate.money <- woman$D10 

### HOUSE 
table(woman$E16) #renting or own home: NO VARIATION 
table(woman$E16_SP)

### KITCHEN 
table(woman$IO11)
woman$kitchen <- woman$IO11

### COOKING FUEL / AIR QUALITY 
table(woman$IO12)
table(woman$IO12_SP)
# categorize to "smoke-producing cooking fuel" (1) versus “non-smoke-producing fuels” (0)
# REF: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8374520/
#[1] Wood
#[2] Charcoal
#[3] LPG/Natural gas
#[4] Straw/shrubs/grass
#[5] Agricultural crop
#[6] Animal dung
#[7] Parafin/Kerosene
#[8] Electricity
woman <- woman %>% 
  mutate(cook.smoke = case_when(
    IO12==1 | IO12==2 | IO12==4 | IO12==5 | IO12==6 | IO12==7  ~ 1,
    IO12==3 | IO12==8 ~0
  ))
woman$cook.smoke[woman$IO12_SP=="Made on both gas and wood" 
                 | woman$IO12_SP=="Wood, dung both equal"] <- 1
table(woman$cook.smoke)
prop.table(table(woman$cook.smoke))
#kitchen ventilation 
table(woman$IO13)
woman$cook.vent <- ifelse((woman$IO13==1 ),1,0)
table(woman$cook.vent)
prop.table(table(woman$cook.vent))
table(woman$IO14) #little variation: almost all covered cook food 
table(woman$IO15) 
woman$cook.bed <- ifelse((woman$IO15==4 ),1,0) #1=Stove is in separate room from sleeping area
table(woman$cook.bed)
prop.table(table(woman$cook.bed))
table(woman$IO16) 
woman$bedroom <- woman$IO16
#has toilet? 
table(woman$IO21) 
woman$toilet <- woman$IO21
table(woman$IO22) ## among those who have toilet, is it shared? 
table(woman$IO24)
table(woman$IO24_SP)
woman$stool.open <- ifelse((woman$IO24==5 ),1,0) 
#handwashing 
table(woman$IO25)
table(woman$IO26)
table(woman$IO27)
woman$handwash.dwell <- ifelse((woman$IO25==1 | woman$IO25==2 ),1,0) #1=inside the dwelling 
woman$handwash.pipe <- ifelse((woman$IO26==1),1,0) #1=pipe water available for handwashing 
woman$handwash.soap <- ifelse((woman$IO27==1),1,0) #1=soap or detergent 
table(woman$handwash.dwell)
table(woman$handwash.pipe)
table(woman$handwash.soap)
#HH smoke
table(woman$IO28)
table(woman$IO28_1)
table(woman$IO28_2)
table(woman$IO28_3)
table(woman$IO28_4)
table(woman$IO28_5)
table(woman$IO28_99)
woman$house.smoke <- ifelse((woman$IO28_99==1),0,1)
table(woman$house.smoke)
prop.table(table(woman$house.smoke))

#dwelling type 
table(woman$IO3)
woman$dwell.attach <- ifelse((woman$IO3==1),1,0)
table(woman$dwell.attach)
prop.table(table(woman$dwell.attach))

#materials of roof: 1=finished roof, 0=unfinished 
table(woman$IO5)
woman$roof <- ifelse((woman$IO5==8 | woman$IO5==9 | woman$IO5==10 | 
                        woman$IO5==11 | woman$IO5==12 | woman$IO5==13),1,0)
table(woman$roof)
prop.table(table(woman$roof))

#walls: 1=finished roof, 0=unfinished 
table(woman$IO6)
woman$wall <- ifelse((woman$IO6==10 | woman$IO6==11 | woman$IO6==12 | 
                        woman$IO6==13 | woman$IO6==14 | woman$IO6==15 | woman$IO6==16),1,0)
table(woman$wall)
prop.table(table(woman$wall))

#floor
table(woman$IO7)
woman$floor <- ifelse((woman$IO7==6 | woman$IO7==7 | woman$IO7==8 | 
                        woman$IO7==9 ),1,0)
table(woman$floor)
prop.table(table(woman$floor))

#sitting water?
table(woman$IO8)
woman$puddle <- ifelse((woman$IO8== 4),0,1)
table(woman$puddle)
prop.table(table(woman$puddle))

# Sewer drainage
table(woman$IO9)
woman$drainage <- woman$IO9

## WASH INDICATORS 
# For this study, we concentrated on three dependent variables defined for the study population, 
# which assign a value of 1 to households that use basic services (water, sanitation, and hygiene) 
# and 0 to those households that use limited or unimproved services, and had no facility of hand-washing, 
# practiced open defecation and used surface water for drinking.
# REF: https://www.frontiersin.org/articles/10.3389/fenvs.2022.1013657/full
#water: is the ware source (1) in house, and (2) purified? 
woman$WASH.water <- ifelse((woman$D01==1 & woman$D03==99 ),1,0)
table(woman$WASH.water)
#toilet: (1) flush toilet and (2) not shared 
woman$WASH.sani <- ifelse((woman$IO21==1 & woman$IO22==2 ),1,0)
table(woman$WASH.sani)
#handwashing: (1) have a facility, and (2) soap & water 
woman$WASH.hyge <- ifelse((woman$IO25!=4 & woman$IO26==1 & woman$IO27==1 ),1,0)
table(woman$WASH.hyge)

## HOUSEHOLD POSESSIONS 
woman$house.asset <- rowSums(cbind(woman$land.own, woman$cell.own, woman$bank.own, 
                                   woman$kitchen, woman$roof, woman$floor, woman$wall,
                                   woman$transport), na.rm = T )
table(woman$house.asset)
woman$house.asset_2 <- ifelse((woman$house.asset>7 ),1,0)
table(woman$house.asset_2 )

###############################
### WOMEN"S HEALTH OUTCOMES ###
###############################

### PREGNANCIES 
#current preg 
woman$preg.current <- ifelse((woman$C35==3),1,0)
table(woman$preg.current)
table(woman$C13_1)
table(woman$C13_2)
table(woman$C13_3)
table(woman$C13_4)
#among all pregancies, whether woman ever experince loss (including stillbirth, abortion, miscarriage)
woman$preg.num <- woman$C13_1 + woman$C13_2 + woman$C13_3 + woman$C13_4 
table(woman$preg.num)
summary(woman$preg.num)
sd(woman$preg.num, na.rm=TRUE)
#live births 
woman$preg.live <-  woman$C13_1  
table(woman$preg.live)
summary(woman$preg.live)
sd(woman$preg.live, na.rm=TRUE)
#still births 
woman$preg.still <- woman$C13_2  
table(woman$preg.still)
summary(woman$preg.still)
sd(woman$preg.still, na.rm=TRUE)
#abortion 
woman$preg.abort <- woman$C13_3  
table(woman$preg.abort)
summary(woman$preg.abort)
sd(woman$preg.abort, na.rm=TRUE)
#miscarriage 
woman$preg.miscar <- woman$C13_4
table(woman$preg.miscar)
summary(woman$preg.miscar)
sd(woman$preg.miscar, na.rm=TRUE)
#pregnancy loss 
woman$preg.loss.num <- woman$C13_2 + woman$C13_3 + woman$C13_4 
table(woman$preg.loss.num)
woman$preg.loss.yn <- ifelse((woman$preg.loss.num==0),0,1)
table(woman$preg.loss.yn)
prop.table(table(woman$preg.loss.yn))
woman$num.liverth <- woman$C13_1 
table(woman$num.liverth)
#anyone got angry when learning woman is pregnant? 
table(woman$C41)
woman$preg.rej <- ifelse((woman$C41==99),0,1)
table(woman$preg.rej)

### EXPERIENCING OWN U5 CHILD DEATH 
table(woman$C13a)
table(woman$C13b_1)
table(woman$C13b_2)
woman$death.u5 <- ifelse((woman$C13a==1),1,0)
table(woman$death.u5)
prop.table(table(woman$death.u5))
#one-in-ten women have experienced u5 death 
#total number of child deaths 
woman$child.loss <- woman$C13b_1 + woman$C13b_2
woman$child.loss[woman$death.u5==0] <-0
table(woman$child.loss) #most lost one kid 

### STRESS/DEPRESSION 
woman <- woman %>% mutate(depress = rowMeans(across(starts_with('B08')), na.rm = TRUE))
summary(woman$depress)
sd(woman$depress)

### PERSONALITY: need to double check the direction of each item (e.g., I like meeting new people)

### HER ILLNESS IN THE PAST 3 MONTHS 
C27 <- woman %>%dplyr::select(starts_with("C27"))
apply(C27, 2, table)
woman$her.ill_3mo <- ifelse((woman$C271==1 | woman$C272==1 | woman$C273==1 |
                                 woman$C274==1 | woman$C275==1 | woman$C276==1 |
                                 woman$C277==1 | woman$C2798==1),1,0)
table(woman$her.ill_3mo)
#total number of symptoms 
sick_count <- function(row) {
  # Count the number of elements >=18
  result <- sum(row ==1, na.rm = T)
  return(result)
}
woman$her.ill_sum <-  apply(dplyr::select(woman, where(is.numeric) & starts_with("C27")), 1, sick_count)
table(woman$her.ill_sum)

### FERTILITY PREFERENCES & DESIRES 
table(woman$C38)
table(woman$C39)
table(woman$C40_1)
table(woman$C40_2)
table(woman$C40_3)
woman$fert.more <- woman$C38
woman$fert.child <- woman$C39
woman$fert.boy <- woman$C40_1
woman$fert.girl <- woman$C40_2
woman$fert.either <- woman$C40_3
woman$male.pref <- ifelse((woman$C40_1 > woman$C40_2),1,0)
table(woman$male.pref)

saveRDS(woman, file = "Data/India_Pathways_recode.rds")
odbcCloseAll()
