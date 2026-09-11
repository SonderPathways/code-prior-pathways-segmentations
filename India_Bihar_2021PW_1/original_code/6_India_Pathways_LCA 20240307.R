rm(list=ls())
pacman::p_load("poLCA", "dplyr", "tidyverse", "tidyLPA", "networkD3", "data.table", "ggpubr", "forcats")

setwd("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2021 Pathways/Data/Bihar_constructed_data")
vulnerability <- readRDS("1_Bihar_Pathways_vulpredictors.rds")
outcomes <- readRDS("1_Bihar_Pathways_outcomes.rds")


colSums(is.na(vulnerability))
table(vulnerability$house.asset)
vulnerability <- vulnerability %>% dplyr::select(
  caseid, 
  his.age,
  age.1stsex.her,
  age.1stpreg.her,
  death.parent,
  bio.parent,
  dist.natal,
  childhood.home.safe,
  her.edulevel,
  his.edulevel,
  caste,
  hindu,
  ipv.accept,
  gender.role,
  hh.size,
  cohab.adult.sum,
  cohab.minor,
  female.help,
  support.first.hh,
  negotiate.preg,
  her.decide.child,
  her.decide.hh,
  bank.own,
  house.stress,
  her.work.year,
  his.work.agri,
  loan,
  house.asset,
  prepare.sex,
  prepare.preg,
  percept.child.sick,
  percept.srh,
  support.first.comm,
  support.comm,
  trust.doctor,
  trust.asha,
  saving.group,
  doctor.distance,
  medical.barrier,
  media,
  smartcell.hh,
  transport.hh,
  climate.impact,
  WASH.water,
  kitchen.vent
  ) %>% drop_na() 
lapply(vulnerability, class)
lapply(vulnerability, table)
## setting nominal to be coded as 1,2,...
num.vars <- names(vulnerability)
class(num.vars)
num.vars <- num.vars[! num.vars %in% c("doctor.distance", "percept.child.sick", "house.asset", "her.work.year", 
                                       "negotiate.preg", "cohab.adult.sum", "hh.size", "caste", "his.edulevel", 
                                       "her.edulevel", "dist.natal", "his.age", "caseid")]


for (i in 1:length(num.vars)){
  vulnerability[,num.vars[i]] <- vulnerability[,num.vars[i]]+1
  print(table(vulnerability[,num.vars[i]]))
}

table(vulnerability$house.asset)


set.seed(99)
formula <- as.formula(cbind(
  his.age,
  age.1stsex.her,
  age.1stpreg.her,
  death.parent,
  bio.parent,
  dist.natal,
  childhood.home.safe,
  her.edulevel,
  his.edulevel,
  caste,
  hindu,
  ipv.accept,
  gender.role,
  hh.size,
  cohab.adult.sum,
  cohab.minor,
  female.help,
  support.first.hh,
  negotiate.preg,
  her.decide.child,
  her.decide.hh,
  bank.own,
  house.stress,
  her.work.year,
  his.work.agri,
  loan,
  house.asset,
  prepare.sex,
  prepare.preg,
  percept.child.sick,
  percept.srh,
  support.first.comm,
  support.comm,
  trust.doctor,
  trust.asha,
  saving.group,
  doctor.distance,
  medical.barrier,
  media,
  smartcell.hh,
  transport.hh,
  climate.impact,
  WASH.water,
  kitchen.vent
)~1)

### fitting 2-7 classes 
for(n in 2:7) {
  name <- paste0('LCA', n)
  data <- poLCA(formula, data=vulnerability, nclass=n, maxiter=3000, nrep=100)
  assign(name, data)
}

saveRDS(LCA2, file = "LCA2.rds")
saveRDS(LCA3, file = "LCA3.rds")
saveRDS(LCA4, file = "LCA4.rds")
saveRDS(LCA5, file = "LCA5.rds")
saveRDS(LCA6, file = "LCA6.rds")
saveRDS(LCA7, file = "LCA7.rds")

###MAKE ONE FOR OUTCOMES!!!
#match with caseid: vul (keep caseid) + outcomes

LCA2_percent_mle = sum(round(LCA2$attempts,digits = -3) == round(LCA2$llik,digits = -3))/length(LCA2$attempts)
LCA2_percent_mle

LCA3_percent_mle = sum(round(LCA3$attempts,digits = -3) == round(LCA3$llik,digits = -3))/length(LCA3$attempts)
LCA3_percent_mle

LCA4_percent_mle = sum(round(LCA4$attempts,digits = -3) == round(LCA4$llik,digits = -3))/length(LCA4$attempts)
LCA4_percent_mle

LCA5_percent_mle = sum(round(LCA5$attempts,digits = -3) == round(LCA5$llik,digits = -3))/length(LCA5$attempts)
LCA5_percent_mle

LCA6_percent_mle = sum(round(LCA6$attempts,digits = -3) == round(LCA6$llik,digits = -3))/length(LCA6$attempts)
LCA6_percent_mle

LCA7_percent_mle = sum(round(LCA7$attempts,digits = -3) == round(LCA7$llik,digits = -3))/length(LCA7$attempts)
LCA7_percent_mle





#check posterior probability
LCA2_posterior <- as.data.frame(LCA2$posterior)
colnames(LCA2_posterior) <- c("cluster_k2_1", "cluster_k2_2")
LCA2_posterior <- cbind(LCA2_posterior, LCA2_class=LCA2$predclass)
LCA2_posterior$p <- ifelse((LCA2_posterior$LCA2_class==1), 
                           LCA2_posterior$cluster_k2_1, LCA2_posterior$cluster_k2_2)
mean(LCA2_posterior$p)
LCA2_p <- mean(LCA2_posterior$p)

LCA3_posterior <- as.data.frame(LCA3$posterior)
colnames(LCA3_posterior) <- c("cluster_k3_1", "cluster_k3_2", "cluster_k3_3")
LCA3_posterior <- cbind(LCA3_posterior, LCA3_class=LCA3$predclass)
LCA3_posterior <- LCA3_posterior %>% 
  mutate(p = case_when(
    LCA3_posterior$LCA3_class==1  ~ LCA3_posterior$cluster_k3_1,
    LCA3_posterior$LCA3_class==2  ~ LCA3_posterior$cluster_k3_2,
    LCA3_posterior$LCA3_class==3 ~  LCA3_posterior$cluster_k3_3
  ))
mean(LCA3_posterior$p)
LCA3_p <- mean(LCA3_posterior$p)


LCA4_posterior <- as.data.frame(LCA4$posterior)
colnames(LCA4_posterior) <- c("cluster_k4_1", "cluster_k4_2", "cluster_k4_3", "cluster_k4_4")
LCA4_posterior <- cbind(LCA4_posterior, LCA4_class=LCA4$predclass)
LCA4_posterior <- LCA4_posterior %>% 
  mutate(p = case_when(
    LCA4_posterior$LCA4_class==1  ~ LCA4_posterior$cluster_k4_1,
    LCA4_posterior$LCA4_class==2  ~ LCA4_posterior$cluster_k4_2,
    LCA4_posterior$LCA4_class==3 ~  LCA4_posterior$cluster_k4_3,
    LCA4_posterior$LCA4_class==4 ~  LCA4_posterior$cluster_k4_4
  ))
mean(LCA4_posterior$p)
LCA4_p <- mean(LCA4_posterior$p)


LCA5_posterior <- as.data.frame(LCA5$posterior)
colnames(LCA5_posterior) <- c("cluster_k5_1", "cluster_k5_2", "cluster_k5_3", "cluster_k5_4", "cluster_k5_5")
LCA5_posterior <- cbind(LCA5_posterior, LCA5_class=LCA5$predclass)
LCA5_posterior <- LCA5_posterior %>% 
  mutate(p = case_when(
    LCA5_posterior$LCA5_class==1  ~ LCA5_posterior$cluster_k5_1,
    LCA5_posterior$LCA5_class==2  ~ LCA5_posterior$cluster_k5_2,
    LCA5_posterior$LCA5_class==3 ~  LCA5_posterior$cluster_k5_3,
    LCA5_posterior$LCA5_class==4 ~  LCA5_posterior$cluster_k5_4,
    LCA5_posterior$LCA5_class==5 ~  LCA5_posterior$cluster_k5_5
  ))
mean(LCA5_posterior$p)
LCA5_p <- mean(LCA5_posterior$p)


LCA6_posterior <- as.data.frame(LCA6$posterior)
colnames(LCA6_posterior) <- c("cluster_k6_1", "cluster_k6_2", 
                              "cluster_k6_3", "cluster_k6_4", "cluster_k6_5", "cluster_k6_6")
LCA6_posterior <- cbind(LCA6_posterior, LCA6_class=LCA6$predclass)
LCA6_posterior <- LCA6_posterior %>% 
  mutate(p = case_when(
    LCA6_posterior$LCA6_class==1  ~ LCA6_posterior$cluster_k6_1,
    LCA6_posterior$LCA6_class==2  ~ LCA6_posterior$cluster_k6_2,
    LCA6_posterior$LCA6_class==3 ~  LCA6_posterior$cluster_k6_3,
    LCA6_posterior$LCA6_class==4 ~  LCA6_posterior$cluster_k6_4,
    LCA6_posterior$LCA6_class==5 ~  LCA6_posterior$cluster_k6_5, 
    LCA6_posterior$LCA6_class==6 ~  LCA6_posterior$cluster_k6_6
  ))
mean(LCA6_posterior$p)
LCA6_p <- mean(LCA6_posterior$p)


LCA7_posterior <- as.data.frame(LCA7$posterior)
colnames(LCA7_posterior) <- c("cluster_k7_1", "cluster_k7_2", "cluster_k7_3", "cluster_k7_4", "cluster_k7_5", "cluster_k7_6", "cluster_k7_7")
LCA7_posterior <- cbind(LCA7_posterior, LCA7_class=LCA7$predclass)
LCA7_posterior <- LCA7_posterior %>% 
  mutate(p = case_when(
    LCA7_posterior$LCA7_class==1  ~ LCA7_posterior$cluster_k7_1,
    LCA7_posterior$LCA7_class==2  ~ LCA7_posterior$cluster_k7_2,
    LCA7_posterior$LCA7_class==3 ~  LCA7_posterior$cluster_k7_3,
    LCA7_posterior$LCA7_class==4 ~  LCA7_posterior$cluster_k7_4,
    LCA7_posterior$LCA7_class==5 ~  LCA7_posterior$cluster_k7_5, 
    LCA7_posterior$LCA7_class==6 ~  LCA7_posterior$cluster_k7_6,
    LCA7_posterior$LCA7_class==7 ~  LCA7_posterior$cluster_k7_7
  ))
mean(LCA7_posterior$p)
LCA7_p <- mean(LCA7_posterior$p)


myds <- c(LCA2_p,LCA3_p,LCA4_p,LCA5_p,LCA6_p, LCA7_p)
x <- (1:length(myds))
#make the main plot
plot(myds, xaxt = "n", type="b", ylim=c(0, 1), 
     xlab="Solutions", ylab="Posterior probability")
axis(1, at=1:6, labels=c("2-class","3-class","4-class", 
                         "5-class", "6-class", "7-class"))



class <- vulnerability_class %>% dplyr::select(LCA2_class, LCA3_class, LCA4_class, LCA5_class, LCA6_class, LCA7_class)
colnames(vulnerability_class) <- c("cluster_k2", "cluster_k3", "cluster_k4", "cluster_k5", "cluster_k6", "cluster_k7")
class <- as.data.table(class)

#make sankey plots 
make_sankey <- function(data){
  links <- lapply(2:6, function(i){
    link.i<-  data[,.(value = .N),.(source = paste0("k", i, "_", get(paste0("cluster_k", i))), target = paste0("k", i+1, "_",get(paste0("cluster_k", i+1))))]
    return(link.i)
  }) %>% rbindlist(use.names = T, fill = T)
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(
    name=c(as.character(links$source), 
           as.character(links$target)) %>% unique()
  )
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  # Make the Network
  p <- sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", 
                     sinksRight=FALSE)
  return(p)
}

sankey_bihar <- make_sankey(class)
sankey_bihar

### DESCRIPTIVES 
descriptives <- recode_class %>% dplyr::select(age.1stsex.her, death.parent, caste_, edulevel.her_,edulevel.his_,
                                   family.size_, support.his_, ipv.accept, gender.role_, decide.big_most_, decide.child_most_,  
                                   house.stress, house.conflict, media_,  house.asset_2, WASH.water_, WASH.hyge_,
                                   self.help_, support.chw_, climate.money_, home.years_, 
                                   LCA2_class, LCA3_class, LCA4_class, LCA5_class, LCA6_class, LCA7_class)


##pop average 
for(i in 1:21){
  # Create a frequency table for the current column
  table_i <- table(descriptives[,i])
  # Create a percentage distribution for the current column
  prop_i <- prop.table(table_i)
  # Print the frequency table and percentage distribution for the current column
  print(colnames(descriptives)[i])
  print(prop_i)
}
  

for(n in 1:2) {
for(i in 1:21){
  # Create a frequency table for the current column
  table_i <- table(descriptives[,i][descriptives$LCA2_class==n])
  # Create a percentage distribution for the current column
  prop_i <- prop.table(table_i)
  # Print the frequency table and percentage distribution for the current column
  print(colnames(descriptives)[i])
  print(prop_i)
}
}

for(n in 1:5) {
  for(i in 1:21){
    # Create a frequency table for the current column
    table_i <- table(descriptives[,i][descriptives$LCA5_class==n])
    # Create a percentage distribution for the current column
    prop_i <- prop.table(table_i)
    # Print the frequency table and percentage distribution for the current column
    print(colnames(descriptives)[i])
    print(prop_i)
  }
}

prop.table(table(descriptives$LCA5_class))
for(i in 1:21){
  # Create a frequency table for the current column
  table_i <- table(descriptives[,i][descriptives$LCA5_class==1])
  # Create a percentage distribution for the current column
  prop_i <- prop.table(table_i)
  # Print the frequency table and percentage distribution for the current column
  print(colnames(descriptives)[i])
  print(prop_i)
}

for(i in 1:21){
  # Create a frequency table for the current column
  table_i <- table(descriptives[,i][descriptives$LCA5_class==2])
  # Create a percentage distribution for the current column
  prop_i <- prop.table(table_i)
  # Print the frequency table and percentage distribution for the current column
  print(colnames(descriptives)[i])
  print(prop_i)
}

for(i in 1:21){
  # Create a frequency table for the current column
  table_i <- table(descriptives[,i][descriptives$LCA5_class==3])
  # Create a percentage distribution for the current column
  prop_i <- prop.table(table_i)
  # Print the frequency table and percentage distribution for the current column
  print(colnames(descriptives)[i])
  print(prop_i)
}

for(i in 1:21){
  # Create a frequency table for the current column
  table_i <- table(descriptives[,i][descriptives$LCA5_class==5])
  # Create a percentage distribution for the current column
  prop_i <- prop.table(table_i)
  # Print the frequency table and percentage distribution for the current column
  print(colnames(descriptives)[i])
  print(prop_i)
}






apply(descriptives[, 1:21], 2, prop.table(table[descriptives$LCA2_class==1]))



for(j in 1:21) {
for(n in 1:2) {
print(prop.table(table(descriptives[j][descriptives$LCA2_class==n])))
}
}


for(n in 1:2) {
  print(prop.table(table(recode_class$age.1stsex.her[recode_class$LCA2_class==n])))
  prop.table(table(recode_class$death.parent[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$caste_[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$edulevel.her_[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$edulevel.his_[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$family.size_[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$support.his_[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$house.stress[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$house.conflict[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$ipv.accept[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$gender.role_[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$decide.child_most_[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$decide.big_most_[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$media_[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$house.asset_2[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$support.chw_[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$WASH.water_[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$WASH.hyge_[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$drainage_[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$self.help_[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$climate.money_[recode_class$LCA2_class==n]))
  prop.table(table(recode_class$home.years_[recode_class$LCA2_class==n]))
}





#### DELETE BELOW 
dat <- readRDS("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/Kenya 2022 DHS/LCA/LCA 10 reps Jan 25/dhs.seg_rural.rds")


LCA3 <- poLCA(formula, data=recode, nclass=2, maxiter=3000, nrep=100)
percent_mle = sum(round(LCA3$attempts,100) == round(LCA3$llik,100))/length(LCA3$attempts)
hist(round(LCA3$llik,100))
LCA3$bic
recode_class <- cbind(recode, LCA3_class=LCA3$predclass)
prop.table(table(recode_class$predclass))

LCA3 <- poLCA(formula, data=recode, nclass=2, maxiter=3000, nrep=100)
LCA3_percent_mle= sum(round(LCA3$attempts,100) == round(LCA3$llik,100))/length(LCA3$attempts)
hist(round(LCA3$llik,100))
LCA3$bic
recode_class <- cbind(recode, LCA3_class=LCA3$predclass)
prop.table(table(recode_class$predclass))


LCA5 <- poLCA(formula, data=recode, nclass=5, maxiter=3000, nrep=100)
percent_mle = sum(round(LCA5$attempts,digits = -1) == round(LCA5$llik,digits = -1))/length(LCA5$attempts)
hist(round(LCA5$attempts, digits = -1))
mean(LCA5$attempts)
LCA5$bic


# Fit LCA with 5 classes (NULL model)
mod_null <- poLCA(formula, data = recode, nclass = 5, verbose = F, maxiter=3000, nrep=10)  
percent_mle_null = sum(round(mod_null$attempts,0) == round(mod_null$llik,0))/length(mod_null$attempts)
# store values baseline model
n <- mod_null$Nobs #number of observations (should be equal in both models)
null_ll <- mod_null$llik #log-likelihood
null_param <- mod_null$npar # number of parameters
null_classes <- length(mod_null$P) # number of classes
mod_null$bic

# Fit LCA with 6 classes (ALTERNATIVE model)
mod_alt <- poLCA(formula, data = recode, nclass = 6, verbose = F, maxiter=3000, nrep=10)  
percent_mle_alt = sum(round(mod_alt$attempts,0) == round(mod_alt$llik,0))/length(mod_alt$attempts)
# Store values alternative model
alt_ll <- mod_alt$llik #log-likelihood
alt_param <- mod_alt$npar # number of parameters
alt_classes <- length(mod_alt$P) # number of classes
summary(mod_alt)
mod_alt$bic

#------ run a sequence of models with 1-10 classes and print out the model with the lowest BIC
max_II <- -100000
min_bic <- 100000
for(i in 2:10){
  lc <- poLCA(formula, recode, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
}    	
LCA_best_model


LCA3 <- poLCA(formula, data=recode, nclass=3, maxiter=3000, nrep=100)
percent_mle = sum(LCA3$attempts == LCA3$llik)/length(LCA3$attempts)
LCA3$attempts
LCA3$llik
LCA3$attempts == LCA3$llik
sum(LCA3$attempts == LCA3$llik)
percent_mle = sum(round(LCA3$attempts,0) == round(LCA3$llik,0))/length(LCA3$attempts)
percent_mle



#attempts: a vector containing the maximum log-likelihood values found in each of the nrep attempts to fit the model.
#llik maximum value of the log-likelihood.

LCA4 <- poLCA(formula, data=recode, nclass=4)
LCA5 <- poLCA(formula, data=recode, nclass=5)
LCA6 <- poLCA(formula, data=recode, nclass=6) #smallest class is 11%

LCA6 <- poLCA(formula, data=recode, nclass=6, maxiter=5000) #smallest class is 11%

LCA7 <- poLCA(formula, data=recode, nclass=7)
LCA8 <- poLCA(formula, data=recode, nclass=8)
LCA9 <- poLCA(formula, data=recode, nclass=9)
LCA10 <- poLCA(formula, data=recode, nclass=10)



### descriptive of each of the 6 classes
for(var in 1:6) {
  print(paste("Class ", var))
  print(prop.table(table(class$age.1stsex.her[class$predclass==var])))
}

for(var in 1:6) {
  print(paste("Class ", var))
  print(prop.table(table(class$death.parent[class$predclass==var])))
} 

for(var in 1:6) {
  print(paste("Class ", var))
  print(prop.table(table(class$caste[class$predclass==var])))
} 

for(var in 1:6) {
  print(paste("Class ", var))
  print(prop.table(table(class$edulevel.her[class$predclass==var])))
  print(prop.table(table(class$edulevel.his[class$predclass==var])))
}

for(var in 1:6) {
  print(paste("Class ", var))
  print(prop.table(table(class$family.size[class$predclass==var])))
} 

for(var in 1:6) {
  print(paste("Class ", var))
  print(prop.table(table(class$gender.role_2[class$predclass==var])))
} 

for(var in 1:6) {
  print(paste("Class ", var))
  print(prop.table(table(class$ipv.accept[class$predclass==var])))
} 

for(var in 1:6) {
  print(paste("Class ", var))
  print(prop.table(table(class$support.his_2[class$predclass==var])))
} 

for(var in 1:6) {
  print(paste("Class ", var))
  print(prop.table(table(class$decide.child_most[class$predclass==var])))
  print(prop.table(table(class$decide.big_most[class$predclass==var])))
} 
# class 4 husband decides the most

for(var in 1:6) {
  print(paste("Class ", var))
  print(prop.table(table(class$house.stress[class$predclass==var])))
  print(prop.table(table(class$house.conflict[class$predclass==var])))
} 

for(var in 1:6) {
  print(paste("Class ", var))
  print(prop.table(table(class$media_2[class$predclass==var])))
  print(prop.table(table(class$drainage[class$predclass==var])))
  print(prop.table(table(class$WASH.water[class$predclass==var])))
  print(prop.table(table(class$WASH.hyge[class$predclass==var])))
  print(prop.table(table(class$house.asset_2[class$predclass==var])))
} 

for(var in 1:6) {
  print(paste("Class ", var))
  print(prop.table(table(class$support.chw[class$predclass==var])))
} 

for(var in 1:6) {
  print(paste("Class ", var))
  print(prop.table(table(class$self.help[class$predclass==var])))
} 


for(var in 1:6) {
  print(paste("Class ", var))
  print(prop.table(table(class$climate.money[class$predclass==var])))
} 

for(var in 1:6) {
  print(paste("Class ", var))
  print(prop.table(table(class$home.years[class$predclass==var])))
}

