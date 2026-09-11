
### 9a.4_Ethiopia_DHS_LCA 
### ### Purpose: fourth script in LCA analysis with converged results, the purpose is to relabel to order the sankey diagram rural and urban without age
### Script to analyze results of LCA analysis 
### Inputs: dhs_seg_LCA.rds, 5_Ethiopia_DHS_exploratory.rds, mlres_rural_wage, mlres_urban_wage
### calls: 10_Ethiopia_DHS_cluster_plots
### outputs:  dhs.seg_urban, dhs.seg_rural
### ancestor: 9a3_Ethiopia_DHS_LCA
### descendent: 11_Ethiopia_DHS_CART
### Note: each user needs to change working directory
### Note: this script requires changing variables by hand and rerunning code. Ideally this could be put in a loop at some point ######


rm(list=ls())
pacman::p_load("poLCA", "plyr", "tidyverse", "readxl", "mice")

#library(poLCA)
#library(plyr)
#library(tidyverse)
#library(readxl)
#library(mice)
require(ggplot2)
require(ggalluvial)
require(ggrepel)

#### set working directory, each user needs to change #######
#setwd("/Users/adigreif/Desktop/Gates Ethiopia/EthiopiaSegmentation/")
method.poLCA <- TRUE
source("10_Ethiopia_DHS_cluster_plots.R")


# ### Getting reformatted data for LCA analysis
 dhs.seg_reform <- readRDS("Data/Ethiopia_constructed_data/dhs_seg_LCA.rds")   ## reformated dhs variable for LCA analysis
 #dhs.seg_reform <- readRDS("Ethiopia Segmentation_v2/LCA/dhs_seg_LCA.rds")   ## reformated dhs variable for LCA analysis
 dhs.seg_reform_urban <- dhs.seg_reform[dhs.seg_reform$hh.urban==1,]
 dhs.seg_reform_urban <- dhs.seg_reform_urban[complete.cases(dhs.seg_reform_urban),]
 dhs.seg_reform_rural <- dhs.seg_reform[dhs.seg_reform$hh.urban==0,]
 dhs.seg_reform_rural <- dhs.seg_reform_rural[complete.cases(dhs.seg_reform_rural),]
# 
# 
# ### Getting original DHS data 
 dhs.seg <- readRDS("Data/Ethiopia_constructed_data/5_Ethiopia_DHS_exploratory.rds")
# ### Doing imputation
# meantest <- dhs.seg %>% 
#   filter(URBAN_RURA=="R") %>% 
#   mutate(ed.lev.ord=as.numeric(ed.lev),wealth.index.ur.ord=as.numeric(wealth.index.ur)) %>%   ###  AM: created these so could be summarised in next step
#   group_by(CLUSTERNO) %>%
#   summarize_at(c("pastoralnoagro.yn", "early.sex.15", "dv.index.yn", "muslim", "ed.lev.ord",         
#                  "hh.land", "asset.goats", "asset.sheep", "asset.chickens", "toilet.facility", 
#                  "water", "wealth.index.ur.ord", "hh.cook.inside.yn", "hh.rooms.2plus","hh.kidwom.rat", "STRATA"),    
#                mean)
# #imputation uses pmm, which is form of k nearest neighbor matching
# #it should have used logreg for pastoralnoagro.yn since it's binary. Setting that:
# imp1 <- mice(meantest, m = 5, maxit = 1, print = TRUE, seed=350)
# meth <- imp1$meth
# meth["pastoralnoagro.yn"] <- "logreg"
# imp1 <- mice(meantest, m = 5, maxit = 1, print = TRUE, seed=350)
# 
# #results of the first imputation. Ideally we would jitter standard error or pool multiple imputations, but unclear how to do that with binary variables.
# #checked these imputation results and they seem plausible by region
# imputed_data=complete(imp1,1)
# #see all the imputations
# c.broad <- complete(imp1, "broad")
# 
# #merge data back in and replace NA with the results. Using the first imputation
# imputed_data <- imputed_data %>% 
#   dplyr::rename('predpastoralnoagro' = 'pastoralnoagro.yn') %>% dplyr::select(c("CLUSTERNO", "predpastoralnoagro"))
# 
# #merge back
# dhs.seg <-merge(dhs.seg, imputed_data, by=c("CLUSTERNO"), all.x=T)
# 
# #replace actual values with predicted ones if actual ones are missing. Note some predicted values are missing due to unimputed urban clusters in meantest
# dhs.seg$pastoralnoagro.yn <- ifelse(!is.na(dhs.seg$predpastoralnoagro) & is.na(dhs.seg$pastoralnoagro.yn), dhs.seg$pastoralnoagro.yn, dhs.seg$pastoralnoagro.yn)
# 
# # variables that need to be categorized for LCA
 dhs.seg$conflict_incidents <- ifelse(dhs.seg$conflict_incidents<=25,'0-25',
                                      ifelse(dhs.seg$conflict_incidents>25 & dhs.seg$conflict_incidents<50,'26-50',
                                             ifelse(dhs.seg$conflict_incidents>=50,'50+',NA)))
 
 dhs.seg$highestyearsedinHH.yrs <- ifelse(dhs.seg$highestyearsedinHH.yrs>=7,'7+','0-7')
 dhs.seg$num.kids.house.4plus <- ifelse(dhs.seg$num.kids.house>=4,1,0)
 
 comple.case.urban <- dhs.seg %>% filter(hh.urban==1) %>% select(highestyearsedinHH.yrs,num.kids.house,num.15up,conflict_incidents,med.index,
                                         wealth.index.ur.ord,dv.index,working.yn,partner.working.yn,hh.electricity,
                                         med.permis,water,any.media.yn,anyed.yn,early.sex.15,work.seasonal,jdwd.index.cat,age.adj,
                                         pship.cat,religionrecode.cat,caseid) %>% drop_na()
   
 comple.case.rural <- dhs.seg %>% filter(hh.urban==0) %>% select(highestyearsedinHH.yrs,num.kids.house,num.15up,conflict_incidents,med.index,
                                                                 wealth.index.ur.ord,dv.index,working.yn,partner.working.yn,hh.electricity,
                                                                 med.permis,water,any.media.yn,anyed.yn,early.sex.15,work.seasonal,jdwd.index.cat,
                                                                 pship.cat,religionrecode.cat,pastoralnoagro.yn,age.adj,caseid) %>% drop_na()
 
 dhs.seg_urban <- dhs.seg[dhs.seg$caseid %in% comple.case.urban$caseid,]
 dhs.seg_rural <- dhs.seg[dhs.seg$caseid %in% comple.case.rural$caseid,]
 
 mle_round <- function(x,roundLev=50){
   x <- as.data.frame(x)
   x$llh <- as.numeric(rownames(x))
   x$llh <- round_any(x =   x$llh, roundLev)
   newX <- aggregate(x$V1,by=list(x$llh),sum)
   print(newX)
 }
 


###### Rural ########
 rural <- readRDS('Data/Ethiopia_constructed_data/mlres_rural_wage.rds')
 #rural <- readRDS('Ethiopia Segmentation_v2/LCA/mlres_rural_wage.rds')
 
 ## check convergence for different class models 
 #k = 2
 rural[[1]]  # converged with llh -163835.63  
 #k = 3
 rural[[2]]  # converged with llh around -181726.92 
 #k = 4
 rural[[3]]  # converged at or around -156000
 mle_round(x=rural[[3]],roundLev = 100)
 #k = 5
 rural[[4]]  # converged with llh around  -171200 but starting to look like not converging
 mle_round(x=rural[[4]],roundLev = 100)
 #k = 6
 rural[[5]]  # converged with llh around -152400  but seems like really starting to not converge
 mle_round(x=rural[[5]],roundLev = 100)
 #k = 7
 rural[[6]]  # seems like really starting to not converge
 mle_round(x=rural[[6]],roundLev = 100)
 #k = 8
 rural[[7]]  # didn't really converge
 mle_round(x=rural[[7]],roundLev = 100)
 #k = 9
 rural[[8]]  # didn't really converge
 mle_round(x=rural[[8]],roundLev = 100)
 #k = 10
 rural[[9]]  # probably didn't converge, but with llh around -148200  seemed ok
 mle_round(x=rural[[9]],roundLev = 100)
 
 
 f <- cbind(highestyearsedinHH.yrs,num.kids.house,num.15up,conflict_incidents,
            med.index,wealth.index.ur.ord,dv.index,
            working.yn,partner.working.yn,hh.electricity,      
            med.permis,pastoralnoagro.yn,
            water,any.media.yn,anyed.yn,early.sex.15,age.adj,
            work.seasonal,jdwd.index.cat,pship.cat,religionrecode.cat)~1
 
 ## based on convergence only go up to k=7
 rural_class_comp <- data.frame('k'=2:7,'BIC'=numeric(6),'AIC'=numeric(6),'Chisq'=numeric(6),'llh'=numeric(6),'iter'=numeric(6))
 rural_pred_class <- list()
 for(i in 1:6){
   print(i)
   res <- poLCA(f,dhs.seg_reform_rural,nclass=(i+1),maxiter=10000,tol=1e-7,nrep=10,verbose = FALSE)
   rural_class_comp[rural_class_comp$k==(i+1),] <- c((i+1),res$bic,res$aic,res$Chisq,res$llik,res$numiter)
   rural_pred_class[[i]] <- res$posterior
   
   dhs.seg_rural$cluster <- res$predclass
   
   pdf(paste0('Plots/LCA_rural_age_plots/LCA_plots_k',(i+1),'.pdf'),height=8,width=12)
   #pdf(paste0('Ethiopia Segmentation_v2/LCA/rural_age/LCA_plots_k',(i+1),'.pdf'),height=8,width=12)
   print(heatmap.bin(dhs.seg_rural))
   print(stacked.cat(dhs.seg_rural))
   dev.off()  
   
   names(dhs.seg_rural)[names(dhs.seg_rural)=='cluster'] <- paste0('cluster_k',(i+1))
 }
#rural_class_comp

#saveRDS(dhs.seg_rural,'C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/Ethiopia Segmentation_v2/LCA/rural_age/#dhs_rural_age.rds')
saveRDS(dhs.seg_rural,'Data/Ethiopia_constructed_data/LCA_rural_age_data/dhs_rural_age.rds')


#### START HERE FOR SANKEY PLOT RENAMING ########
##########################
dhs.seg_rural <- readRDS('Data/Ethiopia_constructed_data/LCA_rural_age_data/dhs_rural_age.rds')
#dhs.seg_rural_LCA <- readRDS('dhs_seg_LCA.rds')

## Based on convegence and BIC will consider 4-8 clusters 
dhs.seg_rural$cluster_k2_rename <- ifelse(dhs.seg_rural$cluster_k2==1,'RM: Married','RU: Unmarried')
dhs.seg_rural$cluster_k2_rename <- factor(dhs.seg_rural$cluster_k2_rename,levels=c('RM: Married','RU: Unmarried'))

dhs.seg_rural$cluster_k3_rename <- ifelse(dhs.seg_rural$cluster_k3==1,'RM.1: Married, not working',
                                          ifelse(dhs.seg_rural$cluster_k3==3,'RM.2: Married, working',
                                                 ifelse(dhs.seg_rural$cluster_k3==2,'RU.1: Unmarried',NA)))
dhs.seg_rural$cluster_k3_rename <- factor(dhs.seg_rural$cluster_k3_rename,levels=c('RM.1: Married, not working','RM.2: Married, working','RU.1: Unmarried'))
table(dhs.seg_rural$cluster_k3_rename,exclude = NULL)

dhs.seg_rural$cluster_k4_rename <- ifelse(dhs.seg_rural$cluster_k4==3,'RM.1: Married, not working',
                                          ifelse(dhs.seg_rural$cluster_k4==2,'RM.2: Married, working',
                                                 ifelse(dhs.seg_rural$cluster_k4==4,'RU.1: Unmarried, working',
                                                        ifelse(dhs.seg_rural$cluster_k4==1, 'RU.2: Unmarried, not working',NA))))
dhs.seg_rural$cluster_k4_rename <- factor(dhs.seg_rural$cluster_k4_rename,levels=c('RM.1: Married, not working','RM.2: Married, working','RU.1: Unmarried, working','RU.2: Unmarried, not working'))
table(dhs.seg_rural$cluster_k4_rename,exclude = NULL)


#in theory this code should work, but everything except RU.1 and RU.2 moved to null, so see alternative code below instead
#dhs.seg_rural$cluster_k5_rename <- ifelse(dhs.seg_rural$cluster_k5==2,'RM.1: Married, not working, lo income',
#                                          ifelse(dhs.seg_rural$cluster_k5==3,'RM.2: Married, not working, higher income',
#                                                 ifelse(dhs.seg_rural$cluster_k5==4,'RM.3: Married, working',
#                                                        ifelse(dhs.seg_rural$cluster_k5==1, 'RU.1: Unmarried, not working',
#                                                               ifelse(dhs.seg_rural$cluster_k5==5,'RU.2: Unmarried, working',NA))))) #had this as not working, and the #difference was younger vs prior category older
#dhs.seg_rural$cluster_k5_rename <- factor(dhs.seg_rural$cluster_k5_rename,levels=c('RM.1: Married,not working, lo-income',
#                                                                                   'RM.2: Married,not working, higher-income',
#                                                                                   'RM.3: Married,working',
#                                                                                   'RU.1: Unmarried, not working',
#                                                                                   'RU.2: Unmarried, working'))
#table(dhs.seg_rural$cluster_k5_rename,exclude = NULL)

dhs.seg_rural<- dhs.seg_rural %>% mutate(cluster_k5_rename = case_when
                                           (
                                             cluster_k5 == "2" ~ "RM.1: Married, not working, lo income",
                                             cluster_k5 == "3" ~ "RM.2: Married, not working, higher income",
                                             cluster_k5 == "4" ~ "RM.3: Married, working",
                                             cluster_k5 == "1" ~ "RU.2: Unmarried, not working",
                                             cluster_k5 == "5" ~ "RU.1: Unmarried, working"
                                           )
)


#dhs.seg_rural$cluster_k6_rename <- ifelse(dhs.seg_rural$cluster_k6==3,'RM.1: Married, not working, lower-income',
#                                          ifelse(dhs.seg_rural$cluster_k6==6,'RM.3: Married, not working, hi income',
#                                                 ifelse(dhs.seg_rural$cluster_k6==1,'RM.2: Married, working, hi med index',
#                                                        ifelse(dhs.seg_rural$cluster_k6==2,'RM.4: Married, working,lo med index', 
#                                                               ifelse(dhs.seg_rural$cluster_k6==5, 'RU.1: Unmarried, working, older',
#                                                                      ifelse(dhs.seg_rural$cluster_k6==4,'RU.2: Unmarried, not working, younger',NA))))))
#
#dhs.seg_rural$cluster_k6_rename <- factor(dhs.seg_rural$cluster_k6_rename,levels=c('RM.1: Married, not working, lower-income',
#                                                                                   'RM.2: Married,working, hi med index',
#                                                                                   'RM.3: Married, not working, hi income',
#                                                                                   'RM.4: Married, working,lo med index',
#                                                                                   'RU.1: Unmarried, working, older',
#                                                                                   'RU.2: Unmarried,not working, younger'))


dhs.seg_rural<- dhs.seg_rural %>% mutate(cluster_k6_rename = case_when
                                         (
                                           cluster_k6 == "3" ~ "RM.2: Married, not working, hi income",
                                           cluster_k6 == "6" ~ "RM.1: Married, not working, lo income",
                                           cluster_k6 == "1" ~ "RM.3: Married, working, hi med index",
                                           cluster_k6 == "2" ~ "RM.4: Married, working, lo med index",
                                           cluster_k6 == "5" ~ "RU.1: Unmarried, working",
                                           cluster_k6 == "4" ~ "RU.2: Unmarried, not working"
                                           
                                         )
)


table(dhs.seg_rural$cluster_k6_rename,exclude = NULL)

saveRDS(dhs.seg_rural,'Data/Ethiopia_constructed_data/LCA_rural_age_data/dhs_rural_age.rds')


#dhs.seg_rural$cluster_k2_rename_short <-   sub("\\:.*", "", dhs.seg_rural$cluster_k2_rename)
#dhs.seg_rural$cluster_k3_rename_short <-   sub("\\:.*", "", dhs.seg_rural$cluster_k3_rename)
#dhs.seg_rural$cluster_k4_rename_short <-   sub("\\:.*", "", dhs.seg_rural$cluster_k4_rename)
#dhs.seg_rural$cluster_k5_rename_short <-   sub("\\:.*", "", dhs.seg_rural$cluster_k5_rename)
#dhs.seg_rural$cluster_k6_rename_short <-   sub("\\:.*", "", dhs.seg_rural$cluster_k6_rename)

## REproduce plots with new categories so matches sanky
#for(i in 1:5){
#  print(i)
#  dhs.seg_rural$cluster <- dhs.seg_rural[,paste0('cluster_k',(i+1),'_rename_short')]
  pdf(paste0('Plots/LCA_rural_age_plots/LCA_plots_k',(i+1),'_ordered.pdf'),height=8,width=12)
#  pdf(paste0('Ethiopia Segmentation_v2/LCA/rural_age/LCA_plots_k',(i+1),'_ordered.pdf'),height=8,width=12)
#  print(heatmap.bin(dhs.seg_rural))
#  print(stacked.cat(dhs.seg_rural))
#  dev.off()  
#  dhs.seg_rural$cluster <- NULL
#}


##simplified data for sankey
sankey_plot <- dhs.seg_rural
sankey_plot$id <- 1:nrow(dhs.seg_rural)
sankey_plot <- sankey_plot %>% dplyr::select(id, cluster_k2_rename,cluster_k3_rename,
                                             cluster_k4_rename, cluster_k5_rename, cluster_k6_rename)
#sankey_plot <- sankey_plot %>% rename('model_k2'='cluster_k2',
#                                      'model_k3'='cluster_k3',
#                                      'model_k4'='cluster_k4',
#                                      'model_k5'='cluster_k5',
#                                      'model_k6'='cluster_k6')


mat <- as.data.frame(table(sankey_plot$cluster_k2_rename, sankey_plot$cluster_k3_rename, sankey_plot$cluster_k4_rename,sankey_plot$cluster_k5_rename, sankey_plot$cluster_k6_rename))

p <- ggplot(mat, aes(axis1 = Var1, axis2 = Var2, 
                     axis3 = Var3, axis4 = Var4, axis5 = Var5, #comment out if using two axes only
                     y = Freq)) +
  geom_alluvium(aes(fill = Var5)) + 
  geom_stratum() +
  geom_label(stat = "stratum",
             aes(label = after_stat(stratum))) +
  scale_fill_brewer(palette = "Set2") + 
  theme_bw() + theme(legend.title = element_blank())
  scale_x_continuous('Model', breaks=1:5,labels=c('Model k=2', 'Model k=3','Model k=4', 'Model k=5', 'Model k=6')) 

  

pdf(paste0('Plots/LCA_rural_age_plots/LCA_rural_sankey_age.pdf'),width=14,height=8)
print(p)
dev.off()

prop.clust <- matrix(NA, nrow=6,ncol=7)
for(i in 1:6){
  x <- as.numeric(t(prop.table(table(dhs.seg_rural[,paste0('cluster_k',(i+1),'_rename')])))) # t transposes
  add <- rep(NA, 7-length(x)) #rep is replicate
  prop.clust[i,] <- c(x,add)
}
write.csv(prop.clust,'Data/Ethiopia_constructed_data/LCA_rural_age_data/cluster_proportions_rural_age.csv',row.names = F)









###### Urban ########
 urban <- readRDS('Data/Ethiopia_constructed_data/mlres_urban_wage.rds')
 
 ## check convergence for different class models 
 #k = 2
 urban[[1]]  # converged 
 #k = 3
 urban[[2]]  # converged 
 #k = 4
 urban[[3]]  # converged
 mle_round(x=urban[[3]],roundLev = 10)
 #k = 5
 urban[[4]]  #  starting to look like not converging
 mle_round(x=urban[[4]],roundLev = 100)
 #k = 6
 urban[[5]]  # starting to look like not converging
 mle_round(x=urban[[5]],roundLev = 100)
 #k = 7
 urban[[6]]  # converged 
 mle_round(x=urban[[6]],roundLev = 100)
 #k = 8
 urban[[7]]  # starting to look like not converging
 mle_round(x=urban[[7]],roundLev = 100)
 #k = 9
 urban[[8]]  # didn't converge
 mle_round(x=urban[[8]],roundLev = 100)
 #k = 10
 urban[[9]]  # didn't converge
 mle_round(x=urban[[9]],roundLev = 100)


 f <- cbind(highestyearsedinHH.yrs,num.kids.house,num.15up,conflict_incidents,
            med.index,wealth.index.ur.ord,dv.index,
            working.yn,partner.working.yn,hh.electricity,      
            med.permis,age.adj,
            water,any.media.yn,anyed.yn,early.sex.15,        
            work.seasonal,jdwd.index.cat,pship.cat,religionrecode.cat)~1
 
 urban_class_comp <- data.frame('k'=2:8,'BIC'=numeric(7),'AIC'=numeric(7),'Chisq'=numeric(7),'llh'=numeric(7),'iter'=numeric(7))
 urban_pred_class <- list()
 for(i in 1:7){
   print(i)
   res <- poLCA(f,dhs.seg_reform_urban,nclass=(i+1),maxiter=10000,tol=1e-7,nrep=10,verbose = FALSE)
   urban_class_comp[urban_class_comp$k==(i+1),] <- c((i+1),res$bic,res$aic,res$Chisq,res$llik,res$numiter)
   urban_pred_class[[i]] <- res$posterior
   
   dhs.seg_urban$cluster <- res$predclass
   
   pdf(paste0('Plots/LCA_urban_age_plots/LCA_plots_k',(i+1),'.pdf'),height=8,width=12)
   print(heatmap.bin(dhs.seg_urban))
   print(stacked.cat(dhs.seg_urban))
   dev.off()  
   
   names(dhs.seg_urban)[names(dhs.seg_urban)=='cluster'] <- paste0('cluster_k',(i+1))
 }

#urban_class_comp
#saveRDS(dhs.seg_urban,'C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/Ethiopia Segmentation_v2/LCA/urban_age/#dhs_urban_age.rds')
saveRDS(dhs.seg_urban,'Data/Ethiopia_constructed_data/LCA_urban_age_data/dhs_urban_age.rds')

####################################
#### START HERE FOR SANKEY RENAMING ########

dhs.seg_urban <- readRDS('Data/Ethiopia_constructed_data/LCA_urban_age_data/dhs_urban_age.rds')
#dhs.seg_urban <- readRDS('dhs_urban_age.rds')
### Based on this can pick 4-6 clusters reasonably
#dhs.seg_urban <- readRDS('C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/Ethiopia Segmentation_v2/LCA/urban_age/dhs_urban_age.rds')
dhs.seg_urban$cluster_k2_rename <- ifelse(dhs.seg_urban$cluster_k2==2,'UM: Married','UU: Unmarried')
dhs.seg_urban$cluster_k2_rename <- factor(dhs.seg_urban$cluster_k2_rename,levels=c('UM: Married','UU: Unmarried'))

dhs.seg_urban$cluster_k3_rename <- ifelse(dhs.seg_urban$cluster_k3==2,'UM: Married',
                                          ifelse(dhs.seg_urban$cluster_k3==3,'UU.1: Unmarried, working',
                                                 ifelse(dhs.seg_urban$cluster_k3==1,'UU.2: Unmarried, not working',NA)))
dhs.seg_urban$cluster_k3_rename <- factor(dhs.seg_urban$cluster_k3_rename,levels=c('UM: Married','UU.1: Unmarried, working','UU.2: Unmarried, not working'))
table(dhs.seg_urban$cluster_k3_rename,exclude = NULL)

#dhs.seg_urban$cluster_k4_rename <- ifelse(dhs.seg_urban$cluster_k3==4,'UM.1: Mostly married, not working, low-income',
#                                          ifelse(dhs.seg_urban$cluster_k4==4,'UM.2: Married, not working, high-income',
#                                                 ifelse(dhs.seg_urban$cluster_k4==1,'UU.1: Unmarried, working',
#                                                        ifelse(dhs.seg_urban$cluster_k4==2, 'UU.2: Unmarried, not working',NA))))
#dhs.seg_urban$cluster_k4_rename <- factor(dhs.seg_urban$cluster_k4_rename,levels=c('UM.1: Mostly married, low-income','UM.2: Married, high-income','UU.1: Unmarried, working','UU.2: Unmarried, not working'))
#table(dhs.seg_urban$cluster_k4_rename,exclude = NULL)

dhs.seg_urban<- dhs.seg_urban %>% mutate(cluster_k4_rename = case_when(
  cluster_k4 == "3" ~ "UM.1: Mostly married, half not working, lo income",
  cluster_k4 == "4" ~ "UM.2: Married, half not working, hi income",
  cluster_k4 == "1" ~ "UU.1: Unmarried, working",
  cluster_k4 == "2" ~ "UU.2: Unmarried, not working"))

table(dhs.seg_urban$cluster_k4_rename,exclude = NULL)


#dhs.seg_urban$cluster_k5_rename <- ifelse(dhs.seg_urban$cluster_k5==2,'UM.1: Married, half working, low-income',
#                                          ifelse(dhs.seg_urban$cluster_k5==5,'UM.2: Married, working',
#                                                 ifelse(dhs.seg_urban$cluster_k5==3,'UM.3: Married, not working, (higher income)',
#                                                        ifelse(dhs.seg_urban$cluster_k5==1, 'UU.1: Unmarried working',
#                                                               ifelse(dhs.seg_urban$cluster_k5==4,'UU.2: Unmarried, not working',NA)))))
#dhs.seg_urban$cluster_k5_rename <- factor(dhs.seg_urban$cluster_k5_rename,levels=c('UM.1: Married,\nhalf working, low-income',
#                                                                                   'UM.2: Married, working',
#                                                                                   'UM.3: Married, not working',
#                                                                                   'UU.1: Unmarried,\nworking',
#                                                                                   'UU.2: Unmarried,\nnot working'))


dhs.seg_urban<- dhs.seg_urban %>% mutate(cluster_k5_rename = case_when(
                                           cluster_k5 == "2" ~ "UM.1: Married, half working, lo income",
                                           cluster_k5 == "5" ~ "UM.2: Married,  working",
                                           cluster_k5 == "3" ~ "UM.3: Married, not working (higher income)",
                                           cluster_k5 == "1" ~ "UU.1: Unmarried, working",
                                           cluster_k5 == "4" ~ "UU.2: Unmarried, not working"))

table(dhs.seg_urban$cluster_k5_rename,exclude = NULL)


dhs.seg_urban$cluster_k2_rename_short <-   sub("\\:.*", "", dhs.seg_urban$cluster_k2_rename)
dhs.seg_urban$cluster_k3_rename_short <-   sub("\\:.*", "", dhs.seg_urban$cluster_k3_rename)
dhs.seg_urban$cluster_k4_rename_short <-   sub("\\:.*", "", dhs.seg_urban$cluster_k4_rename)
dhs.seg_urban$cluster_k5_rename_short <-   sub("\\:.*", "", dhs.seg_urban$cluster_k5_rename)

saveRDS(dhs.seg_urban,'Data/Ethiopia_constructed_data/LCA_urban_age_data/dhs_urban_age.rds')

## REproduce plots with new categories so matches sanky
for(i in 1:5){
  print(i)
  dhs.seg_urban$cluster <- dhs.seg_urban[,paste0('cluster_k',(i+1),'_rename_short')]
  pdf(paste0('Plots/LCA_urban_age_plots/LCA_plots_k',(i+1),'_ordered.pdf'),height=8,width=12)
  #pdf(paste0('Ethiopia Segmentation_v2/LCA/urban_age/LCA_plots_k',(i+1),'_ordered.pdf'),height=8,width=12)
  print(heatmap.bin(dhs.seg_urban))
  print(stacked.cat(dhs.seg_urban))
  dev.off()  
  dhs.seg_urban$cluster <- NULL
}


##simplified data for sankey
sankey_plot <- dhs.seg_urban
sankey_plot$id <- 1:nrow(dhs.seg_urban)
sankey_plot <- sankey_plot %>% dplyr::select(id, cluster_k2_rename,cluster_k3_rename,
                                             cluster_k4_rename,cluster_k5_rename)

mat <- as.data.frame(table(sankey_plot$cluster_k2_rename, sankey_plot$cluster_k3_rename, sankey_plot$cluster_k4_rename,sankey_plot$cluster_k5_rename))

p <- ggplot(mat, aes(axis1 = Var1, axis2 = Var2, 
                     axis3 = Var3, axis4 = Var4,#comment out if using two axes only
                     y = Freq)) +
  geom_alluvium(aes(fill = Var4)) + 
  geom_stratum() +
  geom_label(stat = "stratum",
             aes(label = after_stat(stratum)),size=2) +
  scale_fill_brewer(palette = "Set2") + 
  theme_bw() + theme(legend.title = element_blank()) +guides(fill = 'none') +
  scale_x_continuous('Model', breaks=1:4,labels=c(paste0('Model k=',2:5)))

pdf(paste0('Plots/LCA_urban_age_plots/LCA_urban_sankey_age.pdf'),width=14,height=8)
#pdf(paste0('Ethiopia Segmentation_v2/LCA/urban_age/LCA_urban_sankey_age.pdf'),width=14,height=8)
print(p)
dev.off()

prop.clust <- matrix(NA, nrow=6,ncol=7)
for(i in 1:6){
  x <- as.numeric(t(prop.table(table(dhs.seg_urban[,paste0('cluster_k',(i+1),'_rename')]))))
  add <- rep(NA, 7-length(x))
  prop.clust[i,] <- c(x,add)
}
write.csv(prop.clust,'Data/Ethiopia_constructed_data/LCA_urban_age_data/cluster_proportion_urban_age.csv',row.names = F)
#write.csv(prop.clust,'Ethiopia Segmentation_v2/LCA/urban_age/cluster_prop.csv',row.names = F)
