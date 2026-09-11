
############## 
# 9a2_Ethiopia_DHS_LCA
# Purpose: second script in LCA analysis with converged results, helps to decide on number of clusters by looking at BIC values and plots, rural and urban with no age
# runs the cluster analysis for the cluster sizes with model convergences (k=5 for urban and rural, with and without age). 
# Ethiopia DHS 2016, Recode 7
# Using variables selected based on qualitative insights, univariate analyses, and pca 
# input data: 5_Ethiopia_DHS_exploratory.rds, 9a1_dhs.seg_reform_urban, 9a1_dhs.seg_reform_rural, mlres.rural, mlres.urban -- note that _final versions are saved for #replicability
# input scripts: 10_Ethiopia_DHS_cluster_plots.R
# ancestor code: 9a1_Ethiopia_DHS_LCA
# descendent code: 11_Ethiopia_DHS_CART (or 9a2_Ethiopia_DHS_LCA for sankey plot renaming labels playground)
#output:  9a2_dhs_urban_noage.rds, 9a2_dhs_rural_noage.rds, 9a2_cluster_proportion_rural_noage.csv, 9a2_cluster_proportion_urban_noage.csv and other plots
# output:  9a2_dhs.seg.final.des.rds, 9a2_dhs.seg.final.rds,
# note: each user needs to set the directory
#note: may need to change number of clusters. Some code chunks will show errors (ignorable) depending on the number of clusters selected at various points in this code #vs in creating mlres in 9a1
#note: the versions of the datasets used to make the latest presentations are saved with _final for reproducibility
#note: there may be errors if the number of clusters used in various places through 9a1 and 9a2 do not match, but running the rest of the script will still work
### Note: this script requires changing variables by hand and rerunning code. Ideally this could be put in a loop at some point ######


### Update AM 4/14/23: Swapped the ordering of RM1/RM3 and UM1/UM3 after additional profiling of clusters
### Update AM 6/26/23: Re-running file to produced corrected datasets
### Update DR 6/26/23: Re-running file to remove unncesssary clustering variables and create one final variable called "cluster"

rm(list=ls())
pacman::p_load("poLCA", "plyr", "tidyverse", "readxl", "data.table", "foreign", "survey")

#library(poLCA)
#library(plyr)
#library(tidyverse)
#library(readxl)
#library(mice)
require(ggplot2)
require(ggalluvial)
require(ggrepel)

### set working directory, each user needs to change ########
#setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/")
setwd("/Users/darcyrao/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Pathways - GE Data & Insights/Modeling/") # for use from Darcy's Mac
#setwd("~/Downloads/Modeling")
method.poLCA <- TRUE
source("Code/10_Ethiopia_DHS_cluster_plots.R", local=FALSE)


### Getting reformatted data for LCA analysis (numeric variable responses)

dhs.seg_reform_urban <- readRDS("Data/Ethiopia_constructed_data/LCA_urban_noage_data/9a1_dhs.seg_reform_urban.rds")
dhs.seg_reform_rural <- readRDS("Data/Ethiopia_constructed_data/LCA_rural_noage_data/9a1_dhs.seg_reform_rural.rds")
### ==> 6.26.2023, Anu thinks this this shoudl be 9a1_dhs_seg_LCA.rds, which combined the urban and rural. May need to separate them again for analyses


### Getting original DHS data 
dhs.seg <- readRDS("Data/Ethiopia_constructed_data/5_Ethiopia_DHS_exploratory.rds")

## variables that need to be categorized for LCA
       #A version of the data that still has categories rather than numeric responses is necessary for creating plots that show the categorical responses 
dhs.seg$conflict_incidents <- ifelse(dhs.seg$conflict_incidents<=25,'0-25',
                                     ifelse(dhs.seg$conflict_incidents>25 & dhs.seg$conflict_incidents<50,'26-50',
                                            ifelse(dhs.seg$conflict_incidents>=50,'50+',NA)))

dhs.seg$highestyearsedinHH.yrs <- ifelse(dhs.seg$highestyearsedinHH.yrs>=7,'7+','0-7')

comple.case.urban <- dhs.seg %>% filter(hh.urban==1) %>% select(highestyearsedinHH.7plus,num.kids.house.4plus,num.15up.4plus,conflict_incidents_26plus,med.index,
                                        wealth.index.ur.ord,dv.index,working.yn,partner.working.yn,hh.electricity,
                                        med.permis,water,any.media.yn,anyed.yn,early.sex.15,work.seasonal,jdwd.index.cat,
                                        pship.cat,religionrecode.cat,caseid) %>% drop_na()
  
comple.case.rural <- dhs.seg %>% filter(hh.urban==0) %>% select(highestyearsedinHH.7plus,num.kids.house.4plus,num.15up.4plus,conflict_incidents_26plus,med.index,
                                                                wealth.index.ur.ord,dv.index,working.yn,partner.working.yn,hh.electricity,
                                                                med.permis,water,any.media.yn,anyed.yn,early.sex.15,work.seasonal,jdwd.index.cat,
                                                                pship.cat,religionrecode.cat,pastoralnoagro.yn,caseid) %>% drop_na()

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
rural <- readRDS('Data/Ethiopia_constructed_data/LCA_rural_noage_data/mlres_rural.rds') #make sure mlres_rural was moved manually to the correct folder
#rural <- readRDS('Ethiopia Segmentation_v2/LCA/mlres_rural.rds')

## check convergence for different class models 
#k = 2
rural[[1]]  # converged with llh -163835.63 
#k = 3
rural[[2]]  # converged with llh around -157877.96 
#k = 4
rural[[3]]  # converged at or around -156000
mle_round(x=rural[[3]],roundLev = 100)
#k = 5
rural[[4]]  # converged with llh around -154600 but starting to look like not converging
mle_round(x=rural[[4]],roundLev = 100)
#k = 6
rural[[5]]  # converged with llh around -152400  but seems like really starting to not converge
mle_round(x=rural[[5]],roundLev = 100)
#k = 7
rural[[6]]  # converged with llh around -151000 but seems like really starting to not converge
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


f <- cbind(highestyearsedinHH.7plus,num.kids.house.4plus,num.15up.4plus,conflict_incidents_26plus,
           med.index,wealth.index.ur.ord,dv.index,
           working.yn,partner.working.yn,hh.electricity,      
           med.permis,pastoralnoagro.yn,
           water,any.media.yn,anyed.yn,early.sex.15,        
           work.seasonal,jdwd.index.cat,pship.cat,religionrecode.cat)~1

rural_class_comp <- data.frame('k'=2:10,'BIC'=numeric(9),'AIC'=numeric(9),'Chisq'=numeric(9),'llh'=numeric(9),'iter'=numeric(9))
rural_pred_class <- list()
for(i in 1:9){
  print(i)
  res <- poLCA(f,dhs.seg_reform_rural,nclass=(i+1),maxiter=10000,tol=1e-7,nrep=10,verbose = FALSE)
  rural_class_comp[rural_class_comp$k==(i+1),] <- c((i+1),res$bic,res$aic,res$Chisq,res$llik,res$numiter)
  rural_pred_class[[i]] <- res$posterior
  
  dhs.seg_rural$cluster <- res$predclass
  
  #pdf(paste0('Ethiopia Segmentation_v2/LCA/rural_noage/9a2_LCA_plots_k',(i+1),'.pdf'),height=8,width=12)
  pdf(paste0('Plots/LCA_rural_noage_plots/9a2_LCA_plots_k',(i+1),'.pdf'),height=8,width=12)

  print(heatmap.bin(dhs.seg_rural))
  print(stacked.cat(dhs.seg_rural))
  dev.off()  
  
  names(dhs.seg_rural)[names(dhs.seg_rural)=='cluster'] <- paste0('cluster_k',(i+1))
}



## Based on these will consider 4-8 clusters 
dhs.seg_rural$cluster_k2_rename <- ifelse(dhs.seg_rural$cluster_k2==1,'RM: Married','RU: Unmarried')
dhs.seg_rural$cluster_k2_rename <- factor(dhs.seg_rural$cluster_k2_rename,levels=c('RM: Married','RU: Unmarried'))

dhs.seg_rural$cluster_k3_rename <- ifelse(dhs.seg_rural$cluster_k3==2,'RM.1: Married, not working',
                                          ifelse(dhs.seg_rural$cluster_k3==1,'RM.2: Married, working',
                                                 ifelse(dhs.seg_rural$cluster_k3==3,'RU: Unmaried',NA)))
dhs.seg_rural$cluster_k3_rename <- factor(dhs.seg_rural$cluster_k3_rename,levels=c('RM.1: Married, not working','RM.2: Married, working','RU: Unmaried'))
table(dhs.seg_rural$cluster_k3_rename,exclude = NULL)

dhs.seg_rural$cluster_k4_rename <- ifelse(dhs.seg_rural$cluster_k4==1,'RM.1: Married, not working',
                                          ifelse(dhs.seg_rural$cluster_k4==2,'RM.2: Married, working',
                                                 ifelse(dhs.seg_rural$cluster_k4==3,'RU.1: Unmarried, working',
                                                        ifelse(dhs.seg_rural$cluster_k4==4, 'RU.2: Unmarried, not working',NA))))
dhs.seg_rural$cluster_k4_rename <- factor(dhs.seg_rural$cluster_k4_rename,levels=c('RM.1: Married, not working','RM.2: Married, working','RU.1: Unmarried, working','RU.2: Unmarried, not working'))
table(dhs.seg_rural$cluster_k4_rename,exclude = NULL)



## AM Update 4/14/23 -- Read in existing dataset and swap labels for RM1/RM3 and UM1/UM3
#dhs.seg_rural <- readRDS("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/Data/Ethiopia_constructed_data/LCA_rural_noage_data/dhs_rural_noage.rds")
dhs.seg_rural$cluster_k5_rename <- ifelse(dhs.seg_rural$cluster_k5==4,'RM.3: Married, working',
                                          ifelse(dhs.seg_rural$cluster_k5==1,'RM.2: Married,\nnot working, higher-income',
                                                 ifelse(dhs.seg_rural$cluster_k5==5,'RM.1: Married,\nnot working, low-income',
                                                        ifelse(dhs.seg_rural$cluster_k5==2, 'RU.1: Unmarried,\nworking, older',
                                                               ifelse(dhs.seg_rural$cluster_k5==3,'RU.2: Unmarried,\nnot working, younger',NA)))))
dhs.seg_rural$cluster_k5_rename <- factor(dhs.seg_rural$cluster_k5_rename,levels=c('RM.1: Married,\nnot working, low-income',
                                                                                   'RM.2: Married,\nnot working, higher-income',
                                                                                    'RM.3: Married, working',
                                                                                   'RU.1: Unmarried,\nworking, older',
                                                                                   'RU.2: Unmarried,\nnot working, younger'))
table(dhs.seg_rural$cluster_k5_rename,exclude = NULL)


dhs.seg_rural$cluster_k6_rename <- ifelse(dhs.seg_rural$cluster_k6==6,'RM.1: Married, not working,\nless partner working, low-income',
                                          ifelse(dhs.seg_rural$cluster_k6==3,'RM.3: Married, not working,\npartner working',
                                                 ifelse(dhs.seg_rural$cluster_k6==1,'RM.2: Married,\n half working, high med index',
                                                        ifelse(dhs.seg_rural$cluster_k6==5,'RM.4: Married, working,\npartner working, high JD index',
                                                               ifelse(dhs.seg_rural$cluster_k6==4, 'RU.1: Unmarried, working, older',
                                                                      ifelse(dhs.seg_rural$cluster_k6==2,'RU.2: Unmarried,\nnot working, younger',NA))))))

dhs.seg_rural$cluster_k6_rename <- factor(dhs.seg_rural$cluster_k6_rename,levels=c('RM.1: Married, not working,\nless partner working, low-income',
                                                                                   'RM.2: Married,\n half working, high med index',
                                                                                   'RM.3: Married, not working,\npartner working',
                                                                                   'RM.4: Married, working,\npartner working, high JD index',
                                                                                   'RU.1: Unmarried, working, older',
                                                                                   'RU.2: Unmarried,\nnot working, younger'))

table(dhs.seg_rural$cluster_k6_rename,exclude = NULL)

dhs.seg_rural$cluster_k2_rename_short <-   sub("\\:.*", "", dhs.seg_rural$cluster_k2_rename)
dhs.seg_rural$cluster_k3_rename_short <-   sub("\\:.*", "", dhs.seg_rural$cluster_k3_rename)
dhs.seg_rural$cluster_k4_rename_short <-   sub("\\:.*", "", dhs.seg_rural$cluster_k4_rename)
dhs.seg_rural$cluster_k5_rename_short <-   sub("\\:.*", "", dhs.seg_rural$cluster_k5_rename)
dhs.seg_rural$cluster_k6_rename_short <-   sub("\\:.*", "", dhs.seg_rural$cluster_k6_rename)
#saveRDS(dhs.seg_rural,'C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/Ethiopia Segmentation_v2/LCA/rural_noage/#dhs_rural_noage.rds')
saveRDS(dhs.seg_rural,'Data/Ethiopia_constructed_data/LCA_rural_noage_data/9a2_dhs_rural_noage.rds')




## REproduce plots with new categories so matches sanky
for(i in 1:5){
  print(i)
  dhs.seg_rural$cluster <- dhs.seg_rural[,paste0('cluster_k',(i+1),'_rename_short')]
  #pdf(paste0('Ethiopia Segmentation_v2/LCA/rural_noage/9a2_LCA_plots_k',(i+1),'_ordered.pdf'),height=8,width=12)
pdf(paste0('Plots/LCA_rural_noage_plots/9a2_LCA_plots_k',(i+1),'_ordered.pdf'),height=8,width=12)
  print(heatmap.bin(dhs.seg_rural))
  print(stacked.cat(dhs.seg_rural))
  dev.off()  
  dhs.seg_rural$cluster <- NULL
}


##simplified data for sankey
sankey_plot <- dhs.seg_rural
sankey_plot$id <- 1:nrow(dhs.seg_rural)
sankey_plot <- sankey_plot %>% dplyr::select(id, cluster_k2_rename,cluster_k3_rename,
                                             cluster_k4_rename,cluster_k5_rename,cluster_k6_rename)
sankey_plot <- sankey_plot %>% rename('model_k2'='cluster_k2_rename',
                                      'model_k3'='cluster_k3_rename',
                                      'model_k4'='cluster_k4_rename',
                                      'model_k5'='cluster_k5_rename',
                                      'model_k6'='cluster_k6_rename')


mat <- as.data.frame(table(sankey_plot$model_k2, sankey_plot$model_k3, sankey_plot$model_k4,sankey_plot$model_k5,sankey_plot$model_k6))

p <- ggplot(mat, aes(axis1 = Var1, axis2 = Var2, 
                     axis3 = Var3, axis4 = Var4, axis5 = Var5, #comment out if using two axes only
                     y = Freq)) +
  geom_alluvium(aes(fill = Var5)) + 
  geom_stratum() +
  geom_label(stat = "stratum",
             aes(label = after_stat(stratum)),size=2) +
  scale_fill_brewer(palette = "Set2") + 
  theme_bw() + theme(legend.title = element_blank()) +guides(fill = 'none') +
  scale_x_continuous('Model', breaks=1:5,labels=c(paste0('Model k=',2:6)))

pdf(paste0('Plots/LCA_rural_noage_plots/9a2_LCA_rural_sankey_noage.pdf'),width=14,height=8)
print(p)
dev.off()

prop.clust <- matrix(NA, nrow=6,ncol=7)
for(i in 1:6){
 x <- as.numeric(t(prop.table(table(dhs.seg_rural[,paste0('cluster_k',(i+1),'_rename')]))))
 add <- rep(NA, 7-length(x))
 prop.clust[i,] <- c(x,add)
}
write.csv(prop.clust,'Plots/LCA_rural_noage_plots/9a2_cluster_proportion_rural_noage.csv',row.names = F)



###### Urban ########
urban <- readRDS('Data/Ethiopia_constructed_data/LCA_urban_noage_data/mlres_urban.rds') #may need to manually move mlres_urban to correct folder
#urban <- readRDS('Ethiopia Segmentation_v2/LCA/mlres_urban.rds')

## check convergence for different class models 
#k = 2
urban[[1]]  # converged with llh -65792 
#k = 3
urban[[2]]  # converged with llh around -63982
#k = 4
urban[[3]]  # converged with llh around -62660
mle_round(x=urban[[3]],roundLev = 10)
#k = 5
urban[[4]]  # converged with llh around -61000 but starting to look like not converging
mle_round(x=urban[[4]],roundLev = 100)
#k = 6
urban[[5]]  # converged with llh around -59700 but seems like really starting to not converge
mle_round(x=urban[[5]],roundLev = 100)
#k = 7
urban[[6]]  # converged with llh around -59100 did seem to converge
mle_round(x=urban[[6]],roundLev = 100)
#k = 8
urban[[7]]  # converged with llh around -58600 but doesn't really converge
mle_round(x=urban[[7]],roundLev = 100)
#k = 9
urban[[8]]  # converged with llh around -58100 did seem to converge
mle_round(x=urban[[8]],roundLev = 100)
#k = 10
urban[[9]]  # converged with llh around -57700 did seem to converge
mle_round(x=urban[[9]],roundLev = 100)


f <- cbind(highestyearsedinHH.7plus,num.kids.house.4plus,num.15up.4plus,conflict_incidents_26plus,
           med.index,wealth.index.ur.ord,dv.index,
           working.yn,partner.working.yn,hh.electricity,      
           med.permis,
           water,any.media.yn,anyed.yn,early.sex.15,        
           work.seasonal,jdwd.index.cat,pship.cat,religionrecode.cat)~1

urban_class_comp <- data.frame('k'=2:10,'BIC'=numeric(9),'AIC'=numeric(9),'Chisq'=numeric(9),'llh'=numeric(9),'iter'=numeric(9))
urban_pred_class <- list()
for(i in 1:9){
  print(i)
  res <- poLCA(f,dhs.seg_reform_urban,nclass=(i+1),maxiter=10000,tol=1e-7,nrep=10,verbose = FALSE)
  urban_class_comp[urban_class_comp$k==(i+1),] <- c((i+1),res$bic,res$aic,res$Chisq,res$llik,res$numiter)
  urban_pred_class[[i]] <- res$posterior
  
  dhs.seg_urban$cluster <- res$predclass
  
  pdf(paste0('Plots/LCA_urban_noage_plots/9a2_LCA_plots_k',(i+1),'.pdf'),height=8,width=12)
  print(heatmap.bin(dhs.seg_urban))
  print(stacked.cat(dhs.seg_urban))
  dev.off()  
  
  names(dhs.seg_urban)[names(dhs.seg_urban)=='cluster'] <- paste0('cluster_k',(i+1))
}



dhs.seg_urban$cluster_k2_rename <- ifelse(dhs.seg_urban$cluster_k2==1,'UM: Married','UU: Unmarried')
dhs.seg_urban$cluster_k2_rename <- factor(dhs.seg_urban$cluster_k2_rename,levels=c('UM: Married','UU: Unmarried'))

dhs.seg_urban$cluster_k3_rename <- ifelse(dhs.seg_urban$cluster_k3==3,'UM: Married',
                                          ifelse(dhs.seg_urban$cluster_k3==1,'UU.1: Unmarried, working',
                                                 ifelse(dhs.seg_urban$cluster_k3==2,'UU.2: Unmaried, not working',NA)))
dhs.seg_urban$cluster_k3_rename <- factor(dhs.seg_urban$cluster_k3_rename,levels=c('UM: Married','UU.1: Unmarried, working','UU.2: Unmaried, not working'))
table(dhs.seg_urban$cluster_k3_rename,exclude = NULL)

dhs.seg_urban$cluster_k4_rename <- ifelse(dhs.seg_urban$cluster_k4==3,'UM.1: Mostly married, low-income',
                                          ifelse(dhs.seg_urban$cluster_k4==1,'UM.2: Married, high-income',
                                                 ifelse(dhs.seg_urban$cluster_k4==4,'UU.1: Unmarried, working',
                                                        ifelse(dhs.seg_urban$cluster_k4==2, 'UU.2: Unmarried, not working',NA))))
dhs.seg_urban$cluster_k4_rename <- factor(dhs.seg_urban$cluster_k4_rename,levels=c('UM.1: Mostly married, low-income','UM.2: Married, high-income','UU.1: Unmarried, working','UU.2: Unmarried, not working'))
table(dhs.seg_urban$cluster_k4_rename,exclude = NULL)


## AM Update 4/14/23 -- Read in existing dataset and swap lables for RM1/RM3 and UM1/UM3
#dhs.seg_urban <- readRDS("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/Data/Ethiopia_constructed_data/LCA_urban_noage_data/dhs_urban_noage.rds")
dhs.seg_urban$cluster_k5_rename <- ifelse(dhs.seg_urban$cluster_k5==5,'UM.3: Married, not working',
                                          ifelse(dhs.seg_urban$cluster_k5==2,'UM.2: Married, working',
                                                 ifelse(dhs.seg_urban$cluster_k5==4,'UM.1: Married,\nhalf working, low-income',
                                                        ifelse(dhs.seg_urban$cluster_k5==1, 'UU.1: Unmarried,\nworking',
                                                               ifelse(dhs.seg_urban$cluster_k5==3,'UU.2: Unmarried,\nnot working',NA)))))
dhs.seg_urban$cluster_k5_rename <- factor(dhs.seg_urban$cluster_k5_rename,levels=c('UM.1: Married,\nhalf working, low-income',
                                                                                   'UM.2: Married, working',
                                                                                   'UM.3: Married, not working',
                                                                                   'UU.1: Unmarried,\nworking',
                                                                                   'UU.2: Unmarried,\nnot working'))
table(dhs.seg_urban$cluster_k5_rename,exclude = NULL)


dhs.seg_urban$cluster_k6_rename <- ifelse(dhs.seg_urban$cluster_k6==5,'UM.1: Married,\nhalf working, low-income',
                                          ifelse(dhs.seg_urban$cluster_k6==6,'UM.2: Married, not working,\nhigh-income',
                                                 ifelse(dhs.seg_urban$cluster_k6==4,'UM.3: Married, working,\nhigh income, highest JD',
                                                        ifelse(dhs.seg_urban$cluster_k6==3,'UU.1: Mostly previously partnered\n,working, lower income',
                                                               ifelse(dhs.seg_urban$cluster_k6==1, 'UU.2: Mostly never married,\nnot working, mid income',
                                                                      ifelse(dhs.seg_urban$cluster_k6==2,'UU.3: Unmarnried,\nworking, high-income',NA))))))

dhs.seg_urban$cluster_k6_rename <- factor(dhs.seg_urban$cluster_k6_rename,levels=c('UM.1: Married,\nhalf working, low-income',
                                                                                   'UM.2: Married, not working,\nhigh-income',
                                                                                   'UM.3: Married, working,\nhigh income, highest JD',
                                                                                   'UU.1: Mostly previously partnered\n,working, lower income',
                                                                                   'UU.2: Mostly never married,\nnot working, mid income',
                                                                                   'UU.3: Unmarnried,\nworking, high-income'))

table(dhs.seg_urban$cluster_k6_rename,exclude = NULL)



dhs.seg_urban$cluster_k7_rename <- ifelse(dhs.seg_urban$cluster_k7==3,'UM.1: Mostly married,\nnot working, low income',
                                          ifelse(dhs.seg_urban$cluster_k7==4,'UM.2: Married, working,\nless educated',
                                                 ifelse(dhs.seg_urban$cluster_k7==7,'UM.3: Married,\nnot working, high-income',
                                                        ifelse(dhs.seg_urban$cluster_k7==1,'UM.4: married, working,\nhigh income, high JD',
                                                               ifelse(dhs.seg_urban$cluster_k7==2,'UU.1: Unmarried,\nworking, middle income, more muslim',
                                                                   ifelse(dhs.seg_urban$cluster_k7==6, 'UU.2: Mostly never married,\nworking, high income',
                                                                      ifelse(dhs.seg_urban$cluster_k7==5,'UU.3: Mostly never married,\nnot working, young',NA)))))))

dhs.seg_urban$cluster_k7_rename <- factor(dhs.seg_urban$cluster_k7_rename,levels=c('UM.1: Mostly married,\nnot working, low income',
                                                                                   'UM.2: Married, working,\nless educated',
                                                                                   'UM.3: Married,\nnot working, high-income',
                                                                                   'UM.4: married, working,\nhigh income, high JD',
                                                                                   'UU.1: Unmarried,\nworking, middle income, more muslim',
                                                                                   'UU.2: Mostly never married,\nworking, high income',
                                                                                   'UU.3: Mostly never married,\nnot working, young'))

table(dhs.seg_urban$cluster_k7_rename,exclude = NULL)


dhs.seg_urban$cluster_k2_rename_short <-   sub("\\:.*", "", dhs.seg_urban$cluster_k2_rename)
dhs.seg_urban$cluster_k3_rename_short <-   sub("\\:.*", "", dhs.seg_urban$cluster_k3_rename)
dhs.seg_urban$cluster_k4_rename_short <-   sub("\\:.*", "", dhs.seg_urban$cluster_k4_rename)
dhs.seg_urban$cluster_k5_rename_short <-   sub("\\:.*", "", dhs.seg_urban$cluster_k5_rename)
dhs.seg_urban$cluster_k6_rename_short <-   sub("\\:.*", "", dhs.seg_urban$cluster_k6_rename)
dhs.seg_urban$cluster_k7_rename_short <-   sub("\\:.*", "", dhs.seg_urban$cluster_k7_rename)

#saveRDS(dhs.seg_urban,'C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/Ethiopia Segmentation_v2/LCA/urban_noage/#dhs_urban_noage.rds')
saveRDS(dhs.seg_urban,'Data/Ethiopia_constructed_data/LCA_urban_noage_data/9a2_dhs_urban_noage.rds')

## REproduce plots with new categories so matches sanky
for(i in 1:6){
  print(i)
  dhs.seg_urban$cluster <- dhs.seg_urban[,paste0('cluster_k',(i+1),'_rename_short')]
  #pdf(paste0('Ethiopia Segmentation_v2/LCA/urban_noage/9a2_LCA_plots_k',(i+1),'_ordered.pdf'),height=8,width=12)
  pdf(paste0('Plots/LCA_urban_noage_plots/9a2_LCA_plots_k',(i+1),'_ordered.pdf'),height=8,width=12)
  print(stacked.cat(dhs.seg_urban))
  dev.off()  
  dhs.seg_urban$cluster <- NULL
}


##simplified data for sankey
sankey_plot <- dhs.seg_urban
sankey_plot$id <- 1:nrow(dhs.seg_urban)
sankey_plot <- sankey_plot %>% dplyr::select(id, cluster_k2_rename,cluster_k3_rename,
                                             cluster_k4_rename,cluster_k5_rename,cluster_k6_rename,cluster_k7_rename)
sankey_plot <- sankey_plot %>% rename('model_k2'='cluster_k2_rename',
                                      'model_k3'='cluster_k3_rename',
                                      'model_k4'='cluster_k4_rename',
                                      'model_k5'='cluster_k5_rename',
                                      'model_k6'='cluster_k6_rename',
                                      'model_k7'='cluster_k7_rename')


mat <- as.data.frame(table(sankey_plot$model_k2, sankey_plot$model_k3, sankey_plot$model_k4,sankey_plot$model_k5,
                           sankey_plot$model_k6,sankey_plot$model_k7))

p <- ggplot(mat, aes(axis1 = Var1, axis2 = Var2, 
                     axis3 = Var3, axis4 = Var4, axis5 = Var5,axis6 = Var6,#comment out if using two axes only
                     y = Freq)) +
  geom_alluvium(aes(fill = Var4)) + 
  geom_stratum() +
  geom_label(stat = "stratum",
             aes(label = after_stat(stratum)),size=2) +
  scale_fill_brewer(palette = "Set2") + 
  theme_bw() + theme(legend.title = element_blank()) +guides(fill = 'none') +
  scale_x_continuous('Model', breaks=1:6,labels=c(paste0('Model k=',2:7)))

#pdf(paste0('Ethiopia Segmentation_v2/LCA/urban_noage/LCA_urban_sankey_noage_v2.pdf'),width=14,height=8)
pdf(paste0('Plots/LCA_urban_noage_plots/9a2_LCA_urban_sankey_noage_v2.pdf'),width=14,height=8)
print(p)
dev.off()

prop.clust <- matrix(NA, nrow=6,ncol=7)
for(i in 1:6){
  x <- as.numeric(t(prop.table(table(dhs.seg_urban[,paste0('cluster_k',(i+1),'_rename')]))))
  add <- rep(NA, 7-length(x))
  prop.clust[i,] <- c(x,add)
}
write.csv(prop.clust,'Plots/LCA_urban_noage_plots/9a2_cluster_proportion_urban_noage.csv',row.names = F)
#write.csv(prop.clust,'Ethiopia Segmentation_v2/LCA/urban_noage/cluster_prop.csv',row.names = F)





### creating a final dataset with vulnerabilities, outcomes, and clusters, that also remerges with IR and HH datasets to have more options for variables ####
       ## also see script 10a2 for examples of variables to create and how to merge the datasets


dhs_rural_noage <- readRDS("Data/Ethiopia_constructed_data/LCA_rural_noage_data/9a2_dhs_rural_noage.rds")
dhs_urban_noage <- readRDS("Data/Ethiopia_constructed_data/LCA_urban_noage_data/9a2_dhs_urban_noage.rds")


# Merge urban and rural
dhs.seg.final <- merge(dhs_rural_noage, dhs_urban_noage, all = TRUE)

# Bring in other variables from the original data frame
IR <- data.table(read.dta("Data/Ethiopia DHS/ET_2016/ETIR71DT/ETIR71FL.DTA")) 
HH <- data.table(read.dta("Data/Ethiopia DHS/ET_2016/ETHR71DT/ETHR71FL.DTA")) 

HH$v001<-HH$hv001
HH$v002<-HH$hv002
IR$wt<-IR$v005/1000000

newmerge <-merge(IR, HH, by=c("v001", "v002"), all.x=T)
dhs.seg.final <- merge(dhs.seg.final, newmerge, by="caseid", all.x=T)

## Create new cluster variable with the final cluster names
dhs.seg.final$cluster <- dhs.seg.final$cluster_k5_rename_short

# Drop old cluster names
drop <- c("cluster_k2", "cluster_k3", "cluster_k4", "cluster_k5", "cluster_k6", "cluster_k7", "cluster_k8", "cluster_k9",             
          "cluster_k10", "cluster_k2_rename", "cluster_k3_rename", "cluster_k4_rename", "cluster_k5_rename", "cluster_k6_rename",
          "cluster_k7_rename", "cluster_k2_rename_short", "cluster_k3_rename_short", "cluster_k4_rename_short",
          "cluster_k6_rename_short", "cluster_k7_rename_short")
dhs.seg.final = dhs.seg.final[, !names(dhs.seg.final) %in% drop]
rm(drop)
  
## Create the survey design objects
dhs.seg.final$v021 <-dhs.seg.final$v021.x
dhs.seg.final$wt <-dhs.seg.final$wt.x
dhs.seg.final$v023 <-dhs.seg.final$v023.x

dhs.seg.final.des <-svydesign(id=~v021, strata=~v023, weights=~wt, data=dhs.seg.final)

saveRDS(dhs.seg.final, file="Data/Ethiopia_constructed_data/9a2_dhs.seg.final.rds")
saveRDS(dhs.seg.final.des, file="Data/Ethiopia_constructed_data/9a2_dhs.seg.final.des.rds")
