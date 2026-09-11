############## 
# 9a1_Ethiopia_DHS_LCA
# Purpose: repeated the LCA 500 times to check to see if the model was actually converging for different cluster sizes.  (rural and urban, with and without age)
#Anu Mishra to add in lines that construct dhs_seg_LCA
# Ethiopia DHS 2016, Recode 7
# Using variables selected based on qualitative insights, univariate analyses, and results of pca 
# input data: 5_Ethiopia_DHS_exploratory.rds
# input scripts: 10_Ethiopia_DHS_cluster_plots.R
# output data: mlres.urban, mlres.rural, 9a1_dhs.seg_reform_urban, 9a1_dhs.seg_reform_rural, 
# ancestor code: Code/7_Ethiopia_DHS_construct_mapping_external_final.R
# descendent code: 10_Ethiopia_DHS_cluster_plots.R (note: being currently updated by DR)
# note: each user needs to change the directory 
#note: need to change whether rural =TRUE and add.age=TRUE at the top by hand to get the output datasets
# poLCA only can account for nominal categories, so variables need to be recategorized. All variables must start at 1, not 0 (so binary are coded as 1/2)
# Note: mlres datasets saved here need to be moved by hand to the correct folder 
############## 
############## 


pacman::p_load("poLCA", "tidyverse","readxl")

#library(poLCA)
#library(tidyverse)

### Set working directory - each user needs to change ########
#setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/Ethiopia Segmentation_v2/LCA")
#setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling")
#setwd("~/Downloads/Modeling")

### Read in data - nchange by user ###
dhs.seg <- readRDS("Data/Ethiopia_constructed_data/5_Ethiopia_DHS_exploratory.rds")


### generate formatted data for poLCA package -- user only needs to run once and save dataset for future LCA analysis ## 
## variable categories
var <- read_xlsx("Data/Ethiopia_constructed_data/7_Construct_mapping_external_final.xlsx",sheet = 'final_var_list')
var <- var[!duplicated(var$f.var.name),]

ord.var <- var$f.var.name[var$variable_type=='ordinal']
con.var <- c(var$f.var.name[var$variable_type=='continuous'],'conflict_incidents') #conflict_incidents no longer used here, see conflict_incidents_26plus
cat.var <- var$f.var.name[var$variable_type=='categorical']
bin.var <- c(var$f.var.name[var$variable_type=='binary'])
bin.var <- bin.var[bin.var!='anypastoral.yn']

dhs.seg$highestyearsedinHH.7plus <- ifelse(dhs.seg$highestyearsedinHH.yrs>=7,1,0) 
  #redoing for LCA, highestyearsedinHH.7plus is categorical in the vulnerability script. binary so will be used in loops below 
#dhs.seg$conflict_incidents_26plus <- ifelse(dhs.seg$conflict_incidents>=26,1,0)



## For ordinal re-categorize to start at 1
for(i in 1:length(ord.var)){
  print(table(dhs.seg[,ord.var[i]]))
  dhs.seg[,ord.var[i]] <- dhs.seg[,ord.var[i]] + 1
}

## setting binary to be 1/2
for(i in 1:length(bin.var)){
  dhs.seg[,bin.var[i]] <- ifelse(dhs.seg[,bin.var[i]]==0,1,2)
  print(table(dhs.seg[,bin.var[i]]))
}

## setting nominal to be coded as 1,2,...
for(i in 1:length(cat.var)){
  var.class <- class(dhs.seg[,cat.var[i]])
  if(var.class=='factor'){
    dhs.seg[,cat.var[i]] <- as.numeric(dhs.seg[,cat.var[i]])
  }
  if(var.class=='character'){
    dhs.seg[,cat.var[i]] <- as.numeric(as.factor(dhs.seg[,cat.var[i]]))
  }
  print(table( dhs.seg[,cat.var[i]]))
}


# Cutting continuous variables and other recategorization
#dhs.seg$slum1[dhs.seg$slum1=='urban non-slum'] <- 1 
#dhs.seg$slum1[dhs.seg$slum1=='urban slum'] <- 2
#dhs.seg$slum1[dhs.seg$slum1=='rural'] <- 0
#dhs.seg$highestyearsedinHH.yrs <- ifelse(dhs.seg$highestyearsedinHH.yrs>=7,'7+','0-7')

dhs_seg_lca <- dhs.seg %>% select(highestyearsedinHH.7plus,num.kids.house.4plus,num.15up.4plus,conflict_incidents_26plus,med.index,
                              wealth.index.ur.ord,dv.index,working.yn,partner.working.yn,hh.electricity,slum1,
                              med.permis,water,any.media.yn,anyed.yn,early.sex.15,work.seasonal,jdwd.index.cat,
                              pship.cat,religionrecode.cat,pastoralnoagro.yn,hh.urban,caseid) %>% drop_na() #note inclusion of hh.urban here



comple.case.urban <- dhs.seg %>% filter(hh.urban==1) %>% select(highestyearsedinHH.7plus,num.kids.house.4plus,num.15up.4plus,conflict_incidents_26plus,med.index,
                                                                wealth.index.ur.ord,dv.index,working.yn,partner.working.yn,hh.electricity,
                                                                med.permis,water,any.media.yn,anyed.yn,early.sex.15,work.seasonal,jdwd.index.cat,
                                                                pship.cat,religionrecode.cat,caseid) %>% drop_na()

comple.case.rural <- dhs.seg %>% filter(hh.urban==0) %>% select(highestyearsedinHH.7plus,num.kids.house.4plus,num.15up.4plus,conflict_incidents_26plus,med.index,
                                                                wealth.index.ur.ord,dv.index,working.yn,partner.working.yn,hh.electricity,
                                                                med.permis,water,any.media.yn,anyed.yn,early.sex.15,work.seasonal,jdwd.index.cat,
                                                                pship.cat,religionrecode.cat,pastoralnoagro.yn,caseid) %>% drop_na()

dhs.seg_reform_urban <- dhs.seg[dhs.seg$caseid %in% comple.case.urban$caseid,] 
  #using caseid to link back to the dhs.seg dataset with all the columns, but now with reformatted variables for poLCA
dhs.seg_reform_rural <- dhs.seg[dhs.seg$caseid %in% comple.case.rural$caseid,]

  #For use in 9a2 and 10a2
saveRDS(dhs.seg_reform_urban, file="Data/Ethiopia_constructed_data/LCA_urban_noage_data/9a1_dhs.seg_reform_urban.rds")
saveRDS(dhs.seg_reform_rural, file="Data/Ethiopia_constructed_data/LCA_rural_noage_data/9a1_dhs.seg_reform_rural.rds")

#dhs_seg_lca <- readRDS("Data/Ethiopia_constructed_data/dhs_seg_LCA.rds")


### Set parameters to run LCA 500 times to assess convergence of models by checking frequencey of likelihoods of final model
  #Need to manually modify rural and add.age and run through this multiple times.
rural=TRUE
add.age=FALSE
file.out <- paste0('mlres_',ifelse(rural,'rural','urban'),ifelse(add.age,'_wage.rds','.rds'))
nrep <- 500 #can set to fewer to take less time


### Subset to appropriate variables  ###
if(rural==FALSE){
  dhs_seg_lca <- dhs_seg_lca[dhs_seg_lca$hh.urban==1,!names(dhs_seg_lca) %in% 'pastoralnoagro.yn']
  if(add.age==FALSE){
    dhs_seg_lca <- dhs_seg_lca[,!names(dhs_seg_lca) %in% 'age.adj']
    f <- cbind(highestyearsedinHH.7plus,num.kids.house.4plus,num.15up.4plus,conflict_incidents_26plus,
               med.index,wealth.index.ur.ord,dv.index,
               working.yn,partner.working.yn,hh.electricity,      
               med.permis,
               water,any.media.yn,anyed.yn,early.sex.15,        
               work.seasonal,jdwd.index.cat,pship.cat,religionrecode.cat)~1 #~1 indicates no covariates in model
    }
  else if(add.age==TRUE){
    f <- cbind(highestyearsedinHH.7plus,num.kids.house.4plus,num.15up.4plus,conflict_incidents_26plus,
               med.index,wealth.index.ur.ord,dv.index,
               working.yn,partner.working.yn,hh.electricity,      
               med.permis,
               water,any.media.yn,anyed.yn,early.sex.15,        
               work.seasonal,jdwd.index.cat,pship.cat,religionrecode.cat,age.adj)~1
    }
  
} else if(rural==TRUE){
  dhs_seg_lca <- dhs_seg_lca[dhs_seg_lca$hh.urban==0,!names(dhs_seg_lca) %in% 'slum1']
  if(add.age==FALSE){
    dhs_seg_lca <- dhs_seg_lca[,!names(dhs_seg_lca) %in% 'age.adj']
    f <- cbind(highestyearsedinHH.7plus,num.kids.house.4plus,num.15up.4plus,conflict_incidents_26plus,
               med.index,wealth.index.ur.ord,dv.index,
               working.yn,partner.working.yn,hh.electricity,      
               med.permis,
               water,any.media.yn,anyed.yn,early.sex.15,        
               work.seasonal,jdwd.index.cat,pship.cat,religionrecode.cat,
               pastoralnoagro.yn)~1
  }
  else if(add.age==TRUE){
    f <- cbind(highestyearsedinHH.7plus,num.kids.house.4plus,num.15up.4plus,conflict_incidents_26plus,
               med.index,wealth.index.ur.ord,dv.index,
               working.yn,partner.working.yn,hh.electricity,      
               med.permis,
               water,any.media.yn,anyed.yn,early.sex.15,        
               work.seasonal,jdwd.index.cat,pship.cat,religionrecode.cat,
               pastoralnoagro.yn,age.adj)~1
  }
}



dhs_seg_lca <- dhs_seg_lca[complete.cases(dhs_seg_lca),]

#run LCA 500 times (NOTE: This is portion is recommended to run on a VM or cluster)
mlres.list <- list()
for(k in 2:10){
  mlmat  <- rep(NA,nrep)                     #matrix of log-liklihood and proportion of each class to see how stable it is (if global maxima exists)
  for (i in 1:nrep) {
    k_res <- poLCA(f,dhs.seg,nclass=k,maxiter=10000,tol=1e-7)
    mlmat[i] <- round(k_res$llik,2)
  }
  mlres.list[[(k-1)]] <- as.matrix(table(mlmat))
}

saveRDS(object = mlres.list,file.out) #Need to move this by hand into the Data/Ethiopia_constructed_data/LCA_urban_noage_data or other appropriate folder
