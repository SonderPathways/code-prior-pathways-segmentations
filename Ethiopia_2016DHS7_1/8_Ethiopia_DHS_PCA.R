############## 
# Ethiopia_DHS_PCA
# Principal component analysis of selected vulnerabilities
# Ethiopia DHS 2016, Recode 7
# Prepared by: Anu Mishra  1/3/23    
# input data: 7_Construct mapping.xlsx, 5_Ethiopia_DHS_exploratory.des.rds
# output data: none saved, see plots in the R markdown script
# ancestor script: 7_Ethiopia_DHS_construct_mapping
# descendent script: 9b
#note: working directory needs to be changed for each user
# note: R markdown version is updated from this - it repeats all the domains in the script. Here you would need to rerun by replacing d1 with each domain 
############## 

rm(list=ls())

pacman::p_load(readxl, tidyverse, survey, ggrepel)

#library(readxl)
#library(tidyverse)
#library(survey)
#library(ggrepel)

## set working directory - needs to be changed for each user
setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/")

#### Input data ##
## Read in construct mapping
const <- as.data.frame(read_xlsx("Data/Ethiopia_constructed_data/7_Construct mapping.xlsx"))
const <- const[order(const$category),]
dhs.seg <- readRDS("Data/Ethiopia_constructed_data/5_Ethiopia_DHS_exploratory.rds")
dhs.seg.des <- readRDS("Data/Ethiopia_constructed_data/5_Ethiopia_DHS_exploratory.des.rds")

f_biplot <- function(load,pc){
  plot_dat <- as.data.frame(load[,pc])
  plot_dat$labels <- row.names(plot_dat)
  plot_dat$start_x <- 0
  plot_dat$stary_y <- 0
  max_lim <- ceiling(max(abs(plot_dat[,1:2])))
  
  p <- ggplot(plot_dat) + 
    geom_segment(aes(x=plot_dat$start_x,y=plot_dat$stary_y,xend=plot_dat[,1],yend=plot_dat[,2]),arrow = arrow(length=unit(0.2,'cm'))) +
    coord_cartesian(xlim = c(-max_lim,max_lim),ylim=c(-max_lim,max_lim)) + xlab(names(plot_dat)[1]) + ylab(names(plot_dat)[2]) +
    theme_bw() +
    geom_text_repel(mapping = aes(label=plot_dat$labels,x=plot_dat[,1],y=plot_dat[,2]))
  plot(p)
}

f_screeplot <- function(pca_res){
  prop <- (pca_res$sdev)^2/sum((pca_res$sdev)^2)
  cum_prop <- cumsum(prop)
  cum_prop_label <- paste0(round(100*cum_prop,2),'%')
  plot_dat <- data.frame(PC=paste0('PC',1:length(cum_prop)), sd=pca_res$sdev,prop=prop,cum_prop=cum_prop,label=cum_prop_label)

  p <- ggplot(plot_dat,aes(x=reorder(PC,cum_prop),y=sd)) + geom_bar(stat="identity") + theme_bw() +
       xlab('PC') + geom_text(aes(label = label),vjust=-1)
  plot(p)
}

f_loadings <- function(pca_res){
  plot_dat <- as.data.frame(pca_res$rotation^2)
  plot_dat$vul <- row.names(plot_dat)
  plot_dat <- plot_dat %>% gather(PC, wt, -vul)
  plot_dat$wt <- round(100*plot_dat$wt,2)
  plot_dat$order <- as.numeric(sub('.*PC', '', plot_dat$PC))

  
  p <- ggplot(data=plot_dat,aes(fill=vul, y=wt, x=reorder(PC,order))) + geom_bar(position="stack", stat="identity") +
       ylab('Contribution Wt') + xlab('PC')

  plot(p)
}


###### Notes: PCA analysis starts here  ####
#### For each domain change the 'sub.category' argument below, which will perform the PCA within each of the domains
#### Step 1: Check that all continuous/interval variables are coded as 'integer' or 'numeric' in R
#### Step 2: Fit the PCA and look at the screeplot to tell us how many PCs capture a good amount of variable 
#### Step 3: Using the information from Step 2 look at the the PC loadings. Variables that 'hang together' where we might be
####         able to drop, are those vectors that are pretty much on-top of each other (i.e., like having perfect positive correlation)
###          or those vectors that are pointing in opposite directions (i.e., like having perfect negative correlations)
###          Variables that form right angles are pretty much uncorrelated, definitely worth keeping both
###          NOTE!! Because we are treating categorical variables as a series of dummy variables, we should look to see
###           if any one of the three categories provides some information beyond they other variables (and not look at how the
###            those categories relate to each other). If one 'level' of the cateogry explains something the others don't in at
###            least on PC dimension then I think that's evidence to include it 


## USER INPUT: change domain and re-run from this point each time
domain.var <- const[const$pca=='pca1' & !is.na(const$pca),'f.var.name']
domain.var <- ifelse(domain.var=='motortransport.yn','hh.motortransport.yn',domain.var)

## USER CHECK: check coding of all variables 'character' ok for binary/categorical but for numeric/continuous/ordinal should change to numeric/integer
lapply(dhs.seg[,domain.var],class)  
# if you need to change to numeric copy code below with desirable name
# dhs.seg.des$variables$num.children <- as.numeric(dhs.seg.des$variables$num.children)

#perform weighted PCA where categorical variable is treated as series of dummies
d1 <- svyprcomp(as.formula(paste0('~',paste(domain.var,collapse = '+'))),design = dhs.seg.des,center = TRUE,scale. = TRUE,scores = FALSE)
summary(d1)   #proportion of total variable explained by each component 

### Look at screeplot to determine how many PC's capture the variation in the data (rule of thumb around 60%)
f_screeplot(d1)

### Look at PC factor loadings (can look at any two PC combinations)
d1$rotation
f_loadings(pca_res = d1)

#biplots
f_biplot(d1$rotation,pc=c(1,2))   #PCA 1/2
f_biplot(d1$rotation,pc=c(2,3))   #PCA 2/3
f_biplot(d1$rotation,pc=c(1,3))   #PCA 1/3


### Could add code to save pdfs - see R markdown script

### 


rm(domain.var,d1) ## removing so don't accidentally keep looking at same domains 
