### 7_Ethiopia_DHS_construct_mapping                                ####
### Project: Ethiopia Pathways Analysis                             ####
### Prepared by: Anu  1/3/23                                        ####
### Inputs: Constructs and measures from Pathways qual work.xlsx   ######
### Inputs: Exploratory_analysis_external.xlsx                      ##### 
### AG note: prior version Exploratory_analysis_2.3.2023.xlsx      ######
### Output: 7_Construct_mapping.xlsx                                #####
### Ancestor script: none                                           ##### 
### Descendent script: 9b_Ethiopia_DHS_LCA.R                         ######
### Purpose: Map vulnerability code names to the different constructed domains ###
### Note: need to set working directory

rm(list=ls())
pacman::p_load("readxl", "writexl", "zoo", "tidyverse")

### Set working directory - need to change for each person 
setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/")

### Read and clean construct data ##
#const <- read_xlsx("Vulnerability measures data mapping/Constructs and measures from Pathways qual work.xlsx",sheet = 'Integrated measures')
const <- read_xlsx("Constructs and measures from Pathways qual work.xlsx",sheet = 'Integrated measures')

const <- const[2:nrow(const),names(const) %in% c('Category','Sub-category','Factor/Construct','Variable names')]
names(const) <- c('category','sub.category','construct','var.name')

## carry construct name forward
const$construct <- na.locf(const$construct)
const$category <- na.locf(const$category)
const$sub.category <- na.locf(const$sub.category)

## remove rows without variable name and seperate into seperate columns
const.complt <- const[!is.na(const$var.name),]
const.complt <-  separate(const.complt,var.name, paste("var.name",1:50,sep="."), ",", extra = "merge")   #pick arbitrarily large to make sure all variable names are include
apply(const.complt,2,FUN = function(x){sum(is.na(x))==length(x)})   ## can drop variable 26 onwards 
const.complt <- const.complt[,1:28]

## reshape to long dataset to have one variable per row
const.long <- const.complt %>% gather(var, var.name, var.name.1:var.name.25)
const.long <- const.long[!is.na(const.long$var.name),!names(const.long) %in% 'var']
const.long$var.name <- gsub(' ',replacement = '',const.long$var.name)

## check that vulnerabilities within multiple constructs are repeated
length(const.long$var.name)
length(unique(const.long$var.name))
tmp <- (const.long[duplicated(const.long$var.name) | duplicated(const.long$var.name,fromLast = TRUE),])
View(tmp[order(tmp$var.name),])


### Read in univariate analysis file for variables used
univ <- read_xlsx("Exploratory_analysis_external.xlsx",sheet='univariate_results_decisions') #was Univariate analyses in prior version
f.var <- univ[,1:3]
names(f.var) <- c('var.name','var.type','f.var.name')
f.var <- f.var[!grepl("///|//",f.var$f.var.name) & !is.na(f.var$f.var.name),]

### merge with construct mapping
c_map <- left_join(f.var,const.long,by='var.name')
length(unique(c_map$var.name))  ## extra rows are from repeating different constructs
length(c_map$var.name)
c_map <- c_map[order(c_map$category),]

write_xlsx(x = c_map,path = "Data/Ethiopia_constructed_data/7_Construct_mapping_external.xlsx") #our final version with decision notes in it has _final
