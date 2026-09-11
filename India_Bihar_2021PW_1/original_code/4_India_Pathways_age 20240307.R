#### Setup ####
## Load packages
rm(list=ls())
pacman::p_load("greybox", "readxl", "plyr")
## Set the working directory
setwd("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2021 Pathways/Data/Bihar_constructed_data")
## Import the recode study measures
vul <- readRDS("1_Bihar_Pathways_vulpredictors.rds")
varlist <- names(vul)
varlist <- varlist[5:75]

vulnerability.list <- names(vul)[!names(vul) %in% c("caseid", "psu", "weight", "her.age")]
corr_analysis <- as.data.frame(matrix(nrow = length(vulnerability.list)))
corr_analysis$factor <- vulnerability.list
corr_analysis <- corr_analysis %>% select(factor, everything())

for (v in varlist) {
  print(v)
  # Cramer’s V measures the relation between two variables in categorical scale. 
  res <- cramer(vul$her.age, vul[, v])  
  corr_analysis$V1[corr_analysis$factor==v] <- res$value
  print(res) 
}

colnames(corr_analysis) <- c('factor','corr.age') 

uni_noage <- read_excel("5_assoc_summary_simple_noage.adj.xlsx")
uni_noage <- uni_noage %>% select(factor, pct_sig)
colnames(uni_noage) <- c('factor','pct_sig_noage') 

uni_age <- read_excel("5_assoc_summary_simple_age.adj.xlsx")

uni <- join(uni_age, uni_noage)
colnames(uni)[28] <- "pct_sig_age"
uni_corr <- join(uni, corr_analysis)
write_xlsx(uni_corr, "univariate_age_correlation.xlsx")








