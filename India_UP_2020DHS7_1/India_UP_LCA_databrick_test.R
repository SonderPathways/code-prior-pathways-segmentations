# rm(list=ls())
pacman::p_load("poLCA", "dplyr", "tidyverse", "tidyLPA", "networkD3", "data.table", "ggpubr", "forcats", "readxl")

# load(file = "India_UP_rural_databrick.Rdata")
load(file = "/Volumes/idm_general/pathways/pathways-segmentation-projects/india_up/India_UP_rural_databrick.Rdata")

# file path
path = "/Volumes/idm_general/pathways/pathways-segmentation-projects/india_up/"

##tranforming variables
cat.var <- names(which(sapply(df, is.character)))
num.vars <- names(which(sapply(df, is.numeric)))

for (i in 1:length(num.vars)){
  df[,num.vars[i]] <- df[,num.vars[i]]+1
  print(table(df[,num.vars[i]]))
}

for(i in 1:length(cat.var)){
  var.class <- class(df[,cat.var[i]])
  if(var.class=='factor'){
    df[,cat.var[i]] <- as.numeric(df[,cat.var[i]])
  }
  if(var.class=='character'){
    df[,cat.var[i]] <- as.numeric(as.factor(df[,cat.var[i]]))
  }
  print(table(df[,cat.var[i]]))
}
colnames(df)

### LCA ANALYSIS 
# set.seed(1234)
set.seed(99)
formula <- as.formula(paste("cbind(",paste(names(df),collapse=","),")~1",sep=""))

### fitting 2-7 classes 
for(n in 12:12){
  name <- paste0('LCA', n)
  data <- poLCA(formula, data=df, nclass=n, maxiter=500, nrep=1000)
  assign(name, data)

  save_path = paste0(path, "rural_", name, ".rds")
  # dir.create(dirname(path), showWarnings = F, recursive = T)
  saveRDS(data, save_path)
}

# saveRDS(LCA2,  file = paste0(path, "rural_LCA2.rds"))
# saveRDS(LCA3,  file = paste0(path, "rural_LCA3.rds"))
# saveRDS(LCA4,  file = paste0(path, "rural_LCA4.rds"))
# saveRDS(LCA5,  file = paste0(path, "rural_LCA5.rds"))
# saveRDS(LCA6,  file = paste0(path, "rural_LCA6.rds"))
# saveRDS(LCA7,  file = paste0(path, "rural_LCA7.rds"))
# saveRDS(LCA8,  file = paste0(path, "rural_LCA8.rds"))
# saveRDS(LCA9,  file = paste0(path, "rural_LCA9.rds"))
# saveRDS(LCA10, file = paste0(path, "rural_LCA10.rds"))
# saveRDS(LCA11, file = paste0(path, "rural_LCA11.rds"))
# saveRDS(LCA12, file = paste0(path, "rural_LCA12.rds"))


