rm(list=ls())
pacman::p_load("poLCA", "readxl","tidyr", "dplyr", "mice","networkD3")

################################# RURAL LCA ####################################

# read in vulnerability data, IR data and data on LCA variables 
vulnerability <- readRDS("dataOut/2_MG_DHS_vulnerability.rds")
IR <- readRDS("data/IR_meghalaya.rds")
v025 <- IR %>% dplyr::select(caseid, v025)
vulnerability <- merge(vulnerability, v025, by="caseid")
vulnerability <- vulnerability %>% filter(v025=="rural")
lca_spreadsheet <- read_excel("dataOut/exploratory_summary.xlsx", sheet="Sheet1")

# create data frame for LCA
lca_vars <- lca_spreadsheet %>% filter(rural_lca_keep == 1)
lca_vars <- names(vulnerability)[(names(vulnerability) %in% lca_vars$factor)]

# keep a copy of the original data 
df_orig <- vulnerability %>% dplyr::select(caseid, v025, all_of(lca_vars))
df <- df_orig %>% dplyr::select(-caseid, -v025)

num.vars <- names(which(sapply(df, is.numeric)))
cat.var <- names(which(sapply(df, function(x) is.character(x) || is.factor(x))))

for (i in 1:length(num.vars)){
  df[,num.vars[i]] <- df[,num.vars[i]]+1
  print(table(df[,num.vars[i]]))
}

## setting nominal to be coded as 1,2,...
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
glimpse(df)

colSums(is.na(df))

# impute what we can using random forest to not lose so many observations
# imputed_data <- mice(df, m = 5, method = 'rf', maxit = 50, seed = 500)
# df_imputed <- complete(imputed_data, 1)
# colSums(is.na(df_imputed))

# na_rows <- which(apply(df_imputed, 1, function(row) any(is.na(row))))
# 
# if (length(na_rows) > 0){
#   df_orig <- df_orig[-na_rows,]  
#   df_imputed <- df_imputed %>% drop_na() 
# }

#df <- df_imputed

na_rows <- which(apply(df, 1, function(row) any(is.na(row))))

if (length(na_rows) > 0){
  df_orig <- df_orig[-na_rows,]  
  df <- df %>% drop_na() 
}

set.seed(99)
formula <- as.formula(paste("cbind(",paste(names(df),collapse=","),")~1",sep=""))

### fitting 2-8 classes
for(n in 2:8){
  name <- paste0('LCA', n)
  data <- poLCA(formula, data=df, nclass=n, maxiter = 5000, tol = 1e-7, nrep = 40)
  assign(name, data)
  
}


### BIC Plot
myds <- c(LCA2$bic,
          LCA3$bic,
          LCA4$bic,
          LCA5$bic,
          LCA6$bic,
          LCA7$bic,
          LCA8$bic)
x <- (1:length(myds))

plot(myds, xaxt = "n", type="b",
     xlab="Solutions", ylab="BIC", main="Bayesian Information Criterion (BIC)")
axis(1, at=1:7, labels=c("2-class",
                         "3-class",
                         "4-class",
                         "5-class",
                         "6-class",
                         "7-class",
                         "8-class"))

# check class membership size
prop.table(table(LCA2$predclass))
prop.table(table(LCA3$predclass))
prop.table(table(LCA4$predclass))
prop.table(table(LCA5$predclass))
prop.table(table(LCA6$predclass))
prop.table(table(LCA7$predclass))
prop.table(table(LCA8$predclass))


rural_class <- cbind(df_orig, LCA2$predclass,
                                 LCA3$predclass,
                                 LCA4$predclass,
                                 LCA5$predclass,
                                 LCA6$predclass,
                                 LCA7$predclass,
                                 LCA8$predclass)

rural_class <- rural_class %>% rename(cluster_k2 = `LCA2$predclass`, 
                                      cluster_k3 = `LCA3$predclass`, 
                                      cluster_k4 = `LCA4$predclass`,
                                      cluster_k5 = `LCA5$predclass`, 
                                      cluster_k6 = `LCA6$predclass`, 
                                      cluster_k7 = `LCA7$predclass`, 
                                      cluster_k8 = `LCA8$predclass`)

saveRDS(rural_class, file="segments/rural_meghalaya.rds")


## plot sankey diagram

seg.sankey <- rural_class %>% 
  dplyr::select(cluster_k2, cluster_k3, cluster_k4, cluster_k5, cluster_k6, cluster_k7, cluster_k8) %>% 
  rename(k2 = cluster_k2, k3 = cluster_k3, k4 = cluster_k4, k5 = cluster_k5, k6 = cluster_k6, k7 = cluster_k7, k8 = cluster_k8)

links <- data.frame(source = character(), target = character(), value = character())

for(i in 1:(ncol(seg.sankey) - 1)){
  tmp <- seg.sankey %>%
    group_by(!!sym(names(seg.sankey)[i]), !!sym(names(seg.sankey)[i + 1])) %>%
    summarise(value = n(), .groups = 'drop') 
  
  tmp$source <- paste0(names(seg.sankey)[i], "_", tmp[[1]])
  tmp$target <- paste0(names(seg.sankey)[i+1], "_", tmp[[2]])
  
  links <- rbind(links, tmp[, c("source", "target", "value")])
}

links <- as.data.frame(links)

# Create nodes data frame for Sankey diagram
nodes <- data.frame(name = unique(c(links$source, links$target)))

links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1

# Create Sankey diagram
sankey <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource",
                        Target = "IDtarget", Value = "value", NodeID = "name",
                        units = "TWh", fontSize = 12, nodeWidth = 30)

sankey

# RESULT K = 4 is optimal


################################# URBAN LCA ####################################

rm(list = ls())

# read in vulnerability data, IR data and data on LCA variables 
vulnerability <- readRDS("dataOut/2_MG_DHS_vulnerability.rds")
IR <- readRDS("data/IR_meghalaya.rds")
v025 <- IR %>% dplyr::select(caseid, v025)
vulnerability <- merge(vulnerability, v025, by="caseid")
vulnerability <- vulnerability %>% filter(v025=="urban")
lca_spreadsheet <- read_excel("dataOut/exploratory_summary.xlsx", sheet="Sheet1")

# create data frame for LCA
lca_vars <- lca_spreadsheet %>% filter(urban_lca_keep == 1)
lca_vars <- names(vulnerability)[(names(vulnerability) %in% lca_vars$factor)]

# keep a copy of the original data 
df_orig <- vulnerability %>% dplyr::select(caseid, v025, all_of(lca_vars))
df <- df_orig %>% dplyr::select(-caseid, -v025)

num.vars <- names(which(sapply(df, is.numeric)))
cat.var <- names(which(sapply(df, function(x) is.character(x) || is.factor(x))))

for (i in 1:length(num.vars)){
  df[,num.vars[i]] <- df[,num.vars[i]]+1
  print(table(df[,num.vars[i]]))
}

## setting nominal to be coded as 1,2,...
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
glimpse(df)

colSums(is.na(df))

# impute what we can using random forest to not lose so many observations
# imputed_data <- mice(df, m = 5, method = 'rf', maxit = 50, seed = 500)
# df_imputed <- complete(imputed_data, 1)
# colSums(is.na(df_imputed))
# 
# na_rows <- which(apply(df_imputed, 1, function(row) any(is.na(row))))
# 
# if (length(na_rows) > 0){
#   df_orig <- df_orig[-na_rows,]  
#   df_imputed <- df_imputed %>% drop_na() 
# }
# 
# df <- df_imputed

na_rows <- which(apply(df, 1, function(row) any(is.na(row))))

if (length(na_rows) > 0){
  df_orig <- df_orig[-na_rows,]
  df <- df %>% drop_na()
}

set.seed(99)
formula <- as.formula(paste("cbind(",paste(names(df),collapse=","),")~1",sep=""))

### fitting 2-8 classes
for(n in 2:8){
  name <- paste0('LCA', n)
  data <- poLCA(formula, data=df, nclass=n, maxiter = 5000, tol = 1e-7, nrep = 100)
  assign(name, data)
  
}


### BIC Plot
myds <- c(LCA2$bic,
          LCA3$bic,
          LCA4$bic,
          LCA5$bic,
          LCA6$bic,
          LCA7$bic,
          LCA8$bic)
x <- (1:length(myds))

plot(myds, xaxt = "n", type="b",
     xlab="Solutions", ylab="BIC", main="Bayesian Information Criterion (BIC)")
axis(1, at=1:7, labels=c("2-class",
                         "3-class",
                         "4-class",
                         "5-class",
                         "6-class",
                         "7-class",
                         "8-class"))

# check class membership size
prop.table(table(LCA2$predclass))
prop.table(table(LCA3$predclass))
prop.table(table(LCA4$predclass))
prop.table(table(LCA5$predclass))
prop.table(table(LCA6$predclass))
prop.table(table(LCA7$predclass))
prop.table(table(LCA8$predclass))


urban_class <- cbind(df_orig, LCA2$predclass,
                     LCA3$predclass,
                     LCA4$predclass,
                     LCA5$predclass,
                     LCA6$predclass,
                     LCA7$predclass,
                     LCA8$predclass)

urban_class <- urban_class %>% rename(cluster_k2 = `LCA2$predclass`, 
                                      cluster_k3 = `LCA3$predclass`, 
                                      cluster_k4 = `LCA4$predclass`,
                                      cluster_k5 = `LCA5$predclass`, 
                                      cluster_k6 = `LCA6$predclass`, 
                                      cluster_k7 = `LCA7$predclass`, 
                                      cluster_k8 = `LCA8$predclass`)

saveRDS(urban_class, file="segments/urban_meghalaya.rds")


## plot sankey diagram

seg.sankey <- urban_class %>% 
  dplyr::select(cluster_k2, cluster_k3, cluster_k4, cluster_k5, cluster_k6, cluster_k7, cluster_k8) %>% 
  rename(k2 = cluster_k2, k3 = cluster_k3, k4 = cluster_k4, k5 = cluster_k5, k6 = cluster_k6, k7 = cluster_k7, k8 = cluster_k8)

links <- data.frame(source = character(), target = character(), value = character())

for(i in 1:(ncol(seg.sankey) - 1)){
  tmp <- seg.sankey %>%
    group_by(!!sym(names(seg.sankey)[i]), !!sym(names(seg.sankey)[i + 1])) %>%
    summarise(value = n(), .groups = 'drop') 
  
  tmp$source <- paste0(names(seg.sankey)[i], "_", tmp[[1]])
  tmp$target <- paste0(names(seg.sankey)[i+1], "_", tmp[[2]])
  
  links <- rbind(links, tmp[, c("source", "target", "value")])
}

links <- as.data.frame(links)

# Create nodes data frame for Sankey diagram
nodes <- data.frame(name = unique(c(links$source, links$target)))

links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1

# Create Sankey diagram
sankey <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource",
                        Target = "IDtarget", Value = "value", NodeID = "name",
                        units = "TWh", fontSize = 12, nodeWidth = 30)

sankey

#### 4 or 5 Class solution is optimal
