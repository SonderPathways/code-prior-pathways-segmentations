#CART:(Classification And Regression Tree)
rm(list=ls())

pacman::p_load(rpart, rpart.plot, caret, randomForest, dplyr, readxl, tidyverse)

setwd("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2020 DHS/") 
load("Data/IR.RData")
v025 <- IR %>% dplyr::select(caseid, v025, v001)
##read in seg class solutions 
rural_LCA5 <- readRDS("India_constructed_data/rural_LCA5.rds")
urban_LCA5 <- readRDS("India_constructed_data/urban_LCA5.rds")

##################################################################### RURAL CART 
vulnerability <- readRDS("India_constructed_data/2_UP_DHS_vulpredictors.RData")
#merge v025 with vul files 
vulnerability <- merge(vulnerability, v025, by="caseid")
# vulnerability <- vulnerability %>% filter(v025=="urban")
vulnerability <- vulnerability %>% filter(v025=="rural")
lca_vars <- read_excel("India_constructed_data/UP_DHS_LCAplusPCAresults.xlsx", sheet="map")
# 
# lca_vars <- lca_vars %>% filter(is.na(urban_univarite_drop) & is.na(urban_PCA_drop) & is.na(urban_LCA))
# lca_vars <- names(vulnerability)[(names(vulnerability) %in% lca_vars$urban_factor)]
lca_vars <- lca_vars %>% filter(is.na(rural_univarite_drop) & is.na(rural_PCA_drop) & is.na(rural_LCA))
lca_vars <- names(vulnerability)[(names(vulnerability) %in% lca_vars$rural_factor)]

lca_vars

df <- vulnerability %>% dplyr::select(all_of(lca_vars))
colSums(is.na(df))
df <- df %>% drop_na() 
##merge with class solutions
rural.class <- cbind(df, LCA5_class=rural_LCA5$predclass)
colnames(rural.class)
rural.class$LCA5_class <- as.factor(rural.class$LCA5_class)

# Set the seed
set.seed(123)

fit.tree <- rpart(LCA5_class ~ . , data=rural.class, control=rpart.control(cp=0.001, xval=5), method="class")
fit.tree

# Visualizing the unpruned tree
rpart.plot(fit.tree)
# Checking the order of variable importance
var.importance <- fit.tree$variable.importance
var.importance <- as.data.frame(var.importance)
## visualize cross-validation results
plotcp(fit.tree)
printcp(fit.tree)
# summary(fit.tree)
plot(fit.tree, uniform=TRUE, main="Classification Tree for UP rural LCA")
text(fit.tree, use.n=TRUE, all=TRUE, cex=.7)
prp(fit.tree, type=5, varlen=0, cex=1)
#check sensitivity and specificity
PredictCART_up_rural <- predict(fit.tree, data=rural.class, type="class")
confusionMatrix(data=PredictCART_up_rural, rural.class$LCA5_class)

#prune tree
tree_up_rural_prune <- prune(fit.tree, cp=fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"])
plot(tree_up_rural_prune, uniform=TRUE, main="Classification Tree for UP rural LCA")
# Can try prunign using different error cutoffs
#A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.
# For regression models (see next section) the scaled cp has a very direct interpretation: 
# if any split does not increase the overall R2 of the model by at least cp 
# (where R2 is the usual linear-models definition) then that split is decreed to be, 
# a priori, not worth pursuing. The program does not split said branch any further, 
# and saves considerable computational effort.
tree_up_rural_prune <- prune(fit.tree, cp=.0017)
plot(tree_up_rural_prune, uniform=TRUE, main="Classification Tree for UP rural LCA")
text(tree_up_rural_prune, use.n=TRUE, all=TRUE, cex=.7)
prp(tree_up_rural_prune, type=5, varlen=0, cex=1)
#vars in the prune tree
#check sensitivity and specificity
PredictCART_up_rural <- predict(tree_up_rural_prune, data=rural.class, type="class")
confusionMatrix(data=PredictCART_up_rural, rural.class$LCA5_class)
#heard.hiv
#ed.level
#hh.refrig
#work.12mo
#hh.internet
#edu.hh.highest
#work.current
#hh.WASH.toilet
#hh.tv
#floor.slum 
#her.use.internet
#say.no.sex.sti

############################# remove the "say.no.sex.sti" item -- too sensitive
rural.class <- rural.class %>% select(!say.no.sex.sti)
rural.class <- rural.class %>% select(!say.no.sex.otherwomen)
# Set the seed
set.seed(123)

fit.tree <- rpart(LCA5_class ~ . , data=rural.class, control=rpart.control(cp=0.001, xval=5), method="class")
fit.tree

# Visualizing the unpruned tree
rpart.plot(fit.tree)
# Checking the order of variable importance
var.importance <- fit.tree$variable.importance
var.importance <- as.data.frame(var.importance)
## visualize cross-validation results
plotcp(fit.tree)
printcp(fit.tree)
# summary(fit.tree)
plot(fit.tree, uniform=TRUE, main="Classification Tree for UP rural LCA")
text(fit.tree, use.n=TRUE, all=TRUE, cex=.7)
prp(fit.tree, type=5, varlen=0, cex=1)
#check sensitivity and specificity
PredictCART_up_rural <- predict(fit.tree, data=rural.class, type="class")
confusionMatrix(data=PredictCART_up_rural, rural.class$LCA5_class)

#prune tree
tree_up_rural_prune <- prune(fit.tree, cp=fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"])
plot(tree_up_rural_prune, uniform=TRUE, main="Classification Tree for UP rural LCA")
# Can try prunign using different error cutoffs
#A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.
# For regression models (see next section) the scaled cp has a very direct interpretation: 
# if any split does not increase the overall R2 of the model by at least cp 
# (where R2 is the usual linear-models definition) then that split is decreed to be, 
# a priori, not worth pursuing. The program does not split said branch any further, 
# and saves considerable computational effort.
tree_up_rural_prune <- prune(fit.tree, cp=.0017)
plot(tree_up_rural_prune, uniform=TRUE, main="Classification Tree for UP rural LCA")
text(tree_up_rural_prune, use.n=TRUE, all=TRUE, cex=.7)
prp(tree_up_rural_prune, type=5, varlen=0, cex=1)
#vars in the prune tree
#check sensitivity and specificity
PredictCART_up_rural <- predict(tree_up_rural_prune, data=rural.class, type="class")
confusionMatrix(data=PredictCART_up_rural, rural.class$LCA5_class)

##################################################################### URBAN CART 
vulnerability <- readRDS("India_constructed_data/2_UP_DHS_vulpredictors.RData")
#merge v025 with vul files 
vulnerability <- merge(vulnerability, v025, by="caseid")
vulnerability <- vulnerability %>% filter(v025=="urban")
# vulnerability <- vulnerability %>% filter(v025=="rural")
lca_vars <- read_excel("India_constructed_data/UP_DHS_LCAplusPCAresults.xlsx", sheet="map")

lca_vars <- lca_vars %>% filter(is.na(urban_univarite_drop) & is.na(urban_PCA_drop) & is.na(urban_LCA))
lca_vars <- names(vulnerability)[(names(vulnerability) %in% lca_vars$urban_factor)]
# lca_vars <- lca_vars %>% filter(is.na(rural_univarite_drop) & is.na(rural_PCA_drop) & is.na(rural_LCA))
# lca_vars <- names(vulnerability)[(names(vulnerability) %in% lca_vars$rural_factor)]

lca_vars

df <- vulnerability %>% dplyr::select(all_of(lca_vars))
colSums(is.na(df))
df <- df %>% drop_na() 
##merge with class solutions
urban.class <- cbind(df, LCA5_class=urban_LCA5$predclass)
colnames(urban.class)
urban.class$LCA5_class <- as.factor(urban.class$LCA5_class)

# Set the seed
set.seed(123)

fit.tree <- rpart(LCA5_class ~ . , data=urban.class, control=rpart.control(cp=0.001, xval=5), method="class")
fit.tree

# Visualizing the unpruned tree
rpart.plot(fit.tree)
# Checking the order of variable importance
var.importance <- fit.tree$variable.importance
var.importance <- as.data.frame(var.importance)
## visualize cross-validation results
plotcp(fit.tree)
printcp(fit.tree)
# summary(fit.tree)
plot(fit.tree, uniform=TRUE, main="Classification Tree for UP urban LCA")
text(fit.tree, use.n=TRUE, all=TRUE, cex=.7)
prp(fit.tree, type=5, varlen=0, cex=1)
#check sensitivity and specificity
PredictCART_up_rural <- predict(fit.tree, data=urban.class, type="class")
confusionMatrix(data=PredictCART_up_rural, urban.class$LCA5_class)

#prune tree
tree_up_urban_prune <- prune(fit.tree, cp=fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"])
plot(tree_up_urban_prune, uniform=TRUE, main="Classification Tree for UP urban LCA")
# Can try prunign using different error cutoffs
#A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.
# For regression models (see next section) the scaled cp has a very direct interpretation: 
# if any split does not increase the overall R2 of the model by at least cp 
# (where R2 is the usual linear-models definition) then that split is decreed to be, 
# a priori, not worth pursuing. The program does not split said branch any further, 
# and saves considerable computational effort.
tree_up_urban_prune <- prune(fit.tree, cp=.0042)
plot(tree_up_urban_prune, uniform=TRUE, main="Classification Tree for UP urban LCA")
text(tree_up_urban_prune, use.n=TRUE, all=TRUE, cex=.7)
prp(tree_up_urban_prune, type=5, varlen=0, cex=1)
#vars in the prune tree
#check sensitivity and specificity
PredictCART_up_rural <- predict(tree_up_urban_prune, data=urban.class, type="class")
confusionMatrix(data=PredictCART_up_rural, urban.class$LCA5_class)
#work.12mo
#heard.hiv
#num.under5.cat
#ed.level
#edu.hh.highest
#hh.motor
#hh.ac
#hh.bedrooms

