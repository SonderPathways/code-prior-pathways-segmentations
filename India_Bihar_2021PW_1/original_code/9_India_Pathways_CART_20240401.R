#CART:(Classification And Regression Tree)
rm(list=ls())

pacman::p_load(rpart, rpart.plot, caret, randomForest, dplyr)

setwd("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2021 Pathways/Data/") 


load('Bihar_constructed_data/vuloutcomes.RData')
colnames(vuloutcomes)
#select only LCA predictors and the cluster solution 
vul.class <- vuloutcomes %>% select(his.age:kitchen.vent, LCA5_class)
#need to make the class variable a factor 
class(vul.class$LCA5_class)
vul.class$LCA5_class <- as.factor(vul.class$LCA5_class)

# Set the seed
set.seed(123)

fit.tree <- rpart(LCA5_class ~ . , data=vul.class, control=rpart.control(cp=0.001, xval=5), method="class")
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

#prune tree
tree_bihar_prune<-prune(fit.tree, cp=fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"])
plot(tree_bihar_prune, uniform=TRUE, main="Classification Tree for Bihar LCA")
# Can try prunign using different error cutoffs
#A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.
# For regression models (see next section) the scaled cp has a very direct interpretation: 
# if any split does not increase the overall R2 of the model by at least cp 
# (where R2 is the usual linear-models definition) then that split is decreed to be, 
# a priori, not worth pursuing. The program does not split said branch any further, 
# and saves considerable computational effort.
tree_bihar_prune<-prune(fit.tree, cp=.0033)
# tree_bihar_prune<-prune(fit.tree, cp=.0025)
# tree_bihar_prune<-prune(fit.tree, cp=.0023)
# tree_bihar_prune<-prune(fit.tree, cp=.0022)
# tree_bihar_prune<-prune(fit.tree, cp=.0021)
# tree_bihar_prune<-prune(fit.tree, cp=.0020)
# tree_bihar_prune<-prune(fit.tree, cp=.0001)
plot(tree_bihar_prune, uniform=TRUE, main="Classification Tree for Bihar LCA")
text(tree_bihar_prune, use.n=TRUE, all=TRUE, cex=.7)
prp(tree_bihar_prune, type=5, varlen=0, cex=1)
#vars in the prune tree

#age.1stsex.her
#age.1stpreg.her
#her.edulevel 
#his.edulevel
#cohab.adult.sum 
#hh.size
#female.help
#house.stress
#house.asset
#media 
#smart.cell.hh

PredictCART_bihar <- predict(tree_bihar_prune, data=vul.class, type="class")
confusionMatrix(data=PredictCART_bihar, vul.class$LCA5_class)



############################################ REDUCING HOUSE.ASSET: RANDOM FOREST 

# #Random Forest to find out which individual components of house.asset drive the findings  
library(randomForest)
library(randomForestExplainer)

#get the dataset 
house.asset <- readRDS('Bihar_constructed_data/house.asset.rds')
outcomes <- readRDS('Bihar_constructed_data/1_Bihar_Pathways_outcomes.rds')
caseid <- outcomes %>% select(caseid)
house.asset.caseid <- cbind(house.asset, caseid)
asset.caseid <- subset(house.asset.caseid, select = -c(house.asset, smartcell.hh, transport.hh))
#combine asset item dataset with vul_class; then drop caseid and house.asset variables 
vul.class.caseid <- vuloutcomes %>% select(caseid, his.age:kitchen.vent, LCA5_class)
vul.class.asset.caseid <- merge (vul.class.caseid, asset.caseid, by="caseid")
vul.class.asset <- subset(vul.class.asset.caseid, select = -c(house.asset, caseid, caseid.1))
colnames(vul.class.asset)

vul.class.asset$LCA5_class <- as.factor(vul.class.asset$LCA5_class)


bihar_fit <- randomForest(LCA5_class ~ ., data=vul.class.asset, localImp=T)
print(bihar_fit) # view results
importance <- measure_importance(bihar_fit) # importance of each predictor
plot_min_depth_distribution(bihar_fit, mean_sample = "relevant_trees", k=15)
plot_multi_way_importance(importance, size_measure = "no_of_nodes")

#floor
#wash.toilet 


######################################### REDUCING HOUSE.ASSET: CART TYPING TOOL

# Set the seed
set.seed(123)

fit.tree <- rpart(LCA5_class ~ . , data=vul.class.asset, control=rpart.control(cp=0.001, xval=5), method="class")
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

#prune tree
tree_bihar_prune <- prune(fit.tree, cp=fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"])
plot(tree_bihar_prune, uniform=TRUE, main="Classification Tree for Bihar LCA")
# Can try prunign using different error cutoffs
#A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.
# For regression models (see next section) the scaled cp has a very direct interpretation: 
# if any split does not increase the overall R2 of the model by at least cp 
# (where R2 is the usual linear-models definition) then that split is decreed to be, 
# a priori, not worth pursuing. The program does not split said branch any further, 
# and saves considerable computational effort.
tree_bihar_prune <- prune(fit.tree, cp=0.0031)
plot(tree_bihar_prune, uniform=TRUE, main="Classification Tree for Bihar LCA")
text(tree_bihar_prune, use.n=TRUE, all=TRUE, cex=.7)
prp(tree_bihar_prune, type=5, varlen=0, cex=1)
#vars in the prune tree

PredictCART_bihar <- predict(tree_bihar_prune, data=vul.class.asset, type="class")
confusionMatrix(data=PredictCART_bihar, vul.class.asset$LCA5_class)
#wall
#roof


################################################### REDUCING ITEMS PER QUESTION
predictors <- readRDS('Bihar_constructed_data/1_Bihar_Pathways_vulpredictors.rds')
colnames(predictors)
predictors <- predictors %>% dplyr::select(caseid, media_, transport.hh_, house.stress_)
#merge with other vuls and class so that we can re-do CART again 
vul.class.caseid <- vuloutcomes %>% dplyr::select(caseid, his.age:kitchen.vent, LCA5_class)
vul.class.caseid <- subset(vul.class.caseid, select = -c(media, transport.hh, house.stress))
vul.class.robust <- merge(vul.class.caseid, predictors, by="caseid")
#drop caseid
vul.class.robust <- subset(vul.class.robust, select = -c(caseid))
colnames(vul.class.robust)


#make class into factor 
vul.class.robust$LCA5_class <- as.factor(vul.class.robust$LCA5_class)

# Set the seed
set.seed(123)
fit.tree <- rpart(LCA5_class ~ . , data=vul.class.robust, control=rpart.control(cp=0.001, xval=5), method="class")
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

#prune tree
tree_bihar_prune<-prune(fit.tree, cp=fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"])
plot(tree_bihar_prune, uniform=TRUE, main="Classification Tree for Bihar LCA")
# Can try prunign using different error cutoffs
#A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.
# For regression models (see next section) the scaled cp has a very direct interpretation: 
# if any split does not increase the overall R2 of the model by at least cp 
# (where R2 is the usual linear-models definition) then that split is decreed to be, 
# a priori, not worth pursuing. The program does not split said branch any further, 
# and saves considerable computational effort.
tree_bihar_prune<-prune(fit.tree, cp=.0036)
plot(tree_bihar_prune, uniform=TRUE, main="Classification Tree for Bihar LCA")
text(tree_bihar_prune, use.n=TRUE, all=TRUE, cex=.7)
prp(tree_bihar_prune, type=5, varlen=0, cex=1)
#vars in the prune tree

#age.1stsex.her
#age.1stpreg.her
#her.edulevel 
#his.edulevel
#cohab.adult.sum 
#hh.size
#female.help
#house.stress
#house.asset
#media 
#smart.cell.hh

PredictCART_bihar <- predict(tree_bihar_prune, data=vul.class.robust, type="class")
confusionMatrix(data=PredictCART_bihar, vul.class.robust$LCA5_class)

