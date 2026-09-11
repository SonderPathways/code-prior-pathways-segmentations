

#### 11_Ethiopia_DHS_CART
####purpose: conducts classification analysis with clusters as output
### ancestor: script 9a2_Ethiopia_DHS_LCA
### descendent: none. See script 10a2 for exploration/plots
### input: 9a2_dhs_urban_noage.rds, 9a2_dhs_rural_noage.rds
### output: pictures of trees saved as typingtool.doc in plots folder
### note: working directory needs to be set for each user 
### #R markdown version may be out dated compared to this one

### Note:need to set working directory for each user ######

rm(list=ls())

pacman::p_load(rpart, rpart.plot, caret, randomForest)
#library(rpart)
#library(rpart.plot)
#library(caret)

### set working directory - need to change for each user #########

#setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/") # Anu
setwd('C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways - GE Data & Insights\\Modeling') #Elisabeth

dhs.seg.urban <- readRDS('Data/Ethiopia_constructed_data/LCA_urban_noage_data/9a2_dhs_urban_noage.rds')
dhs.seg.rural <- readRDS('Data/Ethiopia_constructed_data/LCA_rural_noage_data/9a2_dhs_rural_noage.rds')

#dhs.seg.urban <- readRDS('C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways - GE Data & #Insights\\Modeling\\Data\\Ethiopia_constructed_data\\LCA_urban_noage_data\\dhs_urban_noage.rds')
#dhs.seg.rural <- readRDS('C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways - GE Data & #Insights\\Modeling\\Data\\Ethiopia_constructed_data\\LCA_rural_noage_data\\dhs_rural_noage.rds')

r.old<-c("RM.1","RM.2","RM.3","RU.1","RU.2")
r.new<-c("RM.3","RM.2","RM.1","RU.2","RU.1")
rural.new.seg<-data.frame("cluster_k5_rename_short"=r.old, "rename_seg"=r.new, stringsAsFactors=T)

u.old<-c("UM.1","UM.2","UM.3","UU.1","UU.2")
u.new<-c("UM.3","UM.2","UM.1","UU.2","UU.1")
urban.new.seg<-data.frame("cluster_k5_rename_short"=u.old, "rename_seg"=u.new, stringsAsFactors=T)

dhs.seg.rural<-merge(dhs.seg.rural, rural.new.seg, by="cluster_k5_rename_short")
dhs.seg.urban<-merge(dhs.seg.urban, urban.new.seg, by="cluster_k5_rename_short")

set.seed(123)

# URBAN
#grow tree
tree_urban <- rpart(rename_seg ~ pship.cat+ med.permis+wealth.index.ur.ord+ med.index+dv.index+ jdwd.index.cat+
                      partner.working.yn+religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+
                      highestyearsedinHH.yrs+hh.electricity+any.media.yn+anyed.yn+early.sex.15, 
                    data=dhs.seg.urban, control=rpart.control(cp=0.001, xval=5), method="class")

plotcp(tree_urban); printcp(tree_urban)

#prune tree
tree_urban_prune<-prune(tree_urban, cp=tree_urban$cptable[which.min(tree_urban$cptable[,"xerror"]),"CP"])
# Can try prunign using different error cutoffs
# tree_urban_prune<-prune(tree_urban, cp=.005)

plot(tree_urban_prune, uniform=TRUE, main="Classification Tree for Urban LCA")
text(tree_urban_prune, use.n=TRUE, all=TRUE, cex=.7)
prp(tree_urban_prune, type=5, varlen=0, cex=1)

PredictCART_train_urban <- predict(tree_urban_prune, data=dhs.seg.urban, type="class")
confusionMatrix(data=PredictCART_train_urban, dhs.seg.urban$rename_seg)

# RURAL
#grow tree
tree_rural <- rpart(rename_seg ~ pship.cat+med.permis+wealth.index.ur.ord+med.index+dv.index+jdwd.index.cat+partner.working.yn+
                      religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+highestyearsedinHH.yrs+
                      hh.electricity+any.media.yn+anyed.yn+early.sex.15+pastoralnoagro, 
                    data=dhs.seg.rural, control=rpart.control(cp=0.001, xval=5), method="class")
plotcp(tree_rural); printcp(tree_rural)
summary(tree_rural)

#prune tree
#tree_rural_prune<-prune(tree_rural, cp=tree_rural$cptable[which.min(tree_rural$cptable[,"xerror"]),"CP"])
# Alternative prune - used
tree_rural_prune<-prune(tree_rural, cp=.002)

plot(tree_rural_prune, uniform=TRUE, main="Classification Tree for Urban LCA")
text(tree_rural_prune, use.n=TRUE, all=TRUE, cex=.7)
prp(tree_rural_prune, type=5, varlen=0, cex=.9)

PredictCART_train_rural <- predict(tree_rural_prune, data=dhs.seg.rural, type="class")
confusionMatrix(data=PredictCART_train_rural, dhs.seg.rural$rename_seg)


#Random Forest
library(randomForest)
urban_fit <- randomForest(cluster_k5_rename ~ pship.cat+ med.permis+wealth.index.ur.ord+ med.index+dv.index+ jdwd.index.cat+
                      partner.working.yn+religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+
                      highestyearsedinHH.yrs+hh.electricity+any.media.yn+anyed.yn+early.sex.15, 
                    data=dhs.seg.urban, localImp=T)
print(urban_fit) # view results
importance<-measure_importance(urban_fit) # importance of each predictor
plot_min_depth_distribution(urban_fit, mean_sample = "relevant_trees", k=15)
plot_multi_way_importance(importance, size_measure = "no_of_nodes")
plot_min_depth_interactions(interactions_fram)


