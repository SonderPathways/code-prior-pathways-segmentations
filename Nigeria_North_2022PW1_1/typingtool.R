
# Load libraries
pacman::p_load(rpart, rpart.plot, caret, randomForest)
pacman::p_load(rattle, RColorBrewer, foreign, ggplot2, dplyr, tidyr, data.table, survey, srvyr, haven)

# Read in urban file, create new wealth variable
urbseg<- readRDS("C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways - GE Data & Insights\\Modeling\\Data\\Senegal DHS\\outcomes_vulnerability_class_urban.rds")

urbseg <- urbseg %>% mutate(
  wealth.sum = rowSums(across(c(hh.electricity, hh.tv, hh.refrig, hh.bank.acct, hh.computer, hh.car, hh.noimp.latrine))))

# Read in rural file, create new wealth variable
rurseg<- readRDS("C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\scenario_dhs_senegal_2019\\FINAL SOLUTIONS\\rural\\outcomes_vulnerability_class_rural.rds")

rurseg <- rurseg %>% mutate(
  wealth.sum = rowSums(across(c(hh.electricity, hh.tv, hh.refrig, hh.bank.acct, hh.computer, hh.car, hh.stove))))

# Look to see which household asset variables are most correlated with the wealth.index.ur variable so can create a
# new variable that's the sum of these assets rather than a complex PCA
rurseg$wealth.index<-as.numeric(rurseg$wealth.index.ur)
rurseg2<-rurseg[,c(189:207, 230:241, 268, 270)]
rurseg2<-rurseg2[,-c(19,21,30)]

corr <- round(cor(rurseg2), 2)
ggcorrplot(corr)

#### Perform CART ####
set.seed(1235)

## URBAN ####

## Grow tree
tree_urban <- rpart(LCA5_class ~ marital.cat+ any.media.yn+ desc.lrgpur+ desc.ownincome+ wealth.sum+
                      desc.fp+wd.index+ ed.level+ hh.savingsclub+ fertility.pref.cat+ hh.mobile+ hh.electricity+
                      has.bank+ hh.clean.fuel+ hh.noimp.water+ hh.noimp.latrine+yrs.curr.pship.cat+
                      partner.absent.yn+ prev.pship+ideal.n.child.cat+female.circumcision+
                      hh.recv.funds.cat+ internet.use+hh.land+highestyearsedinHH.yrs+med.dist+
                      med.cost+mobile.financial+ num.under5.cat+ partner.ed.level+
                      total.sex.partners+ wife.order+ working+freq.tv,
                    data=urbseg, control=rpart.control(cp=0.001, xval=5), method="class")

plotcp(tree_urban); printcp(tree_urban)

#prune tree
#tree_urban_prune<-prune(tree_urban, cp=tree_urban$cptable[which.min(tree_urban$cptable[,"xerror"]),"CP"])
# Can try pruning using different error cutoffs
tree_urban_prune<-prune(tree_urban, cp=.002)
plotcp(tree_urban_prune); printcp(tree_urban_prune)

# Look at sensitivity/specificity/etc for pruned tree
PredictCART_train_urban <- predict(tree_urban, data=urbseg, type="class")
confusionMatrix(data=PredictCART_train_urban, as.factor(urbseg$LCA5_class))

prp(tree_urban_prune, type=3, varlen=0, cex=1)
#fancyRpartPlot(tree_urban_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)


### RURAL

tree_rural <- rpart(LCA4_class ~ work.seasonal+marital.cat+ wife.order+ any.media.yn+ desc.lrgpur+ desc.ownincome+desc.fp+
                      wd.index+ ed.level+ hh.savingsclub+ fertility.pref.cat+ hh.electricity+head.sex+ num.under5.cat+
                      has.bank+ hh.clean.fuel+ hh.noimp.water+ hh.noimp.latrine+yrs.curr.pship.cat+working+
                      partner.absent.yn+ prev.pship+ideal.n.child.cat+wealth.sum+hh.bourse+med.cost.dist.moh+
                      hh.recv.funds.cat+ internet.use+hh.land+highestyearsedinHH.yrs+
                      mobile.financial+ partner.ed.level+malaria.zone+num.preg.cat+
                      freq.tv+hh.savingsclub+ hw.visit.12mo,
                    data=rurseg, control=rpart.control(cp=0.001, xval=5), method="class")

plotcp(tree_rural); printcp(tree_rural)

#prune tree
#tree_rural_prune<-prune(tree_rural, cp=tree_rural$cptable[which.min(tree_rural$cptable[,"xerror"]),"CP"])
# Can try pruning using different error cutoffs
tree_rural_prune<-prune(tree_rural, cp=.0021)
plotcp(tree_rural_prune); printcp(tree_rural_prune)

# Look at sensitivity/specificity/etc for pruned tree
PredictCART_train_rural <- predict(tree_rural_prune, data=rurseg, type="class")
confusionMatrix(data=PredictCART_train_rural, as.factor(rurseg$LCA4_class))

prp(tree_rural_prune, type=3, varlen=0, cex=1)
#fancyRpartPlot(tree_rural_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)
