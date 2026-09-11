
#### XX: Identify/test alternative parameterizations for wealth and pastoralism
####purpose: Explores the performance of alternative variable sets for the typing tool
### ancestor: NA
### descendent: none. 
### input: 9a2_dhs_urban_noage.rds, 9a2_dhs_rural_noage.rds
### output: pictures of trees 
### note: working directory needs to be set for each user 

rm(list=ls())

pacman::p_load(rpart, rpart.plot, caret, randomForest)
pacman::p_load(rattle, RColorBrewer, foreign, ggplot2, dplyr, tidyr, data.table, survey, srvyr, haven) # Darcy added
#library(rpart)
#library(rpart.plot)
#library(caret)

#### setup ####

##set working directory - need to change for each user
#setwd("/Users/darcyrao/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Pathways - GE Data & Insights/Modeling") # Darcy
# setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/") # Anu
setwd('C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways - GE Data & Insights\\Modeling') #Elisabeth

## load data
# dhs.seg.comb <- readRDS('Data/Ethiopia_constructed_data/9a2_dhs.seg.final.rds')
dhs.seg.urban <- readRDS("Data/Ethiopia_constructed_data/LCA_urban_noage_data/9a2_dhs_urban_noage.rds")
dhs.seg.rural <- readRDS("Data/Ethiopia_constructed_data/LCA_rural_noage_data/9a2_dhs_rural_noage.rds")

# Merge urban and rural
dhs.seg.comb <- merge(dhs.seg.rural, dhs.seg.urban, all = TRUE)

# Bring in other variables from the original data frame
#IR <- data.table(read.dta("Data/Ethiopia DHS/ET_2016/ETIR71DT/ETIR71FL.DTA")) 
HH <- data.table(read.dta("Data/Ethiopia DHS/ET_2016/ETHR71DT/ETHR71FL.DTA")) 

options(max.print=1000000)
#### Define new variables ####

## Region
#IR$region <- IR$v139 # dejure region of usual residence

## pull in new wealth variables
#IR$hh.cook_notwood <- ifelse(IR$v161 == "wood", 0, 1) # does not cook with wood as fuel
#IR$hh.floor_notearthdung <- ifelse(IR$v127 %in% c("earth/sand", "dung"), 0, 1) # Floor is *not* earth/sand or dung (the latter I added)
#IR$hh.floor_notearth <- ifelse(IR$v127 %in% c("earth/sand"), 0, 1) # Floor is *not* earth/sand
#IR$hh.cook_elect <- ifelse(IR$v161 == "electricity", 1, 0) # cooks with electricity
#IR$water.piped <- ifelse(IR$v113=="piped to yard/plot", 1,0)
#IR$water.notpublic <-  ifelse(IR$v113 == "public tap/standpipe", 0, 1)
#IR$roof.notsod <- ifelse(IR$v129 == "sod", 0, 1)
#IR$walls.notbamboo <- ifelse(IR$v128 == "bamboo with mud", 0, 1)

# new variables from HH file
HH$v001<-HH$hv001
HH$v002<-HH$hv002

HH$asset.tv<-ifelse(HH$hv208=="yes",1,0)
HH$asset.mitad<-ifelse(HH$sh121j=="yes",1,0)
HH$asset.frige<-ifelse(HH$hv209=="yes",1,0)
HH$hh.cookelec<-ifelse(HH$hv226=="electricity",1,0)
HH$asset.bed<-ifelse(HH$sh121i=="yes",1,0)
HH$water.piped<-ifelse(HH$hv201=="piped into dwelling" | HH$hv201=="piped to yard/plot",1,0)
HH$asset.comp<-ifelse(HH$hv243e=="yes",1,0)
HH$asset.table<-ifelse(HH$sh121g=="yes",1,0)
HH$asset.radio<-ifelse(HH$hv207=="yes",1,0)
HH$asset.watch<-ifelse(HH$hv243b=="yes",1,0)
HH$asset.chair<-ifelse(HH$sh121h=="yes",1,0)
HH$asset.car<-ifelse(HH$hv212=="yes",1,0)
HH$wall.mud<-ifelse(HH$hv214=="bamboo with mud",1,0)
HH$roof.metal<-ifelse(HH$hv215=="metal/ corrugated iron",1,0)
HH$toilet.pitimp<-ifelse(HH$hv205=="ventilated improved pit latrine (vip)" | HH$hv205=="pit latrine with slab",1,0)
HH$toilet.pit<-ifelse(HH$hv205=="pit latrine without slab/open pit",1,0)
HH$toilet.none<-ifelse(HH$hv205=="no facility/bush/field",1,0)
HH$wall.cement<-ifelse(HH$hv214=="cement",1,0)
HH$toilet.flush<-ifelse(HH$hv205=="flush to piped sewer system" | HH$hv205=="flush to septic tank",1,0)
HH$asset.chicken<-ifelse(HH$hv246f>0 & HH$hv246f<10,1,0)
HH$owns.land<-ifelse(HH$hv244=="yes",1,0)
HH$hh.sleeproom<-ifelse(HH$hv216==1,1,
                        ifelse(HH$hv216==2,2,
                               ifelse(HH$hv216==3,3,4)))

HH[,imp.water:= ifelse(hv201 == "piped water" | hv201== "piped into dwelling" | hv201== "piped to yard/plot" | hv201== "public tap/standpipe" |
                       hv201 == "tube well water" | hv201 == "tube well or borehole" | hv201 == "protected well" | hv201 == "protected spring" | 
                         hv201 == "rainwater", 0,1)]
HH[,imp.latrine:= ifelse(hv205 == "flush to piped sewer system" | hv205 == "flush to septic tank" | hv205 == "pit toilet latrine" | 
                           hv205=="flush to pit latrine" | hv205== "ventilated improved pit latrine (vip)" | hv205== "pit latrine with slab" | 
                           hv205 == "composting toilet", 1,0)]

HH.merge <- HH %>% select(v001, v002, asset.tv:imp.latrine)

#Separate for U/R
dhs.seg.rural <- merge(dhs.seg.rural, HH.merge, by=c("v001", "v002"), all.x=T)
dhs.seg.urban <- merge(dhs.seg.urban, HH.merge, by=c("v001", "v002"), all.x=T)

#Combined file
dhs.seg.comb <- merge(dhs.seg.comb, HH.merge, by=c("v001", "v002"), all.x=T)

# Do some exploratory PCA analysis
dhs.seg.rural.pca <- dhs.seg.rural %>% select(hh.phone, hh.bank.acct, asset.cattle, asset.cows, asset.horses, asset.goats, 
                                       asset.sheep, hh.sleeproom, asset.tv:imp.latrine)


prn<-princomp(dhs.seg.rural.pca, scores=T)
plot(prn)
autoplot(prn, loadings = TRUE, loadings.label = TRUE, loadings.label.size  = 3)




## Set cluster variable
dhs.seg.comb$cluster <- factor(dhs.seg.comb$cluster_k5_rename_short)

## Exploratory analysis to identify important wealth vars

wealth.vars <- c("hh.electricity", "hh.bank.acct", "asset.tv", "asset.bed", "asset.table", "asset.radio", "asset.chair",
                 "hh.phone", "asset.frige", "asset.mitad", "water.piped")

# Function to plot a heatmap of proportions and of difference from the mean value within Urban/rural
heatmap.bin.simple <- function(dt,var.list=vars,title=''){
  
  plot_dt <- dt %>%
    as_survey_design(id=v021, strata=v023, weights=wt) %>%
    group_by(cluster) %>%
    summarize(across(c(all_of(var.list)), survey_mean)) %>%
    select(c("cluster", all_of(var.list)))
  
  plot_dt <- plot_dt %>% tidyr::gather(vul,prop,-cluster)
  plot_dt$cluster <- as.character(plot_dt$cluster)
  plot_dt$urban <- ifelse(grepl('^U', plot_dt$cluster), "U", "R")
  
  # Add values for difference from the mean within urban/rural
  plot_dt <- plot_dt %>% 
    group_by(urban, vul) %>%
    summarise(mean_prop = mean(prop), sd_prop = sd(prop)) %>%
    right_join(plot_dt) %>%
    group_by(urban, vul) %>%
    mutate(abs_diff = abs(mean_prop - prop))
  
  # Plot proportions
  p <- plot_dt %>%
    mutate(label = round(prop, 2)) %>%
    ggplot(aes(cluster,vul, fill= prop)) + 
    geom_tile() +  
    geom_text(aes(label = scales::percent(label)), color = "grey60") +                                       #heatmat plot
    scale_fill_gradient2(low = "#313695", mid = "#FFFFBF", high = "#A50026", midpoint = 0.5,breaks=seq(0,1,0.1)) +
    ggtitle("Proportions") +xlab('') +ylab('') + labs(fill='Proportion within\ncluster') +
    theme(legend.text=element_text(size=8),  legend.title=element_text(size=10) ) + 
    theme(axis.text = element_text(size=10)) + theme(strip.text = element_text(size = 12))
  
  # plot diff from mean
  max_diff <- max(plot_dt$abs_diff)
  order_sd <- plot_dt %>% group_by(urban, vul) %>% filter(row_number() == 1) %>% arrange(desc(sd_prop)) %>% 
    ungroup() %>% arrange(desc(urban)) %>% select(-cluster)
  plot_dt_urban <- plot_dt %>% filter(urban == "U") 
  plot_dt_urban$vul <- factor(plot_dt_urban$vul, levels = order_sd$vul[order_sd$urban == "U"])
  plot_dt_rural <- plot_dt %>% filter(urban == "R") 
  plot_dt_rural$vul <- factor(plot_dt_rural$vul, levels = order_sd$vul[order_sd$urban == "R"])
  
  p2_urban <- plot_dt_urban %>%
    mutate(label = round(abs_diff, 2)) %>%
    ggplot(aes(cluster,vul, fill= abs_diff)) + 
    geom_tile() +  scale_y_discrete(limits = rev) +
    geom_text(aes(label = scales::percent(label)), color = "grey60") +                                       #heatmat plot
    scale_fill_gradient2(low = "#313695", mid = "#FFFFBF", high = "#A50026", midpoint = max_diff/2,breaks=seq(0,max_diff,0.05)) +
    ggtitle("Difference from mean: Urban") +xlab('') +ylab('') + labs(fill='Abs diff from mean by urban/rural') +
    theme(legend.text=element_text(size=8),  legend.title=element_text(size=10) ) + 
    theme(axis.text = element_text(size=10)) + theme(strip.text = element_text(size = 12))
  
  p2_rural <- plot_dt_rural %>%
    mutate(label = round(abs_diff, 2)) %>%
    ggplot(aes(cluster,vul, fill= abs_diff)) + 
    geom_tile() +  scale_y_discrete(limits = rev) +
    geom_text(aes(label = scales::percent(label)), color = "grey60") +                                       #heatmat plot
    scale_fill_gradient2(low = "#313695", mid = "#FFFFBF", high = "#A50026", midpoint = max_diff/2,breaks=seq(0,max_diff,0.05)) +
    ggtitle("Difference from mean: Rural") +xlab('') +ylab('') + labs(fill='Abs diff from mean by urban/rural') +
    theme(legend.text=element_text(size=8),  legend.title=element_text(size=10) ) + 
    theme(axis.text = element_text(size=10)) + theme(strip.text = element_text(size = 12))
  
  out <- list(p, p2_urban, p2_rural, order_sd)
  return(out)
}

wealth_heatmaps <- heatmap.bin.simple(dhs.seg.comb, var.list = wealth.vars)
wealth_heatmaps[[1]]
wealth_heatmaps[[2]]
wealth_heatmaps[[3]]
wealth_heatmaps[[4]] %>% print(n=1000)

# Separate for urban/rural
## Set cluster variable
dhs.seg.urban$cluster <- factor(dhs.seg.urban$cluster_k5_rename_short)

## Exploratory analysis to identify important wealth vars

wealth.vars.u <- c("hh.electricity", "hh.bank.acct", "asset.tv", "asset.bed", "asset.table", "asset.radio", "asset.chair",
                 "hh.phone", "asset.frige", "asset.mitad", "water.piped", "hh.cookelec", "asset.comp", "asset.watch","asset.car", 
                 "imp.latrine", "wall.cement","toilet.flush")
wealth.vars.u2 <- c("hh.electricity", "hh.bank.acct", "asset.tv", "asset.bed", 
                    "hh.phone", "asset.frige", "asset.mitad", "water.piped", "hh.cookelec", "asset.comp")
wealth_heatmaps <- heatmap.bin.simple(dhs.seg.urban, var.list = wealth.vars.u)
wealth_heatmaps[[1]]
wealth_heatmaps[[2]]
wealth_heatmaps[[4]]

# Define wealth index
# full
dhs.seg.urban<-dhs.seg.urban %>% mutate(asset.ind.u = hh.electricity+hh.bank.acct+asset.tv+asset.bed+asset.table+asset.radio+asset.chair+
                                        hh.phone+asset.frige+asset.mitad+water.piped+hh.cookelec+asset.comp+asset.watch)

# with paired down list to top 5 based on SD within urban/rural - don't break down by quintiles, just use the index
dhs.seg.urban<-dhs.seg.urban %>% mutate(asset.ind.alt.u = hh.electricity+hh.bank.acct+asset.tv+asset.bed+ 
                    hh.phone+asset.frige+asset.mitad+asset.comp+water.piped+hh.cookelec)

dhs.seg.urban<-dhs.seg.urban %>% mutate(asset.ind.alt2.u = hh.electricity+hh.bank.acct+asset.tv+asset.bed+ 
                                          hh.phone+asset.mitad+asset.comp+hh.cookelec) # took out piped water, frige

dhs.seg.urban<-dhs.seg.urban %>% mutate(asset.ind.alt3.u = hh.electricity+hh.bank.acct+asset.tv+asset.bed+ 
                                          hh.phone+asset.mitad+hh.cookelec) # took out computer


#### Perform CART ####
set.seed(123)

## URBAN ####

# Original wealth index
## Grow tree
tree_urban <- rpart(cluster ~ pship.cat+ med.permis+wealth.index.ur.ord+ med.index+dv.index+ jdwd.index.cat+
                      partner.working.yn+religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+
                      highestyearsedinHH.yrs+hh.electricity+any.media.yn+anyed.yn+early.sex.15, 
                    data=dhs.seg.urban, control=rpart.control(cp=0.001, xval=5), method="class")

plotcp(tree_urban); printcp(tree_urban)

#prune tree
#tree_urban_prune<-prune(tree_urban, cp=tree_urban$cptable[which.min(tree_urban$cptable[,"xerror"]),"CP"])
# Can try prunign using different error cutoffs
tree_urban_prune<-prune(tree_urban, cp=.002)
plotcp(tree_urban_prune); printcp(tree_urban_prune)

PredictCART_train_urban <- predict(tree_urban, data=dhs.seg.urban, type="class")
confusionMatrix(data=PredictCART_train_urban, dhs.seg.urban$cluster) #97% Accuracy

prp(tree_urban_prune, type=3, varlen=0, cex=1)
# fancyRpartPlot(tree_urban_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)

PredictCART_train_urban <- predict(tree_urban_prune, data=dhs.seg.urban, type="class")
confusionMatrix(data=PredictCART_train_urban, dhs.seg.urban$cluster) #97% Accuracy


# Constructed additive wealth index - reduced to 14 items
## Grow tree
tree_urban <- rpart(cluster ~ pship.cat+med.permis+asset.ind.u+med.index+dv.index+jdwd.index.cat+partner.working.yn+
                      religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+
                      highestyearsedinHH.yrs+hh.electricity+any.media.yn+anyed.yn+early.sex.15, 
                    data=dhs.seg.urban, control=rpart.control(cp=0.001, xval=5), method="class")

plotcp(tree_urban); printcp(tree_urban)
prp(tree_urban, type=5, varlen=0, cex=1)

PredictCART_train_urban <- predict(tree_urban, data=dhs.seg.urban, type="class")
confusionMatrix(data=PredictCART_train_urban, dhs.seg.urban$cluster) #97.3% accuracy

#prune tree
#tree_urban_prune<-prune(tree_urban, cp=tree_urban$cptable[which.min(tree_urban$cptable[,"xerror"]),"CP"])
# Can try prunign using different error cutoffs
tree_urban_prune<-prune(tree_urban, cp=.002)
plotcp(tree_urban_prune); printcp(tree_urban_prune)

prp(tree_urban_prune, type=5, varlen=0, cex=1)
# fancyRpartPlot(tree_urban_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)

PredictCART_train_urban <- predict(tree_urban_prune, data=dhs.seg.urban, type="class")
confusionMatrix(data=PredictCART_train_urban, dhs.seg.urban$cluster) #96.8% accuracy

# Constructed additive wealth index - reduced to 10 items
# THIS IS MY PICK FOR THE BEST MODEL
## Grow tree
tree_urban <- rpart(cluster ~ pship.cat+ med.permis+asset.ind.alt.u+ med.index+dv.index+jdwd.index.cat+
                      partner.working.yn+religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+
                      highestyearsedinHH.yrs+hh.electricity+any.media.yn+anyed.yn+early.sex.15, 
                    data=dhs.seg.urban, control=rpart.control(cp=0.001, xval=5), method="class")

plotcp(tree_urban); printcp(tree_urban)
prp(tree_urban, type=5, varlen=0, cex=1)

PredictCART_train_urban <- predict(tree_urban, data=dhs.seg.urban, type="class")
confusionMatrix(data=PredictCART_train_urban, dhs.seg.urban$cluster) #97.3% accuracy

#prune tree
#tree_urban_prune<-prune(tree_urban, cp=tree_urban$cptable[which.min(tree_urban$cptable[,"xerror"]),"CP"])
# Can try prunign using different error cutoffs
tree_urban_prune<-prune(tree_urban, cp=.0012)
plotcp(tree_urban_prune); printcp(tree_urban_prune)

prp(tree_urban_prune, type=5, varlen=0, cex=1)
# fancyRpartPlot(tree_urban_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)

PredictCART_train_urban <- predict(tree_urban_prune, data=dhs.seg.urban, type="class")
confusionMatrix(data=PredictCART_train_urban, dhs.seg.urban$cluster) #96.9% accuracy

# Constructed additive wealth index - reduced to 8 items
## Grow tree
tree_urban <- rpart(cluster ~ pship.cat+ med.permis+asset.ind.alt2.u+ med.index+dv.index+ jdwd.index.cat+
                      partner.working.yn+religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+
                      highestyearsedinHH.yrs+hh.electricity+any.media.yn+anyed.yn+early.sex.15, 
                    data=dhs.seg.urban, control=rpart.control(cp=0.001, xval=5), method="class")

plotcp(tree_urban); printcp(tree_urban)

#prune tree
#tree_urban_prune<-prune(tree_urban, cp=tree_urban$cptable[which.min(tree_urban$cptable[,"xerror"]),"CP"])
# Can try prunign using different error cutoffs
tree_urban_prune<-prune(tree_urban, cp=.002)
plotcp(tree_urban_prune); printcp(tree_urban_prune)

prp(tree_urban_prune, type=5, varlen=0, cex=1)
# fancyRpartPlot(tree_urban_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)

PredictCART_train_urban <- predict(tree_urban_prune, data=dhs.seg.urban, type="class")
confusionMatrix(data=PredictCART_train_urban, dhs.seg.urban$cluster) #96.7% accuracy

# RURAL
# Separate for urban/rural
## Set cluster variable
dhs.seg.rural$cluster <- factor(dhs.seg.rural$cluster_k5_rename_short)

## Exploratory analysis to identify important wealth vars

wealth.vars.r <- c("hh.electricity", "hh.bank.acct", "asset.tv", "asset.bed", "asset.table", "asset.radio", "asset.chair",
                   "hh.phone", "asset.frige", "asset.mitad", "water.piped", "roof.metal", "toilet.pit", "wall.mud",
                   "asset.chicken", "owns.land", "imp.water","imp.latrine","toilet.none", "toilet.pitimp")
wealth.vars.r2 <- c("hh.electricity", "hh.bank.acct", "asset.tv", "asset.radio", "asset.bed", "asset.table", "asset.chair",
                    "toilet.pit", "wall.mud","roof.metal")
wealth_heatmaps <- heatmap.bin.simple(dhs.seg.rural, var.list = wealth.vars.r)
wealth_heatmaps[[1]]
wealth_heatmaps[[2]]
wealth_heatmaps[[4]]

# Define wealth index
# full
dhs.seg.rural<-dhs.seg.rural %>% mutate(asset.ind.r = hh.electricity+hh.bank.acct+asset.tv+asset.bed+asset.table+asset.radio+asset.chair+
                                          hh.phone+asset.frige+asset.mitad+water.piped+roof.metal+toilet.pit+wall.mud+asset.chicken+owns.land)

# with paired down list to top 13 based on SD
dhs.seg.rural<-dhs.seg.rural %>% mutate(asset.ind.alt.r = hh.electricity+hh.bank.acct+asset.tv+asset.bed+asset.table+asset.radio+asset.chair+
                                          hh.phone+asset.frige+asset.mitad+water.piped+roof.metal+toilet.pit+wall.mud)

dhs.seg.rural<-dhs.seg.rural %>% mutate(asset.ind.alt2.r = hh.electricity+hh.bank.acct+asset.tv+asset.bed+asset.table+asset.radio+asset.chair+
                                          roof.metal+toilet.pit+wall.mud) # took out phone, frige and water - 10

dhs.seg.rural<-dhs.seg.rural %>% mutate(asset.ind.alt3.r = hh.electricity+hh.bank.acct+asset.tv+asset.bed+asset.table+asset.radio+asset.chair+
                                          roof.metal+imp.latrine+imp.water) # swapped out sanitation

dhs.seg.rural<-dhs.seg.rural %>% mutate(asset.ind.alt4.r = hh.electricity+hh.bank.acct+asset.tv+asset.radio+asset.bed+asset.chair+
                                          roof.metal) # took out assets and housing

# Original wealth index
tree_rural <- rpart(cluster ~ pship.cat+med.permis+wealth.index.ur.ord+med.index+dv.index+jdwd.index.cat+partner.working.yn+
                      religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+highestyearsedinHH.yrs+
                      hh.electricity+any.media.yn+anyed.yn+early.sex.15+pastoralnoagro, 
                    data=dhs.seg.rural, control=rpart.control(cp=0.001, xval=5), method="class")
plotcp(tree_rural); printcp(tree_rural)
prp(tree_rural, type=3, varlen=0, cex=.9)

PredictCART_train_rural <- predict(tree_rural, data=dhs.seg.rural, type="class")
confusionMatrix(data=PredictCART_train_rural, dhs.seg.rural$cluster)

#prune tree
#tree_rural_prune<-prune(tree_rural, cp=tree_rural$cptable[which.min(tree_rural$cptable[,"xerror"]),"CP"])
# Alternative prune - used
tree_rural_prune<-prune(tree_rural, cp=.002)
plotcp(tree_rural_prune); printcp(tree_rural_prune)

prp(tree_rural_prune, type=3, varlen=0, cex=.9)
# fancyRpartPlot(tree_rural_prune, type = 3, tweak = 1.5, caption = NULL)

PredictCART_train_rural <- predict(tree_rural_prune, data=dhs.seg.rural, type="class")
confusionMatrix(data=PredictCART_train_rural, dhs.seg.rural$cluster)

# Alternate wealth index from summative assets - full
tree_rural <- rpart(cluster ~ pship.cat+med.permis+asset.ind.alt.r+med.index+dv.index+jdwd.index.cat+partner.working.yn+
                      religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+highestyearsedinHH.yrs+
                      hh.electricity+any.media.yn+anyed.yn+early.sex.15+pastoralnoagro, 
                    data=dhs.seg.rural, control=rpart.control(cp=0.001, xval=5), method="class")
plotcp(tree_rural); printcp(tree_rural)
prp(tree_rural, type=3, varlen=0, cex=.9)

PredictCART_train_rural <- predict(tree_rural, data=dhs.seg.rural, type="class")
confusionMatrix(data=PredictCART_train_rural, dhs.seg.rural$cluster)

#prune tree
# tree_rural_prune<-prune(tree_rural, cp=tree_rural$cptable[which.min(tree_rural$cptable[,"xerror"]),"CP"])
# Alternative prune - used
tree_rural_prune<-prune(tree_rural, cp=.002)
plotcp(tree_rural_prune); printcp(tree_rural_prune)

plot(tree_rural_prune, uniform=TRUE, main="Classification Tree for Urban LCA")
text(tree_rural_prune, use.n=TRUE, all=TRUE, cex=.7)
prp(tree_rural_prune, type=3, varlen=0, cex=.9)
# fancyRpartPlot(tree_rural_prune, type = 3, tweak = 1.5, caption = NULL)

PredictCART_train_rural <- predict(tree_rural_prune, data=dhs.seg.rural, type="class")
confusionMatrix(data=PredictCART_train_rural, dhs.seg.rural$cluster)


# Alternate wealth index from summative assets - top 10
# I think this one is the best - MY CHOICE
tree_rural <- rpart(cluster ~ pship.cat+med.permis+asset.ind.alt2.r+med.index+dv.index+jdwd.index.cat+partner.working.yn+
                      religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+highestyearsedinHH.yrs+
                      hh.electricity+any.media.yn+anyed.yn+early.sex.15+pastoralnoagro, 
                    data=dhs.seg.rural, control=rpart.control(cp=0.001, xval=5), method="class")
plotcp(tree_rural); printcp(tree_rural)
prp(tree_rural, type=3, varlen=0, cex=.9)

PredictCART_train_rural <- predict(tree_rural, data=dhs.seg.rural, type="class")
confusionMatrix(data=PredictCART_train_rural, dhs.seg.rural$cluster)

#prune tree
# tree_rural_prune<-prune(tree_rural, cp=tree_rural$cptable[which.min(tree_rural$cptable[,"xerror"]),"CP"])
# Alternative prune - used
tree_rural_prune<-prune(tree_rural, cp=.002)
plotcp(tree_rural_prune); printcp(tree_rural_prune)


plot(tree_rural_prune, uniform=TRUE, main="Classification Tree for Urban LCA")
text(tree_rural_prune, use.n=TRUE, all=TRUE, cex=.7)
prp(tree_rural_prune, type=3, varlen=0, cex=.9)
# fancyRpartPlot(tree_rural_prune, type = 3, tweak = 1.5, caption = NULL)

PredictCART_train_rural <- predict(tree_rural_prune, data=dhs.seg.rural, type="class")
confusionMatrix(data=PredictCART_train_rural, dhs.seg.rural$cluster)


# Alternate wealth index
tree_rural <- rpart(cluster ~ pship.cat+med.permis+asset.ind.alt3.r+med.index+dv.index+jdwd.index.cat+partner.working.yn+
                      religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+highestyearsedinHH.yrs+
                      hh.electricity+any.media.yn+anyed.yn+early.sex.15, 
                    data=dhs.seg.rural, control=rpart.control(cp=0.001, xval=5), method="class")
plotcp(tree_rural); printcp(tree_rural)
prp(tree_rural, type=3, varlen=0, cex=.9)

PredictCART_train_rural <- predict(tree_rural, data=dhs.seg.rural, type="class")
confusionMatrix(data=PredictCART_train_rural, dhs.seg.rural$cluster)

#prune tree
#tree_rural_prune<-prune(tree_rural, cp=tree_rural$cptable[which.min(tree_rural$cptable[,"xerror"]),"CP"])
# Alternative prune - used
tree_rural_prune<-prune(tree_rural, cp=.002)
plotcp(tree_rural_prune); printcp(tree_rural_prune)

prp(tree_rural_prune, type=3, varlen=0, cex=.9)
# fancyRpartPlot(tree_rural_prune, type = 3, tweak = 1.5, caption = NULL)

PredictCART_train_rural <- predict(tree_rural_prune, data=dhs.seg.rural, type="class")
confusionMatrix(data=PredictCART_train_rural, dhs.seg.rural$cluster)


# Alternate wealth index
tree_rural <- rpart(cluster ~ pship.cat+med.permis+asset.ind.alt4.r+med.index+dv.index+jdwd.index.cat+partner.working.yn+
                      religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+highestyearsedinHH.yrs+
                      hh.electricity+any.media.yn+anyed.yn+early.sex.15, 
                    data=dhs.seg.rural, control=rpart.control(cp=0.001, xval=5), method="class")
plotcp(tree_rural); printcp(tree_rural)
prp(tree_rural, type=3, varlen=0, cex=.9)

PredictCART_train_rural <- predict(tree_rural, data=dhs.seg.rural, type="class")
confusionMatrix(data=PredictCART_train_rural, dhs.seg.rural$cluster)

#prune tree
#tree_rural_prune<-prune(tree_rural, cp=tree_rural$cptable[which.min(tree_rural$cptable[,"xerror"]),"CP"])
# Alternative prune - used
tree_rural_prune<-prune(tree_rural, cp=.002)
plotcp(tree_rural_prune); printcp(tree_rural_prune)

prp(tree_rural_prune, type=3, varlen=0, cex=.9)
# fancyRpartPlot(tree_rural_prune, type = 3, tweak = 1.5, caption = NULL)

PredictCART_train_rural <- predict(tree_rural_prune, data=dhs.seg.rural, type="class")
confusionMatrix(data=PredictCART_train_rural, dhs.seg.rural$cluster)


