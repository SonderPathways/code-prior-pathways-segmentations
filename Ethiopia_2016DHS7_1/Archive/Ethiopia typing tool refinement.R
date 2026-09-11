
#### XX: Identify/test alternative parameterizations for wealth and pastoralism
####purpose: Explores the performance of alternative variable sets for the typing tool
### ancestor: NA
### descendent: none. 
### input: 9a2_dhs_urban_noage.rds, 9a2_dhs_rural_noage.rds
### output: pictures of trees 
### note: working directory needs to be set for each user 

rm(list=ls())

pacman::p_load(rpart, rpart.plot, caret, randomForest)
pacman::p_load(rattle, rpart.plot, RColorBrewer, foreign, ggplot2, dplyr, tidyr, data.table, survey, srvyr) # Darcy added
#library(rpart)
#library(rpart.plot)
#library(caret)

#### setup ####

##set working directory - need to change for each user
setwd("/Users/darcyrao/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Pathways - GE Data & Insights/Modeling") # Darcy
# setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/") # Anu
# setwd('C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways - GE Data & Insights\\Modeling') #Elisabeth

## load data
# dhs.seg.comb <- readRDS('Data/Ethiopia_constructed_data/9a2_dhs.seg.final.rds')
dhs.seg.urban <- readRDS("Data/Ethiopia_constructed_data/LCA_urban_noage_data/9a2_dhs_urban_noage.rds")
dhs.seg.rural <- readRDS("Data/Ethiopia_constructed_data/LCA_rural_noage_data/9a2_dhs_rural_noage.rds")

# Merge urban and rural
dhs.seg.comb <- merge(dhs.seg.rural, dhs.seg.urban, all = TRUE)

# Bring in other variables from the original data frame
IR <- data.table(read.dta("Data/Ethiopia DHS/ET_2016/ETIR71DT/ETIR71FL.DTA")) 
# HH <- data.table(read.dta("Data/Ethiopia DHS/ET_2016/ETHR71DT/ETHR71FL.DTA")) 

options(max.print=1000000)
#### Define new variables ####

## Region
IR$region <- IR$v139 # dejure region of usual residence

## pull in new wealth variables
IR$hh.cook_notwood <- ifelse(IR$v161 == "wood", 0, 1) # does not cook with wood as fuel
IR$hh.floor_notearthdung <- ifelse(IR$v127 %in% c("earth/sand", "dung"), 0, 1) # Floor is *not* earth/sand or dung (the latter I added)
IR$hh.floor_notearth <- ifelse(IR$v127 %in% c("earth/sand"), 0, 1) # Floor is *not* earth/sand
IR$hh.cook_elect <- ifelse(IR$v161 == "electricity", 1, 0) # cooks with electricity
IR$water.piped <- ifelse(IR$v113=="piped to yard/plot", 1,0)
IR$water.notpublic <-  ifelse(IR$v113 == "public tap/standpipe", 0, 1)
IR$roof.notsod <- ifelse(IR$v129 == "sod", 0, 1)
IR$walls.notbamboo <- ifelse(IR$v128 == "bamboo with mud", 0, 1)

IR$wt<-IR$v005/1000000
IR.merge <- IR %>% select(v001, v002, caseid, region, hh.cook_notwood:walls.notbamboo)

dhs.seg.comb <- merge(dhs.seg.comb, IR.merge, by="caseid", all.x=T)

## Set cluster variable
dhs.seg.comb$cluster <- factor(dhs.seg.comb$cluster_k5_rename_short)

## Exploratory analysis to identify important wealth vars

wealth.vars <- c("hh.tv", "hh.refrig", "hh.cook_notwood", "hh.floor_notearth",
                 "hh.cook_elect", "hh.electricity", "hh.bank.acct",
                 "water.piped", "hh.computer", "toilet.facility",
                 "hh.table", "hh.radio", "hh.watch", "hh.chair",
                 "hh.phone", "water.notpublic", "roof.notsod", "walls.notbamboo",
                 "hh.car", "hh.bike", "hh.land", "hh.rooms.2plus", "asset.cattle",
                 "asset.cows", "asset.goats", "asset.chickens")

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


# Define wealth index
# full
dhs.seg.comb<-dhs.seg.comb %>% mutate(asset.ind = hh.tv + hh.refrig + hh.cook_notwood + hh.floor_notearth +
                                        hh.cook_elect + hh.electricity + hh.bank.acct +
                                        water.piped + hh.computer + toilet.facility +
                                        hh.table + hh.radio + hh.watch + hh.chair +
                                        hh.phone + water.notpublic + roof.notsod + walls.notbamboo +
                                        hh.car)
nlab<-c(1,2,3,4,5)
dhs.seg.comb<-mutate(dhs.seg.comb,asset.quint=as.factor(cut_number(dhs.seg.comb$asset.ind,n=5,labels=nlab)))

# with paired down list to top 5 based on SD within urban/rural - don't break down by quintiles, just use the index
dhs.seg.comb<-dhs.seg.comb %>% mutate(asset.ind.alt.urban = hh.tv + hh.cook_notwood + water.piped + hh.bank.acct + 
                                        hh.land,
                                      asset.ind.alt.rural = toilet.facility + hh.table + hh.chair + hh.floor_notearth +
                                        asset.goats)


#### Subset by urban/rural ####
dhs.seg.urban <- dhs.seg.comb %>% filter(URBAN == 1)
dhs.seg.urban$cluster <- droplevels(dhs.seg.urban$cluster)
dhs.seg.rural <- dhs.seg.comb %>% filter(URBAN == 2)
dhs.seg.rural$cluster <- droplevels(dhs.seg.rural$cluster)

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
    tree_urban_prune<-prune(tree_urban, cp=tree_urban$cptable[which.min(tree_urban$cptable[,"xerror"]),"CP"])
      # Can try prunign using different error cutoffs
       # tree_urban_prune<-prune(tree_urban, cp=.005)
    plotcp(tree_urban_prune); printcp(tree_urban_prune)
    

    plot(tree_urban_prune, uniform=TRUE, main="Classification Tree for Urban LCA")
    text(tree_urban_prune, use.n=TRUE, all=TRUE, cex=.7)
    prp(tree_urban_prune, type=3, varlen=0, cex=1)
    # fancyRpartPlot(tree_urban_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)
    
    PredictCART_train_urban <- predict(tree_urban_prune, data=dhs.seg.urban, type="class")
    confusionMatrix(data=PredictCART_train_urban, dhs.seg.urban$cluster)

  # Constructed additive wealth index - full
    ## Grow tree
    tree_urban <- rpart(cluster ~ pship.cat+ med.permis+asset.quint+ med.index+dv.index+ jdwd.index.cat+
                          partner.working.yn+religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+
                          highestyearsedinHH.yrs+hh.electricity+any.media.yn+anyed.yn+early.sex.15, 
                        data=dhs.seg.urban, control=rpart.control(cp=0.001, xval=5), method="class")
    
    plotcp(tree_urban); printcp(tree_urban)
    
    #prune tree
    tree_urban_prune<-prune(tree_urban, cp=tree_urban$cptable[which.min(tree_urban$cptable[,"xerror"]),"CP"])
    # Can try prunign using different error cutoffs
    # tree_urban_prune<-prune(tree_urban, cp=.005)
    plotcp(tree_urban_prune); printcp(tree_urban_prune)
    
    plot(tree_urban_prune, uniform=TRUE, main="Classification Tree for Urban LCA")
    text(tree_urban_prune, use.n=TRUE, all=TRUE, cex=.7)
    prp(tree_urban_prune, type=3, varlen=0, cex=1)
    # fancyRpartPlot(tree_urban_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)
    
    PredictCART_train_urban <- predict(tree_urban_prune, data=dhs.seg.urban, type="class")
    confusionMatrix(data=PredictCART_train_urban, dhs.seg.urban$cluster)
    
  # Constructed additive wealth index - reduced to 5 items
    ## Grow tree
    tree_urban <- rpart(cluster ~ pship.cat+ med.permis+asset.ind.alt.urban+ med.index+dv.index+ jdwd.index.cat+
                          partner.working.yn+religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+
                          highestyearsedinHH.yrs+hh.electricity+any.media.yn+anyed.yn+early.sex.15, 
                        data=dhs.seg.urban, control=rpart.control(cp=0.001, xval=5), method="class")
    
    plotcp(tree_urban); printcp(tree_urban)
    
    #prune tree
    tree_urban_prune<-prune(tree_urban, cp=tree_urban$cptable[which.min(tree_urban$cptable[,"xerror"]),"CP"])
    # Can try prunign using different error cutoffs
    #tree_urban_prune<-prune(tree_urban, cp=.002)
    plotcp(tree_urban_prune); printcp(tree_urban_prune)
    
    plot(tree_urban_prune, uniform=TRUE, main="Classification Tree for Urban LCA")
    text(tree_urban_prune, use.n=TRUE, all=TRUE, cex=.7)
    prp(tree_urban_prune, type=5, varlen=0, cex=1)
    # fancyRpartPlot(tree_urban_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)
    
    PredictCART_train_urban <- predict(tree_urban_prune, data=dhs.seg.urban, type="class")
    confusionMatrix(data=PredictCART_train_urban, dhs.seg.urban$cluster)
  
    
# RURAL
  # Original wealth index
    tree_rural <- rpart(cluster ~ pship.cat+med.permis+wealth.index.ur.ord+med.index+dv.index+jdwd.index.cat+partner.working.yn+
                          religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+highestyearsedinHH.yrs+
                          hh.electricity+any.media.yn+anyed.yn+early.sex.15+pastoralnoagro, 
                        data=dhs.seg.rural, control=rpart.control(cp=0.001, xval=5), method="class")
    plotcp(tree_rural); printcp(tree_rural)
    summary(tree_rural)

    #prune tree
    #tree_rural_prune<-prune(tree_rural, cp=tree_rural$cptable[which.min(tree_rural$cptable[,"xerror"]),"CP"])
      # Alternative prune - used
      tree_rural_prune<-prune(tree_rural, cp=.002)
      plotcp(tree_rural_prune); printcp(tree_rural_prune)
      
    plot(tree_rural_prune, uniform=TRUE, main="Classification Tree for Urban LCA")
    text(tree_rural_prune, use.n=TRUE, all=TRUE, cex=.7)
    prp(tree_rural_prune, type=3, varlen=0, cex=.9)
    # fancyRpartPlot(tree_rural_prune, type = 3, tweak = 1.5, caption = NULL)

    PredictCART_train_rural <- predict(tree_rural_prune, data=dhs.seg.rural, type="class")
    confusionMatrix(data=PredictCART_train_rural, dhs.seg.rural$cluster)
    
  # Alternate wealth index from summative assets - full
    tree_rural <- rpart(cluster ~ pship.cat+med.permis+asset.quint+med.index+dv.index+jdwd.index.cat+partner.working.yn+
                          religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+highestyearsedinHH.yrs+
                          hh.electricity+any.media.yn+anyed.yn+early.sex.15+pastoralnoagro, 
                        data=dhs.seg.rural, control=rpart.control(cp=0.001, xval=5), method="class")
    plotcp(tree_rural); printcp(tree_rural)
    summary(tree_rural)
    
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

 
    # Alternate wealth index from summative assets - top 5
    tree_rural <- rpart(cluster ~ pship.cat+med.permis+asset.ind.alt.rural+med.index+dv.index+jdwd.index.cat+partner.working.yn+
                          religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+highestyearsedinHH.yrs+
                          hh.electricity+any.media.yn+anyed.yn+early.sex.15+pastoralnoagro, 
                        data=dhs.seg.rural, control=rpart.control(cp=0.001, xval=5), method="class")
    plotcp(tree_rural); printcp(tree_rural)
    summary(tree_rural)
    
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
    
    
  # Alternate wealth index from summative assets (top 5) AND goats instead of pastoralism
    tree_rural <- rpart(cluster ~ pship.cat+med.permis+asset.ind.alt+med.index+dv.index+jdwd.index.cat+partner.working.yn+
                          religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+highestyearsedinHH.yrs+
                          hh.electricity+any.media.yn+anyed.yn+early.sex.15+asset.goats, 
                        data=dhs.seg.rural, control=rpart.control(cp=0.001, xval=5), method="class")
    plotcp(tree_rural); printcp(tree_rural)
    summary(tree_rural)
    
    #prune tree
    #tree_rural_prune<-prune(tree_rural, cp=tree_rural$cptable[which.min(tree_rural$cptable[,"xerror"]),"CP"])
    # Alternative prune - used
    tree_rural_prune<-prune(tree_rural, cp=.002)
    plotcp(tree_rural_prune); printcp(tree_rural_prune)
    
    plot(tree_rural_prune, uniform=TRUE, main="Classification Tree for Urban LCA")
    text(tree_rural_prune, use.n=TRUE, all=TRUE, cex=.7)
    prp(tree_rural_prune, type=3, varlen=0, cex=.9)
    # fancyRpartPlot(tree_rural_prune, type = 3, tweak = 1.5, caption = NULL)
    
    PredictCART_train_rural <- predict(tree_rural_prune, data=dhs.seg.rural, type="class")
    confusionMatrix(data=PredictCART_train_rural, dhs.seg.rural$cluster)
    
    
# #Random Forest
# library(randomForest)
# library(randomForestExplainer)
# urban_fit <- randomForest(cluster_k5_rename ~ pship.cat+ med.permis+wealth.index.ur.ord+ med.index+dv.index+ jdwd.index.cat+
#                             partner.working.yn+religionrecode.cat+ work.seasonal+num.15up.4plus+conflict_incidents+num.kids.house.4plus+
#                             highestyearsedinHH.yrs+hh.electricity+any.media.yn+anyed.yn+early.sex.15, 
#                           data=dhs.seg.urban, localImp=T)
# print(urban_fit) # view results
# importance<-measure_importance(urban_fit) # importance of each predictor
# plot_min_depth_distribution(urban_fit, mean_sample = "relevant_trees", k=15)
# plot_multi_way_importance(importance, size_measure = "no_of_nodes")
# plot_min_depth_interactions(interactions_fram)


