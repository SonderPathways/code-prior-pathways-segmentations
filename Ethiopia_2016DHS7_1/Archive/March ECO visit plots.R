rm(list=ls())
library(tidyverse)
setwd('C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/Ethiopia Segmentation_v2/LCA/')


### Urban ####
load("lca_polca_urban_wlabelres.RData")
train_df_urban$cluster_k4_rename_short[train_df_urban$cluster_k4_rename_short=='U1'] <- 'UU2'
train_df_urban$cluster_k4_rename_short[train_df_urban$cluster_k4_rename_short=='U2'] <- 'UU1'
train_df_urban$cluster_k4_rename_short[train_df_urban$cluster_k4_rename_short=='U3'] <- 'UP2'
train_df_urban$cluster_k4_rename_short[train_df_urban$cluster_k4_rename_short=='U4'] <- 'UP1'
prop.table(table(train_df_urban$cluster_k4_rename_short))
train_df_urban$cluster_k4_rename_short <- factor(train_df_urban$cluster_k4_rename_short,levels=c('UU1','UU2','UP1','UP2'))

train_df_urban$wealth.index.ur.ord <- train_df_urban$wealth.index.ur.ord -1

plot_dt <- train_df_urban[,c('wealth.index.ur.ord','cluster_k4_rename_short')]
plot_dt <- plot_dt %>% group_by(cluster_k4_rename_short,wealth.index.ur.ord) %>% summarize(ct = n()) %>% mutate(pct = round(ct / sum(ct), 2)) 
plot_dt$wealth.index.ur.ord <- as.factor(plot_dt$wealth.index.ur.ord)

p <-  ggplot(plot_dt, aes(fill=wealth.index.ur.ord, y=pct, x=cluster_k4_rename_short )) + 
  geom_bar(position="stack", stat="identity") + coord_cartesian(ylim=c(0,1)) +
  xlab('') + ylab('Percent (%)') + theme_bw() +
  scale_fill_brewer(palette = 'Blues') +  labs(fill='') + theme(axis.text= element_text(size=14)) + 
  ggtitle('Wealth Index') + theme(plot.title = element_text(size=16)) + 
  theme(axis.title.y = element_text(size=14)) + theme(legend.text = element_text(size = 20)) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank())


plot_dt <- train_df_urban[,c('pship.cat','cluster_k4_rename_short')]
plot_dt <- plot_dt %>% group_by(cluster_k4_rename_short,pship.cat) %>% summarize(ct = n()) %>% mutate(pct = round(ct / sum(ct), 2)) 
plot_dt$pship.cat <- factor(plot_dt$pship.cat,levels=c('never','no partner now','married/cohab'),labels=c('Never','No partner now','Married/cohab'))

p <-  ggplot(plot_dt, aes(fill=pship.cat, y=pct, x=cluster_k4_rename_short )) + 
  geom_bar(position="stack", stat="identity") + coord_cartesian(ylim=c(0,1)) +
  xlab('') + ylab('Percent (%)') +  theme_bw() +
  scale_fill_brewer(palette = 'Pastel2') +  labs(fill='') + 
  ggtitle('Partnership Category') + theme(plot.title = element_text(size=16)) + 
  theme(axis.title.y = element_text(size=14)) + theme(legend.text = element_text(size = 20)) + 
  theme(axis.text= element_text(size=14)) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank())


### Rural ####
load("lca_polca_rural_res_wcluster.Rdata")
#order clusters
train_df_rural$cluster_k6_rename_short[train_df_rural$cluster_k6=='R2'] <- 'RU3'
train_df_rural$cluster_k6_rename_short[train_df_rural$cluster_k6=='R6'] <- 'RU1'
train_df_rural$cluster_k6_rename_short[train_df_rural$cluster_k6=='R1'] <- 'RU2'

train_df_rural$cluster_k6_rename_short[train_df_rural$cluster_k6=='R5'] <- 'RP3'
train_df_rural$cluster_k6_rename_short[train_df_rural$cluster_k6=='R4'] <- 'RP1'
train_df_rural$cluster_k6_rename_short[train_df_rural$cluster_k6=='R3'] <- 'RP2'
prop.table(table(train_df_rural$cluster_k6_rename_short))
train_df_rural$cluster_k6_rename_short <- factor(train_df_rural$cluster_k6_rename_short,levels=c('RU1','RU2','RU3','RP1','RP2','RP3'))


plot_dt <- train_df_rural[,c('pship.cat','cluster_k6_rename_short')]
plot_dt <- plot_dt %>% group_by(cluster_k6_rename_short,pship.cat) %>% summarize(ct = n()) %>% mutate(pct = round(ct / sum(ct), 2)) 
plot_dt$pship.cat <- factor(plot_dt$pship.cat,levels=c('never','no partner now','married/cohab'),labels=c('Never','No partner now','Married/Cohab'))

p <-  ggplot(plot_dt, aes(fill=pship.cat, y=pct, x=cluster_k6_rename_short )) + 
  geom_bar(position="stack", stat="identity") + coord_cartesian(ylim=c(0,1)) +
  xlab('') + ylab('Percent (%)') +  theme_bw() +
  scale_fill_brewer(palette = 'Pastel2') +  labs(fill='') +
  ggtitle('Partnership Category') + theme(plot.title = element_text(size=16)) + 
  theme(axis.title.y = element_text(size=14)) + theme(legend.text = element_text(size = 20)) +
  theme(axis.text= element_text(size=14)) 


plot_dt <- train_df_rural[,c('work.seasonal','cluster_k6_rename_short')]
plot_dt$work.seasonal <- factor(plot_dt$work.seasonal,levels=c('No work in the past 12 months','Occassional',
                                                           'Seasonal','All  year'),
                            labels=c('No work in the\npast 12 months','Occassional','Seasonal','All  year'))

plot_dt <- plot_dt %>% group_by(cluster_k6_rename_short,work.seasonal) %>% summarize(ct = n()) %>% mutate(pct = round(ct / sum(ct), 2)) 

p <-  ggplot(plot_dt, aes(fill=work.seasonal, y=pct, x=cluster_k6_rename_short )) + 
  geom_bar(position="stack", stat="identity") + coord_cartesian(ylim=c(0,1)) +
  xlab('') + ylab('Percent (%)') + theme_bw() +
  scale_fill_brewer(palette = 'Pastel1') +  labs(fill='') +
  ggtitle('Work category')+ theme(plot.title = element_text(size=16)) + 
  theme(axis.title.y = element_text(size=14)) + theme(legend.text = element_text(size = 20)) +
  theme(axis.text= element_text(size=14)) 



  


plot_dt <- train_df_rural[,c('wealth.index.ur.ord','cluster_k6_rename_short')]
plot_dt$wealth.index.ur.ord <- plot_dt$wealth.index.ur.ord -1
plot_dt <- plot_dt %>% group_by(cluster_k6_rename_short,wealth.index.ur.ord) %>% summarize(ct = n()) %>% mutate(pct = round(ct / sum(ct), 2)) 
plot_dt$wealth.index.ur.ord <- as.factor(plot_dt$wealth.index.ur.ord)

p <-  ggplot(plot_dt, aes(fill=wealth.index.ur.ord, y=pct, x=cluster_k6_rename_short )) + 
  geom_bar(position="stack", stat="identity") + coord_cartesian(ylim=c(0,1)) +
  xlab('') + ylab('Percent (%)') + theme_bw() +
  scale_fill_brewer(palette = 'Blues') +  labs(fill='') +
  ggtitle('Wealth Index') + theme(plot.title = element_text(size=16)) + 
  theme(axis.title.y = element_text(size=14)) + theme(legend.text = element_text(size = 20)) +
  theme(axis.text= element_text(size=14)) 

