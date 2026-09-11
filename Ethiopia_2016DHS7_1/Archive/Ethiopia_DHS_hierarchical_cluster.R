### Hierarchical clustering Ethiopia 2016 DHS ####
### Prepared by: Anu Mishra - 1/5/23          ####


library(MASS)
library(cluster)
library(tidyverse)
library(fpc)
library(Rtsne)
library(readxl)
library(ggplot2)
library(pals)

## Functions: Heatmap of vulnerabilities amongst clusters
heatmap <- function(data,tree,k,title=''){ 
  #Note: tree is the hierchical clustering solution, but you could simplify this code by probably just reading in a dataset with 
  #      cluster assignments merged in
  
  
  ## dataset with vulnerabilities and no cluster assignment
  clust_dat <- data %>% mutate(cluster=factor(cutree(tree,k=k))) %>%   #adding cluster assignment
    mutate(dv.index.yn=ifelse(dv.index==0,1,2)) %>% select(-dv.index) %>%  #recoding text/non-binary variables into dummy variables
    mutate(head.sex=ifelse(head.sex=='female',2,1)) %>%
    mutate_if(is.factor, as.numeric)                                    #converting to numeric so can calculate proportion easily
  
  clust_dat[,!names(clust_dat) %in% 'cluster'] <- clust_dat[,!names(clust_dat) %in% 'cluster']-1   #recoding from 1/2 to 0/1
  
  
  ## calculating proportion of outcome among esta cluster  
  prop_dat <- aggregate(clust_dat[,!names(clust_dat) %in% 'cluster'],by=list(clust_dat$cluster),FUN=mean)
  names(prop_dat)[1] <- 'cluster'                                    ## renaming columns
  prop_dat <- as.data.frame(t(prop_dat))                             ## transposing so that clusters are columns
  prop_dat$var <- row.names(prop_dat)
  names(prop_dat) <- c(paste0('Cluster',1:k),'var.name')
  prop_dat <- prop_dat[prop_dat$var!='cluster',]
  
  ## order by construct domain (not really necessary)
  prop_dat <- merge(prop_dat,construct.mapping[,c('category','var.name')],by='var.name',all.x = T)
  prop_dat$category[is.na(prop_dat$category) | prop_dat$category=='Healthcare and mental models' ] <- 'Misc'
  prop_dat$category <- factor(prop_dat$category,levels=c('Woman and her past experiences','Household relationships','Household economics', 'Larger environment','Misc'))
  prop_dat <- prop_dat[order(prop_dat$category),]
  
  ## convert to long format so there is a cluster variable which works well for heatmap code
  prop_dat_long <- prop_dat %>% gather(cluster,prop,-category,-var.name)
  prop_dat_long <- prop_dat_long[order(prop_dat_long$category),]
  
  
  ## plot heatmat by construct
  p <- ggplot(prop_dat_long, aes(cluster, var.name, fill= prop)) + 
    facet_wrap(~category,nrow=1,scales="free_y") +        #seperates out construct
    geom_tile() +                                         #heatmat plot
    scale_fill_gradient(low="peachpuff", high="orangered") +  ## color for min/max
    ggtitle(title) +xlab('') +ylab('') +
    theme(axis.text.x = element_text(angle = 90))

  
  return(p)
}


### read in data
setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/")
dhs.seg <- readRDS("Data/Ethiopia DHS/ET_2016/dhs.seg.rds")
dhs.seg$jd.index.5plus[dhs.seg$marr.cohab==0] <- 0
construct.mapping <- read_xlsx("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/Construct mapping.xlsx")
construct.mapping <- construct.mapping[!duplicated(construct.mapping[,'var.name']),]

set.seed(141029)



############# Rural ###########
rural <- dhs.seg %>% subset(hh.urban==0) %>%
        select(ever.partnered.yn,early.cohab.15,early.sex.15,preg.4plus,child.4plus,
               jd.index.5plus,husb.ed.mtprim,part.nochores,med.permis,
               polygamy,head.sex,num.15up.4plus,num.under15.3plus,hh.kidwom.rat.2plus,
               working.yn,hh.electricity,hh.land,wealth.index.poor,
               hh.rooms.2plus, hh.cook.inside.yn,
               toilet.facility,hh.wat.time.yn,hh.sanitation.yn,
               med.index.3plus,preg.4plus,any.media.yn,anyed.yn,orthodox,
               muslim,early.cohab.15,dv.index)

apply(rural,2,function(x){table(is.na(x))})
apply(rural,2,table)

rural <- rural[complete.cases(rural),]
rural <- rural %>%mutate_if(is.numeric, as.factor)


#calculate distance metric
gower.dist.rural <- daisy(rural, metric = c("gower"))
#write.table(as.matrix(gower.dist.rural),
#            'C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/Desktop/dissimilarity_mtrx.csv', 
#            row.names=FALSE, 
#            col.names=FALSE, 
#            sep=",")


#complete linkage clustering (dendrogram) 
rur.clust.comp <- hclust(gower.dist.rural, method = "complete")
plot(rur.clust.comp,labels = NULL,main='Rural (complete linkage)')

#single linkage
#rur.clust.single <- hclust(gower.dist.rural, method = "single")
#plot(rur.clust.single,labels = NULL)


## Sillhoute plot  
avg.sil.rur <- NULL
for(i in 2:10){
  print(i)
  avg.sil.rur <- c(avg.sil.rur,cluster.stats(d = gower.dist.rural,clustering = cutree(rur.clust.comp,k=i))$avg.silwidth)
  table(cutree(rur.clust.comp,k=i))
}
plot(2:10,avg.sil.rur,ylab='Average Sil Width',xlab='Num clusters',main='Rural (complete linkage)',type='o')
## suggests 2 or 4 clusters

## Visualizing clusters in 2D
rural_tsne <- Rtsne(gower.dist.rural, is_distance = TRUE)

# 2 clusters
rural_tsne_clust2 <- rural_tsne$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(cutree(rur.clust.comp,k=2)))
ggplot(aes(x = X, y = Y), data = rural_tsne_clust2) +
  geom_point(aes(color = cluster))
heatmap(data = rural,tree = rur.clust.comp,k = 2)

# 3 clusters
rural_tsne_clust3 <- rural_tsne$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(cutree(rur.clust.comp,k=3)))
ggplot(aes(x = X, y = Y), data = rural_tsne_clust3) +
  geom_point(aes(color = cluster))
heatmap(data = rural,tree = rur.clust.comp,k = 3,title = 'Rural')

# 4 clusters
rural_tsne_clust4 <- rural_tsne$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(cutree(rur.clust.comp,k=4)))
ggplot(aes(x = X, y = Y), data = rural_tsne_clust4) +
  geom_point(aes(color = cluster))
heatmap(data = rural,tree = rur.clust.comp,k = 4)





############# Urban ###########
urban <- dhs.seg %>% subset(hh.urban==1) %>%
  select(ever.partnered.yn,early.cohab.15,early.sex.15,preg.4plus,child.4plus,
         jd.index.5plus,husb.ed.mtprim,part.nochores,med.permis,
         polygamy,head.sex,num.15up.4plus,num.under15.3plus,hh.kidwom.rat.2plus,
         work.type.prof,working.yn,hh.electricity,hh.land,hh.bank.acct,wealth.index.poor,
         hh.rooms.2plus,
         toilet.facility,hh.wat.time.yn,hh.sanitation.yn,
         med.index.3plus,preg.4plus,any.media.yn,anyed.yn,orthodox,
         muslim,early.cohab.15,dv.index)

apply(urban,2,function(x){table(is.na(x))})
apply(urban,2,table)

urban <- urban[complete.cases(urban),]
urban <- urban %>%mutate_if(is.numeric, as.factor)


#calculate distance metric
gower.dist.urban <- daisy(urban, metric = c("gower"))


#complete linkage clustering (dendrogram) 
urb.clust.comp <- hclust(gower.dist.urban, method = "complete")
plot(urb.clust.comp,labels = NULL,main='Urban (complete linkage)')

#single linkage
#urb.clust.single <- hclust(gower.dist.urban, method = "single")
#plot(urb.clust.single,labels = NULL)


## Sillhoute plot  
avg.sil.urb <- NULL
for(i in 2:10){
  print(i)
  avg.sil.urb <- c(avg.sil.urb,cluster.stats(d = gower.dist.urban,clustering = cutree(urb.clust.comp,k=i))$avg.silwidth)
  table(cutree(rur.clust.comp,k=i))
}
plot(2:10,avg.sil.urb,ylab='Average Sil Width',xlab='Num clusters',main='Urban (complete linkage)',type='o')
## suggests 2 or 3 clusters

## Visualizing clusters in 2D
urban_tsne <- Rtsne(gower.dist.urban, is_distance = TRUE)

# 2 clusters
urban_tsne_clust2 <- urban_tsne$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(cutree(urb.clust.comp,k=2)))
ggplot(aes(x = X, y = Y), data = urban_tsne_clust2) +
  geom_point(aes(color = cluster))
heatmap(data = urban,tree = urb.clust.comp,k = 2)

# 3 clusters
urban_tsne_clust3 <- urban_tsne$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(cutree(urb.clust.comp,k=3)))
ggplot(aes(x = X, y = Y), data = urban_tsne_clust3) +
  geom_point(aes(color = cluster))
heatmap(data = urban,tree = urb.clust.comp,k = 3)


# 4 clusters
urban_tsne_clust4 <- urban_tsne$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(cutree(urb.clust.comp,k=4)))
ggplot(aes(x = X, y = Y), data = urban_tsne_clust4) +
  geom_point(aes(color = cluster))
heatmap(data = urban,tree = urb.clust.comp,k = 4)

