############## 
# 9b_Ethiopia_DHS_Pamclusters
# Purpose: conduct PAM clustering algorithm
# Ethiopia DHS 2016, Recode 7
# Using variables selected based on qualitative insights, univariate analyses, and pca 
# input data: 5_Ethiopia_DHS_exploratory.rds
# input scripts: 10b_Ethiopia_DHS_cluster_plots.R
# ancestor code: 8_
# note: each user needs to set the directory
### Note: this script requires changing variables by hand and rerunning code. Ideally this could be put in a loop at some point ######

############## 
############## 

rm(list=ls())
pacman::p_load(cluster, dplyr, ggplot2, readr, Rtsne, NbClust, sf)
require(graphics); require(grDevices)

############## Set directory and load data - change directory for a different user
#setwd("/Users/darcyrao/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Pathways - GE Data & Insights") # for use from my Mac
#setwd("C:/Users/darcyra/OneDrive - Bill & Melinda Gates Foundation/Pathways - GE Data & Insights")

## Import the vulnerability measures merged with outcomes - only do the first run through
dhs.seg <- readRDS("5_Ethiopia_DHS_exploratory.rds")

method.poLCA=FALSE ## Need this because of Anu's code for LCA
source("10b_Ethiopia_DHS_cluster_plots.R")

############## Select subset of data to work with [AG edit 3/7/23] ##########
## getting variables for urban and rural samples and getting rid of any N/A

  #lca analysis using this, and also num.15up.4plus
  #dhs.seg$num.kids.house.4plus <- ifelse(dhs.seg$num.kids.house.cat=="4 or more",1,0)

## only do once then use the saved data so as to coordinate with other test by AM
  #cut: hh.bank.acct, early.birth.15, polygamy, hh.animal, hh.mototransport.yn, water, toilet.loc, hh.watpur, male.child.pref, slum1 #earlysex?
dhs.seg.urban <- dhs.seg %>% 
  filter(hh.urban == 1) %>%
  select(CLUSTERNO, HHNUM, LINENO, med.index, working.yn, partner.working.yn, hh.electricity, 
         work.seasonal, wealth.index.ur.ord, med.permis, jdwd.index.cat, highestyearsedinHH.yrs, 
         partner.absent.yn, pship.cat, num.kids.house, num.15up, 
         any.media.yn, anyed.yn, religionrecode.cat, early.sex.15,
        dv.index, conflict_incidents, age.adj
        #hh.bank.acct, early.birth.15, polygamy, hh.animal, hh.motortransport.yn, water, toilet.loc, hh.watpur, male.child.pref, slum1
        )
dhs.seg.urban <- dhs.seg.urban[complete.cases(dhs.seg.urban),]


#In rural, these didn’t vary much by cluster: 
#early.birth (2-6%), hh.animal (88-91%), 
#num.15.up (mean 2.6-3. has high variance but 3rd quintile was 3-4 across clusters), 
#hh.motor transport (0-2%), hh.watpur (6-7%), male.child.pref (22-24%). 
#partner.absent.yn, polygamy, early.birth.15, hh.animal, num.15up, hh.motortransport.yn,hh.watpur.appr, male.child.pref, hh.bank.acct, toilet.loc, hh.animal

#to add in: age.adj, age.yrs. also vary: agropastoral.yn

dhs.seg.rural <- dhs.seg %>% 
  filter(hh.urban == 0) %>%
  select(CLUSTERNO, HHNUM, LINENO, med.index, working.yn, partner.working.yn, 
         work.seasonal, wealth.index.ur.ord, med.permis, jdwd.index.cat, highestyearsedinHH.yrs, 
          pship.cat, num.kids.house, water, num.15up,
         hh.electricity, any.media.yn, anyed.yn, religionrecode.cat, early.sex.15,
          dv.index, conflict_incidents, pastoralnoagro, age.adj)
dhs.seg.rural <- dhs.seg.rural[complete.cases(dhs.seg.rural),]

#no longer doing training vs testing samples, but will still use the name to avoid redoing all the code below

train_df_urban <- dhs.seg.urban
train_df_urban <- dhs.seg.urban %>% select(-c("CLUSTERNO", "HHNUM", "LINENO"))

train_df_rural <- dhs.seg.rural
train_df_rural <- train_df_rural %>% select(-c("CLUSTERNO", "HHNUM", "LINENO"))


#################### changing data types ##############################
#would be better to do this in a loop, or better yet, in the vulnerability script to be carried forward to here

## changing character to factor variables
train_df_urban$work.seasonal <- as.factor(train_df_urban$work.seasonal)
train_df_urban$pship.cat <- as.factor(train_df_urban$pship.cat)
train_df_urban$religionrecode.cat <- as.factor(train_df_urban$religionrecode.cat)
train_df_urban$jdwd.index.cat <- as.factor(train_df_urban$jdwd.index.cat)
#train_df_urban$slum1 <- as.factor(train_df_urban$slum1)
#train_df_urban$toilet.loc <- as.factor(train_df_urban$toilet.loc)

train_df_rural$work.seasonal <- as.factor(train_df_rural$work.seasonal)
train_df_rural$pship.cat <- as.factor(train_df_rural$pship.cat)
train_df_rural$religionrecode.cat <- as.factor(train_df_rural$religionrecode.cat)
train_df_rural$jdwd.index.cat <- as.factor(train_df_rural$jdwd.index.cat)
#train_df_rural$toilet.loc <- as.factor(train_df_rural$toilet.loc)

##setting ordinal variables
  #note: not setting conflict_incidents as ordered since in theory it can range continuously

###
train_df_urban$wealth.index.ur.ord <- as.ordered(train_df_urban$wealth.index.ur.ord)  
#(minor note: for consistency would've been better if wealth.index.ur.ord wasn't named with .ord prior to being set as ordinal...)
train_df_urban$med.index.ord <- as.ordered(train_df_urban$med.index)  
train_df_urban$dv.index.ord <- as.ordered(train_df_urban$dv.index)  
#dropping non-ordinal versions
train_df_urban <- train_df_urban[,!names(train_df_urban) %in% c("dv.index", "med.index")]

###
  #note: not setting conflict_incidents as ordered since in theory it can range continuously
###
train_df_rural$wealth.index.ur.ord <- as.ordered(train_df_rural$wealth.index.ur.ord)  
train_df_rural$med.index.ord <- as.ordered(train_df_rural$med.index)  
train_df_rural$dv.index.ord <- as.ordered(train_df_rural$dv.index)  
#dropping non-ordinal versions
train_df_rural <- train_df_rural[,!names(train_df_rural) %in% c("dv.index", "med.index")]

############## Select the variables of interest

# Check for missingness - none
#gg_miss_upset(train_df_urban, nsets = n_var_miss(train_df_urban))
#gg_miss_upset(train_df_rural, nsets = n_var_miss(train_df_rural))

############## Define clusters - urban

## Calculate distance
#Although gower can take character variables, daisy cannot. Made them factors above.
#Automatically uses gower's defaults for standardization. If we specify weights, it will use them instead.
#logging these continuous variables to handle non-normality - num.kids.house and num.15up and conflict_incidents 
#Although highestyearsedinHH.yrs didn't pass SW test, histogram doesn't look badly skewed so will keep the way it is. Doesn't make a difference to results
  which( colnames(train_df_urban)=="conflict_incidents" )
  which( colnames(train_df_urban)=="num.kids.house" )
  which( colnames(train_df_urban)=="num.15up" )
  
gdist_urban <- daisy(train_df_urban, metric = "gower", type = list(logratio = c(17,12,11)))
  ##Warning messages state which columsn are asymmetric binary and which interval scaled
gdist_urban_mat <- as.matrix(gdist_urban)

#save(train_df_urban, file = "9_Ethiopia_DHS_train_df_urban_wage.RData")

############# Urban training data ####################################
  # Print most similar
#train_df_urban[which(gdist_urban_mat == min(gdist_urban_mat[gdist_urban_mat != min(gdist_urban_mat)]), arr.ind = TRUE)[1,],]
  # Print most dissimilar
#train_df_urban[which(gdist_urban_mat == max(gdist_urban_mat[gdist_urban_mat != max(gdist_urban_mat)]), arr.ind = TRUE)[1,],]

## Look at silhouette plots

sil_width_u <- c(NA)
for(i in 2:10){
  pam_fit_urban <- pam(gdist_urban, diss = TRUE, k = i)
  sil_width_u[i] <- pam_fit_urban$silinfo$avg.width
}

#With 2 clusters, the first is pretty low at 0.12 ish average silhouette width, 2nd close to 0.3
#With 4 clusters widths are: 0.258, 0.297, 0.12, 0.244

plot(1:10, sil_width_u,
     xlab = "Number of clusters",
     ylab = "Silhouette width")
lines(1:10, sil_width_u)

# anything under .25 is likely to be
#false, above .25 may be artifice
#https://stats.stackexchange.com/questions/45232/do-low-silhouette-widths-mean-the-data-has-little-underlying-structure

## Look at results with r clusters
ku <- 5 #selected four
pam_fit_urban <- pam(gdist_urban, diss = TRUE, ku)
pam_results_urban <- train_df_urban %>%
  mutate(cluster = pam_fit_urban$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results_urban$the_summary

## Plot in lower dimensional state
tsne_obj_u <- Rtsne(gdist_urban, is_distance = TRUE)
tsne_data_u <- tsne_obj_u$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit_urban$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data_u) +
  geom_point(aes(color = cluster))
 pdf("urban_tsne_k2_noage.pdf",width = 8,height=8)

train_df_urban$cluster<-pam_fit_urban$clustering # attach it back to the training dataset
train_df_urban$cluster<-paste0('U',train_df_urban$cluster)

#merge back to dhs.seg
#train_df_urban$CLUSTERNO<-dhs.seg.urban$CLUSTERNO ##attach caseid's back to dataset for merge, DHS cluster, NOT clustering result
#train_df_urban$LINENO<-dhs.seg.urban$LINENO
#train_df_urban$HHNUM<-dhs.seg.urban$HHNUM

#dhs.seg <-merge(dhs.seg, train_df_urban[,c("CLUSTERNO", "HHNUM", "LINENO", "cluster")], by=c("CLUSTERNO", "HHNUM", "LINENO"), all.x=T)
#dhs.seg %>% select(c("cluster"))  %>% View


### Descriptive plots of clusters
## binary variables
bin.train.urb <- heatmap.bin(dt=train_df_urban,urban = TRUE)
#pdf("urban_heat_k3_noage.pdf",width = 12,height=8)
plot(bin.train.urb)
#dev.off()

##categorical
cat.train.urb <- stacked.cat(dt=train_df_urban)
plot(cat.train.urb)
#pdf("urban_stacked_k3_noage.pdf",width = 12,height=8)
#dev.off()

#continuous
con.train.urb <- box.cont(dt=train_df_urban)
#pdf("Ethiopia Segmentation_v2/urban_train_box.pdf",width = 12,height=8)
plot(con.train.urb)
#dev.off()

#loop through the 'Look at results with r clusters' code, rename the clusters and save to dataset
train_df_urban$cluster_k2<-train_df_urban$cluster
#next run:
train_df_urban$cluster_k3<-train_df_urban$cluster
#next run
train_df_urban$cluster_k4<-train_df_urban$cluster
#next run:
train_df_urban$cluster_k5<-train_df_urban$cluster

#save(train_df_urban, file = "9_Ethiopia_DHS_train_df_urban_wclusters.RData")
load("9_Ethiopia_DHS_train_df_urban_wclusters.RData")


#ordering may change every time clustering is rerun
train_df_urban<- train_df_urban %>% mutate(cluster_k2 = case_when
                   (
                     cluster_k2 == "U1" ~ "U1: mostly married/cohab",
                     cluster_k2 == "U2" ~ "U2: mostly not partnered"
                   )
)

train_df_urban<- train_df_urban %>% mutate(cluster_k3 = case_when
                                           (
                                             cluster_k3 == "U1" ~ "U1: mostly married/cohab",
                                             cluster_k3 == "U2" ~ "U2: mostly not partnered, not working",
                                             cluster_k3 == "U3" ~ "U3: mostly not partnered, working"
                                           )
)

train_df_urban<- train_df_urban %>% mutate(cluster_k4 = case_when
                                           (
                                             cluster_k4 == "U3" ~ "U3: mostly not partnered, not working",
                                             cluster_k4 == "U2" ~ "U2: mostly married/cohab, not working",
                                             cluster_k4 == "U1" ~ "U1: mostly married/cohab, working",
                                             cluster_k4 == "U4" ~ "U4: mostly not partnered, working"
                                           )
)

train_df_urban<- train_df_urban %>% mutate(cluster_k5 = case_when
                                           (
                                             cluster_k5 == "U3" ~ "U3: mostly not partnered, not working",
                                             cluster_k5 == "U2" ~ "U2: mostly married/cohab, not working, very poor",
                                             cluster_k5 == "U1" ~ "U1: mostly married/cohab, working",
                                             cluster_k5 == "U4" ~ "U4: mostly not partnered, working",
                                             cluster_k5 == "U5" ~ "U5: mostly married/cohab, not working, less poor"
                                           )
)

sankey_plot <- train_df_urban
sankey_plot$id <- 1:nrow(train_df_urban)
sankey_plot <- sankey_plot %>% dplyr::select(id, cluster_k2, cluster_k3, cluster_k4)
sankey_plot <- sankey_plot %>% rename(
                              
                                      'model_k4'='cluster_k4',
                                      'model_k3'='cluster_k3',
                                      'model_k2'='cluster_k2'
                                      )


mat <- as.data.frame(table(sankey_plot$model_k2, sankey_plot$model_k3, sankey_plot$model_k4))
require(ggplot2)
require(ggalluvial)
require(ggrepel)

p <- ggplot(mat, aes(axis1 = Var1, axis2 = Var2, axis3=Var3, #comment out if using two axes only
                     y = Freq)) +
  geom_alluvium(aes(fill = Var3)) + 
  geom_stratum() +
  geom_label(stat = "stratum",
             aes(label = after_stat(stratum))) +
  scale_fill_brewer(palette = "Set2") + 
  theme_bw() + theme(legend.title = element_blank())
scale_x_continuous('Model', breaks=c(2,3,4), labels=c('Model k=2', 'Model k=3','Model k=4')) 

#pdf(paste0('PCA_urban_train_sankey.pdf'),width=12,height=8)
print(p)
#dev.off()


#addmargins(table(train_df_urban$cluster_k4, train_df_urban$cluster_k5, dnn = c("clusterk4", "clusterk5")))
  #shows that splitting into 5 clusters results in a pretty small cluster (U2 here)
#What was U2 splits into U2 and U5
#    U1   U2   U3   U4   U5  Sum
#U1  1231   55    0    2   32 1320
#U2    53  372    8    0  699 1132
#U3     0   32 1094    0   31 1157
#U4     0   10    0 1709    0 1719
#Sum 1284  469 1102 1711  762 5328
#469/5328=8% of obs in cluster u2
#762/5328 =14% of obs in cluster u5



#indices
#suggests 2 clusters 
nb = NbClust(data=NULL, diss=gdist_urban,
             min.nc=2, max.nc=5, distance=NULL,
             method="median", index ="mcclain")
nb$Best.nc

#suggests 5 clusters
nb = NbClust(data=NULL, diss=gdist_urban,
             min.nc=2, max.nc=5, distance=NULL,
             method="median", index ="cindex")
nb$Best.nc

#suggests 2 clusters
nb = NbClust(data=NULL, diss=gdist_urban,
             min.nc=2, max.nc=5, distance=NULL,
             method="median", index ="dunn")
nb$Best.nc

#suggests 2 clusters
nb = NbClust(data=NULL, diss=gdist_urban,
             min.nc=2, max.nc=5, distance=NULL,
             method="median", index ="frey")
nb$Best.nc

#suggests 2 clusters
nb = NbClust(data=NULL, diss=gdist_urban,
             min.nc=2, max.nc=5, distance=NULL,
             method="median", index ="silhouette")
nb$Best.nc

save(train_df_urban, file = "9_Ethiopia_DHS_train_df_urban_wclusters.RData")
save(train_df_urban, file = "9_Ethiopia_DHS_train_df_urban_wclusterlabels_age.RData")



###################### RURAL TRAINING DATA ##############################


## Calculate distance
#Although gower can take character variables, daisy cannot. Made them factors above.
#Automatically uses gower's defaults for standardization. If we specify weights, it will use them instead.
#logging these continuous variables to handle non-normality - num.kids.house and num.15up and conflict_incidents 
#Although highestyearsedinHH.yrs didn't pass SW test, histogram doesn't look badly skewed so will keep the way it is. Doesn't make a difference to results
which( colnames(train_df_rural)=="conflict_incidents" )
which( colnames(train_df_rural)=="num.kids.house" )
which( colnames(train_df_rural)=="num.15up" )


gdist_rural <- daisy(train_df_rural, metric = "gower", type = list(logratio = c(17,9,11)))


##Warning messages state which columsn are asymmetric binary and which interval scaled
gdist_rural_mat <- as.matrix(gdist_rural)


# Print most similar
#train_df_rural[which(gdist_urban_mat == min(gdist_rural_mat[gdist_rural_mat != min(gdist_rural_mat)]), arr.ind = TRUE)[1,],]
# Print most dissimilar
#train_df_rural[which(gdist_urban_mat == max(gdist_rural_mat[gdist_rural_mat != max(gdist_rural_mat)]), arr.ind = TRUE)[1,],]

## Look at silhouette plots

sil_width_r <- c(NA)
for(i in 2:10){
  pam_fit_rural <- pam(gdist_rural, diss = TRUE, k = i)
  sil_width_r[i] <- pam_fit_rural$silinfo$avg.width
}

#With 2 clusters, the first is pretty low at 0.12 ish average silhouette width, 2nd close to 0.3
#With 4 clusters widths are: 0.258, 0.297, 0.12, 0.244

plot(1:10, sil_width_r,
     xlab = "Number of clusters",
     ylab = "Silhouette width")
lines(1:10, sil_width_r)

## Look at results with r clusters
kr <- 5
pam_fit_rural <- pam(gdist_rural, diss = TRUE, kr)
pam_results_rural <- train_df_rural %>%
  mutate(cluster = pam_fit_rural$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results_rural$the_summary

#pam_results_rural <- train_df_rural %>%
#  group_by(cluster_k6) %>%
#  do(the_summary = summary(.))
#pam_results_rural$the_summary

## Plot in lower dimensional state
tsne_obj_r <- Rtsne(gdist_rural, is_distance = TRUE)
tsne_data_r <- tsne_obj_r$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit_rural$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data_r) +
  geom_point(aes(color = cluster))
# pdf("Ethiopia Segmentation_v2/urban_train_tsne.pdf",width = 8,height=8)


train_df_rural$cluster<-pam_fit_rural$clustering # attach it back to the training dataset
train_df_rural$cluster<-paste0('R',train_df_rural$cluster)

### Descriptive plots of clusters
  #rerun for differnt k
## binary variables
bin.train.rur <- heatmap.bin(dt=train_df_rural,urban = FALSE)

##pdf("Ethiopia Segmentation_v2/urban_train_heat.pdf",width = 12,height=8)
plot(bin.train.rur)
##dev.off()

##categorical
cat.train.rur <- stacked.cat(dt=train_df_rural)
#pdf("Ethiopia Segmentation_v2/urban_train_stacked.pdf",width = 12,height=8)
plot(cat.train.rur)
#dev.off()

#continuous
con.train.rur <- box.cont(dt=train_df_rural)
#pdf("Ethiopia Segmentation_v2/urban_train_box.pdf",width = 12,height=8)
plot(con.train.rur)
#dev.off()


  #rerun above code with different clusters
#after running with k=6
train_df_rural <- train_df_rural %>% rename(cluster_k6=cluster)
#after running with k=5
train_df_rural <- train_df_rural %>% rename(cluster_k5=cluster)
#k=4
train_df_rural <- train_df_rural %>% rename(cluster_k4=cluster)
#k=3
train_df_rural <- train_df_rural %>% rename(cluster_k3=cluster)
#k=2
train_df_rural <- train_df_rural %>% rename(cluster_k2=cluster)

  #attach back for merging to dhs
#train_df_rural$CLUSTERNO<-dhs.seg.rural$CLUSTERNO #DHS cluster, NOT clustering result
#train_df_rural$LINENO<-dhs.seg.rural$LINENO
#train_df_rural$HHNUM<-dhs.seg.rural$HHNUM

#merge back to dhs.seg
#dhs.seg <-merge(dhs.seg, train_df_rural[,c("CLUSTERNO", "HHNUM", "LINENO", "cluster_k6")], by=c("CLUSTERNO", "HHNUM", "LINENO"), all.x=T)
#dhs.seg %>% select(c("cluster_k6"))  %>% View

save(train_df_rural, file = "9_Ethiopia_DHS_df_train_rural_wclusters_age.RData")
load("9_Ethiopia_DHS_df_train_rural_wclusters_age.RData")


#ordering may change every time clustering is rerun
train_df_rural<- train_df_rural %>% mutate(cluster_k2 = case_when
                                           (
                                             cluster_k2 == "R1" ~ "R1: mostly married/cohab",
                                             cluster_k2 == "R2" ~ "R2: mostly not partnered"
                                           )
)
#without age, there are 2 unmarried categories
train_df_rural<- train_df_rural %>% mutate(cluster_k3 = case_when
                                           (
                                             cluster_k3 == "R1" ~ "R1: mostly married/cohab",
                                             cluster_k3 == "R2" ~ "R2: mostly not partnered, not working",
                                             cluster_k3 == "R3" ~ "R3: mostly married/cohab, working" 
                                           )
)

train_df_rural<- train_df_rural %>% mutate(cluster_k4 = case_when
                                           (
                                             cluster_k4 == "R3" ~ "R3: mostly not partnered, not working",
                                             cluster_k4 == "R2" ~ "R2: mostly married/cohab, not working, lo med index",
                                             cluster_k4 == "R1" ~ "R1: mostly married/cohab, not working, hi med index",
                                             cluster_k4 == "R4" ~ "R4: mostly married/cohab, working"
                                           )
)

train_df_rural<- train_df_rural %>% mutate(cluster_k5 = case_when
                                           (
                                             cluster_k5 == "R3" ~ "R3: married/cohab, working",
                                             cluster_k5 == "R2" ~ "R1: married/cohab, not working, hi med index",
                                             cluster_k5 == "R1" ~ "R2: married/cohab, not working, low med index",
                                             cluster_k5 == "R4" ~ "R4: not partnered, not working",
                                             cluster_k5 == "R5" ~ "R5: not partnered, working"
                                           )
)

train_df_rural<- train_df_rural %>% mutate(cluster_k6 = case_when
                                           (
                                             cluster_k6 == "R3" ~ "R3: married/cohab, working, less poor",
                                             cluster_k6 == "R2" ~ "R2: married/cohab, not working, hi med index",
                                             cluster_k6 == "R1" ~ "R1: married/cohab, not working, low med index",
                                             cluster_k6 == "R4" ~ "R4: not partnered, not working",
                                             cluster_k6 == "R5" ~ "R5: married/cohab, working, poor",
                                             cluster_k6 == "R6" ~ "R6: not partnered, working"
                                           )
)


sankey_plot <- train_df_rural
sankey_plot$id <- 1:nrow(train_df_rural)
sankey_plot <- sankey_plot %>% dplyr::select(id, cluster_k2, cluster_k3, cluster_k4, cluster_k5)

mat <- as.data.frame(table(sankey_plot$cluster_k2, sankey_plot$cluster_k3, sankey_plot$cluster_k4, sankey_plot$cluster_k5))
require(ggplot2)
require(ggalluvial)
require(ggrepel)

p <- ggplot(mat, aes(axis1 = Var1, axis2 = Var2, axis3 = Var3, axis4=Var4,
                      #comment out if using two axes only
                     y = Freq)) +
  geom_alluvium(aes(fill = Var4)) + 
  geom_stratum() +
  geom_label(stat = "stratum",
             aes(label = after_stat(stratum))) +
  scale_fill_brewer(palette = "Set2") + 
  theme_bw() + theme(legend.title = element_blank())
scale_x_continuous('Model', breaks=c(1,2,3), labels=c('Model k=2','Model k=3','Model k=4')) 

#pdf(paste0('PCA_rural_train_sankey.pdf'),width=12,height=8)
print(p)
#dev.off()


#showing that  6 clusters is small and doesn't cleanly come out of another cluster
#addmargins(table(train_df_rural$cluster_k6, train_df_rural$cluster_k5, dnn = c("clusterk6", "clusterk5")))
# with age.yrs
#clusterk6   R1   R2   R3   R4   R5  Sum
#R1  1804   50   74    7    2 1937
#R2   180   65 1644   10   14 1913
#R3    25   33   33 1173  145 1409
#R4   153  332  473  189   75 1222
#R5    36 2010  153   14   30 2243
#R6    10   39   16  110  928 1103
#Sum 2208 2529 2393 1503 1194 9827
#     22%	26%	  24%	15%	  12%

#from 5 to 6, without age
##what was R3 splits into R3, R5 
#      R1   R2   R3   R4   R5  Sum
#R1  2227    0   55   29    0 2311
#R2     0 1936   63   20    0 2019
#R3    73  141 1550    7   44 1815
#R4    85   57   23 1072  157 1394
#R5   165   55  673   96   97 1086
#R6     0    0   14  124 1064 1202
#Sum 2550 2189 2378 1348 1362 9827

#addmargins(table(train_df_rural$cluster_k5, train_df_rural$cluster_k4, dnn = c("clusterk5", "clusterk4")))

#from 4 to 5, with age
#clusterk4
#clusterk5   R1   R2   R3   R4  Sum
#R1  1585  502   16  105 2208
#R2    94  209   54 2172 2529
#R3   574 1510  104  205 2393
#R4    81  145 1213   64 1503
#R5    54   69  859  212 1194
#Sum 2388 2435 2246 2758 9827

#from 4 to 5, without age
# less clean in R1, R2, more in R3
#.     R1   R2   R3   R4  Sum
#R1   336 1615  190  409 2550
#R2  1786  280   11  112 2189
#R3    97  397   26 1858 2378
#R4    51  158 1128   11 1348
#R5    54   24  835  449 1362
#Sum 2324 2474 2190 2839 9827

#suggests 2 clusters w/asymm and without
nb = NbClust(data=NULL, diss=gdist_rural,
             min.nc=2, max.nc=5, distance=NULL,
             method="median", index ="mcclain")
nb$Best.nc

#suggests 5 clusters
nb = NbClust(data=NULL, diss=gdist_rural,
             min.nc=2, max.nc=5, distance=NULL,
             method="median", index ="cindex")
nb$Best.nc

#suggests 2 clusters
nb = NbClust(data=NULL, diss=gdist_rural,
             min.nc=2, max.nc=5, distance=NULL,
             method="median", index ="dunn")
nb$Best.nc

#suggests 2 clusters
nb = NbClust(data=NULL, diss=gdist_rural,
             min.nc=2, max.nc=5, distance=NULL,
             method="median", index ="frey")
nb$Best.nc

#suggests 2 clusters
nb = NbClust(data=NULL, diss=gdist_rural,
             min.nc=2, max.nc=5, distance=NULL,
             method="median", index ="silhouette")
nb$Best.nc

#save versions with and without age
save(train_df_rural, file = "9_Ethiopia_DHS_df_train_rural_wclusters_age.RData")

save(train_df_rural, file = "9_Ethiopia_DHS_df_train_rural_wclusterlabels_age.RData")

save(train_df_rural, file = "9_Ethiopia_DHS_df_train_rural_wclusters.RData")

save(train_df_rural, file = "9_Ethiopia_DHS_df_train_rural_wclusterlabels.RData")


###################################### SCRAPS looking at age within clusters #############################################

#age distribution by cluster
#barplot(dhs.seg$age.yrs, names.arg=dhs.seg$cluster_k6, xlab="Cluster", ylab="Age",col="blue",
#        main="Age by cluster",border="red")

#not working and partner not working (unmarried): almost all 15-20
#dhs.seg.R1 <- dhs.seg
#dhs.seg.R1 <-dhs.seg.R1 %>% filter(cluster == "R1")
#hist(dhs.seg.U1$age.yrs, freq=FALSE)

#not working, partner mostly working: essentially normally distributed, peak around 25-30
#dhs.seg.R2 <- dhs.seg
#dhs.seg.R2 <-dhs.seg.R2 %>% filter(cluster == "R2")
#hist(dhs.seg.R2$age.yrs, freq=FALSE)

#working, partner not (most not married): most 15-20 but declining slope of the rest, unlike U1
#dhs.seg.R3 <- dhs.seg
#dhs.seg.R3 <-dhs.seg.R3 %>% filter(cluster == "R3")
#hist(dhs.seg.UR$age.yrs, freq=FALSE)

#working and partner working: more normally distributed
#dhs.seg.R4 <- dhs.seg
#dhs.seg.R4 <-dhs.seg.R4 %>% filter(cluster == "R4")
#hist(dhs.seg.R4$age.yrs, freq=FALSE)

#explorations


#meantest <- dhs.seg %>% group_by(cluster_k6) %>%
#  summarize_at(c('age.yrs'),mean)
#meantest
#no age in clustering
#1 R1            30.1
#2 R2            30.1
#3 R3            32.3
#4 R4            20.8
#5 R5            29.1
#6 R6            23.9

#with age
#1 R1            30.4
#2 R2            20.0
#3 R3            30.3
#4 R4            28.0
#5 R5            33.2
#6 R6            22.6
#7 NA            27.3

#meantest <- dhs.seg %>% group_by(cluster_k6) %>%
#  summarize_at(c('wealth.index.ur.ord'),mean)
#meantest
#no age in clustering
#1 R1                        2.21
#2 R2                        2.01
#3 R3                        2.64
#4 R4                        3.27
#5 R5                        4.01
#6 R6                        2.85
#7 NA                        3.30

#with age in clustering
#1 R1                        2.03
#2 R2                        2.84
#3 R3                        2.00
#4 R4                        3.79
#5 R5                        2.76
#6 R6                        3.39
#7 NA                        3.30

#addmargins(table(dhs.seg$zerodose.yn, dhs.seg$cluster_k6, dnn = c("dose", "cluster")))

#zerodose rural clusters vary from 5%, many 9%, then two 20ish%. so most below avg, one above
#mytable <- table(dhs.seg$zerodose.yn, dhs.seg$cluster_k6)
#prop.table(mytable, 2)

#avg zerodose in rural is 15%, avg in hh.urban is 2.8%
#mytable <- table(dhs.seg$zerodose.yn, dhs.seg$hh.urban)
#prop.table(mytable, 2)

#cor(dhs.seg$wealth.index.ur.ord, dhs.seg$age.yrs)
