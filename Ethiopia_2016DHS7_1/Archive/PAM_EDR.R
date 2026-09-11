library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
library(factoextra)
library(NbClust)

setwd("C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways - GE Data & Insights")
dhs.seg <- readRDS("Modeling/Data/Ethiopia DHS/ET_2016/dhs.seg.rds")
dhs.seg$hh.rooms<-ifelse(dhs.seg$hh.rooms=="4+","4",dhs.seg$hh.rooms); dhs.seg$hh.rooms2<-as.numeric(dhs.seg$hh.rooms)
dhs.seg$hh.cook.inside2<-ifelse(dhs.seg$hh.cook.inside=="In a separate building/room","Separate",dhs.seg$hh.cook.inside); dhs.seg$hh.cook.inside2<-factor(dhs.seg$hh.cook.inside2)
dhs.seg$hh.wat.time2<-ifelse(dhs.seg$hh.wat.time=="Water on premises",0,
                             ifelse(dhs.seg$hh.wat.time=="0-10 mintues",1,
                                    ifelse(dhs.seg$hh.wat.time=="11-20 minutes" | dhs.seg$hh.wat.time=="21-30 minutes",2,3)))
dhs.seg$partner.working2<-ifelse(dhs.seg$partner.working=="Didn't work last 12 months" | dhs.seg$partner.working=="Don't know","No",
                                 ifelse(dhs.seg$partner.working=="No current partner","No partner",
                                        ifelse(dhs.seg$partner.working=="Worked last 12 months", "Yes year", "Yes week"))); dhs.seg$partner.working2<-factor(dhs.seg$partner.working2)
dhs.seg$working2<-ifelse(dhs.seg$working=="Currently working","Yes",
                         ifelse(dhs.seg$working=="No work in the past 12 months","No","Past year")); dhs.seg$working2<-factor(dhs.seg$working2)

dhs.seg <- dhs.seg %>% mutate(jd.index.cat = case_when(
  marr.cohab == 0 ~ "Not partnered",
  jd.index == 0 ~ "None",
  jd.index %in% c(1:4) ~ "One to four",
  jd.index %in% c(5:6) ~ "Five to six")); dhs.seg$jd.index.cat <- factor(dhs.seg$jd.index.cat)

dhs.seg <- dhs.seg %>% mutate(ed.lev.cat = case_when(
  ed.lev =="no education" ~ "no ed",
  ed.lev %in% c("incomplete primary", "complete primary")  ~ "primary",
  ed.lev %in% c("incomplete secondary", "complete secondary") ~ "secondary",
  ed.lev == "higher" ~ "higher")); dhs.seg$ed.lev.cat <- factor(dhs.seg$ed.lev.cat)

dhs.seg <- dhs.seg %>% mutate(ed.lev.cat2 = case_when(
  ed.lev =="no education" ~ 0,
  ed.lev == "incomplete primary" ~ 1,
  ed.lev == "complete primary"~ 2,
  ed.lev == "incomplete secondary" ~ 3, 
  ed.lev == "complete secondary" ~ 4,
  ed.lev == "higher" ~ 5))

dhs.seg <- dhs.seg %>% mutate(husb.ed.lev.cat = case_when(
  husb.ed.grp =="no education" ~ "no ed",
  husb.ed.grp %in% c("incomplete primary", "complete primary", "don't know")  ~ "primary",
  husb.ed.grp %in% c("incomplete secondary", "complete secondary") ~ "secondary",
  husb.ed.grp == "higher" ~ "higher",
  husb.ed.grp == "Not in union" ~ "not in union")); dhs.seg$husb.ed.lev.cat <- factor(dhs.seg$husb.ed.lev.cat)

dhs.seg <- dhs.seg %>% mutate(pship.status.cat = case_when(
  pship.status == "never in union" ~ "never in union", 
  pship.status %in% c("married", "living with partner") ~ "married or cohabitating",
  pship.status %in% c("widowed", "divorced", "no longer living together/separated") ~ "separated or widowed")); dhs.seg$pship.status.cat <- factor(dhs.seg$pship.status.cat)

dhs.seg <- dhs.seg %>% mutate(age.1stsex.cat2 = case_when(
  age.1stsex < 15 ~ "8-14",
  age.1stsex >= 15 & dhs.seg$age.1stsex <20 ~ "15-20",
  age.1stsex >= 20~ "20+",
  age.1stsex.cat == "Inconsistent or don't know" ~ "15-20",
  age.1stsex.cat == "never" ~"never")); dhs.seg$age.1stsex.cat2 <- factor(dhs.seg$age.1stsex.cat2)

dhs.seg <- dhs.seg %>% mutate(religion.cat = case_when(
  religion == "orthodox" ~ "orthodox",
  religion %in% c("catholic","protestant") ~ "christian",
  religion == "muslin" ~ "muslim",
  religion %in% c("traditional", "other") ~ "other")); dhs.seg$religion.cat <- factor(dhs.seg$religion.cat)

dhs.seg <- dhs.seg %>% mutate(wealth.index2 = case_when(
  wealth.index.ur == "poorest" ~ 1,
  wealth.index.ur == "poorer" ~ 2,
  wealth.index.ur == "middle" ~ 3,
  wealth.index.ur == "richer" ~ 4,
  wealth.index.ur == "richest" ~ 5))

dhs.seg$age.1stcohab.cat <- factor(dhs.seg$age.1stcohab.cat)
dhs.seg$slum1 <- factor(dhs.seg$slum1)


dhs.seg$any.media.yn <- factor(dhs.seg$any.media.yn)
dhs.seg$hh.bank.acct <- factor(dhs.seg$hh.bank.acct)
dhs.seg$hh.electricity <- factor(dhs.seg$hh.electricity)
dhs.seg$hh.sanitation.yn <- factor(dhs.seg$hh.sanitation.yn)
dhs.seg$part.nochores <- factor(dhs.seg$part.nochores)
dhs.seg$polygamy <- factor(dhs.seg$polygamy)
dhs.seg$toilet.facility <- factor(dhs.seg$toilet.facility)
dhs.seg$work.type.prof <- factor(dhs.seg$work.type.prof)

dhs.seg.u<-subset(dhs.seg, hh.urban==1)
dhs.seg.r<-subset(dhs.seg, hh.urban==0)


# 2 - rural, 2/3 - urban

#### URBAN CLUSTERING
dhs.seg.clust.u<- dhs.seg.u %>% select(c(any.media.yn,
                                     ed.lev.cat,
                                     dv.index,
                                     age.1stcohab.cat,
                                     age.1stsex.cat2,
                                     pship.status.cat,
                                     head.sex,
                                     hh.bank.acct,
                                     hh.cook.inside2,
                                     hh.electricity,
                                     hh.kidwom.rat,
                                     hh.land,
                                     hh.rooms2,
                                     hh.sanitation.yn,
                                     hh.wat.time2,
                                     husb.ed.lev.cat,
                                     jd.index.cat,
                                     med.index,
                                     religion.cat,
                                     num.15up,
                                     num.under15,
                                     part.nochores,
                                     partner.working2,
                                     polygamy,
                                     toilet.facility,
                                     wealth.index2,
                                     work.type.prof,
                                     working2,
                                     slum1))

gower_distu <- daisy(dhs.seg.clust.u, metric = "gower")
gower_matu <- as.matrix(gower_distu)

sil_widthu <- c(NA)
for(i in 2:6){  
  pam_fit <- pam(gower_distu, diss = TRUE, k = i)  
  sil_widthu[i] <- pam_fit$silinfo$avg.width  
}
plot(1:6, sil_widthu,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:6, sil_widthu)

pam_fit2 <- pam(gower_distu, diss = TRUE, 2, trace.lev = 10)
pam_fit3 <- pam(gower_distu, diss = TRUE, 3, trace.lev = 10) # going to choose this one
pam_fit4 <- pam(gower_distu, diss = TRUE, 4, trace.lev = 10)

tsne_obj <- Rtsne(gower_distu, is_distance = TRUE)

tsne_data2 <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit2$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data2) + geom_point(aes(color = cluster))

tsne_data3 <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit3$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data3) + geom_point(aes(color = cluster))

tsne_data4 <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit4$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data4) + geom_point(aes(color = cluster))

dhs.seg.u$cluster<-pam_fit4$clustering # attach it back to the dhs.seg dataset
dhs.seg.u$cluster<-paste0('U',dhs.seg.u$cluster)

# Choosing number of clusters
fit<-hclust(gower_distu, method="ward.D")
plot(fit, cex=0.4)
rect.hclust(fit, k=3)
rect.hclust(fit, k=4)
fit2 <- hclust(gower_distu, method="ward.D2")
plot(fit2, cex=0.4)
rect.hclust(fit2, k=2)
rect.hclust(fit2, k=4)
fit3 <- hclust(gower_distu, method="complete")
plot(fit3, cex=0.4)
rect.hclust(fit3, k=3)
rect.hclust(fit3, k=4)

# Determine number of clusters
# Only frey, mcclain, cindex, sihouette and dunn can be computed
list.indices = c("mcclain", "dunn", "cindex", "silhouette","frey")
list.method = c("ward.D", "ward.D2","complete","median")
tabla = as.data.frame(matrix(ncol = length(list.method), nrow = length(list.indices)))
names(tabla) = list.method

for (j in 2:length(list.method)){
  for(i in 1:length(list.indices)){
    
    nb = NbClust(data=NULL, diss=gower_distu,
                 min.nc=2, max.nc=6, distance=NULL,
                 method=list.method[j], index =list.indices[i])
    tabla[i,j] = nb$Best.nc[1]
    tabla[i,5] = list.indices[i]
    
  }}
tabla

nbc.gow <- NbClust(data=NULL, diss=gower_distu,
                   min.nc=2, max.nc=6, distance=NULL,
                   method='ward.D2', index="dunn")
nbc.gow$Best.n

# PAM-k
pam_fit_k <- pamk(gower_dist, diss = TRUE, krange=1:6, criterion='asw', critout=TRUE)

df_pamk <- melt(pam_fit_k$crit)
df_pamk$index <- rownames(df_pamk)

ggplot(df_pamk, aes(x=index, y=value)) +
  geom_bar(stat='identity', fill='steelblue') + 
  ggtitle('Silhouette Width Criterion\nMedoid + Gower Distance') +
  labs(x='Number of clusters k', y='Average silhouette width criterion value')

##### RURAL Clustering
dhs.seg.clust.r<- dhs.seg.r %>% select(c(any.media.yn,
                                         ed.lev.cat,
                                         dv.index,
                                         age.1stcohab.cat,
                                         age.1stsex.cat2,
                                         pship.status.cat,
                                         head.sex,
                                         hh.bank.acct,
                                         hh.cook.inside2,
                                         hh.electricity,
                                         hh.kidwom.rat,
                                         hh.land,
                                         hh.rooms2,
                                         hh.sanitation.yn,
                                         hh.wat.time2,
                                         husb.ed.lev.cat,
                                         jd.index.cat,
                                         med.index,
                                         religion.cat,
                                         num.15up,
                                         num.under15,
                                         part.nochores,
                                         partner.working2,
                                         polygamy,
                                         toilet.facility,
                                         wealth.index2,
                                         work.type.prof,
                                         working2))

gower_distr <- daisy(dhs.seg.clust.r, metric = "gower")
gower_matr <- as.matrix(gower_distr)

sil_widthr <- c(NA)
for(i in 2:6){  
  pam_fit <- pam(gower_distr, diss = TRUE, k = i)  
  sil_widthr[i] <- pam_fit$silinfo$avg.width  
}
plot(1:6, sil_widthr,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:6, sil_widthr)

pam_fit2 <- pam(gower_distr, diss = TRUE, 2, trace.lev = 10)
pam_fit3 <- pam(gower_distr, diss = TRUE, 3, trace.lev = 10) 
pam_fit4 <- pam(gower_distr, diss = TRUE, 4, trace.lev = 10)
pam_fit5 <- pam(gower_distr, diss = TRUE, 5, trace.lev = 10)# going to choose this one

tsne_obj <- Rtsne(gower_distr, is_distance = TRUE)

tsne_data2 <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit2$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data2) + geom_point(aes(color = cluster))

tsne_data3 <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit3$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data3) + geom_point(aes(color = cluster))

tsne_data4 <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit4$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data4) + geom_point(aes(color = cluster))

tsne_data5 <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit5$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data5) + geom_point(aes(color = cluster))

dhs.seg.r$cluster<-pam_fit5$clustering # attach it back to the dhs.seg dataset
dhs.seg.r$cluster<-paste0('R',dhs.seg.r$cluster)

# Choosing number of clusters
fit<-hclust(gower_distr, method="ward.D")
plot(fit, cex=0.4)
rect.hclust(fit, k=3)
rect.hclust(fit, k=4)
fit2 <- hclust(gower_distr, method="ward.D2")
plot(fit2, cex=0.4)
rect.hclust(fit2, k=2)
rect.hclust(fit2, k=4)
rect.hclust(fit2, k=5)
fit3 <- hclust(gower_distr, method="complete")
plot(fit3, cex=0.4)
rect.hclust(fit3, k=3)
rect.hclust(fit3, k=4)

# Determine number of clusters
# Only frey, mcclain, cindex, sihouette and dunn can be computed
list.indices = c("mcclain", "dunn", "cindex", "silhouette","frey")
list.method = c("ward.D", "ward.D2","complete","median")
tabla = as.data.frame(matrix(ncol = length(list.method), nrow = length(list.indices)))
names(tabla) = list.method

for (j in 2:length(list.method)){
  for(i in 1:length(list.indices)){
    
    nb = NbClust(data=NULL, diss=gower_distu,
                 min.nc=2, max.nc=6, distance=NULL,
                 method=list.method[j], index =list.indices[i])
    tabla[i,j] = nb$Best.nc[1]
    tabla[i,5] = list.indices[i]
    
  }}
tabla

nbc.gow <- NbClust(data=NULL, diss=gower_distu,
                   min.nc=2, max.nc=6, distance=NULL,
                   method='ward.D2', index="dunn")
nbc.gow$Best.n

# PAM-k
pam_fit_k <- pamk(gower_dist, diss = TRUE, krange=1:6, criterion='asw', critout=TRUE)

df_pamk <- melt(pam_fit_k$crit)
df_pamk$index <- rownames(df_pamk)

ggplot(df_pamk, aes(x=index, y=value)) +
  geom_bar(stat='identity', fill='steelblue') + 
  ggtitle('Silhouette Width Criterion\nMedoid + Gower Distance') +
  labs(x='Number of clusters k', y='Average silhouette width criterion value')


dhs.seg.2<-rbind(dhs.seg.r, dhs.seg.u)
saveRDS(dhs.seg.2, file = "Modeling/Data/Ethiopia DHS/ET_2016/dhs.seg.clust.rds")




