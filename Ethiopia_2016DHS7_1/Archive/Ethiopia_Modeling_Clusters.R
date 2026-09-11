rm(list=ls())
pacman::p_load("tidyverse", "knitr", "data.table", "lmtest", "logbin", "survey")

setwd("C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways - GE Data & Insights")

dhs.seg.clust <- readRDS("Modeling/Data/Ethiopia DHS/ET_2016/dhs.seg.clust.rds")

table(dhs.seg.clust$cluster, useNA="ifany")

old<-c("U1","U2","U3","U4","R1","R2","R3","R4","R5")
new<-c("U3","U2.1","U2.2","U1","R4","R3.1","R2","R3.2","R1")
new.seg<-data.frame("cluster"=old, "new.cluster"=new)
dhs.seg.clust<-merge(dhs.seg.clust, new.seg, by="cluster",all.x=T)

dhs.seg.clust$hh.trans<-ifelse(dhs.seg.clust$hh.car==1 | dhs.seg.clust$hh.motor==1,1,0)
dhs.seg.clust$fp.mod.no<-ifelse(dhs.seg.clust$fp.mod.ever == 1,0,1)

dhs.seg.des <-svydesign(id=~v021, strata=~v023, weights=~wt, data=dhs.seg.clust)

# Look at clusters
prop.table(svytable((~cluster+wealth.index.ur), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+hh.electricity), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+slum1), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+any.media.yn), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+ed.lev.cat), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+age.1stcohab.cat), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+pship.status.cat), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+age.1stsex.cat2), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+head.sex), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+hh.bank.acct), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+hh.land), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+hh.sanitation.yn), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+husb.ed.lev.cat), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+jd.index.cat), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+med.index), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+religion.cat), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+polygamy), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+working2), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+partner.working2), dhs.seg.des), margin=1)
prop.table(svytable((~cluster+toilet.facility), dhs.seg.des), margin=1)

svyby(~hh.kidwom.rat, ~cluster, dhs.seg.des, svymean)
svyby(~num.under15, ~cluster, dhs.seg.des, svymean)
svyby(~num.15up, ~cluster, dhs.seg.des, svymean)
svyby(~num.preg, ~cluster, dhs.seg.des, svymean)
svyby(~hh.rooms2, ~cluster, dhs.seg.des, svymean)

# Pie chart of segments
seg<-prop.table(svytable(~new.cluster, dhs.seg.des))
seg.df<-as.data.frame(seg)
seg.df$prop<-round(seg.df$Freq*100,1)

ggplot(seg.df, aes(x = "", y = prop, fill = new.cluster)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(prop,"%")),
            position = position_stack(vjust = 0.5), size=8) +
  coord_polar(theta = "y") +
  scale_fill_manual(values=c('#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e'), name="Segment") +
  theme_void()+
  theme(legend.position = "right", legend.title=element_text(size=16),legend.text = element_text(size = 14))


# Plotting segs
tot <- melt(svytable(~new.cluster, dhs.seg.des))
hb <- melt(svytable(~zerodose.yn, dhs.seg.des))

tot <-data.table(tot) 
setkey(tot,value)

####### 
zdseg <- melt(svytable(~new.cluster+zerodose.yn, dhs.seg.des))
perc <- merge(zdseg,tot,by="new.cluster")
perc$perct <- 100*perc$value.x/perc$value.y
#perc <- perc[perc$value.y>=50,]

theme_set(theme_bw(30))
ggplot(aes(x=new.cluster),data=perc)+  geom_col(aes(y=perct, fill=as.factor(zerodose.yn))) + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Segments")+ ylab("% of Women in each Segment") +ggtitle("") + 
  guides(fill=guide_legend(title="HH has Zero Dose Child"))+ scale_fill_manual(values=c("grey", "darkcyan"), labels=c("No", "Yes"))

## Ladder plot
svytotal(~new.cluster, dhs.seg.des)
svyciprop(~zerodose.yn, dhs.seg.des, na.rm=T)

zd<-svyby(~zerodose.yn, ~new.cluster, dhs.seg.des, svyciprop,ci=TRUE,vartype="ci")
as.data.frame(zd)
zd<-zd[-c(3,7),]
zd$zerodose.yn<-zd$zerodose.yn*100
zd$ci_l<-zd$ci_l*100
zd$ci_u<-zd$ci_u*100

ggplot(zd) + 
  geom_col(aes(x=reorder(new.cluster, desc(new.cluster)), y=zerodose.yn), alpha=0.9, size=5, fill="darkcyan", show.legend = FALSE) +
  geom_errorbar(aes(x=reorder(new.cluster, desc(new.cluster)), y=zerodose.yn, ymin=ci_l, ymax=ci_u), size=1, width=.5, show.legend = FALSE) +
  geom_text(aes(y=zerodose.yn, x=new.cluster, label=round(zerodose.yn,1)), vjust=-1, hjust=.5)+
  coord_flip()+
  xlab("") +
  scale_y_continuous(name = "% Households with Zero Dose Children", breaks=seq(0, 100, 10)) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

#Ladder plot 2
svy.glm<-svyglm(zerodose.yn~new.cluster,family=quasibinomial,design=dhs.seg.des, data=kenya)

pred.data<-data.frame("new.cluster"=new.seg$new.cluster)
pred.data3<-data.frame(type=c("Urban","Urban","Urban","Urban","Rural","Rural","Rural","Rural","Rural"))

# Plot extended segments
probabilities <- predict(svy.glm, pred.data, type = "response", se.fit=T)
prob1<-as.data.frame(probabilities)
prob1$lower<- prob1$response - (1.96*prob1$SE)
prob1$upper<- prob1$response + (1.96*prob1$SE)
prob1$segX4<- pred.data$new.cluster
prob1$type<-pred.data3$type

cbPalette <- c("#A6611A","#018571")
ggplot(prob1, aes(x=reorder(segX4, desc(segX4)), y=response)) + 
  geom_point(aes(colour=type), alpha=0.9, size=5, show.legend = FALSE) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=type), size=1, width=.5, show.legend = FALSE) +
  geom_text(aes(y=response, x=segX4, label=round(response,2)), vjust=-1.2, hjust=.5) +
  coord_flip()+
  xlab("") +
  ylab("Probability of Zero Dose") +
  scale_colour_manual(values=cbPalette) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

## Ladder plot
svytotal(~new.cluster, dhs.seg.des)
svyciprop(~hb.1, dhs.seg.des, na.rm=T)

hb<-svyby(~hb.1, ~new.cluster, dhs.seg.des, svyciprop,ci=TRUE,vartype="ci")
as.data.frame(hb)
hb<-hb[-c(3,7),]
hb$hb.1<-hb$hb.1*100
hb$ci_l<-hb$ci_l*100
hb$ci_u<-hb$ci_u*100

ggplot(hb) + 
  geom_col(aes(x=reorder(new.cluster, desc(new.cluster)), y=hb.1), alpha=0.9, size=5, fill="darkcyan", show.legend = FALSE) +
  geom_errorbar(aes(x=reorder(new.cluster, desc(new.cluster)), y=hb.1, ymin=ci_l, ymax=ci_u), size=1, width=.5, show.legend = FALSE) +
  geom_text(aes(y=hb.1, x=new.cluster, label=round(hb.1,1)), vjust=-1, hjust=.5)+
  coord_flip()+
  xlab("") +
  scale_y_continuous(name = "% Women with Home Birth", breaks=seq(0, 100, 10)) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

## Ladder plot
svytotal(~new.cluster, dhs.seg.des)
svyciprop(~underweight, dhs.seg.des, na.rm=T)

und<-svyby(~bmi, ~new.cluster, dhs.seg.des, svymean, na=T, ci=TRUE,vartype="ci")
as.data.frame(und)
hb<-hb[-c(3,7),]
und$underweight<-und$underweight*100
und$ci_l<-und$ci_l*100
und$ci_u<-und$ci_u*100

ggplot(und) + 
  geom_col(aes(x=reorder(new.cluster, desc(new.cluster)), y=underweight), alpha=0.9, size=5, fill="darkcyan", show.legend = FALSE) +
  geom_errorbar(aes(x=reorder(new.cluster, desc(new.cluster)), y=underweight, ymin=ci_l, ymax=ci_u), size=1, width=.5, show.legend = FALSE) +
  geom_text(aes(y=bmi, x=new.cluster, label=round(underweight,1)), vjust=-1, hjust=.5)+
  coord_flip()+
  xlab("") +
  scale_y_continuous(name = "% Women Underweight", breaks=seq(0, 100, 10)) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

## Ladder plot
svytotal(~new.cluster, dhs.seg.des)
svyciprop(~anyanc, dhs.seg.des, na.rm=T)

df<-svyby(~anyanc, ~new.cluster, dhs.seg.des, svymean, na=T, ci=TRUE,vartype="ci")
as.data.frame(df)
df<-df[-c(3,7),]
df$anyanc<-df$anyanc*100
df$ci_l<-df$ci_l*100
df$ci_u<-df$ci_u*100

ggplot(df) + 
  geom_col(aes(x=reorder(new.cluster, desc(new.cluster)), y=anyanc), alpha=0.9, size=5, fill="darkcyan", show.legend = FALSE) +
  geom_errorbar(aes(x=reorder(new.cluster, desc(new.cluster)), y=anyanc, ymin=ci_l, ymax=ci_u), size=1, width=.5, show.legend = FALSE) +
  geom_text(aes(y=anyanc, x=new.cluster, label=round(anyanc,1)), vjust=-1, hjust=.5)+
  coord_flip()+
  xlab("") +
  scale_y_continuous(name = "% Women Recieving Any ANC", breaks=seq(0, 100, 10)) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

#FP use and ANC
svy.glm.acc<-svyglm(hb.1~hh.trans+hh.phone+med.dist+med.cost+med.alone+anyanc, 
                    family=quasibinomial, design=dhs.seg.des, rescale=T, data=dhs.seg.r)
summary(svy.glm.acc)

# Create forest plot of regression results
coef<-coef(svy.glm.acc)
se<-SE(svy.glm.acc)
confint<-confint(svy.glm.acc)

final<-as.data.frame(cbind(coef,se, confint))
final$vars <- c("Intercept","Household has Transportation", "Household has Phone", "Distance is a Problem","Cost is a Problem", "Going Alone is a Problem","Any ANC Visits")
final<-final[-c(1), ]
colnames(final)<-c("coef", "se", "lcl", "ucl", "vars")

label<-final$vars
mean<-exp(final$coef)
lower<-exp(final$lcl)
upper<-exp(final$ucl)
index<-1:6
df<-data.frame(label, index, mean, lower, upper)

orplot<-ggplot(df) + 
  geom_point(aes(x=index, y=mean), alpha=0.9, size=4, fill="darkcyan", show.legend = FALSE) +
  geom_errorbar(aes(x=index, y=mean, ymin=lower, ymax=upper), size=1, width=.3, show.legend = FALSE) +
  geom_text(aes(y=mean, x=index, label=round(mean,2)), vjust=-.9, hjust=.5)+
  geom_hline(yintercept=1, lty=2) +
  coord_flip()+
  scale_x_continuous(name = "", breaks=1:nrow(df), labels=df$label)+
  scale_y_continuous(name= "Odds Ratio (95% CI)", breaks=seq(0, 3, .5), limits=c(0,3)) +
  theme(axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

svy.glm.dd<-svyglm(hb.ever~anc.4plus+anc.month2+brth.hosp.cat+brth.hbsafe.cat+no.plan, 
                   family=quasibinomial, design=svy.ne, rescale=T, data=ne)
summary(svy.glm.dd)
