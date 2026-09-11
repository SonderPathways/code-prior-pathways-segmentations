##################################################################################
## Using data created from Nigeria_Pathways_Recode                              ##
## this code base creates a series of plots and runs regressions                ##
## to model home birth from the N Nigeria Pathways dataset                      ##
##                                                                              ##
## Last updated 10/7/2022 by E.D. Root                                          ##
##################################################################################

library(foreign)
library(ggplot2)
library(ggbreak)
library(dplyr)
library(tidyr)
library(data.table)
library(gmodels)
library(caret)
library(gbm)
library(psych)
library(survey)
library(haven)
library(psych)
library(sjPlot)
library(naniar)

######################## plotting 
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

Pathdesign<-svydesign(id=nigeria$PSUclean, strata=nigeria$rural, weights=nigeria$base.wt, survey.lonely.psu="adjust", nest=T, data=nigeria)
tot <- melt(svytable(~new.seg, Pathdesign))
hb <- melt(svytable(~dlvhm.ever, Pathdesign))

tot <-data.table(tot) 
setkey(tot,value)

####### 
hbseg <- melt(svytable(~new.seg+dlvhm.ever, Pathdesign))
perc <- merge(hbseg,tot,by="new.seg")
perc$perct <- 100*perc$value.x/perc$value.y
#perc <- perc[perc$value.y>=50,]

theme_set(theme_bw(30))
ggplot(aes(x=new.seg),data=perc)+  geom_col(aes(y=perct, fill=as.factor(dlvhm.ever))) + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Segments")+ ylab("% of Women in each Segment") +ggtitle("") + 
  guides(fill=guide_legend(title="Home Birth"))+ scale_fill_manual(values=c("grey", "darkcyan"), labels=c("No", "Yes"))

## Ladder plot
svytotal(~new.seg, Pathdesign)
svyciprop(~dlvhm.ever, Pathdesign, na.rm=T)

hb<-svyby(~dlvhm.ever, ~new.seg, Pathdesign, svymean,ci=TRUE,vartype="ci"); as.data.frame(hb)
hb$dlvhm.ever<-hb$dlvhm.ever*100
hb$ci_l<-hb$ci_l*100
hb$ci_u<-hb$ci_u*100

ggplot(hb) + 
  geom_col(aes(x=reorder(new.seg, desc(new.seg)), y=dlvhm.ever), alpha=0.9, size=5, fill="darkcyan", show.legend = FALSE) +
  geom_errorbar(aes(x=reorder(new.seg, desc(new.seg)), y=dlvhm.ever, ymin=ci_l, ymax=ci_u), size=1, width=.5, show.legend = FALSE) +
  geom_text(aes(y=dlvhm.ever, x=new.seg, label=round(dlvhm.ever,1)), vjust=-1, hjust=.5)+
  coord_flip()+
  xlab("") +
  scale_y_continuous(name = "Percent of Births at Home", breaks=seq(0, 100, 10)) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

## Ladder plot - preference
svytotal(~pref.home, Pathdesign, na.rm=T)
svyciprop(~dlvhm.ever, Pathdesign, na.rm=T)
svyciprop(~pref.home+dlvhm.ever, Pathdesign, na.rm=T)

hb<-svyby(~dlvhm.ever, ~pref.home, Pathdesign, svymean,ci=TRUE,vartype="ci"); as.data.frame(hb)
hb$dlvhm.ever<-hb$dlvhm.ever*100
hb$ci_l<-hb$ci_l*100
hb$ci_u<-hb$ci_u*100

ggplot(hb) + 
  geom_col(aes(x=reorder(pref.home, desc(pref.home)), y=dlvhm.ever), alpha=0.9, size=5, fill="darkcyan", show.legend = FALSE) +
  geom_errorbar(aes(x=reorder(pref.home, desc(pref.home)), y=dlvhm.ever, ymin=ci_l, ymax=ci_u), size=1, width=.2, show.legend = FALSE) +
  geom_text(aes(y=dlvhm.ever, x=pref.home, label=round(dlvhm.ever,1)), vjust=-2, hjust=.5)+
  coord_flip()+
  xlab("") +
  scale_y_continuous(name = "Percent of Home Births", breaks=seq(0, 100, 10)) +
  scale_x_discrete(labels=c("0" = "Do not Prefer", "1" = "Prefer")) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

## Ladder plot - pref by seg
svytotal(~new.seg, Pathdesign, na.rm=T)
svyciprop(~pref.home, Pathdesign, na.rm=T)

hb<-svyby(~pref.home, ~new.seg, Pathdesign, svymean,ci=TRUE,vartype="ci", na.rm=T); as.data.frame(hb)
hb$pref.home<-hb$pref.home*100
hb$ci_l<-hb$ci_l*100
hb$ci_u<-hb$ci_u*100

ggplot(hb) + 
  geom_col(aes(x=reorder(new.seg, desc(new.seg)), y=pref.home), alpha=0.9, size=5, fill="darkcyan", show.legend = FALSE) +
  geom_errorbar(aes(x=reorder(new.seg, desc(new.seg)), y=pref.home, ymin=ci_l, ymax=ci_u), size=1, width=.5, show.legend = FALSE) +
  geom_text(aes(y=pref.home, x=new.seg, label=round(pref.home,1)), vjust=-1, hjust=.5)+
  coord_flip()+
  xlab("") +
  scale_y_continuous(name = "Percent Prefering Home Birth", breaks=seq(0, 100, 10)) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

#Ladder plot 2
svy.glm<-svyglm(dlvhm.ever~new.seg,family=quasibinomial,design=Pathdesign, data=nigeria)

pred.data<-data.frame(new.seg=c("RM4.1-NN",
                                "RM4.2-NN",
                                "RM3-NN",
                                "RF1-NN",
                                "RF4-NN",
                                "UM4-NN",
                                "UM1-NN",
                                "UF3-NN"))
pred.data3<-data.frame(type=c("RM" ,"RM", "RM" ,"RF" ,"RF" , "UM" ,"UM", "UF"))

# Plot extended segments
probabilities <- predict(svy.glm, pred.data, type = "response", se.fit=T)
prob1<-as.data.frame(probabilities)
prob1$lower<- prob1$response - (1.96*prob1$SE)
prob1$upper<- prob1$response + (1.96*prob1$SE)
prob1$segX4<- pred.data$new.seg
prob1$type<-pred.data3$type

cbPalette <- c("#A6611A", "#DFC27D", "#018571", "#80CDC1")
ggplot(prob1, aes(x=reorder(segX4, desc(segX4)), y=response)) + 
  geom_point(aes(colour=type), alpha=0.9, size=5, show.legend = FALSE) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=type), size=1, width=.5, show.legend = FALSE) +
  geom_text(aes(y=response, x=segX4, label=signif(response,2)), vjust=-1.2, hjust=.5) +
  coord_flip()+
  xlab("") +
  scale_y_continuous(name = "Probability of Home Birth", breaks=seq(0,1,.2), limits=c(0,1))+
  scale_colour_manual(values=cbPalette) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

#Lasso regression to reduce data dimentionality
ctrlspecs<-trainControl(method="cv", number=10, savePredictions="all")
lambda_vec<-10^seq(5, -5, length=500)

set.seed(98040)

#HB by HB Pref scatter plot
svytotal(~new.seg, Pathdesign, na.rm=T)
svyciprop(~hb.ever, Pathdesign, na.rm=T)
svyciprop(~pref.home, Pathdesign, na.rm=T)

hb<-svyby(~hb.ever, ~new.seg, Pathdesign, svymean,ci=TRUE,vartype="ci", na.rm=T); as.data.frame(hb)
pref<-svyby(~pref.home, ~new.seg, Pathdesign, svymean,ci=TRUE,vartype="ci", na.rm=T); as.data.frame(pref)

scat<-data.frame(Segment=hb$new.seg, hb=hb$hb.ever, hb.cil=hb$ci_l, hb.ciu=hb$ci_u, pref=pref$pref.home,
                 pref.cil=pref$ci_l, pref.ciu=pref$ci_u)
scat<-scat %>% mutate_if(is.numeric, ~ . * 100)

ggplot(scat, aes(x=hb, y=pref, color=Segment)) +
  geom_point(aes(x=hb, y=pref), size=4) +
  geom_errorbar(aes(x=hb, y=pref, ymin=pref.cil, ymax=pref.ciu), linetype="dashed", size=1, width=1, show.legend = FALSE) +
  geom_errorbar(aes(x=hb, y=pref, xmin=hb.cil, xmax=hb.ciu), linetype="dashed", size=1, width=1, show.legend = FALSE) +
  scale_x_continuous(name="Percent Giving Birth at Home", breaks=seq(0, 100, 10)) +
  scale_y_continuous(name = "Percent with Preference for Home Birth", breaks=seq(0, 100, 10)) +
  scale_colour_brewer(type="qual", palette=2)+
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12), axis.title=element_text(size=12),
        legend.text = element_text(size=12), legend.title=element_text(size=12))


svyciprop(~pref.home, Pathdesign, na.rm=T)

hb<-svyby(~pref.home, ~new.seg, Pathdesign, svymean,ci=TRUE,vartype="ci", na.rm=T); as.data.frame(hb)
hb$pref.home<-hb$pref.home*100
hb$ci_l<-hb$ci_l*100
hb$ci_u<-hb$ci_u*100


### Specify/train model
# Mental mod/intent
nigeria_intent1 <- nigeria %>% select(c("hb.ever","gen.rolez","bar.hcz", "brth.normz",
                            "risky.brthz","more.anc", "trust.doc", "trust.prov"))

glm_int1<-train(as.factor(hb.ever)~., 
                  data=nigeria_intent1,
                  preProcess=c("center","scale"),
                  method="glmnet",
                  tueGrid=expand.grid(alpha=1, lambda=lambda_vec),
                  family="binomial",
                  trControl=ctrlspecs,
                  na.action=na.omit)

glm_int1$bestTune;glm_int1$results$Accuracy
ggplot(varImp(glm_int1)) + theme(axis.text = element_text(size = 15), axis.title = element_text(size = 20))

#Experience/intent
nigeria_intent2 <- ng %>% select(c("hb.ever","preg.know","anc.visit","age.1stpreg","wom.ed.cat","media.allz","still.mis","live.brth"))

glm_int2<-train(as.factor(hb.ever)~., 
                data=nigeria_intent2,
                preProcess=c("center","scale"),
                method="glmnet",
                tueGrid=expand.grid(alpha=1, lambda=lambda_vec),
                family="binomial",
                trControl=ctrlspecs,
                na.action=na.omit)

glm_int2$bestTune;glm_int2$results$Accuracy
ggplot(varImp(glm_int2)) + theme(axis.text = element_text(size = 15), axis.title = element_text(size = 20))


#Social structure/intent
nigeria_intent3 <- ng %>% select(c("hb.ever", "num.hh.members","total_sons_daughters", "age.resp","part.in.hh",
                                 "part.trustz","muslim","wom.dec.hlth","wom.dec.big","dv.notokz","C41_preg_rej_partner"))

glm_int3<-train(as.factor(hb.ever)~., 
                data=nigeria_intent3,
                preProcess=c("center","scale"),
                method="glmnet",
                tueGrid=expand.grid(alpha=1, lambda=lambda_vec),
                family="binomial",
                trControl=ctrlspecs,
                na.action=na.omit)

glm_int3$bestTune;glm_int3$results$Accuracy
ggplot(varImp(glm_int3)) + theme(axis.text = element_text(size = 15), axis.title = element_text(size = 20))

#HH SES/intent
nigeria_intent4 <- ng %>% select(c("hb.ever", "wealth.quint", "wom.savgrp","wom.work","hh.bank.acct"))

glm_int4<-train(as.factor(hb.ever)~., 
                data=nigeria_intent4,
                preProcess=c("center","scale"),
                method="glmnet",
                tueGrid=expand.grid(alpha=1, lambda=lambda_vec),
                family="binomial",
                trControl=ctrlspecs,
                na.action=na.omit)

glm_int4$bestTune;glm_int4$results$Accuracy
ggplot(varImp(glm_int4)) + theme(axis.text = element_text(size = 15), axis.title = element_text(size = 20))

#Community/intent
nigeria_prep5 <- ng %>% select(c("hb.ever", "supp.out.hh", "part.trustz", "wom.savgrp","comm.trustz"))

glm_prep5<-train(as.factor(hb.ever)~., 
                data=nigeria_prep5,
                preProcess=c("center","scale"),
                method="glmnet",
                tueGrid=expand.grid(alpha=1, lambda=lambda_vec),
                family="binomial",
                trControl=ctrlspecs,
                na.action=na.omit)

glm_prep5$bestTune;glm_prep5$results$Accuracy
ggplot(varImp(glm_prep5)) + theme(axis.text = element_text(size = 15), axis.title = element_text(size = 20))


#Final models
final<-nigeria %>% select(c("hb.ever", "gen.rolez","bar.hcz","brth.normz","risky.brthz","pref.home","no.plan",
                       "anc.visit","wom.ed.cat","media.allz","live.brth","age.1stpreg", "age.resp",
                       "wealth.quint", "muslim","wom.dec.hlth", "comm.trustz","part.trustz", "dv.notokz",
                       "hh.phone","hh.trans"))

glm_final<-train(as.factor(hb.ever)~., 
                 data=final,
                 preProcess=c("center","scale"),
                 method="glmnet",
                 tueGrid=expand.grid(alpha=1, lambda=lambda_vec),
                 family="binomial",
                 trControl=ctrlspecs,
                 na.action=na.omit)

glm_final$bestTune;glm_final$results$Accuracy
ggplot(varImp(glm_final)) + theme(axis.text = element_text(size = 15), axis.title = element_text(size = 20))


# Prediction with segments
index<-createDataPartition(final$hb.ever, p=.8, list=F, times=1)
train_df<-final[index,]
test_df<-final[-index,]
test_df<-test_df[complete.cases(test_df), ]

# k-fold CV
ctrlspecs<-trainControl(method="cv", number=10, savePredictions="all")

glmfinal2<-train(as.factor(hb.ever)~., 
                 data=train_df,
                 method="glm",
                 family="binomial",
                 trControl=ctrlspecs,
                 na.action=na.omit)

### Model prediction
pred2<-predict(glmfinal2, newdata=test_df)
confusionMatrix(data=pred2, as.factor(test_df$hb.ever))

#Lasso regression - look at predictive power of just segments
final.seg<-nigeria %>% select(c("hb.ever","new.seg","pref.home"))

# Create test/training datasets
index<-createDataPartition(final.seg$hb.ever, p=.8, list=F, times=1)
train_df<-final.seg[index,]
test_df<-final.seg[-index,]
test_df<-test_df[complete.cases(test_df), ]

# k-fold CV
ctrlspecs<-trainControl(method="cv", number=10, savePredictions="all")

glmfinal<-train(as.factor(hb.ever)~as.factor(new.seg)+pref.home, 
                data=train_df,
                method="glm",
                family="binomial",
                trControl=ctrlspecs,
                na.action=na.omit)

### Model prediction
pred1<-predict(glmfinal, newdata=test_df)
confusionMatrix(data=pred1, as.factor(test_df$hb.ever))

# Final models
svy.des<-svydesign(id=nigeria$PSUclean, strata=nigeria$rural, weights=nigeria$base.wt, data=nigeria)
##############################
svy.glm.fin<-svyglm(hb.ever~brth.normz+risky.brthz+trust.chw+anc.visit.cat+onechild+hc.alone.cat+muslim+wom.ed.cat+age.resp+I(age.resp^2)+num.hh.members+
                    wealth.index+media.allz+comm.trustz+part.trust.quint, 
                    family=quasibinomial, design=svy.des, rescale=T, data=nigeria)
summary(svy.glm.fin)

svy.glm.int<-svyglm(hb.ever~pref.home2*new.seg, 
                    family=quasibinomial, design=svy.des, rescale=T, data=nigeria)

plot_model(svy.glm.int, type="pred", terms=c("pref.home2", "new.seg"), dot.size = 6, line.size = 1.5, title="",
                 axis.title=c("Preferred Location of Birth", "Probability of Home Birth"), legend.title = "Segment")

# Create forest plot of regression results
coef<-coef(svy.glm.fin)
se<-SE(svy.glm.fin)
confint<-confint(svy.glm.fin)

final<-as.data.frame(cbind(coef,se, confint))
final$vars <- c("Intercept","Birthing Norms","Told Risky Birth","High Trust in CHW", "No CHW", "ANC: 1-3 visits", "ANC: 4+ visits", "ANC: DNK",
                "Only one child", "Problem to Travel Alone","Muslim Religion", "Woman has Primary Ed","Woman has Secondary Ed", "Woman has Higher Ed", 
                "Woman's Age", "Age^2", "Number of HH Members",
                "Wealth Index (z-score)", "Media Exposure", "Trust in Community", "Trust with Partner: Low", "Trust with Partner: Mid-High", "Trust: High", "Trust: No Partner")
final<-final[-c(1,5,8,16,22:24), ]
colnames(final)<-c("coef", "se", "lcl", "ucl", "vars")

label<-final$vars
mean<-exp(final$coef)
lower<-exp(final$lcl)
upper<-exp(final$ucl)
index<-1:17
df<-data.frame(label, index, mean, lower, upper)

ggplot(df) + 
  geom_point(aes(x=index, y=mean), alpha=0.9, size=4, fill="darkcyan", show.legend = FALSE) +
  geom_errorbar(aes(x=index, y=mean, ymin=lower, ymax=upper), size=1, width=.3, show.legend = FALSE) +
  geom_text(aes(y=mean, x=index, label=round(mean,2)), vjust=-.9, hjust=.5)+
  geom_hline(yintercept=1, lty=2) +
  coord_flip()+
  scale_x_continuous(name = "", breaks=1:nrow(df), labels=df$label)+
  scale_y_continuous(name= "Odds Ratio (95% CI)", limits=c(0,3), breaks=seq(0,3,.5)) +
  theme(axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

plot_model(svy.glm.fin, type="pred", terms=c("age.resp", "onechild"))


#Ladder plot probability of preferring home
svy.glm<-svyglm(pref.home~new.seg,family=quasibinomial,design=Pathdesign, data=nigeria)

pred.data<-data.frame(new.seg=c("RM4.1-NN",
                                "RM4.2-NN",
                                "RM3-NN",
                                "RF1-NN",
                                "RF4-NN",
                                "UM4-NN",
                                "UM1-NN",
                                "UF3-NN"))
pred.data3<-data.frame(type=c("RM" ,"RM", "RM" ,"RF" ,"RF" , "UM" ,"UM", "UF"))

# Plot extended segments
probabilities <- predict(svy.glm, pred.data, type = "response", se.fit=T)
prob1<-as.data.frame(probabilities)
prob1$lower<- prob1$response - (1.96*prob1$SE)
prob1$upper<- prob1$response + (1.96*prob1$SE)
prob1$segX4<- pred.data$new.seg
prob1$type<-pred.data3$type

cbPalette <- c("#A6611A", "#DFC27D", "#80CDC1", "#018571")
ggplot(prob1, aes(x=reorder(segX4, desc(segX4)), y=response)) + 
  geom_point(aes(colour=type), alpha=0.9, size=5, show.legend = FALSE) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=type), size=1, width=.5, show.legend = FALSE) +
  geom_text(aes(y=response, x=segX4, label=round(response,2)), vjust=-1.2, hjust=.5) +
  coord_flip()+
  xlab("") +
  scale_y_continuous(name="Probability of Prefer Home Birth", limits=c(0,1)) +
  scale_colour_manual(values=cbPalette) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

# Looking for interactions
twoseg<-subset(nigeria, new.seg=="UF1"|new.seg=="RF1")
glm.int<-glm(hb.ever~new.seg+pref.home+new.seg*pref.home, 
             family=binomial, data=nigeria)

summary(glm.int)
plot_model(glm.int, type="pred", terms=c("pref.home", "new.seg"), 
           axis.title = c("Prefer Home Birth", "Predicted Probability of Home Birth"), title="",
           legend.title = "Segment")

###################################
# Age dynamics
glm.int<-glm(hb.ever~age.1stbrth.cat, 
             family=binomial, data=nigeria)

summary(glm.int)
plot_model(glm.int, type="pred", terms=c("age.1stbrth.cat"))


# Deep dive into certain constructs - access to care
svy.glm.dd<-svyglm(hb.ever~hh.trans+hc.dist.cat+hc.cost.cat+hc.alone.cat+wom.dec.hlth+trust.prov, 
                    family=quasibinomial, design=svy.des, rescale=T, data=nigeria)
summary(svy.glm.dd)

# Create forest plot of regression results
coef<-coef(svy.glm.dd)
se<-SE(svy.glm.dd)
confint<-confint(svy.glm.dd)

final<-as.data.frame(cbind(coef,se, confint))
final$vars <- c("Intercept","Household has Transportation", "Distance is a Problem","Cost is a Problem", "Going Alone is a Problem", "Woman Makes Decisions about HC",
                "Trust in Provider")
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

# Deep dive into certain constructs - use of care
svy.glm.dd<-svyglm(hb.ever~fp.use.now+use.hc.sick+anc.visit.cat, 
                   family=quasibinomial, design=svy.des, rescale=T, data=nigeria)
summary(svy.glm.dd)

# Create forest plot of regression results
coef<-coef(svy.glm.dd)
se<-SE(svy.glm.dd)
confint<-confint(svy.glm.dd)

final<-as.data.frame(cbind(coef,se, confint))
final$vars <- c("Intercept","Using Modern Contraception", "Currently Pregnant", "Go to Facility when Sick","ANC: 1-3 visits","ANC: 4+ visits", "ANC: DNK")
final<-final[-c(1,3,7), ]
colnames(final)<-c("coef", "se", "lcl", "ucl", "vars")

label<-final$vars
mean<-exp(final$coef)
lower<-exp(final$lcl)
upper<-exp(final$ucl)
index<-1:4
df<-data.frame(label, index, mean, lower, upper)

orplot<-ggplot(df) + 
  geom_point(aes(x=index, y=mean), alpha=0.9, size=4, fill="darkcyan", show.legend = FALSE) +
  geom_errorbar(aes(x=index, y=mean, ymin=lower, ymax=upper), size=1, width=.3, show.legend = FALSE) +
  geom_text(aes(y=mean, x=index, label=round(mean,2)), vjust=-.9, hjust=.5)+
  geom_hline(yintercept=1, lty=2) +
  coord_flip()+
  scale_x_continuous(name = "", breaks=1:nrow(df), labels=df$label)+
  scale_y_continuous(name= "Odds Ratio (95% CI)", breaks=seq(0, 3, .2)) +
  theme(axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

# Predicting preference
svy.glm.pref<-svyglm(pref.home~brth.normz+risky.brthz+anc.visit.cat+preg.know+age.resp+muslim+wom.ed.cat+age.1stbrth+
                      wealth.index+comm.trustz+part.trustz, 
                    family=quasibinomial, design=svy.des, rescale=T, data=nigeria)
summary(svy.glm.pref)


# Looking at state-level dynamics
# Subset 10 target states
north<-nigeria %>% filter(STATE %in% c("BAUCHI   ", "BORNO    ", "KADUNA   ", "KANO     ", "SOKOTO   ",
                                      "YOBE     ", "GOMBE    ", "NASARAWA ", "NIGER    "))
north$STATE<-gsub(" ", "", north$STATE, fixed=F)
nodesign<-svydesign(id=north$PSUclean, strata=north$rural, weights=north$base.wt, survey.lonely.psu="adjust", nest=T, data=north)

tot <- melt(svytable(~new.seg, nodesign))
st <- melt(svytable(~STATE, nodesign))

tot <-data.table(tot) 
setkey(tot,value)

stseg <- melt(svytable(~STATE+new.seg, nodesign))
perc <- merge(stseg,st,by="STATE")
perc$perct <- 100*perc$value.x/perc$value.y
#perc <- perc[perc$value.y>=50,]

theme_set(theme_bw(30))
ggplot(aes(x=STATE),data=perc)+
  geom_col(aes(y=perct, fill=as.factor(new.seg))) + 
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("State")+ ylab("% of Women in each Segment") +ggtitle("") + 
  guides(fill=guide_legend(title="Segment")) + 
  scale_fill_brewer(type="qual", palette = 2) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12), axis.title=element_text(size=12),
        legend.text=element_text(size=12), legend.title=element_text(size=14))

## Ladder plot - pref by seg
svytotal(~STATE, nodesign, na.rm=T)
svyciprop(~pref.home, nodesign, na.rm=T)

hb<-svyby(~hb.young, ~STATE, nodesign, svymean,ci=TRUE,vartype="ci", na.rm=T); as.data.frame(hb)
hb$pref.home<-hb$hb.young*100
hb$ci_l<-hb$ci_l*100
hb$ci_u<-hb$ci_u*100

ggplot(hb) + 
  geom_col(aes(x=reorder(STATE, desc(STATE)), y=pref.home), alpha=0.9, size=5, fill="darkcyan", show.legend = FALSE) +
  geom_errorbar(aes(x=reorder(STATE, desc(STATE)), y=pref.home, ymin=ci_l, ymax=ci_u), size=1, width=.5, show.legend = FALSE) +
  geom_text(aes(y=pref.home, x=STATE, label=round(pref.home,1)), vjust=-1, hjust=.5)+
  coord_flip()+
  xlab("") +
  scale_y_continuous(name = "Percent Prefering Home Birth", breaks=seq(0, 100, 10)) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))

# ANC
anc<-svyby(~anc.4plus, ~STATE, nodesign, svymean,ci=TRUE,vartype="ci", na.rm=T); as.data.frame(anc)
anc$anc.4plus<-anc$anc.4plus*100
anc$ci_l<-anc$ci_l*100
anc$ci_u<-anc$ci_u*100

ggplot(anc) + 
  geom_col(aes(x=reorder(STATE, desc(STATE)), y=anc.4plus), alpha=0.9, size=5, fill="darkcyan", show.legend = FALSE) +
  geom_errorbar(aes(x=reorder(STATE, desc(STATE)), y=anc.4plus, ymin=ci_l, ymax=ci_u), size=1, width=.5, show.legend = FALSE) +
  geom_text(aes(y=anc.4plus, x=STATE, label=round(anc.4plus,1)), vjust=-1, hjust=.5)+
  coord_flip()+
  xlab("") +
  scale_y_continuous(name = "Percent With 4+ ANC Visits", breaks=seq(0, 100, 10)) +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 14), axis.title=element_text(size=14))


svyby(~anyanc, ~new.seg, nodesign, svymean, na.rm=T)
