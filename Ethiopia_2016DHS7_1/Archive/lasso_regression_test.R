#Adi Greif note 3/7: we decided to use PCA analysis instead of lasso

library(gmodels)
library(caret)
library(gbm)
library(dplyr)
library(tidyr)

setwd("C:/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways - Gender Equality Division")
dhs.seg <- readRDS("Modeling/Data/Ethiopia DHS/ET_2016/dhs.seg.rds")

dhs.seg<-subset(dhs.seg, hh.urban==1)

#Lasso regression to reduce data dimentionality
ctrlspecs<-trainControl(method="cv", number=10, savePredictions="all")
lambda_vec<-10^seq(5, -5, length=500)

set.seed(98040)

### Specify/train model
# "underweight", "wom.hlthck", "hb.1", "fp.mod.ever", "anc.4plus","anyanc","anc.1sttri",
# "baby.hlthck", "stunt.cat2.last","waste.cat2.last", "zerodose.yn", "dpt1.yn", "u1mort.yn", "u5mort.yn", "stl.cnt"

# LASSO 1
data <- dhs.seg %>% select(c("underweight","ever.partnered.yn",
                             "early.cohab.15",
                             "early.sex.15", "early.birth.15",
                             "preg.4plus",
                             "child.4plus"))

# For logistic models, outcome must be a factor, I've forced it below, but may not need to if variable is already a factor in R
glm1<-train(as.factor(underweight)~., 
                data=data,
                preProcess=c("center","scale"),
                method="glmnet",
                tueGrid=expand.grid(alpha=1, lambda=lambda_vec),
                family="binomial",
                trControl=ctrlspecs,
                na.action=na.omit)

glm1$bestTune;glm1$results$Accuracy
ggplot(varImp(glm1)) + theme(axis.text = element_text(size = 15), axis.title = element_text(size = 20))

# Predictive ability of model
data2<-subset(data, !is.na(data$hb.1))
index<-createDataPartition(data2$hb.1, p=.8, list=F, times=1)
train_df<-data2[index,]
test_df<-data2[-index,]
test_df<-test_df[complete.cases(test_df), ]

# k-fold CV
ctrlspecs<-trainControl(method="cv", number=10, savePredictions="all")

glm2<-train(as.factor(hb.1)~., 
                 data=train_df,
                 method="glmnet",
                 family="binomial",
                 trControl=ctrlspecs,
                 na.action=na.omit)

### Model prediction
pred1<-predict(glm2, newdata=test_df)
confusionMatrix(data=pred1, as.factor(test_df$hb.1))

####### LASSO 2
data <- dhs.seg %>% select(c("u5mort.yn","jd.index.5plus",
                             "wd.index.none",
                             "husb.ed.mtprim",
                             "part.nochores",
                             "med.permis"))

# For logistic models, outcome must be a factor, I've forced it below, but may not need to if variable is already a factor in R
glm1<-train(as.factor(u5mort.yn)~., 
            data=data,
            preProcess=c("center","scale"),
            method="glmnet",
            tueGrid=expand.grid(alpha=1, lambda=lambda_vec),
            family="binomial",
            trControl=ctrlspecs,
            na.action=na.omit)

glm1$bestTune;glm1$results$Accuracy
ggplot(varImp(glm1)) + theme(axis.text = element_text(size = 15), axis.title = element_text(size = 20))

####### LASSO 3
data <- dhs.seg %>% select(c("u5mort.yn",
                             "ever.partnered.yn",
                             "polygamy",
                             "partner.absent.yn",
                             "head.sex",
                             "kids.under5.2plus",
                             "num.15up.4plus",
                             "num.under15.3plus",
                             "hh.kidwom.rat.2plus"))

# For logistic models, outcome must be a factor, I've forced it below, but may not need to if variable is already a factor in R
glm1<-train(as.factor(u5mort.yn)~., 
            data=data,
            preProcess=c("center","scale"),
            method="glmnet",
            tueGrid=expand.grid(alpha=1, lambda=lambda_vec),
            family="binomial",
            trControl=ctrlspecs,
            na.action=na.omit)

glm1$bestTune;glm1$results$Accuracy
ggplot(varImp(glm1)) + theme(axis.text = element_text(size = 15), axis.title = element_text(size = 20))

####### LASSO 4
dhs.seg$working.yn<-ifelse(dhs.seg$working=="Currently working" | dhs.seg$working=="Worked in the past year",1,0)
data <- dhs.seg %>% select(c("wom.hlthck", "work.type.prof",
                             "working.yn",
                             "hh.electricity",
                             "hh.land",
                             "hh.animal",
                             "hh.bank.acct",
                             "wealth.index.poor"))

# For logistic models, outcome must be a factor, I've forced it below, but may not need to if variable is already a factor in R
glm1<-train(as.factor(wom.hlthck)~., 
            data=data,
            preProcess=c("center","scale"),
            method="glmnet",
            tueGrid=expand.grid(alpha=1, lambda=lambda_vec),
            family="binomial",
            trControl=ctrlspecs,
            na.action=na.omit)

glm1$bestTune;glm1$results$Accuracy
ggplot(varImp(glm1)) + theme(axis.text = element_text(size = 15), axis.title = element_text(size = 20))

