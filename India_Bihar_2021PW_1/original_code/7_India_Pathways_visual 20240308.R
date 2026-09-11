rm(list=ls())
pacman::p_load("poLCA", "dplyr", "tidyverse", "tidyLPA", "networkD3", "data.table", 
               "ggpubr", "forcats", "scales", "purrr")

setwd("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2021 Pathways/Data/Bihar_constructed_data")

#read in predictors and outcomes file 
vulnerability <- readRDS("1_Bihar_Pathways_vulpredictors.rds")
outcomes <- readRDS("1_Bihar_Pathways_outcomes.rds")

#read in LCA results for for class membership 
LCA2 <- readRDS("LCA2.rds")
LCA3 <- readRDS("LCA3.rds")
LCA4 <- readRDS("LCA4.rds")
LCA5 <- readRDS("LCA5.rds")
LCA6 <- readRDS("LCA6.rds")
LCA7 <- readRDS("LCA7.rds")

################################################## MODEL FIT STATISTICS ########

### BIC 
LCA2$bic
LCA3$bic 
LCA4$bic
LCA5$bic #best classes 
LCA6$bic
LCA7$bic

myds <- c(LCA2$bic,LCA3$bic,LCA4$bic,LCA5$bic,LCA6$bic,LCA7$bic)
x <- (1:length(myds))
#make the main plot
plot(myds, xaxt = "n", type="b", 
     xlab="Solutions", ylab="BIC", main="Bayesian Information Criterion (BIC)")
axis(1, at=1:6, labels=c("2-class","3-class","4-class", 
                         "5-class", "6-class", "7-class"))

### % reaching MLE (round llik to the 10 digit)
#round to the nearest 10
LCA2_percent_mle = sum(round(LCA2$attempts,digits = -1) == round(LCA2$llik,digits = -1))/length(LCA2$attempts)
LCA2_percent_mle <- LCA2_percent_mle*100
LCA3_percent_mle = sum(round(LCA3$attempts,digits = -1) == round(LCA3$llik,digits = -1))/length(LCA3$attempts)
LCA3_percent_mle <- LCA3_percent_mle*100
LCA4_percent_mle = sum(round(LCA4$attempts,digits = -1) == round(LCA4$llik,digits = -1))/length(LCA4$attempts)
LCA4_percent_mle <- LCA4_percent_mle*100
LCA5_percent_mle = sum(round(LCA5$attempts,digits = -1) == round(LCA5$llik,digits = -1))/length(LCA5$attempts)
LCA5_percent_mle <- LCA5_percent_mle*100
LCA6_percent_mle = sum(round(LCA6$attempts,digits = -1) == round(LCA6$llik,digits = -1))/length(LCA6$attempts)
LCA6_percent_mle <- LCA6_percent_mle*100
LCA7_percent_mle = sum(round(LCA7$attempts,digits = -1) == round(LCA7$llik,digits = -1))/length(LCA7$attempts)
LCA7_percent_mle <- LCA7_percent_mle*100

myds <- c(LCA2_percent_mle,LCA3_percent_mle,LCA4_percent_mle,
          LCA5_percent_mle,LCA6_percent_mle,LCA7_percent_mle)
x <- (1:length(myds))
#make the main plot
plot(myds, xaxt = "n", type="b", ylim=c(0, 100), 
     xlab="Solutions", ylab="percent_MLE", main="Maximum likelihood estimation (MLE)")
axis(1, at=1:6, labels=c("2-class","3-class","4-class", 
                         "5-class", "6-class", "7-class"))



colSums(is.na(outcomes))
colSums(is.na(vulnerability))
vulnerability <- vulnerability %>% dplyr::select(
  caseid, 
  his.age,
  age.1stsex.her,
  age.1stpreg.her,
  death.parent,
  bio.parent,
  dist.natal,
  childhood.home.safe,
  her.edulevel,
  his.edulevel,
  caste,
  hindu,
  ipv.accept,
  gender.role,
  hh.size,
  cohab.adult.sum,
  cohab.minor,
  female.help,
  support.first.hh,
  negotiate.preg,
  her.decide.child,
  her.decide.hh,
  bank.own,
  house.stress,
  her.work.year,
  his.work.agri,
  loan,
  house.asset,
  prepare.sex,
  prepare.preg,
  percept.child.sick,
  percept.srh,
  support.first.comm,
  support.comm,
  trust.doctor,
  trust.asha,
  saving.group,
  doctor.distance,
  medical.barrier,
  media,
  smartcell.hh,
  transport.hh,
  climate.impact,
  WASH.water,
  kitchen.vent
  ) %>% drop_na() 
lapply(vulnerability, class)

#recode predictors to binary (for the ease of interpretation)
vulnerability$age.1stsex.her_         <- 1- vulnerability$age.1stsex.her
vulnerability$age.1stpreg.her_        <- 1- vulnerability$age.1stpreg.her
vulnerability$bio.parent_             <- 1- vulnerability$bio.parent
vulnerability$dist.natal_             <- ifelse(vulnerability$dist.natal==1, 0, 1) #not living w. bio.parent
table(vulnerability$dist.natal_  )
vulnerability$childhood.home.safe_    <- 1- vulnerability$childhood.home.safe
vulnerability$her.edulevel_           <- ifelse(vulnerability$her.edulevel==1, 1, 0) #no education
vulnerability$his.edulevel_           <- ifelse(vulnerability$his.edulevel==1, 1, 0) #no education
vulnerability$caste_                  <- ifelse(vulnerability$caste==1, 1, 0) #Scheduled castes/tribes
vulnerability$hh.size_                <- ifelse(vulnerability$hh.size>=3, 1, 0) #hh size greater than 4 (TFR=4)
table(vulnerability$hh.size_ )
vulnerability$female.help_            <- 1- vulnerability$female.help
vulnerability$support.first.hh_       <- 1- vulnerability$support.first.hh
vulnerability$negotiate.preg_         <- ifelse(vulnerability$negotiate.preg==1, 1, 0) #she cannot negotiate
vulnerability$her.decide.child_       <- 1- vulnerability$her.decide.child
vulnerability$her.decide.hh_          <- 1- vulnerability$her.decide.hh
vulnerability$bank.own_               <- 1- vulnerability$bank.own
vulnerability$her.work_               <- ifelse(vulnerability$her.work.year==1, 1, 0) #she does not have work
vulnerability$house.asset_            <- ifelse(vulnerability$house.asset<=3, 1, 0) #poor
vulnerability$bank.own_               <- 1- vulnerability$bank.own
vulnerability$prepare.sex_            <- 1- vulnerability$prepare.sex
vulnerability$prepare.preg_           <- 1- vulnerability$prepare.preg
vulnerability$percept.srh_            <- 1- vulnerability$percept.srh
vulnerability$support.first.comm_     <- 1- vulnerability$support.first.comm
vulnerability$support.comm_           <- 1- vulnerability$support.comm
vulnerability$trust.doctor_           <- 1- vulnerability$trust.doctor
vulnerability$trust.asha_             <- 1- vulnerability$trust.asha
vulnerability$doctor.distance_        <- ifelse(vulnerability$doctor.distance==1, 0, 1) #>15 minutes 
vulnerability$media_                  <- 1- vulnerability$media
vulnerability$smartcell.hh_           <- 1- vulnerability$smartcell.hh
vulnerability$transport.hh_           <- 1- vulnerability$transport.hh
vulnerability$WASH.water_             <- 1- vulnerability$WASH.water
vulnerability$kitchen.vent_           <- 1- vulnerability$kitchen.vent

#recode outcomes  
colnames(outcomes)
outcomes$cohab.kid.sum_               <- ifelse(outcomes$cohab.kid.sum>=3, 1, 0) #higher fertility  
outcomes$anc.total_                   <- ifelse(outcomes$anc.total<4, 1, 0) #less than 4+ ANC visits  
outcomes$fp.ever.modern_              <- 1 - outcomes$fp.ever.modern #never use modern FP 
outcomes$fp.ever.any_                 <- 1 - outcomes$fp.ever.any #never use modern FP 
outcomes$breastfed_                   <- 1 - outcomes$breastfed #breastfed the last child 
outcomes$kid.ill_doctor_              <- 1 - outcomes$kid.ill_doctor #breastfed the last child 
outcomes$her.ill_doctor_              <- 1 - outcomes$her.ill_doctor #breastfed the last child 

#merge w class membership 
vulnerability_class <- cbind(vulnerability,       LCA2_class=LCA2$predclass)
vulnerability_class <- cbind(vulnerability_class, LCA3_class=LCA3$predclass)
vulnerability_class <- cbind(vulnerability_class, LCA4_class=LCA4$predclass)
vulnerability_class <- cbind(vulnerability_class, LCA5_class=LCA5$predclass)
vulnerability_class <- cbind(vulnerability_class, LCA6_class=LCA6$predclass)
vulnerability_class <- cbind(vulnerability_class, LCA7_class=LCA7$predclass)

#merge with outcomes 
vuloutcomes <- merge(vulnerability_class, outcomes, by="caseid")
#save for mapping 
save(vuloutcomes, file = "vuloutcomes.RData")



#a class only file 
class <- vulnerability_class[76:81]

colnames(class) <- c("cluster_k2", "cluster_k3", "cluster_k4", "cluster_k5", "cluster_k6", "cluster_k7")
class <- as.data.table(class)

#make sankey plots 
make_sankey <- function(data){
  links <- lapply(2:6, function(i){
    link.i<-  data[,.(value = .N),.(source = paste0("k", i, "_", get(paste0("cluster_k", i))), target = paste0("k", i+1, "_",get(paste0("cluster_k", i+1))))]
    return(link.i)
  }) %>% rbindlist(use.names = T, fill = T)
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(
    name=c(as.character(links$source), 
           as.character(links$target)) %>% unique()
  )
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  # Make the Network
  p <- sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", 
                     sinksRight=FALSE)
  return(p)
}

sankey_bihar <- make_sankey(class)
sankey_bihar

link.2<-  class[,.(value = .N),.(source = paste0("k", 2, "_", get(paste0("cluster_k", 2))), target = paste0("k", 2+1, "_",get(paste0("cluster_k", 2+1))))]

##example, 5-class solution class k5_3 split into 6-class k6_4, and k6_3
#just plot simple descriptives of these two groups and see what differs them (outcomes and descriptives)
vuloutcomes_k5_3 <- vuloutcomes %>% filter(LCA5_class==3)
vuloutcomes_k6_4 <- vuloutcomes %>% filter(LCA6_class==4)
vuloutcomes_k6_3 <- vuloutcomes %>% filter(LCA6_class==3)

outcomes <- c(
  'anc.total_',
  'fp.ever.modern_',
  'fp.ever.any_',
  'deliver.home',
  'kid.ill_doctor_',
  'her.ill_doctor_',
  'cohab.kid.sum_',
  'preg.loss',  
  'her.ill_3mo',
  'her.wasting',
  'breastfed_',
  'kid.wasting',
  'kid.stunt',
  'death.u5',
  'kid.ill_3mo',
  'kid.hosp')
vuloutcomes_k5_3_outcomes <- vuloutcomes_k5_3[,outcomes]
vuloutcomes_k6_4_outcomes <- vuloutcomes_k6_4[,outcomes]
vuloutcomes_k6_3_outcomes <- vuloutcomes_k6_3[,outcomes]
k5_3_outcomes <- lapply(vuloutcomes_k5_3_outcomes, mean, na.rm=T)
k6_4_outcomes <- lapply(vuloutcomes_k6_4_outcomes, mean, na.rm=T)
k6_3_outcomes <- lapply(vuloutcomes_k6_3_outcomes, mean, na.rm=T)
k5_3_outcomes <- as.data.frame(k5_3_outcomes)
k6_4_outcomes <- as.data.frame(k6_4_outcomes)
k6_3_outcomes <- as.data.frame(k6_3_outcomes)
df1 <- rbind(k5_3_outcomes, k6_4_outcomes, k6_3_outcomes)

predictors <- c(
  'age.1stsex.her_',
  'age.1stpreg.her_',
  'death.parent',
  'bio.parent_',
  'dist.natal_',
  'childhood.home.safe_',
  'her.edulevel_',
  'his.edulevel_',
  'caste_',
  'hindu',
  'ipv.accept',
  'gender.role',
  'hh.size_',
  'female.help_',
  'support.first.hh_',
  'negotiate.preg_',
  'her.decide.child_',
  'her.decide.hh_',
  'bank.own_',
  'house.stress',
  'her.work_',
  'loan',
  'house.asset_',
  'prepare.sex_',
  'prepare.preg_',
  'percept.srh_',
  'support.first.comm_',
  'support.comm_',
  'trust.doctor_',
  'trust.asha_',
  'saving.group',
  'doctor.distance_',
  'medical.barrier',
  'media_',
  'smartcell.hh_',
  'transport.hh_',
  'climate.impact',
  'WASH.water_',
  'kitchen.vent_')
vuloutcomes_k5_3_predictors <- vuloutcomes_k5_3[,predictors]
vuloutcomes_k6_4_predictors <- vuloutcomes_k6_4[,predictors]
vuloutcomes_k6_3_predictors <- vuloutcomes_k6_3[,predictors]
k5_3_predictors <- lapply(vuloutcomes_k5_3_predictors, mean, na.rm=T)
k6_4_predictors <- lapply(vuloutcomes_k6_4_predictors, mean, na.rm=T)
k6_3_predictors <- lapply(vuloutcomes_k6_3_predictors, mean, na.rm=T)
k5_3_predictors <- as.data.frame(k5_3_predictors)
k6_4_predictors <- as.data.frame(k6_4_predictors)
k6_3_predictors <- as.data.frame(k6_3_predictors)
df2 <- rbind(k5_3_predictors, k6_4_predictors, k6_3_predictors)
summary(vuloutcomes$hh.size[vuloutcomes$LCA5_class==3])
summary(vuloutcomes$hh.size[vuloutcomes$LCA6_class==4])
summary(vuloutcomes$hh.size[vuloutcomes$LCA6_class==3])
summary(vuloutcomes$cohab.kid.sum[vuloutcomes$LCA5_class==3])
summary(vuloutcomes$cohab.kid.sum[vuloutcomes$LCA6_class==4])
summary(vuloutcomes$cohab.kid.sum[vuloutcomes$LCA6_class==3])

#########################
#### FIG 1: PIE CHART ###
#########################

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank()
  )

count <- class %>% dplyr::group_by(cluster_k5) %>% 
  dplyr::summarise(total_count=n(),
            .groups = 'drop')
count$class <- paste("Class ", count$cluster_k5)
table(count$class)
prop.table(table(class$cluster_k5))
# Barplot
ggplot(count, aes(x="", y=total_count, fill=class, alpha=0.4)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_fill_manual(values=c("blue", "darkgreen", "purple", "red", "orange")) +
  blank_theme +
  theme(axis.text.x=element_blank()) +  
  theme(legend.position="none")
  #geom_text(aes(y = total_count/8 + c(0, cumsum(total_count)[-length(total_count)]), 
  #              label = percent(total_count/1886)), size=6, color="white", fontface = "bold")


####################################
###  FIG 2: OUTCOMES DESCRIPTION ###
####################################

colnames(outcomes)
allclass <- vuloutcomes[,outcomes]
class1 <- vuloutcomes %>% filter(LCA5_class==1)
class2 <- vuloutcomes %>% filter(LCA5_class==2)
class3 <- vuloutcomes %>% filter(LCA5_class==3)
class4 <- vuloutcomes %>% filter(LCA5_class==4)
class5 <- vuloutcomes %>% filter(LCA5_class==5)
class1 <- class1[,outcomes]
class2 <- class2[,outcomes]
class3 <- class3[,outcomes]
class4 <- class4[,outcomes]
class5 <- class5[,outcomes]

##variables to plot percentages (50% might be a very good ref)
#first calculate group avgs 
all.means <- colMeans(allclass, na.rm=T)
class1.means <- colMeans(class1, na.rm=T)
class2.means <- colMeans(class2, na.rm=T)
class3.means <- colMeans(class3, na.rm=T)
class4.means <- colMeans(class4, na.rm=T)
class5.means <- colMeans(class5, na.rm=T)

all.means <- as.data.frame(all.means)
class1.means <- as.data.frame(class1.means)
class2.means <- as.data.frame(class2.means)
class3.means <- as.data.frame(class3.means)
class4.means <- as.data.frame(class4.means)
class5.means <- as.data.frame(class5.means)

names <- colnames(allclass)
names <- as.data.frame(names)

df <- cbind(names, all.means, 
            class1.means, class2.means, class3.means, class4.means, class5.means)
df$all.means    <- 100*df$all.means
df$class1.means <- 100*df$class1.means
df$class2.means <- 100*df$class2.means
df$class3.means <- 100*df$class3.means
df$class4.means <- 100*df$class4.means
df$class5.means <- 100*df$class5.means

##diff from the sample mean 
df$class1.mean_diff <- 100* (df$class1.means - df$all.means) / df$all.means
df$class2.mean_diff <- 100* (df$class2.means - df$all.means) / df$all.means
df$class3.mean_diff <- 100* (df$class3.means - df$all.means) / df$all.means
df$class4.mean_diff <- 100* (df$class4.means - df$all.means) / df$all.means
df$class5.mean_diff <- 100* (df$class5.means - df$all.means) / df$all.means

class1 <- df %>% dplyr::select(names, class1.mean_diff)
class2 <- df %>% dplyr::select(names, class2.mean_diff)
class3 <- df %>% dplyr::select(names, class3.mean_diff)
class4 <- df %>% dplyr::select(names, class4.mean_diff)
class5 <- df %>% dplyr::select(names, class5.mean_diff)

class1$class <- 1
class2$class <- 2
class3$class <- 3
class4$class <- 4
class5$class <- 5

colnames(class1) <- c("names", "mean_diff", "class")
colnames(class2) <- c("names", "mean_diff", "class")
colnames(class3) <- c("names", "mean_diff", "class")
colnames(class4) <- c("names", "mean_diff", "class")
colnames(class5) <- c("names", "mean_diff", "class")

#preparing datasets 
df_long <- rbind(class1, class2, class3, class4, class5)

varlabs <- c(
  '<4 ANC visits',
  'Never used modern contraceptives',
  "Never used any contraceptives", 
  'Ever delivered home birth',
  'Do not seek medical care when child was sick',
  'Do not seek medical care when she was sick',
  'Living with >=3 biological children',
  'Experience of pregnancy loss',
  'She was ill in the last 3mos',
  'She is wasted',
  'Breastfed',
  'Child wasted',
  'Child stunted',
  'Under-five death',
  'Child was ill in ths last 3mos',
  'Child was hospitalized'
)

table(df$names)

### Grey out all class
p <- ggplot(data=df_long, aes(x=fct_inorder(names), y=mean_diff, group=class)) +
  geom_line(aes(color=factor(class)), lwd=2) +
  ylim(-60, 60) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 8.5) +
  geom_vline(xintercept = 10.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10)) + 
  xlab("Outcomes") +
  ylab("% difference from the sample mean") + 
  scale_color_manual(values=c("lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey")) +
  scale_x_discrete(labels=varlabs)
p

### Descriptives: in original scale 
p <- ggplot(data=df, aes(x=fct_inorder(names), y=all.means)) +
  geom_bar(stat="identity") +
  ylim(0, 100) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10)) + 
  xlab("Outcomes") +
  ylab("Percentage of women") +
  geom_hline(yintercept = 50, color="red") +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 8.5) +
  geom_vline(xintercept = 10.5) +
  scale_x_discrete(labels=varlabs) +
  ggtitle("All 5 classes")
p 

### ALL Groups together: % from sample mean 
p <- ggplot(data=df_long, aes(x=fct_inorder(names), y=mean_diff, group=class)) +
  geom_line(aes(color=factor(class)), lwd=2, alpha=0.4) +
  ylim(-60, 60) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 8.5) +
  geom_vline(xintercept = 10.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10)) + 
  xlab("Outcomes") +
  ylab("% difference from the sample mean") + 
  scale_color_manual(values=c("blue", "darkgreen", "purple", "red", "orange")) +
  scale_x_discrete(labels=varlabs)
p

### highlighting only one class
p <- ggplot(data=df_long, aes(x=fct_inorder(names), y=mean_diff, group=class)) +
  geom_line(aes(color=factor(class)), lwd=2, alpha=0.4) +
  ylim(-60, 60) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 8.5) +
  geom_vline(xintercept = 10.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10)) + 
  xlab("Outcomes") +
  ylab("% difference from the sample mean") + 
  scale_color_manual(values=c("blue", "lightgrey", "lightgrey", "lightgrey", "lightgrey")) +
  scale_x_discrete(labels=varlabs)
p

### highlighting only one class
p <- ggplot(data=df_long, aes(x=fct_inorder(names), y=mean_diff, group=class)) +
  geom_line(aes(color=factor(class)), lwd=2, alpha=0.4) +
  ylim(-60, 60) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 8.5) +
  geom_vline(xintercept = 10.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10)) + 
  xlab("Outcomes") +
  ylab("% difference from the sample mean") + 
  scale_color_manual(values=c("lightgrey", "darkgreen", "lightgrey", "lightgrey", "lightgrey")) +
  scale_x_discrete(labels=varlabs)
p


### highlighting only one class
p <- ggplot(data=df_long, aes(x=fct_inorder(names), y=mean_diff, group=class)) +
  geom_line(aes(color=factor(class)), lwd=2, alpha=0.4) +
  ylim(-60, 60) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 8.5) +
  geom_vline(xintercept = 10.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10)) + 
  xlab("Outcomes") +
  ylab("% difference from the sample mean") + 
  scale_color_manual(values=c("lightgrey", "lightgrey", "purple", "lightgrey", "lightgrey")) +
  scale_x_discrete(labels=varlabs)
p

### highlighting only one class
p <- ggplot(data=df_long, aes(x=fct_inorder(names), y=mean_diff, group=class)) +
  geom_line(aes(color=factor(class)), lwd=2, alpha=0.4) +
  ylim(-60, 60) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 8.5) +
  geom_vline(xintercept = 10.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10)) + 
  xlab("Outcomes") +
  ylab("% difference from the sample mean") + 
  scale_color_manual(values=c("lightgrey", "lightgrey", "lightgrey", "red", "lightgrey")) +
  scale_x_discrete(labels=varlabs)
p

### highlighting only one class
p <- ggplot(data=df_long, aes(x=fct_inorder(names), y=mean_diff, group=class)) +
  geom_line(aes(color=factor(class)), lwd=2, alpha=0.4) +
  ylim(-60, 60) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 8.5) +
  geom_vline(xintercept = 10.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10)) + 
  xlab("Outcomes") +
  ylab("% difference from the sample mean") + 
  scale_color_manual(values=c("lightgrey", "lightgrey", "lightgrey", "lightgrey", "orange")) +
  scale_x_discrete(labels=varlabs)
p



#####################################
###  FIG 3: PREDICTOR DESCRIPTION ###
#####################################
allclass <- vuloutcomes[,predictors]
class1 <- vuloutcomes %>% filter(LCA5_class==1)
class2 <- vuloutcomes %>% filter(LCA5_class==2)
class3 <- vuloutcomes %>% filter(LCA5_class==3)
class4 <- vuloutcomes %>% filter(LCA5_class==4)
class5 <- vuloutcomes %>% filter(LCA5_class==5)
class1 <- class1[,predictors]
class2 <- class2[,predictors]
class3 <- class3[,predictors]
class4 <- class4[,predictors]
class5 <- class5[,predictors]

##variables to plot percentages (50% might be a very good ref)
#first calculate group avgs 
all.means <- colMeans(allclass)
class1.means <- colMeans(class1)
class2.means <- colMeans(class2)
class3.means <- colMeans(class3)
class4.means <- colMeans(class4)
class5.means <- colMeans(class5)

all.means <- as.data.frame(all.means)
class1.means <- as.data.frame(class1.means)
class2.means <- as.data.frame(class2.means)
class3.means <- as.data.frame(class3.means)
class4.means <- as.data.frame(class4.means)
class5.means <- as.data.frame(class5.means)

names <- colnames(allclass)
names <- as.data.frame(names)

df <- cbind(names, all.means, 
            class1.means, class2.means, class3.means, class4.means, class5.means)
df$all.means    <- 100*df$all.means
df$class1.means <- 100*df$class1.means
df$class2.means <- 100*df$class2.means
df$class3.means <- 100*df$class3.means
df$class4.means <- 100*df$class4.means
df$class5.means <- 100*df$class5.means

##diff from the sample mean a
df$class1.mean_diff <- 100* (df$class1.means - df$all.means) / df$all.means
df$class2.mean_diff <- 100* (df$class2.means - df$all.means) / df$all.means
df$class3.mean_diff <- 100* (df$class3.means - df$all.means) / df$all.means
df$class4.mean_diff <- 100* (df$class4.means - df$all.means) / df$all.means
df$class5.mean_diff <- 100* (df$class5.means - df$all.means) / df$all.means

class1 <- df %>% dplyr::select(names, class1.mean_diff)
class2 <- df %>% dplyr::select(names, class2.mean_diff)
class3 <- df %>% dplyr::select(names, class3.mean_diff)
class4 <- df %>% dplyr::select(names, class4.mean_diff)
class5 <- df %>% dplyr::select(names, class5.mean_diff)

class1$class <- 1
class2$class <- 2
class3$class <- 3
class4$class <- 4
class5$class <- 5

colnames(class1) <- c("names", "mean_diff", "class")
colnames(class2) <- c("names", "mean_diff", "class")
colnames(class3) <- c("names", "mean_diff", "class")
colnames(class4) <- c("names", "mean_diff", "class")
colnames(class5) <- c("names", "mean_diff", "class")

#preparing datasets 
df_long <- rbind(class1, class2, class3, class4, class5)

varlabs <- c(
  "First sex < 18 years", #age.1stsex.her_,
  "First preg < 18 years", #age.1stpreg.her_,
  "Experience of parental death", #death.parent,
  "Not raised by biological parents", #bio.parent_,
  "Not living with biological parents", #dist.natal_,
  "Grew up in an unsafe home", #childhood.home.safe_,
  "She has no education", #her.edulevel_,
  "He has no education", #his.edulevel_,
  "Scheduled castes/tribes", #caste_,
  "Hindu", 
  "Justifying wife-beating", #ipv.accept,
  "Conforming to traditional gender norms", #gender.role,
  "hh size >4", #hh.size_,
  "No other women can help sick child", #female.help_,
  "No hh member in 1st trust cycle", #support.first.hh_,
  "Cannot negotiate pregnancy", #negotiate.preg_,
  "Cannot decide child health", #her.decide.child_,
  "Cannot decide hh big ticket decisions", #her.decide.hh_,
  "Does not own a bank account", #bank.own_,
  "Household stressors", #house.stress,
  "Has no work", #her.work_,
  "Has loan", #loan,
  "House asset: <8", #wealth.quint_,
  "Not prepared for first sex", #prepare.sex_,
  "Not prepared for first preg", #prepare.preg_,
  "SRH misinformation", #percept.srh_,
  "No community member in 1st trust cycle", #support.first.comm_,
  "Community isolation", #support.comm_,
  "Don't trust doctor", #trust.doctor_,
  "Don't trust asha", #trust.asha_,
  "Join women's saving group", #saving.group,
  ">15 min away from doctors", #doctor.distance_,
  "Having barrier accessing healthcare", #medical.barrier,
  "No media exposure", #media_,
  "No hh member owns a smartphone", #smartcell.hh_,
  "No hh member owns a transport vehicle", #transport.hh_,
  "Perceived impact of climate change", #climate.impact,
  "No WASH water", #WASH.water_,
  "No kitchen ventilation" #kitchen.vent_
)

## descriptives 
table(df$names)
ggplot(data=df, aes(x=fct_inorder(names), y=all.means)) +
  geom_bar(stat="identity") +
  ylim(0, 100) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=8)) + 
  xlab("Predictors") +
  ylab("Percentage of women") +
  geom_hline(yintercept = 50, color="red") +
  geom_vline(xintercept = 12.5) +
  geom_vline(xintercept = 20.5) +
  geom_vline(xintercept = 23.5) +
  geom_vline(xintercept = 26.5) +
  geom_vline(xintercept = 31.5) +
  scale_x_discrete(labels=varlabs) +
  ggtitle("All 5 classes")

### ALL Groups together: group comparison 
p <- ggplot(data=df_long, aes(x=fct_inorder(names), y=mean_diff, group=class)) +
  geom_line(aes(color=factor(class)), lwd=2) +
  ylim(-200, 200) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 12.5) +
  geom_vline(xintercept = 20.5) +
  geom_vline(xintercept = 23.5) +
  geom_vline(xintercept = 26.5) +
  geom_vline(xintercept = 31.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=8)) + 
  xlab("Predictors") +
  ylab("% difference from the sample mean") + 
  scale_color_manual(values=c("lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey")) +
  scale_x_discrete(labels=varlabs)
p

### ALL Groups together: group comparison 
p <- ggplot(data=df_long, aes(x=fct_inorder(names), y=mean_diff, group=class)) +
  geom_line(aes(color=factor(class)), lwd=2, alpha=0.4) +
  ylim(-200, 200) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 12.5) +
  geom_vline(xintercept = 20.5) +
  geom_vline(xintercept = 23.5) +
  geom_vline(xintercept = 26.5) +
  geom_vline(xintercept = 31.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=8)) + 
  xlab("Predictors") +
  ylab("% difference from the sample mean") + 
  scale_color_manual(values=c("blue", "darkgreen", "purple", "red", "orange")) +
  scale_x_discrete(labels=varlabs)
p

### highlight only one class
p <- ggplot(data=df_long, aes(x=fct_inorder(names), y=mean_diff, group=class)) +
  geom_line(aes(color=factor(class)), lwd=2, alpha=0.4) +
  ylim(-200, 200) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 12.5) +
  geom_vline(xintercept = 20.5) +
  geom_vline(xintercept = 23.5) +
  geom_vline(xintercept = 26.5) +
  geom_vline(xintercept = 31.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=8)) + 
  xlab("Predictors") +
  ylab("% difference from the sample mean") + 
  scale_color_manual(values=c("blue", "lightgrey", "lightgrey", "lightgrey", "lightgrey")) +
  scale_x_discrete(labels=varlabs) +
  aes(group=rev(class))
p

### highlight only one class
p <- ggplot(data=df_long, aes(x=fct_inorder(names), y=mean_diff, group=class)) +
  geom_line(aes(color=factor(class)), lwd=2, alpha=0.4) +
  ylim(-200, 200) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 12.5) +
  geom_vline(xintercept = 20.5) +
  geom_vline(xintercept = 23.5) +
  geom_vline(xintercept = 26.5) +
  geom_vline(xintercept = 31.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=8)) + 
  xlab("Predictors") +
  ylab("% difference from the sample mean") + 
  scale_color_manual(values=c("lightgrey", "darkgreen", "lightgrey", "lightgrey", "lightgrey")) +
  scale_x_discrete(labels=varlabs) +
  aes(group=rev(class))
p

### highlight only one class
p <- ggplot(data=df_long, aes(x=fct_inorder(names), y=mean_diff, group=class)) +
  geom_line(aes(color=factor(class)), lwd=2, alpha=0.4) +
  ylim(-200, 200) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 12.5) +
  geom_vline(xintercept = 20.5) +
  geom_vline(xintercept = 23.5) +
  geom_vline(xintercept = 26.5) +
  geom_vline(xintercept = 31.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=8)) + 
  xlab("Predictors") +
  ylab("% difference from the sample mean") + 
  scale_color_manual(values=c("lightgrey", "lightgrey", "purple", "lightgrey", "lightgrey")) +
  scale_x_discrete(labels=varlabs) +
  aes(group=rev(class))
p

### highlight only one class
p <- ggplot(data=df_long, aes(x=fct_inorder(names), y=mean_diff, group=class)) +
  geom_line(aes(color=factor(class)), lwd=2, alpha=0.4) +
  ylim(-200, 200) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 12.5) +
  geom_vline(xintercept = 20.5) +
  geom_vline(xintercept = 23.5) +
  geom_vline(xintercept = 26.5) +
  geom_vline(xintercept = 31.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=8)) + 
  xlab("Predictors") +
  ylab("% difference from the sample mean") + 
  scale_color_manual(values=c("lightgrey", "lightgrey", "lightgrey", "red", "lightgrey")) +
  scale_x_discrete(labels=varlabs) +
  aes(group=rev(class))
p

### highlight only one class
p <- ggplot(data=df_long, aes(x=fct_inorder(names), y=mean_diff, group=class)) +
  geom_line(aes(color=factor(class)), lwd=2, alpha=0.4) +
  ylim(-200, 200) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 12.5) +
  geom_vline(xintercept = 20.5) +
  geom_vline(xintercept = 23.5) +
  geom_vline(xintercept = 26.5) +
  geom_vline(xintercept = 31.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=8)) + 
  xlab("Predictors") +
  ylab("% difference from the sample mean") + 
  scale_color_manual(values=c("lightgrey", "lightgrey", "lightgrey", "lightgrey", "orange")) +
  scale_x_discrete(labels=varlabs) +
  aes(group=rev(class))
p
