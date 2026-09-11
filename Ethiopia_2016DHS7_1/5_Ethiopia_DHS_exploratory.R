###################################################################
########          5_Ethiopia_DHS_exploratory          ###############
######## Vulnerability measures exploratory analysis ##############
########              Ethiopia DHS 2016              ##############
########              Author: Darcy Rao              ##############
########         Purpose: test relationship vulnerabilities and outcomes ######### 
#######          Last update: Feb 16, 2023 AG added pastoralism ###
########                       Feb 24, AM edited pastoralism  ###
#######                        Feb 28, AG changed filepaths ###########
########                       section to drop variables not in dhs.seg ######
########                       See comments marked AM to follow-up for consistency of flow #####
########                                             ##############
######## input data files: 4_Ethiopia_DHS_vulnerability, 1_Ethiopia_DHS_outcomes
######## output data files: "assoc_summary.rds", "dhs.seg", "dhs.seg.des"               ###########
######## ancestor scripts: 1_Ethiopia_DHS_outcomes, 4_Ethiopia_DHS_vulnerability_measures  ##
######## descendant scripts: script 5 and 6 is exploratory, 7_Ethiopia_DHS_construct_mapping ##
######## Note: need to set directory for each user
######## Note: this script requires changing variables by hand and rerunning code. Variables selected are those in the exploratory_analysis_external.xls
######## Note: There is some code that needs to be done the first round through, see comments 
###################################################################


#### Setup ####

## Load packages
rm(list=ls())
pacman::p_load("tidyverse", "knitr", "data.table", "lmtest", "logbin", "survey")


## Set the working directory - need to change for each user
#setwd("/Users/darcyrao/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Pathways - GE Data & Insights/Modeling") # for use from Darcy's Mac
#setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling") # for use from Anu's PC
#setwd("/Users/adigreif/Desktop/Gates Ethiopia/EthiopiaSegmentation/")

## Import the vulnerability measures and merge with outcomes
vulnerability <- readRDS("Data/Ethiopia_constructed_data/4_Ethiopia_DHS_vulnerability.rds")
#OLD load("Data/Ethiopia DHS/ET_2016/Ethiopia_DHS_outcomes.RData") #for use from Anu's PC
load("Data/Ethiopia_constructed_data/1_Ethiopia_DHS_outcomes.RData") #for use from Anu's PC

dhs.seg <- merge(vulnerability, outcomes, by="caseid", all.x=T)


#### Define categorical age variable for adjustment ####

## Cutoffs defined based on exploratory analysis with outcomes
### ==> update based on exploratory analysis if needed
dhs.seg$age.adj <- cut(dhs.seg$age.yrs,
                       breaks = c(14, 19, 24, 29, 34, 39, 44, 49), 
                       labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))


# Group outcomes
outcomes.list <- c("underweight", "wom.hlthck", "hb.1", "fp.mod.ever", "anc.4plus","anyanc","anc.num", "anc.1sttri",
                  "baby.hlthck", "breastfed",	"breastfed2",	"breastfed2.excl", "stunt.cat2.last","waste.cat2.last", "haz.last", "waz.last", "whz.last", 
                  "undwgt.last", "ovrwgt.last", "zerodose.yn", "dpt1.yn", "u1mort.yn", "u5mort.yn", "stl.cnt")

  # Exclude anc.1stvisit for now because majority are coded as "no visits prior to birth" and we already have any.anc and anc.1sttri

# Exclude anc.1stvisit for now because majority are coded as "no visits prior to birth" and we already have any.anc and anc.1sttri


## Set survey design matrix
dhs.seg.des <-svydesign(id=~v021, strata=~v023, weights=~wt, data=dhs.seg)

## Save the datasets
saveRDS(dhs.seg, file = "Data/Ethiopia_constructed_data/5_Ethiopia_DHS_exploratory.rds")
saveRDS(dhs.seg.des, file = "Data/Ethiopia_constructed_data/5_Ethiopia_DHS_exploratory.des.rds")

#### Define functions ####

# Define function to explore 

f_pattern_explore <- function(measure, outcome, poisson = FALSE, age.adjust = TRUE){

  # Indicator if outcome is binary
  binary.exp <- all(na.omit(dhs.seg %>% pull(eval(parse(text=measure)))) %in% 0:1)
  binary.out <- all(na.omit(dhs.seg %>% pull(eval(parse(text=outcome)))) %in% 0:1)
  
  
  # Look at summary stats (paste0 concatenates into string. pull takes one column)
  if(binary.exp == FALSE & (is.integer(dhs.seg %>% pull(eval(parse(text=measure)))) | is.numeric(dhs.seg %>% pull(eval(parse(text=measure)))))) {
    stats <- svyby(as.formula(paste0("~",measure)), as.formula(paste0("~",outcome)), dhs.seg.des, svymean,ci=TRUE,vartype="ci", na.rm=T)
  } else if (binary.out == FALSE & (is.integer(dhs.seg %>% pull(eval(parse(text=outcome)))) | is.numeric(dhs.seg %>% pull(eval(parse(text=outcome)))))) {
    stats <-svyby(as.formula(paste0("~",outcome)), as.formula(paste0("~",measure)), dhs.seg.des, svymean,ci=TRUE,vartype="ci", na.rm=T)
  } else {
    stats <- prop.table(svytable(as.formula(paste0("~",outcome,"+",measure)), dhs.seg.des), margin = 2)
  }

  # Test association - age adjustment
  if(age.adjust == TRUE) {
    if(binary.out == FALSE){
      if(poisson == TRUE) {
        svy.glm <- svyglm(as.formula(paste0(outcome, "~", measure, "+", "age.adj")),family=quasipoisson,design=dhs.seg.des, data=dhs.seg)
      }else{
        svy.glm <- svyglm(as.formula(paste0(outcome, "~", measure, "+", "age.adj")),family=gaussian(link ="identity"),design=dhs.seg.des, data=dhs.seg)
      }
    }else{
      svy.glm<-svyglm(as.formula(paste0(outcome, "~", measure, "+", "age.adj")),family=quasibinomial,design=dhs.seg.des, data=dhs.seg)
    }
  }

  # Test association - no age adjustment
  if(age.adjust == FALSE) {
    if(binary.out == FALSE){
      if(poisson == TRUE) {
        svy.glm <- svyglm(as.formula(paste0(outcome, "~", measure)),family=quasipoisson,design=dhs.seg.des, data=dhs.seg)
      }else{
        svy.glm <- svyglm(as.formula(paste0(outcome, "~", measure)),family=gaussian(link ="identity"),design=dhs.seg.des, data=dhs.seg)
      }
    }else{
      svy.glm<-svyglm(as.formula(paste0(outcome, "~", measure)),family=quasibinomial,design=dhs.seg.des, data=dhs.seg)
    }
  }
    
  
  # Model summary 
  summary <- as.data.frame(summary(svy.glm)$coefficients)
  summary$sig <-  ifelse(summary$`Pr(>|t|)`<0.001, "xxx", ifelse(summary$`Pr(>|t|)`<0.01, "xx", ifelse(summary$`Pr(>|t|)`<0.05, "x", ifelse(is.na(summary$`Pr(>|t|)`), "", ""))))
  summary$sig[1] <- ""
  
  LRT.pval.measure <- regTermTest(svy.glm, measure, method = "LRT")$p
  if(age.adjust == TRUE){
  LRT.pval.age <- regTermTest(svy.glm, "age.adj", method = "LRT")$p
  } else {
    LRT.pval.age <- NA
  } #cbind drops zero row and zero column, combines rows or columns
  LRT.pval <- cbind.data.frame("measure" = LRT.pval.measure, "age" = LRT.pval.age)
  
  # Plot coefficients
  if(age.adjust == TRUE){
    ncoefs <- length(svy.glm$coefficients) - (length(na.omit(unique(dhs.seg$age.adj))) - 1)
  } else {
  ncoefs <- length(svy.glm$coefficients)
  }
  
  if(ncoefs ==2){
    labs = measure
  } else {
    labs = svy.glm$xlevels[[1]][-1]
  }
  coefs <- cbind.data.frame(labels = labs, coefs = as.vector(svy.glm$coefficients[2:ncoefs]), 
                            se = as.vector(coef(summary(svy.glm))[2:ncoefs,2]),
                            sig = as.vector(summary$sig[2:ncoefs]))
  coefs$sig.bin <- ifelse(coefs$sig %in% c("xxx", "xx", "x"), "yes", "no")
  
  plot.coefs <- ggplot(coefs, aes(x=labels, y=coefs)) + 
    geom_pointrange(aes(ymin=coefs - 1.96*se, ymax=coefs + 1.96*se, color = sig.bin)) + 
    scale_color_manual(name = "significance",
                       breaks = c("no", "yes"),
                       values = c("Black", "Red")) +
    coord_flip()+
    labs(x=measure, y="Adjusted coefficients and CI", title=paste0("Association of ", measure, " and ", outcome)) + 
    theme(legend.position = "none") 
  
  # Plot predicted probabilities
  pred.data<-data.frame(unique(na.omit(dhs.seg %>% pull(eval(parse(text=measure))))))
  names(pred.data) <- substitute(measure)
  pred.data$age.adj <- "30-34"
  
  probabilities <- predict(svy.glm, pred.data, type = "response", se.fit=T)
  
  prob1<-as.data.frame(probabilities)
  prob1$lower<- prob1$response - (1.96*prob1$SE)
  prob1$upper<- prob1$response + (1.96*prob1$SE)
  prob1$levels<- pred.data[,1]
  
  significance <- data.frame("levels" = coefs$labels, "sig.bin" = coefs$sig.bin)
  prob1 <- merge(prob1, significance, by="levels", all.x = T)
  
  plot.prob <- ggplot(prob1, aes(x=levels, y=response)) + 
    geom_point(aes(colour = sig.bin), alpha=0.9, size=2, show.legend = FALSE) +
    geom_errorbar(aes(ymin=lower, ymax=upper, colour = sig.bin), linewidth=0.5, width=.4, show.legend = FALSE) +
    geom_text(aes(y=response, x=levels, label=round(response,2)), vjust=-1.2, hjust=.5) +
    scale_color_manual(name = "significance",
                       breaks = c("no", "yes"),
                       values = c("Black", "Red")) +
    coord_flip()+
    xlab("") +
    ylab(paste0("Probability of ",outcome)) +
    theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 12), axis.title=element_text(size=12))
  
  var = substitute(measure)
  
  out <- list(stats, summary, LRT.pval, plot.prob, plot.coefs, var)
  return(out) 
}


# Multiplot function for combining plots
multiplot <- function(..., plotlist=NULL, file, cols=2, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#### Explore associations ####

##' NOTES: for a new variable, copy and paste the code under an existing variable. Replace the variable name, and then
##' find and replace the prefix (e.g. "ed.") with a new prefix for the variable of interest.
##' Among the results, I suggest focusing on the plots of the predicted probabilities (plots.prob) first. These are grouped
##' by mom-focused and baby-focused outcomes. The color coding is: grey - reference level, red = p<0.05, black = p>=0.05.
##' Whether or not any levels were significanly different from the null for each outcome will be added to a data frame that 
##' can be exported.
## AG: for list of variables used, see exploratory_analysis_external.xls

  ## If running for the first time, uncomment and run the following
#  assoc.summary <- as.data.frame(matrix(nrow = length(names(vulnerability)), ncol = length(outcomes.list)))
# colnames(assoc.summary) <- outcomes.list
# assoc.summary$factor <- names(vulnerability)
# assoc.summary <- assoc.summary %>% select(factor, everything())
# assoc.summary <- assoc.summary[8:length(names(vulnerability)),]
# saveRDS(assoc.summary, file = "Data/Ethiopia_constructed_data/5_assoc.summary.rds")
 

  # Otherwise, if adding to an existing and previously saved "assoc.summary" object, load it!
  assoc.summary <- readRDS("Data/Ethiopia_constructed_data/5_assoc.summary.rds")


  ## Define a vector for whether outcomes are poisson
  poisson.yn <- outcomes.list == "anc.num"
  
##' ==> Run the below code for each new variable. 
##' ==> Replace the variable name in quotes in the call to f_pattern_explore and, if a household variable, set age.adjust == FALSE
##' ==> if the factor is numeric/integer, try forcing it to factor (replace "var" with the variable name)

    stats <- list()
    summary <- list()
    LRT.vals <- list()
    any.sig.levels <- as.data.frame(matrix(nrow = 1, ncol = length(outcomes.list)))
    colnames(any.sig.levels) <- outcomes.list
    plots.prob <- list()
    plots.coefs <- list()
      #numeric/int measures are: 
    ## If the measure is numeric/integer, try forcing it to factor to see the pattern
      # variable can't factor without removing two cases of only 1 observation each, so changing here to get a sense of patterns:
      dhs.seg$highestyearsedinHH.yrs <- ifelse(dhs.seg$highestyearsedinHH.yrs<20, dhs.seg$highestyearsedinHH.yrs, NA)

    dhs.seg$early.sex.15<- as.factor(dhs.seg$early.sex.15) #changed age.yrs to early.sex.15
    dhs.seg.des$variables$early.sex.15 <- as.factor(dhs.seg.des$variables$early.sex.15)
    
    for(i in 1:length(outcomes.list)){
      out <- f_pattern_explore("early.sex.15", outcomes.list[i], poisson = poisson.yn[i], age.adjust = FALSE)
      stats[[i]] <- out[[1]]
      summary[[i]] <- out[[2]]
      LRT.vals[[i]] <- out[[3]]
      any.sig.levels[[i]] <- ifelse(out[[3]][1] < 0.05, 1, 0)
      plots.prob[[i]] <- out[[4]]
      plots.coefs[[i]] <- out[[5]]
      var <- out[[6]]
    }
    names(stats) <- outcomes.list
    names(summary) <- outcomes.list
    names(LRT.vals) <- outcomes.list
    names(plots.prob) <- outcomes.list
    names(plots.coefs) <- outcomes.list
    
    # Look at results (I find looking at the plots much more informative; this info is harder to digest but here if needed)
    stats
    summary
    LRT.vals
    
    # Mom-focused outcomes
    multiplot(plotlist = plots.prob[1:8])
    #multiplot(plotlist = plots.coefs[1:8])
    
  # Baby-focused outcomes
  multiplot(plotlist = plots.prob[c(9:12, 20:24)])
  #multiplot(plotlist = plots.coefs[c(9:12, 20:24)])
  
  multiplot(plotlist = plots.prob[13:19])
  #multiplot(plotlist = plots.coefs[13:19])
  

  # Merge any.sig.levels with the assoc.summary object
  any.sig.levels$factor <- var
  assoc.summary[assoc.summary$factor == any.sig.levels$factor, 2:ncol(assoc.summary)] <- any.sig.levels[-ncol(any.sig.levels)]
  #assoc.summary <- merge(assoc.summary, any.sig.levels, by = "factor", all.x = TRUE)
  
  # save assoc.summary
  saveRDS(assoc.summary, file = "Data/Ethiopia_constructed_data/5_assoc.summary.rds")
  
  #look at output (AG added 2/7/23)
  assoc.summary <- readRDS("Data/Ethiopia_constructed_data/5_assoc.summary.rds")

 