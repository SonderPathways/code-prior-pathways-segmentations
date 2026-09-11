###################################################################
######## Vulnerability measures exploratory analysis ##############
########              Ethiopia DHS 2016              ##############
###################################################################


#### Load packages ####
rm(list=ls())
pacman::p_load("tidyverse", "knitr", "data.table", "lmtest", "logbin", "survey")

## Plot settings
plot_background <- theme(panel.background = element_rect(fill="white", colour = "black")) + theme(panel.grid.major = element_line(colour = "grey90"))
plot_titles <- theme(plot.title = element_text(hjust = 0.5, size=14, colour = "black", face = "bold")) + 
  theme(legend.title = element_text(colour = "black", size = 13), legend.text = element_text(colour = "black", size = 13)) +
  theme(axis.title = element_text(colour = "black", size = 13), axis.text = element_text(colour = "black", size = 12))

## Import the vulnerability measures and merge with outcomes

setwd("C:\\Users\\elisabethro\\OneDrive - Bill & Melinda Gates Foundation\\Pathways - Gender Equality Division")

vulnerability <- readRDS("Modeling/Data/Ethiopia DHS/ET_2016/seg_dat.rds")
load("Modeling/Data/Ethiopia DHS/ET_2016/Ethiopia_DHS_outcomes.RData")

dhs.seg <- merge(vulnerability, outcomes, by="caseid", all.x=T)

#### Make binary outcomes ####
# Underweight
dhs.seg$underweight <- ifelse(dhs.seg$bmi.cat2 == "Underweight", 1, 
                              ifelse(!is.na(dhs.seg$bmi.cat2), 0, NA))

# Zero dose
dhs.seg$zerodose.yn <- ifelse(dhs.seg$zerodosecnt %in% c(1:3), 1, 
                              ifelse(dhs.seg$zerodosecnt == 0, 0, NA))

dhs.seg$dpt1.yn <- ifelse(dhs.seg$dpt1cnt %in% c(1:3), 1, 
                          ifelse(dhs.seg$dpt1cnt == 0, 0, NA))

# Child morality
dhs.seg$u1mort.yn<- ifelse(dhs.seg$u1mortcnt > 0, 1,
                   ifelse(dhs.seg$u1mortcnt ==0,0,NA))
dhs.seg$u5mort.yn<- ifelse(dhs.seg$u5mortcnt > 0, 1,
                   ifelse(dhs.seg$u5mortcnt ==0,0,NA))

# Set survey design matrix
dhs.seg.des <-svydesign(id=~v021, strata=~v023, weights=~wt, data=dhs.seg)


f_test <- function(measure){
  nm <- substitute(measure)
  df <- data.frame(c(0,1,3))
  names(df) <- substitute(measure)
  return(df)
}

#### Define functions ####

# Define function to explore 
f_pattern_explore <- function(measure, outcome){
  
  # Look at summary stats
  if(is.integer(dhs.seg %>% pull(eval(parse(text=measure)))) | is.numeric(dhs.seg %>% pull(eval(parse(text=measure))))) {
    stats <-svyby(~eval(parse(text=outcome)), ~eval(parse(text=measure)), dhs.seg.des, svymean,ci=TRUE,vartype="ci", na.rm=T)
  } else {
    stats <- prop.table(svytable(~eval(parse(text=outcome)) + eval(parse(text=measure)), dhs.seg.des), margin = 2)
  }

  # Test association
  svy.glm<-svyglm(as.formula(paste0(outcome, "~", measure)),family=quasibinomial,design=dhs.seg.des, data=dhs.seg)
  
  # Modelsummary 
  summary <- as.data.frame(summary(svy.glm)$coefficients)
  summary$sig <-  ifelse(summary$`Pr(>|t|)`<0.001, "xxx", ifelse(summary$`Pr(>|t|)`<0.01, "xx", ifelse(summary$`Pr(>|t|)`<0.05, "x", ifelse(is.na(summary$`Pr(>|t|)`), "", ""))))
  summary$sig[1] <- ""
  
  # Plot predicted probabilities
  if(is.integer(dhs.seg %>% pull(eval(parse(text=measure)))) | is.numeric(dhs.seg %>% pull(eval(parse(text=measure))))) {
    pred.data<-data.frame(unique(dhs.seg %>% pull(eval(parse(text=measure)))))
  } else {
    pred.data<-data.frame(levels(as.factor(dhs.seg %>% pull(eval(parse(text=measure))))))
  }
  names(pred.data) <- substitute(measure)

  probabilities <- predict(svy.glm, pred.data, type = "response", se.fit=T)
  
  prob1<-as.data.frame(probabilities)
  prob1$lower<- prob1$response - (1.96*prob1$SE)
  prob1$upper<- prob1$response + (1.96*prob1$SE)
  prob1$levels<- pred.data[,1]

  plot.prob <- ggplot(prob1, aes(x=levels, y=response)) + 
    geom_point(alpha=0.9, size=2, show.legend = FALSE) +
    geom_errorbar(aes(ymin=lower, ymax=upper), size=0.5, width=.4, show.legend = FALSE) +
    geom_text(aes(y=response, x=levels, label=round(response,2)), vjust=-1.2, hjust=.5) +
    coord_flip()+
    xlab("") +
    ylab(paste0("Probability of ",outcome)) +
    theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 12), axis.title=element_text(size=12))
  
  # Plot coefficients
  ncoefs <- length(svy.glm$coefficients)
  coefs <- cbind.data.frame(labels = svy.glm$xlevels[[1]][-1], coefs = as.vector(svy.glm$coefficients[2:ncoefs]), 
                            se = as.vector(coef(summary(svy.glm))[2:ncoefs,2]),
                            sig = as.vector(summary$sig[-1]))
  coefs$sig.bin <- ifelse(coefs$sig %in% c("xxx", "xx", "x"), "yes", "no")
  plot.coefs <- ggplot(coefs, aes(x=labels, y=coefs)) + 
    geom_pointrange(aes(ymin=coefs - 1.96*se, ymax=coefs + 1.96*se, color = sig.bin)) + 
    scale_color_manual(name = "significance",
                       breaks = c("no", "yes"),
                       values = c("Black", "Red")) +
    labs(x=measure, y="Adjusted coefficients and CI", title=paste0("Association of ", measure, " and ", outcome)) + 
    theme(legend.position = "none")
  out <- list(stats, summary, plot.prob, plot.coefs)
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

# Outcomes to focus on - no breastfeeding, continuous bmi, haz, waz, etc.
# program another regression as continuous

# Binary - underweight vs. not, "hb.1", "fp.mod.ever", "anc.4plus","anyanc","anc.1sttri", "stunt.cat2.last","waste.cat2.last", 
# binary of zero dose and dpt1, u1mortcnt, u5mortcnt, "stl.cnt" 
#lower priority - "undwgt.last","ovrwgt.last", "breastfed","breastfed2","breastfed2.excl"
# continuous - "anc.num", "anc.1stvisit", "haz.last", "waz.last", "whz.last"
v.outcomes <- c("baby.hlthck","wom.hlthck", "hb.1","fp.mod.ever","anc.4plus","anyanc","anc.1sttri",
                "stunt.cat2.last","waste.cat2.last","undwgt.last","ovrwgt.last")

# "underweight","zerodose.yn","dpt.yn", "u1mort.yn","u5mort.yn","stl.cnt"

## Generic - just change variable
stats <- list()
summary <- list()
plots.prob <- list()
plots.coefs <- list()

for(i in 1:length(v.outcomes)){
  out <- f_pattern_explore("dv.out", v.outcomes[i])
  stats[[i]] <- out[[1]]
  summary[[i]] <- out[[2]]
  plots.prob[[i]] <- out[[3]]
  plots.coefs[[i]] <- out[[4]]
  
}
names(stats) <- v.outcomes
names(summary) <- v.outcomes
names(plots.prob) <- v.outcomes
names(plots.coefs) <- v.outcomes

stats
summary
multiplot(plotlist = plots.prob[1:4])
multiplot(plotlist = plots.prob[5:9])
multiplot(plotlist = plots.prob[10:15])
