##########################  KENYA_DHS22_age explore  ###########################

# Purpose: Identify vulnerabilities that have strong age patterns. 
#          This script creates the pdf age explore_vulnerability (?)
# Authors: Anu Mishra (Ethiopia resource code), Helena Manguerra (UW START, Kenya)
#
# Input files: "dhs.seg"
#              "dhs.seg.des"
#              "[vulnerability folder]/5_assoc.summary.rds"
#              
# Last edited: 
#
# Output files: *_age_unadj.pdf" 
#
# Ancestor scripts: 5_Kenya_DHS_exploratory_13Dec2023.R
#
# Descendant script: 7_Kenya_DHS_construct_mapping.R
# 
# Note: Need to set directory for each user
# Note: This script requires changing variables by hand and rerunning code. Ideally this could be put in a loop at some point
# Notes AG: Reran from beginning each time or plot would not refresh
#_______________________________________________________________________________ 

# Clearing the workspace -------------------------------------------------------

rm(list=ls())

# Load libraries ---------------------------------------------------------------

pacman::p_load(ggplot2, tidyverse, survey, data.table, openxlsx)

# Set working directory --------------------------------------------------------

user = "Helena"

if (tolower(user) == "helena") {
  rootdir <- "C:/Users/hmang/UW START Dropbox/Helena Manguerra/"
} else if (tolower(user) == "mac") {
  rootdir <- "~/UW START Dropbox/Helena Manguerra/"
} else if (tolower(user) == "amanda") {
  rootdir <- "C:/Users/manda/UW START Dropbox/Amanda Brumwell/"
} else if (tolower(user) == "andrea") {
  rootdir <- "C:/Users/Andrea Rivas/UW START Dropbox/Andrea Rivas/"
} else if (tolower(user) == "bih") {
  rootdir <- "C:/Users/bihmo/UW START Dropbox/Bih Epse Cuba Moki Suh/"
} else if (tolower(user) == "sarah") {
  rootdir <- "C:/Users/sshaw/UW START Dropbox/Sarah Shaw/"
} else {
  print("No filepath")
}

setwd(file.path(rootdir, "START_229_Gender Equity/Working Files/"))


# Load data --------------------------------------------------------------------

vulnerability <- readRDS("Data/KE_2022_constructed_data/4_Kenya_DHS_vulnerability.rds")
outcomes <- readRDS("Data/KE_2022_constructed_data/1_Kenya_DHS_outcomes.rds")

dhs.seg <- merge(vulnerability, outcomes, by="caseid", all.x=T)

# subset to long
dhs.seg <- dhs.seg %>% filter(sshort %like% "long")

p_list <- list()  #empty plotting list that needs to be initialized

vulnerability_map <- data.table(read.xlsx("Data/KE_2022_constructed_data/0_Kenya_DHS_Data_Dictionary.xlsx", sheet = "Vulnerabilities_full_list_SS"))

names(vulnerability_map) <- as.character(vulnerability_map[5, ])
vulnerability_map <- vulnerability_map[!(is.na(`variable name`))]
vulnerability_map <- vulnerability_map[`variable name` != "variable name"]

# Get list of vars where useful to subset out irrelevant categories
vulns.subset_map <- vulnerability_map[!is.na(exploratory_subset), .(vulnerability = `variable name`, exploratory_subset)]


dhs.seg$age.adj <- cut(dhs.seg$age.yrs,
                       breaks = c(14, 19, 24, 29, 34, 39, 44, 49),
                       labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))



# Functions --------------------------------------------------------------------

f_ageExplore <- function(measure,var.class,data,subset){
  
  print(subset)
  print(nrow(data))
  
  if(subset == "currently_partnered"){
    data <- data %>% filter(marr.cohab == 1)
  } else if(subset == "ever_partnered"){
    data <- data %>% filter(ever.partnered.yn == 1)
  } else if(subset == "ever_givenbirth"){
    data <- data %>% filter(bio.children.yn == 1)
  } else if(subset == "ever_partnered_givenbirth"){
    data <- data %>% filter(ever.partnered.yn == 1 & bio.children.yn == 1)
  } else if(subset == "urban"){
    data <- data %>% filter(hh.urban == 1)
  } else if(subset == "full_sample"){
    data <- copy(data)
  } else{
    message("No subset identified (if no subset intended, set to 'full_sample')")
    break
  }
  
  survey.des <-svydesign(id=~v021, strata=~v023, weights=~wt, data=data) # Reset the survey design object with these new vars defined
  
  print(nrow(data))
  
  #out <- tryCatch({
    if(is.null(var.class)){
      print('Variable type/class not provided so using default class coding, check if this is appropriate!')
      v.class <- class(survey.des$variables[,measure])
      
      #recoding integers that are not 0/1 to numeric
      v.class <- ifelse(v.class!='numeric' & v.class!='factor' & !all(na.omit(dhs.seg %>% pull(eval(parse(text=measure)))) %in% 0:1),'numeric',v.class)
    } else{v.class <- var.class}
    
    
  #* 1: Descriptive tables: Contigency table for categorical/binary data, means and CIs for continuous by age group ----
  
    if(v.class %in% c('binary','categorical','factor','character')){
      des.res.count <- round(svytable(as.formula(paste0("~age.adj +",measure)), survey.des),digits=2)
      ## proportion of education level within each age group (same as plot below)
      des.res.prop <- round(prop.table(svytable(as.formula(paste0("~age.adj +",measure)), survey.des),margin=1)*100,2)  
      
      des.res <- list('tab.count'=des.res.count,'tab.prop'=des.res.prop)
    } else if(v.class %in% c('numeric')){
      des.res <- svyby(as.formula(paste0("~",measure)), ~age.adj, survey.des, svymean,ci=TRUE,vartype="ci", na.rm=T)
    }
    
    
  #* 2: Descriptive charts of distribution of vulnerability by age group -------
  
    if(v.class %in% c('binary','categorical','factor','character')){
      pdat <- data.frame(round(prop.table(svytable(as.formula(paste0("~age.adj +",measure)), survey.des),margin=1)*100,2))
      names(pdat)[names(pdat)==measure] <- 'measure'
      p <- ggplot(data=pdat,aes(fill=measure, y=Freq, x=age.adj)) + geom_bar(position="stack", stat="identity") +
        xlab('Age group') + ylab(measure) + theme(legend.title = element_blank()) + ggtitle('Proportion of vulnerability by age group')
    } else if(v.class %in% c('numeric')){
      pdat <- svyby(as.formula(paste0("~",measure)), ~age.adj, survey.des, svyquantile,       
                    quantiles = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE, ci=TRUE, 
                    keep.var = TRUE, 
                    vartype = "ci")
      boxstats <- pdat[,1:6]
      names(boxstats) <- c('age.adj','q0','q25','q50','q75','q100')
      p <- ggplot(boxstats,
                  aes(x=age.adj, ymin=q0, lower=q25,middle=q50,
                      upper=q75,ymax=q100)) + geom_boxplot(stat='identity') + 
        coord_cartesian(ylim= c(min(boxstats[,2:6]),1.05*max(boxstats$q75))) + ylab(measure)
      
    }
    
  #* 3: Tests of association between age and vulnerability ---------------------
  
    if(v.class %in% c('binary','categorical','factor','character')){
      svy.chi <- svychisq(as.formula(paste0('~',measure, "+ age.adj")), survey.des, statistic="Chisq")
      mod.res <- svy.chi
    } else if(v.class %in% c('numeric')){
      svy.glm <- svyglm(as.formula(paste0(measure, "~ age.adj")),family=gaussian(link ="identity"),design=survey.des, data=dhs.seg)
      LRT.pval.measure <- regTermTest(svy.glm, 'age.adj', method = "LRT")$p
      mod.res <- list('model'=summary(svy.glm),'global.p'=LRT.pval.measure)
    }
    
    all_res <- list('stats'=des.res, 'plot'=p, 'model'=mod.res)
    return(all_res)  
  # },
  # error = function(cond){
  #   message(cond)
  #   return("TEST")
  # },
  # finally = function(cond){
  #   message("Completed one plot")
  # })
  
  #return(out)
  
}


# Explore age associations -----------------------------------------------------


##' NOTES: for each age exploration variable, 
##' Starting from line (104 - may change) below, rerun for vulnerability that is potentially a proxy for by assinging 
##' measure and class (if necessary) values to the function. The inputs are
##' measure = text string for vulnerability you want to explore
##' class = whether a vulnerability is numeric or factor/binary/categorical. If not supplied the function will use
##'         the current class assignment of that variable, but try to account for binary dummy variables 
##'         for any categorical variable valid inputs are: 'binary','categorical','factor','character'
##'         for continuous variable valid inputs are: numeric
##'         
##' The function gives the following output
##' stats =  for categorical variables: a contingency table (counts and proportions) of the age categories and the vulnerability  
##'          for continuous variable: the mean and CI of the vulernability within each age group
##'          
##' plot = for categorical variables: a stacked bar chart of % of vulnerability category within each age group 
##'        for continuous variables: a box plot of the distribution of the vulnerability by age group 
##'        plotted for up to the largest 75% to avoid extereme y-axis bounds due to outliers
##'        
##' model = for cateogircal variables: results of a chi-sq test for association between age cateogries and vulnerability
##'       = for continuous variables: results of a linear model fit with age predicting the vulnerability 
##'         the model summary as well as a global p-value testing for any association are returned        
##'        
##' I was thinking that if we see a strong trend in this graph where as age increases the distribution of 
##' vulnerability changes, we should flag this variable as something we might need to re categorize, 
##' age-standardize, or drop entirely 
##' The third/final chunk of code performs a chi-sq test for categorical/binary vulnerabilities and a linear regression
##' for continuous vulnerabilities to assess if age is associated with the vulnerability.
##' If age is not associated with the vulnerability, I think we can move it to the next step (PCA)
##' All analyses account for survey design


vulnerabilities <- list.dirs("Data/KE_2022_constructed_data/5_assoc_summaries", recursive = F, full.names = F) 

#for(vul.measure in vulnerabilities[1:length(vulnerabilities)]){
 
for(vul.measure in c("alwaysres.yn", "age.diff.bi", "highestyearsedinHH.ur.under10", "highestyearsedinHH.ru.under9", "ed.lev3")){
  #vul.measure <- 'alwaysres.yn'#
  
  message(vul.measure)
  
  if(file.exists(paste0("Data/KE_2022_constructed_data/5_assoc_summaries/", vul.measure, "/5_assoc.summary.rds"))){
  
    # if(file.exists(paste0("Data/KE_2022_constructed_data/5_assoc_summaries/", vul.measure, "/age_explore_", vul.measure, ".rds"))){
    #   next
    # }
    
    tryCatchResults <- tryCatch({
    if(class(dhs.seg[[vul.measure]]) %like% "integer|numeric" & length(unique(na.omit(dhs.seg[[vul.measure]]))) != 2){
      out <- f_ageExplore(measure = vul.measure,
                          var.class = 'numeric',
                          data = dhs.seg,
                          subset=unique(vulns.subset_map[vulnerability == vul.measure, exploratory_subset]))
      
    } else{
      out <- f_ageExplore(measure = vul.measure,
                          var.class = 'character',
                          data = dhs.seg,
                          subset = unique(vulns.subset_map[vulnerability == vul.measure, exploratory_subset]))
      
    }
    
    
    pdf(file = paste0("Data/KE_2022_constructed_data/5_assoc_summaries/", vul.measure, "/age_explore_",vul.measure,'.pdf'),height=8,width=12)
    plot(out$plot)
    
    dev.off()
    
    },
    error=function(cond) {
      message(paste("ERROR:"))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NULL)
    },
    # warning=function(cond) {
    #   message(paste("WARNING"))
    #   message("Here's the original warning message:")
    #   message(cond)
    #   # Choose a return value in case of warning
    #   return(NULL)
    # },
    finally={

    })
      
  }
  
  
  
}
