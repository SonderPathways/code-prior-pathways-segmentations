#####################  KENYA_DHS22_exploratory_analysis  #######################

# Purpose:Test relationship between vulnerabilities and outcomes
# Authors: Darcy, Helena, Sarah, Amanda, Andrea
# Input files: "1_Kenya_DHS_outcomes_2Nov2023.rds"
#              "4_Kenya_DHS_vulnerability_2Feb2024.rds"
#              
# Last edited: Oct 22, 2023: SS merged scripts from response and variable reviews
#              Nov 27, 2023: HM added in indices #update when in github
#              Jan 27, 2024: AB recoded selected variables for LCA based on Darcy input 
#
# Output files: "" 
# Ancestor scripts: 1_Kenya_DHS_Outcomes_2Nov2023.R
#                   4_Kenya_DHS2022_vulnerability_measures_27Jan2024-wnewvars.R
#
# Descendant script: ""
# 
# Note: Need to set directory for each user
# Note: Need to set directory for each user
# Note: This script requires changing variables by hand and rerunning code. Variables selected are those in the exploratory_analysis_external.xls
# Note: There is some code that needs to be done the first round through, see comments
#_______________________________________________________________________________ 


# Clearing the workspace -------------------------------------------------------

rm(list=ls())

# Install and load libraries ---------------------------------------------------

pacman::p_load("tidyverse", "knitr", "data.table", "lmtest", "logbin", "survey", "grid", "openxlsx", "colorspace", "viridis")

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

poisson.vars = "anc.num"

# Import the vulnerability measures and merge with outcomes --------------------

vulnerability <- readRDS("Data/KE_2022_constructed_data/4_Kenya_DHS_vulnerability.rds")
vulnerability_map <- data.table(read.xlsx("Data/KE_2022_constructed_data/0_Kenya_DHS_Data_Dictionary.xlsx", sheet = "Vulnerabilities_full_list_SS"))

names(vulnerability_map) <- as.character(vulnerability_map[5, ])
vulnerability_map <- vulnerability_map[!(is.na(`variable name`))]
vulnerability_map <- vulnerability_map[`variable name` != "variable name"]

# Get list of vars for modeling interaction
vulns.int_marrcohab <- vulnerability_map[model.int_marrcohab == TRUE, `variable name`]

# Get list of vars where useful to subset out irrelevant categories
vulns.subset_map <- vulnerability_map[!is.na(exploratory_subset), .(vulnerability = `variable name`, exploratory_subset)]

vulnerability_list <- vulnerability_map[, `variable name`]

outcomes_list <- data.table(read.xlsx("Data/KE_2022_constructed_data/0_Kenya_DHS_Data_Dictionary.xlsx", sheet = "Outcomes (updated)"))
outcomes <- readRDS("Data/KE_2022_constructed_data/1_Kenya_DHS_outcomes.rds")

dhs.seg.all <- merge(vulnerability, outcomes, by="caseid", all.x=T)
dhs.seg.long <- dhs.seg.all %>% filter(sshort %like% "long")



# Define and group outcomes ----------------------------------------------------

  #* Group outcomes ------------------------------------------------------------
outcomes_list <- outcomes_list[keep == "Keep"]
outcomes.list <- outcomes_list[, variable.name]
outcomes_list[, index := .I]


# Define functions -------------------------------------------------------------

  #* Define function to explore ------------------------------------------------
  

f_pattern_explore <- function(measure, 
                              outcome, 
                              poisson, 
                              int_marrcohab,
                              subset){
  
  tryCatchResults <- tryCatch({
    
    if(subset == "currently_partnered"){
      dhs.seg <- dhs.seg.long %>% filter(marr.cohab == 1)
    } else if(subset == "ever_partnered"){
      dhs.seg <- dhs.seg.long %>% filter(ever.partnered.yn == 1)
    } else if(subset == "ever_givenbirth"){
      dhs.seg <- dhs.seg.long %>% filter(bio.children.yn == 1)
    } else if(subset == "ever_partnered_givenbirth"){
      dhs.seg <- dhs.seg.long %>% filter(ever.partnered.yn == 1 & bio.children.yn == 1)
    } else if(subset == "urban"){
      dhs.seg <- dhs.seg.long %>% filter(hh.urban == 1)
    } else if(subset == "full_sample"){
      dhs.seg <- copy(dhs.seg.long)
    } else{
      message("No subset identified (if no subset intended, set to 'full_sample')")
      break
    }
    
    # Convert all to factor
    message("Converting ", measure, " to factor, currently these levels: ", paste0(unique(dhs.seg[,measure]),collapse = ","))
    dhs.seg[, measure]<- factor(dhs.seg[,measure], levels = sort(unique(dhs.seg[,measure])), ordered = F)
    
    # Create new variables in the DF for measure and outcome 
    dhs.seg$var <- dhs.seg %>% pull(eval(parse(text=measure)))
    dhs.seg$outcome <- dhs.seg %>% pull(eval(parse(text=outcome)))
    df.design <-svydesign(id=~v021, strata=~v023, weights=~wt, data=dhs.seg) # Reset the survey design object with these new vars defined
    df.design$variables[, measure] <- as.factor(df.design$variables[, measure])
    
    # make indicators for subset of observations to include for interaction models (those categories which have data for both levels of interaction)
    dhs.seg$urban.int.include <- 1
    dhs.seg$pship.int.include <- 1
    dhs.seg$both.int.include <- 1
    if(is.factor(dhs.seg$var) | is.character(dhs.seg$var)){
      # If outcome is not binary, don't restrict on outcome
      ## urban.int.include -- Restrict dataset to only include levels of var with observations for both urban and rural
      if(length(unique(dhs.seg$outcome[!is.na(dhs.seg$outcome)]))>2){
        freqXurban <- as.data.frame(svytable(~var + hh.urban, df.design)) %>% filter(Freq>0) %>%
          group_by(var) %>% count() %>% ungroup() %>% mutate(maxfreq = max(n)) %>% filter(n == maxfreq) %>% 
          mutate(var = factor(var)) %>% select(var) 
        levels.urban <- levels(freqXurban$var)
        dhs.seg$urban.int.include <- ifelse(dhs.seg$var %in% levels.urban, 1, 0)
        
        ## pship.int.include -- Restrict dataset to only include levels of var with observations for both partnered and unpartnered
        freqXpship <- as.data.frame(svytable(~var + marr.cohab, df.design)) %>% filter(Freq>0) %>%
          group_by(var) %>% count() %>% ungroup() %>% mutate(maxfreq = max(n)) %>% filter(n == maxfreq) %>% 
          mutate(var = factor(var)) %>% select(var) 
        levels.pship <- levels(freqXpship$var)
        dhs.seg$pship.int.include <- ifelse(dhs.seg$var %in% levels.pship, 1, 0)
        
        ## both.int.include -- Restrict dataset to only include levels of var with observations for urban, rural, partnered, and unpartnered
        freqXboth <- as.data.frame(svytable(~var + hh.urban + marr.cohab, df.design)) %>% filter(Freq>0) %>%
          group_by(var) %>% count() %>% ungroup() %>% mutate(maxfreq = max(n)) %>% filter(n == maxfreq) %>% 
          mutate(var = factor(var)) %>% select(var) 
        levels.both <- levels(freqXboth$var)
        dhs.seg$both.int.include <- ifelse(dhs.seg$var %in% levels.both, 1, 0)
      }
      
      # If outcome is binary, restrict based on outcome as well
      ## urban.int.include -- Restrict dataset to only include levels of var with observations for both urban and rural and outcome
      if(length(unique(dhs.seg$outcome[!is.na(dhs.seg$outcome)]))==2){
        freqXurban <- as.data.frame(svytable(~var + hh.urban + outcome, df.design)) %>% filter(Freq>0) %>%
          group_by(var) %>% count() %>% ungroup() %>% mutate(maxfreq = max(n)) %>% filter(n == maxfreq) %>% 
          mutate(var = factor(var)) %>% select(var) 
        levels.urban <- levels(freqXurban$var)
        dhs.seg$urban.int.include <- ifelse(dhs.seg$var %in% levels.urban, 1, 0)
        
        ## pship.int.include -- Restrict dataset to only include levels of var with observations for both partnered and unpartnered
        freqXpship <- as.data.frame(svytable(~var + marr.cohab + outcome, df.design)) %>% filter(Freq>0) %>%
          group_by(var) %>% count() %>% ungroup() %>% mutate(maxfreq = max(n)) %>% filter(n == maxfreq) %>% 
          mutate(var = factor(var)) %>% select(var) 
        levels.pship <- levels(freqXpship$var)
        dhs.seg$pship.int.include <- ifelse(dhs.seg$var %in% levels.pship, 1, 0)
        
        ## both.int.include -- Restrict dataset to only include levels of var with observations for urban, rural, partnered, and unpartnered
        freqXboth <- as.data.frame(svytable(~var + hh.urban + marr.cohab + outcome, df.design)) %>% filter(Freq>0) %>%
          group_by(var) %>% count() %>% ungroup() %>% mutate(maxfreq = max(n)) %>% filter(n == maxfreq) %>% 
          mutate(var = factor(var)) %>% select(var) 
        levels.both <- levels(freqXboth$var)
        dhs.seg$both.int.include <- ifelse(dhs.seg$var %in% levels.both, 1, 0)
      }
    }
    
    df.design <-svydesign(id=~v021, strata=~v023, weights=~wt, data=dhs.seg) # Reset the survey design object with the new indicators
    
    # Create an indicator to note if exposure and/or outcome are binary (and coded as 0 vs 1)
    binary.exp <- all(na.omit(dhs.seg$var) %in% 0:1)
    binary.out <- all(na.omit(dhs.seg$outcome) %in% 0:1)
    
    # Look at summary stats
    if(binary.exp == FALSE & (is.integer(dhs.seg$var) | is.numeric(dhs.seg$outcome))) {
      stats <- svyby(~var, ~outcome, df.design, svymean,ci=TRUE,vartype="ci", na.rm=T)
    } else if (binary.out == FALSE & (is.integer(dhs.seg$outcome)) | is.numeric(dhs.seg$outcome)) {
      stats <-svyby(~outcome, ~var, df.design, svymean,ci=TRUE,vartype="ci", na.rm=T)
    } else {
      stats <- prop.table(svytable(~ outcome + var, df.design), margin = 2)
    }
    
    # Set the reference level to be the level with the largest number, for factor/character vars with more than 2 levels
    
    if((is.character(dhs.seg$var) | is.factor(dhs.seg$var)) & 
       length(unique(dhs.seg$var)) >2) {
      # Find the biggest category
      biggest_group <- dhs.seg %>% filter(!is.na(var)) %>% group_by(var) %>% count(sort = T) %>% ungroup() %>% filter(row_number()==1)
      biggest_group <- as.character(biggest_group[[1,1]])
      
      # Set as the reference group
      dhs.seg <- within(dhs.seg, var <- relevel(var, ref = biggest_group[1]))
      df.design$variables$var <- relevel(df.design$variables$var, ref = biggest_group[1])
      reordered <- TRUE
      
    } else{
      reordered <- FALSE
    }
    
    
    # GLM for main effect and urban/rural and partnership status interaction terms ------------------------
    if(binary.out == FALSE){
      if(poisson == TRUE) {
        svy.glm.main <- svyglm(outcome ~ var,family=quasipoisson,design=df.design, data=dhs.seg)
        svy.glm.int_urban <- svyglm(outcome ~ var*hh.urban,family=quasipoisson,design=subset(df.design, urban.int.include == 1))
        if(int_marrcohab == TRUE){
          svy.glm.int_pship <- svyglm(outcome ~ var*marr.cohab,family=quasipoisson,design=subset(df.design, pship.int.include ==1))
        }
      }else{
        svy.glm.main <- svyglm(outcome ~ var,family=gaussian(link ="identity"),design=df.design, data=dhs.seg)
        svy.glm.int_urban <- svyglm(outcome ~ var*hh.urban,family=gaussian(link ="identity"),design=subset(df.design, urban.int.include == 1))
        if(int_marrcohab == TRUE){
          svy.glm.int_pship <- svyglm(outcome ~ var*marr.cohab,family=gaussian(link ="identity"),design=subset(df.design, pship.int.include ==1))
        }
      }
    }else{
      svy.glm.main <-svyglm(outcome ~ var,family=quasibinomial,design=df.design, data=dhs.seg)
      svy.glm.int_urban<-svyglm(outcome ~ var*hh.urban,family=quasibinomial,design=subset(df.design, urban.int.include == 1))
      if(int_marrcohab == TRUE){
        svy.glm.int_pship<-svyglm(outcome ~ var*marr.cohab, family=quasibinomial,design=subset(df.design, pship.int.include ==1))
      }
    }
    
    # Model summaries ----------------------------
    summary.main <- as.data.frame(summary(svy.glm.main)$coefficients)
    summary.main$sig <-  ifelse(summary.main$`Pr(>|t|)`<0.001, "xxx", ifelse(summary.main$`Pr(>|t|)`<0.01, "xx", ifelse(summary.main$`Pr(>|t|)`<0.05, "x", ifelse(is.na(summary.main$`Pr(>|t|)`), "", ""))))
    summary.main$sig[1] <- ""
    summary.main <- cbind(rownames(summary.main), data.frame(summary.main, row.names=NULL))
    names(summary.main) <- c("term", "Estimate", "StdError", "t.value", "p.value", "sig")
    
    summary.int_urban <- as.data.frame(summary(svy.glm.int_urban)$coefficients)
    summary.int_urban$sig <-  ifelse(summary.int_urban$`Pr(>|t|)`<0.001, "xxx", ifelse(summary.int_urban$`Pr(>|t|)`<0.01, "xx", ifelse(summary.int_urban$`Pr(>|t|)`<0.05, "x", ifelse(is.na(summary.int_urban$`Pr(>|t|)`), "", ""))))
    summary.int_urban$sig[1] <- ""
    summary.int_urban <- cbind(rownames(summary.int_urban), data.frame(summary.int_urban, row.names=NULL))
    names(summary.int_urban) <- c("term", "Estimate", "StdError", "t.value", "p.value", "sig")
    
    if(int_marrcohab == TRUE){
      summary.int_pship <- as.data.frame(summary(svy.glm.int_pship)$coefficients)
      summary.int_pship$sig <-  ifelse(summary.int_pship$`Pr(>|t|)`<0.001, "xxx", ifelse(summary.int_pship$`Pr(>|t|)`<0.01, "xx", ifelse(summary.int_pship$`Pr(>|t|)`<0.05, "x", ifelse(is.na(summary.int_pship$`Pr(>|t|)`), "", ""))))
      summary.int_pship$sig[1] <- ""
      summary.int_pship <- cbind(rownames(summary.int_pship), data.frame(summary.int_pship, row.names=NULL))
      names(summary.int_pship) <- c("term", "Estimate", "StdError", "t.value", "p.value", "sig")
    }
    
    
    # Check if interaction terms are significant and extract relevant p-value - models with only one interaction term ---------------
    LRT.pval.int.urban <- regTermTest(svy.glm.int_urban, "var:hh.urban", method = "LRT")$p
    
    if(int_marrcohab == TRUE){
      LRT.pval.int.marrcohab <- regTermTest(svy.glm.int_pship, "var:marr.cohab", method = "LRT")$p
    } else{
      # HPM7: Added this in, otherwise later if else statement wont find it
      LRT.pval.int.marrcohab <- NA_real_
    }
    
    # If one of these is significant, run stratified models and report the most significant main effect p-value
    if(int_marrcohab == TRUE){
      if (LRT.pval.int.urban <0.05 & LRT.pval.int.marrcohab >=0.05){
        interaction <- "Urban"
        svy.glm <- svy.glm.int_urban
        summary <- summary.int_urban
        # Stratified models for p-value
        if(binary.out == FALSE){
          if(poisson == TRUE) {
            svy.glm.urban <- svyglm(outcome ~ var,family=quasipoisson,design=subset(df.design, hh.urban == 1))
            svy.glm.rural <- svyglm(outcome ~ var,family=quasipoisson,design=subset(df.design, hh.urban == 0))
          }else{
            svy.glm.urban <- svyglm(outcome ~ var,family=gaussian(link ="identity"),design=subset(df.design, hh.urban == 1))
            svy.glm.rural <- svyglm(outcome ~ var,family=gaussian(link ="identity"),design=subset(df.design, hh.urban == 0))
          }
        }else{
          svy.glm.urban <- svyglm(outcome ~ var,family=quasibinomial,design=subset(df.design, hh.urban == 1))
          svy.glm.rural <- svyglm(outcome ~ var,family=quasibinomial,design=subset(df.design, hh.urban == 0))
        }
        # Take the minimum of the two p-values
        LRT.pval.urban <- regTermTest(svy.glm.urban, "var", method = "LRT")$p
        LRT.pval.rural <- regTermTest(svy.glm.rural, "var", method = "LRT")$p
        LRT.pval <- min(LRT.pval.urban, LRT.pval.rural)
        
      } else if (LRT.pval.int.urban >=0.05 & LRT.pval.int.marrcohab <0.05) {
        interaction <- "Partnership status"
        svy.glm <- svy.glm.int_pship
        summary <- summary.int_pship
        # Stratified models for p-value
        if(binary.out == FALSE){
          if(poisson == TRUE) {
            svy.glm.partnered <- svyglm(outcome ~ var,family=quasipoisson,design=subset(df.design, marr.cohab == 1))
            svy.glm.unpartnered <- svyglm(outcome ~ var,family=quasipoisson,design=subset(df.design, marr.cohab == 0))
          }else{
            svy.glm.partnered <- svyglm(outcome ~ var,family=gaussian(link ="identity"),design=subset(df.design, marr.cohab == 1))
            svy.glm.unpartnered <- svyglm(outcome ~ var,family=gaussian(link ="identity"),design=subset(df.design, marr.cohab == 0))
          }
        }else{
          svy.glm.partnered <- svyglm(outcome ~ var,family=quasibinomial,design=subset(df.design, marr.cohab == 1))
          svy.glm.unpartnered <- svyglm(outcome ~ var,family=quasibinomial,design=subset(df.design, marr.cohab == 0))
        }
        
        # Take the minimum of the two p-values
        LRT.pval.partnered <- regTermTest(svy.glm.partnered, "var", method = "LRT")$p
        LRT.pval.unpartnered <- regTermTest(svy.glm.unpartnered, "var", method = "LRT")$p
        LRT.pval <- min(LRT.pval.partnered, LRT.pval.unpartnered)
        
      }
    }else if (int_marrcohab == FALSE & LRT.pval.int.urban <0.05) {
      interaction <- "Urban"
      svy.glm <- svy.glm.int_urban
      summary <- summary.int_urban
      # Stratified models for p-value
      if(binary.out == FALSE){
        if(poisson == TRUE) {
          svy.glm.urban <- svyglm(outcome ~ var,family=quasipoisson,design=subset(df.design, hh.urban == 1))
          svy.glm.rural <- svyglm(outcome ~ var,family=quasipoisson,design=subset(df.design, hh.urban == 0))
        }else{
          svy.glm.urban <- svyglm(outcome ~ var,family=gaussian(link ="identity"),design=subset(df.design, hh.urban == 1))
          svy.glm.rural <- svyglm(outcome ~ var,family=gaussian(link ="identity"),design=subset(df.design, hh.urban == 0))
        }
      }else{
        svy.glm.urban <- svyglm(outcome ~ var,family=quasibinomial,design=subset(df.design, hh.urban == 1))
        svy.glm.rural <- svyglm(outcome ~ var,family=quasibinomial,design=subset(df.design, hh.urban == 0))
      }
      # Take the minimum of the two p-values
      LRT.pval.urban <- regTermTest(svy.glm.urban, "var", method = "LRT")$p
      LRT.pval.rural <- regTermTest(svy.glm.rural, "var", method = "LRT")$p
      LRT.pval <- min(LRT.pval.urban, LRT.pval.rural)
    }
    
    # If both are significant, run a combined model
    if(int_marrcohab == TRUE){
      if(LRT.pval.int.urban <0.05 & LRT.pval.int.marrcohab <0.05) {
        # run combined model
        if(binary.out == FALSE){
          if(poisson == TRUE) {
            svy.glm.int_both <- svyglm(outcome ~ var*hh.urban + var*marr.cohab,family=quasipoisson,design=subset(df.design, both.int.include ==1))
          }else{
            svy.glm.int_both <- svyglm(outcome ~ var*hh.urban + var*marr.cohab,family=gaussian(link ="identity"),design=subset(df.design, both.int.include ==1))
          }
        }else{
          svy.glm.int_both<-svyglm(outcome ~ var*hh.urban + var*marr.cohab,family=quasibinomial,design=subset(df.design, both.int.include ==1))
        }
        # Get LRT p-values for each interaction term
        LRT.pval.int.urban.both <- regTermTest(svy.glm.int_both, "var:hh.urban", method = "LRT")$p
        LRT.pval.int.marrcohab.both <- regTermTest(svy.glm.int_both, "var:marr.cohab", method = "LRT")$p
        
        # If both of those are significant, define the summary object for both and get p-values from 4-way stratified models -- report the min p-value
        if(LRT.pval.int.urban.both <0.05 & LRT.pval.int.marrcohab.both <0.05) {
          # Model summary
          summary.int_both <- as.data.frame(summary(svy.glm.int_both)$coefficients)
          summary.int_both$sig <-  ifelse(summary.int_both$`Pr(>|t|)`<0.001, "xxx", ifelse(summary.int_both$`Pr(>|t|)`<0.01, "xx", ifelse(summary.int_both$`Pr(>|t|)`<0.05, "x", ifelse(is.na(summary.int_both$`Pr(>|t|)`), "", ""))))
          summary.int_both$sig[1] <- ""
          summary.int_both <- cbind(rownames(summary.int_both), data.frame(summary.int_both, row.names=NULL))
          names(summary.int_both) <- c("term", "Estimate", "StdError", "t.value", "p.value", "sig")
          
          # 4-way stratification for p-values
          if(binary.out == FALSE){
            if(poisson == TRUE) {
              svy.glm.urban_partnered <- svyglm(outcome ~ var,family=quasipoisson,design=subset(df.design, hh.urban == 1 & marr.cohab == 1))
              svy.glm.urban_unpartnered <- svyglm(outcome ~ var,family=quasipoisson,design=subset(df.design, hh.urban == 1 & marr.cohab == 0))
              svy.glm.rural_partnered <- svyglm(outcome ~ var,family=quasipoisson,design=subset(df.design, hh.urban == 0 & marr.cohab == 1))
              svy.glm.rural_unpartnered <- svyglm(outcome ~ var,family=quasipoisson,design=subset(df.design, hh.urban == 0 & marr.cohab == 0))
            }else{
              svy.glm.urban_partnered <- svyglm(outcome ~ var,family=gaussian(link ="identity"),design=subset(df.design, hh.urban == 1 & marr.cohab == 1))
              svy.glm.urban_unpartnered <- svyglm(outcome ~ var,family=gaussian(link ="identity"),design=subset(df.design, hh.urban == 1 & marr.cohab == 0))
              svy.glm.rural_partnered <- svyglm(outcome ~ var,family=gaussian(link ="identity"),design=subset(df.design, hh.urban == 0 & marr.cohab == 1))
              svy.glm.rural_unpartnered <- svyglm(outcome ~ var,family=gaussian(link ="identity"),design=subset(df.design, hh.urban == 0 & marr.cohab == 0))
            }
          }else{
            svy.glm.urban_partnered <- svyglm(outcome ~ var,family=quasibinomial,design=subset(df.design, hh.urban == 1 & marr.cohab == 1))
            svy.glm.urban_unpartnered <- svyglm(outcome ~ var,family=quasibinomial,design=subset(df.design, hh.urban == 1 & marr.cohab == 0))
            svy.glm.rural_partnered <- svyglm(outcome ~ var,family=quasibinomial,design=subset(df.design, hh.urban == 0 & marr.cohab == 1))
            svy.glm.rural_unpartnered <- svyglm(outcome ~ var,family=quasibinomial,design=subset(df.design, hh.urban == 0 & marr.cohab == 0))
          }
          
          # Set p-value to min of the 4 LRT p-values
          LRT.pval.urban.partnered <- regTermTest(svy.glm.urban_partnered, "var", method = "LRT")$p
          LRT.pval.urban.unpartnered <- regTermTest(svy.glm.urban_unpartnered, "var", method = "LRT")$p
          LRT.pval.rural.partnered <- regTermTest(svy.glm.rural_partnered, "var", method = "LRT")$p
          LRT.pval.rural.unpartnered <- regTermTest(svy.glm.rural_unpartnered, "var", method = "LRT")$p
          
          LRT.pval <- min(LRT.pval.urban.partnered, LRT.pval.urban.unpartnered, LRT.pval.rural.partnered, LRT.pval.rural.unpartnered)
          
          interaction <- "Both"
          svy.glm <- svy.glm.int_both
          summary <- summary.int_both
          
          # If the urban interaction is significant and the pship interaction is not, run stratified model by urban and report smaller of the two LRT p-values
        } else if (LRT.pval.int.urban.both <0.05 & LRT.pval.int.marrcohab.both >=0.05){
          interaction <- "Urban"
          svy.glm <- svy.glm.int_urban
          summary <- summary.int_urban
          
          # Stratified models for p-value
          svy.glm.urban <- svyglm(outcome ~ var,family=quasipoisson,design=subset(df.design, hh.urban == 1))
          svy.glm.rural <- svyglm(outcome ~ var,family=quasipoisson,design=subset(df.design, hh.urban == 0))
          # Take the minimum of the two p-values
          LRT.pval.urban <- regTermTest(svy.glm.urban, "var", method = "LRT")$p
          LRT.pval.rural <- regTermTest(svy.glm.rural, "var", method = "LRT")$p
          LRT.pval <- min(LRT.pval.urban, LRT.pval.rural)
          
          # If the pship interaction is significant and the urban interaction is not, run stratified model by pship status and report smaller of the two LRT p-values
        } else if (LRT.pval.int.urban.both >=0.05 & LRT.pval.int.marrcohab.both <0.05) {
          interaction <- "Partnership status"
          svy.glm <- svy.glm.int_pship
          summary <- summary.int_pship
          
          # Stratified models for p-value
          svy.glm.partnered <- svyglm(outcome ~ var,family=quasipoisson,design=subset(df.design, marr.cohab == 1))
          svy.glm.unpartnered <- svyglm(outcome ~ var,family=quasipoisson,design=subset(df.design, marr.cohab == 0))
          # Take the minimum of the two p-values
          LRT.pval.partnered <- regTermTest(svy.glm.partnered, "var", method = "LRT")$p
          LRT.pval.unpartnered <- regTermTest(svy.glm.unpartnered, "var", method = "LRT")$p
          LRT.pval <- min(LRT.pval.partnered, LRT.pval.unpartnered)
        }
      }
    }
    
    # If neither interaction term is significant, test the measure in the main effect model
    if((int_marrcohab == TRUE & LRT.pval.int.urban >=0.05 & LRT.pval.int.marrcohab >=0.05) | 
       (int_marrcohab == FALSE & LRT.pval.int.urban >=0.05)) {
      LRT.pval.main <- regTermTest(svy.glm.main, "var", method = "LRT")$p
      LRT.pval <- LRT.pval.main
      interaction <- "None"
      svy.glm <- svy.glm.main
      summary <- summary.main
    } 
    
    
    # Plot predicted probabilities-------------------------
    
    ## Make the data frame for prediction
    if(is.integer(dhs.seg$var) | is.numeric(dhs.seg$var)) {
      pred.data<-data.frame(unique(na.omit(dhs.seg$var)))
    } else {
      pred.data<-data.frame(svy.glm$xlevels[[1]])
    }
    names(pred.data) <- "var"
    
    ## If the interaction with urban is significant, pred.data should include urban.rural
    if(interaction == "Urban"){
      nlevels <- nrow(pred.data) # Calculate the number of levels of the measure
      pred.data <- cbind.data.frame(rep(pred.data[,1],2),
                                    c(rep(1, nlevels), rep(0, nlevels)))
      names(pred.data) <- c("var", "hh.urban")
      pred.data$var <- factor(pred.data$var)
      
      # Reorder alphabetically (for cases where set the reference level)
      if (reordered == TRUE){
        levels <- levels(pred.data$var)
        pred.data$var <- factor(pred.data$var, levels = sort(levels))
      }
    }
    ## If the interaction with pship status is significant, pred.data should include marr.cohab
    if(interaction == "Partnership status"){
      nlevels <- nrow(pred.data) # Calculate the number of levels of the measure
      pred.data <- cbind.data.frame(rep(pred.data[,1],2),
                                    c(rep(1, nlevels), rep(0, nlevels)))
      names(pred.data) <- c("var", "marr.cohab")
      pred.data$var <- factor(pred.data$var)
      
      # Reorder alphabetically (for cases where set the reference level)
      if (reordered == TRUE){
        levels <- levels(pred.data$var)
        pred.data$var <- factor(pred.data$var, levels = sort(levels))
      }
    }
    
    ## If both interaction terms are significant, pred.data should include marr.cohab and hh.urban
    if(interaction == "Both"){
      nlevels <- nrow(pred.data) # Calculate the number of levels of the measure
      pred.data <- cbind.data.frame(rep(pred.data[,1],4),
                                    c(rep(1, nlevels), rep(0, nlevels), rep(1, nlevels), rep(0, nlevels)),
                                    c(rep(1, nlevels*2), rep(0, nlevels*2)))
      names(pred.data) <- c("var", "hh.urban", "marr.cohab")
      pred.data$var <- factor(pred.data$var)
      
      # Reorder alphabetically (for cases where set the reference level)
      if (reordered == TRUE){
        levels <- levels(pred.data$var)
        pred.data$var <- factor(pred.data$var, levels = sort(levels))
      }
    }
    
    
    ## Predict values 
    probabilities <- predict(svy.glm, pred.data, type = "response", se.fit=T, na.action = na.omit)
    
    prob1<-as.data.frame(probabilities)
    prob1$lower<- prob1$response - (1.96*prob1$SE)
    prob1$upper<- prob1$response + (1.96*prob1$SE)
    prob1 <- cbind.data.frame(pred.data, prob1)
    
    # Add labels for interaction var and include p-values 
    if("marr.cohab" %in% names(prob1) & interaction == "Partnership status") {
      # round p-values
      pval.partnered <- round(LRT.pval.partnered,3)
      pval.unpartnered <- round(LRT.pval.unpartnered,3)
      # Set labels
      prob1$marr.cohab <- factor(prob1$marr.cohab, levels = c(0, 1), labels = c(paste0("Not partnered"," (p=", pval.unpartnered, ")"), paste0("Married/cohabitating"," (p=", pval.partnered, ")")))
    }
    
    if("hh.urban" %in% names(prob1) & interaction == "Urban") {
      # round p-values
      pval.urban <- round(LRT.pval.urban,3)
      pval.rural <- round(LRT.pval.rural,3)
      # Set labels
      prob1$hh.urban <- factor(prob1$hh.urban, levels = c(0, 1), labels = c(paste0("Rural"," (p=", pval.rural, ")"), paste0("Urban"," (p=", pval.urban, ")")))
    }
    if(interaction == "Both") {
      # round p-values
      pval.urban.partnered <- round(LRT.pval.urban.partnered,3)
      pval.urban.unpartnered <- round(LRT.pval.urban.unpartnered,3)
      pval.rural.partnered <- round(LRT.pval.rural.partnered,3)
      pval.rural.unpartnered <- round(LRT.pval.rural.unpartnered,3)
      
      # Set labels
      prob1$marr.cohab <- factor(prob1$marr.cohab, levels = c(0, 1), labels = c("Not partnered", "Married/cohabitating"))
      prob1$hh.urban <- factor(prob1$hh.urban, levels = c(0, 1), labels = c("Rural","Urban"))
      prob1$joint.int <- paste0(prob1$hh.urban, ", ", prob1$marr.cohab)
      prob1$joint.int[prob1$joint.int == "Urban, Married/cohabitating"] <- paste0("Urban,\nMarried/cohabitating", "\n(p=", pval.urban.partnered, ")")
      prob1$joint.int[prob1$joint.int == "Rural, Married/cohabitating"] <- paste0("Rural,\nMarried/cohabitating", "\n(p=", pval.rural.partnered, ")")
      prob1$joint.int[prob1$joint.int == "Urban, Not partnered"] <- paste0("Urban,\nNot partnered", "\n(p=", pval.urban.unpartnered, ")")
      prob1$joint.int[prob1$joint.int == "Rural, Not partnered"] <- paste0("Rural,\nNot partnered", "\n(p=", pval.rural.unpartnered, ")")
    }
    
    # Bring in the significance, for models with factor vars and only main effects (complicated for interaction)
    if(interaction == "None" & (is.factor(dhs.seg$var) | is.character(dhs.seg$var))) {
      ncoefs <- length(svy.glm$coefficients)
      
      if(ncoefs ==2){
        labs = "var"
      } else {
        labs = svy.glm$xlevels[[1]][-1]
      }
      coefs <- cbind.data.frame(labels = labs, coefs = as.vector(svy.glm$coefficients[2:ncoefs]),
                                se = as.vector(coef(summary(svy.glm))[2:ncoefs,2]),
                                sig = as.vector(summary$sig[2:ncoefs]))
      coefs$sig.bin <- ifelse(coefs$sig %in% c("xxx", "xx", "x"), "yes", "no")
      
      significance <- data.frame(coefs$labels, coefs$sig.bin)
      names(significance) <- c("var", "sig.bin")
      prob1 <- merge(prob1, significance, by="var", all.x = T)
    }
    
    
    ## Plot
    
    # Set plot colors colors
    colors <- viridis(n = 4, end = 0.8, option = "D")
    # If no interaction and factor or character variable, plot the main effects with color coding for dummy var significance
    if (interaction == "None" & (is.factor(dhs.seg$var) | is.character(dhs.seg$var))) {
      plot.prob <- ggplot(prob1, aes(x=var, y=response)) + 
        geom_point(aes(colour = sig.bin), alpha=0.9, size=2, show.legend = FALSE) +
        geom_errorbar(aes(ymin=lower, ymax=upper), linewidth=0.5, width=.4, show.legend = FALSE) +
        geom_text(aes(y=response, x=var, label=round(response,2)), vjust=-1.2, hjust=.5) +
        scale_color_manual(name = "significance",
                           breaks = c("no", "yes"),
                           values = c("Black", "Red")) +
        coord_flip()+
        xlab("") +
        ylab(paste0("Probability of ",outcome)) +
        theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 12), axis.title=element_text(size=12)) + 
        theme_bw()
    }
    # If no interaction and numeric or interger variable, plot the main effects
    if (interaction == "None" & (is.numeric(dhs.seg$var) | is.integer(dhs.seg$var))) {
      plot.prob <- ggplot(prob1, aes(x=var, y=response)) + 
        geom_point(alpha=0.9, size=2, show.legend = FALSE) +
        geom_errorbar(aes(ymin=lower, ymax=upper), linewidth=0.5, width=.4, show.legend = FALSE) +
        geom_text(aes(y=response, x=var, label=round(response,2)), vjust=-1.2, hjust=.5) +
        coord_flip()+
        xlab("") +
        ylab(paste0("Probability of ",outcome)) +
        theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 12), axis.title=element_text(size=12))
    }
    # If urban interaction is significant
    if (interaction == "Urban") {
      plot.prob <- ggplot(prob1, aes(x=var, y=response, colour = hh.urban)) + 
        geom_point(alpha=0.9, size=2, position = position_dodge(width = 0.7)) +
        geom_errorbar(aes(ymin=lower, ymax=upper), linewidth=0.5, width=.4, show.legend = FALSE, position = position_dodge(width = 0.7)) +
        geom_text(aes(y=response, x=var, label=round(response,2)), vjust=-1.2, hjust=.5, position = position_dodge(width = 0.7)) +
        scale_color_manual(name = "Urban", values = c(colors[1], colors[2])) + 
        coord_flip()+
        xlab("") +
        ylab(paste0("Probability of ",outcome)) +
        theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 12), axis.title=element_text(size=12),
              legend.position = "bottom") + 
        theme_bw()
    }
    # If marr.cohab interaction is significant
    if (interaction == "Partnership status") {
      plot.prob <- ggplot(prob1, aes(x=var, y=response, colour = marr.cohab)) + 
        geom_point(alpha=0.9, size=2, position = position_dodge(width = 0.7)) +
        geom_errorbar(aes(ymin=lower, ymax=upper), linewidth=0.5, width=.4, show.legend = FALSE, position = position_dodge(width = 0.7)) +
        geom_text(aes(y=response, x=var, label=round(response,2)), vjust=-1.2, hjust=.5, position = position_dodge(width = 0.7)) +
        scale_color_manual(name = "Married/cohabitating", values = c(colors[3], colors[4])) +
        coord_flip()+
        xlab("") +
        ylab(paste0("Probability of ",outcome)) +
        theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 12), axis.title=element_text(size=12),
              legend.position = "bottom") + 
        theme_bw()
    }
    # If both interactions are significant
    if (interaction == "Both") {
      plot.prob <- ggplot(prob1, aes(x= var, y=response, colour = joint.int)) + 
        geom_point(aes(), alpha=0.9, size=2, position = position_dodge(width = 0.7)) +
        geom_errorbar(aes(ymin=lower, ymax=upper), linewidth=0.5, width=.4, show.legend = FALSE, position = position_dodge(width = 0.7)) +
        geom_text(aes(y=response, x=var, label=round(response,2)), vjust=-1.2, hjust=.5,  position = position_dodge(width = 0.7)) +
        scale_color_manual(name = "Stratum", values = colors) +
        coord_flip()+
        xlab("") +
        ylab(paste0("Probability of ",outcome)) +
        theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 12), axis.title=element_text(size=12),
              legend.position = "bottom") +
        guides(colour = guide_legend(nrow = 2)) + 
        theme_bw()
    }
    
    
    var = substitute(measure)
    
    dhs.seg <- dhs.seg %>% select(-c(var, outcome))
    
    out <- list(stats, interaction, summary, LRT.pval, plot.prob, var)
    
    return(out)
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
    message(paste("Done with:", measure, outcome))
  })
  return(tryCatchResults) 
}


# Multiplot function for combining plots ---------------------------------------

multiplot <- function(..., plotlist=NULL, file, cols=2, layout=NULL, title) {
  
  
  #* Make a list from the arguments and plotlist -------------------------------
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  #* If layout is NULL, then use 'cols' to determine layout --------------------
  
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
    
  #* Set up the page -----------------------------------------------------------
  
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout) + 1, ncol(layout), heights = c(0.2, rep(1, times =nrow(layout))))))
    
  #* Make each plot, in the correct location -----------------------------------
    
    #** Add title --------------------------------------------------------------
  
    grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row + 1,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# Explore associations ---------------------------------------------------------

##' NOTES: for a new variable, copy and paste the code under an existing variable. Replace the variable name, and then
##' find and replace the prefix (e.g. "ed.") with a new prefix for the variable of interest.
##' Among the results, I suggest focusing on the plots of the predicted probabilities (plots.prob) first. These are grouped
##' by mom-focused and baby-focused outcomes. The color coding is: grey - reference level, red = p<0.05, black = p>=0.05.
##' Whether or not any levels were significantly different from the null for each outcome will be added to a data frame that 
##' can be exported.
create_assoc.summary <- function(measure, 
                                 outcomes.list,
                                 poisson.vars, 
                                 int_marrcohab,
                                 subset){
  
  
  # measure = "num.children"
  # poisson.vars = "anc.num"
  # int_marrcohab = TRUE

  dir.create(file.path("Data/KE_2022_constructed_data/", 
                       "5_assoc_summaries", 
                       measure), 
             recursive = T, showWarnings = F)
  
  ## If running for the first time, run the following. Otherwise, uncomment and run the code below instead
  assoc.summary <- as.data.frame(matrix(nrow = length(names(vulnerability)), ncol = length(outcomes.list)))
  colnames(assoc.summary) <- outcomes.list
  assoc.summary$factor <- names(vulnerability)
  assoc.summary <- assoc.summary %>% select(factor, everything())
  assoc.summary <- assoc.summary[8:length(names(vulnerability)),]
  
  
# Define a vector for whether outcomes are poisson -----------------------------
  
  poisson.yn <- outcomes.list %in% poisson.vars
  
  ##' ==> Run the below code for each new variable. 
  ##' ==> Replace the variable name in quotes in the call to f_pattern_explore and, if a household variable, set age.adjust == FALSE
  ##' ==> if the factor is numeric/integer, try forcing it to factor (replace "var" with the variable name)
  
  stats <- list()
  interaction <- list()
  summary <- list()
  LRT.vals <- list()
  any.sig.levels <- as.data.frame(matrix(nrow = 1, ncol = length(outcomes.list)))
  colnames(any.sig.levels) <- outcomes.list
  plots.prob <- list()
  var <- list()

  for(i in 1:length(outcomes.list)){
    
    print(i)
    
    data_availability <- data.table(dhs.seg.long)
    data_availability <- data_availability[,.N,.(measure = get(measure), outcome = get(outcomes.list[i]))]
    data_availability <- data_availability[!is.na(outcome) & !is.na(measure)]
    
    if(nrow(data_availability) < 3){
      write.csv(data_availability, file.path("Data/KE_2022_constructed_data", "5_assoc_summaries", measure, paste0("WARNING_not_enough_variability_",outcomes.list[i],".csv")))
      next
    }
    
    out <- f_pattern_explore(measure = measure, 
                             outcome = outcomes.list[i], 
                             poisson = poisson.yn[i], 
                             int_marrcohab = int_marrcohab,
                             subset = subset)
    if(!is.null(out)){
      stats[[i]] <- out[[1]]
      interaction[[i]] <- out[[2]]
      summary[[i]] <- out[[3]]
      LRT.vals[[i]] <- out[[4]]
      any.sig.levels[[i]] <- out[[4]][1] # p-value from LRT for summary
      plots.prob[[i]] <- out[[5]]
      var <- measure
    } else{
      stats[[i]] <- NA
      interaction[[i]] <- NA
      summary[[i]] <- NA
      LRT.vals[[i]] <- NA
      any.sig.levels[[i]] <- NA # p-value from LRT for summary
      plots.prob[[i]] <- ggplot() + geom_blank() + geom_text(aes(x = 1, y = 1, label = paste0("Model failed")))
      var <- measure

    }
    
   
  }
  
  if(!exists("out")){
    message("No output")
    write.csv(data.table(warning = "No output"), file.path("Data/KE_2022_constructed_data", "5_assoc_summaries", measure, paste0("WARNING_no_output.csv")))
  } else{
    
    names(stats) <- outcomes.list
    names(interaction) <- outcomes.list
    names(summary) <- outcomes.list
    names(LRT.vals) <- outcomes.list
    names(plots.prob) <- outcomes.list

# Look at results (I find looking at the plots much more informative; this info is harder to digest but here if needed) ----
   
    saveRDS(stats, file.path("Data/KE_2022_constructed_data", "5_assoc_summaries", measure, "stats.rds"))
    saveRDS(summary, file.path("Data/KE_2022_constructed_data", "5_assoc_summaries", measure, "summary.rds"))
    saveRDS(LRT.vals, file.path("Data/KE_2022_constructed_data", "5_assoc_summaries", measure, "LRT.vals.rds"))
    # saveRDS(plots.prob, file.path("Data/KE_2022_constructed_data", "5_assoc_summaries", measure, "plots.prob.rds"))
    
    pdf(file.path("Data/KE_2022_constructed_data", "5_assoc_summaries", measure, paste0(measure, ".pdf")), height = 24, width = 19)
    
    for(categ.i in unique(outcomes_list$PDFGrouping)){
      
      #categ.i = unique(outcomes_list$PDFGrouping)[3]
      
      outcomes.list.i <- outcomes_list[PDFGrouping == categ.i, index]
      outcomes.num_signif <- sum(unlist(lapply(LRT.vals[outcomes.list.i], function(x){x[[1]][1]})) < 0.05, na.rm=T) 
      multiplot(plotlist = plots.prob[outcomes.list.i], title = paste0(categ.i, " - ", measure, "\nNumber Significant: ", round(outcomes.num_signif/length(outcomes.list), 3)*100, "% (", outcomes.num_signif, "/", length(outcomes.list.i), ")"))
      
    }
    
    dev.off()
    
# Merge any.sig.levels with the assoc.summary object ---------------------------
    
    any.sig.levels$factor <- var
    
    assoc.summary[assoc.summary$factor == any.sig.levels$factor, 2:ncol(assoc.summary)] <- any.sig.levels[-ncol(any.sig.levels)]
    #assoc.summary <- merge(assoc.summary, any.sig.levels, by = "factor", all.x = TRUE)
    
    
# Save assoc.summary -----------------------------------------------------------
    
    assoc.summary <- data.table(assoc.summary)
    assoc.summary <- assoc.summary[factor == measure]
    
    saveRDS(assoc.summary, file.path("Data/KE_2022_constructed_data", "5_assoc_summaries", measure, "5_assoc.summary.rds")) 
    write.csv(assoc.summary, file.path("Data/KE_2022_constructed_data/", "5_assoc_summaries", measure, "5_assoc.summary.csv"),row.names=F) 
    
  }
  
  
}

# List of variables currently in the folder ------------------------------------

existing_files <- list.dirs("Data/KE_2022_constructed_data/5_assoc_summaries", recursive = F, full.names = F) 

# Code to populate folders for added variables -----
# Note: To rerun for all variables, in the dropbox change 5_assoc_summaries to a new file name with old date
list.vuln_to_create <- unique(vulnerability_list[!vulnerability_list %in% existing_files & vulnerability_list %in% names(vulnerability)])

for(measure in list.vuln_to_create){

  #measure = "slum2"
  
  message(measure)
  create_assoc.summary(measure = measure, 
                       outcomes.list = outcomes.list,
                       poisson.vars = poisson.vars, #update this line if have addition outcomes which are counts 
                       int_marrcohab = ifelse(measure %in% vulns.int_marrcohab, TRUE, FALSE),
                       subset = unique(vulns.subset_map[vulnerability == measure, exploratory_subset]))
  
}


# Missing vulnerabilities ------------------------------------------------------

vulnerability_list[!vulnerability_list %in% names(vulnerability)]

