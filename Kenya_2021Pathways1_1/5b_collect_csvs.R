#########################  KENYA_DHS22_collect_csvs  ###########################

# Purpose: Create a csv files that summarized the exploratory analysis
# Authors: Helena Manguerra
# Input files: "5_assoc.summary.csv"              
# Last edited: Nov 8th, 2023 (HM)
# Output files: "5_assoc.summary_full.csv" 
# Ancestor scripts: "5_Kenya_DHS_exploratory_13Dec2023.R"
# Descendant script: "6_Kenya_DHS_age explore.R"
#_______________________________________________________________________________ 


# Clearing the workspace -------------------------------------------------------

rm(list=ls())

# Load libraries ---------------------------------------------------------------

library(data.table)
library(magrittr)
library(openxlsx)
library(ggplot2)

# Set working directory --------------------------------------------------------

user = "Andrea"

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


# List directories -------------------------------------------------------------

vulns <- list.dirs("Data/KE_2022_constructed_data/5_assoc_summaries/", full.names = F, recursive = F)

vulns <- vulns[which(vulns != "")]


# Apply function to list -------------------------------------------------------

assoc_summaries <- lapply(vulns, function(vuln){
  
  #vuln = "ed.lev2"
  
  fp = file.path("Data/KE_2022_constructed_data/5_assoc_summaries", vuln, "5_assoc.summary.csv")
  if(file.exists(fp)){
    data<-fread(fp)
    data[, fp:=fp]
    return(data)
  }
  
  
})


# Combine objects by row -------------------------------------------------------

assoc_summaries <- rbindlist(assoc_summaries,use.names =T, fill = T)



interaction_counts <- lapply(vulns, function(vuln){
  
  #vuln = "age.1stbrth"
  
  print(vuln)
  fp = file.path("Data/KE_2022_constructed_data/5_assoc_summaries", vuln, "summary.rds")
  if(file.exists(fp)){
    
    data<-readRDS(fp)
    
    vuln_counts <- lapply(1:length(data), function(i){
      #i=22
      print(i)
      summary <- data.table(data[[i]])
      
      if(nrow(summary) <= 1){
        
      } else{
        return(data.table(factor = vuln,
                          outcome = names(data[i]),
                          int_marr.cohab = ifelse(summary[term %like% ":marr.cohab", .N] > 0, 1, 0), 
                          int_urban.rural = ifelse(summary[term %like% ":hh.urban", .N] > 0, 1, 0)))
      }
      

      
    })
    
    vuln_counts <- rbindlist(vuln_counts, use.names=T, fill =T)
    
    return(vuln_counts)
  }
  
  
})

interaction_counts <- rbindlist(interaction_counts, use.names = T, fill = T)



# Save as csv file -------------------------------------------------------------

# 
# exploratory <- data.table(read.xlsx(file.path("Data/KE_2022_constructed_data/Exploratory_analysis_Kenya_updated.xlsx"), sheet = "univariate_results_KE_decisions"))
# 
# names(exploratory) <- as.character(exploratory[2, ])
# exploratory <- exploratory[3:nrow(exploratory)]
# exploratory[, index := .I]

vulnerability_list <- data.table(read.xlsx("Data/KE_2022_constructed_data/0_Kenya_DHS_Data_Dictionary.xlsx", sheet = "Vulnerabilities_full_list_SS"))

names(vulnerability_list) <- as.character(vulnerability_list[5, ])
vulnerability_list <- vulnerability_list[!(is.na(`variable name`))]
vulnerability_list <- vulnerability_list[`variable name` != "variable name"]

vulnerability_list <- vulnerability_list[, .(`variable name`)]

vulnerability_list[,.N,`variable name`][N>1]

vulnerability_list <- vulnerability_list[!duplicated(vulnerability_list)]

vulnerability_list[, index := .I]

# exploratory <- exploratory[, .(`variable explored`, 
                            # `Final decision - exploratory analysis`,`Notes on age associations`,`Notes on associations with outcomes (e.g. patterns to inform categorization, or if the main significant difference is between those who were not asked the original question, such as those not partnered, vs. all others); mostly decreasing risk with age but not in all cases - would hypothesize youngest/oldest ages might have some similar probabilities for some outcomes; not sure the plots provide clear guidance on cutoffs/a threshold `)]

setnames(assoc_summaries, "factor", "variable")

assoc_summaries <- merge(vulnerability_list[, .(index, variable = `variable name`)], 
                         assoc_summaries,
                         all = T, sort = F, by = "variable")

assoc_summaries <- assoc_summaries[, -c("fp", "anc.8plus")]

outcomes_list <- data.table(read.xlsx("Data/KE_2022_constructed_data/0_Kenya_DHS_Data_Dictionary.xlsx", sheet = "Outcomes (updated)"))
outcomes_list <- outcomes_list[keep == "Keep"]

mat.outcomes <-outcomes_list[Category %like% "Maternal", variable.name]
woman.outcomes <-outcomes_list[Category %like% "Women", variable.name]
child.outcomes <-outcomes_list[Category %like% "Child", variable.name]

counts <- melt(assoc_summaries, id.vars = c("index", "variable"), variable.name = "outcome")
counts <- merge(counts, outcomes_list[, .(outcome = variable.name, Category)], all.x=T)
counts[, signif := ifelse(value < 0.05, 1, 0)]

counts <- counts[, .(signif_pvals = sum(signif, na.rm=T)), by = .(index, variable, Category)]

counts <- dcast(counts, index + variable ~ Category)
counts[, Total := `Child outcomes` + `Maternal outcomes` + `Women's health`]

assoc_summaries <- merge(assoc_summaries, counts, all.x=T, sort = F)

assoc_summaries <- assoc_summaries[order(index)]

interaction_counts[outcome %in% mat.outcomes, outcome_category := "Maternal outcomes"]
interaction_counts[outcome %in% woman.outcomes, outcome_category := "Women's health"]
interaction_counts[outcome %in% child.outcomes, outcome_category := "Child outcomes"]

interaction_counts <- 
  interaction_counts[,.(int_marr.cohab = sum(int_marr.cohab), 
                        int_urban.rural = sum(int_urban.rural)), by = .(factor, outcome_category)]

interaction_counts <- dcast(interaction_counts, factor ~ outcome_category, value.var = c("int_marr.cohab", "int_urban.rural"))
interaction_counts[, int_urban.rural_Total := `int_urban.rural_Child outcomes` + `int_urban.rural_Maternal outcomes` + `int_urban.rural_Women's health`]
interaction_counts[, int_mar.cohab_Total := `int_marr.cohab_Child outcomes` + `int_marr.cohab_Maternal outcomes` + `int_marr.cohab_Women's health`]

interaction_counts <- 
interaction_counts[, .(variable = factor, 
                       int_mar.cohab_Total, `int_marr.cohab_Child outcomes`, `int_marr.cohab_Maternal outcomes`, `int_marr.cohab_Women's health`,
                       int_urban.rural_Total, `int_urban.rural_Child outcomes`, `int_urban.rural_Maternal outcomes`, `int_urban.rural_Women's health`
                       )]


assoc_summaries <- merge(assoc_summaries, interaction_counts, all.x=T)

# Read in exploratory to get order 

exploratory <- data.table(read.xlsx("Data/KE_2022_constructed_data/Exploratory_analysis_Kenya_updated.xlsx", sheet = "univariate_results_KE_decisions"))

names(exploratory) <- as.character(exploratory[2, ])

exploratory <- exploratory[3:nrow(exploratory)]

exploratory <- exploratory[,.(variable,
                              final_decision_up = `Urban + Partnered (UP)`,
                              final_decision_rp = `Rural + partnered (RP)`,
                              final_decision_unp = `Urban + Not currently partnered (UNP)`,
                              final_decision_rnp = `Rural + not currently partnered (RNP)`,
                              long_only = `Constructed from var only in long questionnaire? (1 = yes, 0 = no)`)]

exploratory <- exploratory[!is.na(variable), .(variable, exploratory_index = .I)]

assoc_summaries <- merge(assoc_summaries, exploratory, all=T)
assoc_summaries <- assoc_summaries[order(exploratory_index)]

write.csv(assoc_summaries, "Data/KE_2022_constructed_data/5_assoc_summaries/5_assoc.summary_full.csv", row.names = F, na = "")



outcomes_list[,.N,Category]

# # Collect warnings -------------------------------------------------------------
# 
# warnings.fps <- list.files(file.path("Data/KE_2022_constructed_data/5_assoc_summaries_age_unadj"), pattern = "WARNING", recursive = T, full.names = T)
# 
# warnings <- data.table(warnings.fp = warnings.fps)
# 
# warnings[, warnings.fp := gsub(warnings.fp, pattern = "Data/KE_2022_constructed_data/5_assoc_summaries_age_unadj/", replacement = "")]
# warnings[, warning := tstrsplit(warnings.fp, split = "/WARNING_", keep = 2)]
# warnings[, vulnerability := tstrsplit(warnings.fp, split = "/WARNING_", keep = 1)]
# warnings[warning %like% "not_enough_variability", WARNING_variability := 1]
# warnings[warning %like% "dropped", WARNING_dropped_factors := 1]
# 
# warnings[WARNING_variability == 1, outcome := gsub(warning, pattern = "not_enough_variability_|.csv", replacement = "")]
# warnings[WARNING_dropped_factors == 1, outcome := gsub(warning, pattern = "dropped_unused_var_in_predData_|.csv", replacement = "")]
# 
# warnings[,.N,keyby=.(vulnerability, outcome, WARNING_dropped_factors, WARNING_variability)]
# 
# write.csv(warnings[,.N,keyby=.(vulnerability, WARNING_dropped_factors, WARNING_variability)], 
#           "Data/KE_2022_constructed_data/5_assoc_summaries_age_unadj/5_warnings_review_by_vulnerability.csv")
# 
# write.csv(warnings[,.N,keyby=.(vulnerability, outcome, WARNING_dropped_factors, WARNING_variability)], 
#           "Data/KE_2022_constructed_data/5_assoc_summaries_age_unadj/5_warnings_review_by_vulnerability_outcome.csv")
