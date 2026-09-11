


# EXPLORATORY DATA ANALYSIS ###############################################################################

# rm(list=ls())

# RUN SETUP ###################################

source("1_setup.R")


# READ PATHWAYS WORKBOOOK SHEETS ###################################

outcomes_vars_eda <- read_excel(pathways_workbook_path, sheet = 'outcomes') %>%
  dplyr::filter(eda_include == 1) %>%
  dplyr::select(outcome_variable)

vulnerability_vars_eda <- read_excel(pathways_workbook_path, sheet = 'vulnerabilities') %>%
  dplyr::filter(eda_include == 1) %>%
  dplyr::select(vulnerability_variable)

# GET OUTCOMES AND VULNERABILITY FACTORS ###################################

outcomes <- readRDS(outcomes_file)
vulnerability <- readRDS(vulnerability_file)
outcomes_vulnerability <- readRDS(outcomes_vulnerability_file)


# DEFINE LIST OF OUTCOMES AND VULNERABILITY FACTORS FOR INITIAL BIVARIATE REGRESSION ANALYSIS ###################################

# OUTCOMES
outcomes.list <- names(outcomes)[names(outcomes) %in% outcomes_vars_eda$outcome_variable]

# VULNERABILITY FACTORS
vulnerability.list <- names(vulnerability)[names(vulnerability) %in% vulnerability_vars_eda$vulnerability_variable]
vulnerability.list <- sort(vulnerability.list)


# GENERATE EDA PLOTS ###################################


for(m in vulnerability.list){

  try({

    output <- gen_eda(df = outcomes_vulnerability, outcomes.list = outcomes.list, measure = m, plot_path = exploratory_plots)
    output$updated <- format(Sys.time(), "%a %b %d %X")

  }, silent = TRUE)

}


