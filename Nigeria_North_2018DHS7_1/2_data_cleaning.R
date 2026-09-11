


################################################################################
# DATA CLEANING
################################################################################


# rm(list=ls())
###################################
# RUN SETUP
source("1_setup.R")

###################################
# READ IN RAW DHS FILES
IR1 <- data.table(read.dta13(paste0(data_path, "NGIR7BFL.DTA"), fromEncoding="utf-8"))
BR1 <- data.table(read.dta13(paste0(data_path, "NGBR7BFL.DTA"), fromEncoding="utf-8"))
KR1 <- data.table(read.dta13(paste0(data_path, "NGKR7BFL.DTA"), fromEncoding="utf-8"))
HH1 <- data.table(read.dta13(paste0(data_path, "NGHR7BFL.DTA"), fromEncoding="utf-8"))
MR1 <- data.table(read.dta13(paste0(data_path, "NGMR7AFL.DTA"), fromEncoding="utf-8"))

######################################################################
# 1 | GENERATE VULNERABILITY FACTORS
######################################################################

# RUN FOR EACH SURVEY INDEPENDENTLY AND BIND TOGETHER

vulnerability1 <- gen_vulnerability_factors(IR=IR1, BR=BR1, HH=HH1, MR = MR1) %>% mutate(survey = "NG7BFL")
vulnerability1_vars <- setdiff(names(vulnerability1), c(names(IR1), names(BR1), names(HH1), names(MR1)))
vulnerability1 <- subset(vulnerability1, select=c("caseid", "v001", "v002", "v021", data_strata_var, vulnerability1_vars))

# SAVE VULNERABILITY DATASET
dir.create(dirname(vulnerability_file), showWarnings = F, recursive = T)
saveRDS(vulnerability1, file = vulnerability_file)

vulnerability <- readRDS(vulnerability_file)


######################################################################
# 2 | GENERATE OUTCOMES VARIABLES
######################################################################


# RUN FOR EACH SURVEY INDEPENDENTLY AND BIND TOGETHER
outcomes1 <- gen_outcome_variables(IR=IR1, KR=KR1, BR=BR1) %>% mutate(survey = "NG7BFL")
outcomes1_vars <- setdiff(names(outcomes1), c(names(IR1), names(BR1), names(KR1)))
outcomes1 <- subset(outcomes1, select=c("caseid", "v001", "v002", outcomes1_vars))

# ADD IN strata VARIABLE
outcomes <- outcomes1 %>%
  right_join(vulnerability %>% dplyr::select(caseid, survey, strata, wt), by=c("caseid", "survey"))

# SAVE OUTCOME DATASET
dir.create(dirname(outcomes_file), showWarnings = F, recursive = T)
saveRDS(outcomes, file = outcomes_file)

outcomes <- readRDS(outcomes_file)

# MERGE TOGETHER AND SAVE
outcomes_vulnerability <- outcomes %>% left_join(vulnerability, by=c("caseid", "v001", "v002","wt", "survey", "strata"))
saveRDS(outcomes_vulnerability, file = outcomes_vulnerability_file)


######################################################################
# 3 | CREATE NEW PATHWAYS WORKBOOK
######################################################################
if (create_new_pathways_workbook==TRUE){

  # CREATE PARAMS SHEET
  df_params <- data.frame(strata = NA,
                          domains = NA)

  # CREATE OUTCOMES SHEET
  df_outcomes <- data.frame(category = NA,
                            outcome_variable = unique(c(outcomes1_vars)),
                            short_name = NA,
                            description = NA,
                            univariate_include = NA,
                            eda_include = NA,
                            profile_include = NA,
                            notes = NA) %>%
    arrange(outcome_variable)

  # CREATE VULNERABILITY SHEET
  df_vulnerability <- data.frame(vulnerability_variable = unique(c(vulnerability1_vars)),
                                 short_name = NA,
                                 description = NA,
                                 univariate_include = NA,
                                 eda_include = NA,
                                 pca_strata = NA,
                                 pca_include = NA,
                                 lca_strata = NA,
                                 lca_include = NA,
                                 profile_include = NA,
                                 typing_tool_strata = NA,
                                 typing_tool_include =NA,
                                 notes = NA) %>%
    arrange(vulnerability_variable)


  l <- list("params" = df_params, "outcomes" = df_outcomes, "vulnerability" = df_vulnerability)
  path = config::get("new_pathways_workbook_path")
  write.xlsx(l, file = path)
  print(paste0("New Pathways Workbook Created: ", path))

}
