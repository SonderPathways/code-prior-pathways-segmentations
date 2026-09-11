


################################################################################
# DATA CLEANING
################################################################################


###################################
# RUN SETUP
source("1_setup.R")


###################################
# PARAMETERS
create_new_pathways_workbook = config::get("create_new_pathways_workbook")


###################################
# READ PATHWAYS WORKBOOOK
###################################


if (file.exists(outcomes_excel_file)){ outcomes_sheet <- readRDS(outcomes_excel_file) }
if (file.exists(vulnerability_excel_file)){ vulnerability_sheet <- readRDS(vulnerability_excel_file) }


###################################
# READ IN RAW DHS FILES


# IR1 <- data.table(read.dta13(paste0(data_path, "SNIR8BFL.DTA"), fromEncoding="utf-8"))
# IR2 <- data.table(read.dta13(paste0(data_path, "SNIR81FL.DTA"), fromEncoding="utf-8"))
# BR1 <- data.table(read.dta13(paste0(data_path, "SNBR8BFL.DTA"), fromEncoding="utf-8"))
# BR2 <- data.table(read.dta13(paste0(data_path, "SNBR81FL.DTA"), fromEncoding="utf-8"))
# KR1 <- data.table(read.dta13(paste0(data_path, "SNKR8BFL.DTA"), fromEncoding="utf-8"))
# KR2 <- data.table(read.dta13(paste0(data_path, "SNKR81FL.DTA"), fromEncoding="utf-8"))
# HH1 <- data.table(read.dta13(paste0(data_path, "SNHR8BFL.DTA"), fromEncoding="utf-8"))
# HH2 <- data.table(read.dta13(paste0(data_path, "SNHR81FL.DTA"), fromEncoding="utf-8"))
# MR1 <- data.table(read.dta13(paste0(data_path, "SNMR8BFL.DTA"), fromEncoding="utf-8"))
# MR2 <- data.table(read.dta13(paste0(data_path, "SNMR81FL.DTA"), fromEncoding="utf-8"))

IR1 <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "SNIR8BFL.DTA"))) %>% dplyr::mutate(survey = "SN8BFL")
IR2 <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "SNIR81FL.DTA"))) %>% dplyr::mutate(survey = "SN81FL")
BR1 <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "SNBR8BFL.DTA"))) %>% dplyr::mutate(survey = "SN8BFL")
BR2 <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "SNBR81FL.DTA"))) %>% dplyr::mutate(survey = "SN81FL")
KR1 <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "SNKR8BFL.DTA"))) %>% dplyr::mutate(survey = "SN8BFL")
KR2 <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "SNKR81FL.DTA"))) %>% dplyr::mutate(survey = "SN81FL")
HH1 <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "SNHR8BFL.DTA"))) %>% dplyr::mutate(survey = "SN8BFL")
HH2 <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "SNHR81FL.DTA"))) %>% dplyr::mutate(survey = "SN81FL")
MR1 <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "SNMR8BFL.DTA"))) %>% dplyr::mutate(survey = "SN8BFL")
MR2 <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "SNMR81FL.DTA"))) %>% dplyr::mutate(survey = "SN81FL")

# IR1 <- data.table(gen_dhs_data(source = "Databricks Local", collist = "*", catalog = "idm_dhs", schema = "sen", table = "SNIR8BFL")) %>% dplyr::mutate(survey = "SN8BFL")
# IR2 <- data.table(gen_dhs_data(source = "Databricks Local", collist = "*", catalog = "idm_dhs", schema = "sen", table = "SNIR81FL")) %>% dplyr::mutate(survey = "SN81FL")
# BR1 <- data.table(gen_dhs_data(source = "Databricks Local", collist = "*", catalog = "idm_dhs", schema = "sen", table = "SNBR8BFL")) %>% dplyr::mutate(survey = "SN8BFL")
# BR2 <- data.table(gen_dhs_data(source = "Databricks Local", collist = "*", catalog = "idm_dhs", schema = "sen", table = "SNBR81FL")) %>% dplyr::mutate(survey = "SN81FL")
# KR1 <- data.table(gen_dhs_data(source = "Databricks Local", collist = "*", catalog = "idm_dhs", schema = "sen", table = "SNKR8BFL")) %>% dplyr::mutate(survey = "SN8BFL")
# KR2 <- data.table(gen_dhs_data(source = "Databricks Local", collist = "*", catalog = "idm_dhs", schema = "sen", table = "SNKR81FL")) %>% dplyr::mutate(survey = "SN81FL")
# HH1 <- data.table(gen_dhs_data(source = "Databricks Local", collist = "*", catalog = "idm_dhs", schema = "sen", table = "SNHR8BFL")) %>% dplyr::mutate(survey = "SN8BFL")
# HH2 <- data.table(gen_dhs_data(source = "Databricks Local", collist = "*", catalog = "idm_dhs", schema = "sen", table = "SNHR81FL")) %>% dplyr::mutate(survey = "SN81FL")
# MR1 <- data.table(gen_dhs_data(source = "Databricks Local", collist = "*", catalog = "idm_dhs", schema = "sen", table = "SNMR8BFL")) %>% dplyr::mutate(survey = "SN8BFL")
# MR2 <- data.table(gen_dhs_data(source = "Databricks Local", collist = "*", catalog = "idm_dhs", schema = "sen", table = "SNMR81FL")) %>% dplyr::mutate(survey = "SN81FL")


######################################################################
# 1 | GENERATE VULNERABILITY FACTORS
######################################################################


# RUN FOR EACH SURVEY INDEPENDENTLY AND BIND TOGETHER
vulnerability1 <- gen_vulnerability_factors(IR=IR1, BR=BR1, HH=HH1, MR=MR1) #%>% mutate(survey = "SN8BFL")
vulnerability1_vars <- setdiff(names(vulnerability1), c(names(IR1), names(IR2), names(BR1), names(BR2), names(HH1), names(HH2), names(MR1), names(MR2)))
vulnerability1 <- subset(vulnerability1, select=unique(c("caseid", "v001", "v002", data_id_var, data_strata_var, data_state_var, "wt", "survey", "strata", vulnerability1_vars)))

vulnerability2 <- gen_vulnerability_factors(IR=IR2, BR=BR2, HH=HH2, MR=MR2) #%>% mutate(survey = "SN81FL")
vulnerability2_vars <- setdiff(names(vulnerability2), c(names(IR1), names(IR2), names(BR1), names(BR2), names(HH1), names(HH2), names(MR1), names(MR2)))
vulnerability2 <- subset(vulnerability2, select=unique(c("caseid", "v001", "v002", data_id_var, data_strata_var, data_state_var, "wt", "survey", "strata", vulnerability1_vars)))


vulnerability <- rbind(vulnerability1, vulnerability2)


# CODE TO CONVERT BINARY VARIABLES TO YES/NO (BEST PRACTICE IS TO CODE THEM INITIALLY AS YES/NO)
for (col in names(vulnerability)){

  if (all(na.omit(vulnerability[[col]]) %in% c(0,1)) == TRUE){

    print(col)
    vulnerability[[col]] <- ifelse(vulnerability[[col]] == 1, "Yes", vulnerability[[col]])
    vulnerability[[col]] <- ifelse(vulnerability[[col]] == 0, "No", vulnerability[[col]])

  }
}


# SAVE VULNERABILITY DATASET
dir.create(dirname(vulnerability_file), showWarnings = F, recursive = T)
saveRDS(vulnerability, file = vulnerability_file)

vulnerability <- readRDS(vulnerability_file)


######################################################################
# 2 | GENERATE OUTCOMES VARIABLES
######################################################################


# RUN FOR EACH SURVEY INDEPENDENTLY AND BIND TOGETHER
outcomes1 <- gen_outcome_variables(IR=IR1, KR=KR1, BR=BR1) #%>% dplyr::mutate(survey = "SN8BFL")
outcomes1_vars <- setdiff(names(outcomes1), c(names(IR1), names(IR2), names(BR1), names(BR2), names(KR1), names(KR2)))
outcomes1 <- subset(outcomes1, select=unique(c("caseid", "v001", "v002", "survey", outcomes1_vars)))

outcomes2 <- gen_outcome_variables(IR=IR2, KR=KR2, BR=BR2) #%>% mutate(survey = "SN81FL")
outcomes2_vars <- setdiff(names(outcomes2), c(names(IR1), names(IR2), names(BR1), names(BR2), names(KR1), names(KR2)))
outcomes2 <- subset(outcomes2, select=unique(c("caseid", "v001", "v002", "survey", outcomes2_vars)))


# ADD IN strata VARIABLE TO OUTCOMES
outcomes <- rbind(outcomes1, outcomes2) %>%
  base::merge(vulnerability[,c("caseid", "wt", "survey", "strata")], by=c("caseid", "survey"))

# SAVE OUTCOME DATASET
dir.create(dirname(outcomes_file), showWarnings = F, recursive = T)
saveRDS(outcomes, file = outcomes_file)

outcomes <- readRDS(outcomes_file)

# MERGE TOGETHER AND SAVE
outcomes_vulnerability <- base::merge(outcomes, vulnerability, by=c("caseid", "v001", "v002", "wt", "survey", "strata"))
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
                            outcome_variable = unique(c(outcomes1_vars, outcomes2_vars)),
                            short_name = NA,
                            description = NA,
                            univariate_include = NA,
                            eda_include = NA,
                            profile_include = NA,
                            notes = NA) %>%
    arrange(outcome_variable)

  # CREATE VULNERABILITY SHEET
  df_vulnerability <- data.frame(vulnerability_variable = unique(c(vulnerability1_vars, vulnerability2_vars)),
                                 short_name = NA,
                                 description = NA,
                                 univariate_include = NA,
                                 eda_include = NA,
                                 pca_strata = NA,
                                 pca_include = NA,
                                 lca_strata = NA,
                                 lca_include = NA,
                                 profile_strata = NA,
                                 profile_include = NA,
                                 typing_tool_strata = NA,
                                 typing_tool_include = NA,
                                 notes = NA) %>%
    arrange(vulnerability_variable)


  l <- list("params" = df_params, "outcomes" = df_outcomes, "vulnerability" = df_vulnerability)
  path = config::get("new_pathways_workbook_path")
  write.xlsx(l, file = path)
  print(paste0("New Pathways Workbook Created:", path))

}












