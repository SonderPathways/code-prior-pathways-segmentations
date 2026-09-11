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
# READ PATHWAYS WORKBOOK
###################################

if (file.exists(outcomes_excel_file)){ outcomes_sheet <- readRDS(outcomes_excel_file) }
if (file.exists(vulnerability_excel_file)){ vulnerability_sheet <- readRDS(vulnerability_excel_file) }

###################################
# READ IN PATHWAYS SURVEY DATA
NN1 <-  read_sav(paste0(data_path,"pathways_nigeria.sav"))
NN1 <- as.data.frame(NN1)
#filter the NN dataset to only include rows where the value of the ITYP column
# is equal to 1, i.e., the women who had U5 children.Pathways survey collects
# data from the mother-in-law/matriarch/female influencer (ITYP == 2),
# husband/partner (ITYP == 3), and male HoH (ITYP == 4).
NN1 <- subset(NN1, NN1$ITYP==1)

NN1 <- NN1 %>%
  mutate(across(where(is.character), ~ if_else(. == "", NA_character_, trimws(.))))
NN1$caseid <- paste(NN1$PSUclean,NN1$HHIDclean,NN1$ITYP, NN1$SbjNum)
NN1$caseid <- gsub(" ", "", NN1$caseid, fixed=TRUE)

NN1 <- NN1 %>%
  mutate(URBAN_RURA = recode(URBAN_RURA, "R" = "rural", "U" = "urban"))

######################################################################
# 1 | GENERATE VULNERABILITY FACTORS
######################################################################

# RUN FOR EACH SURVEY INDEPENDENTLY AND BIND TOGETHER
vulnerability <- gen_vulnerability_factors(NN=NN1)
vulnerability_vars <- setdiff(names(vulnerability), names(NN1))
vulnerability <- subset(vulnerability, select=c("caseid", "muslim", "freq.tv", data_id_var, data_strata_var, data_state_var, vulnerability_vars))

# CODE TO CONVERT BINARY VARIABLES TO YES/NO (BEST PRACTICE IS TO CODE THEM INITIALLY AS YES/NO)
for (col in names(vulnerability)){

  if (all(vulnerability[[col]] %in% c(0,1)) == TRUE){

    print(col)
    vulnerability[[col]] <- ifelse(vulnerability[[col]] == 1, "yes", vulnerability[[col]])
    vulnerability[[col]] <- ifelse(vulnerability[[col]] == 0, "no", vulnerability[[col]])

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
outcomes <- gen_outcome_variables(NN=NN1)
outcome_vars <- setdiff(names(outcomes), names(NN1))
outcomes <- subset(outcomes, select=c("caseid", "strata",outcome_vars))


# SAVE OUTCOME DATASET
dir.create(dirname(outcomes_file), showWarnings = F, recursive = T)
saveRDS(outcomes, file = outcomes_file)

outcomes <- readRDS(outcomes_file)

# MERGE TOGETHER AND SAVE
outcomes_vulnerability <- base::merge(outcomes, vulnerability, by= c("caseid","strata", "wt"))
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
                            outcome_variable = unique(outcome_vars),
                            short_name = NA,
                            description = NA,
                            univariate_include = NA,
                            eda_include = NA,
                            profile_include = NA,
                            notes = NA) %>%
    arrange(outcome_variable)

  # CREATE VULNERABILITY SHEET
  df_vulnerability <- data.frame(vulnerability_variable = unique(vulnerability_vars),
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




