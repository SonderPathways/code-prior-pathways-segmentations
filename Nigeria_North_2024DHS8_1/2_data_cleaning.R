


################################################################################
# DATA CLEANING
################################################################################


###################################
# RUN SETUP
source("1_setup.R")

###################################
# PARAMETERS
create_new_pathways_workbook = config::get("create_new_pathways_workbook")


# ###################################
# # READ IN SURVEY | DHS
# # RUN 1_import_data.R TO IMPORT AND SAVE RAW SURVEY DATA (REPLACE THE FILE HERE IF NOT DHS)
#
if (length(list.files(path = data_path, pattern = "\\.rds$", full.names = TRUE)) == 0){

  print("Running 1_import_data.R to load survey data and save as .rds objects for quicker load.")
  source("1_import_data.R")

}

# COMMENT THIS CODE ACCORDINGLY IF USING DHS OR A DIFFERENT SURVEY
if (file.exists(paste0(data_path, "IR.rds"))) {
  IR <- readRDS(file = paste0(data_path, "IR.rds"))
  message("IR file imported.")}

if (file.exists(paste0(data_path, "BR.rds"))) {
  BR <- readRDS(file = paste0(data_path, "BR.rds"))
  message("BR file imported.")}

if (file.exists(paste0(data_path, "KR.rds"))) {
  KR <- readRDS(file = paste0(data_path, "KR.rds"))
  message("KR file imported.")}

if (file.exists(paste0(data_path, "HH.rds"))) {
  HH <- readRDS(file = paste0(data_path, "HH.rds"))
  message("HH file imported.")}


IR <- readRDS(file = paste0(data_path, "IR.rds"))
# IR <- IR %>% dplyr::filter(v024 %in% c("south west", "south south" , "south east"))
IR <- IR %>% dplyr::filter(v024 %in% c("north west", "north east", "north central"))
# dim(IR)
BR <- readRDS(file = paste0(data_path, "BR.rds"))
# BR <- BR %>% dplyr::filter(v024 %in% c("south west", "south south" , "south east"))
BR <- BR %>% dplyr::filter(v024 %in% c("north west", "north east", "north central"))

KR <- readRDS(file = paste0(data_path, "KR.rds"))
# KR <- KR %>% dplyr::filter(v024 %in% c("south west", "south south" , "south east"))
KR <- KR %>% dplyr::filter(v024 %in% c("north west", "north east", "north central"))

HH <- readRDS(file = paste0(data_path, "HH.rds"))
# HH <- HH %>% dplyr::filter(hv024 %in% c("south west", "south south" , "south east"))
HH <- HH %>% dplyr::filter(hv024 %in% c("north west", "north east", "north central"))

PR <- readRDS(file = paste0(data_path, "PR.rds"))
# PR <- PR %>% dplyr::filter(hv024 %in% c("south west", "south south" , "south east"))
PR <- PR %>% dplyr::filter(hv024 %in% c("north west", "north east", "north central"))


######################################################################
# 1 | GENERATE VULNERABILITY FACTORS
######################################################################

# GENERATE VULNERABILITY FACTORS USING STARTER DHS SCRIPT
vulnerability <- gen_vulnerability_factors_dhs(IR=IR, HH=HH, BR=BR, KR=KR, PR=PR, dhs=8)

vulnerability_vars <- setdiff(
  names(vulnerability),
  unique(c(
    if (exists("IR")==TRUE) names(IR) else character(),
    if (exists("BR")==TRUE) names(BR) else character(),
    if (exists("HH")==TRUE) names(HH) else character(),
    if (exists("MR")==TRUE) names(MR) else character(),
    if (exists("PR")==TRUE) names(PR) else character())))

#take out "survey",
vulnerability <- subset(vulnerability, select=unique(c("caseid", all_of(svy_id_var), all_of(svy_strata_var), all_of(data_state_var), vulnerability_vars)))

# CODE TO CONVERT BINARY VARIABLES TO YES/NO (BEST PRACTICE IS TO CODE THEM INITIALLY AS YES/NO)
for (col in names(vulnerability)){

  if ((all(c(0, 1) %in% na.omit(vulnerability[[col]])) == TRUE) & (all(na.omit(vulnerability[[col]]) %in% c(0, 1)) == TRUE) |
      (all(c("Yes", "No") %in% str_to_title(na.omit(vulnerability[[col]]))) == TRUE) & (all(str_to_title(na.omit(vulnerability[[col]])) %in% c("Yes", "No")) == TRUE)){

    print(col)
    vulnerability[[col]] <- ifelse(vulnerability[[col]] == 1, "Yes", vulnerability[[col]])
    vulnerability[[col]] <- ifelse(vulnerability[[col]] == 0, "No", vulnerability[[col]])
    vulnerability[[col]] <- ifelse(vulnerability[[col]] == "yes", "Yes", vulnerability[[col]])
    vulnerability[[col]] <- ifelse(vulnerability[[col]] == "no", "No", vulnerability[[col]])
  }
}


# SAVE VULNERABILITY DATASET
dir.create(dirname(vulnerability_file), showWarnings = F, recursive = T)
saveRDS(vulnerability, file = paste0(vulnerability_file, ".rds"))
write.csv(vulnerability, file = paste0(vulnerability_file, ".csv"), row.names = FALSE)

vulnerability <- readRDS(file = paste0(vulnerability_file, ".rds"))

######################################################################
# 2 | GENERATE OUTCOMES VARIABLES
######################################################################


# GENERATE HEALTH OUTCOMES USING STARTER DHS SCRIPT

outcomes <- gen_outcome_variables_dhs(IR=IR, KR=KR, BR=BR, dhs=8)

outcomes_vars <- setdiff(
  names(outcomes),
  unique(c(
    if (exists("IR")) names(IR) else character(),
    if (exists("BR")) names(BR) else character(),
    if (exists("KR")) names(KR) else character()
  ))
)

outcomes <- subset(outcomes, select=unique(c("caseid", "v001", "v002", "v003", outcomes_vars)))


# ADD IN strata VARIABLE TO OUTCOMES
outcomes <- outcomes %>%
  base::merge(vulnerability[,c("caseid", "wt")], by=c("caseid"))

# SAVE OUTCOME DATASET
dir.create(dirname(outcomes_file), showWarnings = F, recursive = T)
saveRDS(outcomes, file = paste0(outcomes_file, ".rds"))
write.csv(outcomes, file = paste0(outcomes_file, ".csv"), row.names = FALSE)

outcomes <- readRDS(file = paste0(outcomes_file, ".rds"))

# MERGE TOGETHER AND SAVE
outcomes_vulnerability <- base::merge(outcomes, vulnerability, by=c("caseid", "wt"))
saveRDS(outcomes_vulnerability, file = paste0(outcomes_vulnerability_file, ".rds"))
write.csv(outcomes_vulnerability, file = paste0(outcomes_vulnerability_file, ".csv"), row.names = FALSE)

######################################################################
# 3 | CREATE NEW PATHWAYS WORKBOOK
######################################################################


if (create_new_pathways_workbook==TRUE){


  # GET DATA DICTIONARIES
  dd_outcomes <- readRDS(dd_outcomes_excel_file)
  dd_vulnerabilities <- readRDS(dd_vulnerabilities_excel_file)

  # FILTER VARIABLE LIST
  vulnerability_vars_excel <- vulnerability_vars[!vulnerability_vars %in% c("survey", "strata", "wt", all_of(svy_id_var), all_of(svy_strata_var))]
  outcomes_vars_excel <- outcomes_vars[!outcomes_vars %in% c("survey", "strata", "LB", "STL", "str")]

  # CREATE PARAMS SHEET
  df_params <- data.frame(domains = c("Woman and her past experiences",
                                      "Health and mental models",
                                      "Natural and human systems",
                                      "Household relationships",
                                      "Household economics and living conditions",
                                      "Social support"),
                          strata = c("urban",
                                     "rural",
                                     NA, NA, NA, NA),
                          final_model = NA)

  # CREATE OUTCOMES SHEET
  df_outcomes <- data.frame(
    outcome_variable = unique(c(outcomes_vars_excel)),
    univariate_include = 1,
    eda_include = NA,
    ranking_include = NA,
    profile_include = NA,
    notes = NA
  ) %>%
    base::merge(dd_outcomes, by=c("outcome_variable"), all.x=TRUE) %>%
    dplyr::mutate(ranking_include = case_when(outcome_variable %in% c("anc.less4.last", "home.birth.last", "nofp.mod.ever", "u5mort.yn", "waste.cat2.yn") ~ 1)) %>%
    dplyr::select(category, outcome_variable, short_name, description, univariate_include, eda_include, ranking_include, profile_include, notes) %>%
    arrange(category, outcome_variable)

  # CREATE VULNERABILITY SHEET
  df_vulnerability <- data.frame(
    vulnerability_variable = unique(c(vulnerability_vars_excel)),
    univariate_include = 1,
    eda_include = NA,
    pca_strata = NA,
    pca_include = NA,
    lca_strata = NA,
    lca_include = NA,
    profile_strata = NA,
    profile_include = NA,
    typing_tool_strata = NA,
    typing_tool_include = NA,
    notes = NA
  ) %>%
    base::merge(dd_vulnerabilities, by=c("vulnerability_variable"), all.x=TRUE) %>%
    dplyr::select(vulnerability_variable, short_name, description, univariate_include, eda_include, pca_strata, pca_include, lca_strata, lca_include, profile_strata, profile_include, typing_tool_strata, typing_tool_include, notes,
                  Woman.and.her.past.experiences, Health.and.mental.models, Natural.and.human.systems, Household.relationships, Household.economics.and.living.conditions, Social.support) %>%
    arrange(vulnerability_variable)


  if (pathways_workbook_is_excel == TRUE){


    l <- list("params" = df_params, "outcomes" = df_outcomes, "vulnerabilities" = df_vulnerability)
    path = paste0(new_pathways_workbook_path, ".xlsx")
    write.xlsx(l, file = path)

    print(paste0("New Pathways Workbook Excel created: ", new_pathways_workbook_path))


  } else if (pathways_workbook_is_excel == FALSE){


    # PARAMS AS CSV
    write.csv(df_params, file = paste0(new_pathways_workbook_path, " - params.csv"), row.names = FALSE)

    # OUTCOMES AS CSV
    write.csv(df_outcomes, file = paste0(new_pathways_workbook_path, " - outcomes.csv"), row.names = FALSE)

    # VULNERABILITIES AS CSV
    write.csv(df_vulnerability, file = paste0(new_pathways_workbook_path, " - vulnerabilities.csv"), row.names = FALSE)

    print(paste0("New Pathways Workbook CSVs created: ", new_pathways_workbook_path))


  }

}


###################################
print("2_data_cleaning.R script complete! Proceed to run 3_univariate_analysis.R script. If you've created a new Pathways Workbook to be used going forward, be sure to change the create_new_pathways_workbook parameter in the config.yml file to FALSE and rename the Pathways Workbook to that defined in the pathways_workbook_path parameter. Refer to the README for instructions if needed.")


#
# data <- readRDS("output/2_outcomes_vulnerability.rds")
# dim(data)


# # Read expert panel data (Sheet1)
# expert_panel <- read_excel("output/expert_panel_meeting_1.xlsx", sheet = "Sheet1")
#
# # Read pathways vulnerabilities
# pathways_vuln <- read_excel("output/pathways workbook - North_Nigeria_2024DHS8_1.0.xlsx", sheet = "vulnerabilities")
#
# merged_data <- pathways_vuln %>%
#   left_join(expert_panel,
#             by = "vulnerability_variable",
#             suffix = c("_pathways", "_expert"))
# install.packages("writexl")
# write.xlsx(merged_data, "output/merged_expert_panel_meeting_1.xlsx")
