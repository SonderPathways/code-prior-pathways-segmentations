
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
# READ IN PATHWAYS DATA

survey <- read_sav(paste0("data/","pathways_nigeria.sav"))
# survey <- read_sav(paste0(data_path,"pathways_nigeria.sav"))
survey <- as.data.frame(survey)
survey <- subset(survey, survey$ITYP==1)
survey <- survey %>%
  mutate(across(where(is.character), ~ if_else(. == "", NA_character_, trimws(.))))
survey$caseid <- paste(survey$PSUclean,survey$HHIDclean,survey$ITYP, survey$SbjNum)
survey$caseid <- gsub(" ", "", survey$caseid, fixed=TRUE)

survey <- survey %>%
  mutate(URBAN_RURA = recode(URBAN_RURA, "R" = "rural", "U" = "urban")) %>%
  dplyr::mutate(survey = "NNPW")

######################################################################
# 1 | GENERATE VULNERABILITY FACTORS
######################################################################


# RUN FOR EACH SURVEY INDEPENDENTLY AND BIND TOGETHER
vulnerability <- gen_vulnerability_factors_pathways_sonder(NN=survey)
vulnerability_vars <- setdiff(names(vulnerability), c(names(survey)))
vulnerability <- subset(vulnerability, select=unique(c("caseid", "survey", all_of(svy_id_var), all_of(svy_strata_var), all_of(data_state_var), vulnerability_vars)))



# CODE TO CONVERT BINARY VARIABLES TO YES/NO (BEST PRACTICE IS TO CODE THEM INITIALLY AS YES/NO)
for (col in names(vulnerability)){

  if ((all(c(0, 1) %in% na.omit(vulnerability[[col]])) == TRUE) & (all(na.omit(vulnerability[[col]]) %in% c(0, 1)) == TRUE)){

    print(col)
    vulnerability[[col]] <- ifelse(vulnerability[[col]] == 1, "Yes", vulnerability[[col]])
    vulnerability[[col]] <- ifelse(vulnerability[[col]] == 0, "No", vulnerability[[col]])

  }
}


# CHECK IF ANY COLUMNS WERE CODED AS INTERGERS AND CAHNGE THEM TO CHARACTERS


# Function to convert numeric and integer columns to character, except "wt"
convert_to_character_except_wt <- function(data) {

  cols_to_convert <- sapply(data, function(col) is.numeric(col) || is.integer(col)) &
    names(data) != "wt"

  data[cols_to_convert] <- lapply(data[cols_to_convert], as.character)

  return(data)
}

# Apply the function vulnerability
vulnerability <- convert_to_character_except_wt(vulnerability)


# SAVE VULNERABILITY DATASET
dir.create(dirname(vulnerability_file), showWarnings = F, recursive = T)
saveRDS(vulnerability, file = vulnerability_file)

vulnerability <- readRDS(vulnerability_file)


######################################################################
# 2 | GENERATE OUTCOMES VARIABLES
######################################################################


# RUN FOR EACH SURVEY INDEPENDENTLY AND BIND TOGETHER
outcomes <- gen_outcome_variables_pathways_sonder(NN=survey)
outcomes_vars <- setdiff(names(outcomes), c(names(survey)))
outcomes <- subset(outcomes, select=unique(c("caseid", "survey", outcomes_vars)))

# outcomes <- gen_outcome_variables_dhs(IR=IR, KR=KR, BR=BR, DHS=7)
# outcomes_vars <- setdiff(names(outcomes), c(names(IR), names(BR), names(KR)))
# outcomes <- subset(outcomes, select=unique(c("caseid", "v001", "v002", "survey", outcomes_vars)))


# ADD IN strata VARIABLE TO OUTCOMES
# outcomes <- rbind(outcomes1, outcomes2) %>%
outcomes <- outcomes %>%
  base::merge(vulnerability[,c("caseid", "wt", "survey")], by=c("caseid", "survey"))

# SAVE OUTCOME DATASET
dir.create(dirname(outcomes_file), showWarnings = F, recursive = T)
saveRDS(outcomes, file = outcomes_file)

outcomes <- readRDS(outcomes_file)

# MERGE TOGETHER AND SAVE
outcomes_vulnerability <- base::merge(outcomes, vulnerability, by=c("caseid", "survey", "wt"))
saveRDS(outcomes_vulnerability, file = outcomes_vulnerability_file)


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
                                      "Healthcare and mental models",
                                      "Natural and human systems",
                                      "Household relationships",
                                      "Household economics",
                                      "Community Support"),
                          strata = c("urban",
                                     "rural",
                                     NA, NA, NA, NA),
                          final_model = NA)

  # CREATE OUTCOMES SHEET
  df_outcomes <- data.frame(
    outcome_variable = unique(c(outcomes_vars_excel)),
    univariate_include = NA,
    eda_include = NA,
    profile_include = NA,
    notes = NA
  ) %>%
    base::merge(dd_outcomes, by=c("outcome_variable"), all.x=TRUE) %>%
    dplyr::select(category, outcome_variable, short_name, description, univariate_include, eda_include, profile_include, notes) %>%
    arrange(category, outcome_variable)

  # CREATE VULNERABILITY SHEET
  df_vulnerability <- data.frame(
    vulnerability_variable = unique(c(vulnerability_vars_excel)),
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
    notes = NA,
    `Woman and her past experiences` = NA,
    `Healthcare and mental models` = NA,
    `Natural and human systems` = NA,
    `Household relationships` = NA,
    `Household economics` = NA,
    `Community Support` = NA) %>%
    base::merge(dd_vulnerabilities, by=c("vulnerability_variable"), all.x=TRUE) %>%
    dplyr::select(vulnerability_variable, short_name, description, univariate_include, eda_include, pca_strata, pca_include, lca_strata, lca_include, profile_strata, profile_include, typing_tool_strata, typing_tool_include, notes,
                  Woman.and.her.past.experiences, Healthcare.and.mental.models, Natural.and.human.systems, Household.relationships, Household.economics, Community.Support) %>%
    arrange(vulnerability_variable)


  l <- list("params" = df_params, "outcomes" = df_outcomes, "vulnerabilities" = df_vulnerability)
  path = config::get("new_pathways_workbook_path")
  write.xlsx(l, file = path)
  print(paste0("New Pathways Workbook Created: ", path))

}












