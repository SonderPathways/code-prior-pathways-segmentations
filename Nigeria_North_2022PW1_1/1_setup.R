################################################################################
# ENVIRONMNET SETUP
################################################################################

rm(list=ls())
###################################
# SET CONFIG AND GET CONFIG VALUES
Sys.setenv(R_CONFIG_ACTIVE = "default")

create_new_pathways_workbook = config::get("create_new_pathways_workbook")
project_name = config::get("project_name")

data_id_var = config::get("data_id_var")
data_strata_var = config::get("data_strata_var")
data_state_var = config::get("data_state_var")


###################################
# LIBRARY INSTALL

if (!require("pacman")) install.packages("pacman")

pacman::p_load(dplyr, stringr, reshape2, data.table, survey, ggplot2, broom, jtools, readxl, openxlsx, gridExtra, factoextra, modelsummary,
               poLCA, readstata13, fastDummies, huxtable, ggsankey, openxlsx, config, ggdist, sf, scatterpie, networkD3, htmlwidgets, remotes,
               conflicted, webshot, magick, haven, zscorer)

if (!require("ggsankey")) remotes::install_github("davidsjoberg/ggsankey")
if (!require("ggradar")) remotes::install_github("ricardo-bion/ggradar")


# # SET PACKAGE PREFERENCES #################
conflicted::conflict_prefer_all('dplyr')
conflicts_prefer(data.table::dcast)


###################################
# R VERSION UPGRADE
R.version.string

# IF R VERSION < THEN UPDATE TO VERSION 4.3.2 OR LATER
# https://cran.r-project.org/src/base/R-4/


###################################
# MAIN FILE PATHS
root_path = config::get("root_path")

# user_path = "jc/" #
user_path = config::get("user_path")

dir.create(file.path(paste0(root_path, user_path)), showWarnings = F, recursive = T)

pathways_workbook_path = config::get("pathways_workbook_path")


###################################
# ADDITIONAL FILE PATHS

# DHS DATA
data_path = paste0(root_path, "data/")

# SHP FILE FOR MAPPING
shp_path = paste0(data_path, config::get("shp_file"))

# PARAMS
params_excel_file = paste0(root_path, user_path, "1_params_excel.rds")
dir.create(dirname(params_excel_file), showWarnings = F, recursive = T)

# OUTCOMES EXCEL
outcomes_excel_file = paste0(root_path, user_path, "1_outcomes_excel.rds")
dir.create(dirname(outcomes_excel_file), showWarnings = F, recursive = T)

# VULNERABILITY EXCEL
vulnerability_excel_file = paste0(root_path, user_path, "1_vulnerability_excel.rds")
dir.create(dirname(vulnerability_excel_file), showWarnings = F, recursive = T)

# OUTCOMES
outcomes_file = paste0(root_path, user_path, "2_outcomes.rds")
dir.create(dirname(outcomes_file), showWarnings = F, recursive = T)

# VULNERABILITY FACTORS
vulnerability_file = paste0(root_path, user_path, "2_vulnerability.rds")
dir.create(dirname(vulnerability_file), showWarnings = F, recursive = T)

# OUTCOMES_VULNERABILITY FILE
outcomes_vulnerability_file = paste0(root_path, user_path, "2_outcomes_vulnerability.rds")
dir.create(dirname(outcomes_vulnerability_file), showWarnings = F, recursive = T)

# EXPLORATORY ANALYSIS
exploratory_analysis_file = paste0(root_path, user_path, "exploratory_analysis.rds")
dir.create(dirname(exploratory_analysis_file), showWarnings = F, recursive = T)

# PCA
pca_file = paste0(root_path, user_path, "pca_analysis.rds")
dir.create(dirname(pca_file), showWarnings = F, recursive = T)

# LCA
lca_path = paste0(root_path, user_path, "lca/")
dir.create(dirname(lca_path), showWarnings = F, recursive = T)

# CART
cart_path = paste0(root_path, user_path, "cart/")
dir.create(dirname(cart_path), showWarnings = F, recursive = T)

# UNIVARIATE PLOTS
univariate_plots = paste0(root_path, user_path, "plots/univariate_plots/")
dir.create(dirname(univariate_plots), showWarnings = F, recursive = T)

# EXPLORATORY PLOTS
exploratory_plots = paste0(root_path, user_path, "plots/exploratory_plots/")
dir.create(dirname(exploratory_plots), showWarnings = F, recursive = T)

# PCA PLOTS
pca_plots = paste0(root_path,  user_path, "plots/pca_plots/")
dir.create(dirname(pca_plots), showWarnings = F, recursive = T)

# LCA PLOTS
lca_plots = paste0(root_path, user_path, "/plots/lca_plots/")
dir.create(dirname(lca_plots), showWarnings = F, recursive = T)

# PROFILE PLOTS
profile_plots = paste0(root_path, user_path, "/plots/profile_plots/")
dir.create(dirname(profile_plots), showWarnings = F, recursive = T)

# CART PLOTS
cart_plots = paste0(root_path, user_path, "/plots/cart_plots/")
dir.create(dirname(cart_plots), showWarnings = F, recursive = T)


###################################
# DEFINE FUNCTIONS
###################################
functions = list.files("functions")
for (f in functions){
  source(paste0("functions/",f))
}



# READ IN PATHWAYS WORKBOOK ###################################
if(create_new_pathways_workbook == FALSE){

  outcomes_sheet <- read_excel(pathways_workbook_path, sheet="outcomes")

  # PARAMS
  params_sheet <- read_excel(pathways_workbook_path, sheet="params")

  saveRDS(params_sheet, file = params_excel_file)


  # OUTCOMES
  outcomes_sheet <- read_excel(pathways_workbook_path, sheet="outcomes")
  outcomes_sheet <- outcomes_sheet %>%
    mutate(short_name = ifelse(is.na(short_name), outcome_variable, short_name))

  saveRDS(outcomes_sheet, file = outcomes_excel_file)


  # VULNERABILITY
  vulnerability_sheet <- read_excel(pathways_workbook_path, sheet="vulnerability")
  vulnerability_sheet <- vulnerability_sheet %>%
    mutate(short_name = ifelse(is.na(short_name), vulnerability_variable, short_name))

  saveRDS(vulnerability_sheet, file = vulnerability_excel_file)

}


###################################


