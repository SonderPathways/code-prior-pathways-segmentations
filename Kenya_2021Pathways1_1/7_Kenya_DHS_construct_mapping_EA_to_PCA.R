#########################  KENYA_DHS22_construct mapping #######################
#
# Purpose: Principal component analysis of selected vulnerabilities
#
# Authors: Anu Mishra (Ethiopia resource code), Helena Manguerra (UW START, Kenya)
#
# Input files: "0_Kenya_DHS_Data_Dictionary.xlsx"
#              
# Last edited:
#
# Objects created:
#
# Output files: "7_Kenya_Construct_mapping.xlsx"
#
# Ancestor scripts: 5_Kenya_DHS_exploratory.R
#
# Descendant script: 8_Kenya_DHS_PCA.R
# 
# Note: Need to set directory for each user
# Note: This script requires changing variables in the Data Dictionary
#_______________________________________________________________________________ 

# Clearing the workspace -------------------------------------------------------

# rm(list=ls())

# Load libraries ---------------------------------------------------------------

pacman::p_load("readxl", "zoo", "tidyverse", "data.table", "openxlsx")

# Set working directory --------------------------------------------------------

pacman::p_load("readxl", "zoo", "tidyverse", "data.table", "openxlsx")


user = "Darcy"

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
} else if (tolower(user) == "darcy") {
  rootdir <-"/Users/darcyrao/Dropbox"
} else {
  print("No filepath")
}


### Set working directory - need to change for each person 
setwd(file.path(rootdir, "START_229_Gender Equity/Working Files/Data/KE_2022_constructed_data/"))



# Load data --------------------------------------------------------------------

const <- data.table(read_xlsx("0_Kenya_DHS_Data_Dictionary.xlsx",sheet = 'Vulnerabilities_full_list_SS'))

const <- const[6:nrow(const)]
names(const) <- as.character(const[1, ])

const <- const[2:nrow(const),names(const) %in% c('category','subcategory','construct','PCA.grouping', 'variable name', "variable type", "urban_PCA_decision_primary", "rural_PCA_decision_primary"), with =F]
names(const) <- c('category','sub.category','construct','pca.grouping', 'var.name', "var.type", "urban_PCA_decision_primary", "rural_PCA_decision_primary")

const <- const[!is.na(var.name)]
const <- const[!is.na(sub.category)]
const <- const[!is.na(category)]


# Check that vulnerabilities within multiple constructs are repeated -----------

length(const$var.name)
length(unique(const$var.name))

const[, f.var.name := var.name]

# Read in exploratory only to get long/short -----------------------------------

exploratory <- data.table(read.xlsx("Exploratory_analysis_Kenya_updated.xlsx", sheet = "univariate_results_KE_decisions"))

names(exploratory) <- as.character(exploratory[2, ])

exploratory <- exploratory[3:nrow(exploratory)]

exploratory <- exploratory[,.(var.name = `variable`,
                              final_decision_up = `Urban + Partnered (UP)`,
                              final_decision_rp = `Rural + partnered (RP)`,
                              final_decision_unp = `Urban + Not currently partnered (UNP)`,
                              final_decision_rnp = `Rural + not currently partnered (RNP)`,
                              explore_lca = `Explore in LCA? (for variables dropped now)`,
                              long_only = `Constructed from var only in long questionnaire? (1 = yes, 0 = no)`)]

exploratory <- exploratory[!is.na(var.name)]
exploratory[, keep_up := ifelse(grepl("^keep", tolower(final_decision_up)), 1, 0)]
exploratory[, keep_rp := ifelse(grepl("^keep", tolower(final_decision_rp)) , 1, 0)]
exploratory[, keep_unp := ifelse(grepl("^keep", tolower(final_decision_unp)) , 1, 0)]
exploratory[, keep_rnp := ifelse(grepl("^keep", tolower(final_decision_rnp)), 1, 0)]

exploratory[, keep_urban := ifelse(grepl("^keep", tolower(final_decision_up)) | 
                                   grepl("^keep", tolower(final_decision_unp)), 1, 0)]

exploratory[, keep_rural := ifelse(grepl("^keep", tolower(final_decision_rp)) | 
                                     grepl("^keep", tolower(final_decision_rnp)), 1, 0)]

exploratory[, keep_any := ifelse(grepl("^keep", tolower(final_decision_up)) | 
                                   grepl("^keep", tolower(final_decision_rp)) | 
                                   grepl("^keep", tolower(final_decision_unp)) | 
                                   grepl("^keep", tolower(final_decision_rnp)), 1, 0)]

const <- 
  merge(const, 
        exploratory, 
        by = "var.name",
        all.x = T)

const[is.na(keep_up), `:=` (keep_up = 0, note = "not evaluated in exploratory analysis")]
const[is.na(keep_rp), `:=` (keep_rp = 0, note = "not evaluated in exploratory analysis")]
const[is.na(keep_unp), `:=` (keep_unp = 0, note = "not evaluated in exploratory analysis")]
const[is.na(keep_rnp), `:=` (keep_rnp = 0, note = "not evaluated in exploratory analysis")]
const[is.na(keep_urban), keep_urban := 0]
const[is.na(keep_rural), keep_rural := 0]
const[is.na(keep_any), keep_any := 0]


const[,.N,keyby=.(category, sub.category)]

const[keep_any==1,.N,keyby=.(category, sub.category)]


write.xlsx(const, "7_Kenya_Construct_mapping.xlsx")



# ### If pca_round == "repeat", merge in decisions from first round of PCA
# if(pca_round == "repeat"){
#   pca_decis_round1 <- data.table(read.xlsx("PCA decisions.xlsx", sheet = "round 1"))
#   pca_decis_round1 <- pca_decis_round1 %>% select(var.name, up_PCA_decision_primary, up_PCA_decision_secondary, rp_PCA_decision_primary,
#                                                   rp_PCA_decision_secondary, unp_PCA_decision_primary, unp_PCA_decision_secondary, 
#                                                   rnp_PCA_decision_primary, rnp_PCA_decision_secondary)
#   const <-   merge(const, 
#                    pca_decis_round1, 
#                    by = "var.name",
#                    all.x = T)
# }

