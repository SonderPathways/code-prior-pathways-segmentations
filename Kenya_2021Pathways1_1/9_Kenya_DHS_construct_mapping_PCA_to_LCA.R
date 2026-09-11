###########################  KENYA_DHS22_PCA_to_LCA ############################
#
# Purpose: Principal component analysis of selected vulnerabilities
#
# Authors: Anu Mishra (Ethiopia resource code), Helena Manguerra (UW START, Kenya)
#
# Input files: "0_Kenya_DHS_Data_Dictionary.xlsx"
#              "7_Kenya_Construct_mapping.xlsx"
#             
# Last edited: 
#
# Output files: "9_Kenya_DHS_construct_mapping_PCA_to_LCA.xlsx"
#
# Ancestor scripts: 7_Kenya_Construct_mapping_EA_to_PCA.R
#
# Descendant script: 9a_Kenya_DHS_DHS_LCA.R
# 
# Note: Need to set directory for each user
# Note: This script requires changing variables in the Data Dictionary
#_______________________________________________________________________________ 

# Clearing the workspace -------------------------------------------------------

rm(list=ls())

# Load libraries ---------------------------------------------------------------

pacman::p_load("readxl", "zoo", "tidyverse", "data.table", "openxlsx")

# Set working directory --------------------------------------------------------


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

# Load data from script 7 (construct mapping) ----------------------------------

const <- data.table(read.xlsx("7_Kenya_Construct_mapping.xlsx"))


# Load data from the Data Dictionary -------------------------------------------

dd <- data.table(read_xlsx("0_Kenya_DHS_Data_Dictionary.xlsx",sheet = 'Vulnerabilities_full_list_SS'))

names(dd) <- as.character(dd[6, ])

dd <- dd[7:nrow(dd)]

const <- merge(const, 
               dd[, .(category, 
                      sub.category = subcategory,  
                      construct, 
                      pca.grouping = PCA.grouping, 
                      var.name = `variable name`, 
                      urban_currentLCA,
                      rural_currentLCA)], 
               all=T,
               by = c("category", "sub.category", "construct", "pca.grouping", "var.name"))

const[str_squish(tolower(urban_currentLCA)) %like% "^keep", urban_currentLCA := "keep"]
const[str_squish(tolower(rural_currentLCA)) %like% "^keep", rural_currentLCA := "keep"]


# Check no duplicated var.names (bc in different constructs) with different keep/drops ----

if(const[,.N,.(var.name, str_squish(tolower(urban_currentLCA)))][,.N,var.name][N>1, .N] > 0 | 
   const[,.N,.(var.name, str_squish(tolower(rural_currentLCA)))][,.N,var.name][N>1, .N] > 0 ){

  
  const[,.N,.(var.name, urban_currentLCA)][,.N,var.name][N>1, ]
  const[,.N,.(var.name, rural_currentLCA)][,.N,var.name][N>1, ]
  
  message("ERROR: Check vars with more than one keep/drop decision")
    
} else{
  write.xlsx(const, "9_Kenya_DHS_construct_mapping_PCA_to_LCA.xlsx")
  
}


