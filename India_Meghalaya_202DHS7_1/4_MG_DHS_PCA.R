# Install and load necessary packages
rm(list=ls())
pacman::p_load("reshape2", "readxl","tidyr", "dplyr","corrplot", "writexl", "survey", "stringr")

################################ RURAL #########################################

# read in vulnerability data, IR data and data on LCA variables 
vulnerability <- readRDS("dataOut/2_MG_DHS_vulnerability.rds")
IR <- readRDS("data/IR_meghalaya.rds")
v025 <- IR %>% dplyr::select(caseid, v021,v023, v025)
vulnerability <- merge(vulnerability, v025, by="caseid")
vulnerability <- vulnerability %>% filter(v025=="rural")
lca_spreadsheet <- read_excel("dataOut/exploratory_summary.xlsx", sheet="Sheet1")

# create data frame for PCA and Correlations
lca_vars <- lca_spreadsheet %>% filter(rural_univarite_keep == 1)
lca_vars <- names(vulnerability)[(names(vulnerability) %in% lca_vars$factor)]
all_columns <- c("v021", "v023", "weight", lca_vars)
df <- vulnerability %>% dplyr::select(all_of(all_columns))

# Identify columns with zero variance for character columns
df <- df %>% mutate(across(all_of(lca_vars), as.character))

# PCA
df.design <- svydesign(id= ~v021, strata= ~v023, weights= ~weight, data=df, nest=TRUE)

# remove unneeded columns
columns_to_remove <- c("v021", "v023", "weight")
df <- df %>% dplyr::select(-all_of(columns_to_remove))

# Define your formula for PCA (adjust as needed)
pca_formula <- as.formula(paste("~", paste(names(df), collapse = " + ")))
pca_res <- svyprcomp(pca_formula, design = df.design, center = TRUE, scale. = TRUE, scores = FALSE)

# how many PC's to use
explained_variance <- (pca_res$sdev)^2
proportion_variance_explained <- explained_variance / sum(explained_variance)
cumulative_variance_explained <- cumsum(proportion_variance_explained)
threshold <- .60
idx <- which(cumulative_variance_explained > threshold)[1]

# Extract the loadings from PCA
loadings <- pca_res$rotation
loadings <- as.data.frame(loadings[,1:idx])

# Compute the magnitude of each variables loadings (sum of squared loadings) 
# length of the arrow in biplot
loadings$loadings_mag <- apply(loadings^2, 1, sum)
loadings$one_hot_encoded_names <- rownames(loadings)

loadings <- loadings %>%
  mutate(variable = sapply(one_hot_encoded_names, function(name) {
    matches <- lca_vars[str_detect(name, lca_vars)]
    if (length(matches) > 0) {
      return(matches[1])
    } else {
      return(NA)
    }
  }))

loadings <- loadings %>% 
  group_by(variable) %>% 
  summarise(var_importance = sum(loadings_mag, na.rm = TRUE)) %>% 
  ungroup()

loadings <- data.frame(loadings)

## setting nominal to be coded as 1,2,... so we can calc correlation matrix
df <- df %>%
  mutate(across(all_of(cat.var), ~ as.numeric(as.factor(.))))

# Calculate the correlation matrix from the covariance matrix
cor_matrix <- cor(df, use = "complete.obs")
corrplot(cor_matrix)

# Function to identify and remove variables that have strong correlations but lowest
# loadings values
remove_high_correlation <- function(cor_matrix, loadings, threshold = 0.7) {
  while(TRUE) {
    # Find all variable pairs with correlation above the threshold
    high_corr_pairs <- which(abs(cor_matrix) > threshold, arr.ind = TRUE)
    high_corr_pairs <- high_corr_pairs[high_corr_pairs[, 1] != high_corr_pairs[, 2], ]
    
    # Check if there are any correlations above the threshold
    if (nrow(high_corr_pairs) == 0) break
    
    # Identify the unique variables that are part of these high correlations
    high_corr_vars <- unique(c(rownames(high_corr_pairs), colnames(high_corr_pairs)))
    
    # Filter the loadings data frame to include only the high correlation variables
    filtered_loadings <- loadings[loadings$variable %in% high_corr_vars, ]
    
    # Find the variable with the lowest var_importance
    var_to_remove <- filtered_loadings$variable[which.min(filtered_loadings$var_importance)]
    
    # Remove the chosen variable from the correlation matrix
    var_index <- which(colnames(cor_matrix) == var_to_remove)
    if(length(var_index) == 0) break  # In case the variable is not found in the correlation matrix
    cor_matrix <- cor_matrix[, -var_index]
    cor_matrix <- cor_matrix[-var_index, ]
    
    # Remove the variable from loadings to prevent re-selection
    #loadings <- loadings[loadings$variable != var_to_remove, ]
  }
  return(colnames(cor_matrix))
}

# Apply the function to remove variables with high correlations
remaining_vars_rural <- remove_high_correlation(cor_matrix, loadings)

# Subset the original data frame to keep only the remaining variables
df_filtered <- df[, remaining_vars_rural]
cor_matrix <- cor(df_filtered,use = "complete.obs")
corrplot(cor_matrix)


################################ URBAN #########################################

# read in vulnerability data, IR data and data on LCA variables 
vulnerability <- readRDS("dataOut/2_MG_DHS_vulnerability.rds")
IR <- readRDS("data/IR_meghalaya.rds")
v025 <- IR %>% dplyr::select(caseid, v021,v023, v025)
vulnerability <- merge(vulnerability, v025, by="caseid")
vulnerability <- vulnerability %>% filter(v025=="urban")
lca_spreadsheet <- read_excel("dataOut/exploratory_summary.xlsx", sheet="Sheet1")

# create data frame for PCA and Correlations
lca_vars <- lca_spreadsheet %>% filter(rural_univarite_keep == 1)
lca_vars <- names(vulnerability)[(names(vulnerability) %in% lca_vars$factor)]
all_columns <- c("v021", "v023", "weight", lca_vars)
df <- vulnerability %>% dplyr::select(all_of(all_columns))

# Identify columns with zero variance for character columns
df <- df %>% mutate(across(all_of(lca_vars), as.character))

# PCA
df.design <- svydesign(id= ~v021, strata= ~v023, weights= ~weight, data=df, nest=TRUE)

# remove unneeded columns
columns_to_remove <- c("v021", "v023", "weight")
df <- df %>% dplyr::select(-all_of(columns_to_remove))

# Define your formula for PCA (adjust as needed)
pca_formula <- as.formula(paste("~", paste(names(df), collapse = " + ")))
pca_res <- svyprcomp(pca_formula, design = df.design, center = TRUE, scale. = TRUE, scores = FALSE)

# how many PC's to use
explained_variance <- (pca_res$sdev)^2
proportion_variance_explained <- explained_variance / sum(explained_variance)
cumulative_variance_explained <- cumsum(proportion_variance_explained)
threshold <- .60
idx <- which(cumulative_variance_explained > threshold)[1]

# Extract the loadings from PCA
loadings <- pca_res$rotation
loadings <- as.data.frame(loadings[,1:idx])

# Compute the magnitude of each variables loadings (sum of squared loadings) 
# length of the arrow in biplot
loadings$loadings_mag <- apply(loadings^2, 1, sum)
loadings$one_hot_encoded_names <- rownames(loadings)

loadings <- loadings %>%
  mutate(variable = sapply(one_hot_encoded_names, function(name) {
    matches <- lca_vars[str_detect(name, lca_vars)]
    if (length(matches) > 0) {
      return(matches[1])
    } else {
      return(NA)
    }
  }))

loadings <- loadings %>% 
  group_by(variable) %>% 
  summarise(var_importance = sum(loadings_mag, na.rm = TRUE)) %>% 
  ungroup()

loadings <- data.frame(loadings)

## setting nominal to be coded as 1,2,... so we can calc correlation matrix
df <- df %>%
  mutate(across(all_of(cat.var), ~ as.numeric(as.factor(.))))

# Calculate the correlation matrix from the covariance matrix
cor_matrix <- cor(df, use = "complete.obs")
corrplot(cor_matrix)

# Function to identify and remove variables that have strong correlations but lowest
# loadings values
remove_high_correlation <- function(cor_matrix, loadings, threshold = 0.7) {
  while(TRUE) {
    # Find all variable pairs with correlation above the threshold
    high_corr_pairs <- which(abs(cor_matrix) > threshold, arr.ind = TRUE)
    high_corr_pairs <- high_corr_pairs[high_corr_pairs[, 1] != high_corr_pairs[, 2], ]
    
    # Check if there are any correlations above the threshold
    if (nrow(high_corr_pairs) == 0) break
    
    # Identify the unique variables that are part of these high correlations
    high_corr_vars <- unique(c(rownames(high_corr_pairs), colnames(high_corr_pairs)))
    
    # Filter the loadings data frame to include only the high correlation variables
    filtered_loadings <- loadings[loadings$variable %in% high_corr_vars, ]
    
    # Find the variable with the lowest var_importance
    var_to_remove <- filtered_loadings$variable[which.min(filtered_loadings$var_importance)]
    
    # Remove the chosen variable from the correlation matrix
    var_index <- which(colnames(cor_matrix) == var_to_remove)
    if(length(var_index) == 0) break  # In case the variable is not found in the correlation matrix
    cor_matrix <- cor_matrix[, -var_index]
    cor_matrix <- cor_matrix[-var_index, ]
    
    # Remove the variable from loadings to prevent re-selection
    #loadings <- loadings[loadings$variable != var_to_remove, ]
  }
  return(colnames(cor_matrix))
}

# Apply the function to remove variables with high correlations
remaining_vars_urban <- remove_high_correlation(cor_matrix, loadings)

# Subset the original data frame to keep only the remaining variables
df_filtered <- df[, remaining_vars_rural]
cor_matrix <- cor(df_filtered,use = "complete.obs")
corrplot(cor_matrix)


######################### Fill in Spreadsheet #################################

lca_spreadsheet_filled <- lca_spreadsheet %>% 
  mutate(urban_lca_keep = ifelse(factor %in%  remaining_vars_urban, 1, 0)) %>% 
  mutate(rural_lca_keep = ifelse(factor %in%  remaining_vars_rural, 1, 0)) 


write_xlsx(lca_spreadsheet_filled, "dataOut/exploratory_summary.xlsx")

