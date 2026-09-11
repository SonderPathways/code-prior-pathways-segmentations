
# A PCA biplot shows both PC scores of samples (dots) and loadings of variables 
# (vectors). The further away these vectors are from a PC origin, 
# the more influence they have on that PC. Loading plots also hint 
# at how variables correlate with one another: a small angle implies 
# positive correlation, a large one suggests negative correlation, 
# and a 90° angle indicates no correlation between two characteristics. 

# A scree plot displays how much variation each principal component captures 
# from the data. If the first two or three PCs are sufficient to describe the essence 
# of the data, the scree plot is a steep curve that bends quickly and flattens out.

rm(list=ls())
setwd("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2020 DHS")
pacman::p_load("poLCA", "tidyverse","readxl", "openxlsx", "dplyr", "gridExtra", "FactoMineR", 
               "vcd", "factoextra", "ggrepel", "corrplot", "survey", "knitr")
load("Data/IR.RData")
v025 <- IR %>% select(caseid, v025, v001)

vulnerability <- readRDS("India_constructed_data/2_UP_DHS_vulpredictors.RData")
#merge v025 with vul files 
vulnerability <- merge(vulnerability, v025, by="caseid")

table(vulnerability$v025)
# vulnerability <- vulnerability %>% filter(v025=="urban")
vulnerability <- vulnerability %>% filter(v025=="rural")

pca_vars <- read_excel("India_constructed_data/UP_DHS_LCA.xlsx", sheet="map")
unique(pca_vars$sub_domain)

#urban
# pca_vars <- pca_vars %>% filter(sub_domain == "SES" & is.na(urban_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "SRH" & is.na(urban_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "IPV" & is.na(urban_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "HH.structure" & is.na(urban_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "partner" & is.na(urban_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "agency" & is.na(urban_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "decision" & is.na(urban_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "work" & is.na(urban_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "asset" & is.na(urban_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "medical_access" & is.na(urban_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "knowledge" & is.na(urban_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "community" & is.na(urban_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "large_env" & is.na(urban_univarite_drop))

#rural
# pca_vars <- pca_vars %>% filter(sub_domain == "SES" & is.na(rural_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "SRH" & is.na(rural_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "IPV" & is.na(rural_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "HH.structure" & is.na(rural_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "partner" & is.na(rural_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "agency" & is.na(rural_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "decision" & is.na(rural_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "work" & is.na(rural_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "asset" & is.na(rural_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "medical_access" & is.na(rural_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "knowledge" & is.na(rural_univarite_drop))
# pca_vars <- pca_vars %>% filter(sub_domain == "community" & is.na(rural_univarite_drop))
pca_vars <- pca_vars %>% filter(sub_domain == "large_env" & is.na(rural_univarite_drop))

vulnerability_vars <- names(vulnerability)[(names(vulnerability) %in% pca_vars$factor)]


vulnerability$psu <- vulnerability$v001
vulnerability$weight <- vulnerability$sdweight

df <- vulnerability %>% dplyr::select(psu, weight, all_of(vulnerability_vars))
df.design <- svydesign(id=~psu, weights=~weight, data=df, nest=TRUE)
formula <- as.formula(paste0("~", paste(vulnerability_vars, collapse = "+")))
pca_res <- svyprcomp(formula, design = df.design, center = TRUE, scale. = TRUE, scores = FALSE)

# Open pdf file 
# pdf(file= "PCA_urban_married_nokid_domain1.pdf" ) 
###################################
# EIGENVALUES: 
eigen_values <- pca_res$sdev^2
plot <- pca_res$sdev %>%
  as.data.frame() %>%
  rownames_to_column("component") %>%
  bind_cols(variance = round(sqrt(eigen_values), 3),
            proportion_variance = round(eigen_values/sum(eigen_values), 3),
            cumulative_proportion = round(cumsum(eigen_values)/sum(eigen_values), 3)) %>%
  dplyr::select(component, variance, proportion_variance, cumulative_proportion) %>%
  kable(caption = "Eigen Values of Principal Component Analysis")
# assign(paste0("tbl_eigen_values_", sec), plot)
plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
grid.table(plot)

###################################
# SCREE PLOT: 70% variance explained?
plot <- fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 100))
print(plot)

###################################
# BIPLOT
#graph of variables: variable correlation plots 
pc <- c(1,2)
#The most important (or, contributing) variables can be highlighted on the correlation plot as follow:
plot <- fviz_pca_var(pca_res, axes = pc,
                     col.var = "contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     labsize=0.1, repel=T)
print(plot)

pc <- c(2,3)
#The most important (or, contributing) variables can be highlighted on the correlation plot as follow:
plot <- fviz_pca_var(pca_res, axes = pc,
                     col.var = "contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     labsize=0.1, repel=T)
print(plot)

pc <- c(1,3)
#The most important (or, contributing) variables can be highlighted on the correlation plot as follow:
plot <- fviz_pca_var(pca_res, axes = pc,
                     col.var = "contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     labsize=0.1, repel=T)
print(plot)

#Positively correlated variables are grouped together.
#Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
#The distance between variables and the origin measures the quality of the variables on the factor map. 
#Variables that are away from the origin are well represented on the factor map.

#The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates) 
#visualize the cos2 of variables on all the dimensions using the corrplot package:
var <- get_pca_var(pca_res)
plot <- corrplot(var$cos2, is.corr=FALSE)
print(plot)

#The total contribution to all PCs
plot <- fviz_contrib(pca_res, choice = "var", axes = 1:5, top = 80)
print(plot)
#The red dashed line on the graph above indicates the expected average contribution.
# dev.off()
