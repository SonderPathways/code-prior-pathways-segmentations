################################################################################
# COMBINE DATASETS
################################################################################


################################################################################
# RUN SETUP
################################################################################


source("1_setup.R")

################################################################################
# READ IN DATASETS
################################################################################
nreps = 100

df_u <- readRDS(paste0(lca_path, "nreps", nreps, "/urban_outcomes_vulnerability_class_ranked.rds"))
df_r <- readRDS(paste0(lca_path, "nreps", nreps, "/rural_outcomes_vulnerability_class_ranked.rds"))

df <- rbind(df_u, df_r)
df <- df %>% rename(segment_name = segment_rank)

write.csv(df, "data/Nigeria_South_2024DHS8_1.0.csv", row.names = FALSE)
