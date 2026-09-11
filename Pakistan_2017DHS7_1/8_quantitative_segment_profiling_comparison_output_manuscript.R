################################################################################
# VULNERABILITY RESPONSE COMPARISON
################################################################################


###################################
# RUN SETUP
source("1_setup.R")

nreps = 50


###################################
# SET PARAMETERS
params_sheet <- readRDS(params_excel_file)

final_models <- params_sheet %>%
  dplyr::select(strata, final_model) %>%
  dplyr::filter(!is.na(strata))

strata_set <- unique(final_models$strata)


###################################
# OUTCOME VARIABLE DESCRIPTIONS
outcome_descriptions <- tibble::tribble(
  ~short_name,                   ~description,
  "ANC Visits < 4",              "Less than 4 ANC Visits",
  "No Woman Checkup",            "Woman didn't had health check after birth",
  "No Baby Checkup",             "Baby didn't had health check after birth",
  "No Breastfeed Last",          "Latest child not breastfed",
  "No Modern FP Use Now",        "Not currently using modern FP method",
  "No FP Dis 5yr",               "Last method discontinued in last 5 years",
  "No Modern FP Use",            "Never used modern FP method",
  "Diarrhea 2 Weeks Last",       "Had diarrhea in last two week (LAST CHILD)",
  "Fever / Cough 2 Weeks Last",  "Had fever/cough in last two weeks (LAST CHILD)",
  "Diff Breathing Last",         "Had difficulty breathing in last two week (LAST CHILD)",
  "No Breastfeed Y/N",           "No Breastfeed (Any Child)",
  "Fever Y/N",                   "Had fever in last two weeks (Any Child)",
  "Cough Y/N",                   "Had cough in last two weeks (Any Child)",
  "Diff Breathing Y/N",          "Had difficulty breathing in last two week (Any Child)",
  "U1 Mortality Y/N",            "Ever had Under-1 year Death",
  "U5 Mortality Y/N",            "Ever had Under-5 years Death",
  "Home Birth Y/N",              "Ever had Home Birth",
  "Still birth Y/N",             "Ever had Still Birth"
)


###################################
#

for (stratum in strata_set){
  print(stratum)
  
  
  # SET SELECTED MODEL TO PROFILE
  n_class <- final_models %>%
    dplyr::filter(strata == stratum) %>%
    pull(final_model)
  print(n_class)
  
  # GET COMBINED DATASET WITH MODELED SEGMENTS
  path = paste0(lca_path, "nreps", nreps, "/", stratum, "_outcomes_vulnerability_class_ranked.rds")
  df <- readRDS(path)
  
  df$segment <- df %>% pull(eval(parse(text=n_class)))
  df$model_cat <- n_class
  df$segment = paste0("segment_", df$segment_rank)
  
  
  # GET DOMAINS
  domain_set <- params_sheet %>%
    dplyr::select(domains) %>%
    dplyr::filter(!is.na(domains)) %>%
    distinct() %>%
    dplyr::mutate(domains = str_replace_all(domains, " ", "."))
  
  
  # GET VULNERABILITY LIST
  vulnerability_vars <- readRDS(vulnerability_excel_file)
  
  vulnerability_vars_profile <- vulnerability_vars %>%
    dplyr::filter(profile_include == 1) %>%
    dplyr::filter(profile_strata %in% c("both", "all", stratum)) %>%
    dplyr::select(vulnerability_variable, short_name, description, profile_strata, domain_set$domains) %>%
    reshape2::melt(id.vars=c("vulnerability_variable", "short_name", "description", "profile_strata"), variable.name="domain", value.name="domain_include") %>%
    dplyr::filter(domain_include == 1) %>%
    dplyr::select(vulnerability_variable, short_name, description, domain) %>%
    setNames(c("variable", "short_name", "description", "domain")) %>%
    distinct()
  
  vulnerability_names <- names(df)[(names(df) %in% c("caseid", "wt", "survey", "strata", "model_cat", "segment", vulnerability_vars_profile$variable))]
  df_vulnerability <- subset(df, select=c(vulnerability_names))
  
  
  # VULNERABILITY RESPONSE PROPORTIONS
  df_vulnerability <- df_vulnerability %>%
    reshape2::melt(id.vars=c("caseid", "wt", "survey", "strata", "model_cat", "segment"), variable.name = "variable", value.name = "value") %>%
    dplyr::mutate(value = ifelse(is.na(value), "*NA", value)) %>%
    group_by(segment, variable) %>%
    dplyr::mutate(total = sum(wt)) %>%
    group_by(segment, variable, value) %>%
    dplyr::mutate(count = sum(wt),
                  prop = round(count / total * 100, 1))
  
  
  # VULNERABILITY RESPONSE PROPORTIONS + DOMAINS
  df_vulnerability <- df_vulnerability %>%
    base::merge(vulnerability_vars_profile, by=c("variable")) %>%
    dplyr::select(strata, domain, description, value, prop, segment) %>%
    distinct() %>%
    reshape2::dcast(strata + domain + description + value ~ segment, value.var = "prop")
  
  assign(paste0("df_vulnerability_", stratum, "_manuscript"), df_vulnerability)
  
  write.xlsx(base::get(paste0("df_vulnerability_", stratum, "_manuscript")),
             file = paste0(root_path, user_path, "df_vulnerability_", stratum, ".xlsx"))
  
  
  # GET OUTCOMES LIST
  outcome_vars <- readRDS(outcomes_excel_file)
  
  outcome_vars_profile <- outcome_vars %>%
    dplyr::filter(eda_include == 1) %>%
    dplyr::select(outcome_variable, short_name, category) %>%
    setNames(c("variable", "short_name", "category")) %>%
    distinct()
  
  
  # SEGMENT-LEVEL OUTCOME PROPORTIONS
  outcome_names <- names(df)[(names(df) %in% c("caseid", "wt", "survey", "strata", "model_cat", "segment", outcome_vars_profile$variable))]
  df_outcomes <- subset(df, select = c(outcome_names))
  
  df_outcomes <- df_outcomes %>%
    reshape2::melt(id.vars = c("caseid", "wt", "survey", "strata", "model_cat", "segment"), variable.name = "variable", value.name = "value") %>%
    dplyr::filter(!is.na(value)) %>%
    group_by(segment, variable) %>%
    dplyr::mutate(total = sum(wt)) %>%
    group_by(segment, variable, value) %>%
    dplyr::mutate(count = sum(wt),
                  prop = round(count / total * 100, 1))
  
  df_outcomes <- df_outcomes %>%
    base::merge(outcome_vars_profile, by = c("variable")) %>%
    dplyr::left_join(outcome_descriptions, by = "short_name") %>%
    dplyr::select(strata, description, category, value, prop, segment) %>%
    distinct() %>%
    reshape2::dcast(strata + description + category + value ~ segment, value.var = "prop") %>%
    dplyr::filter(value == 1)
  
  assign(paste0("df_outcomes_", stratum, "_manuscript"), df_outcomes)
  
  write.xlsx(base::get(paste0("df_outcomes_", stratum, "_manuscript")),
             file = paste0(root_path, user_path, "df_outcomes_", stratum, ".xlsx"))
  
  
  # POPULATION-LEVEL OUTCOME PROPORTIONS
  df_outcomes_population <- subset(df, select = c(outcome_names))
  
  df_outcomes_population <- df_outcomes_population %>%
    reshape2::melt(id.vars = c("caseid", "wt", "survey", "strata", "model_cat"),
                   variable.name = "variable", value.name = "value") %>%
    dplyr::filter(!is.na(value)) %>%
    group_by(variable) %>%
    dplyr::mutate(total = sum(wt),
                  n = n()) %>%
    group_by(variable, value) %>%
    dplyr::mutate(count = sum(wt),
                  prop = round(count / total * 100, 1),
                  se = sqrt((prop/100) * (1 - (prop/100)) / n),
                  ci_lower = pmax(0, prop - 1.96 * se * 100),
                  ci_upper = pmin(100, prop + 1.96 * se * 100)) %>%
    base::merge(outcome_vars_profile, by = "variable") %>%
    dplyr::left_join(outcome_descriptions, by = "short_name") %>%
    dplyr::select(strata, description, category, value, prop, ci_lower, ci_upper) %>%
    distinct() %>%
    dplyr::filter(value == 1)
  
  assign(paste0("df_outcomes_population_", stratum, "_manuscript"), df_outcomes_population)
  
  # WRITE TO EXCEL - drop CI columns
  write.xlsx(df_outcomes_population %>% dplyr::select(-ci_lower, -ci_upper),
             file = paste0(root_path, user_path, "df_outcomes_population_", stratum, ".xlsx"))
  
  
  # BAR PLOT - POPULATION OUTCOME PROPORTIONS
  plot_population_outcomes <- ggplot(df_outcomes_population,
                                     aes(x = reorder(description, prop), y = prop)) +
    geom_bar(stat = "identity", fill = "#2F87C5", width = 0.7) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 0.25, color = "#E8A317", linewidth = 1) +
    geom_hline(yintercept = 50, linetype = "dashed", linewidth = 0.8, color = "black") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    labs(x = "outcome", y = "prop (%)") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.title = element_text(size = 11),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  ggsave(filename = paste0(root_path, user_path, "plot_outcomes_population_", stratum, ".png"),
         plot = plot_population_outcomes,
         width = 12, height = 6, dpi = 300)
  
  
}