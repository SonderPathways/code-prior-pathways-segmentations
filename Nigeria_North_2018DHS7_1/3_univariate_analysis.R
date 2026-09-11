


################################################################################
# UNIVARIATE ANALYSIS
################################################################################


###################################
# RUN SETUP
source("1_setup.R")


###################################
# OUTCOMES
outcomes <- readRDS(outcomes_file)

outcomes_sheet <- read_excel(pathways_workbook_path, sheet = 'outcomes') %>%
  filter(univariate_include == 1) %>%
  dplyr::select(outcome_variable, short_name) %>%
  setNames(c("indicator", "short_name")) %>%
  mutate(short_name = ifelse(is.na(short_name), indicator, short_name)) %>%
  distinct()

univariate_plots_file = paste0(univariate_plots, "univariate_plots_outcomes.pdf")
gen_univariate_plots(df = outcomes, metadata = outcomes_sheet, plot_path = univariate_plots_file)


# VULNERABILITY
vulnerability <- readRDS(vulnerability_file)

vulnerability_sheet <- read_excel(pathways_workbook_path, sheet = 'vulnerabilities') %>%
  filter(univariate_include == 1) %>%
  dplyr::select(vulnerability_variable, short_name) %>%
  setNames(c("indicator", "short_name")) %>%
  mutate(short_name = ifelse(is.na(short_name), indicator, short_name)) %>%
  distinct()

univariate_plots_file = paste0(univariate_plots, "univariate_plots_vulnerability.pdf")
gen_univariate_plots(df = vulnerability, metadata = vulnerability_sheet, plot_path = univariate_plots_file)
