

################################################################################
# SEGMENT PROFILING PLOTS
################################################################################


###################################
# RUN SETUP
source("1_setup.R")


###################################
# SET PARAMETERS
# pal = "Dark2"

urban_seg = "LCA5_class"
rural_seg = "LCA4_class"


###################################
# GET INPUT DATA


params_sheet <- readRDS(params_excel_file)


strata_set <- params_sheet %>%
  dplyr::select(strata) %>%
  dplyr::filter(!is.na(strata)) %>%
  distinct()


shp_file <- read_sf(shp_path)

shp_file <- shp_file %>%
  mutate(NAME_1 = stringi::stri_trans_general(NAME_1, "latin-ascii"))


###################################
#


for (stratum in strata_set$strata){
  print(stratum)

  # SET SELECTED MODEL TO PROFILE
  if (stratum == "urban"){
    n_class = urban_seg
  } else if (stratum == "rural"){
    n_class = rural_seg
  }

  gen_profiles(stratum=stratum, n_class=n_class, shp_file=shp_file)

}


