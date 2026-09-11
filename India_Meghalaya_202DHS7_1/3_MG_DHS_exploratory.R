rm(list = ls())
pacman::p_load(readxl, writexl, dplyr, survey, zoo, tidyr, generics,
               colorspace, viridis, broom, jtools, openxlsx, writexl, dplyr, survey)

###################################
# GET OUTCOMES AND VULNERABILITY FACTORS
outcomes <- readRDS("dataOut/1_MG_DHS_outcomes.rds")
vul <- readRDS("dataOut/2_MG_DHS_vulnerability.rds")

###################################
# MERGE TOGETHER
outcomes_vulnerability <- merge(vul, outcomes, by="caseid")
colnames(outcomes_vulnerability)
sum(is.na(outcomes_vulnerability$v001))

#sub-sample 1: Urban & Rural
table(outcomes_vulnerability$v025)
# outcomes_vulnerability <- outcomes_vulnerability %>% filter(v025=="urban")
outcomes_vulnerability <- outcomes_vulnerability %>% filter(v025=="rural")
sum.miss <- colSums(is.na(outcomes_vulnerability))
sum.miss <- as.data.frame(sum.miss)

###################################
# DEFINE LIST OF OUTCOMES AND VULNERABILITY FACTORS FOR INITIAL BIVARIATE REGRESSOIN ANALYSIS

#outcomes and vuls for married sample
outcomes.list <- names(outcomes)[!names(outcomes) %in% c("caseid", "v001", "v002","v021" ,"v022", "v023", "v025")]
outcomes.list
vulnerability.list <- names(vul)[!names(vul) %in% c("caseid", "weight")]
vulnerability.list

###################################
# CREATE A MATRIX TO POPULATE REGRESSION INFERENCE OR READ IN IF ALREADY EXISTS

bireg_analysis <- as.data.frame(matrix(nrow = length(vulnerability.list), ncol = length(outcomes.list)))
colnames(bireg_analysis) <- outcomes.list
bireg_analysis$factor <- vulnerability.list
bireg_analysis <- bireg_analysis %>% dplyr::select(factor, everything())

###################################
# DEFINE FUNCTION
###################################

get_bivariate_regression <- function(df, measure, outcome, int_marrcohab=TRUE){

  # Create new variables in the DF for measure and outcome
  df$var <- df %>% pull(eval(parse(text=measure)))
  df$outcome <- df %>% pull(eval(parse(text=outcome)))

  df.design <- svydesign(id=~v021,strata=~v023, weights=~weight, data=df, nest=TRUE)
  # df.design <- svydesign(id=~v021,strata=~v023, data=df, nest=TRUE)

  binary.out <- all(na.omit(df$outcome) %in% 0:1)


  # GLM for main effect and urban/rural and partnership status interaction terms ------------------------
  if(binary.out == FALSE){
    svy.glm.main <- svyglm(outcome ~ var  , family=gaussian(link ="identity"), design=df.design, data=df)

  }else{
    svy.glm.main <- svyglm(outcome ~ var , family=quasibinomial, design=df.design, data=df)

  }

  # GET P-VALUES
  print(summary(svy.glm.main))

  # MAIN EFFECT
  res_svy.glm.main <- tidy(summ(svy.glm.main)) %>%
    filter(term != "(Intercept)"

           ) %>%
    mutate(p.value = round(p.value, 4)) %>%
    filter(p.value <= 0.05)
  if(nrow(res_svy.glm.main) > 0){
    main_effect = "m"
  } else {
    main_effect = ""
  }

  p_value_out <- paste0(main_effect)
  out <- list(measure, outcome, p_value_out)
  return(out)
}


###################################
# LOOP THROUGH VULNERABILITY FACTORS AND OUTCOMES, GET A BASELINE QUANTITATIVE ASSESSMENT THROUGH BIVARIATE REGRESSION
# KEY OF OUTPUT:
## M: MAIN EFFECT SIGNIFICANT AT 95% CL
for(m in vulnerability.list){

  # INITIALIZE DATAFRAME WE WILL POPULATE WITH BIVARIATE REGRESSION INFERENCE
  reg_sig <- as.data.frame(matrix(nrow = 1, ncol = length(outcomes.list)))

  try({

    for(i in 1:length(outcomes.list)){

      try({

        out <- get_bivariate_regression(df=outcomes_vulnerability, measure=m, outcome=outcomes.list[i])
        reg_sig[[i]] <- out[[3]]
        print(paste(m,outcomes.list[i]))

      }, silent = TRUE)
    }

    # POPULATE reg_inf WITH OUTPUTS
    reg_sig$factor <- m
    bireg_analysis[bireg_analysis$factor == reg_sig$factor, 2:ncol(bireg_analysis)] <- reg_sig[-ncol(reg_sig)]

  }, silent = TRUE)
}

#clean up the final matrix
final = bireg_analysis
final[final == ""] <- 0
final[final == "m"] <- 1
final <- final %>% mutate_at(outcomes.list, as.numeric)
#for each predictor, calculate % sig associations

final <- final %>%
  rowwise() %>%
  mutate(pct_sig = mean(c_across(2:25), na.rm = TRUE)) %>%
  ungroup()

table(final$pct_sig)
hist(final$pct_sig)

write_xlsx(final, "dataOut/1_biassoc_summary_rural.xlsx")
write_xlsx(final, "dataOut/1_biassoc_summary_urban.xlsx")

##merge the two Excel files
urban <- read_excel("dataOut/1_biassoc_summary_urban.xlsx")
rural <- read_excel("dataOut/1_biassoc_summary_rural.xlsx")
urban <- urban %>% dplyr::select(factor, pct_sig)
names(urban) <- c("factor", "urban_pct_sig")
rural <- rural %>% dplyr::select(factor, pct_sig)
names(rural) <- c("factor", "rural_pct_sig")

univariate <- merge(urban, rural, by="factor")
#read in data dictionary to keep the order of predictors
predictors <- read_excel("MG_India_DHS_2020_Data_Dictionary.xlsx", sheet="predictors")
predictors <- predictors %>%
  dplyr::select(order, varnames) %>%
  filter(!is.na(varnames))
names(predictors) <- c("order", "factor")
univariate <- merge(predictors, univariate, by="factor")

# add descriptions
predictors <- read_excel("MG_India_DHS_2020_Data_Dictionary.xlsx", sheet="predictors")
predictors <- predictors %>% dplyr::select("pathways domains", "survey variables", "varnames", "description")
predictors <- na.locf(predictors)

univariate <- left_join(univariate, predictors, by = c("factor" = "varnames"))

univariate$urban_univarite_keep <- array(NA, nrow(univariate))
univariate$rural_univarite_keep <- array(NA, nrow(univariate))

univariate <- univariate %>% dplyr::select("order", "pathways domains","survey variables","factor",
                                    "description",
                                    "urban_pct_sig","urban_univarite_keep",
                                    "rural_pct_sig","rural_univarite_keep")


urban <- read_excel("dataOut/1_biassoc_summary_urban.xlsx")
rural <- read_excel("dataOut/1_biassoc_summary_rural.xlsx")


urban <- urban %>%  dplyr::select(factor, fp.mod.now)
rural <- rural %>%  dplyr::select(factor, fp.mod.now)

univariate <- left_join(univariate, urban)
univariate <- univariate %>% 
  rename(fp.mod.now.u = fp.mod.now)

univariate <- left_join(univariate, rural)
univariate <- univariate %>% 
  rename(fp.mod.now.r = fp.mod.now)

write_xlsx(univariate, "dataOut/exploratory_summary.xlsx")

# look how many remain per domain after cuttoffs

df_1  <- univariate %>% filter(fp.mod.now.u == 1)
table(df_1$`pathways domains`)

df_2  <- univariate %>% filter(fp.mod.now.r == 1)
table(df_2$`pathways domains`)



