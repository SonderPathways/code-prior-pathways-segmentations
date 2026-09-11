rm(list = ls())
pacman::p_load(colorspace, viridis, broom, jtools, openxlsx, writexl, dplyr, survey)
setwd("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2020 DHS")

###################################
# GET OUTCOMES AND VULNERABILITY FACTORS
load("India_constructed_data/1_UP_DHS_outcomes.RData")
vul <- readRDS("India_constructed_data/2_UP_DHS_vulpredictors.RData")

###################################
# MERGE TOGETHER
outcomes_vulnerability <- merge(vul, outcomes, by="caseid")
colnames(outcomes_vulnerability)
sum(is.na(outcomes_vulnerability$v001))

#sub-sample 1: Urban & NIU 
table(outcomes_vulnerability$v025)
# outcomes_vulnerability <- outcomes_vulnerability %>% filter(v025=="urban")
outcomes_vulnerability <- outcomes_vulnerability %>% filter(v025=="rural")
sum.miss <- colSums(is.na(outcomes_vulnerability))
sum.miss <- as.data.frame(sum.miss)

###################################
# DEFINE LIST OF OUTCOMES AND VULNERABILITY FACTORS FOR INITIAL BIVARIATE REGRESSOIN ANALYSIS

#outcomes and vuls for married sample 
outcomes.list <- names(outcomes)[!names(outcomes) %in% c("caseid", "v001", "v002", "v025")]
outcomes.list
vulnerability.list <- names(vul)[!names(vul) %in% c("v001", "v002", "caseid", "v025", "sdweight")]
vulnerability.list

###################################
# CREATE A MATRIX TO POPULATE REGRESSION INFERENCE OR READ IN IF ALREADY EXISTS

bireg_analysis <- as.data.frame(matrix(nrow = length(vulnerability.list), ncol = length(outcomes.list)))
colnames(bireg_analysis) <- outcomes.list
bireg_analysis$factor <- vulnerability.list
bireg_analysis <- bireg_analysis %>% select(factor, everything())


###################################
# DEFINE FUNCTION
###################################

get_bivariate_regression <- function(df, measure, outcome, int_marrcohab=TRUE){
  
  # Create new variables in the DF for measure and outcome
  df$var <- df %>% pull(eval(parse(text=measure)))
  df$outcome <- df %>% pull(eval(parse(text=outcome)))
  
  df.design <- svydesign(id=~v001, weights=~sdweight, data=df, nest=TRUE) ## Reset the survey design object with these new vars defined
  #df.design <-svydesign(id=~v021+survey, strata=~v023, weights=~wt, data=df, nest=TRUE) 
  
  # multilevel.out <- length(unique(na.omit(df1$outcome))) > 2
  binary.out <- all(na.omit(df$outcome) %in% 0:1)
  
  
  # GLM for main effect and urban/rural and partnership status interaction terms ------------------------
  if(binary.out == FALSE){
    svy.glm.main <- svyglm(outcome ~ var  , family=gaussian(link ="identity"), design=df.design, data=df)
    # svy.glm.main <- svyglm(outcome ~ var  + age.group, family=gaussian(link ="identity"), design=df.design, data=df)
  }else{
    svy.glm.main <- svyglm(outcome ~ var , family=quasibinomial, design=df.design, data=df)
    # svy.glm.main <-svyglm(outcome ~ var + age.group, family=quasibinomial, design=df.design, data=df)
  }
  
  # GET P-VALUES
  print(summary(svy.glm.main))
  
  # MAIN EFFECT
  res_svy.glm.main <- tidy(summ(svy.glm.main)) %>%
    filter(term != "(Intercept)"
           # & term != "age.group20-24" 
           # & term != "age.group25-29" 
           # & term != "age.group30-34" 
           # & term != "age.group35-39" 
           # & term != "age.group40-44" 
           # & term != "age.group45-49" 
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
final <- final %>% mutate(pct_sig = rowMeans(select(., 2:29), na.rm=T))
table(final$pct_sig)
hist(final$pct_sig)

# write_xlsx(final, "1_biassoc_summary_rural.xlsx")
# write_xlsx(final, "1_biassoc_summary_urban.xlsx")

##merge the two Excel files 
urban <- read_excel("India_constructed_data/1_biassoc_summary_urban.xlsx")
rural <- read_excel("India_constructed_data/1_biassoc_summary_rural.xlsx")
urban <- urban %>% select(factor, pct_sig)
names(urban) <- c("factor", "urban_pct_sig")
rural <- rural %>% select(factor, pct_sig)
names(rural) <- c("factor", "rural_pct_sig")

univariate <- merge(urban, rural, by="factor")
#read in data dictionary to keep the order of predictors 
predictors <- read_excel("UP_DHS_2020_Data_Dictionary.xlsx", sheet="predictors")
predictors <- predictors %>% 
  select(order, varnames) %>%
  filter(!is.na(varnames))
names(predictors) <- c("order", "factor")
univariate <- merge(predictors, univariate, by="factor")

write_xlsx(univariate, "India_constructed_data/1_univariate_summary.xlsx")
