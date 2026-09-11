rm(list = ls())
pacman::p_load(colorspace, viridis, broom, devtools,jtools, openxlsx, writexl, survey, dplyr)
setwd("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2021 Pathways/Data/Bihar_constructed_data")

###################################
# DEFINE FUNCTION
###################################

get_bivariate_regression <- function(df, measure, outcome, int_marrcohab=TRUE){
  
  # Create new variables in the DF for measure and outcome
  df$var <- df %>% pull(eval(parse(text=measure)))
  df$outcome <- df %>% pull(eval(parse(text=outcome)))
  
  df.design <-svydesign(id=~psu, weights=~weight, data=df, nest=TRUE) ## Reset the survey design object with these new vars defined
  #df.design <-svydesign(id=~v021+survey, strata=~v023, weights=~wt, data=df, nest=TRUE) 
  
  # Create an indicator to note if exposure and/or outcome are binary (and coded as 0 vs 1)
  binary.exp <- all(na.omit(df$var) %in% 0:1)
  binary.out <- all(na.omit(df$outcome) %in% 0:1)
  
  
  # GLM for main effect and urban/rural and partnership status interaction terms ------------------------
  if(binary.out == FALSE){
      svy.glm.main <- svyglm(outcome ~ var  , family=gaussian(link ="identity"), design=df.design, data=df)
      # svy.glm.main <- svyglm(outcome ~ var + as.factor(her.age) , family=gaussian(link ="identity"), design=df.design, data=df)
  }else{
    svy.glm.main <-svyglm(outcome ~ var , family=quasibinomial, design=df.design, data=df)
    # svy.glm.main <-svyglm(outcome ~ var + as.factor(her.age), family=quasibinomial, design=df.design, data=df)
  }
  
  # GET P-VALUES
  print(summary(svy.glm.main))
  
  # MAIN EFFECT
  res_svy.glm.main <- tidy(summ(svy.glm.main)) %>%
    filter(term != "(Intercept)"
           # & term != "as.factor(her.age)2"
           # & term != "as.factor(her.age)3"
           # & term != "as.factor(her.age)4"
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
# GET OUTCOMES AND VULNERABILITY FACTORS
outcomes      <- readRDS("1_Bihar_Pathways_outcomes.rds")
vulnerability <- readRDS("1_Bihar_Pathways_vulpredictors.rds")


###################################
# MERGE TOGETHER
outcomes_vulnerability <- merge(vulnerability, outcomes, by="caseid", all.x=T)

###################################
# DEFINE LIST OF OUTCOMES AND VULNERABILITY FACTORS FOR INITIAL BIVARIATE REGRESSOIN ANALYSIS

# OUTCOMES
outcomes.list <- names(outcomes)[!names(outcomes) %in% c("caseid")]

# VULNERABILITY FACTORS
vulnerability.list <- names(vulnerability)[!names(vulnerability) %in% c("caseid", "psu", "weight", "her.age")]


###################################
# CREATE A MATRIX TO POPULATE REGRESSION INFERENCE OR READ IN IF ALREADY EXISTS

bireg_analysis <- as.data.frame(matrix(nrow = length(vulnerability.list), ncol = length(outcomes.list)))
colnames(bireg_analysis) <- outcomes.list
bireg_analysis$factor <- vulnerability.list
bireg_analysis <- bireg_analysis %>% select(factor, everything())

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
  mutate(pct_sig = rowMeans(select(., her.ill_3mo:kid.hosp)))
table(final$pct_sig)
hist(final$pct_sig)

write_xlsx(final, "5_assoc_summary_simple_noage.adj.xlsx")
#write_xlsx(final, "5_assoc_summary_simple_age.adj.xlsx")
