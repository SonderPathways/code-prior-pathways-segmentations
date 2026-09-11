
################################################################################
# GENERATE WEALTH SUM
################################################################################
# PROGRAM:
# PURPOSE: To create a simplified wealth sum to be used in typing tools (instead of a wealth index)
#
# RUN SETUP
# DATA INPUTS:
# data = dataframe with vulnerability factors + LCA classification
# vars = wealth_vars vector defined in setup, contains the list of wealth variables to be considered for the wealth sum
# wealth.index.varname is a character string, and is the name of the wealth index variable in "data"
#
# DATA OUTPUTS: A dataframe with an additional column "wealth.sum"
# AUTHOR: VALERIE C. VALERIO
#
###################################
fun_generate_wealth_sum<-function(data= NULL, vars = NULL, wealth.index.varname = NULL){
  stopifnot(wealth.index.varname %in% (colnames(data)))
  data$wealth.index = as.numeric(data %>% pull(wealth.index.varname))
  data2<-data %>% dplyr::select(all_of(vars))

  # Assuming that there is no high correlation amongst wealth variables
  corr <- round(cor(data$wealth.index, data2, method = "spearman", use='complete.obs'), 2)
  message('The following variables will be used to create the wealth sum: \n',
          paste0(vars[which(abs(corr)>0.5)], sep="\n"))

  # Create wealth sum
  data <- data %>% mutate(
    wealth.sum = rowSums(across(vars[which(abs(corr)>0.5)])))

  stopifnot(all(!data$wealth.sum %>% is.na()))

  return(data)

}
