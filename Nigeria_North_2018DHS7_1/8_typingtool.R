# Typing tool ################################################################################

# Load libraries ----
pacman::p_load(rpart, rpart.plot, caret)
pacman::p_load(rattle, foreign, ggplot2, dplyr, tidyr, data.table, haven,
               ggcorrplot)

# Load scripts #####
source("1_setup.R")

# Read in data ####
# Read in urban file
urbseg<- readRDS(paste0(lca_path, "urban_outcomes_vulnerability_class.rds"))
# Read in rural file
rurseg<- readRDS(paste0(lca_path, "rural_outcomes_vulnerability_class.rds"))

# Read in LCA segmentation variables #####
## Urban ----
urbvars = read_excel(pathways_workbook_path, sheet = 'vulnerabilities')  %>%
  # Filter to only vars used in LCA
  dplyr::filter(lca_include == 1,
         # For urban strata
         lca_strata %in% c('urban', 'both'),
         # Exclude wealth index (we are creating a new wealth var below)
         vulnerability_variable !='wealth.index.ur') %>%
  pull(vulnerability_variable)

## Rural ----
rurvars = read_excel(pathways_workbook_path, sheet = 'vulnerabilities') %>%
  # Filter to only vars used in LCA
  dplyr::filter(lca_include == 1,
         # For urban strata
         lca_strata %in% c('rural', 'both'),
         # Exclude wealth index
         vulnerability_variable !='wealth.index.ur') %>%
  pull(vulnerability_variable)

# Create new (simple) wealth variable ######
# Look to see which household asset variables are most correlated with the wealth.index.ur
# variable so can create a new variable that's the sum of these assets rather than a complex PCA
# wealth_vars (defined in 1_setup.R) has wealth variables to be considered for the sum

wealth_vars = c("hh.electricity", "hh.radio", "hh.tv", "hh.refrig",
                "hh.bike", "hh.motor", "hh.car", "hh.phone", "hh.watch",
                "hh.cart", "hh.motorboat", "hh.motortransport.yn",
                "hh.computer", "hh.land", "hh.animal", "hh.table", "hh.chair",
                "hh.bank.acct", "hh.rooms.2plus",
                "hh.water.notpiped", "hh.nowatpur", "hh.noimp.water",
                "hh.wat.time.yn",  "hh.sanitation.yn")

urbseg <- fun_generate_wealth_sum(data = urbseg, vars = wealth_vars, wealth.index.varname = 'wealth.index.ur')
rurseg <- fun_generate_wealth_sum(data = rurseg, vars = wealth_vars, wealth.index.varname = 'wealth.index.ur')


# Perform CART ####
set.seed(1235)

## URBAN ####
### Grow tree ####
tree_urban <- rpart(LCA6_class~
                          # segmentation variables
                          any.media.yn + desc.ownincome+ #discharge.checkedhealth +
                          #dv.index +
                          early.cohab.15 + ed.lev.low +
                          fertility.pref + freq.tv + hh.bank.acct +
                          hh.clean.fuel + #hh.kidwom.rat.cat +
                          #hh.sanitation.yn +
                          highestyearsedinHH.7plus + jdwd.index + med.index +
                          mobile.financial + #partner.absent.yn +
                          slum.sum +
                          wealth.sum,
                        data = urbseg,
                        control=rpart.control(cp=0.001, xval=5), method="class")

# Confusion matrix
(confusion.matrix = confusionMatrix(data= predict(tree_urban,
                                                  data=urbseg,
                                                  type="class"),
                                    as.factor(urbseg$LCA6_class)))
# CP
printcp(tree_urban)

# Predictions
PredictCART_train_urban =predict(tree_urban, data = rurseg, type = "class")

# Var importance
tree_urban$variable.importance


### Prune tree ----
# It's almost identical, so using unpruned tree as final
tree_urban_prune = prune(tree_urban, cp=0.0010604        ) # no need to use min error cps
printcp(tree_urban_prune); plotcp(tree_urban_prune)
(confusionMatrix(data= predict(tree_urban_prune, data=urbseg,
                               type="class"), as.factor(urbseg$LCA6_class)))

### Test accuracy, class Specificity and sensitivity -----
# Accuracy
if((confusion.matrix)$overall[1]<.80){
  message("The typing tool accuracy is under 80%. Consider modifying the input variables.")
}
# Sensitivity
if(any(confusion.matrix$byClass[,1]<.80)){
  message('Some of the classes have Sensitivity <80%: Class ',
          which(confusion.matrix$byClass[,1]<.80))
}

# Specificity
if(any(confusion.matrix$byClass[,2]<.80)){
  message('Some of the classes have Specificity <80%')
}


### Save fancy Rpart plot ----
pdf(file = paste0(cart_plots, 'urban_typing_tool.pdf'), width = 60, height = 10)
fancyRpartPlot(tree_urban_prune, type = 3, cex = 0.7, caption = NULL, uniform=T)
dev.off()

### Save typing outcome -----
saveRDS(urbseg %>%
          cbind(pred_LCA6_class= PredictCART_train_urban),
        file = paste0(cart_path, 'urban_outcomes_typing_tool_class.rds'))


saveRDS(urbseg %>%
          cbind(pred_LCA6_class= predict(tree_urban_prune, data=urbseg,
                                         type="class")),
        file = paste0(cart_path, 'urban_outcomes_typing_tool_class_v6_final.rds'))

## RURAL ----
### Grow tree ----
# Remove dv.index
tree_rural<- rpart(LCA5_class ~ .,
                     data=rurseg %>%
                       dplyr::select(all_of(rurvars), wealth.sum, LCA5_class) %>%
                       dplyr::select(-dv.index),
                     control=rpart.control(cp=0.0001, xval=5), method="class")
# CP
printcp(tree_rural);# plotcp(tree_rural) #13 vars, excludes dv.index

# Confusion matrix
# 90.86%, complex tree but only 13 vars
(confusion.matrix = confusionMatrix(data=predict(tree_rural, data=rurseg, type="class"),
                as.factor(rurseg$LCA5_class)))

# Var importance
tree_rural$variable.importance

# Predictions
PredictCART_train_rural =predict(tree_rural, data = rurseg, type = "class")

### Prune tree ----
min.cp = tree_rural$cptable[which.min(tree_rural$cptable[,"xerror"]),"CP"]
tree_rural_prune<-prune(tree_rural, cp=min.cp) # 0.001# .0021
plotcp(tree_rural_prune); printcp(tree_rural_prune)

# Look at eval metrics for pruned tree and compare to
confusionMatrix(data = PredictCART_train_rural, as.factor(rurseg$LCA5_class))
confusionMatrix(data=predict(tree_rural_prune, data=rurseg, type="class"), as.factor(rurseg$LCA5_class))

prp(tree_rural_prune, type=3, varlen=0, cex=1)

# Because they are very similar but the unpruned tree has higher accuracy, we choose the
# unpruned tree

### Test accuracy, class Specificity and sensitivity -----
# Accuracy
if((confusion.matrix)$overall[1]<.80){
  message("The typing tool accuracy is under 80%. Consider modifying the input variables.")
}
# Sensitivity
if(any(confusion.matrix$byClass[,1]<.80)){
  if(all(confusion.matrix$byClass[,1]>=.80)!=TRUE)
  message('Some of the classes have Sensitivity <80%: Class ',
          which(confusion.matrix$byClass[,1]<.80))
}

# Specificity
if(any(confusion.matrix$byClass[,2]<.80)){
  message('Some of the classes have Specificity <80%')
}

### Save fancy Rpart plot ----
pdf(file = paste0(cart_plots, 'rural_typing_tool.pdf'), width = 120, height = 10)
fancyRpartPlot(tree_rural, type = 3, cex = 0.7, caption = NULL, uniform=T)
dev.off()

### Save typing outcome -----
saveRDS(rurseg %>%
          cbind(pred_LCA5_class= PredictCART_train_rural),
        file = paste0(cart_path, 'rural_outcomes_typing_tool_class.rds'))
