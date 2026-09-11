################################################################################
# TYPING TOOL
################################################################################


###################################
# RUN SETUP
source("1_setup.R")
pacman::p_load(rpart, rpart.plot, caret)

###################################
# GET DATA INPUTS

vulnerability_vars <- readRDS(vulnerability_excel_file)

vulnerability_vars_tt <- vulnerability_vars %>%
   dplyr::filter(typing_tool_include == 1)


path = paste0(lca_path, "urban_outcomes_vulnerability_class_ranked.rds")
urban_input <- readRDS(path)


path = paste0(lca_path, "rural_outcomes_vulnerability_class_ranked.rds")
rural_input <- readRDS(path)

###################################
# URBAN
stratum = "urban"

urban_input$n_class <- urban_input %>% pull(eval(parse(text=segment_name))) # segment name is the ranked segment name

urban_tt_vars <- vulnerability_vars_tt %>%
  dplyr::filter(typing_tool_strata %in% c("both", stratum)) %>%
  dplyr::select(vulnerability_variable) %>%
  setNames(c("variable"))

tt_input_names <- names(urban_input)[(names(urban_input) %in% c("n_class", urban_tt_vars$variable))]
urban_input <- subset(urban_input, select=c(tt_input_names))

# GROW TREE
tree_urban <- rpart(n_class ~ .,
                    data=urban_input, control=rpart.control(cp=0.0001, xval=5), method="class")


rpart.plot(tree_urban, type = 5, fallen.leaves = T, varlen = -15, tweak = 2, extra = 0, clip.facs = F, legend.x = NA, cex = .5)

plotcp(tree_urban); printcp(tree_urban)


# PRUNE TREE
tree_urban_prune <- prune(tree_urban, cp=.005)
plotcp(tree_urban_prune); printcp(tree_urban_prune)

PredictCART_train_urban <- predict(tree_urban_prune, data=urban_input, type="class")
confusionMatrix(data=PredictCART_train_urban, as.factor(urban_input$n_class))

rpart.plot(tree_urban_prune, type = 5, fallen.leaves = F, varlen = -15, tweak = 2, extra = 0, clip.facs = F, legend.x = NA, cex = .3)

###################################
# RURAL
stratum = "rural"

rural_input$n_class <- rural_input %>% pull(eval(parse(text=segment_name)))

rural_tt_vars <- vulnerability_vars_tt %>%
  dplyr::filter(typing_tool_strata %in% c("both", stratum)) %>%
  dplyr::select(vulnerability_variable) %>%
  setNames(c("variable"))

tt_input_names <- names(rural_input)[(names(rural_input) %in% c("n_class", rural_tt_vars$variable))]
rural_input <- subset(rural_input, select=c(tt_input_names))


# GROW TREE
tree_rural <- rpart(n_class ~ .,
                    data=rural_input, control=rpart.control(cp=0.0001, xval=5), method="class")

rpart.plot(tree_rural, type = 3, fallen.leaves = T, varlen = -15, tweak = 2, extra = 0, clip.facs = F, legend.x = NA, cex = .3)

# PRUNE TREE
plotcp(tree_rural); printcp(tree_rural)

tree_rural_prune <- prune(tree_rural, cp=.001)
plotcp(tree_rural_prune); printcp(tree_rural_prune)

PredictCART_train_rural <- predict(tree_rural_prune, data=rural_input, type="class")
confusionMatrix(data=PredictCART_train_rural, as.factor(rural_input$n_class))



rpart.plot(tree_rural_prune, type = 5, fallen.leaves = F, varlen = -15, tweak = 2, extra = 0, clip.facs = F, legend.x = NA, cex = .3)
