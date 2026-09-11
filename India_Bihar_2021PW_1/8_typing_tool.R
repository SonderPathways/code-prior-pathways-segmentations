


################################################################################
# TYPING TOOL
################################################################################


###################################
# RUN SETUP
source("1_setup.R")
pacman::p_load(rpart, rpart.plot, caret, randomForest)


###################################
# DEFINE PARAMETERS

urban_seg = "LCA5_class"
rural_seg = "LCA4_class"


###################################
# GET DATA INPUTS


vulnerability_vars <- readRDS(vulnerability_excel_file)

vulnerability_vars_tt <- vulnerability_vars %>%
  dplyr::filter(typing_tool_include == 1)


path = paste0(lca_path, "urban_outcomes_vulnerability_class.rds")
urban_input <- readRDS(path)


path = paste0(lca_path, "rural_outcomes_vulnerability_class.rds")
rural_input <- readRDS(path)


###################################
# URBAN
stratum = "urban"

urban_input$n_class <- urban_input %>% pull(eval(parse(text=urban_seg)))

urban_tt_vars <- vulnerability_vars_tt %>%
  filter(typing_tool_strata %in% c("both", stratum)) %>%
  dplyr::select(vulnerability_variable) %>%
  setNames(c("variable"))

tt_input_names <- names(urban_input)[(names(urban_input) %in% c("n_class", urban_tt_vars$variable))]
urban_input <- subset(urban_input, select=c(tt_input_names))


# GROW TREE
tree_urban <- rpart(n_class ~ .,
                    data=urban_input, control=rpart.control(cp=0.001, xval=5), method="class")

prp(tree_urban, type=3, varlen=0, cex=1)

plotcp(tree_urban); printcp(tree_urban)


# PRUNE TREE
tree_urban_prune <- prune(tree_urban, cp=.02)
plotcp(tree_urban_prune); printcp(tree_urban_prune)

PredictCART_train_urban <- predict(tree_urban, data=urban_input, type="class")
confusionMatrix(data=PredictCART_train_urban, as.factor(urban_input$n_class))

prp(tree_urban_prune, type=3, varlen=0, cex=1)


###################################
# RURAL
stratum = "rural"

rural_input$n_class <- rural_input %>% pull(eval(parse(text=rural_seg)))

rural_tt_vars <- vulnerability_vars_tt %>%
  filter(typing_tool_strata %in% c("both", stratum)) %>%
  dplyr::select(vulnerability_variable) %>%
  setNames(c("variable"))

tt_input_names <- names(urban_input)[(names(urban_input) %in% c("n_class", urban_tt_vars$variable))]
rural_input <- subset(rural_input, select=c(tt_input_names))


# GROW TREE
tree_rural <- rpart(n_class ~ .,
                    data=rural_input, control=rpart.control(cp=0.001, xval=5), method="class")

prp(tree_rural, type=3, varlen=0, cex=1)


# PRUNE TREE
tree_rural_prune <- prune(tree_rural, cp=.02)
plotcp(tree_rural_prune); printcp(tree_rural_prune)

PredictCART_train_rural <- predict(tree_rural, data=rural_input, type="class")
confusionMatrix(data=PredictCART_train_rural, as.factor(rural_input$n_class))

prp(tree_rural_prune, type=3, varlen=0, cex=1)



###################################
# DATA PREP

# urban_seg <- urban_seg %>%
#   mutate(wealth.sum = rowSums(across(c(hh.electricity, hh.tv, hh.refrig, hh.bank.acct, hh.computer, hh.car, hh.noimp.latrine))))
#
# rural_seg <- rural_seg %>%
#   mutate(wealth.sum = rowSums(across(c(hh.electricity, hh.tv, hh.refrig, hh.bank.acct, hh.computer, hh.car, hh.stove))))


###################################
# sample_ind <- sample(nrow(urban_seg),nrow(urban_seg)*0.70)
# train <- urban_seg[sample_ind,]
# test <- urban_seg[-sample_ind,]
#
# base_model <- rpart(LCA5_class ~ marital.cat+ any.media.yn+ desc.lrgpur+ desc.ownincome+ wealth.sum+
#                          desc.fp+wd.index+ ed.level+ hh.savingsclub+ fertility.pref.cat+ hh.mobile+ hh.electricity+
#                          has.bank+ hh.clean.fuel+ hh.noimp.water+ hh.noimp.latrine+yrs.curr.pship.cat+
#                          partner.absent.yn+ prev.pship+ideal.n.child.cat+female.circumcision+
#                          hh.recv.funds.cat+ internet.use+hh.land+highestyearsedinHH.yrs+med.dist+
#                          med.cost+mobile.financial+ num.under5.cat+ partner.ed.level+
#                          total.sex.partners+ wife.order+ working+freq.tv, data = train, method = "class",
#                        control = rpart.control(cp = 0))
#
# summary(base_model)
# #Plot Decision Tree
# plot(base_model)
# # Examine the complexity plot
# printcp(base_model)
# plotcp(base_model)
#
# # Compute the accuracy of the pruned tree
# test$pred <- predict(base_model, test, type = "class")
# base_accuracy <- mean(test$pred == test$LCA5_class)
#
#
# ###################################
# # GROW TREES
# tree_urban <- rpart(LCA5_class ~ marital.cat+ any.media.yn+ desc.lrgpur+ desc.ownincome+ wealth.sum+
#                       desc.fp+wd.index+ ed.level+ hh.savingsclub+ fertility.pref.cat+ hh.mobile+ hh.electricity+
#                       has.bank+ hh.clean.fuel+ hh.noimp.water+ hh.noimp.latrine+yrs.curr.pship.cat+
#                       partner.absent.yn+ prev.pship+ideal.n.child.cat+female.circumcision+
#                       hh.recv.funds.cat+ internet.use+hh.land+highestyearsedinHH.yrs+med.dist+
#                       med.cost+mobile.financial+ num.under5.cat+ partner.ed.level+
#                       total.sex.partners+ wife.order+ working+freq.tv,
#                     data=urban_seg, control=rpart.control(cp=0.001, xval=5), method="class")
#
#
# plotcp(tree_urban); printcp(tree_urban)
#
# tree_urban_prune<-prune(tree_urban, cp=.02)
# plotcp(tree_urban_prune); printcp(tree_urban_prune)
#
# PredictCART_train_urban <- predict(tree_urban, data=urban_seg, type="class")
# confusionMatrix(data=PredictCART_train_urban, as.factor(urban_seg$LCA5_class))
#
# prp(tree_urban_prune, type=3, varlen=0, cex=1)


