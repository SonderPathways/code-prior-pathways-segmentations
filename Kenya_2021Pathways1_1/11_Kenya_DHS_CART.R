############################  KENYA_DHS22_PCA_CART #############################
#
# Purpose: Conducts classification analysis with clusters as outputs
#
# Authors: Anu Mishra (original script), Helena Manguerra (UW START, Kenya revisions), Darcy Rao
#
# Input files: dhs.seg.rds #Check this 
#             
# Last edited: 
#
# Output files: Pictures of trees saved as typingtool.doc in plots folder
#
# Ancestor scripts: 9a_Kenya_DHS_LCA
#
# Descendant script: None.
# 
# Note: Need to set directory for each user
# Note: This script requires changing variables in the Data Dictionary
#_______________________________________________________________________________ 

# Clearing the workspace -------------------------------------------------------

rm(list=ls())

# Load libraries ---------------------------------------------------------------

pacman::p_load(dplyr, tidyverse, data.table, rpart, rpart.plot, caret, randomForest, stringr)

# Set working directory --------------------------------------------------------

user = "Darcy"

if (tolower(user) == "helena") {
  rootdir <- "C:/Users/hmang/UW START Dropbox/Helena Manguerra/"
} else if (tolower(user) == "mac") {
  rootdir <- "~/UW START Dropbox/Helena Manguerra/"
} else if (tolower(user) == "pc") {
  rootdir <- "C:/Users/theja/UW START Dropbox/Helena Manguerra/"
} else if (tolower(user) == "amanda") {
  rootdir <- "C:/Users/manda/UW START Dropbox/Amanda Brumwell/"
} else if (tolower(user) == "andrea") {
  rootdir <- "C:/Users/Andrea Rivas/UW START Dropbox/Andrea Rivas/"
} else if (tolower(user) == "bih") {
  rootdir <- "C:/Users/bihmo/UW START Dropbox/Bih Epse Cuba Moki Suh/"
} else if (tolower(user) == "sarah") {
  rootdir <- "C:/Users/sshaw/UW START Dropbox/Sarah Shaw/"
} else if (tolower(user) == "darcy") {
  # rootdir <-"/Users/darcyrao/Dropbox"
  rootdir <- "/Users/darcyrao/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Pathways - GE Data & Insights/Modeling/Kenya 2022 DHS/"
} else if (tolower(user) == "elisabeth") {
  rootdir <- "/Users/elisabethro/OneDrive - Bill & Melinda Gates Foundation/Pathways - GE Data & Insights/Modeling/Kenya 2022 DHS"
} else {
  print("No filepath")
}

if (tolower(user) %in% c("elisabeth", "darcy")) {
  input_dir <- rootdir
}else{
  input_dir <- file.path(rootdir, "START_229_Gender Equity/Working Files/Data/KE_2022_constructed_data")
}

## Load the full DHS manipulated dataset to pull in other vars
vulnerability <- readRDS(file.path(input_dir, "Data/Constructed data", "4_Kenya_DHS_vulnerability.rds"))
outcomes <- readRDS(file.path(input_dir, "Data/Constructed data", "1_Kenya_DHS_outcomes.rds"))
dhs.full <- merge(vulnerability, outcomes, by="caseid", all.x=T)


#### URBAN ####

subset = "urban_round6_u5"
kclass = "cluster_k4"
#' This code is used to check whether any misclassification from the tool moves people to a "neighboring" class or one at the other end of the spectrum. 
#' The vulnerability ranking here is very preliminary based on visual review of outcomes plots and will change with refined ranking processes. 
# clusters_least_to_most_vuln = c(2,3,1,4)
# labels_least_to_most_vuln = c("Least Vuln", 
#                               "Less Vuln",
#                               "More Vuln",
#                               "Most Vuln")


# Read in the data produced by LCA script -----------------------------------

dhs.lca.u <- readRDS(file.path(input_dir, "Solutions/KE 2022 (women with kids - final solution)",  "LCA", subset, "dhs.seg.rds"))  # Use dhs.seg because it has the categorical labels
dhs.lca.u[, cluster_kSelected := as.factor(get(kclass))]

# Merge with the full dhs data object to pull in other variables
dhs.seg.urban <- dhs.lca.u %>%
  dplyr::select(names(dhs.lca.u)[which(str_detect(names(dhs.lca.u), "cluster_|caseid"))]) %>%
  left_join(dhs.full, by = "caseid")
dhs.seg.urban <- as.data.table(dhs.seg.urban)


# Define variables from LCA model ----------------------------------------------

vuln.vars.u <- names(dhs.lca.u)[which(!str_detect(names(dhs.lca.u), "cluster_|caseid"))]

# Remove variables that are too sensitive for inclusion in the typing tool or difficult to measure and the indices (will add in components of the indices below)
vuln.vars.u <- setdiff(vuln.vars.u, c("polygamy", "wife.just.cond", "female.circumcision.yn",
                                  "early.sex.15", "dv.index.3plus","electric.assets.index", "transpo.index.cat",
                                  "slum1", "partner.worktype",
                                  "decis.fp.cat", "decis.hlth.cat", "preg.pressured", "pship.cat", # drop decis.fp.cat and decis.hlth.cat and preg.pressured because the solution is splitting on this instead of marr.cohab
                                  "highestyearsedinHH.ur.under10")) # Drop highest years of education in the household to try using highest level instead

# Replace indices with component variables
vuln.vars.u <- c(vuln.vars.u, 
                   c("hh.computer", "hh.phone", "hh.radio", "hh.refrig",  "hh.tv"), #"hh.cd", "hh.clock",  "hh.dvd",  "hh.microwave"
                   c("hh.bike", "hh.motor", "hh.motorboat", "hh.car"),
                   c("water"))
                   # c("floor","water","latrine","living"))

# Add marr.cohab as a simpler partnership type var
vuln.vars.u <- c(vuln.vars.u, "marr.cohab", "partner.notworking.yn")

# Add highest level of education instead of highest years
vuln.vars.u <- c(vuln.vars.u, "highestleveledinHH")



# # Alternate to explore based on random forest to find variables that differentiate segments best. NOTE THIS DID NOT IMPROVE PERFORMANCE
# # variables used in best-fitting initial tree
# vuln.vars.u1 <- c("pship.cat", "clean.fuel", "wall", "muslim", "age.1stbrth.cat", "highestyearsedinHH.ur.under10",
#                   "internet.pastmonth", "hh.refrig", "hh.where.cook2", "hh.wat.time.yn", "ed.lev3", "hh.tv", "freq.tv",
#                   "toilet.improved")
# # merge with variables that came out from random forest to improve differentiation of classes 1 and 2
# vuln.vars.u2 <- union(vuln.vars.u1, vars.rf_u)


# Set the seed -----------------------------------------------------------------

set.seed(124)

# Split the data into test and train -------------------------------------------

train.u <- sample(1:nrow(dhs.seg.urban), 0.7*nrow(dhs.seg.urban), replace = F)
df.train.u <- dhs.seg.urban[train.u,]
df.test.u <- dhs.seg.urban[-train.u,]

# Grow up to full tree, finding optimal cost-complexity parameter (cp) for each sub-tree using cross-valiation  -------------------------------

 #* Grow up to tree, saving out all sub-trees with optimal cp ------------------

tree <- rpart(as.formula(paste0("cluster_kSelected ~ ", paste0(vuln.vars.u, collapse = " + "))), 
              data=df.train.u, 
              control=rpart.control(cp=0, # set to 0 to prevent premature stop of growth
                                    xval=10), # xval = number of cross-validations 
              method="class")

 #* Save out cp parameters -----------------------------------------------------
 
cptable <- data.table(tree$cptable)

message("Number of sub-trees tested: ", nrow(cptable))
cptable[, index := .I]

 #* Identify subtree with minimum error ----------------------------------------
 
minError_index <- cptable[xerror == min(xerror), index][1]
minError_cp <- cptable[index == minError_index, CP]
minError_xerror <- cptable[index == minError_index, xerror]
minError_xstd <- cptable[index == minError_index, xstd]

 #* Identify simplest substree within 1SE of the minimum error across tested sub-trees ----
 
oneSE_index <- max(cptable[xerror >= minError_xerror + minError_xstd & 
                             index < minError_index, index+1])
oneSE_cp <- cptable[index == oneSE_index, CP]
oneSE_xerror <- cptable[index == oneSE_index, xerror]
oneSE_xstd <- cptable[index == oneSE_index, xstd]


 #* Print plot of sub-trees  - mimimum error = blue, simplest model within 1SD of min error = red ----
 
# pdf(file.path(input_dir, "Solutions/KE 2022 (women with kids - final solution)", "LCA", subset, "plotcp.pdf"))
plotcp(tree, minline = T)
abline(v=minError_index, col = "blue")
abline(v=oneSE_index, col = "red")
# dev.off()

 #* Print out summary ----------------------------------------------------------
summary(tree)

# Select optimally pruned tree -------------------------------------------------

# Select final tree, using simplest tree with 1 SE of minimum error ------------
pruned_tree<-prune(tree, 
                   cp=0.0066)

summary(pruned_tree)

# Identify most important variables --------------------------------------------

sort(pruned_tree$variable.importance)

# Plot three versions of the optimal trees ---------------------------------------

pdf(file.path(input_dir, "Solutions/KE 2022 (women with kids - final solution)", "Typing tool", "urban_pruned_tree_v1.pdf"), width = 16, height = 8)
plot(pruned_tree, uniform=TRUE, main="Classification Tree for LCA")
text(pruned_tree, use.n=F, all=T, cex=.7)
dev.off()

pdf(file.path(input_dir, "Solutions/KE 2022 (women with kids - final solution)", "Typing tool", "urban_pruned_tree_v2.pdf"))
rpart.plot(pruned_tree, type = 5)
dev.off()

pdf(file.path(input_dir, "Solutions/KE 2022 (women with kids - final solution)", "Typing tool", "urban_pruned_tree_v3.pdf"))
rpart.plot(pruned_tree, type = 0, fallen.leaves = F, varlen = -15, tweak = 2, extra = 0, clip.facs = F, legend.x = NA)
dev.off()


# prp(pruned_tree, type=1, varlen=0, cex=1) # This layer makes the plot very hard to read, at least with current formatting. Cutting for now but we could adjust the arguments to make it look better


# Compare Performance on Training vs Test Dataset ------------------------------

 #* Calculate performance in the training data set -----------------------------

PredictCART_train.u <- predict(pruned_tree, df.train.u, type="class")
df.train.u <- data.table(df.train.u, PredictCART_train.u)
(cm_train.u <- confusionMatrix(data=PredictCART_train.u, df.train.u$cluster_kSelected))
message("Training accuracy: ", round(cm_train.u$overall[1],2))

 #* Calculate performance in the hold-out test data set ------------------------
 
PredictCART_test.u <- predict(pruned_tree, df.test.u, type="class")
df.test.u <- data.table(df.test.u, PredictCART_test.u)
(cm_test.u <- confusionMatrix(data=PredictCART_test.u, df.test.u$cluster_kSelected))
message("Test accuracy: ", round(cm_test.u$overall[1],2))

#  #* Counts table ---------------------------------------------------------------
#  
# labels_least_to_most_vuln <- paste0(labels_least_to_most_vuln, " (", clusters_least_to_most_vuln, ")")
# 
# df.test.u[, predicted := factor(as.integer(PredictCART_test.u), labels = labels_least_to_most_vuln, levels = clusters_least_to_most_vuln, ordered = T)]
# df.test.u[, observed := factor(as.integer(cluster_kSelected), labels = labels_least_to_most_vuln, levels = clusters_least_to_most_vuln, ordered = T)]
# 
# counts_tbl <- table(df.test.u$predicted, 
#                     df.test.u$observed) # distribution of classes assigned to individuals of each class based on LCA assignment
# 
#  #* Proportion table -----------------------------------------------------------
#  #
# round(prop.table(counts_tbl, 2),3) # distribution of classes assigned to individuals of each class based on LCA assignment
# 


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
#### RURAL ####

subset = "rural_round6_u5"
kclass = "cluster_k4"
# clusters_least_to_most_vuln = c(3,4,1,2)
# labels_least_to_most_vuln = c("Least Vuln", 
#                               "Less Vuln", 
#                               "More Vuln", 
#                               "Most Vuln")
# 


# Read in the data produced by LCA script -----------------------------------

dhs.lca.r <- readRDS(file.path(input_dir, "Solutions/KE 2022 (women with kids - final solution)",  "LCA", subset, "dhs.seg.rds"))  # Use dhs.seg because it has the categorical labels
dhs.lca.r[, cluster_kSelected := as.factor(get(kclass))]

# Merge with the full dhs data object to pull in other variables
dhs.seg.rural <- dhs.lca.r %>%
  dplyr::select(names(dhs.lca.r)[which(str_detect(names(dhs.lca.r), "cluster_|caseid"))]) %>%
  left_join(dhs.full, by = "caseid")
dhs.seg.rural <- as.data.table(dhs.seg.rural)


# Define variables from LCA model ----------------------------------------------

vuln.vars.r <- names(dhs.lca.r)[which(!str_detect(names(dhs.lca.r), "cluster_|caseid"))]

# Remove variables that are too sensitive for inclusion in the typing tool or difficult to measure and the indices (will add in components of the indices below)
vuln.vars.r <- setdiff(vuln.vars.r, c("polygamy", "wife.just.cond", "female.circumcision.yn", 
                                      "early.sex.15", "dv.index.3plus", "electric.assets.index", "transpo.index.cat",
                                      "household.assets.index.2.bi", "preg.pressured", "first.wife", "months.cohabbirth.cat",
                                      "partner.worktype",
                                      "highestyearsedinHH.ru.under9")) # Drop highest years of education in the household to try using highest level instead

# Replace indices with component variables
vuln.vars.r <- c(vuln.vars.r, 
                 c("hh.computer", "hh.phone", "hh.radio", "hh.refrig",  "hh.tv"), # drop those that might change more quickly: "hh.cd", "hh.clock",  "hh.dvd", "hh.microwave"
                 c("hh.bike", "hh.motor", "hh.motorboat", "hh.car"),
                 c("hh.chair", "hh.table",   "hh.bed", "hh.sofa", "hh.cupboard", "toilet.improved")) # drop those that might change more quickly: "hh.watch"

# Add marr.cohab as a simpler partnership type var, add partner.notworking as simpler measure of partner work status
vuln.vars.r <- c(vuln.vars.r, "marr.cohab", "partner.notworking.yn")

# take out decision-making because it is using that variable to split on partnership status for some reason
vuln.vars.r <- setdiff(vuln.vars.r, c("decis.fp.cat", "decis.hlth.cat"))

# Add in highest leve of education in the household, swapped from highest years of education
vuln.vars.r <- c(vuln.vars.r, "highestleveledinHH")



# Set the seed -----------------------------------------------------------------

set.seed(124)

# Split the data into test and train -------------------------------------------

train.r <- sample(1:nrow(dhs.seg.rural), 0.7*nrow(dhs.seg.rural), replace = F)
df.train.r <- dhs.seg.rural[train.r,]
df.test.r <- dhs.seg.rural[-train.r,]

# Grow up to full tree, finding optimal cost-complexity parameter (cp) for each sub-tree using cross-valiation  -------------------------------

#* Grow up to tree, saving out all sub-trees with optimal cp ------------------

tree <- rpart(as.formula(paste0("cluster_kSelected ~ ", paste0(vuln.vars.r, collapse = " + "))), 
              data=df.train.r, 
              control=rpart.control(cp=0, # set to 0 to prevent premature stop of growth
                                    xval=10), # xval = number of cross-validations 
              method="class")

#* Save out cp parameters -----------------------------------------------------

cptable <- data.table(tree$cptable)

message("Number of sub-trees tested: ", nrow(cptable))
cptable[, index := .I]

#* Identify subtree with minimum error ----------------------------------------

minError_index <- cptable[xerror == min(xerror), index][1]
minError_cp <- cptable[index == minError_index, CP]
minError_xerror <- cptable[index == minError_index, xerror]
minError_xstd <- cptable[index == minError_index, xstd]

#* Identify simplest substree within 1SE of the minimum error across tested sub-trees ----

oneSE_index <- max(cptable[xerror >= minError_xerror + minError_xstd & 
                             index < minError_index, index+1])
oneSE_cp <- cptable[index == oneSE_index, CP]
oneSE_xerror <- cptable[index == oneSE_index, xerror]
oneSE_xstd <- cptable[index == oneSE_index, xstd]


#* Print plot of sub-trees  - mimimum error = blue, simplest model within 1SD of min error = red ----

# pdf(file.path(input_dir, "Solutions/KE 2022 (women with kids - final solution)", "LCA", subset, "plotcp.pdf"))
plotcp(tree, minline = T)
abline(v=minError_index, col = "blue")
abline(v=oneSE_index, col = "red")
# dev.off()

#* Print out summary ----------------------------------------------------------
summary(tree)

# Select optimally pruned tree -------------------------------------------------

# Select final tree, using simplest tree with 1 SE of minimum error ------------
pruned_tree<-prune(tree, 
                   cp=0.0065) 

summary(pruned_tree)

# Identify most important variables --------------------------------------------

sort(pruned_tree$variable.importance)

# Plot three versions of the optimal trees ---------------------------------------

pdf(file.path(input_dir, "Solutions/KE 2022 (women with kids - final solution)", "Typing tool", "rural_pruned_tree_v1.pdf"), width = 16, height = 8)
plot(pruned_tree, uniform=TRUE, main="Classification Tree for LCA")
text(pruned_tree, use.n=F, all=T, cex=.7)
dev.off()

pdf(file.path(input_dir, "Solutions/KE 2022 (women with kids - final solution)", "Typing tool",  "rural_pruned_tree_v2.pdf"))
rpart.plot(pruned_tree, type = 5)
dev.off()

pdf(file.path(input_dir, "Solutions/KE 2022 (women with kids - final solution)", "Typing tool", "rural_pruned_tree_v3.pdf"))
rpart.plot(pruned_tree, type = 0, fallen.leaves = F, varlen = -15, tweak = 2, extra = 0, clip.facs = F, legend.x = NA)
dev.off()

# alternative veiw, needs refinement
# prp(pruned_tree, type=1, varlen=0, cex=1) # This layer makes the plot very hard to read, at least with current formatting. Cutting for now but we could adjust the arguments to make it look better


# Compare Performance on Training vs Test Dataset ------------------------------

#* Calculate performance in the training data set -----------------------------

PredictCART_train.r <- predict(pruned_tree, df.train.r, type="class")
df.train.r <- data.table(df.train.r, PredictCART_train.r)
(cm_train.r <- confusionMatrix(data=PredictCART_train.r, df.train.r$cluster_kSelected))
message("Training accuracy: ", round(cm_train.r$overall[1],2))

#* Calculate performance in the hold-out test data set ------------------------

PredictCART_test.r <- predict(pruned_tree, df.test.r, type="class")
df.test.r <- data.table(df.test.r, PredictCART_test.r)
(cm_test.r <- confusionMatrix(data=PredictCART_test.r, df.test.r$cluster_kSelected))
message("Test accuracy: ", round(cm_test.r$overall[1],2))

# #* Counts table ---------------------------------------------------------------
# 
# labels_least_to_most_vuln <- paste0(labels_least_to_most_vuln, " (", clusters_least_to_most_vuln, ")")
# 
# df.test.r[, predicted := factor(as.integer(PredictCART_test.r), labels = labels_least_to_most_vuln, levels = clusters_least_to_most_vuln, ordered = T)]
# df.test.r[, observed := factor(as.integer(cluster_kSelected), labels = labels_least_to_most_vuln, levels = clusters_least_to_most_vuln, ordered = T)]
# 
# counts_tbl <- table(df.test.r$predicted, 
#                     df.test.r$observed) # distribution of classes assigned to individuals of each class based on LCA assignment
# 
# #* Proportion table -----------------------------------------------------------
# #
# round(prop.table(counts_tbl, 2),3) # distribution of classes assigned to individuals of each class based on LCA assignment



#' ## NOT USED ##
#' -------------------------------------------------------------------------------
#' -------------------------------------------------------------------------------
#' ### Random forest to find important variables to force in the CART in to improve performance for specific segs
#'   #' look at the counts and proportion table showing where misclassification happens and identify variables that differentiate segments with higher misclassification
#' 
#' #Random Forest
#' library(randomForest)
#' library(randomForestExplainer)
#' 
#' # full variables from the vulnerability script
#' allvars <- names(vulnerability)
#' allvars <- setdiff(allvars, c("caseid", "v001", "v002", "v005", "v021", "v023", "v023", "v024", "v201", "wt", "hv021", "hv023", "hv022", "hv024", "hv025",
#'                               "hv205", "SWPER.SCORE.1", "SWPER.SCORE.2", "SWPER.SCORE.3", "SWPER.EMPOWERMENT.1", "SWPER.EMPOWERMENT.2", "SWPER.EMPOWERMENT.3",
#'                               "age.yrs", "med.nacnt", "hh.urban", "sshort", "early.birth.15")) 
#' 
#' # Between the Urban segments 1 and 2
#' urban1_2 <- dhs.seg.urban %>% filter(cluster_k4 %in% c(1,2))
#' urban1_2$cluster_k4 <- as.factor(urban1_2$cluster_k4)
#'  
#'   # First 100 vars
#'   urban.1_2_fit <- randomForest(as.formula(paste0("cluster_k4"," ~ ", paste0(allvars[c(1:100)], collapse = " + "))),
#'                               data=urban1_2, localImp=T, na.action = na.exclude)
#'   print(urban.1_2_fit) # view results
#'   print(importance(urban.1_2_fit))
#'   varImpPlot(urban.1_2_fit)
#'   
#'   importance<-measure_importance(urban.1_2_fit) # importance of each predictor
#'   plot_min_depth_distribution(urban.1_2_fit, mean_sample = "relevant_trees", k=15)
#'   plot_multi_way_importance(importance, size_measure = "no_of_nodes")
#' 
#'   # keep (don't keep those that have measurement error and/or sensitive)
#'   varsr1 <- c("ed.lev", "age.1stbrth", "husb.ed.grp", "internet.pastmonth", "age.1stcohab")
#'   
#'   # Next 100 vars
#'   urban.1_2_fit <- randomForest(as.formula(paste0("cluster_k4"," ~ ", paste0(allvars[c(101:200)], collapse = " + "))),
#'                                 data=urban1_2, localImp=T, na.action = na.exclude)
#'   print(urban.1_2_fit) # view results
#'   print(importance(urban.1_2_fit))
#'   varImpPlot(urban.1_2_fit)
#'   
#'   importance<-measure_importance(urban.1_2_fit) # importance of each predictor
#'   plot_min_depth_distribution(urban.1_2_fit, mean_sample = "relevant_trees", k=15)
#'   plot_multi_way_importance(importance, size_measure = "no_of_nodes")
#'   
#'   # keep (don't keep those that have measurement error and/or sensitive, take only one variable from related sets)
#'   varsr2 <- c("transport.mode1", "num.15up", "travtime.fact.cat2", "work.seasonal")
#'   
#'   # Last 100 vars
#'   urban.1_2_fit <- randomForest(as.formula(paste0("cluster_k4"," ~ ", paste0(allvars[c(201:length(allvars))], collapse = " + "))),
#'                                 data=urban1_2, localImp=T, na.action = na.exclude)
#'   print(urban.1_2_fit) # view results
#'   print(importance(urban.1_2_fit))
#'   varImpPlot(urban.1_2_fit)
#'   
#'   importance<-measure_importance(urban.1_2_fit) # importance of each predictor
#'   plot_min_depth_distribution(urban.1_2_fit, mean_sample = "relevant_trees", k=15)
#'   plot_multi_way_importance(importance, size_measure = "no_of_nodes")
#'   
#'   # keep (don't keep those that have measurement error and/or sensitive, take only one variable from related sets)
#'   varsr3 <- c("clean.fuel", "highestyearsedinHH.ur.under10", "w.nofinancial.acct", "hh.wat.time")
#' 
#'   
#'   
#' # Between the Rural segments 3 and 1
#' rural1_3 <- dhs.seg.rural %>% filter(cluster_k4 %in% c(1,3))
#' rural1_3$cluster_k3 <- as.factor(rural1_3$cluster_k3)
#' rural.1_3_fit <- randomForest(as.formula(paste0("cluster_k4"," ~ ", paste0(vuln.vars.u, collapse = " + "))),
#'                               data=rural1_3, localImp=T, na.action = na.exclude)
#' print(rural.1_3_fit) # view results
#' print(importance(rural.1_3_fit))
#' varImpPlot(rural.1_3_fit)
#' 
#' importance<-measure_importance(rural.1_3_fit) # importance of each predictor
#' plot_min_depth_distribution(rural.1_3_fit, mean_sample = "relevant_trees", k=15)
#' plot_multi_way_importance(importance, size_measure = "no_of_nodes")
#' 
#' 
#' # ---------------------------------------------
#' #### Save the data with the final segment variable
#' 
#' subset.u = "urban_round6_u5"
#' kclass.u = "cluster_k4"
#' subset.r = "rural_round6_u5"
#' kclass.r = "cluster_k4"
#' 
#' dhs.lca.u <- readRDS(file.path(input_dir, "Solutions/KE 2022 (women with kids - final solution)",  "LCA", subset.u, "dhs.seg.rds"))  # Use dhs.seg because it has the categorical labels
#' dhs.lca.u[, cluster_kSelected := as.factor(get(kclass.u))]
#' dhs.lca.u$segment <- paste0("U", dhs.lca.u$cluster_kSelected, "-K")
#' dhs.lca.r <- readRDS(file.path(input_dir, "Solutions/KE 2022 (women with kids - final solution)",  "LCA", subset.r, "dhs.seg.rds"))  # Use dhs.seg because it has the categorical labels
#' dhs.lca.r[, cluster_kSelected := as.factor(get(kclass.r))]
#' dhs.lca.r$segment <- paste0("R", dhs.lca.r$cluster_kSelected, "-K")
#' 
#' # Merge with the full dhs data object to pull in other variables
#' dhs.seg.urban <- dhs.lca.u %>%
#'   select(names(dhs.lca.u)[which(str_detect(names(dhs.lca.u), "cluster_|caseid|segment"))]) %>%
#'   left_join(dhs.full, by = "caseid")
#' dhs.seg.urban <- as.data.table(dhs.seg.urban)
#' 
#' dhs.seg.rural <- dhs.lca.r %>%
#'   select(names(dhs.lca.r)[which(str_detect(names(dhs.lca.r), "cluster_|caseid|segment"))]) %>%
#'   left_join(dhs.full, by = "caseid")
#' dhs.seg.rural <- as.data.table(dhs.seg.rural)
#' 
#' dhs.seg.comb <- dhs.seg.urban %>%
#'   bind_rows(dhs.seg.rural)
#' dhs.seg.comb.des <- svydesign(id=~v021, strata=~v023, weights=~wt, data=dhs.seg.comb)
#' 
#' # save
#' saveRDS(dhs.seg.comb, file.path(input_dir, "Data/Constructed data/dhs.seg.comb.rds"))
#' saveRDS(dhs.seg.comb.des, file.path(input_dir, "Data/Constructed data/dhs.seg.comb.des.rds"))
#' 
#' 

