
# Load libraries
pacman::p_load(rpart, rpart.plot, caret, randomForest)
pacman::p_load(readxl, rattle, RColorBrewer, foreign, ggplot2, dplyr, tidyr, data.table, survey, srvyr, haven)

# Read in data

df_seg <- read.csv("final_dataset_with_additional_indicators/meghalaya_segmentation_2024_indicators_added_final.csv")

# Pull in variables used in LCA
lca_spreadsheet <- read_excel("dataOut/exploratory_summary.xlsx", sheet="Sheet1")

############################## urban data ######################################
lca_vars <- lca_spreadsheet %>% filter(urban_lca_keep == 1) %>% 
  filter(factor != "wealth.index.ur" )
lca_vars <- names(df_seg)[(names(df_seg) %in% lca_vars$factor)]
urbseg <- df_seg %>% filter(v025 == "urban") %>% 
  dplyr::select(segment, lca_vars)


set.seed(1235)
formula <- as.formula(paste("segment ~",paste(lca_vars,collapse="+"),sep=""))

## Grow tree
tree_urban <- rpart(formula ,
                    data=urbseg, control=rpart.control(cp=0.0001, xval=5), method="class")
plotcp(tree_urban)

#prune tree
tree_urban_prune <-prune(tree_urban, cp=.001)
plotcp(tree_urban_prune) 
printcp(tree_urban_prune)

# Look at sensitivity/specificity/etc for pruned tree
PredictCART_train_urban <- predict(tree_urban_prune, data=urbseg, type="class")
confusionMatrix(data=PredictCART_train_urban, as.factor(urbseg$segment))

# Use the prp function to plot the tree with detailed information
prp(tree_urban_prune,
    type = 5,           # Type 2 splits the nodes to make the tree more readable
    fallen.leaves = FALSE, # Place leaves at the bottom of the tree
    varlen = 0,         # Do not abbreviate variable names
    faclen = 0,  # Do not abbreviate factor levels
    facsep = "\n",  
    shadow.col = "gray", # Adds a shadow to the nodes for better visualization
    box.palette = "RdYlGn", # Color palette for the nodes
    cex = NULL,          # Adjust the text size
    legend.x = NA       # Remove the legend
)



############################## rural data ######################################
lca_vars <- lca_spreadsheet %>% filter(rural_lca_keep == 1)  %>% 
  filter(factor != "wealth.index.ur") 

lca_vars <- names(df_seg)[(names(df_seg) %in% lca_vars$factor)]
rurseg <- df_seg %>% filter(v025 == "rural") %>% 
  dplyr::select(segment, lca_vars)

set.seed(1235)
formula <- as.formula(paste("segment ~",paste(lca_vars,collapse="+"),sep=""))

loss_1 <- 1
loss_2 <- 1.2
loss_3 <- 1.5
loss_4 <- 1

loss_matrix <- matrix(c(
  0, loss_1, loss_1, loss_1,  # Cost of predicting each class when the actual class is 1
  loss_2, 0, loss_2, loss_2,  # Cost of predicting each class when the actual class is 2
  loss_3, loss_3, 0, loss_3,  # Cost of predicting each class when the actual class is 3
  loss_4, loss_4, loss_4, 0   # Cost of predicting each class when the actual class is 4
), nrow = 4, byrow = TRUE)

## Grow tree
tree_rural <- rpart(formula ,
                    data=rurseg, control=rpart.control(cp=0.001, xval=10, minsplit = 30), method="class",
                    parms = list(split = "information", loss = loss_matrix))

plotcp(tree_rural)

# split = "gini", 
#prune tree
tree_rural_prune <-prune(tree_rural, cp=.003)
plotcp(tree_rural_prune)

# Look at sensitivity/specificity/etc for pruned tree
PredictCART_train_rural <- predict(tree_rural_prune, data=rurseg, type="class")
confusionMatrix(data=PredictCART_train_rural, as.factor(rurseg$segment))

# Use the prp function to plot the tree with detailed information
prp(tree_rural_prune,
    xflip = TRUE,
    type = 5,           # Type 2 splits the nodes to make the tree more readable
    fallen.leaves = FALSE, # Place leaves at the bottom of the tree
    varlen = 0,         # Do not abbreviate variable names
    faclen = 0,  # Do not abbreviate factor levels
    facsep = "\n",  
    shadow.col = "gray", # Adds a shadow to the nodes for better visualization
    box.palette = "RdYlGn", # Color palette for the nodes
    cex = .2,          # Adjust the text size
    legend.x = NA, # Remove the legend
    branch = 1,
    tweak = 2.2,
    space = 0,            # Increase space between nodes
    gap = 0,              # Increase gap between leaves
    yspace = 1
)






