

################################################################################
# GENERATE TYPING TOOL OUTPUT
################################################################################
#
# df=df_sol
# stratum=stratum
# tt_vars=tt_vars
# final_seg=final_seg



fun_gen_typing_tool <- function(df=NULL, stratum=NULL, tt_vars=NULL, final_seg=NULL){


  # SUBSET TO TT VARS
  input <- subset(df, select=c(tt_vars))


  ###################################
  # DATA PREP
  # CODE TO CONVERT BINARY VARIABLES TO YES/NO (BEST PRACTICE IS TO CODE THEM INITIALLY AS YES/NO)
  for (col in names(input)){

    if ((all(c(0, 1) %in% na.omit(input[[col]])) == TRUE) & (all(na.omit(input[[col]]) %in% c(0, 1)) == TRUE) |
        (all(c("Yes", "No") %in% str_to_title(na.omit(input[[col]]))) == TRUE) & (all(str_to_title(na.omit(input[[col]])) %in% c("Yes", "No")) == TRUE)){

      print(col)
      input[[col]] <- ifelse(input[[col]] == 1, "Yes", input[[col]])
      input[[col]] <- ifelse(input[[col]] == 0, "No", input[[col]])
      input[[col]] <- ifelse(input[[col]] == "yes", "Yes", input[[col]])
      input[[col]] <- ifelse(input[[col]] == "no", "No", input[[col]])
    }
  }


  # GROW TREE
  tree <- rpart(n_class ~ .,
                data=input,
                control=rpart.control(cp=0.0001, xval=5), method="class")


  # CONFUSION MATRIX - UNPRUNED
  confusion.matrix = confusionMatrix(data = predict(tree,
                                                    data=input,
                                                    type="class"),
                                     as.factor(input$n_class))
  print(confusion.matrix)
  tbl_cm_1 <- data.frame(confusion.matrix$byClass)
  tbl_cm_1 <- cbind(variable = rownames(tbl_cm_1), tbl_cm_1)
  rownames(tbl_cm_1) <- NULL


  # VARIABLE IMPORTANCE
  tbl_vi_1 <- data.frame(tree$variable.importance)
  tbl_vi_1
  tbl_vi_1 <- cbind(variable = rownames(tbl_vi_1), tbl_vi_1)
  rownames(tbl_vi_1) <- NULL

  # VIEW TREE
  prp(tree, type=3, varlen=0, cex=.2)

  # PRINT COMPLEXITY PARAMTER TABLE & PLOT
  plotcp(tree); printcp(tree)

  tbl_cp_1 <- data.frame(printcp(tree))

  optimal_xerror <- tbl_cp_1 %>%
    dplyr::slice_min(xerror, n=1, with_ties=FALSE) %>%
    dplyr::mutate(optimal_xerror = xerror + xstd) %>%
    pull(optimal_xerror)

  optimal_cp <- tbl_cp_1 %>%
    dplyr::filter(xerror == xerror[which.min(abs(xerror - optimal_xerror))]) %>%
    dplyr::filter(nsplit == min(nsplit))

  cp <- optimal_cp %>% pull(CP)
  size <- optimal_cp %>% pull(nsplit)


  ###################################
  # PRUNE TREE
  if (stratum == "rural"){
    tree_prune <- prune(tree, cp=.004)
  } else {
    tree_prune <- prune(tree, cp=.006)
  }


  plotcp(tree_prune); printcp(tree_prune)

  PredictCART_train <- predict(tree_prune, data=input, type="class")

  confusion.matrix <- confusionMatrix(data=PredictCART_train,
                                      as.factor(input$n_class))
  print(confusion.matrix)
  tbl_cm_2 <- data.frame(confusion.matrix$byClass)
  tbl_cm_2 <- cbind(variable = rownames(tbl_cm_2), tbl_cm_2)
  rownames(tbl_cm_2) <- NULL

  # VIEW TREE
  prp(tree_prune, type=4, varlen=0, cex=.5)


   saveRDS(tree_prune, "rural_tree_prune_model.rds")
   saveRDS(tree_prune, "urban_tree_prune_model.rds")




}
