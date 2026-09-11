


################################################################################
# GENERATE PCA OUTPUT VISUALS
################################################################################

###################################
# RUN SETUP
# df <- vulnerability_input
# pca_varlist <- pca_strata_input


###################################
# DEFINE FUNCTION
###################################


gen_pca_output <- function(df=NULL, stratum=NULL, pca_varlist=NULL, file_name=NULL){

  dir.create(dirname(file_name), showWarnings = F, recursive = T)
  ###################################
  # GENERATE PDF OF OUTPUTS
  cairo_pdf(filename = file_name,
            width = 15,
            height = 10,
            onefile=T)


  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  text(5, 8, paste0("Principal Component Analysis | ", stratum), cex = 2)

  for (dom in unique(pca_varlist$domain)){
    print(paste(stratum, dom))


    plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
    text(5, 8, paste0(dom), cex = 2)

    # try({

      ###################################
      # SET UP DATASETS
      pca_varlist1 <- pca_varlist %>%
        dplyr::filter(domain==dom)

      vulnerability_vars <- names(df)[(names(df) %in% pca_varlist1$vulnerability_variable)]


      df1 <- df %>%
        dplyr::select(all_of(svy_id_var), all_of(svy_strata_var), wt, all_of(vulnerability_vars)) %>%
        na.omit() %>%
        data.table()


      df2 <- df1 %>%
        dplyr::select(all_of(svy_id_var), all_of(svy_strata_var), wt, all_of(vulnerability_vars)) %>%
        data.frame()


      ### Generate dummy variables for all categorical variables
      ## for categorical variables with two levels -- just treat as integer
      for(k in 1:length(vulnerability_vars)){
        if(length(unique(df2[,vulnerability_vars[k]]))==2){
          df2[,vulnerability_vars[k]] <- as.numeric(as.factor(df2[,vulnerability_vars[k]]))
        }
      }

      cat.var.names <- vulnerability_vars[sapply(df2[,vulnerability_vars], is.character)]
      if(length(cat.var.names)>=1){
        df3 <- dummy_cols(df2, select_columns = cat.var.names)
        df3 <- df3[,!names(df3) %in% cat.var.names]
      } else{ df3 <- df2}


      #### Correcting categorical variable names
      names(df3) <- gsub(x = names(df3),pattern = " ",replacement = "_")
      names(df3)  <- gsub(x = names(df3) ,pattern = "'",replacement = "")
      names(df3)  <- gsub(x = names(df3) ,pattern = "-",replacement = "")
      names(df3)  <- gsub(x = names(df3) ,pattern = "/",replacement = "_")
      names(df3)  <- gsub(x = names(df3) ,pattern = "\\+",replacement = "p")
      names(df3)  <- gsub(x = names(df3) ,pattern = "<",replacement = "lt")
      names(df3)  <- gsub(x = names(df3) ,pattern = ">",replacement = "gt")

      vulnerability_vars2 <- names(df3)[!names(df3) %in% c(svy_id_var, svy_strata_var, "wt")]


      ###################################


      # DEFINE SURVEY OBJECT
      if (config::get("use_svy_design") == FALSE){

        df.design <- svydesign(id=~1, strata=NULL, weights=~wt, data=df1)

      } else {

        df.design <- svydesign(id=~eval(parse(text = svy_id_var)), strata=~eval(parse(text = svy_strata_var)), weights=~wt, data=df1, nest=TRUE)

      }


      formula <- as.formula(paste0("~", paste(vulnerability_vars2, collapse = "+")))
      pca_res <- svyprcomp(formula, design = df.design, center = TRUE, scale. = TRUE, scores = FALSE)


    # }, silent = TRUE)


      ###################################

      ### Data frame for color
      color.names <- data.frame("var.name"=character(),"pca.name"=character())
      for(j in 1:length(vulnerability_vars)){
        vul.name <- vulnerability_vars[j]

        if (is.character(df1[[vul.name]])){
          pca.name <- paste0(vul.name,'_',unique(df1[[vul.name]]))
          pca.name <- gsub(x = pca.name,pattern = " ",replacement = "_")
          pca.name <- gsub(x = pca.name ,pattern = "'",replacement = "")
          pca.name  <- gsub(x = pca.name ,pattern = "-",replacement = "")
          pca.name <- gsub(x = pca.name ,pattern = "/",replacement = "_")
          pca.name  <- gsub(x = pca.name ,pattern = "\\+",replacement = "p")
          pca.name  <- gsub(x = pca.name ,pattern = "<",replacement = "lt")
          pca.name  <- gsub(x = pca.name ,pattern = ">",replacement = "gt")

        } else {
          pca.name <- vul.name
        }
        add.rows <- cbind(vul.name,pca.name)
        color.names <- rbind(color.names,add.rows)
      }

      pca.names <- as.data.frame(rownames(pca_res$rotation))
      names(pca.names) <- "pca.name"
      color.names <- left_join(pca.names,color.names,by = 'pca.name')

      ###################################
      # HOW MANY PCS EXPLAIN 60% VARIANCE?
      eigenvalues <- get_eig(pca_res) %>%
        dplyr::filter(cumulative.variance.percent < 60)

      n_pcs = nrow(eigenvalues) + 1

      ###################################
      # SCREE PLOT
      prop <- (pca_res$sdev)^2/sum((pca_res$sdev)^2)
      cum_prop <- cumsum(prop)
      cum_prop_label <- paste0(round(100*cum_prop,2),'%')
      plot_dat <- data.frame(
        pc=paste0('PC',1:length(cum_prop)),
        sd=pca_res$sdev,
        prop=prop,
        cum_prop=cum_prop,
        label=cum_prop_label
      ) %>%
        mutate(threshold = factor(ifelse(row_number() <= n_pcs, 1, 0), levels=c(1,0)),
               pc_order = as.integer(str_replace(pc, "PC", "")))


      plot1 <- ggplot(plot_dat, aes(x=reorder(pc, -pc_order), y=cum_prop, fill=threshold)) +
        geom_bar(stat="identity") +
        theme_bw() +
        geom_text(aes(label = label), hjust = 1, size = 2.5) +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(legend.position="none") +
        coord_flip() +
        xlab('PC') +
        ylab("Cumulative Proportion") +
        scale_fill_brewer(palette="Dark2") +
        ggtitle(paste0("Cumulative Proportion of Variance Explained | ", dom, " | ", stratum))
      # print(plot1)


      # QUALITY OF VARIABLE REPRSENTATION IN EACH PC
      plot2 <- as.data.frame(get_pca_var(pca_res)$cos2) %>%
        mutate(factor = rownames(.)) %>%
        reshape2::melt(id.var="factor", variable.name="pc", value.name = "value") %>%
        mutate(pc = str_replace(pc, "Dim.", "PC"),
               pc_order = as.integer(str_replace(pc, "PC", ""))) %>%
        ggplot(aes(x = reorder(pc, pc_order), y = factor, fill = value)) +
        geom_tile(color = "white",
                  lwd = 1.5,
                  linetype = 1) +
        coord_fixed() +
        scale_fill_gradient2(low = "white", high = "steelblue", name = "Factor Rep") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) +
        # scale_y_discrete(labels = function(x) str_wrap(x, width = 15), guide = guide_axis(check.overlap = TRUE)) +
        xlab("Principal Component") +
        ylab("") +
        ggtitle(paste0("Variable Representation across PCs"))
      # print(plot2)


      grid.arrange(plot1, plot2, ncol=2, widths = c(1,2))


      ###################################
      # BIPLOT & COS2 BAR PLOT

      try({

        for (i in seq(n_pcs-1)){

          pc = c(i, i+1)

          df_plot <- data.frame(
            var_level = rownames(pca_res$rotation),
            rotation = pca_res$rotation
          )


          plot1 <- fviz_pca_var(pca_res,
                                axes = pc,
                                repel = TRUE,
                                #habillage = TRUE,
                                col.var = color.names$vul.name,
                                ggtheme = theme_bw()) +
            theme(legend.position = "")
          # print(plot1)

          title =  paste0("Variable Representation for pc: ", i, ",", i+1)
          plot2 <- fviz_cos2(pca_res,
                             choice = "var",
                             axes = pc,
                             sort.val = "asc",
                             #fill = color.names$vul.name,
                             title = title,
                             ggtheme = theme_bw()) +
            coord_flip()
          # print(plot2)

          grid.arrange(plot1, plot2, ncol=2)

        }

      }, silent = TRUE)

  }

  ###################################
  dev.off()
  graphics.off()


}

