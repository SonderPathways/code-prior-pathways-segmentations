
################################################################################
# GENERATE EXPLORATORY DATA ANALYSIS
################################################################################


# ###################################
# RUN SETUP
# outcomes.list <- c("wom.nohlthck")
# # measure = "num.under5"
# measure = "med.index"
# df = outcomes_vulnerability
# gen_plots = TRUE
# plot_path = exploratory_plots


###################################
# DEFINE FUNCTION
###################################

gen_eda <- function(df=NULL, outcomes.list=NULL, measure=NULL, plot_path){

  # DEFINE CONFIDENCE LEVEL
  clevel = .05

  # SET PLOTTING FLAG
  gen_plots = TRUE

  # GET METADATA FOR PLOTTING
  outcome_metadata <- read_excel(pathways_workbook_path, sheet = 'outcomes')
  vulnerability_metadata <-  read_excel(pathways_workbook_path, sheet = 'vulnerabilities')

  # # SET ORDER OF STRATA VARIABLE FOR PLOTTING
  df$strata <- factor(df$strata, levels=c("urban", "rural"))


  ###################################
  # SET EXPLANATORY VARIABLE
  df$var <- df %>% pull(eval(parse(text=measure)))


  # DEFINE DATA TYPE FOR EXPLANATORY VARIABLE
  exp.binary <- all(na.omit(df$var) %in% 0:1) | all(na.omit(df$var) %in% c("yes", "no"))
  exp.numeric <- is.numeric(df$var) & exp.binary == FALSE
  exp.character <- (is.character(df$var) | is.factor(df$var)) & exp.binary == FALSE


  # RECODE BINARY VARIABLES AS YES NO
  if (exp.binary == TRUE ){

    df <- df %>%
      mutate(var = case_when(
                             is.na(var) ~ NA,
                             var==1 ~ "yes",
                             var==0 ~ "no",
                             var== "yes" ~ "yes",
                             var == "no" ~ "no"))

  }

  if (exp.character == TRUE){

    df$var <- as.character(df$var)

  }


  # # SPECIAL CASE FOR MED.INDEX - NUMERICAL VALUE BUT WANT TO TREAT AS CATEGORICAL
  # if (measure == "med.index"){
  #
  #   df$var <- as.character(df$var)
  #
  # }


  ###################################


  eda_plots_file = paste0(plot_path, "eda_", measure, ".pdf")
  dir.create(dirname(eda_plots_file), showWarnings = F, recursive = T)
  ###################################
  # GENERATE PDF OF OUTPUTS
  cairo_pdf(filename = eda_plots_file,
            width = 15,
            height = 10,
            onefile=T)


  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  text(5, 8, paste0(project_name, " | EDA for: ", measure), cex = 2)


  regouts <- data.frame()
  for (out in outcomes.list){
    print(paste(out, " ~ ", measure))

    # try({


    ###################################
    # DEFINE OUTCOME VARAIBLE AND DATA TYPE FOR OUTCOME
    df$outcome <- df %>% pull(eval(parse(text=out)))

    out.binary <- all(na.omit(df$outcome) %in% 0:1) | all(na.omit(tolower(df$outcome)) %in% c("yes", "no"))
    out.numeric <- is.numeric(df$outcome) & out.binary == FALSE
    out.character <- (is.character(df$outcome) | is.factor(df$outcome)) & exp.binary == FALSE


    ###################################
    # SURVEY WEIGHTED BIVARIATE REGRESSION


    df_exp_plot <- data.frame()
    for (stratum in unique(df$strata)){


      df1 <- df %>% filter(strata == stratum) %>%
        dplyr::select(caseid, data_id_var, data_strata_var, wt, strata, outcome, var)


      ###################################
      #


      if (out.binary == TRUE){


        df1$outcome <- as.factor(df1$outcome)
        df.design <- svydesign(id=~eval(parse(text = data_id_var)), strata=~eval(parse(text = data_strata_var)), weights=~wt, data=df1, nest=TRUE)
        res_glm <- svyglm(outcome ~ var, family=quasibinomial, design=df.design, data=df1)
        exp = FALSE
        # xlab = "Odds Ratio Estimates and 95% Confidence Intervals"
        xlab = "Predicted Probabilities and 95% Confidence Intervals"


      } else {


        df.design <- svydesign(id=~eval(parse(text = data_id_var)), strata=~eval(parse(text = data_strata_var)), weights=~wt, data=df1, nest=TRUE)
        res_glm <- svyglm(outcome ~ var, family=gaussian(link ="identity"), design=df.design, data=df1)
        exp = FALSE
        xlab = "Predicted Probabilities and 95% Confidence Intervals"


      }


      ###################################
      # INSERT CODE TO FIX MODEL ERROR
      ###################################


      if (length(res_glm$coefficients[exp(res_glm$coefficients) > 100])>0  |
          length(res_glm$coefficients[exp(res_glm$coefficients) < -100])>0){

        coefname <- names(res_glm$coefficients)[exp(res_glm$coefficients) > 100 | exp(res_glm$coefficients) < -100]
        res_glm$coefficients[exp(res_glm$coefficients) > 100 | exp(res_glm$coefficients) < -100] <- 0
        status = paste0(stratum, " model did not fit - level: ", coefname, " has too few observations\n")
        print(status)

      } else {

        status = paste0(stratum, " model successfully fit")

      }

      assign(paste0("status_", stratum), status)


      ###################################
      # PREP MODEL OUTPUTS FOR PLOTTING
      df_exp_plot <- rbind(df_exp_plot, df1)

      if (exp.numeric == TRUE){

        pred_prob <- data.frame(var = unique(na.omit(df$var)),
                                p.value = tidy(res_glm)$p.value[[2]])

      } else {

        pred_prob <- data.frame(var = res_glm$xlevels[[1]],
                                p.value = tidy(res_glm)$p.value)

      }

      probabilities <- predict(res_glm, pred_prob, type = "response", se.fit=T, na.action = na.omit)
      prob1 <- as.data.frame(probabilities)
      prob1$lower <- prob1$response - (1.96*prob1$SE)
      prob1$upper <- prob1$response + (1.96*prob1$SE)
      pred_prob <- cbind(pred_prob, prob1) %>%
        mutate(model = factor(stratum, levels=c("urban", "rural")))


      # RENAME FOR MODEL PLOTTING
      assign(paste0("glm_", stratum), pred_prob)



    }


    ###################################
    # GET LABELS
    outcome_label <- outcome_metadata[outcome_metadata$outcome_variable==out, "short_name"][[1]]
    measure_label <- vulnerability_metadata[vulnerability_metadata$vulnerability_variable==measure, "short_name"][[1]]


    ###################################
    # PLOT REGRESSION COEFFICIENT ESTIMATES
    if (gen_plots==TRUE){


      glm_models <- rbind(glm_urban, glm_rural)

      urbanDNF <- grepl(" model did not fit",status_urban)
      ruralDNF <- grepl(" model did not fit",status_rural)
      plot.color <- c(ifelse(urbanDNF,"transparent","#D95F02"), ifelse(ruralDNF,"transparent","#1B9E77"))
      plot_text_urb <- ifelse(any(urbanDNF),paste(status_urban,collapse = ""),"")
      plot_text_rur <- ifelse(any(ruralDNF),paste(status_rural,collapse = ""),"")
      plot_text <- paste0(plot_text_urb,"\n",plot_text_rur)


      #####################
      # # plot1 <- modelplot(glm_models, conf_level = 1-clevel, coef_omit = "Interc", coef_rename = TRUE, exponentiate = exp) +
      # plot1 <- modelplot(glm_models,
      #                    conf_level = 1-clevel,
      #                    coef_rename = TRUE,
      #                    exponentiate = exp
      # ) +
      #   geom_vline(xintercept = ifelse(exp == TRUE, 1, 0)) +
      #   theme_bw() +
      #   theme(legend.position = "bottom") +
      #   aes(color = strata) +
      #   scale_color_manual(values=plot.color) +
      #   # scale_fill_brewer(palette="Dark2") +
      #   xlab(xlab) +
      #   annotate("text",  x=Inf, y = Inf, label = plot_text, vjust=1, hjust=1)+
      #   # scale_y_discrete(labels = function(x) str_wrap(x, width = 15), guide = guide_axis(check.overlap = TRUE)) +
      #   ggtitle(paste0(out, " ~ ", measure))
      # # print(plot1)
      #####################


      # PLOT MODEL PROBABILITIES
      plot1 <- glm_models %>%
        ggplot(aes(
          y = var,
          x = response,
          xmin = lower,
          xmax = upper,
          color = model
        )) +
        ggdist::geom_pointinterval(
          position = "dodge",
          # interval_size_range = c(1, 3),
          fatten_point = 3) +
        geom_vline(xintercept = ifelse(exp == TRUE, 1, 0)) +
        theme_bw() +
        theme(legend.position = "bottom") +
        # aes(color = strata) +
        scale_color_manual(values=plot.color) +
        ylab("") +
        xlab(xlab) +
        annotate("text",  x=Inf, y = Inf, label = plot_text, vjust=1, hjust=1)+
        ggtitle(paste0(outcome_label, " ~ ", measure_label))



      # CREATE TABLE OF MODEL OUTPUT
      plot2 <- glm_models %>%
        mutate(sig = ifelse(p.value <= 0.05, "*", ""),
               statistic = paste0(round(response, 2), sig, "  [", round(lower, 2), ", ", round(upper, 2), "] ")) %>%
        reshape2::dcast(var ~ model, value.var = "statistic") %>%
        arrange(desc(row_number())) %>%
        tail(10)


      # plot2 <- modelsummary(glm_models,
      #                       fmt = NULL,
      #                       # title = paste(status_urban, status_rural),
      #                       # shape = term ~ model,
      #                       # coef_omit = "Interc",
      #                       coef_rename = TRUE,
      #                       # exponentiate = exp,
      #                       estimate = "{round(exp(estimate) / (1 + exp(estimate)), 3)}{stars}",
      #                       # statistic = 'conf.int',
      #                       statistic = "[{round(exp(conf.low) / (1 + exp(conf.low)), 3)}, {round(exp(conf.high) / (1 + exp(conf.high)), 3)}]",
      #                       gof_omit = 'R2|R2 Adj.|AIC|BIC|Log.Lik.|RMSE',
      #                       output = "huxtable") %>%
      #   filter(row_number() > 1) %>%
      #   setNames(c("v", "urban", "rural")) %>%
      #   mutate(v = str_replace_all(v, c("var " = "", "\\[" = "", "\\]" = ""))) %>%
      #   setNames(c("", "urban", "rural"))


      ###################################
      # BAR CHART TO SHOW EXPLANATORY VARIABLE DISTRIBUTION
      if (exp.numeric == TRUE) {


        # STRATIFIED SAMPLE DISTRIBUTION OF EXPLANATORY VARIABLE
        df_plot <- df_exp_plot %>%
          # filter(!is.na(outcome)) %>%
          mutate(is_na = ifelse(is.na(var), "yes", "no"),
                 outcome = ifelse(is.na(var), -1, var)) %>%
          group_by(strata, var, is_na) %>%
          mutate(count=n(),
                 sum_wt=sum(wt)) %>%
          group_by(strata) %>%
          mutate(total=n(),
                 prop=count/total,
                 total_wt=sum(wt),
                 prop_wt=sum_wt/total_wt,
                 sample_size = sum(ifelse(is_na == "yes", 0, count)),
                 strata = paste0(strata, " sample size = ", sample_size))


        plot3 <- df_plot %>%
          ggplot(aes(x=var, y=prop_wt, fill=strata)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=paste0(count, " | ", sprintf("%1.1f%%", prop_wt*100))), hjust = 1, size = 2.5, fontface = "bold") +
          geom_point(aes(y=prop), shape=5, size=2, color="darkred") +
          facet_wrap(~strata, ncol = 1) +
          theme_bw() +
          coord_flip() +
          theme(plot.title = element_text(size=10)) +
          theme(legend.text=element_text(size=6),  legend.title=element_text(size=8)) +
          theme(axis.title.y = element_text(size=8)) +
          theme(plot.title = element_text(size=10)) +
          theme(legend.position="none") +
          # theme(aspect.ratio = .60) +
          xlab("") +
          ylab("") +
          # ylim(0, 1) +
          scale_fill_brewer(palette="Dark2") +
          ggtitle(paste0("Stratified Population Weighted Distribution of Explanatory Variable: ", measure_label))
        # print(plot3)


      } else  if (exp.character == TRUE | exp.binary == TRUE){


        # STRATIFIED SAMPLE DISTRIBUTION OF EXPLANATORY VARIABLE
        df_plot <- df_exp_plot %>%
          # filter(!is.na(outcome)) %>%
          mutate(var = ifelse(is.na(var), "*NA", var)) %>%
          group_by(strata, var) %>%
          mutate(count=n(),
                 sum_wt=sum(wt)) %>%
          group_by(strata) %>%
          mutate(total=n(),
                 prop=count/total,
                 total_wt=sum(wt),
                 prop_wt=sum_wt/total_wt,
                 sample_size = ifelse(var == "*NA", 0, count)) %>%
          dplyr::select(strata, var, count, sum_wt, total, prop, total_wt, prop_wt, sample_size) %>%
          distinct() %>%
          group_by(strata) %>%
          mutate(sample_size = sum(sample_size),
                 strata = paste0(strata, " sample size = ", sample_size))


        plot3 <- df_plot %>%
          ggplot(aes(x=var, y=prop_wt, fill=strata)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=paste0(count, " | ", sprintf("%1.1f%%", prop_wt*100))), hjust = 1, size = 2.5, fontface = "bold") +
          geom_point(aes(y=prop), shape=5, size=2, color="darkred") +
          facet_wrap(~strata, ncol = 1) +
          theme_bw() +
          coord_flip() +
          theme(plot.title = element_text(size=10)) +
          theme(legend.text=element_text(size=6),  legend.title=element_text(size=8)) +
          theme(axis.title.y = element_text(size=8)) +
          theme(plot.title = element_text(size=10)) +
          theme(legend.position="none") +
          # theme(aspect.ratio = .60) +
          xlab("") +
          ylab("") +
          ylim(0, 1) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 15), guide = guide_axis(check.overlap = TRUE)) +
          scale_fill_brewer(palette="Dark2") +
          ggtitle(paste0("Stratified Population Weighted Distribution of Explanatory Variable: ", measure_label))
        # print(plot3)


      }


      # BAR CHART TO SHOW OUTCOME VARIABLE DISTRIBUTION
      if (out.binary == TRUE){

        df_plot <- df_exp_plot %>%
          mutate(outcome = case_when(outcome == 1 ~ "yes",
                                     outcome == 0 ~ "no",
                                     is.na(outcome) ~ "*NA")) %>%
          group_by(strata, outcome) %>%
          mutate(count=n(),
                 sum_wt=sum(wt)) %>%
          group_by(strata) %>%
          mutate(total=n(),
                 prop=count/total,
                 total_wt=sum(wt),
                 prop_wt=sum_wt/total_wt,
                 sample_size = ifelse(outcome == "*NA", 0, count)) %>%
          dplyr::select(strata, outcome, count, sum_wt, total, prop, total_wt, prop_wt, sample_size) %>%
          distinct() %>%
          group_by(strata) %>%
          mutate(sample_size = sum(sample_size),
                 strata = paste0(strata, " sample size = ", sample_size))

        plot4 <- df_plot %>%
          ggplot(aes(x=outcome, y=prop_wt, fill=strata)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=paste0(count, " | ", sprintf("%1.1f%%", prop_wt*100))), hjust = 1, size = 2.5, fontface = "bold") +
          geom_point(aes(y=prop), shape=5, size=2, color="darkred") +
          facet_wrap(~strata, ncol = 1) +
          theme_bw() +
          coord_flip() +
          theme(plot.title = element_text(size=10)) +
          theme(legend.text=element_text(size=6),  legend.title=element_text(size=8)) +
          theme(axis.title.y = element_text(size=8)) +
          theme(plot.title = element_text(size=10)) +
          theme(legend.position="none") +
          # theme(aspect.ratio = .20) +
          xlab("") +
          ylab("") +
          ylim(0, 1) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 15), guide = guide_axis(check.overlap = TRUE)) +
          scale_fill_brewer(palette="Dark2") +
          ggtitle(paste0("Stratified Population Weighted Distribution of Outcome Variable: ", outcome_label))
        # print(plot4)


      } else if (out.numeric == TRUE){


        df_plot <- df_exp_plot %>%
          # mutate(outcome = as.numeric(as.character(outcome))) %>%
          mutate(is_na = ifelse(is.na(outcome), "yes", "no"),
                 outcome = ifelse(is.na(outcome), -1, outcome)) %>%
          group_by(strata, outcome, is_na) %>%
          mutate(count=n(),
                 sum_wt=sum(wt)) %>%
          group_by(strata) %>%
          mutate(total=n(),
                 prop=count/total,
                 total_wt=sum(wt),
                 prop_wt=sum_wt/total_wt,
                 sample_size = ifelse(is_na == "yes", 0, count)) %>%
          dplyr::select(strata, outcome, is_na, count, sum_wt, total, prop, total_wt, prop_wt, sample_size) %>%
          distinct() %>%
          group_by(strata) %>%
          mutate(sample_size = sum(sample_size),
                 strata = paste0(strata, " sample size = ", sample_size))


        plot4 <- df_plot %>%
          ggplot(aes(x=outcome, y=prop_wt, fill=strata)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=paste0(count, " | ", sprintf("%1.1f%%", prop_wt*100))), hjust = 1, size = 2.5, fontface = "bold") +
          geom_point(aes(y=prop), shape=5, size=2, color="darkred") +
          facet_wrap(~strata, ncol = 1) +
          theme_bw() +
          coord_flip() +
          theme(plot.title = element_text(size=10)) +
          theme(legend.text=element_text(size=6),  legend.title=element_text(size=8)) +
          theme(axis.title.y = element_text(size=8)) +
          theme(plot.title = element_text(size=10)) +
          theme(legend.position="none") +
          # theme(aspect.ratio = .60) +
          xlab("") +
          ylab("") +
          # ylim(0, 1) +
          scale_fill_brewer(palette="Dark2") +
          ggtitle(paste0("Stratified Population Weighted Distribution of Explanatory Variable: ", outcome_label))
        # print(plot4)


      }




      ###################################
      # PLOT TOGETHER
      grid.arrange(plot1, arrangeGrob(plot4, plot3, tableGrob(plot2, rows = NULL)),
                   ncol=2
                   # widths = c(2,1),
      )

    }

    # }, silent = TRUE)

  }

  ###################################
  dev.off()
  graphics.off()

  return(regouts)

}
