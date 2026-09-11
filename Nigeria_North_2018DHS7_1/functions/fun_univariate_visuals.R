
################################################################################
# GENERATE UNIVARIATE PLOTS
################################################################################


# ###################################
# RUN SETUP
# df <- vulnerability
# metadata <- vulnerability_sheet
# plot_path = univariate_plots_file


###################################
# DEFINE FUNCTION
###################################

gen_univariate_plots <- function(df=NULL, metadata=NULL, plot_path=NULL){


  dir.create(dirname(plot_path), showWarnings = F, recursive = T)
  ###################################
  # GENERATE PDF OF OUTPUTS
  cairo_pdf(filename = plot_path,
            width = 15,
            height = 10,
            onefile=T)

  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  text(5, 8, paste0("Univariate Variable Distributions"), cex = 2)

  # for (group in unique(metadata$grouping)){
  #
  #   plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  #   text(5, 8, paste0("Variables | ", group), cex = 2)
  #
  #   metadata1 <- metadata %>%
  #     filter(grouping == group) %>%
  #     arrange(grouping, indicator)

    # var_list <- unique(metadata1$indicator)
  var_list <- sort(names(df)[(names(df) %in% metadata$indicator)])

  plot_list <- list()

  for (i in 1:length(var_list)){

      try({

        var = var_list[i]
        df$variable = var
        df$value = df %>% pull(eval(parse(text=var)))

        binary.var <- all(na.omit(df$value) %in% 0:1)


        # RECODE BINARY VARIABLES AS YES NO
        if (binary.var == TRUE){

          df_plot <- df %>%
            mutate_if(is.factor, as.character) %>%
            mutate(value_plot = case_when(value==1 ~ "yes",
                                          value==0 ~ "no"))

        } else {

          df_plot <- df %>%
            mutate_if(is.factor, as.character) %>%
            mutate(value_plot = value)

        }

        # # GET SHORT_NAME FOR PLOT LABEL
        # df_plot <- df_plot %>%
        #   base::merge(metadata, by.x=c("variable"), by.y=c("indicator"))
        #
        # plot_label = df_plot$short_name[1]
        plot_label = metadata[metadata$indicator == var, c("short_name")][[1]]


        # LOGIC FOR HANDLING NAS
        if (is.numeric(df_plot$value_plot) == TRUE){


          df_plot <- df_plot %>%
            dplyr::mutate(is_na = ifelse(is.na(value_plot), "yes", "no"),
                          value_plot = ifelse(is.na(value_plot), -1, value_plot)) %>%
            dplyr::group_by(variable, value_plot, is_na) %>%
            dplyr::summarize(count=n()) %>%
            dplyr::group_by(variable) %>%
            dplyr::mutate(total=sum(count),
                          prop=count/total)


          plot <- df_plot %>%
            ggplot(aes(x=value_plot, y=prop, fill=is_na)) +
            geom_bar(stat="identity") +
            geom_text(aes(label=paste0(count, " | ", sprintf("%1.1f%%", prop*100))), hjust = 1, size = 3.5, fontface = "bold") +
            theme_bw() +
            coord_flip() +
            ggtitle(paste0(plot_label)) +
            theme(plot.title = element_text(size=10)) +
            theme(legend.text=element_text(size=6),  legend.title=element_text(size=8)) +
            theme(axis.title.y = element_text(size=8)) +
            theme(plot.title = element_text(size=10)) +
            theme(legend.position="none") +
            theme(aspect.ratio = .60) +
            xlab("") +
            ylab("") +
            # scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
            scale_fill_brewer(palette="Dark2")


        } else if (is.character(df_plot$value_plot) == TRUE){


          df_plot <- df_plot %>%
            dplyr::mutate(value_plot = ifelse(is.na(value_plot), "*NA", value_plot)) %>%
            dplyr::group_by(variable, value_plot) %>%
            dplyr::summarize(count=n()) %>%
            dplyr::group_by(variable) %>%
            dplyr::mutate(total=sum(count),
                          prop=count/total)

          plot <- df_plot %>%
            ggplot(aes(x=value_plot, y=prop)) +
            geom_bar(stat="identity", fill="deepskyblue3") +
            geom_text(aes(label=paste0(count, " | ", sprintf("%1.1f%%", prop*100))), hjust = 1, size = 3.5, fontface = "bold") +
            theme_bw() +
            coord_flip() +
            ggtitle(paste0(plot_label)) +
            theme(plot.title = element_text(size=10)) +
            theme(legend.text=element_text(size=6),  legend.title=element_text(size=8)) +
            theme(axis.title.y = element_text(size=8)) +
            theme(plot.title = element_text(size=10)) +
            theme(legend.position="none") +
            theme(aspect.ratio = .60) +
            xlab("") +
            ylab("") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 15), guide = guide_axis(check.overlap = TRUE))
            # scale_fill_brewer(palette="Dark2")


        }

        # print(plot)


        plot_list[[i]] <- plot

      }, silent = TRUE)

    }

  # LOOP OVER LIST OF PLOTS, PLOTTING 9 PER PAGE
  total_plots <- 1:length(plot_list)
  plot_list_sub <- split(total_plots, ceiling(seq(total_plots)/9))

  for (p in plot_list_sub){

      print(p)
      # title <- ggdraw() + draw_label("MPG declines with displacement and horsepower", fontface='bold')
      title <- "outcomes"
      p.fin <- cowplot::plot_grid(plotlist = plot_list[p], nrow = 3, ncol = 3)
      print(p.fin)

  }

  # }

  ###################################
  dev.off()
  graphics.off()

}







# gen_univariate_plots <- function(df=NULL, grouping_var=NULL, plot_path=NULL){
#
#
#   dir.create(dirname(plot_path), showWarnings = F, recursive = T)
#   ###################################
#   # GENERATE PDF OF OUTPUTS
#   cairo_pdf(filename = plot_path,
#             width = 15,
#             height = 10,
#             onefile=T)
#
#   plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
#   text(5, 8, paste0("Univariate Variable Distributions"), cex = 2)
#
#   df$grouping_var <- df %>% pull(eval(parse(text=grouping_var)))
#
#   df <- df %>%
#     mutate(value = ifelse(is.na(value), "NA", value))
#
#   for (group in unique(df$grouping_var)){
#     print(group)
#
#     df1 <- df %>% filter(grouping_var == group)
#
#     plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
#     text(5, 8, paste0("Outcome Variables | ", group), cex = 2)
#
#       # PLOT DISCRETE VARIABLES
#       try({
#
#         df_plot <- df1 %>%
#           dplyr::filter(plot_type == "bar") %>%
#           dplyr::mutate(value = ifelse(is.na(value), "NA", value)) %>%
#           dplyr::group_by(variable, value) %>%
#           dplyr::summarize(count=n()) %>%
#           dplyr::group_by(variable) %>%
#           dplyr::mutate(total=sum(count),
#                  prop=count/total)
#
#         n_pages <- ceiling(length(unique(df_plot$variable))/9)
#         lapply(seq(n_pages), function(page) {
#           plot <- df_plot %>%
#             ggplot(aes(x=value, y=count)) +
#             geom_bar(stat="identity", fill="deepskyblue4") +
#             geom_text(aes(label=paste0(count, " | ", sprintf("%1.1f%%", prop*100))), hjust = 1, size = 2.5) +
#             facet_wrap_paginate(~variable, scales = "free", ncol = 3, nrow = 3, page = page) +
#             theme_bw() +
#             coord_flip() +
#             # theme(axis.text.x = element_text(angle = -45, vjust = 0.5)) +
#             # theme(aspect.ratio = .75) +
#             ylab("count") +
#             scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
#             # scale_fill_brewer(palette="Dark2")
#             ggtitle(paste0("Outcome Variables | ", group))
#           print(plot)
#         })
#
#       }, silent = TRUE)
#
#
#       # # PLOT CONTINUOUS VARIABLES
#       #   df_plot <- df1 %>%
#       #     dplyr::filter(plot_type == "continuous") %>%
#       #     dplyr::mutate(value = as.numeric(value)) %>%
#       #     group_by(variable) %>%
#       #     mutate(med = median(value, na.rm=TRUE))
#       #
#       #   if (nrow(df_plot) > 0){
#       #
#       #     plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
#       #     text(5, 8, paste0("Continuous Variables | ", group), cex = 2)
#       #
#       #     # n_pages <- ceiling(length(unique(df_plot$variable))/9)
#       #     lapply(seq(n_pages), function(page) {
#       #       plot <- df_plot %>%
#       #         ggplot(aes(x=value)) +
#       #         geom_histogram(aes(y = after_stat(density)), alpha = 0.3, position = "dodge", fill="darkgreen") +
#       #         geom_density(alpha = 0.3) +
#       #         geom_vline(aes(xintercept = med), linetype = "dashed", linewidth = 1) +
#       #         # facet_wrap_paginate(~variable, scales = "free", page = page) +
#       #         facet_wrap(~variable, scales = "free") +
#       #         theme_bw() +
#       #         theme(aspect.ratio = .75) +
#       #         ylab("count") +
#       #         ggtitle(paste0("Continuous Variables | ", group))
#       #       print(plot)
#       #     })
#       #
#       #   }
#
#         # }, silent = TRUE)
#
#   }
#
#   ###################################
#   dev.off()
#   graphics.off()
#
# }



