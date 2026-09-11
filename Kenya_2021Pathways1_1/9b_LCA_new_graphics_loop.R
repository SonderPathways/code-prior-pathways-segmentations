###------------------------------------------------
## Graphics to compare clustering solutions from LCA output
## Rural & Urban 
###------------------------------------------------

library("dplyr")
library("openxlsx")

###------------------------------------------------
###-------------- Read-in and prep Data Dictionary 
###-------------  

## Edit to the correct filepath

user = "Amanda"

if (tolower(user) == "helena") {
  rootdir <- "C:/Users/hmang/UW START Dropbox/Helena Manguerra"
} else if (tolower(user) == "mac") {
  rootdir <- "~/UW START Dropbox/Helena Manguerra"
} else if (tolower(user) == "pc") {
  rootdir <- "C:/Users/theja/UW START Dropbox/Helena Manguerra"
} else if (tolower(user) == "amanda") {
  rootdir <- "C:/Users/manda/UW START Dropbox/Amanda Brumwell"
} else if (tolower(user) == "andrea") {
  rootdir <- "C:/Users/Andrea Rivas/UW START Dropbox/Andrea Rivas"
} else if (tolower(user) == "bih") {
  rootdir <- "C:/Users/bihmo/UW START Dropbox/Bih Epse Cuba Moki Suh"
} else if (tolower(user) == "sarah") {
  rootdir <- "C:/Users/sshaw/UW START Dropbox/Sarah Shaw"
} else {
  print("No filepath")
}

input_dir <- file.path(rootdir, "START_229_Gender Equity/Working Files/Data/KE_2022_constructed_data")

Data_Dictionary <- read_xlsx(file.path(input_dir,"0_Kenya_DHS_Data_Dictionary.xlsx"), 
                                       sheet = "Vulnerabilities_full_list_SS") 

colnames(Data_Dictionary) <- Data_Dictionary[6, ]
Data_Dictionary <- Data_Dictionary[-6,]

dhs.seg_rural <- readRDS(file.path(input_dir,"LCA/rp/dhs.seg.rds"))
dhs.seg_urban <- readRDS(file.path(input_dir,"LCA/up/dhs.seg.rds"))

## ------------------------------------------------

# The script below will need to be run once for the rural partnered set, and 
# again for the urban partnered set. 

###------------------------------------------------
###-------------- RURAL PARTNERED Plots 
###-------------  

# View(Data_Dictionary)
keep_vars_rur <- Data_Dictionary %>%
  slice(6:n()) %>% 
  ## The column name will need to be changed once the first round of LCA decisions are incorporated in the DD. 
  ## Change the name column name from `rp_PCA_decision_primary` to `rp_currentLCA`
  filter(startsWith(tolower(`rp_PCA_decision_primary`), "keep")) 

# View(keep_vars_rur)

keep_vars_rur_list <- unique(keep_vars_rur$`variable name`)

keep_vars_rur_list <- unlist(keep_vars_rur_list)

print(keep_vars_rur_list)

## Unclear why we still need the below code 
custom_labels <- c(
  "ed.lev.cat1" = "Women's edu",
  "wd.ownincome.cat" = "Women's deicision on own income",
  "jd.hlth.cat" = "Joined decision on women's health",
  "work.seasonal" = "Women's work",
  "water.cat" = "Water source",
  "partner.working" = "Partner's work",
  "auto.contra" = "Autonomy on contraceptive",
  "vehic.index" = "Vehicle ownership"
)

### -----------------------------------
### 
# 
# rur_var <- c("anyed.yn", "age.1stcohab.cat", "female.circumcision.yn2", "age.diff.bi", 
#              "muslim", "rad.yn", "freq.tv" ,"w.financial.acct", "work.seasonal2","partner.working.yn",
#              "slum1", "hh.wat.time.cat2","highestyearsedinHH.7plus","travtime.fac.cat2","pship.cat2",
#              "husb.ed.mtprim2","hh.kidwom.rat.2plus" ,"electric.assets.index","alwaysres.yn",
#              "jdwd.index.cat3", "transpo.index.cat","household.assets.index.cat2", "cook.wood",
#              "earnings.type2")

rur_var <- c(keep_vars_rur_list)



### -------------------- Prep rural data to include all
### -------------------- categorical and binary variables 

## IMPORTANT Need to run this chunk each time we want to generate rural graphs after running the urban graphs 
# to resave long_dt

long_dt <- dhs.seg_rural %>%
  select(c(all_of(rur_var), 'cluster_k2','cluster_k3', 'cluster_k4','cluster_k5', 
           'cluster_k6','cluster_k7', 'cluster_k8','cluster_k9', 'cluster_k10')) 


long_dt <- melt(long_dt, measure.vars=c(rur_var), id.vars= c('cluster_k2','cluster_k3', 'cluster_k4','cluster_k5', 
                                                  'cluster_k6','cluster_k7', 'cluster_k8','cluster_k9', 'cluster_k10'
                                                  ), value.name="level")


### -------------------- Create PDF and run loop 
### -------------------- 


pdf_file <- file.path(input_dir,"LCA/rp/LCA_rural_plots.pdf")

pdf_width <- 8.5  # inches
pdf_height <- 11  # inches

# Set margins and spacing
margin_left <- 1  # inches
margin_right <- 1
margin_top <- 1
margin_bottom <- 1
spacing_between_plots <- 0.5

pdf(pdf_file, width = 11, height=8.5)

for(i in rur_var)  {
  
  # i = rur_var[1:2] 
  
  ### Plot k2
  
  cluster_size_k2 <- long_dt %>%
    group_by(cluster_k2, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')
  
  agg_dt_k2 <- long_dt %>%
    group_by(cluster_k2, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k2, by = c("cluster_k2", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))
  
  agg_dt_k2 <- merge(agg_dt_k2, var, by.x = "variable", by.y = "f.var.name")
  
  agg_dt1_k2 <- agg_dt_k2
  agg_dt1_k2 <- agg_dt1_k2 %>% filter(variable == i)
  plot_2 <- ggplot(agg_dt1_k2, aes(x=var.name, y=pct, fill=level)) + 
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k2, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    ggtitle(paste("Rural LCA Variable:", i)) +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )
 
#view(agg_dt_k2)

  ### Plot k3

  cluster_size_k3 <- long_dt %>%
    group_by(cluster_k3, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')

  agg_dt_k3 <- long_dt %>%
    group_by(cluster_k3, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k3, by = c("cluster_k3", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))

  agg_dt_k3 <- merge(agg_dt_k3, var, by.x = "variable", by.y = "f.var.name")

  agg_dt1_k3 <- agg_dt_k3
  agg_dt1_k3 <- agg_dt1_k3 %>% filter(variable == i)
  plot_3 <- ggplot(agg_dt1_k3, aes(x=var.name, y=pct, fill=level)) +
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k3, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )

  ### Plot k4

  cluster_size_k4 <- long_dt %>%
    group_by(cluster_k4, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')

  agg_dt_k4 <- long_dt %>%
    group_by(cluster_k4, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k4, by = c("cluster_k4", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))

  agg_dt_k4 <- merge(agg_dt_k4, var, by.x = "variable", by.y = "f.var.name")

  agg_dt1_k4 <- agg_dt_k4
  agg_dt1_k4 <- agg_dt1_k4 %>% filter(variable == i)
  plot_4 <- ggplot(agg_dt1_k4, aes(x=var.name, y=pct, fill=level)) +
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k4, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )

  ### Plot k5

  cluster_size_k5 <- long_dt %>%
    group_by(cluster_k5, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')

  agg_dt_k5 <- long_dt %>%
    group_by(cluster_k5, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k5, by = c("cluster_k5", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))

  agg_dt_k5 <- merge(agg_dt_k5, var, by.x = "variable", by.y = "f.var.name")

  agg_dt1_k5 <- agg_dt_k5
  agg_dt1_k5 <- agg_dt1_k5 %>% filter(variable == i)
  plot_5 <- ggplot(agg_dt1_k5, aes(x=var.name, y=pct, fill=level)) +
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k5, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )

  ### Plot k6

  cluster_size_k6 <- long_dt %>%
    group_by(cluster_k6, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')

  agg_dt_k6 <- long_dt %>%
    group_by(cluster_k6, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k6, by = c("cluster_k6", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))

  agg_dt_k6 <- merge(agg_dt_k6, var, by.x = "variable", by.y = "f.var.name")

  agg_dt1_k6 <- agg_dt_k6
  agg_dt1_k6 <- agg_dt1_k6 %>% filter(variable == i)
  plot_6 <- ggplot(agg_dt1_k6, aes(x=var.name, y=pct, fill=level)) +
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k6, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )

  ### Plot k7

  cluster_size_k7 <- long_dt %>%
    group_by(cluster_k7, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')

  agg_dt_k7 <- long_dt %>%
    group_by(cluster_k7, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k7, by = c("cluster_k7", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))

  agg_dt_k7 <- merge(agg_dt_k7, var, by.x = "variable", by.y = "f.var.name")

  agg_dt1_k7 <- agg_dt_k7
  agg_dt1_k7 <- agg_dt1_k7 %>% filter(variable == i)
  plot_7 <- ggplot(agg_dt1_k7, aes(x=var.name, y=pct, fill=level)) +
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k7, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )

  ### Plot k8

  cluster_size_k8 <- long_dt %>%
    group_by(cluster_k8, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')

  agg_dt_k8 <- long_dt %>%
    group_by(cluster_k8, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k8, by = c("cluster_k8", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))

  agg_dt_k8 <- merge(agg_dt_k8, var, by.x = "variable", by.y = "f.var.name")

  agg_dt1_k8 <- agg_dt_k8
  agg_dt1_k8 <- agg_dt1_k8 %>% filter(variable == i)
  plot_8 <- ggplot(agg_dt1_k8, aes(x=var.name, y=pct, fill=level)) +
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k8, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )

### Plot k9

cluster_size_k9 <- long_dt %>%
  group_by(cluster_k9, variable) %>%
  summarize(cluster_total = n(), .groups = 'drop')

agg_dt_k9 <- long_dt %>%
  group_by(cluster_k9, variable, level) %>%
  summarize(ct = n(), .groups = 'drop') %>%
  left_join(cluster_size_k9, by = c("cluster_k9", "variable")) %>%
  mutate(pct = round((ct/cluster_total)*100, 2)) %>%
  mutate(var.label = ifelse(variable %in% names(custom_labels),
                            custom_labels[variable],
                            variable))

agg_dt_k9 <- merge(agg_dt_k9, var, by.x = "variable", by.y = "f.var.name")

agg_dt1_k9 <- agg_dt_k9
agg_dt1_k9 <- agg_dt1_k9 %>% filter(variable == i)
plot_9 <- ggplot(agg_dt1_k9, aes(x=var.name, y=pct, fill=level)) +
  geom_bar(stat="identity", position="stack",width=0.5) +
  facet_grid(category ~ cluster_k9, scales = "free_y", space = "free_x", switch = "y") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  #scale_fill_manual(values=my_colors) +
  labs(title = "", x = "", y = "") +
  guides(fill=guide_legend(title="Level")) +
  geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
            position=position_stack(vjust=0.5), size=3) +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 5.5),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        strip.text = element_text(size = 6),
        legend.position = "none" )

### Plot k10

cluster_size_k10 <- long_dt %>%
  group_by(cluster_k10, variable) %>%
  summarize(cluster_total = n(), .groups = 'drop')

agg_dt_k10 <- long_dt %>%
  group_by(cluster_k10, variable, level) %>%
  summarize(ct = n(), .groups = 'drop') %>%
  left_join(cluster_size_k10, by = c("cluster_k10", "variable")) %>%
  mutate(pct = round((ct/cluster_total)*100, 2)) %>%
  mutate(var.label = ifelse(variable %in% names(custom_labels),
                            custom_labels[variable],
                            variable))

agg_dt_k10 <- merge(agg_dt_k10, var, by.x = "variable", by.y = "f.var.name")

agg_dt1_k10 <- agg_dt_k10
agg_dt1_k10 <- agg_dt1_k10 %>% filter(variable == i)
plot_10 <- ggplot(agg_dt1_k10, aes(x=var.name, y=pct, fill=level)) +
  geom_bar(stat="identity", position="stack",width=0.5) +
  facet_grid(category ~ cluster_k10, scales = "free_y", space = "free_x", switch = "y") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  #scale_fill_manual(values=my_colors) +
  labs(title = "", x = "", y = "") +
  guides(fill=guide_legend(title="Level")) +
  geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
            position=position_stack(vjust=0.5), size=3) +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 5.5),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
        strip.text = element_text(size = 6),
        legend.position = "none" )

cluster_compare <- ggarrange(plot_2,
                             plot_3, plot_4,
                             plot_5, plot_6,plot_7,
                             plot_8, plot_9, plot_10,
                             ncol = 3, nrow = 3 ) 


print(cluster_compare)


}

dev.off()



###-----------------------------------------------------------------------------
###------------------Same process for URBAN Variables
###------------------


keep_vars_urb <- Data_Dictionary %>%
  slice(6:n()) %>% 
  ## The column name will need to be changed once the first round of LCA decisions are incorporated in the DD. 
  ## Change the name column name from `up_PCA_decision_primary` to `up_currentLCA`
  filter(startsWith(tolower(`up_PCA_decision_primary`), "keep")) 

keep_vars_urb_list <- unique(keep_vars_urb$`variable name`)

keep_vars_urb_list <- unlist(keep_vars_urb_list)

print(keep_vars_urb_list)

custom_labels <- c(
  "ed.lev.cat1" = "Women's edu",
  "wd.ownincome.cat" = "Women's deicision on own income",
  "jd.hlth.cat" = "Joined decision on women's health",
  "work.seasonal" = "Women's work",
  "water.cat" = "Water source",
  "partner.working" = "Partner's work",
  "auto.contra" = "Autonomy on contraceptive",
  "vehic.index" = "Vehicle ownership"
)

### -----------------------------------
### For now, copy and pasted list of variables selected in keep_vars_rur_list. 


# urb_var <- c("anyed.yn", "age.1stcohab.cat", "female.circumcision.yn2", "age.diff.bi", 
#              "muslim", "rad.yn", "freq.tv" ,"w.financial.acct", "work.seasonal2","partner.working.yn",
#              "slum1", "hh.wat.time.cat2","highestyearsedinHH.7plus","travtime.fac.cat2","pship.cat2",
#              "husb.ed.mtprim2","hh.kidwom.rat.2plus" ,"electric.assets.index","alwaysres.yn",
#              "jdwd.index.cat3", "transpo.index.cat","household.assets.index.cat2", "cook.wood",
#              "earnings.type2")

urb_var <- c(keep_vars_urb_list)

#var.rural <- cat.var[cat.var != "slum1"]

### -------------------- Prep rural data to include all
### -------------------- categorical and binary variables 

## IMPORTANT Need to run this chunk each time we want to generate urban graphs after running the rural graphs 
# to resave long_dt

long_dt <- dhs.seg_urban %>%
  select(c(all_of(urb_var), 'cluster_k2','cluster_k3', 'cluster_k4','cluster_k5', 
           'cluster_k6','cluster_k7', 'cluster_k8','cluster_k9', 'cluster_k10')) 

long_dt <- melt(long_dt, measure.vars=c(urb_var), id.vars= c('cluster_k2','cluster_k3', 'cluster_k4','cluster_k5', 
                                                             'cluster_k6','cluster_k7', 'cluster_k8','cluster_k9', 'cluster_k10'), value.name="level")


### -------------------- Create PDF and run loop 
### --------------------


pdf(file.path(input_dir,"LCA/up/LCA_urban_plots.pdf"), width = 11, height=8.5)

for(i in urb_var)  {
  
  # i = urb_var[1:2] 
  
  ### Plot k2
  
  cluster_size_k2 <- long_dt %>%
    group_by(cluster_k2, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')
  
  agg_dt_k2 <- long_dt %>%
    group_by(cluster_k2, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k2, by = c("cluster_k2", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))
  
  agg_dt_k2 <- merge(agg_dt_k2, var, by.x = "variable", by.y = "f.var.name")
  
  agg_dt1_k2 <- agg_dt_k2
  agg_dt1_k2 <- agg_dt1_k2 %>% filter(variable == i)
  plot_2 <- ggplot(agg_dt1_k2, aes(x=var.name, y=pct, fill=level)) + 
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k2, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    ggtitle(paste("Urban LCA Variable:", i)) +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )
  
  #view(agg_dt_k2)
  
  ### Plot k3
  
  cluster_size_k3 <- long_dt %>%
    group_by(cluster_k3, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')
  
  agg_dt_k3 <- long_dt %>%
    group_by(cluster_k3, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k3, by = c("cluster_k3", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))
  
  agg_dt_k3 <- merge(agg_dt_k3, var, by.x = "variable", by.y = "f.var.name")
  
  agg_dt1_k3 <- agg_dt_k3
  agg_dt1_k3 <- agg_dt1_k3 %>% filter(variable == i)
  plot_3 <- ggplot(agg_dt1_k3, aes(x=var.name, y=pct, fill=level)) +
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k3, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )
  
  ### Plot k4
  
  cluster_size_k4 <- long_dt %>%
    group_by(cluster_k4, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')
  
  agg_dt_k4 <- long_dt %>%
    group_by(cluster_k4, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k4, by = c("cluster_k4", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))
  
  agg_dt_k4 <- merge(agg_dt_k4, var, by.x = "variable", by.y = "f.var.name")
  
  agg_dt1_k4 <- agg_dt_k4
  agg_dt1_k4 <- agg_dt1_k4 %>% filter(variable == i)
  plot_4 <- ggplot(agg_dt1_k4, aes(x=var.name, y=pct, fill=level)) +
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k4, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )
  
  ### Plot k5
  
  cluster_size_k5 <- long_dt %>%
    group_by(cluster_k5, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')
  
  agg_dt_k5 <- long_dt %>%
    group_by(cluster_k5, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k5, by = c("cluster_k5", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))
  
  agg_dt_k5 <- merge(agg_dt_k5, var, by.x = "variable", by.y = "f.var.name")
  
  agg_dt1_k5 <- agg_dt_k5
  agg_dt1_k5 <- agg_dt1_k5 %>% filter(variable == i)
  plot_5 <- ggplot(agg_dt1_k5, aes(x=var.name, y=pct, fill=level)) +
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k5, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )
  
  ### Plot k6
  
  cluster_size_k6 <- long_dt %>%
    group_by(cluster_k6, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')
  
  agg_dt_k6 <- long_dt %>%
    group_by(cluster_k6, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k6, by = c("cluster_k6", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))
  
  agg_dt_k6 <- merge(agg_dt_k6, var, by.x = "variable", by.y = "f.var.name")
  
  agg_dt1_k6 <- agg_dt_k6
  agg_dt1_k6 <- agg_dt1_k6 %>% filter(variable == i)
  plot_6 <- ggplot(agg_dt1_k6, aes(x=var.name, y=pct, fill=level)) +
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k6, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )
  
  ### Plot k7
  
  cluster_size_k7 <- long_dt %>%
    group_by(cluster_k7, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')
  
  agg_dt_k7 <- long_dt %>%
    group_by(cluster_k7, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k7, by = c("cluster_k7", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))
  
  agg_dt_k7 <- merge(agg_dt_k7, var, by.x = "variable", by.y = "f.var.name")
  
  agg_dt1_k7 <- agg_dt_k7
  agg_dt1_k7 <- agg_dt1_k7 %>% filter(variable == i)
  plot_7 <- ggplot(agg_dt1_k7, aes(x=var.name, y=pct, fill=level)) +
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k7, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )
  
  ### Plot k8
  
  cluster_size_k8 <- long_dt %>%
    group_by(cluster_k8, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')
  
  agg_dt_k8 <- long_dt %>%
    group_by(cluster_k8, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k8, by = c("cluster_k8", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))
  
  agg_dt_k8 <- merge(agg_dt_k8, var, by.x = "variable", by.y = "f.var.name")
  
  agg_dt1_k8 <- agg_dt_k8
  agg_dt1_k8 <- agg_dt1_k8 %>% filter(variable == i)
  plot_8 <- ggplot(agg_dt1_k8, aes(x=var.name, y=pct, fill=level)) +
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k8, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )
  
  ### Plot k9
  
  cluster_size_k9 <- long_dt %>%
    group_by(cluster_k9, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')
  
  agg_dt_k9 <- long_dt %>%
    group_by(cluster_k9, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k9, by = c("cluster_k9", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))
  
  agg_dt_k9 <- merge(agg_dt_k9, var, by.x = "variable", by.y = "f.var.name")
  
  agg_dt1_k9 <- agg_dt_k9
  agg_dt1_k9 <- agg_dt1_k9 %>% filter(variable == i)
  plot_9 <- ggplot(agg_dt1_k9, aes(x=var.name, y=pct, fill=level)) +
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k9, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )
  
  ### Plot k10
  
  cluster_size_k10 <- long_dt %>%
    group_by(cluster_k10, variable) %>%
    summarize(cluster_total = n(), .groups = 'drop')
  
  agg_dt_k10 <- long_dt %>%
    group_by(cluster_k10, variable, level) %>%
    summarize(ct = n(), .groups = 'drop') %>%
    left_join(cluster_size_k10, by = c("cluster_k10", "variable")) %>%
    mutate(pct = round((ct/cluster_total)*100, 2)) %>%
    mutate(var.label = ifelse(variable %in% names(custom_labels),
                              custom_labels[variable],
                              variable))
  
  agg_dt_k10 <- merge(agg_dt_k10, var, by.x = "variable", by.y = "f.var.name")
  
  agg_dt1_k10 <- agg_dt_k10
  agg_dt1_k10 <- agg_dt1_k10 %>% filter(variable == i)
  plot_10 <- ggplot(agg_dt1_k10, aes(x=var.name, y=pct, fill=level)) +
    geom_bar(stat="identity", position="stack",width=0.5) +
    facet_grid(category ~ cluster_k10, scales = "free_y", space = "free_x", switch = "y") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #scale_fill_manual(values=my_colors) +
    labs(title = "", x = "", y = "") +
    guides(fill=guide_legend(title="Level")) +
    geom_text(aes(label=round(pct, 0)), color = "grey", alpha = 0.7,
              position=position_stack(vjust=0.5), size=3) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(size = 6, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 5.5),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
          strip.text = element_text(size = 6),
          legend.position = "none" )
  
  cluster_compare <- ggarrange(plot_2,
                               plot_3, plot_4,
                               plot_5, plot_6,plot_7,
                               plot_8, plot_9, plot_10,
                               ncol = 3, nrow = 3 ) 
  
  
  print(cluster_compare)
  
  
}

dev.off()




