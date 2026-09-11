###############################  KENYA_DHS22_PCA ###############################
#
# Purpose: Principal component analysis 
#
# Authors: Anu Mishra (Ethiopia resource code), Helena Manguerra (UW START, Kenya)
#
# Input files: "4_Kenya_DHS_vulnerability.rds", "1_Kenya_DHS_outcomes.rds"
#               
#              
# Last edited:
#
# Objects created: dhs.seg, dhs.seg.des
#
# Output files: "pdf"
#
# Ancestor scripts: 7_Kenya_DHS_construct_mapping_EA_to_PCA.R
#
# Descendant script: 9_Kenya_DHS_construct_mapping_PCA_to_LCA.R
# 
# Note: Need to set directory for each user
# 
#_______________________________________________________________________________ 

# 1. Set up --------------------------------------------------------------------

  #* Clearing the workspace ----------------------------------------------------

  rm(list=ls())
  
  #* Set whether want to use long subset of questionnaire or full --------------
  
  q.type <- "long"
  
  #* Set grouping level for running PCA ----------------------------------------
  
  grouping <- "pca.grouping" # options: "category", "sub.category", "pca.grouping"
  
  #* Specify whether this is the first or a repeat round of PCA. If repeat, script 7 will import and filter on decisions from first round ----
  pca_round <- "repeat" # options: "first", "repeat"
  
  #* specify population subgroup
  popgroup <- "Women with kids under 5"
  
  #* Set working directory -----------------------------------------------------
  user = "Darcy"
  
  if (tolower(user) == "helena") {
    rootdir <- "C:/Users/hmang/UW START Dropbox/Helena Manguerra/"
  } else if (tolower(user) == "mac") {
    rootdir <- "~/UW START Dropbox/Helena Manguerra/"
  } else if (tolower(user) == "amanda") {
    rootdir <- "C:/Users/manda/UW START Dropbox/Amanda Brumwell/"
  } else if (tolower(user) == "andrea") {
    rootdir <- "C:/Users/Andrea Rivas/UW START Dropbox/Andrea Rivas/"
  } else if (tolower(user) == "bih") {
    rootdir <- "C:/Users/bihmo/UW START Dropbox/Bih Epse Cuba Moki Suh/"
  } else if (tolower(user) == "sarah") {
    rootdir <- "C:/Users/sshaw/UW START Dropbox/Sarah Shaw/"
  } else if (tolower(user) == "darcy") {
    rootdir <-"/Users/darcyrao/Dropbox"
  } else {
    print("No filepath")
  }
  
  input_dir = file.path(rootdir, "START_229_Gender Equity/Working Files/Data/KE_2022_constructed_data")
  output_dir = file.path(rootdir, "START_229_Gender Equity/Working Files/Data/KE_2022_constructed_data/pcas")
  
  
  #* Load packages -------------------------------------------------------------
  
  pacman::p_load("tidyverse", "knitr", "data.table", "lmtest", "logbin", "survey","readxl", "ggrepel", "FactoMineR", "corrr", "haven","ggplot2","ggcorrplot","factoextra", "BiocManager", "mice", "finalfit")
  # change factoMineR to FactoMiner
  # install.packages('BiocManager')
  
  #* Import data and spreadsheets ----------------------------------------------
  
  vulnerability <- readRDS(file.path(input_dir, "4_Kenya_DHS_vulnerability.rds"))
  outcomes <- readRDS(file.path(input_dir, "1_Kenya_DHS_outcomes.rds"))
  
  dhs.seg <- merge(vulnerability, outcomes, by="caseid", all.x=T)
  dhs.seg <- data.table(dhs.seg)
  dhs.seg.des <-svydesign(id=~v021, strata=~v023, weights=~wt, data=dhs.seg)
  
  
  source(file.path(input_dir, "scripts_for_darcy/7_Kenya_DHS_construct_mapping_EA_to_PCA.R"))
  
  const <- const[order(const$category),]
  
  const <- data.table(const)
  
  #* Dropping everything that is not in vulnerabilities script -----------------
  
  const <- const[var.name %in% names(vulnerability)]
  
  #* Define subsets and categories to loop through -----------------------------
  
  subset.list <- c("urban", "rural")
  
  if(grouping == "pca.grouping"){
    category.list <- unique(as.vector(const$pca.grouping))
  } else if(grouping == "sub.category"){
    category.list <- unique(as.vector(const$sub.category))
  } else if(grouping == "category"){
    category.list <- unique(as.vector(const$category))
  }
  

# 2. Define functions ----------------------------------------------------------  

  
  f_biplot <- function(load,pc){
    plot_dat <- as.data.frame(load[,pc])
    plot_dat$labels <- row.names(plot_dat)
    plot_dat$start_x <- 0
    plot_dat$stary_y <- 0
    max_lim <- ceiling(max(abs(plot_dat[,1:2])))
    
    p <- ggplot(plot_dat) + 
      geom_segment(aes(x=plot_dat$start_x,y=plot_dat$stary_y,xend=plot_dat[,1],yend=plot_dat[,2]),arrow = arrow(length=unit(0.2,'cm'))) +
      coord_cartesian(xlim = c(-max_lim,max_lim),ylim=c(-max_lim,max_lim)) + xlab(names(plot_dat)[1]) + ylab(names(plot_dat)[2]) +
      theme_bw() +
      geom_text_repel(mapping = aes(label=plot_dat$labels,x=plot_dat[,1],y=plot_dat[,2])) 
    plot(p)
  }
  
  f_biplot2 <- function(pca_res, pc){
    plot <- fviz_pca_var(pca_res, axes = pc,
                         col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         labsize=0.1, repel=T)
    print(plot)
  }
  
  f_screeplot <- function(pca_res){
    prop <- (pca_res$sdev)^2/sum((pca_res$sdev)^2)
    cum_prop <- cumsum(prop)
    cum_prop_label <- paste0(round(100*cum_prop,2),'%')
    plot_dat <- data.frame(PC=paste0('PC',1:length(cum_prop)), sd=pca_res$sdev,prop=prop,cum_prop=cum_prop,label=cum_prop_label)
    
    p <- ggplot(plot_dat,aes(x=reorder(PC,cum_prop),y=sd)) + geom_bar(stat="identity") + theme_bw() +
      xlab('PC') + geom_text(aes(label = label),vjust=-1)
    plot(p)
  }
  
  f_loadings <- function(pca_res){
    plot_dat <- as.data.frame(pca_res$rotation^2)
    plot_dat$vul <- row.names(plot_dat)
    plot_dat <- plot_dat %>% gather(PC, wt, -vul)
    plot_dat$wt <- round(100*plot_dat$wt,2)
    plot_dat$order <- as.numeric(sub('.*PC', '', plot_dat$PC))
    
    p <- ggplot(data = plot_dat, aes(fill = vul, y = wt, x = reorder(PC, order))) +
      geom_bar(position = "stack", stat = "identity") +
      ylab('Contribution Wt') + xlab('PC') + 
      theme(legend.position = "bottom") +
      guides(fill=guide_legend(ncol=2))
    
    plot(p)
  }
  
  f_loadings_1 <- function(pca_res,start_pc, end_pc){
    plot_dat <- as.data.frame(pca_res$rotation^2)
    plot_dat$vul <- row.names(plot_dat)
    plot_dat <- plot_dat %>% gather(PC, wt, -vul)
    plot_dat$wt <- round(100*plot_dat$wt,2)
    plot_dat$order <- as.numeric(sub('.*PC', '', plot_dat$PC))
    
    plot_dat <- plot_dat %>%
      filter(order >= start_pc & order <= end_pc)
    
    p <- ggplot(data = plot_dat, aes(fill = vul, y = wt, x = reorder(PC, order))) +
      geom_bar(position = "stack", stat = "identity") +
      ylab('Contribution Wt') + xlab('PC') + 
      theme(legend.position = "bottom") +
      guides(fill=guide_legend(ncol=2))
    
    plot(p)
  }
  
  f_eigenvalues <- function(pca_res){
    eigenvalues <- pca_res$sdev^2
    plot_data <- data.frame(PC = paste0('PC', 1:length(eigenvalues)), Eigenvalues = eigenvalues)
    
    p <- ggplot(plot_data, aes(x = reorder(PC, -Eigenvalues), y = Eigenvalues)) +
      geom_bar(stat = "identity", fill = "#2A3B90") +  
      geom_text(aes(label = round(Eigenvalues, 2)), vjust = -1) +  
      geom_line(aes(x = reorder(PC, -Eigenvalues), y = Eigenvalues), color = "#92278F", group = 1) +
      theme_bw() +
      xlab('PC') +
      ylab('Eigenvalues') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotating x-axis labels for readability 
    
    plot(p)
  }
  
  #* Plot total contribution to PCs that contribute to 70% of cumulative variance or have eigenvalue >1 ----
  
  f_contrib <- function(pca_res){
    # find the number of factors with eigenvalue >=1
    max.axis.eigen <- max(which(pca_res$sdev^2>=1))
    # find the number of factors that explain 70% of variance
    importance <- summary(pca_res)$importance
    max.axis.var <- min(which(importance[3,]>0.7))
    
    # tax the min of these
    max.axis <- min(max.axis.eigen, max.axis.var)
    
    plot <- fviz_contrib(pca_res, choice = "var", axes = 1:max.axis, top = 25)
    print(plot)
    #The red dashed line on the graph above indicates the expected value if contribution were uniform, so anything above the reference line is important for the dimension
  }
  
# 3. Run PCA and produce output for each category/subset combination -----------
  
  for(c in category.list){
    for(s in subset.list){
    
      # Open PDF file
      
      pdf(file= paste0(output_dir, "PCA_Pathways_", c, "_", s, ".pdf" )) 
      
      # Subset data
     
      ## Keep only final decision vars for the subset. If this is the first round of PCA, base on "keep_" variables from exploratory.
      
      if(pca_round == "first") {
        if(s == "urban"){
          const_sub <- const[keep_urban==1]  
        } else if(s == "rural"){
          const_sub <- const[keep_rural==1]
        } else{
          message("ISSUE: Check subset. Should be one of: urban, rural")
          break
        }
      
      # If it's a subsequent round, use decisions from the first round
        
      } else {
        if(s == "urban"){
          const_sub <- const[urban_PCA_decision_primary=="keep"]  
        } else if(s == "rural"){
          const_sub <- const[rural_PCA_decision_primary=="keep"]
        } else{
          message("ISSUE: Check subset. Should be one of: urban, rural")
          break
        }
      }
      
      
      # Subset to the grouping
      
      pca_group = c
      
      if(q.type %like% "long"){
        df <- dhs.seg[sshort == "long questionnaire"]
        df.des <-subset(dhs.seg.des, sshort == "long questionnaire")
      }
      
      # Subset data 
      if(popgroup == "Women with kids under 5"){
        df <- df[kids.under5 >= 1]
        df.des <- subset(df.des, kids.under5 >= 1)
        
        if(s == "urban"){
          df <- df[hh.urban == 1]  
          df.des <- subset(df.des, hh.urban == 1)
        } else if(s == "rural"){
          df <- df[hh.urban == 0]  
          df.des <- subset(df.des, hh.urban == 0)
        } else{
          message("ISSUE: Check subset. Should be one of: urban, rural")
          break
        }
      }else{
        message("ISSUE: need to define a different popgroup")
        break
      }
      
      domain.var <- unique(const_sub[get(grouping) == pca_group, var.name])
      
      if(length(domain.var) <= 1){ 
        print("Only one variable in sub.category")
        } else {
      
      # Look at missing data
          
      missing_pattern(data.table(df[, domain.var, with = F]))
  
      # Run PCA
          
      d.pca <- svyprcomp(as.formula(paste0('~',paste(domain.var,collapse = '+'))),design = df.des,center = TRUE,scale. = TRUE,scores = FALSE)
      
      character_vars <- domain.var[sapply(df[,..domain.var], is.character)]
      numeric_vars <- domain.var[sapply(df[,..domain.var], is.numeric)]
      
      summary(d.pca)
      
      # Screeplot
      
      ## Eigenvalues
      f_eigenvalues(d.pca)
      
      ## Explained variances per PC
      fviz_eig(d.pca, addlabels = TRUE, ylim = c(0, 30), linecolor = "#92278F") 
   
      ## Cumulative explained variances
      f_screeplot(d.pca)
           # SD measures the variability of data points along a PC. A higher SD indicates that the PC captures more variability in the data.
      
      ## Examine the loadings
      d.pca$rotation
      f_loadings(pca_res = d.pca)
      
      # Examine contribution of variables across top 5 PCs
      
      ## The dashed line is the reference showing the expected value if the contribution were equal. Anything above this is considered important in contibuting to the dimensions.
      f_contrib(d.pca)
      
      # Examine biplots
      
      ## Find the number of factors with eigenvalue >=1
      max.axis.eigen <- max(which(d.pca$sdev^2>=1))
      
      ## Find the number of factors that explain 70% of variance
      importance <- summary(d.pca)$importance
      max.axis.var <- min(which(importance[3,]>0.7))
      max.axis <- max(max(max.axis.eigen, max.axis.var), 2) # take the maximum of these, and make sure it is at least 2 to generate a plot
      
      possible_biplots <- data.table(expand.grid(i = 1:max.axis, j = 1:max.axis))
      possible_biplots <- possible_biplots[order(i,j)]
      possible_biplots <- possible_biplots[i != j & j > i] # filter to remove those with i = j
      
      possible_biplots_l <- as.list(as.data.frame(t(possible_biplots)))
      possible_biplots_unique <- unique(lapply(possible_biplots_l, sort))
  
      for(i in 1:length(possible_biplots_unique)){
        
        f_biplot2(d.pca,pc=possible_biplots_unique[[i]])  
        
      }
      
      }
    
      # Close PDF file
      dev.off()
         
    }
  }
  
  
  
  
  
  
  
  
  
