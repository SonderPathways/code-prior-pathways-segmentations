#### 10a.1_Ethiopia_DHS_cluster_plots
#### purpose: creates functions called in scripts 9 to plot cluster results 
### input: 7_Construct mapping_external.xlsx",sheet=1 and final_var_list
### edited by Darcy Rao
### note: working directory needs to be set for each user 
## Changes from Anu's version: 
  # Add labels
  # Use the survey package to calculate weighted proportions --
  # Change the orientation
  # Change the colors

###AG 4/15 inserted this code to make this a standalone do file, but commented out. Can delete if choose to combine with 10_Ethiopia_cluster_plots
#AG edit to rename to 10a1 
#AG 4/15 edited to use 7_Construct_mapping_external_final rather than 7_Construct mapping 
#AG edited to label pastoralnoagro.yn correctly. (Anypastoral refers to pastoralists and agropastoralists)

#rm(list=ls())
##install.packages('pacman') -- only needs to be done once
#pacman::p_load(gridExtra)
##library(gridExtra)

## Set working directory - change for different users
#setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/") # Anu
#setwd("/Users/darcyrao/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Pathways - GE Data & Insights/Modeling") # for use from Darcy's Mac

##reading in construct mapping
#constr <- readxl::read_excel("Data/Ethiopia_constructed_data/7_Construct mapping_external_final.xlsx",sheet="Sheet1")
#constr <- constr[!duplicated(constr$f.var.name),]
#constr$category[constr$f.var.name=='early.birth.15'] <-  "Woman and her past experiences"


### categorize variables as binary/categorical/continuous
#var <- readxl::read_xlsx("Data/Ethiopia_constructed_data/7_Construct_mapping_external_final.xlsx",sheet = "final_var_list")
#var <- var[is.na(var$Cut),]
#var <- var[!duplicated(var$f.var.name),]
#var$f.var.name[var$f.var.name=='pastoralnoagro.yn'] <- 'pastoralnoagro.yn'

#con.var <- c(var$f.var.name[var$variable_type=='continuous'],'conflict_incidents')
#cat.var <- c(var$f.var.name[var$variable_type=='categorical' | var$variable_type=='ordinal'],'age.adj')
#bin.var <- c(var$f.var.name[var$variable_type=='binary'])

#### end of inserted code ######

# Heatmap function

heatmap.bin <- function(dt,var.list=bin.var,urban=TRUE,constr.map=constr,title='',plot_text=FALSE){
  if(urban==TRUE){
    var.list <- var.list[!var.list %in% 'pastoralnoagro.yn']
    dt$slum1 <- ifelse(dt$slum1=='urban slum',1,0)
  }
  else{    var.list <- var.list[!var.list %in% 'slum1']}
  
  plot_dt <- dt %>%
      as_survey_design(id=v021, strata=v023, weights=wt) %>%
      group_by(cluster) %>%
      summarize(across(c(all_of(var.list)), survey_mean)) %>%
      select(c("cluster", all_of(var.list)))

  
  plot_dt <- plot_dt %>% tidyr::gather(vul,prop,-cluster)
  plot_dt <- left_join(plot_dt,constr[,c('category','f.var.name', 'var.label')],by=c('vul'='f.var.name'))
  plot_dt$category[plot_dt$category=="Woman and her past experiences"] <- "Woman and her past experiences / \nhealthcare and mental models"
  plot_dt$category[plot_dt$category=="Larger environment"] <- "Larger \nenvironment"
  plot_dt$category[plot_dt$category=="Household relationships"] <- "Household \nrelationships"
  plot_dt$category[plot_dt$category=="Household economics"] <- "Household \neconomics"
  plot_dt$category[plot_dt$vul=="pastoralnoagro.yn"] <- "Household \neconomics"
  plot_dt$category[plot_dt$vul=="med.index.4plus"] <- "Woman and her past experiences / \nhealthcare and mental models"
  
  
  # order the variables by category
  plot_dt$category <- factor(plot_dt$category, 
                    levels = c("Woman and her past experiences / \nhealthcare and mental models", "Household \nrelationships", "Household \neconomics", "Larger \nenvironment"),
                    ordered = TRUE)
  plot_dt <- plot_dt %>% arrange(category)
  plot_dt$vul <- factor(plot_dt$vul, levels = unique(plot_dt$vul), ordered = TRUE)
  plot_dt$vul <- factor(plot_dt$var.label, levels = unique(plot_dt$var.label), ordered = TRUE)
  
  # Plot
  if(plot_text == TRUE){
    if(urban==TRUE){
      p <- plot_dt %>%
        mutate(label = percent(prop, round(2))) %>%
        filter(grepl('^U', cluster)) %>% 
        ggplot(aes(cluster,var.label, fill= prop)) + 
        facet_grid(rows=vars(category),scales="free_y", space = "free_y") +        #seperates out construct
        geom_tile() + 
        geom_text(aes(label = label), color = "grey60") +                                       #heatmat plot
        scale_fill_gradient2(low = "#313695", mid = "#FFFFBF", high = "#A50026", midpoint = 0.5,breaks=seq(0,1,0.1)) +
        ggtitle(title) +xlab('') +ylab('') + labs(fill='Proportion within\ncluster') +
         theme(legend.text=element_text(size=8),  legend.title=element_text(size=10) ) + 
          theme(axis.text = element_text(size=10)) + theme(strip.text = element_text(size = 12))
    } else {
      p <- plot_dt %>%
        mutate(label = percent(prop, round(2))) %>%
        filter(grepl('^R', cluster)) %>% 
                 ggplot(aes(cluster, var.label, fill= prop)) + 
                 facet_grid(rows=vars(category),scales="free_y", space = "free_y") +        #seperates out construct
                 geom_tile() +                                      #heatmat plot
                 geom_text(aes(label = label), color = "grey60") +                                       #heatmat plot
                 scale_fill_gradient2(low = "#313695", mid = "#FFFFBF", high = "#A50026", midpoint = 0.5,breaks=seq(0,1,0.1)) +
                 ggtitle(title) +xlab('') +ylab('') + labs(fill='Proportion within\ncluster') +
                 theme(legend.text=element_text(size=8),  legend.title=element_text(size=10) ) + 
                 theme(axis.text = element_text(size=10)) + theme(strip.text = element_text(size = 12))
    }
  } else {
    if(urban==TRUE){
      p <- plot_dt %>%
        filter(grepl('^U', cluster)) %>% 
        ggplot(cluster, aes(var.label, fill= prop)) + 
        facet_grid(rows=vars(category),scales="free_y", space = "free_y") +        #seperates out construct
        geom_tile() +                                      #heatmat plot
        scale_fill_gradient2(low = "#313695", mid = "#FFFFBF", high = "#A50026", midpoint = 0.5,breaks=seq(0,1,0.1)) +
        ggtitle(title) +xlab('') +ylab('') + labs(fill='Proportion within\ncluster') +
        theme(legend.text=element_text(size=8),  legend.title=element_text(size=10) ) + 
        theme(axis.text = element_text(size=10)) + theme(strip.text = element_text(size = 12))
    } else {
      p <- plot_dt %>%
        filter(grepl('^R', cluster)) %>% 
        ggplot(aes(cluster, var.label, fill= prop)) + 
        facet_grid(rows=vars(category),scales="free_y", space = "free_y") +        #seperates out construct
        geom_tile() +                                  #heatmat plot
        scale_fill_gradient2(low = "#313695", mid = "#FFFFBF", high = "#A50026", midpoint = 0.5,breaks=seq(0,1,0.1)) +
        ggtitle(title) +xlab('') +ylab('') + labs(fill='Proportion within\ncluster') +
        theme(legend.text=element_text(size=8),  legend.title=element_text(size=10) ) + 
        theme(axis.text = element_text(size=10)) + theme(strip.text = element_text(size = 12))
    }
  }
    
  
  return(p)
}
  

## Did not yet update these functions to use the survey package or update colors 
# 
# stacked.cat <- function(dt,var.list=cat.var,constr.map=constr,RcolorPallete='Accent'){
#   var.list2 <- var.list
# 
#   p.list <- list()
#   for(i in 1:length(var.list)){
#     plot_dt <- dt[,c(var.list[i],'cluster')]
#     plot_dt$vul <- as.factor(plot_dt[,var.list[i]])
#     domain <- constr$category[var.list2[i]==constr$f.var.name]
# 
#     if(var.list[i]=='work.seasonal'){
#       plot_dt$vul <- factor(plot_dt$vul,levels=c('No work in the past 12 months','Occassional','Seasonal','All  year'))}
#     if(var.list[i]=='jdwd.index.cat'){
#       plot_dt$vul <- factor(plot_dt$vul,levels=c('Not partnered','None','One to four','Five to six'))}
#     if(var.list[i]=='religionrecode.cat'){
#        plot_dt$vul <- factor(plot_dt$vul,levels=c('muslim','orthodox','protestant','other'))}
# 
#     plot_dt <- plot_dt %>% group_by(cluster,vul) %>% summarize(ct = n()) %>% mutate(pct = round(ct / sum(ct), 2)) 
#     
# 
#     
#     p <-  ggplot(plot_dt, aes(fill=vul, y=pct, x=cluster)) + 
#             geom_bar(position="stack", stat="identity") + coord_cartesian(ylim=c(0,1)) +
#             xlab('') + ylab('Dist. of vul. by cluster') + 
#             scale_fill_brewer(palette = RcolorPallete) +  labs(fill='') +
#             ggtitle(paste0(domain,":\n",var.list2[i])) + theme(plot.title = element_text(size=10)) + 
#             theme(legend.text=element_text(size=6),  legend.title=element_text(size=8) ) + 
#             theme(axis.title.y = element_text(size=8)) 
#       
#     
#     
#     p.list[[i]] <- p
#   }
#   p.fin <- cowplot::plot_grid(plotlist = p.list, ncol = 3)
#   return(p.fin)
# }
# 
# 
# box.cont <- function(dt,var.list=con.var,constr.map=constr){
#   plot_dt <- dt %>% select(all_of(c(var.list,'cluster'))) 
#   plot_dt <- plot_dt %>% tidyr::gather(vul,obs,-cluster)
#   plot_dt <- left_join(plot_dt,constr[,c('f.var.name','category')],by=c('vul'='f.var.name'))
#   plot_dt$facet_label <- paste0(plot_dt$category,':\n',plot_dt$vul)
# 
# 
#   p <- ggplot(plot_dt, aes(x=cluster, y=obs)) + stat_summary(fun.data = calc_stat, geom="boxplot") + 
#        facet_wrap(~facet_label,scale='free_y') +
#         theme_bw() + xlab('') + ylab('Distribution') 
#           
#   
#   return(p)
# }
# 
# 
# calc_stat <- function(x) {
#   coef <- 1.5
#   n <- sum(!is.na(x))
#   # calculate quantiles
#   stats <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
#   names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
#   return(stats)
# }

