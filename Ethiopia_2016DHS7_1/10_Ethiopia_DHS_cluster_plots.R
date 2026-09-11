#### 10_Ethiopia_DHS_cluster_plots
####purpose: assigns variables to constructs and creates functions called in scripts 9 to plot cluster results 
### input: 7_Construct_mapping_external_final.xlsx",sheet=1, sheet=final_var_list (used to be sheet3)
### Note: although sheet1 is constructed in script 7, the final_var_list sheet is constructed by hand after reviewing the PCA results in script 8 and the first round of ###cluster analysis. The notes and decisions are in 7_Construct_mapping_external_final.
### constructed by Anu Mishra
### edits by AG April 15: pastoralnoagro.yn instead of anypastoral.yn, they are not the same although what we name it doesn't change the data
### note: working directory needs to be set for each user 
### note: no longer need the code on poLCA =false, since the PAM analysis is secondary, and also now has its own script 10b to run instead of this one

rm(list=ls())

pacman::p_load(gridExtra, cowplot)
#library(gridExtra)

## Set working directory - change for different users
#setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/") # Anu
setwd("/Users/darcyrao/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Pathways - GE Data & Insights/Modeling") # for use from Darcy's Mac
#setwd("/Users/darcyrao/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Pathways - GE Data & Insights/Modeling") # for use from Darcy's Mac

##reading in construct mapping
constr <- readxl::read_excel("Data/Ethiopia_constructed_data/7_Construct_mapping_external_final.xlsx",sheet="Sheet1")
constr <- constr[!duplicated(constr$f.var.name),]
constr$category[constr$f.var.name=='early.birth.15'] <-  "Woman and her past experiences"


### categorize variables as binary/categorical/continuous
var <- readxl::read_xlsx("Data/Ethiopia_constructed_data/7_Construct_mapping_external_final.xlsx",sheet = "final_var_list")
#var <- var[is.na(var$Cut),] #AG commenting out - no longer necessary
var <- var[!duplicated(var$f.var.name),]
var$f.var.name[var$f.var.name=='pastoralnoagro.yn'] <- 'pastoralnoagro.yn' # this had anypastoral.yn, which is not the same variable.

con.var <- c(var$f.var.name[var$variable_type=='continuous'],'conflict_incidents_26plus')
cat.var <- c(var$f.var.name[var$variable_type=='categorical' | var$variable_type=='ordinal'],'age.adj') # AG: 7_Construct_mapping_external_final now already has age.adj                                                                                                            #so do not need to add it separately
bin.var <- c(var$f.var.name[var$variable_type=='binary'])

### Anu needs this for her analysis
method.poLCA=TRUE #selected this method rather than PAM

if(method.poLCA==TRUE ){
  bin.var <- c(bin.var,'num.15up.4plus','num.kids.house.4plus') # AG: 7_Construct_mapping_external_final now already has num.15up.4plus and kids.house.4plus,                                                                           #no need to mention them separately
  cat.var <- c(cat.var, 'highestyearsedinHH.yrs','conflict_incidents')
  
  #constr$f.var.name[constr$f.var.name=='num.15up'] <- 'num.15up.4plus' 
  #constr$f.var.name[constr$f.var.name=='num.kids.house'] <- 'num.kids.house.4plus'
}
if(method.poLCA==FALSE ){
  cat.var[var.list=='med.index'] <- 'med.index.ord' 
  cat.var[var.list=='dv.index'] <- 'dv.index.ord' 
}

heatmap.bin <- function(dt,var.list=bin.var,urban=TRUE,constr.map=constr,title=''){
  if(urban==TRUE){
    var.list <- var.list[!var.list %in% 'pastoralnoagro.yn'] 
    dt$slum1 <- ifelse(dt$slum1=='urban slum',1,0)
  }
  else{    var.list <- var.list[!var.list %in% 'slum1']}
  
  plot_dt <- dt[,c(var.list,'cluster')]
  plot_dt <- aggregate(plot_dt[,var.list],by=list(plot_dt$cluster),mean)
  names(plot_dt)[1] <- 'cluster'
  
  plot_dt <- plot_dt %>% tidyr::gather(vul,prop,-cluster)
  plot_dt <- left_join(plot_dt,constr[,c('category','f.var.name')],by=c('vul'='f.var.name'))
  plot_dt$category[plot_dt$category=="Woman and her past experiences"] <- "Woman and her\npast experiences"
  plot_dt$category[plot_dt$category=="Larger environment"] <- "Larger\nenvironment"
  plot_dt$category[plot_dt$category=="Household relationships"] <- "Household\nrelationships"
  plot_dt$category[plot_dt$category=="Household economics"] <- "Household\neconomics"

  
  p <- ggplot(plot_dt, aes(cluster, vul, fill= prop)) + 
    facet_wrap(~category,nrow=1,scales="free_y") +        #seperates out construct
    geom_tile() +                                         #heatmat plot
    scale_fill_gradient2(low = "mediumseagreen", mid = "ivory", high = "coral", midpoint = 0.5,breaks=seq(0,1,0.1)) +
    ggtitle(title) +xlab('') +ylab('') + labs(fill='Proportion of\nvulnerability\nwithin cluster') +
     theme(legend.text=element_text(size=6),  legend.title=element_text(size=8) ) + 
      theme(axis.text.y = element_text(size=8,angle = 45))
  
  
  
  return(p)
}
  


stacked.cat <- function(dt,var.list=cat.var,constr.map=constr,RcolorPallete='Accent'){
  var.list2 <- var.list

  p.list <- list()
  for(i in 1:length(var.list)){
    plot_dt <- dt[,c(var.list[i],'cluster')]
    plot_dt$vul <- as.factor(plot_dt[,var.list[i]])
    domain <- constr$category[var.list2[i]==constr$f.var.name]

    if(var.list[i]=='work.seasonal'){
      plot_dt$vul <- factor(plot_dt$vul,levels=c('No work in the past 12 months','Occassional','Seasonal','All  year'))}
    if(var.list[i]=='jdwd.index.cat'){
      plot_dt$vul <- factor(plot_dt$vul,levels=c('Not partnered','None','One to four','Five to six'))}
    if(var.list[i]=='religionrecode.cat'){
       plot_dt$vul <- factor(plot_dt$vul,levels=c('muslim','orthodox','protestant','other'))}

    plot_dt <- plot_dt %>% group_by(cluster,vul) %>% summarize(ct = n()) %>% mutate(pct = round(ct / sum(ct), 2)) 
    

    
    p <-  ggplot(plot_dt, aes(fill=vul, y=pct, x=cluster)) + 
            geom_bar(position="stack", stat="identity") + coord_cartesian(ylim=c(0,1)) +
            xlab('') + ylab('Dist. of vul. by cluster') + 
            scale_fill_brewer(palette = RcolorPallete) +  labs(fill='') +
            ggtitle(paste0(domain,":\n",var.list2[i])) + theme(plot.title = element_text(size=10)) + 
            theme(legend.text=element_text(size=6),  legend.title=element_text(size=8) ) + 
            theme(axis.title.y = element_text(size=8)) 
      
    
    
    p.list[[i]] <- p
  }
  p.fin <- cowplot::plot_grid(plotlist = p.list, ncol = 3)
  return(p.fin)
}


box.cont <- function(dt,var.list=con.var,constr.map=constr){
  plot_dt <- dt %>% select(all_of(c(var.list,'cluster'))) 
  plot_dt <- plot_dt %>% tidyr::gather(vul,obs,-cluster)
  plot_dt <- left_join(plot_dt,constr[,c('f.var.name','category')],by=c('vul'='f.var.name'))
  plot_dt$facet_label <- paste0(plot_dt$category,':\n',plot_dt$vul)


  p <- ggplot(plot_dt, aes(x=cluster, y=obs)) + stat_summary(fun.data = calc_stat, geom="boxplot") + 
       facet_wrap(~facet_label,scale='free_y') +
        theme_bw() + xlab('') + ylab('Distribution') 
          
  
  return(p)
}


calc_stat <- function(x) {
  coef <- 1.5
  n <- sum(!is.na(x))
  # calculate quantiles
  stats <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(stats)
}

