#### 10b_Ethiopia_DHS_cluster_plots_PAM
####purpose: assigns variables to constructs and creates functions called in scripts 9 to plot cluster results 
### input: 7_Construct mapping_external.xlsx",sheet=1, finalvarlist_pam
### constructed by Anu Mishra
### edits by AG
### note: working directory needs to be set for each user 


pacman::p_load(gridExtra)
#library(gridExtra)

# set working directory - needs changing for each user
setwd("/Users/adigreif/Desktop/Gates Ethiopia/EthiopiaSegmentation/")


##reading in construct mapping
constr <- readxl::read_excel("7_Construct_mapping_external.xlsx",sheet="sheet1")
constr <- constr[!duplicated(constr$f.var.name),]
constr$category[constr$f.var.name=='early.birth.15'] <-  "Woman and her past experiences"


### categorize variables as binary/categorical/continuous
#var <- readxl::read_xlsx("7_Construct_mapping_external.xlsx",sheet = "final_var_list_PAM")
#var <- var[!duplicated(var$f.var.name),]
#var$f.var.name[var$f.var.name=='pastoralnoagro'] <- 'pastoralnoagro' #note, tried using both agropastoral.yn (inconsistently named anypastoral.yn in some places) and pastoralnoagro

#con.var <- c(var$f.var.name[var$variable_type=='continuous'],'conflict_incidents')
#cat.var <- var$f.var.name[var$variable_type=='categorical' | var$variable_type=='ordinal']
#bin.var <- c(var$f.var.name[var$variable_type=='binary'])

con.var <- c('num.15up','conflict_incidents', 'num.kids.house', 'highestyearsedinHH.yrs', 'age.yrs')
cat.var <- c('wealth.index.ur.ord', 'dv.index.ord', 'med.index.ord','conflict_incidents', 'jdwd.index.cat', 'pship.cat', 'religionrecode.cat', 'work.seasonal')
bin.var <- c('working.yn', 'partner.working.yn','med.permis','hh.electricity','any.media.yn','anyed.yn','early.sex.15','pastoralnoagro')

### Anu needs this for her analysis
if(method.poLCA==TRUE ){
  bin.var <- c(bin.var,'num.15up.4plus','num.kids.house.4plus')
  cat.var <- c(cat.var, 'highestyearsedinHH.yrs','conflict_incidents')
  
  constr$f.var.name[constr$f.var.name=='num.15up'] <- 'num.15up.4plus'
  constr$f.var.name[constr$f.var.name=='num.kids.house'] <- 'num.kids.house.4plus'
}
if(method.poLCA==FALSE ){
  cat.var[cat.var=='med.index'] <- 'med.index.ord' 
  cat.var[cat.var=='dv.index'] <- 'dv.index.ord' 
}


heatmap.bin <- function(dt,var.list=bin.var,urban=TRUE,constr.map=constr,title=''){
  if(urban==TRUE){
    var.list <- var.list[!var.list %in% 'pastoralnoagro']
    #dt$slum1 <- ifelse(dt$slum1=='urban slum',1,0)
  }
  else{    var.list <- var.list[!var.list %in% 'slum1']}
  
  plot_dt <- dt[,c(var.list,'cluster')]
  plot_dt <- aggregate(plot_dt[,var.list],by=list(plot_dt$cluster),mean)
  names(plot_dt)[1] <- 'cluster'
    #vul is for vulnerability
  plot_dt <- plot_dt %>% tidyr::gather(vul,prop,-cluster)
  plot_dt <- left_join(plot_dt,constr[,c('category','f.var.name')],by=c('vul'='f.var.name'))
  plot_dt$category[plot_dt$category=="Woman and her past experiences"] <- "Woman and her\npast experiences"
  plot_dt$category[plot_dt$category=="Larger environment"] <- "Larger\nenvironment"
  plot_dt$category[plot_dt$category=="Household relationships"] <- "Household\nrelationships"
  plot_dt$category[plot_dt$category=="Household economics"] <- "Household\neconomics"
  
  
  p <- ggplot(plot_dt, aes(cluster, vul, fill= prop)) + 
    facet_wrap(~category,nrow=1,scales="free_y") +        #seperates out construct
    geom_tile() +                                         #heatmat plot
    scale_fill_gradient2(low = "deepskyblue3", mid = "ivory", high = "darkorange", midpoint = 0.5,breaks=seq(0,1,0.1)) +
    ggtitle(title) +xlab('') +ylab('') + labs(fill='Proportion of\nvulnerability\nwithin cluster') +
    theme(legend.text=element_text(size=6),  legend.title=element_text(size=8) ) + 
    theme(axis.text.y = element_text(size=8,angle = 45))
  
  
  
  return(p)
}



stacked.cat <- function(dt,var.list=cat.var,constr.map=constr){
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
      scale_fill_brewer(palette = 'Accent') +  labs(fill='') +
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

