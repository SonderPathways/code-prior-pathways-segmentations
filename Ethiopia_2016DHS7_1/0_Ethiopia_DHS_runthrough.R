############################################################################# 
#############################################################################
# Ethiopia_DHS_runthrough
# Ethiopia DHS 2016, Recode 7
# Adi Greif Feb 27 2023
# input data: see individual scripts
# Purpose: run through the project scripts in order in one script to ensure they all work together
# Note: have not tested
# Note: the working directory needs to be changed at the top of each script
############################################################################# 
############################################################################# 


#install.packages

rm(list=ls())


############## Set directory - change for a different user here, and within each script
#setwd("/Users/darcyrao/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Pathways - GE Data & Insights/Modeling") # Darcy
#setwd("/Users/adigreif/Desktop/Gates Ethiopia/EthiopiaSegmentation/") # for use from my Mac
#setwd("/Users/adigreif/Desktop/Gates Ethiopia/EthiopiaSegmentation/")
setwd("C:/Users/anumi/OneDrive - Bill & Melinda Gates Foundation/All Pathways/WS06 Pathways/Pathways/Modeling/")

############### Run scripts in order 

source("Code/1_Ethiopia_DHS_Outcomes.R")
    #this script constructs outcome variables

source("Code/4_Ethiopia_DHS_vulnerability_measures.R")
    #constructs vulnerability variables
    #note vulnerability script also calls 2_dhs_swper and 3_Ethiopia_DHS_pastoralism


source("Code/5_Ethiopia_DHS_exploratory.R")
    #note: variable exploration here that requires changing variable names by hand
    #the exploratory file also saves the final version of the dataset

source("Code/6_Ethiopia_age_explore.R")
    #note: variable exploration here that requires changing variable names by hand 

source("Code/7_Ethiopia_DHS_construct_mapping.R")
    #only needs to be run once to construct the excel constructs for vulnerabilities, used in the LCA (and PAM) analyses

#source("Code/8_Ethiopia_DHS_PCA_Rmd.Rmd")
    #runs PCA analyis, plots used to decide on variables for use in cluster analysis 
    #have not tested to see if source command works with R markdown files

#source("Code/9a1_Ethiopia_DHS_LCA")
    #Purposes: conducts LCA cluster analysis repeating many times to see if it truly converges (k=2 to 10), not necessary to replicate results
    #requires high processing power


source("Code/9a2_Ethiopia_DHS_LCA")
    #runs cluster analysis to determine number of clusters k using plots and BIC values
    #calls 10_Ethiopia_DHS_cluster_plots

source("Code/10a2_Ethiopia_DHS_clusters")
    #Purposes: creates segment profiling
    #calls  10a1_Ethiopia_DHS_cluster_plots

source("Code/11_Ethiopia_DHS_CART")
    #conducts CART to classify women based on what predicts cluster membership
    #note currently this creates a full dataset merged back with DHS data that also includes clusters

##2011 validation scripts
#script 3b, 4b replicate scripts 3 and 4 above with 2011 data, 12 applies the typing tool and 13 a tests the differences between clusters on 2011 data. 

##PAM scripts
#source("Code/9b_Ethiopia_DHS_PAMclusters")
    #Purposes: does PAM cluster analysis and plots results - use if have low computing resources and are avoiding LCA,  not necessary to replicate results
    #calls  10b_Ethiopia_DHS_cluster_plots

##Age robustness checks
#source("Code/9a3_Ethiopia_DHS_LCA")
    #renaming labels for sankey diagram without age,  not necessary to replicate results
     #calls 10_Ethiopia_DHS_cluster_plots

#source("Code/9a4_Ethiopia_DHS_LCA")
    #renaming labels for sankey diagram with age,  not necessary to replicate results
     #calls 10_Ethiopia_DHS_cluster_plots