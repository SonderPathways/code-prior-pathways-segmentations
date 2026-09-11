##### Constructs survey-based women's empowerment index from DHS data ###################
## called by script: 4_Indonesia_DHS_vulnerability ###########

computeSWPER <- function(dhsdata) {
  if (is.data.table(dhsdata)) {
    dhsdata = as.data.frame(dhsdata)
  }
  
  
  list.swper <- data.frame(itemname = c("Beating justified if wife goes out without telling husband",
                                        "Beating justified if wife neglects the children",
                                        "Beating justified if wife argues with husband",
                                        "Beating justified if wife refuses to have sex with husband",
                                        "Beating justified if wife burns the food",
                                        "Frequency of reading newspaper or magazine",
                                        "Woman education",
                                        "Age of respondent at cohabitation",
                                        "Age of respondent at first birth",
                                        "Age difference: woman?s minus husband?s age",
                                        "Education difference: woman?s minus husband?s years of schooling",
                                        "Who usually decides on respondent's health care",
                                        "Who usually decides on large household purchases",
                                        "Who usually decides on visits to family or relatives"),
                           variable = c("v744a","v744b","v744c","v744d","v744e",
                                        "v157","v133","v511","v212","AGE.DIFF",
                                        "EDUCATION.DIFF","v743a","v743b","v743d"),
                           weight1 = c(0.508,0.508,0.526,0.538,0.588,
                                       0.083,0.016,-0.006,-0.010,0.001,
                                       0.002,0.001,-0.017,0.002),
                           weight2 = c(-0.012,-0.026,0.001,0.001,-0.015,
                                       0.422,0.081,0.133,0.139,0.031,
                                       0.054,-0.004,-0.022,-0.034),
                           weight3 = c(-0.003,-0.040,0.007,0.028,-0.020,
                                       0.121,0.022,-0.012,-0.016,0.013,
                                       0.001,0.599,0.601,0.619))
  tempdat <- as.data.frame(dhsdata[,intersect(list.swper$variable, names(dhsdata))])
  tempdat$AGE.DIFF <- dhsdata$v012-dhsdata$v730
  if (sum(!is.na(dhsdata$v715))>0) { # husband's years of education is available
    tempdat$EDUCATION.DIFF <- dhsdata$v133-dhsdata$v715
    tempdat$EDUCATION.DIFF[(dhsdata$v715 %in% c(97,98)) |
                             (dhsdata$v133 %in% c(97,98))] <- NA
  } else { # husband's years of education is not available
    ed.woman <- paste(dhsdata$v106, dhsdata$v107) # woman highest education level + years at level
    ed.husband <- paste(dhsdata$v701, ifelse(dhsdata$v702==98,1,dhsdata$v702)) # husband highest education level + years at level
    years.husband <- dhsdata$v133[match(ed.husband,ed.woman)] # estimate husband total years of education
    years.husband[is.na(years.husband) & as_factor(dhsdata$v701)=="no education"] <- 0
    tempdat$EDUCATION.DIFF <- ifelse(as_factor(dhsdata$v106)=="no education",0,dhsdata$v133)-years.husband
  }
  tempdat <- tempdat[,match(list.swper$variable, colnames(tempdat))] # re-arrange the columns to match list.swper order
  
  for (i in which(grepl("Beating",list.swper$itemname))) {
    tempdat[,i] <- ifelse(as_factor(tempdat[,i])=="yes",-1,
                          ifelse(as_factor(tempdat[,i])=="no",1,0))
  }
  tempdat[,6] <- ifelse(as_factor(tempdat[,6])=="not at all",0,
                        ifelse(as_factor(tempdat[,6])=="less than once a week",1,
                               ifelse(as_factor(tempdat[,6]) %in% c("at least once a week","almost every day"), 2, NA)))
  tempdat[tempdat[,7] %in% c(98),7] <- NA
  for (i in which(grepl("Who usually decides",list.swper$itemname))) {
    tempdat[,i] <- ifelse(as_factor(tempdat[,i]) %in% c("respondent alone","respondent and husband/partner","respondent and other person"),1,
                          ifelse(as_factor(tempdat[,i]) %in% c("husband/partner alone","someone else","other"),-1,NA))
  }
  # compute SWPER scores
  data.swperscore <- data.frame (SWPER.SCORE.1 = ((-1.202 + (as.matrix(tempdat) %*% as.matrix(x=list.swper$weight1))) - 0.000)/1.811,
                                 SWPER.SCORE.2 = ((-5.661 + (as.matrix(tempdat) %*% as.matrix(x=list.swper$weight2))) - 0.000)/1.526,
                                 SWPER.SCORE.3 = ((-0.168 + (as.matrix(tempdat) %*% as.matrix(x=list.swper$weight3))) - 0.000)/1.502)
  data.swperscore$SWPER.EMPOWERMENT.1 <- factor(x=ifelse(is.na(data.swperscore$SWPER.SCORE.1), NA,
                                                         ifelse(data.swperscore$SWPER.SCORE.1<=-0.700, "low",
                                                                ifelse(data.swperscore$SWPER.SCORE.1<=0.400, "medium",
                                                                       ifelse(data.swperscore$SWPER.SCORE.1>0.400, "high",NA)))),
                                                levels=c("low","medium","high"), ordered=TRUE)
  data.swperscore$SWPER.EMPOWERMENT.2 <- factor(x=ifelse(is.na(data.swperscore$SWPER.SCORE.2), NA,
                                                         ifelse(data.swperscore$SWPER.SCORE.2<=-0.559, "low",
                                                                ifelse(data.swperscore$SWPER.SCORE.2<=0.293, "medium",
                                                                       ifelse(data.swperscore$SWPER.SCORE.2>0.293, "high",NA)))),
                                                levels=c("low","medium","high"), ordered=TRUE)
  data.swperscore$SWPER.EMPOWERMENT.3 <- factor(x=ifelse(is.na(data.swperscore$SWPER.SCORE.3), NA,
                                                         ifelse(data.swperscore$SWPER.SCORE.3<=-1.000, "low",
                                                                ifelse(data.swperscore$SWPER.SCORE.3<=0.600, "medium",
                                                                       ifelse(data.swperscore$SWPER.SCORE.3>0.600, "high",NA)))),
                                                levels=c("low","medium","high"), ordered=TRUE)
  rm(tempdat)
  data.swperscore
}