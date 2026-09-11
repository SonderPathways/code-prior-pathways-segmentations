


## Kasi



rm(list = ls())
gc()

#install.packages("installr")
library(installr)

#install.packages("data.table")
library(data.table)

#install.packages("ggplot2")
library(ggplot2)


#updateR()





############################
## Data Prep and Vaariable Definiiton
############################

data <- as.data.frame(fread("C:/Users/johnvand/Desktop/child_vaccine_survey.csv"))
data$segment <- ""
data$segment2 <- ""



#data <- data[which(data$`45.Gender`== "Female"), ]
data$personal_ed <- factor(data$`35. What is the highest level of education you have completed?`, levels = c("No education", "Incomplete primary", "Complete primary", "Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)"))
data$partner_ed <- factor(data$`36. What is the highest level of education that your partner has completed?`, levels = c("No partner","No education", "Incomplete primary", "Complete primary", "Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)"))
data$religion <- factor(data$`34. What religion do you identify with?`)
data$hh <- factor(data$`33. Is the head of your household a male or a female?`)
data$partner_prof <- (data$`37. What does your partner do for work?`== "Professional, technical, managerial")
data$home_birth <- (data$`41. Have you ever given birth at home?`=="Yes")
data$anc <- as.factor(data$`40. How many times did you receive antenatal care during your last pregnancy?`)
data$radio <- as.factor(data$`43. How often do you listen to the radio?`)
data$tv <- as.factor(data$`44. How often do you watch TV`)
contra <- grep('Condom|condom|sterilization|IUD|Injectables|Implants|pill|Emergency|Modern', data$`42. Which ways or methods are you or your partner using to avoid pregnancy, if any? `, value = TRUE)
data$modern_contra <- data$`42. Which ways or methods are you or your partner using to avoid pregnancy, if any? ` %in% contra
data$bio_kids <- as.numeric(data$`39. How many biological children do you have?`)
data <- data[which(data$bio_kids > 0), ]
data$assets <- 0
data$assets[which(data$`38. Does your household own or have : Bank account` == "Yes")] <- (data$assets[which(data$`38. Does your household own or have : Bank account` == "Yes")] + 1)
data$assets[which(data$`38. Does your household own or have : Car/Truck` == "Yes")] <- (data$assets[which(data$`38. Does your household own or have : Car/Truck` == "Yes")] + 1)
data$assets[which(data$`38. Does your household own or have : Cell phone` == "Yes")] <- (data$assets[which(data$`38. Does your household own or have : Cell phone` == "Yes")] + 1)
data$assets[which(data$`38. Does your household own or have : Motorcycle/Scooter` == "Yes")] <- (data$assets[which(data$`38. Does your household own or have : Motorcycle/Scooter` == "Yes")] + 1)

################################################
## Nigerian Urban Typing Tool Logic (each end point gets one line of code)
################################################


############################################################
### Urban Nigerian Male Headed households UM1, UM3, UM4, UM5
############################################################

# UM1 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("No education", "Incomplete primary", "Complete primary") & 
                     data$religion != "Muslim" &
                     data$anc != "4 or more" &
                     data$radio != "At least once a week")] <- "UM1"
# UM1 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("No education", "Incomplete primary", "Complete primary") & 
                     data$religion != "Muslim" &
                     data$anc != "4 or more" &
                     data$radio == "At least once a week" &
                     data$partner_ed %in% c("No partner","No education", "Incomplete primary", "Complete primary"))] <- "UM1"
# UM1 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("No education", "Incomplete primary", "Complete primary") & 
                     data$religion != "Muslim" &
                     data$anc != "4 or more" &
                     data$radio == "At least once a week" &
                     data$partner_ed %in% c("Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)") &
                     data$modern_contra == FALSE)] <- "UM1"
# UM5 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("No education", "Incomplete primary", "Complete primary") & 
                     data$religion != "Muslim" &
                     data$anc != "4 or more" &
                     data$radio == "At least once a week" &
                     data$partner_ed %in% c("Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)") &
                     data$modern_contra == TRUE)] <- "UM5"
# UM1 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("No education", "Incomplete primary", "Complete primary") & 
                     data$religion != "Muslim" &
                     data$anc == "4 or more" &
                     data$home_birth == TRUE)] <- "UM1"
# UM1 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("No education", "Incomplete primary") & 
                     data$religion != "Muslim" &
                     data$anc == "4 or more" &
                     data$home_birth != TRUE)] <- "UM1"
# UM1 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Complete primary") & 
                     data$religion != "Muslim" &
                     data$anc == "4 or more" &
                     data$home_birth != TRUE &
                     data$radio == "Not at all")] <- "UM1"
# UM3 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Complete primary") & 
                     data$religion != "Muslim" &
                     data$anc == "4 or more" &
                     data$home_birth != TRUE &
                     data$radio != "Not at all" &
                     data$partner_prof == TRUE)] <- "UM3"
# UM5 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Complete primary") & 
                     data$religion != "Muslim" &
                     data$anc == "4 or more" &
                     data$home_birth != TRUE &
                     data$radio != "Not at all" &
                     data$partner_prof != TRUE)] <- "UM5"
# UM1 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("No education") & 
                     data$religion == "Muslim")] <- "UM1"
# UM1 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Incomplete primary", "Complete primary") & 
                     data$religion == "Muslim" &
                     data$partner_ed %in% c("No partner","No education", "Incomplete primary", "Complete primary", "Incomplete secondary"))] <- "UM1"
# UM4 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Incomplete primary", "Complete primary") & 
                     data$religion == "Muslim" &
                     data$partner_ed %in% c("Complete secondary", "Higher education (e.g., college, university)"))] <- "UM4"
# UM4 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)") & 
                     data$partner_prof != TRUE &
                     data$religion == "Muslim")] <- "UM4"
# UM5 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)") & 
                     data$partner_prof != TRUE &
                     data$religion != "Muslim" &
                     data$anc == "4 or more")] <- "UM5"
# UM5 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)") & 
                     data$partner_prof != TRUE &
                     data$religion != "Muslim" &
                     data$anc != "4 or more" &
                     data$bio_kids < 4)] <- "UM5"
# UM5 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)") & 
                     data$partner_prof != TRUE &
                     data$religion != "Muslim" &
                     data$anc != "4 or more" &
                     data$bio_kids > 3 &
                     data$modern_contra == TRUE)] <- "UM5"
# UM4 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)") & 
                     data$partner_prof != TRUE &
                     data$religion != "Muslim" &
                     data$anc != "4 or more" &
                     data$bio_kids > 3 &
                     data$modern_contra != TRUE &
                     data$partner_ed %in% c("Complete secondary", "Higher education (e.g., college, university)"))] <- "UM4"
# UM1 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)") & 
                     data$partner_prof != TRUE &
                     data$religion != "Muslim" &
                     data$anc != "4 or more" &
                     data$bio_kids > 3 &
                     data$modern_contra != TRUE &
                     data$partner_ed %in% c("No partner","No education", "Incomplete primary", "Complete primary", "Incomplete secondary") &
                     data$assets < 3)] <- "UM1"
# UM5 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)") & 
                     data$partner_prof != TRUE &
                     data$religion != "Muslim" &
                     data$anc != "4 or more" &
                     data$bio_kids > 3 &
                     data$modern_contra != TRUE &
                     data$partner_ed %in% c("No partner","No education", "Incomplete primary", "Complete primary", "Incomplete secondary") &
                     data$assets > 2)] <- "UM5"
# UM3 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)") & 
                     data$partner_prof == TRUE &
                     data$partner_ed %in% c("Higher education (e.g., college, university)"))] <- "UM3"
# UM4 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)") & 
                     data$partner_prof == TRUE &
                     data$partner_ed != "Higher education (e.g., college, university)" &
                     data$religion == "Muslim")] <- "UM4"
# UM5 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)") & 
                     data$partner_prof == TRUE &
                     data$partner_ed != "Higher education (e.g., college, university)" &
                     data$religion != "Muslim" &
                     data$bio_kids < 4)] <- "UM5"
# UM5 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)") & 
                     data$partner_prof == TRUE &
                     data$partner_ed != "Higher education (e.g., college, university)" &
                     data$religion != "Muslim" &
                     data$bio_kids > 3 &
                     data$anc == "4 or more")] <- "UM5"
# UM4 ##############################
data$segment[which(data$hh == "Male" & 
                     data$personal_ed %in% c("Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)") & 
                     data$partner_prof == TRUE &
                     data$partner_ed != "Higher education (e.g., college, university)" &
                     data$religion != "Muslim" &
                     data$bio_kids > 3 &
                     data$anc != "4 or more")] <- "UM4"



##########################################################
### Urban Nigerian FEmale headed households - UF1 and UF3
##########################################################

# UF1 ##############################
data$segment[which(data$hh == "Female" & 
                     data$personal_ed %in% c("Complete secondary", "Higher education (e.g., college, university)") &
                     data$assets < 2 &
                     data$bio_kids < 2 &
                     data$anc != "4 or more")] <- "UF1"
# UF3 ##############################
data$segment[which(data$hh == "Female" & 
                     data$personal_ed %in% c("Complete secondary", "Higher education (e.g., college, university)") &
                     data$assets < 2 &
                     data$bio_kids < 2 &
                     data$anc == "4 or more")] <- "UF3"
# UF1 ##############################
data$segment[which(data$hh == "Female" & 
                     data$personal_ed %in% c("Complete secondary", "Higher education (e.g., college, university)") &
                     data$assets < 2 &
                     data$bio_kids > 1)] <- "UF1"
# UF3 ##############################
data$segment[which(data$hh == "Female" & 
                     data$personal_ed %in% c("Complete secondary", "Higher education (e.g., college, university)") &
                     data$assets > 1 &
                     data$bio_kids < 5)] <- "UF3"
# UF3 ##############################
data$segment[which(data$hh == "Female" & 
                     data$personal_ed %in% c("Complete secondary", "Higher education (e.g., college, university)") &
                     data$assets > 1 &
                     data$bio_kids < 5)] <- "UF3"
# UF1 ##############################
data$segment[which(data$hh == "Female" & 
                     data$personal_ed %in% c("Complete secondary", "Higher education (e.g., college, university)") &
                     data$assets > 1 &
                     data$bio_kids > 4)] <- "UF1"
# UF1 ##############################
data$segment[which(data$hh == "Female" & 
                     data$personal_ed %in% c("No education", "Incomplete primary", "Complete primary", "Incomplete secondary") &
                     data$assets < 2 )] <- "UF1"
# UF1 ##############################
data$segment[which(data$hh == "Female" & 
                     data$personal_ed %in% c("No education", "Incomplete primary", "Complete primary", "Incomplete secondary") &
                     data$assets > 1 &
                     data$bio_kids > 4)] <- "UF1"
# UF3 ##############################
data$segment[which(data$hh == "Female" & 
                     data$personal_ed %in% c("No education", "Incomplete primary", "Complete primary", "Incomplete secondary") &
                     data$assets > 1 &
                     data$bio_kids < 5 &
                     data$anc == "4 or more")] <- "UF3"
# UF1 ##############################
data$segment[which(data$hh == "Female" & 
                     data$personal_ed %in% c("No education", "Incomplete primary", "Complete primary", "Incomplete secondary") &
                     data$assets > 1 &
                     data$bio_kids < 5 &
                     data$anc != "4 or more")] <- "UF1"


# remove men as the typing tool assumes you are typing women ####################
data$segment[which(data$`45.Gender`!= "Female")] <- ""

table(data$segment)


#####################

################################################
## Kenyan Urban Typing Tool Logic (each end point gets one line of code)
################################################

####################################################
### Urban Kenyan Male Headed Households UM1 and UM5
####################################################

# UM1 ##############################
data$segment2[which(data$hh == "Male" & 
                     data$personal_ed %in% c("No education"))] <- "UM1"
# UM5 ##############################
data$segment2[which(data$hh == "Male" & 
                      data$personal_ed %in% c("Complete primary", "Incomplete secondary", "Complete secondary", "Higher education (e.g., college, university)"))] <- "UM5"
# UM1 ##############################
data$segment2[which(data$hh == "Male" & 
                      data$personal_ed %in% c("Incomplete primary") &
                      data$bio_kids < 4 &
                      data$tv == "Not at all")] <- "UM1"
# UM5 ##############################
data$segment2[which(data$hh == "Male" & 
                      data$personal_ed %in% c("Incomplete primary") &
                      data$bio_kids < 4 &
                      data$tv != "Not at all")] <- "UM5"
# UM1 ##############################
data$segment2[which(data$hh == "Male" & 
                      data$personal_ed %in% c("Incomplete primary") &
                      data$bio_kids > 3 &
                      data$radio == "Not at all")] <- "UM1"
# UM5 ##############################
data$segment2[which(data$hh == "Male" & 
                      data$personal_ed %in% c("Incomplete primary") &
                      data$bio_kids > 3 &
                      data$radio != "Not at all")] <- "UM5"

#####################################################
### Urban Kenyan Female Headed Households UF1 and UF3
#####################################################

# UF1 ##############################
data$segment2[which(data$hh == "Female" & 
                      data$assets < 2 &
                      data$bio_kids < 4 &
                      data$radio == "Not at all")] <- "UF1"
# UF3 ##############################
data$segment2[which(data$hh == "Female" & 
                      data$assets < 2 &
                      data$bio_kids < 4 &
                      data$radio != "Not at all")] <- "UF3"
# UF1 ##############################
data$segment2[which(data$hh == "Female" & 
                      data$assets < 2 &
                      data$bio_kids > 3)] <- "UF1"
# UF3 ##############################
data$segment2[which(data$hh == "Female" & 
                      data$assets > 1 &
                      data$bio_kids < 4)] <- "UF3"
# UF3 ##############################
data$segment2[which(data$hh == "Female" & 
                      data$assets > 1 &
                      data$bio_kids < 4)] <- "UF3"
# UF1 ##############################
data$segment2[which(data$hh == "Female" & 
                      data$assets > 1 &
                      data$bio_kids > 3 &
                      data$radio == "Not at all")] <- "UF1"
# UF3 ##############################
data$segment2[which(data$hh == "Female" & 
                      data$assets > 1 &
                      data$bio_kids > 3 &
                      data$radio != "Not at all")] <- "UF3"




# remove men as the typing tool assumes you are typing women ####################
data$segment2[which(data$`45.Gender`!= "Female")] <- ""

table(data$segment2)



#####################




table(data$segment, data$segment2)
## This shows that the Nigerian Logic places more women in vulnerable segments, 
## the Kenyan logic places more women in less vulnerable segments.

data$NGA_Logic_Segment <- data$segment
data$KEN_Logic_Segment <- data$segment2



gg_NGA_total <- ggplot() + geom_bar(data = data, aes(x = NGA_Logic_Segment, fill = country))

gg_NGA_Cameroon <- ggplot() + geom_bar(data = data[which(data$country == "Cameroon"), ], aes(x = NGA_Logic_Segment, fill = country))
gg_NGA_Cote <- ggplot() + geom_bar(data = data[which(data$country == "Cote d'Ivoire"), ], aes(x = NGA_Logic_Segment, fill = country))
gg_NGA_DRC <- ggplot() + geom_bar(data = data[which(data$country == "DRC- Democratic republic of Congo"), ], aes(x = NGA_Logic_Segment, fill = country))
gg_NGA_Ghana <- ggplot() + geom_bar(data = data[which(data$country == "Ghana"), ], aes(x = NGA_Logic_Segment, fill = country))
gg_NGA_Kenya <- ggplot() + geom_bar(data = data[which(data$country == "Kenya"), ], aes(x = NGA_Logic_Segment, fill = country))
gg_NGA_Nigeria <- ggplot() + geom_bar(data = data[which(data$country == "Nigeria"), ], aes(x = NGA_Logic_Segment, fill = country))
gg_NGA_SouthAfrica <- ggplot() + geom_bar(data = data[which(data$country == "South Africa"), ], aes(x = NGA_Logic_Segment, fill = country))
gg_NGA_Tanzania <- ggplot() + geom_bar(data = data[which(data$country == "Tanzania"), ], aes(x = NGA_Logic_Segment, fill = country))



gg_KEN_total <- ggplot() + geom_bar(data = data, aes(x = KEN_Logic_Segment, fill = country))

gg_KEN_Cameroon <- ggplot() + geom_bar(data = data[which(data$country == "Cameroon"), ], aes(x = KEN_Logic_Segment, fill = country))
gg_KEN_Cote <- ggplot() + geom_bar(data = data[which(data$country == "Cote d'Ivoire"), ], aes(x = KEN_Logic_Segment, fill = country))
gg_KEN_DRC <- ggplot() + geom_bar(data = data[which(data$country == "DRC- Democratic republic of Congo"), ], aes(x = KEN_Logic_Segment, fill = country))
gg_KEN_Ghana <- ggplot() + geom_bar(data = data[which(data$country == "Ghana"), ], aes(x = KEN_Logic_Segment, fill = country))
gg_KEN_Kenya <- ggplot() + geom_bar(data = data[which(data$country == "Kenya"), ], aes(x = KEN_Logic_Segment, fill = country))
gg_KEN_Nigeria <- ggplot() + geom_bar(data = data[which(data$country == "Nigeria"), ], aes(x = KEN_Logic_Segment, fill = country))
gg_KEN_SouthAfrica <- ggplot() + geom_bar(data = data[which(data$country == "South Africa"), ], aes(x = KEN_Logic_Segment, fill = country))
gg_KEN_Tanzania <- ggplot() + geom_bar(data = data[which(data$country == "Tanzania"), ], aes(x = KEN_Logic_Segment, fill = country))







pdf(file = "C:/Users/johnvand/Desktop/kasi_segments_m.pdf")
print(gg_NGA_total)
print(gg_KEN_total)
print(gg_NGA_Cameroon)
print(gg_KEN_Cameroon)
print(gg_NGA_Cote)
print(gg_KEN_Cote)
print(gg_NGA_DRC)
print(gg_KEN_DRC)
print(gg_NGA_Ghana)
print(gg_KEN_Ghana)
print(gg_NGA_Kenya)
print(gg_KEN_Kenya)
print(gg_NGA_Nigeria)
print(gg_KEN_Nigeria)
print(gg_NGA_SouthAfrica)
print(gg_KEN_SouthAfrica)
print(gg_NGA_Tanzania)
print(gg_KEN_Tanzania)
dev.off()


Sum_NGA_logic <- as.data.frame(table(data$country, data$segment))
NGA_test <- Sum_NGA_logic[which(Sum_NGA_logic$Var1 == "Nigeria"), ]
tot <- sum(NGA_test$Freq)
NGA_test$prop <- NGA_test$Freq/tot
NGA_test$real <- 0

## These numbers are coming from a presentation by Steve
NGA_test$real[which(NGA_test$Var2 == "UF1")] <- 0.06
NGA_test$real[which(NGA_test$Var2 == "UF3")] <- 0.07
NGA_test$real[which(NGA_test$Var2 == "UM1")] <- 0.1
NGA_test$real[which(NGA_test$Var2 == "UM3")] <- 0.12
NGA_test$real[which(NGA_test$Var2 == "UM4")] <- 0.18
NGA_test$real[which(NGA_test$Var2 == "UM5")] <- 0.43


NGA_test$diff <- NGA_test$prop - NGA_test$real

gg_stacked <- ggplot() + geom_bar(data = Sum_NGA_logic, position = "fill", stat = "identity", aes(fill = Var2, x = Var1, y = Freq))

print(gg_stacked)
write.csv(data, file = "C:/Users/johnvand/Desktop/kasi_segments.csv")

