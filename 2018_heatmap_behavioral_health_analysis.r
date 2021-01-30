#rm(list=ls())

lifedata_csv <- read.csv(file='____/behavioral_health_analysis_2018.csv', header = T)
lifedata <- lifedata_csv

#dim(lifedata_csv)
#dim(lifedata)

lifedata <- lifedata[which(lifedata$Primary_Program_Enrollment != 'Tobacco Cessation'),]
#biodata <- biodata[which(biodata$Completed_Calls %nin% c(1,2,3)),]
#biodata$engage_flag <- ifelse(biodata$Completed_Calls >= 4, 1, 0)
#levels(lifedata$Relationship)
lifedata$Relationship <- factor(lifedata$Relationship, levels = c("Primary", "Spouse", "DomesticPartner", "Dependent", "Unknown"))
lifedata$Gender[lifedata$Gender == "f"] <- "Female"
lifedata$Gender[lifedata$Gender == "m"] <- "Male"

lifedata <- lifedata[which(   lifedata$ChallengeParticipant == 0 &
                                lifedata$OnlineClassParticipant == 0 &
                                lifedata$ConnectedParticipant == 0),]





################################################################################################
#Is there a difference between three years of coaching engagement vs three years of no coaching?
################################################################################################

#subset data to include only those three years engaged and three years of no coaching
lifedata_three_vs_three <- lifedata[which(    (lifedata$Completed_Calls_y1 == 0 &   #no calls for 3 years
                                                 lifedata$Completed_Calls_y2 == 0 & 
                                                 lifedata$Completed_Calls_y3 == 0 & 
                                                 lifedata$eligible_yr1==1 & 
                                                 lifedata$eligible_yr2==1 & 
                                                 lifedata$eligible_yr3==1)
                                              |
                                                (lifedata$Completed_Calls_y1>=4 &     #coaching engaged for all 3 years
                                                   lifedata$Completed_Calls_y2>=4 & 
                                                   lifedata$Completed_Calls_y3>=4 & 
                                                   lifedata$eligible_yr1==1 & 
                                                   lifedata$eligible_yr2==1 & 
                                                   lifedata$eligible_yr3==1)         ),]

#dim(lifedata_three_vs_three)




#flag those with three years engaged
lifedata_three_vs_three$three_years_engaged <- ifelse((lifedata_three_vs_three$Completed_Calls_y1>=4 & 
                                                         lifedata_three_vs_three$Completed_Calls_y2>=4 & 
                                                         lifedata_three_vs_three$Completed_Calls_y3>=4 & 
                                                         lifedata_three_vs_three$eligible_yr1==1 & 
                                                         lifedata_three_vs_three$eligible_yr2==1 & 
                                                         lifedata_three_vs_three$eligible_yr3==1),1,0)

#sum(lifedata_three_vs_three$three_years_engaged)


#create datasets with moderate or high risk for each metric at baseline
#lifedata_three_vs_three_modhigh_Activity <- lifedata_three_vs_three[which((lifedata_three_vs_three$Activity_Moderate_Risk_t1 == 1 | lifedata_three_vs_three$Activity_High_Risk_t1 == 1) & lifedata_three_vs_three$Has_t1_t2_Activity == 1),]
lifedata_three_vs_three_modhigh_Diet <- lifedata_three_vs_three[which((lifedata_three_vs_three$Diet_Moderate_Risk_t1 == 1 | lifedata_three_vs_three$Diet_High_Risk_t1 == 1) & lifedata_three_vs_three$Has_t1_t2_Diet == 1),]
lifedata_three_vs_three_modhigh_Stress <- lifedata_three_vs_three[which((lifedata_three_vs_three$Stress_Moderate_Risk_t1 == 1 | lifedata_three_vs_three$Stress_High_Risk_t1 == 1) & lifedata_three_vs_three$Has_t1_t2_Stress == 1),]




#ACTIVITY
#newdata <- lifedata_three_vs_three_modhigh_Activity
#Activity_Healthy_at_t2
#model <- glm(Activity_Healthy_at_t2 ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
#summary(model)

#p-value = 0.627225


#head(newdata_all$HealthPlanName,10)


#DIET
newdata_all <- lifedata_three_vs_three_modhigh_Diet
newdata <- newdata_all
newdata <- newdata_all[which(newdata_all$HealthPlanName == "Standalone Employer Groups"),]
newdata <- newdata_all[which(newdata_all$HealthPlanName == "PepsiCo"),]
newdata <- newdata_all[which(newdata_all$HealthPlanName == "Ingersoll Rand"),]
newdata <- newdata_all[which(newdata_all$HealthPlanName == "ExxonMobil - Health and Wellness"),]
newdata <- newdata_all[which(newdata_all$HealthPlanName == "Exelon"),]
newdata <- newdata_all[which(newdata_all$HealthPlanName == "MVP/Preferred Care"),]
newdata <- newdata_all[which(newdata_all$HealthPlanName == "Tyco International"),]
newdata <- newdata_all[which(newdata_all$HealthPlanName == "Nordstrom, Inc"),]


#write.csv(newdata_all, file="N:/DataAnalytics/Internal/OUT Team/Brandon Boylan/Sustained Coaching Analysis/heatmap R/diet_IR.csv")

#dim(newdata_all)

dim(newdata)[1]
model <- glm(Diet_Healthy_at_t2 ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#model_Nord <- glm(Diet_Healthy_at_t2 ~ three_years_engaged + Gender*Age, family=binomial(link = "probit"), data=newdata)
#summary(model_Nord)

#20543 0.000676
#7249 0.000343
#10684 0.33903
#0
#845 0.439
#0
#807 0.4459
#0
#459 0.369



#unique(newdata$Gender)
#unique(newdata$Age)
#unique(newdata$Relationship)



gender_to_use <- "Female"

age_to_use <- mean(newdata$Age) - 2*sd(newdata$Age)
AA <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age) - 1*sd(newdata$Age)
BB <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age)
CC <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age) + 1*sd(newdata$Age)
DD <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age) + 2*sd(newdata$Age)
EE <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )


gender_to_use <- "Male"

age_to_use <- mean(newdata$Age) - 2*sd(newdata$Age)
FF <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age) - 1*sd(newdata$Age)
GG <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age)
HH <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age) + 1*sd(newdata$Age)
II <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age) + 2*sd(newdata$Age)
JJ <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )


heatdata_F <- data.frame(rbind(AA,BB,CC,DD,EE))
names(heatdata_F) <- "Female"
heatdata_M <- data.frame(rbind(FF,GG,HH,II,JJ))
names(heatdata_M) <- "Male"

print(heatdata_F, row.names = FALSE)
print(heatdata_M, row.names = FALSE)



#the reason why the '% more likely' increases is because GenderMale and Age have negative coefficients. So when we plug in male and old age, the three_years_engaged term (positive coefficient) becomes more weighted, so the ratio of p-values goes up.
#In other words, at higher age for males, we get lower p-values so a few percent now doubles the lower p-value.
#ex "MVP/Preferred Care" 0.2462131 vs 0.1028014 which is more than 2x higher

#pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) )  /   
#  pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) )







#STRESS
newdata_all <- lifedata_three_vs_three_modhigh_Stress
newdata <- newdata_all
newdata <- newdata_all[which(newdata_all$HealthPlanName == "Standalone Employer Groups"),]
newdata <- newdata_all[which(newdata_all$HealthPlanName == "PepsiCo"),]
newdata <- newdata_all[which(newdata_all$HealthPlanName == "Ingersoll Rand"),]
newdata <- newdata_all[which(newdata_all$HealthPlanName == "ExxonMobil - Health and Wellness"),]
newdata <- newdata_all[which(newdata_all$HealthPlanName == "Exelon"),]
newdata <- newdata_all[which(newdata_all$HealthPlanName == "MVP/Preferred Care"),]
newdata <- newdata_all[which(newdata_all$HealthPlanName == "Tyco International"),]
newdata <- newdata_all[which(newdata_all$HealthPlanName == "Nordstrom, Inc"),]



dim(newdata)[1]
model <- glm(Stress_Healthy_at_t2 ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#model_Nord <- glm(Stress_Healthy_at_t2 ~ three_years_engaged + Gender*Age, family=binomial(link = "probit"), data=newdata)
#summary(model_Nord)


#15414 0.01179
#6230 0.09448
#7219 0.44768
#0
#627 0.320
#0
#743 0.5669
#0
#397 0.970




gender_to_use <- "Female"

age_to_use <- mean(newdata$Age) - 2*sd(newdata$Age)
AA <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age) - 1*sd(newdata$Age)
BB <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age)
CC <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age) + 1*sd(newdata$Age)
DD <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age) + 2*sd(newdata$Age)
EE <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )


gender_to_use <- "Male"

age_to_use <- mean(newdata$Age) - 2*sd(newdata$Age)
FF <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age) - 1*sd(newdata$Age)
GG <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age)
HH <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age) + 1*sd(newdata$Age)
II <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )

age_to_use <- mean(newdata$Age) + 2*sd(newdata$Age)
JJ <- as.numeric( pnorm( predict(model, data.frame(three_years_engaged=1, Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) /   pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = gender_to_use, Relationship="Primary", Age = age_to_use)) ) - 1 )


heatdata_F <- data.frame(rbind(AA,BB,CC,DD,EE))
names(heatdata_F) <- "Female"
heatdata_M <- data.frame(rbind(FF,GG,HH,II,JJ))
names(heatdata_M) <- "Male"

print(heatdata_F, row.names = FALSE)
print(heatdata_M, row.names = FALSE)




#####################################
###Heatmap_Heatmap_Heatmap_Heatmap###
###Heatmap_Heatmap_Heatmap_Heatmap###
###Heatmap_Heatmap_Heatmap_Heatmap###
#####################################

#library(shiny)
#library(heatmaply)
#library(shinyHeatmaply)
#runApp(system.file("shinyapp", package = "shinyHeatmaply"))
#library(plotly)
#packageVersion('plotly')

library("gplots")
library("ggplot2")

T2H8_data2 <- read.csv(file = "N:/DataAnalytics/Internal/OUT Team/Brandon Boylan/Text2HealthyW8/Heatmap R/week_gender4 _age redone.csv", header = T)
#head(T2H8_data2)
#names(T2H8_data2)
#dim(T2H8_data2)


names(T2H8_data2) <- c("Type",	"Welcome Message ",	"Adhoc Message ",	"Keyword Message ",	"Week 1 Food Journaling ",	"Week 2 Food Choices ",	"Week 3 Portion Control ",	"Week 4-5 Food Cravings ",	"Week 6-7 Emotional Eating ",	"Week 8-9 Mindful Eating ",	"Week 10 Food Environment ",	"Week 11-12 Social Eating  ",	"Week 13-14 Exercise ",	"Week 15 Barriers ",	"Week 16 Refl and Main LPA ")
T2H8_data2 <- T2H8_data2[,c(-2,-3,-4)]
T2H8_data2 <- T2H8_data2[c(1:9,20),]
rownames(T2H8_data2)[3:5]  <- c("20-39",  "40-49",  "50+")
T2H8_data2_matrix <- data.matrix(T2H8_data2[,-1])
row.names(T2H8_data2_matrix) <- T2H8_data2$Type

setwd("N:/DataAnalytics/Internal/OUT Team/Brandon Boylan/Sustained Coaching Analysis/heatmap R")

my_palette <- colorRampPalette(c("red", "yellow", "white"))(n = 299)
col_breaks = c(seq(.10,0.30,length=100),               # for red
               seq(0.31,0.50,length=100),           # for yellow
               seq(0.51,0.70,length=100))             # for white


my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
col_breaks = c(seq(.10,0.30,length=100),               # for red
               seq(0.31,0.50,length=100),           # for yellow
               seq(0.51,0.70,length=100))             # for green


my_palette <- colorRampPalette(c("red", "yellow", "dark green"))(n = 299)
col_breaks = c(seq(.10,0.30,length=100),               # for red
               seq(0.31,0.50,length=100),           # for yellow
               seq(0.51,0.70,length=100))             # for dark green


jpeg("myheatmap _dark green.jpeg", width = 5*300,        # 5 x 300 pixels
     height = 5*300,
     res = 300,            # 300 pixels per inch
     pointsize = 5)
heatmap.2(T2H8_data2_matrix, cellnote = T2H8_data2_matrix, notecol="black",
          density.info="none",trace="none", Rowv=NA, Colv=NA, breaks=col_breaks, 
          col =my_palette , scale="none", margins=c(15,15), main="Text2HealthyW8 Response Rates")

dev.off()











