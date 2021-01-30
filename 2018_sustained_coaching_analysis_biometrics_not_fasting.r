#rm(list=ls())

#install.packages("Hmisc")
#library(Hmisc)


#'eligible' here means eligible for that part of the analysis

Biodata_csv <- read.csv(file='____/biometrics_2018.csv', header = T)
Biodata <- Biodata_csv

#dim(Biodata_csv)
#dim(Biodata)

Biodata <- Biodata[which(Biodata$Primary_Program_Enrollment != 'Tobacco Cessation'),]
#levels(Biodata$Relationship)
Biodata$Relationship <- factor(Biodata$Relationship, levels = c("Primary", "Spouse", "DomesticPartner", "Dependent", "Unknown"))

levels(Biodata$Gender) <- c(levels(Biodata$Gender), 'Female', 'Male')

Biodata <- Biodata[which(Biodata$Gender != 'U'),]
Biodata$Gender[Biodata$Gender == 'f' | Biodata$Gender == 'F'] <- 'Female'
Biodata$Gender[Biodata$Gender == 'm' | Biodata$Gender == 'M'] <- 'Male'

Biodata <- Biodata[which(   Biodata$ChallengeParticipant == 0 &
                              Biodata$OnlineClassParticipant == 0 &
                              Biodata$ConnectedParticipant == 0),]











#########################################################################
#Is there a difference between three years of engagement vs just one year?
#########################################################################

#subset data to include those with only one year engaged or three years engaged
Biodata_three_vs_one <- Biodata[which(    (Biodata$Completed_Calls_y1>=4 & 
                                             Biodata$eligible_yr1==1 & 
                                             Biodata$eligible_yr2==0 & 
                                             Biodata$eligible_yr3==0)
                                          |
                                            (Biodata$Completed_Calls_y1>=4 & 
                                               Biodata$Completed_Calls_y2>=4 & 
                                               Biodata$Completed_Calls_y3>=4 & 
                                               Biodata$eligible_yr1==1 & 
                                               Biodata$eligible_yr2==1 & 
                                               Biodata$eligible_yr3==1)         ),]

#dim(Biodata_three_vs_one)




#flag those with three years engaged
Biodata_three_vs_one$three_years_engaged <- ifelse((Biodata_three_vs_one$Completed_Calls_y1>=4 & 
                                                      Biodata_three_vs_one$Completed_Calls_y2>=4 & 
                                                      Biodata_three_vs_one$Completed_Calls_y3>=4 & 
                                                      Biodata_three_vs_one$eligible_yr1==1 & 
                                                      Biodata_three_vs_one$eligible_yr2==1 & 
                                                      Biodata_three_vs_one$eligible_yr3==1),1,0)



#sum(Biodata_three_vs_one$three_years_engaged)




#create datasets with moderate or high risk for each metric at baseline
###LDL, Triglycerides, and Glucose are fasting only###
Biodata_three_vs_one_modhigh_Glucose <- Biodata_three_vs_one[which(Biodata_three_vs_one$Glucose_ModerateHigh_t1 == 1 & Biodata_three_vs_one$has_t1_t2_Glucose == 1 & Biodata_three_vs_one$IsFasting_t1 == 1), ]
#Biodata_three_vs_one_modhigh_Triglycerides <- Biodata_three_vs_one[which(Biodata_three_vs_one$Trig_ModerateHigh_t1 == 1 & Biodata_three_vs_one$has_t1_t2_Triglycerides == 1 & Biodata_three_vs_one$IsFasting_t1 == 1), ]
#Biodata_three_vs_one_modhigh_LDL <- Biodata_three_vs_one[which(Biodata_three_vs_one$LDL_ModerateHigh_t1 == 1 & Biodata_three_vs_one$has_t1_t2_LDL == 1 & Biodata_three_vs_one$IsFasting_t1 == 1), ]

Biodata_three_vs_one_modhigh_Triglycerides <- Biodata_three_vs_one[which(Biodata_three_vs_one$Trig_ModerateHigh_t1 == 1 & Biodata_three_vs_one$has_t1_t2_Triglycerides == 1), ]
Biodata_three_vs_one_modhigh_LDL <- Biodata_three_vs_one[which(Biodata_three_vs_one$LDL_ModerateHigh_t1 == 1 & Biodata_three_vs_one$has_t1_t2_LDL == 1), ]

Biodata_three_vs_one_modhigh_HDL <- Biodata_three_vs_one[which(Biodata_three_vs_one$HDL_ModerateHigh_t1 == 1 & Biodata_three_vs_one$has_t1_t2_HDL == 1), ]
Biodata_three_vs_one_modhigh_Sys <- Biodata_three_vs_one[which(Biodata_three_vs_one$BP_ModerateHigh_t1 == 1 & Biodata_three_vs_one$has_t1_t2_Systolic == 1), ]
Biodata_three_vs_one_modhigh_Dias <- Biodata_three_vs_one[which(Biodata_three_vs_one$BP_ModerateHigh_t1 == 1 & Biodata_three_vs_one$has_t1_t2_Diastolic == 1), ]
#Biodata_three_vs_one_modhigh_Chol <- Biodata_three_vs_one[which(Biodata_three_vs_one$Cholesterol_ModerateHigh_t1 == 1 & Biodata_three_vs_one$has_t1_t2_Cholesterol == 1), ]
Biodata_three_vs_one_modhigh_BMI <- Biodata_three_vs_one[which(Biodata_three_vs_one$BMI_ModerateHigh_t1 == 1 & Biodata_three_vs_one$has_t1_t2_BMI == 1), ]
Biodata_three_vs_one_modhigh_MetsRes <- Biodata_three_vs_one[which(Biodata_three_vs_one$METS_t1 == 1 & Biodata_three_vs_one$has_t1_t2_METS_all_scores == 1), ]


#X20.mgdl.Glucose.Reduction
#X40.mgdl.Triglycerides.Reduction
#X10..LDL.Reduction
#X5.mgdl.HDL.Increase
#X5.mmHg.Systolic.Reduction
#X5.mmHg.Diastolic.Reduction
#X5..Weight.Loss
#METs.Resolution




#head(newdata)


newdata <- Biodata_three_vs_one_modhigh_Glucose
dim(newdata)[1] #661
#X20.mgdl.Glucose.Reduction
model <- glm(X20.mgdl.Glucose.Reduction ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.1236



newdata <- Biodata_three_vs_one_modhigh_Triglycerides
dim(newdata)[1] #761
#X40.mgdl.Triglycerides.Reduction
model <- glm(X40.mgdl.Triglycerides.Reduction ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.1160



newdata <- Biodata_three_vs_one_modhigh_LDL
dim(newdata)[1] #734
#X10..LDL.Reduction
model <- glm(X10..LDL.Reduction ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.4615



newdata <- Biodata_three_vs_one_modhigh_HDL
dim(newdata)[1] #2564
#X5.mgdl.HDL.Increase
model <- glm(X5.mgdl.HDL.Increase ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#beta = 0.197611
#p-value = 0.00251

#There DOES appear to have been a sigficant difference between three years coaching engaged vs one year 
#of coaching engaged on having had a 5 mg/dL increase in HDL (controlling for age, gender, and relationship; 
#among those with moderate to high HDL risk at baseline; also among those WITHOUT challenge, connected, or 
#online class participation; TC members removed).

med_Age <- median(newdata$Age)

pnorm( predict(model, data.frame(three_years_engaged=1, Gender = "Female", Relationship="Primary", Age = med_Age)) ) /
  pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = "Female", Relationship="Primary", Age = med_Age)) )
#1.198925


round((pnorm( predict(model, data.frame(three_years_engaged=1, Gender = "Female", Relationship="Primary", Age = med_Age)) ) -
         pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = "Female", Relationship="Primary", Age = med_Age)) ))*100,1)
#7.7

#A median-aged female with 'primary' relationship was 19.9% (1.199 times) MORE LIKELY to have reported a 
#5 mg/dL increase in HDL at follow-up given three years coaching-engaged vs one year of coaching controlling
#for age, gender, and relationship; among those with moderate to high HDL risk at baseline; also among those 
#WITHOUT challenge, connected, or online class participation; TC members removed). This is a higher probability
#by 7.7 percentage points.






newdata <- Biodata_three_vs_one_modhigh_Sys
dim(newdata)[1] #2284
#X5.mmHg.Systolic.Reduction
model <- glm(X5.mmHg.Systolic.Reduction ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.0795



newdata <- Biodata_three_vs_one_modhigh_Dias
dim(newdata)[1] #2283
#X5.mmHg.Diastolic.Reduction
model <- glm(X5.mmHg.Diastolic.Reduction ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#beta = 0.154802
#p-value = 0.0233

#There DOES appear to have been a sigficant difference between three years coaching engaged vs one year 
#of coaching engaged on having had a 5 mmHg Diastolic Reduction (controlling for age, gender, and relationship; 
#among those with moderate to high HDL risk at baseline; also among those WITHOUT challenge, connected, or 
#online class participation; TC members removed).

med_Age <- median(newdata$Age)

pnorm( predict(model, data.frame(three_years_engaged=1, Gender = "Female", Relationship="Primary", Age = med_Age)) ) /
  pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = "Female", Relationship="Primary", Age = med_Age)) )
#1.154313


round((pnorm( predict(model, data.frame(three_years_engaged=1, Gender = "Female", Relationship="Primary", Age = med_Age)) ) -
         pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = "Female", Relationship="Primary", Age = med_Age)) ))*100,1)
#6.1

#A median-aged female with 'primary' relationship was 15.4% (1.154 times) MORE LIKELY to have reported a 
#5 mmHg Diastolic Reduction at follow-up given three years coaching-engaged vs one year of coaching controlling
#for age, gender, and relationship; among those with moderate to high HDL risk at baseline; also among those 
#WITHOUT challenge, connected, or online class participation; TC members removed). This is a higher probability
#by 6.1 percentage points.







newdata <- Biodata_three_vs_one_modhigh_BMI
dim(newdata)[1] #2412
#X5..Weight.Loss
model <- glm(X5..Weight.Loss ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#beta = 0.197143
#p-value = 0.00948

med_Age <- median(newdata$Age)

pnorm( predict(model, data.frame(three_years_engaged=1, Gender = "Female", Relationship="Primary", Age = med_Age)) ) /
  pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = "Female", Relationship="Primary", Age = med_Age)) )
#1.34037


round((pnorm( predict(model, data.frame(three_years_engaged=1, Gender = "Female", Relationship="Primary", Age = med_Age)) ) -
         pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = "Female", Relationship="Primary", Age = med_Age)) ))*100,1)
#5.0






newdata <- Biodata_three_vs_one_modhigh_MetsRes
dim(newdata)[1] #802
#METs.Resolution
model <- glm(METs.Resolution ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#beta = 0.277572
#p-value = 0.0164

med_Age <- median(newdata$Age)

pnorm( predict(model, data.frame(three_years_engaged=1, Gender = "Female", Relationship="Primary", Age = med_Age)) ) /
  pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = "Female", Relationship="Primary", Age = med_Age)) )
#1.297879


round((pnorm( predict(model, data.frame(three_years_engaged=1, Gender = "Female", Relationship="Primary", Age = med_Age)) ) -
         pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = "Female", Relationship="Primary", Age = med_Age)) ))*100,1)
#10.8









newdata <- Biodata_three_vs_one_modhigh_Glucose
newdata <- Biodata_three_vs_one_modhigh_Triglycerides
newdata <- Biodata_three_vs_one_modhigh_LDL
newdata <- Biodata_three_vs_one_modhigh_HDL
newdata <- Biodata_three_vs_one_modhigh_Sys
newdata <- Biodata_three_vs_one_modhigh_Dias
newdata <- Biodata_three_vs_one_modhigh_BMI
newdata <- Biodata_three_vs_one_modhigh_MetsRes


dim(newdata)[1]
median(newdata$Age)
round(dim(newdata[which(newdata$Gender == 'Female'),])[1] / dim(newdata)[1],3) * 100


table(newdata$three_years_engaged)



################################################################################################
#Is there a difference between three years of coaching engagement vs three years of no coaching?
################################################################################################





#subset data to include only those three years engaged and three years of no coaching
Biodata_three_vs_three <- Biodata[which(    (Biodata$Completed_Calls_y1 == 0 &   #no calls for 3 years
                                               Biodata$Completed_Calls_y2 == 0 & 
                                               Biodata$Completed_Calls_y3 == 0 & 
                                               Biodata$eligible_yr1==1 & 
                                               Biodata$eligible_yr2==1 & 
                                               Biodata$eligible_yr3==1)
                                            |
                                              (Biodata$Completed_Calls_y1>=4 &     #coaching engaged for all 3 years
                                                 Biodata$Completed_Calls_y2>=4 & 
                                                 Biodata$Completed_Calls_y3>=4 & 
                                                 Biodata$eligible_yr1==1 & 
                                                 Biodata$eligible_yr2==1 & 
                                                 Biodata$eligible_yr3==1)         ),]

#dim(Biodata_three_vs_three)




#flag those with three years engaged
Biodata_three_vs_three$three_years_engaged <- ifelse((Biodata_three_vs_three$Completed_Calls_y1>=4 & 
                                                        Biodata_three_vs_three$Completed_Calls_y2>=4 & 
                                                        Biodata_three_vs_three$Completed_Calls_y3>=4 & 
                                                        Biodata_three_vs_three$eligible_yr1==1 & 
                                                        Biodata_three_vs_three$eligible_yr2==1 & 
                                                        Biodata_three_vs_three$eligible_yr3==1),1,0)

#sum(Biodata_three_vs_three$three_years_engaged)


#create datasets with moderate or high risk for each metric at baseline
###LDL, Triglycerides, and Glucose are fasting only###
Biodata_three_vs_three_modhigh_Glucose <- Biodata_three_vs_three[which(Biodata_three_vs_three$Glucose_ModerateHigh_t1 == 1 & Biodata_three_vs_three$has_t1_t2_Glucose == 1 & Biodata_three_vs_three$IsFasting_t1 == 1), ]
#Biodata_three_vs_three_modhigh_Triglycerides <- Biodata_three_vs_three[which(Biodata_three_vs_three$Trig_ModerateHigh_t1 == 1 & Biodata_three_vs_three$has_t1_t2_Triglycerides == 1 & Biodata_three_vs_three$IsFasting_t1 == 1), ]
#Biodata_three_vs_three_modhigh_LDL <- Biodata_three_vs_three[which(Biodata_three_vs_three$LDL_ModerateHigh_t1 == 1 & Biodata_three_vs_three$has_t1_t2_LDL == 1 & Biodata_three_vs_three$IsFasting_t1 == 1), ]

Biodata_three_vs_three_modhigh_Triglycerides <- Biodata_three_vs_three[which(Biodata_three_vs_three$Trig_ModerateHigh_t1 == 1 & Biodata_three_vs_three$has_t1_t2_Triglycerides == 1), ]
Biodata_three_vs_three_modhigh_LDL <- Biodata_three_vs_three[which(Biodata_three_vs_three$LDL_ModerateHigh_t1 == 1 & Biodata_three_vs_three$has_t1_t2_LDL == 1), ]

Biodata_three_vs_three_modhigh_HDL <- Biodata_three_vs_three[which(Biodata_three_vs_three$HDL_ModerateHigh_t1 == 1 & Biodata_three_vs_three$has_t1_t2_HDL == 1), ]
Biodata_three_vs_three_modhigh_Sys <- Biodata_three_vs_three[which(Biodata_three_vs_three$BP_ModerateHigh_t1 == 1 & Biodata_three_vs_three$has_t1_t2_Systolic == 1), ]
Biodata_three_vs_three_modhigh_Dias <- Biodata_three_vs_three[which(Biodata_three_vs_three$BP_ModerateHigh_t1 == 1 & Biodata_three_vs_three$has_t1_t2_Diastolic == 1), ]
Biodata_three_vs_three_modhigh_BMI <- Biodata_three_vs_three[which(Biodata_three_vs_three$BMI_ModerateHigh_t1 == 1 & Biodata_three_vs_three$has_t1_t2_BMI == 1), ]
Biodata_three_vs_three_modhigh_MetsRes <- Biodata_three_vs_three[which(Biodata_three_vs_three$METS_t1 == 1 & Biodata_three_vs_three$has_t1_t2_METS_all_scores == 1), ]


#how much does fasting reduce the sample size?
#dim( Biodata_three_vs_three[which(Biodata_three_vs_three$Trig_ModerateHigh_t1 == 1 & Biodata_three_vs_three$has_t1_t2_Triglycerides == 1 & Biodata_three_vs_three$IsFasting_t1 == 1), ] )
#dim( Biodata_three_vs_three[which(Biodata_three_vs_three$Trig_ModerateHigh_t1 == 1 & Biodata_three_vs_three$has_t1_t2_Triglycerides == 1), ] )
# 3740 vs 3967 




newdata <- Biodata_three_vs_three_modhigh_Glucose
dim(newdata)[1] #3680
#X20.mgdl.Glucose.Reduction
model <- glm(X20.mgdl.Glucose.Reduction ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.1417



newdata <- Biodata_three_vs_three_modhigh_Triglycerides
dim(newdata)[1] #3967
#X40.mgdl.Triglycerides.Reduction
model <- glm(X40.mgdl.Triglycerides.Reduction ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.359



newdata <- Biodata_three_vs_three_modhigh_LDL
dim(newdata)[1] #3956
#X10..LDL.Reduction
model <- glm(X10..LDL.Reduction ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.34987



newdata <- Biodata_three_vs_three_modhigh_HDL
dim(newdata)[1] #19368
#X5.mgdl.HDL.Increase
model <- glm(X5.mgdl.HDL.Increase ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.23794






newdata <- Biodata_three_vs_three_modhigh_Sys
dim(newdata)[1] #17543
#X5.mmHg.Systolic.Reduction
model <- glm(X5.mmHg.Systolic.Reduction ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.07983




newdata <- Biodata_three_vs_three_modhigh_Dias
dim(newdata)[1] #17540
#X5.mmHg.Diastolic.Reduction
model <- glm(X5.mmHg.Diastolic.Reduction ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value =0.13407







newdata <- Biodata_three_vs_three_modhigh_BMI
dim(newdata)[1] #19001
#X5..Weight.Loss
model <- glm(X5..Weight.Loss ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#beta = 0.180854
#p-value = 0.00729

med_Age <- median(newdata$Age)

pnorm( predict(model, data.frame(three_years_engaged=1, Gender = "Female", Relationship="Primary", Age = med_Age)) ) /
  pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = "Female", Relationship="Primary", Age = med_Age)) )
#1.295925


round((pnorm( predict(model, data.frame(three_years_engaged=1, Gender = "Female", Relationship="Primary", Age = med_Age)) ) -
         pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = "Female", Relationship="Primary", Age = med_Age)) ))*100,1)
#4.9





newdata <- Biodata_three_vs_three_modhigh_MetsRes
dim(newdata)[1] #4831
#METs.Resolution
model <- glm(METs.Resolution ~ three_years_engaged + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#beta = 0.216211
#p-value =0.0404

med_Age <- median(newdata$Age)

pnorm( predict(model, data.frame(three_years_engaged=1, Gender = "Female", Relationship="Primary", Age = med_Age)) ) /
  pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = "Female", Relationship="Primary", Age = med_Age)) )
#1.215508


round((pnorm( predict(model, data.frame(three_years_engaged=1, Gender = "Female", Relationship="Primary", Age = med_Age)) ) -
         pnorm( predict(model, data.frame(three_years_engaged=0 , Gender = "Female", Relationship="Primary", Age = med_Age)) ))*100,1)
#8.5









newdata <- Biodata_three_vs_three_modhigh_Glucose
newdata <- Biodata_three_vs_three_modhigh_Triglycerides
newdata <- Biodata_three_vs_three_modhigh_LDL
newdata <- Biodata_three_vs_three_modhigh_HDL
newdata <- Biodata_three_vs_three_modhigh_Sys
newdata <- Biodata_three_vs_three_modhigh_Dias
newdata <- Biodata_three_vs_three_modhigh_BMI
newdata <- Biodata_three_vs_three_modhigh_MetsRes


dim(newdata)[1]
median(newdata$Age)
round(dim(newdata[which(newdata$Gender == 'Female'),])[1] / dim(newdata)[1],3) * 100


table(newdata$three_years_engaged)




#############################################################
#Does consistent year-over-year call-making improve outcomes?
#############################################################


Biodata_yr_over_yr <- Biodata[which(  Biodata$eligible_yr3==1 & Biodata$Completed_Calls >= 12),]

Biodata_yr_over_yr$Calls_CV <- apply(cbind(Biodata_yr_over_yr$Completed_Calls_y1, Biodata_yr_over_yr$Completed_Calls_y2, Biodata_yr_over_yr$Completed_Calls_y3),1,sd) / 
  apply(cbind(Biodata_yr_over_yr$Completed_Calls_y1, Biodata_yr_over_yr$Completed_Calls_y2, Biodata_yr_over_yr$Completed_Calls_y3),1,mean)

Biodata_yr_over_yr$Calls_CV[is.na(Biodata_yr_over_yr$Calls_CV)] <- 0


#create datasets with moderate or high risk for each metric at baseline
###LDL, Triglycerides, and Glucose are fasting only###
Biodata_yr_over_yr_modhigh_Glucose <- Biodata_yr_over_yr[which(Biodata_yr_over_yr$Glucose_ModerateHigh_t1 == 1 & Biodata_yr_over_yr$has_t1_t2_Glucose == 1 & Biodata_yr_over_yr$IsFasting_t1 == 1), ]
#Biodata_yr_over_yr_modhigh_Triglycerides <- Biodata_yr_over_yr[which(Biodata_yr_over_yr$Trig_ModerateHigh_t1 == 1 & Biodata_yr_over_yr$has_t1_t2_Triglycerides == 1 & Biodata_yr_over_yr$IsFasting_t1 == 1), ]
#Biodata_yr_over_yr_modhigh_LDL <- Biodata_yr_over_yr[which(Biodata_yr_over_yr$LDL_ModerateHigh_t1 == 1 & Biodata_yr_over_yr$has_t1_t2_LDL == 1 & Biodata_yr_over_yr$IsFasting_t1 == 1), ]

Biodata_yr_over_yr_modhigh_Triglycerides <- Biodata_yr_over_yr[which(Biodata_yr_over_yr$Trig_ModerateHigh_t1 == 1 & Biodata_yr_over_yr$has_t1_t2_Triglycerides == 1), ]
Biodata_yr_over_yr_modhigh_LDL <- Biodata_yr_over_yr[which(Biodata_yr_over_yr$LDL_ModerateHigh_t1 == 1 & Biodata_yr_over_yr$has_t1_t2_LDL == 1), ]

Biodata_yr_over_yr_modhigh_HDL <- Biodata_yr_over_yr[which(Biodata_yr_over_yr$HDL_ModerateHigh_t1 == 1 & Biodata_yr_over_yr$has_t1_t2_HDL == 1), ]
Biodata_yr_over_yr_modhigh_Sys <- Biodata_yr_over_yr[which(Biodata_yr_over_yr$BP_ModerateHigh_t1 == 1 & Biodata_yr_over_yr$has_t1_t2_Systolic == 1), ]
Biodata_yr_over_yr_modhigh_Dias <- Biodata_yr_over_yr[which(Biodata_yr_over_yr$BP_ModerateHigh_t1 == 1 & Biodata_yr_over_yr$has_t1_t2_Diastolic == 1), ]
Biodata_yr_over_yr_modhigh_BMI <- Biodata_yr_over_yr[which(Biodata_yr_over_yr$BMI_ModerateHigh_t1 == 1 & Biodata_yr_over_yr$has_t1_t2_BMI == 1), ]
Biodata_yr_over_yr_modhigh_MetsRes <- Biodata_yr_over_yr[which(Biodata_yr_over_yr$METS_t1 == 1 & Biodata_yr_over_yr$has_t1_t2_METS_all_scores == 1), ]



newdata <- Biodata_yr_over_yr_modhigh_Glucose
dim(newdata)[1] #328
#X20.mgdl.Glucose.Reduction
model <- glm(X20.mgdl.Glucose.Reduction ~ Calls_CV + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.0277
#beta = 0.46572

med_Age <- median(newdata$Age)
max_cv <- max(newdata$Calls_CV)

pnorm( predict(model, data.frame(Calls_CV=0, Gender = "Female", Relationship="Primary", Age = med_Age)) ) /
  pnorm( predict(model, data.frame(Calls_CV=max_cv , Gender = "Female", Relationship="Primary", Age = med_Age)) )
#0.2752239
# 0.06787864 / 0.2466306 = 0.2752239
#1 / 0.2752239 = 3.633405
# 0.2466306 / 0.06787864 = 3.633405


round((pnorm( predict(model, data.frame(Calls_CV=0, Gender = "Female", Relationship="Primary", Age = med_Age)) ) -
         pnorm( predict(model, data.frame(Calls_CV=max_cv , Gender = "Female", Relationship="Primary", Age = med_Age)) ))*100,1)
#-17.9



newdata <- Biodata_yr_over_yr_modhigh_Triglycerides
dim(newdata)[1] #394
#X40.mgdl.Triglycerides.Reduction
model <- glm(X40.mgdl.Triglycerides.Reduction ~ Calls_CV + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.896





newdata <- Biodata_yr_over_yr_modhigh_LDL
dim(newdata)[1] #420
#X10..LDL.Reduction
model <- glm(X10..LDL.Reduction ~ Calls_CV + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.525





newdata <- Biodata_yr_over_yr_modhigh_HDL
dim(newdata)[1] #1409
#X5.mgdl.HDL.Increase
model <- glm(X5.mgdl.HDL.Increase ~ Calls_CV + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.205247






newdata <- Biodata_yr_over_yr_modhigh_Sys
dim(newdata)[1] #1228
#X5.mmHg.Systolic.Reduction
model <- glm(X5.mmHg.Systolic.Reduction ~ Calls_CV + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.48767





newdata <- Biodata_yr_over_yr_modhigh_Dias
dim(newdata)[1] #1228
#X5.mmHg.Diastolic.Reduction
model <- glm(X5.mmHg.Diastolic.Reduction ~ Calls_CV + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.542





newdata <- Biodata_yr_over_yr_modhigh_BMI
dim(newdata)[1] #1376
#X5..Weight.Loss
model <- glm(X5..Weight.Loss ~ Calls_CV + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.13751





newdata <- Biodata_yr_over_yr_modhigh_MetsRes
dim(newdata)[1] #425
#METs.Resolution
model <- glm(METs.Resolution ~ Calls_CV + Gender*Age*Relationship, family=binomial(link = "probit"), data=newdata)
summary(model)

#p-value = 0.165







newdata <- Biodata_yr_over_yr_modhigh_Glucose

#PLOT
buckets <- 6 #10
newdata2 <- as.data.frame(newdata$X20.mgdl.Glucose.Reduction)
newdata2$myvariable <- cut(newdata$Calls_CV,buckets)
names(newdata2) <- c("X20.mgdl.Glucose.Reduction", "Coefficient_of_Variation")

levels(newdata2$Coefficient_of_Variation) <- rev(levels(newdata2$Coefficient_of_Variation))

#head(newdata2)
#table(newdata2)
#barplot(table(newdata2))

library(ggplot2)
myframe <- aggregate(newdata2$X20.mgdl.Glucose.Reduction, list(newdata2$Coefficient_of_Variation), mean)

library(dplyr)
myframe <- arrange(myframe, -row_number())


ggplot(myframe,aes(x = Group.1,rev(myframe$x)))+geom_bar(stat ="identity")+
  xlab("Coefficient of Variation Groupings")+
  ylab("Proportion that Achieved 20 mg/dl Glucose Reduction")








newdata <- Biodata_yr_over_yr_modhigh_Glucose
newdata <- Biodata_yr_over_yr_modhigh_Triglycerides
newdata <- Biodata_yr_over_yr_modhigh_LDL
newdata <- Biodata_yr_over_yr_modhigh_HDL
newdata <- Biodata_yr_over_yr_modhigh_Sys
newdata <- Biodata_yr_over_yr_modhigh_Dias
newdata <- Biodata_yr_over_yr_modhigh_BMI
newdata <- Biodata_yr_over_yr_modhigh_MetsRes


dim(newdata)[1]
median(newdata$Age)
round(dim(newdata[which(newdata$Gender == 'Female'),])[1] / dim(newdata)[1],3) * 100






################################################################################
################################################################################
################################################################################

#beta = 

med_Age <- median(newdata$Age)

pnorm( predict(model, data.frame(Calls_CV=1, Gender = "Female", Relationship="Primary", Age = med_Age)) ) /
  pnorm( predict(model, data.frame(Calls_CV=0 , Gender = "Female", Relationship="Primary", Age = med_Age)) )
#


round((pnorm( predict(model, data.frame(Calls_CV=1, Gender = "Female", Relationship="Primary", Age = med_Age)) ) -
         pnorm( predict(model, data.frame(Calls_CV=0 , Gender = "Female", Relationship="Primary", Age = med_Age)) ))*100,1)
#






################################################################################
################################################################################
################################################################################



library(sm)

Biodata_yr_over_yr_modhigh_Glucose <- Biodata_yr_over_yr[which(Biodata_yr_over_yr$Glucose_ModerateHigh_t1 == 1 & Biodata_yr_over_yr$has_t1_t2_Glucose == 1 & Biodata_yr_over_yr$IsFasting_t1 == 1), ]

newdata <- Biodata_yr_over_yr_modhigh_Glucose

newdataframe <- data.frame(newdata$Glucose_t1, newdata$Calls_CV)
colnames(newdataframe) <- c("Glucose", "Coefficient_of_Variation")
#head(newdataframe)
newdataframe$Glucose <- as.integer(as.character(newdataframe$Glucose))
newdataframe$GoodAdherence <- ifelse(newdataframe$Coefficient_of_Variation <= 1.15, 1, 0)

#str(newdataframe)



sm.density.compare(newdataframe$Glucose, newdataframe$GoodAdherence, xlab="Baseline Glucose Score", lwd = 4, 
                   lty = c(1,1), col=c("purple", "black"))
title(main="Baseline Glucose Score by Coefficient of Variation")

Coefficient_of_Variation.f <- factor(newdataframe$GoodAdherence, levels= c(0,1),
                    labels = c("Poor Coaching Adherence", "Good Coaching Adherence")) 

#colfill<-c(2:(2+length(levels(Coefficient_of_Variation.f)))) 
#legend(locator(1), levels(Coefficient_of_Variation.f), fill=colfill)
legend(locator(1), levels(Coefficient_of_Variation.f), fill=c("purple", "black"))



#CV (1.15,1.73]
#CV [0,1.15]





#str(Biodata_yr_over_yr_modhigh_Glucose)








