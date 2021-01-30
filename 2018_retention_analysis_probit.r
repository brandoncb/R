###Employee Retention Analysis
###Study Goal:
###The goal of this study is to determine how much more likely ASHM-engaged members are to stay employed with their organization.
###Brandon Boylan, MS; Statistician II


  #rm(list=ls())

retentiondata <- read.csv(file= '____/Retention analysis data SQL output _ClientName 10-3-18.csv', header = T)

mydata <- retentiondata



# # # # # # # # # # # # #
# # # Data Cleaning # # #
# # # Data Cleaning # # #
# # # Data Cleaning # # #
# # # # # # # # # # # # #



#format product engagements as factors for regression model
mydata$CoachingEngaged <- as.factor(mydata$CoachingEngaged)
mydata$ChallengeCompleter <- as.factor(mydata$ChallengeCompleter)
mydata$OnlineClassQuizCompleter <- as.factor(mydata$OnlineClassQuizCompleter)
mydata$ConnectedEngaged <- as.factor(mydata$ConnectedEngaged)


#remove odd and unexpected data
mydata <- mydata[-which(mydata$State == 'BX'),]
mydata <- mydata[-which(mydata$State == 'PR'),]
mydata <- mydata[-which(mydata$State == 'VI'),]
mydata <- mydata[-which(mydata$State == ''),]


#format state as a factor with alphabetical levels
mydata$State <- factor(mydata$State, levels = as.vector(sort(unique(mydata$State))))


#re-code gender with reference group as female
mydata <- mydata[-which(mydata$Gender == "U"),] 
mydata$Gender[mydata$Gender == "f"] <- "F"
mydata$Gender[mydata$Gender == "m"] <- "M"
mydata$Gender <- factor(mydata$Gender, levels = c("F", "M"))


#limit the age range to remove outliers and noise from retirees
mydata <- mydata[which(mydata$Age %in% 18:65),]


#additional clients were requested to be removed. note it is case-sensitive
#removeclients <- toupper(c("asdf"))


  #dim(mydata)[1]
  #length(unique(mydata$ClientName))
  #dim(mydata[which(mydata$ClientName == toupper(c("asdf"))),])
  #unique(mydata$ClientName)
mydata <- mydata[-which(toupper(mydata$ClientName) %in% removeclients),]
  #dim(mydata)[1]
  #length(unique(mydata$ClientName))
  #dim(mydata[which(mydata$ClientName == toupper(c("#head(mydata2)"))),])




#categorize states into US Census Region Divisions
StateRegion <- read.csv(file= '____/regions_by_state_table.csv', header = T)

#left join
mydata <- merge(x = mydata, y = StateRegion, by = "State", all.x = TRUE)

  #names(mydata)
mydata <- mydata[,-c(19:21)]


#categorize clients by incentive design
IncentiveDesign <- read.csv(file= '____/Incentive Design flag.csv', header = T)

#left join
  #head(mydata)
mydata2 <- merge(x = mydata, y = IncentiveDesign, by = "GroupID", all.x = TRUE)
  #names(mydata2)
  #head(mydata2)


#flag for outcome-based incentive design #column 26
mydata2[which(mydata2$HealthPlanID.x %in% c(70, 245, 50, 257, 178)),26] <- 0
mydata2[which(mydata2$HealthPlanID.x %in% c(71, 142, 190)),26] <- 1
  #unique(mydata2$HealthPlanID.x)


#remove unused columns and rename others
mydata3 <- mydata2[,-c(9,14,24,25)]
names(mydata3)[5] <- 'HealthPlanID'
names(mydata3)[22] <- 'OutcomeIncentiveDesign'


mydata3$OutcomeIncentiveDesign <- as.factor(mydata3$OutcomeIncentiveDesign)

mydata3 <- mydata3[-which(mydata3$EmploymentDuration == 'Not employed at this time'),]




#client incentive reward amount
incentivespreadsheetdata <- read.csv(file = "____reward amount fixed.csv", header = T)
  #names(incentivespreadsheetdata)

incentivespreadsheetdata <- incentivespreadsheetdata[,c(4,13)]
incentivespreadsheetdata$ClientID <- factor(incentivespreadsheetdata$ClientID)

  #unique(incentivespreadsheetdata$EmployeeRewardAmount)

mydata3$ClientID <- factor(ifelse(mydata3$HealthPlanID %in% c(18,50,245,142,190,178) , mydata3$GroupID, mydata3$HealthPlanID))
  #tail(mydata3)
  #str(mydata3)
  #str(incentivespreadsheetdata)


#left join
mydata3 <- merge(x = mydata3, y = incentivespreadsheetdata, by = "ClientID", all.x = TRUE)
  #mydata3$EmployeeRewardAmount <- as.character(mydata3$EmployeeRewardAmount)
  #unique(mydata3$EmployeeRewardAmount)
  #head(mydata3)
  #tail(mydata3)


#7.7% have unknown reward amounts and were removed
dim(mydata3[which(mydata3$EmployeeRewardAmount == 'Unknown'),])[1] /
  dim(mydata3)[1]

mydata3 <- mydata3[-which(mydata3$EmployeeRewardAmount == 'Unknown'),]

names(mydata3)[24] <- "RewardAmount"

  #head(mydata3$RewardAmount)
  #head(as.numeric(as.character(mydata3$RewardAmount)))

mydata3$RewardAmount <- as.numeric(as.character(mydata3$RewardAmount))
  #sort(unique(m  ydata3$RewardAmount))


#absolute value for 'take-away' plans
mydata3$RewardAmount <- abs(mydata3$RewardAmount)
  #str(mydata3)
  #dim(mydata3)[1] #184532


#ouptut data file for exploratory analysis of predictor variables in excel
write.csv(mydata3, file = "____/finalmodel data 10-15-18.csv")





# # # # # # # # # # # # # # # # #
# # # Summary statistics  # # # #
# # # Summary statistics  # # # #
# # # Summary statistics  # # # #
# # # # # # # # # # # # # # # # #



#Summary statistics
median(mydata3$Age)
dim(mydata3)
round(dim(mydata3[mydata3$Gender == "F",])[1] / dim(mydata3)[1],3) * 100
#unique(mydata3$Gender)
#head(mydata3)
round(table(mydata3$CoachingEngaged)[2] / dim(mydata3)[1],3) * 100
round(table(mydata3$ChallengeCompleter)[2] / dim(mydata3)[1],3) * 100
round(table(mydata3$OnlineClassQuizCompleter)[2] / dim(mydata3)[1],3) * 100
round(table(mydata3$ConnectedEngaged)[2] / dim(mydata3)[1],3) * 100

# str(mydata3)
# unique(mydata3$CoachingEngaged)
# unique(mydata3$ChallengeCompleter)
# unique(mydata3$OnlineClassQuizCompleter)
# unique(mydata3$ConnectedEngaged)

unique(mydata3$JobCategory)
unique(mydata3$Education)
unique(mydata3$EmploymentDuration)
unique(mydata3$Division)
unique(mydata3$JobCategory)

table(mydata3$Education) / dim(mydata3)[1]
table(mydata3$EmploymentDuration) / dim(mydata3)[1]
table(mydata3$Division) / dim(mydata3)[1]
table(mydata3$JobCategory) / dim(mydata3)[1]





# # # # # # # # # # # # #
# # # Model build # # # #
# # # Model build # # # #
# # # Model build # # # #
# # # # # # # # # # # # #


#backward selection model build

#set reference groups for model covariates
mydata4 <- mydata3
mydata4$JobCategory <- relevel(mydata4$JobCategory, ref = "Professional")
mydata4$Education <- relevel(mydata4$Education, ref = "High school graduate or GED")
mydata4$Division <- relevel(mydata4$Division, ref = "NEW ENGLAND")



#str(mydata4)
model1 <- glm(Eligible_in_2017 ~ CoachingEngaged*ChallengeCompleter*OnlineClassQuizCompleter*ConnectedEngaged + 
                Gender*Age + 
                JobCategory + 
                Education + 
                EmploymentDuration + 
                Division + 
                OutcomeIncentiveDesign + 
                RewardAmount +  
                Group_Fraction_With_One_Or_More_Product_Engagements, 
              family=binomial(link = "probit"), 
              data=mydata4)

summary(model1)


#Expand multi-product interaction to be able to remove non-significant terms
model1b <- glm(Eligible_in_2017 ~ 
                 CoachingEngaged + 
                 ChallengeCompleter + 
                 OnlineClassQuizCompleter + 
                 ConnectedEngaged + 
                 CoachingEngaged:ChallengeCompleter + 
                 CoachingEngaged:OnlineClassQuizCompleter + 
                 ChallengeCompleter:OnlineClassQuizCompleter + 
                 CoachingEngaged:ConnectedEngaged + 
                 ChallengeCompleter:ConnectedEngaged + 
                 OnlineClassQuizCompleter:ConnectedEngaged + 
                 CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                 CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                 CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                 ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                 CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                 Gender*Age + 
                 JobCategory + 
                 Education + 
                 EmploymentDuration + 
                 Division +
                 OutcomeIncentiveDesign + 
                 RewardAmount +  
                 Group_Fraction_With_One_Or_More_Product_Engagements, 
               family=binomial(link = "probit"), 
               data=mydata4)

summary(model1b)



#create binary flags for the job category variable
mydata5 <- mydata4
mydata5$JobCategory_Professional <- as.factor(ifelse(mydata5$JobCategory == "Professional", 1, 0))
mydata5$JobCategory_Clerical <- as.factor(ifelse(mydata5$JobCategory == "Clerical", 1, 0))
mydata5$JobCategory_Laborer <- as.factor(ifelse(mydata5$JobCategory == "Laborer", 1, 0))
mydata5$JobCategory_Managerial <- as.factor(ifelse(mydata5$JobCategory == "Managerial", 1, 0))
mydata5$JobCategory_Other <- as.factor(ifelse(mydata5$JobCategory == "Other", 1, 0))
mydata5$JobCategory_Sales <- as.factor(ifelse(mydata5$JobCategory == "Sales", 1, 0))
mydata5$JobCategory_Technical <- as.factor(ifelse(mydata5$JobCategory == "Technical", 1, 0))

#names(mydata5)
mydata5 <- mydata5[,-9]
#str(mydata5)
#dim(mydata5)
#sort(unique(mydata5$Age))



model1c <- glm(Eligible_in_2017 ~ 
                 CoachingEngaged + 
                 ChallengeCompleter + 
                 OnlineClassQuizCompleter + 
                 ConnectedEngaged + 
                 CoachingEngaged:ChallengeCompleter + 
                 CoachingEngaged:OnlineClassQuizCompleter + 
                 ChallengeCompleter:OnlineClassQuizCompleter + 
                 CoachingEngaged:ConnectedEngaged + 
                 ChallengeCompleter:ConnectedEngaged + 
                 OnlineClassQuizCompleter:ConnectedEngaged + 
                 CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                 CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                 CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                 ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                 CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                 Gender*Age + 
                 JobCategory_Laborer + 
                 JobCategory_Technical + 
                 JobCategory_Sales + 
                 JobCategory_Other +
                 JobCategory_Managerial + 
                 JobCategory_Clerical + 
                 #JobCategory_Professional +     #do not list 'Professional' because it is baseline.
                 Education + 
                 EmploymentDuration + 
                 Division +
                 OutcomeIncentiveDesign + 
                 RewardAmount +  
                 Group_Fraction_With_One_Or_More_Product_Engagements, 
               family=binomial(link = "probit"), 
               data=mydata5)

summary(model1c)
#str(mydata5)




#backward selection model build; highest p-value variable removed is commented out below
#variable removed:CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged
model2 <- glm(Eligible_in_2017 ~ 
                CoachingEngaged + 
                ChallengeCompleter + 
                OnlineClassQuizCompleter + 
                ConnectedEngaged + 
                CoachingEngaged:ChallengeCompleter + 
                CoachingEngaged:OnlineClassQuizCompleter + 
                ChallengeCompleter:OnlineClassQuizCompleter + 
                CoachingEngaged:ConnectedEngaged + 
                ChallengeCompleter:ConnectedEngaged + 
                OnlineClassQuizCompleter:ConnectedEngaged + 
                CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                Gender*Age + 
                JobCategory_Laborer + 
                JobCategory_Technical + 
                JobCategory_Sales + 
                JobCategory_Other +
                JobCategory_Managerial + 
                JobCategory_Clerical + 
                Education + 
                EmploymentDuration + 
                Division +
                OutcomeIncentiveDesign + 
                RewardAmount +  
                Group_Fraction_With_One_Or_More_Product_Engagements, 
              family=binomial(link = "probit"), 
              data=mydata5)

summary(model2)


#variable removed: ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged
model3 <- glm(Eligible_in_2017 ~ 
                CoachingEngaged + 
                ChallengeCompleter + 
                OnlineClassQuizCompleter + 
                ConnectedEngaged + 
                CoachingEngaged:ChallengeCompleter + 
                CoachingEngaged:OnlineClassQuizCompleter + 
                ChallengeCompleter:OnlineClassQuizCompleter + 
                CoachingEngaged:ConnectedEngaged + 
                ChallengeCompleter:ConnectedEngaged + 
                OnlineClassQuizCompleter:ConnectedEngaged + 
                CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                #ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                Gender*Age + 
                JobCategory_Laborer + 
                JobCategory_Technical + 
                JobCategory_Sales + 
                JobCategory_Other +
                JobCategory_Managerial + 
                JobCategory_Clerical + 
                Education + 
                EmploymentDuration + 
                Division +
                OutcomeIncentiveDesign + 
                RewardAmount +  
                Group_Fraction_With_One_Or_More_Product_Engagements, 
              family=binomial(link = "probit"), 
              data=mydata5)

summary(model3)


#variable removed: CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged
model4 <- glm(Eligible_in_2017 ~ 
                CoachingEngaged + 
                ChallengeCompleter + 
                OnlineClassQuizCompleter + 
                ConnectedEngaged + 
                CoachingEngaged:ChallengeCompleter + 
                CoachingEngaged:OnlineClassQuizCompleter + 
                ChallengeCompleter:OnlineClassQuizCompleter + 
                CoachingEngaged:ConnectedEngaged + 
                ChallengeCompleter:ConnectedEngaged + 
                OnlineClassQuizCompleter:ConnectedEngaged + 
                CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                #CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                #ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                Gender*Age + 
                JobCategory_Laborer + 
                JobCategory_Technical + 
                JobCategory_Sales + 
                JobCategory_Other +
                JobCategory_Managerial + 
                JobCategory_Clerical + 
                Education + 
                EmploymentDuration + 
                Division +
                OutcomeIncentiveDesign + 
                RewardAmount +  
                Group_Fraction_With_One_Or_More_Product_Engagements, 
              family=binomial(link = "probit"), 
              data=mydata5)

summary(model4)


#variable removed: CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter
model5 <- glm(Eligible_in_2017 ~ 
                CoachingEngaged + 
                ChallengeCompleter + 
                OnlineClassQuizCompleter + 
                ConnectedEngaged + 
                CoachingEngaged:ChallengeCompleter + 
                CoachingEngaged:OnlineClassQuizCompleter + 
                ChallengeCompleter:OnlineClassQuizCompleter + 
                CoachingEngaged:ConnectedEngaged + 
                ChallengeCompleter:ConnectedEngaged + 
                OnlineClassQuizCompleter:ConnectedEngaged + 
                #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                #CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                #ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                Gender*Age + 
                JobCategory_Laborer + 
                JobCategory_Technical + 
                JobCategory_Sales + 
                JobCategory_Other +
                JobCategory_Managerial + 
                JobCategory_Clerical + 
                Education + 
                EmploymentDuration + 
                Division +
                OutcomeIncentiveDesign + 
                RewardAmount +  
                Group_Fraction_With_One_Or_More_Product_Engagements, 
              family=binomial(link = "probit"), 
              data=mydata5)

summary(model5)


#variable removed: CoachingEngaged:ChallengeCompleter has the highest p-value, but this pair is contained within a three-way interaction.
#So, CoachingEngaged:ChallengeCompleter:ConnectedEngaged is removed.
model6 <- glm(Eligible_in_2017 ~ 
                CoachingEngaged + 
                ChallengeCompleter + 
                OnlineClassQuizCompleter + 
                ConnectedEngaged + 
                CoachingEngaged:ChallengeCompleter + 
                CoachingEngaged:OnlineClassQuizCompleter + 
                ChallengeCompleter:OnlineClassQuizCompleter + 
                CoachingEngaged:ConnectedEngaged + 
                ChallengeCompleter:ConnectedEngaged + 
                OnlineClassQuizCompleter:ConnectedEngaged + 
                #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                #CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                #CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                #ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                Gender*Age + 
                JobCategory_Laborer + 
                JobCategory_Technical + 
                JobCategory_Sales + 
                JobCategory_Other +
                JobCategory_Managerial + 
                JobCategory_Clerical + 
                Education + 
                EmploymentDuration + 
                Division +
                OutcomeIncentiveDesign + 
                RewardAmount +  
                Group_Fraction_With_One_Or_More_Product_Engagements, 
              family=binomial(link = "probit"), 
              data=mydata5)

summary(model6)


#variable removed: CoachingEngaged:ConnectedEngaged
model7 <- glm(Eligible_in_2017 ~ 
                CoachingEngaged + 
                ChallengeCompleter + 
                OnlineClassQuizCompleter + 
                ConnectedEngaged + 
                CoachingEngaged:ChallengeCompleter + 
                CoachingEngaged:OnlineClassQuizCompleter + 
                ChallengeCompleter:OnlineClassQuizCompleter + 
                #CoachingEngaged:ConnectedEngaged + 
                ChallengeCompleter:ConnectedEngaged + 
                OnlineClassQuizCompleter:ConnectedEngaged + 
                #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                #CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                #CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                #ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                Gender*Age + 
                JobCategory_Laborer + 
                JobCategory_Technical + 
                JobCategory_Sales + 
                JobCategory_Other +
                JobCategory_Managerial + 
                JobCategory_Clerical + 
                Education + 
                EmploymentDuration + 
                Division +
                OutcomeIncentiveDesign + 
                RewardAmount +  
                Group_Fraction_With_One_Or_More_Product_Engagements, 
              family=binomial(link = "probit"), 
              data=mydata5)

summary(model7)


#variable removed: CoachingEngaged:ChallengeCompleter
model8 <- glm(Eligible_in_2017 ~ 
                 CoachingEngaged + 
                 ChallengeCompleter + 
                 OnlineClassQuizCompleter + 
                 ConnectedEngaged + 
                 #CoachingEngaged:ChallengeCompleter + 
                 CoachingEngaged:OnlineClassQuizCompleter + 
                 ChallengeCompleter:OnlineClassQuizCompleter + 
                 #CoachingEngaged:ConnectedEngaged + 
                 ChallengeCompleter:ConnectedEngaged + 
                 OnlineClassQuizCompleter:ConnectedEngaged + 
                 #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                 #CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                 #CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                 #ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                 #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                 Gender*Age + 
                 JobCategory_Laborer + 
                 JobCategory_Technical + 
                 JobCategory_Sales + 
                 JobCategory_Other +
                 JobCategory_Managerial + 
                 JobCategory_Clerical + 
                 Education + 
                 EmploymentDuration + 
                 Division +
                 OutcomeIncentiveDesign + 
                 RewardAmount +  
                 Group_Fraction_With_One_Or_More_Product_Engagements, 
               family=binomial(link = "probit"), 
               data=mydata5)

summary(model8)


#variable removed: JobCategory_Other
model9 <- glm(Eligible_in_2017 ~ 
                 CoachingEngaged + 
                 ChallengeCompleter + 
                 OnlineClassQuizCompleter + 
                 ConnectedEngaged + 
                 #CoachingEngaged:ChallengeCompleter + 
                 CoachingEngaged:OnlineClassQuizCompleter + 
                 ChallengeCompleter:OnlineClassQuizCompleter + 
                 #CoachingEngaged:ConnectedEngaged + 
                 ChallengeCompleter:ConnectedEngaged + 
                 OnlineClassQuizCompleter:ConnectedEngaged + 
                 #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                 #CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                 #CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                 #ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                 #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                 Gender*Age + 
                 JobCategory_Laborer + 
                 JobCategory_Technical + 
                 JobCategory_Sales + 
                 #JobCategory_Other +
                 JobCategory_Managerial + 
                 JobCategory_Clerical + 
                 Education + 
                 EmploymentDuration + 
                 Division +
                 OutcomeIncentiveDesign + 
                 RewardAmount +  
                 Group_Fraction_With_One_Or_More_Product_Engagements, 
               family=binomial(link = "probit"), 
               data=mydata5)

summary(model9)



#create binary flags for the job category variable
table(mydata5$Education)
mydata5$Education_HSgrad_or_GED <- as.factor(ifelse(mydata5$Education == "High school graduate or GED", 1, 0))
mydata5$Education_2_year_degree <- as.factor(ifelse(mydata5$Education == "2-year degree", 1, 0))
mydata5$Education_4_year_degree <- as.factor(ifelse(mydata5$Education == "4-year degree", 1, 0))
mydata5$Education_8th_grade_or_less <- as.factor(ifelse(mydata5$Education == "8th grade or less", 1, 0))
mydata5$Education_Post_graduate <- as.factor(ifelse(mydata5$Education == "Post graduate", 1, 0))
mydata5$Education_Some_college_or_voc_train <- as.factor(ifelse(mydata5$Education == "Some college or vocational training", 1, 0))
mydata5$Education_Some_HS <- as.factor(ifelse(mydata5$Education == "Some high school", 1, 0))

#str(mydata5)
#table(mydata5$Education_HSgrad_or_GED)


model10a <- glm(Eligible_in_2017 ~ 
                  CoachingEngaged + 
                  ChallengeCompleter + 
                  OnlineClassQuizCompleter + 
                  ConnectedEngaged + 
                  #CoachingEngaged:ChallengeCompleter + 
                  CoachingEngaged:OnlineClassQuizCompleter + 
                  ChallengeCompleter:OnlineClassQuizCompleter + 
                  #CoachingEngaged:ConnectedEngaged + 
                  ChallengeCompleter:ConnectedEngaged + 
                  OnlineClassQuizCompleter:ConnectedEngaged + 
                  #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                  #CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                  #CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                  #ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                  #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                  Gender*Age + 
                  JobCategory_Laborer + 
                  JobCategory_Technical + 
                  JobCategory_Sales + 
                  #JobCategory_Other +
                  JobCategory_Managerial + 
                  JobCategory_Clerical +
                  
                  mydata5$Education_2_year_degree + 
                  mydata5$Education_4_year_degree + 
                  mydata5$Education_8th_grade_or_less + 
                  mydata5$Education_Post_graduate + 
                  mydata5$Education_Some_college_or_voc_train + 
                  mydata5$Education_Some_HS + 
                  
                  EmploymentDuration + 
                  Division +
                  OutcomeIncentiveDesign + 
                  RewardAmount +  
                  Group_Fraction_With_One_Or_More_Product_Engagements, 
                family=binomial(link = "probit"), 
                data=mydata5)

summary(model10a)


#variable removed: mydata5$Education_Some_HS
model10b <- glm(Eligible_in_2017 ~ 
                  CoachingEngaged + 
                  ChallengeCompleter + 
                  OnlineClassQuizCompleter + 
                  ConnectedEngaged + 
                  #CoachingEngaged:ChallengeCompleter + 
                  CoachingEngaged:OnlineClassQuizCompleter + 
                  ChallengeCompleter:OnlineClassQuizCompleter + 
                  #CoachingEngaged:ConnectedEngaged + 
                  ChallengeCompleter:ConnectedEngaged + 
                  OnlineClassQuizCompleter:ConnectedEngaged + 
                  #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                  #CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                  #CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                  #ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                  #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                  Gender*Age + 
                  JobCategory_Laborer + 
                  JobCategory_Technical + 
                  JobCategory_Sales + 
                  #JobCategory_Other +
                  JobCategory_Managerial + 
                  JobCategory_Clerical +
                  
                  mydata5$Education_2_year_degree + 
                  mydata5$Education_4_year_degree + 
                  mydata5$Education_8th_grade_or_less + 
                  mydata5$Education_Post_graduate + 
                  mydata5$Education_Some_college_or_voc_train + 
                  #mydata5$Education_Some_HS + 
                  
                  EmploymentDuration + 
                  Division +
                  OutcomeIncentiveDesign + 
                  RewardAmount +  
                  Group_Fraction_With_One_Or_More_Product_Engagements, 
                family=binomial(link = "probit"), 
                data=mydata5)

summary(model10b)


#variable removed: ChallengeCompleter:ConnectedEngaged
#Also, individual gender and age variables are created
model11a <- glm(Eligible_in_2017 ~ 
                  CoachingEngaged + 
                  ChallengeCompleter + 
                  OnlineClassQuizCompleter + 
                  ConnectedEngaged + 
                  #CoachingEngaged:ChallengeCompleter + 
                  CoachingEngaged:OnlineClassQuizCompleter + 
                  ChallengeCompleter:OnlineClassQuizCompleter + 
                  #CoachingEngaged:ConnectedEngaged + 
                  #ChallengeCompleter:ConnectedEngaged + 
                  OnlineClassQuizCompleter:ConnectedEngaged + 
                  #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                  #CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                  #CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                  #ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                  #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                  Gender + 
                  Age + 
                  Gender:Age + 
                  JobCategory_Laborer + 
                  JobCategory_Technical + 
                  JobCategory_Sales + 
                  #JobCategory_Other +
                  JobCategory_Managerial + 
                  JobCategory_Clerical +
                  
                  mydata5$Education_2_year_degree + 
                  mydata5$Education_4_year_degree + 
                  mydata5$Education_8th_grade_or_less + 
                  mydata5$Education_Post_graduate + 
                  mydata5$Education_Some_college_or_voc_train + 
                  #mydata5$Education_Some_HS + 
                  
                  EmploymentDuration + 
                  Division +
                  OutcomeIncentiveDesign + 
                  RewardAmount +  
                  Group_Fraction_With_One_Or_More_Product_Engagements, 
                family=binomial(link = "probit"), 
                data=mydata5)

summary(model11a)


#variable removed: Gender:Age
model11b <- glm(Eligible_in_2017 ~ 
                  CoachingEngaged + 
                  ChallengeCompleter + 
                  OnlineClassQuizCompleter + 
                  ConnectedEngaged + 
                  #CoachingEngaged:ChallengeCompleter + 
                  CoachingEngaged:OnlineClassQuizCompleter + 
                  ChallengeCompleter:OnlineClassQuizCompleter + 
                  #CoachingEngaged:ConnectedEngaged + 
                  #ChallengeCompleter:ConnectedEngaged + 
                  OnlineClassQuizCompleter:ConnectedEngaged + 
                  #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                  #CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                  #CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                  #ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                  #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                  Gender + 
                  Age + 
                  #Gender:Age + 
                  JobCategory_Laborer + 
                  JobCategory_Technical + 
                  JobCategory_Sales + 
                  #JobCategory_Other +
                  JobCategory_Managerial + 
                  JobCategory_Clerical +
                  
                  mydata5$Education_2_year_degree + 
                  mydata5$Education_4_year_degree + 
                  mydata5$Education_8th_grade_or_less + 
                  mydata5$Education_Post_graduate + 
                  mydata5$Education_Some_college_or_voc_train + 
                  #mydata5$Education_Some_HS + 
                  
                  EmploymentDuration + 
                  Division +
                  OutcomeIncentiveDesign + 
                  RewardAmount +  
                  Group_Fraction_With_One_Or_More_Product_Engagements, 
                family=binomial(link = "probit"), 
                data=mydata5)

summary(model11b)


#variable removed: OnlineClassQuizCompleter:ConnectedEngaged
model12 <- glm(Eligible_in_2017 ~ 
                 CoachingEngaged + 
                 ChallengeCompleter + 
                 OnlineClassQuizCompleter + 
                 ConnectedEngaged + 
                 #CoachingEngaged:ChallengeCompleter + 
                 CoachingEngaged:OnlineClassQuizCompleter + 
                 ChallengeCompleter:OnlineClassQuizCompleter + 
                 #CoachingEngaged:ConnectedEngaged + 
                 #ChallengeCompleter:ConnectedEngaged + 
                 #OnlineClassQuizCompleter:ConnectedEngaged + 
                 #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                 #CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                 #CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                 #ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                 #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                 Gender + 
                 Age + 
                 #Gender:Age + 
                 JobCategory_Laborer + 
                 JobCategory_Technical + 
                 JobCategory_Sales + 
                 #JobCategory_Other +
                 JobCategory_Managerial + 
                 JobCategory_Clerical +
                 
                 mydata5$Education_2_year_degree + 
                 mydata5$Education_4_year_degree + 
                 mydata5$Education_8th_grade_or_less + 
                 mydata5$Education_Post_graduate + 
                 mydata5$Education_Some_college_or_voc_train + 
                 #mydata5$Education_Some_HS + 
                 
                 EmploymentDuration + 
                 Division +
                 OutcomeIncentiveDesign + 
                 RewardAmount +  
                 Group_Fraction_With_One_Or_More_Product_Engagements, 
               family=binomial(link = "probit"), 
               data=mydata5)

summary(model12)


#variable removed: JobCategory_Clerical
model13 <- glm(Eligible_in_2017 ~ 
                 CoachingEngaged + 
                 ChallengeCompleter + 
                 OnlineClassQuizCompleter + 
                 ConnectedEngaged + 
                 #CoachingEngaged:ChallengeCompleter + 
                 CoachingEngaged:OnlineClassQuizCompleter + 
                 ChallengeCompleter:OnlineClassQuizCompleter + 
                 #CoachingEngaged:ConnectedEngaged + 
                 #ChallengeCompleter:ConnectedEngaged + 
                 #OnlineClassQuizCompleter:ConnectedEngaged + 
                 #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter + 
                 #CoachingEngaged:ChallengeCompleter:ConnectedEngaged + 
                 #CoachingEngaged:OnlineClassQuizCompleter:ConnectedEngaged + 
                 #ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                 #CoachingEngaged:ChallengeCompleter:OnlineClassQuizCompleter:ConnectedEngaged + 
                 Gender + 
                 Age + 
                 #Gender:Age + 
                 JobCategory_Laborer + 
                 JobCategory_Technical + 
                 JobCategory_Sales + 
                 #JobCategory_Other +
                 JobCategory_Managerial + 
                 #JobCategory_Clerical +
                 
                 mydata5$Education_2_year_degree + 
                 mydata5$Education_4_year_degree + 
                 mydata5$Education_8th_grade_or_less + 
                 mydata5$Education_Post_graduate + 
                 mydata5$Education_Some_college_or_voc_train + 
                 #mydata5$Education_Some_HS + 
                 
                 EmploymentDuration + 
                 Division +
                 OutcomeIncentiveDesign + 
                 RewardAmount +  
                 Group_Fraction_With_One_Or_More_Product_Engagements, 
               family=binomial(link = "probit"), 
               data=mydata5)

summary(model13)



#finalmodel is the same as the previous model but with the commented-out variables removed
finalmodel <- glm(Eligible_in_2017 ~ 
                    CoachingEngaged + 
                    ChallengeCompleter + 
                    OnlineClassQuizCompleter + 
                    ConnectedEngaged + 
                    CoachingEngaged:OnlineClassQuizCompleter + 
                    ChallengeCompleter:OnlineClassQuizCompleter + 
                    Gender + 
                    Age + 
                    JobCategory_Laborer + 
                    JobCategory_Technical + 
                    JobCategory_Sales + 
                    JobCategory_Managerial + 
                    Education_2_year_degree + 
                    Education_4_year_degree + 
                    Education_8th_grade_or_less + 
                    Education_Post_graduate + 
                    Education_Some_college_or_voc_train + 
                    EmploymentDuration + 
                    Division +
                    OutcomeIncentiveDesign + 
                    RewardAmount +  
                    Group_Fraction_With_One_Or_More_Product_Engagements, 
                  family=binomial(link = "probit"), 
                  data=mydata5)

summary(finalmodel)
summary(finalmodel)$coeff



#table(mydata5$Age)
#print(summary(finalmodel),digits=3)
#format(summary(finalmodel), scientific=FALSE)






# # # # # # # # # # # # # # # # # # # # # # # # #
# # # 'Percent more likely' calculations  # # # #
# # # 'Percent more likely' calculations  # # # #
# # # 'Percent more likely' calculations  # # # #
# # # # # # # # # # # # # # # # # # # # # # # # #


#reference member was chosen to have median and mode values

med_Age <- median(mydata5$Age)
med_grp_fraction <- median(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements)
med_reward <- median(mydata5$RewardAmount)



#'Percent more likely' for: CoachingEngaged
pnorm( predict(finalmodel, data.frame(CoachingEngaged="1" , 
                                      ChallengeCompleter="0" , 
                                      OnlineClassQuizCompleter="0" , 
                                      ConnectedEngaged="0" ,
                                      JobCategory_Laborer="0" ,
                                      JobCategory_Technical="0" ,
                                      JobCategory_Sales="0" ,
                                      JobCategory_Managerial="0" ,
                                      JobCategory_Clerical="0" ,
                                      Education_2_year_degree="0",
                                      Education_4_year_degree="1",
                                      Education_Some_college_or_voc_train="0",
                                      Education_8th_grade_or_less="0",
                                      Education_Post_graduate="0",
                                      EmploymentDuration="More than 5 years" ,
                                      Division="PACIFIC" ,
                                      Group_Fraction_With_One_Or_More_Product_Engagements=med_grp_fraction,
                                      OutcomeIncentiveDesign = "0",
                                      RewardAmount = med_reward,
                                      Gender = "F",
                                      Age = med_Age)) ) /
  pnorm( predict(finalmodel, data.frame(CoachingEngaged="0" , 
                                        ChallengeCompleter="0" , 
                                        OnlineClassQuizCompleter="0" , 
                                        ConnectedEngaged="0" ,
                                        JobCategory_Laborer="0" ,
                                        JobCategory_Technical="0" ,
                                        JobCategory_Sales="0" ,
                                        JobCategory_Managerial="0" ,
                                        JobCategory_Clerical="0" ,
                                        Education_2_year_degree="0",
                                        Education_4_year_degree="1",
                                        Education_Some_college_or_voc_train="0",
                                        Education_8th_grade_or_less="0",
                                        Education_Post_graduate="0",
                                        EmploymentDuration="More than 5 years" ,
                                        Division="PACIFIC" ,
                                        Group_Fraction_With_One_Or_More_Product_Engagements=med_grp_fraction,
                                        OutcomeIncentiveDesign = "0",
                                        RewardAmount = med_reward,
                                        Gender = "F",
                                        Age = med_Age)) )


#'Percent more likely' for: ChallengeCompleter
pnorm( predict(finalmodel, data.frame(CoachingEngaged="0" , 
                                      ChallengeCompleter="1" , 
                                      OnlineClassQuizCompleter="0" , 
                                      ConnectedEngaged="0" ,
                                      JobCategory_Laborer="0" ,
                                      JobCategory_Technical="0" ,
                                      JobCategory_Sales="0" ,
                                      JobCategory_Managerial="0" ,
                                      JobCategory_Clerical="0" ,
                                      Education_2_year_degree="0",
                                      Education_4_year_degree="1",
                                      Education_Some_college_or_voc_train="0",
                                      Education_8th_grade_or_less="0",
                                      Education_Post_graduate="0",
                                      EmploymentDuration="More than 5 years" ,
                                      Division="PACIFIC" ,
                                      Group_Fraction_With_One_Or_More_Product_Engagements=med_grp_fraction,
                                      OutcomeIncentiveDesign = "0",
                                      RewardAmount = med_reward,
                                      Gender = "F",
                                      Age = med_Age)) ) /
  pnorm( predict(finalmodel, data.frame(CoachingEngaged="0" , 
                                        ChallengeCompleter="0" , 
                                        OnlineClassQuizCompleter="0" , 
                                        ConnectedEngaged="0" ,
                                        JobCategory_Laborer="0" ,
                                        JobCategory_Technical="0" ,
                                        JobCategory_Sales="0" ,
                                        JobCategory_Managerial="0" ,
                                        JobCategory_Clerical="0" ,
                                        Education_2_year_degree="0",
                                        Education_4_year_degree="1",
                                        Education_Some_college_or_voc_train="0",
                                        Education_8th_grade_or_less="0",
                                        Education_Post_graduate="0",
                                        EmploymentDuration="More than 5 years" ,
                                        Division="PACIFIC" ,
                                        Group_Fraction_With_One_Or_More_Product_Engagements=med_grp_fraction,
                                        OutcomeIncentiveDesign = "0",
                                        RewardAmount = med_reward,
                                        Gender = "F",
                                        Age = med_Age)) )


#'Percent more likely' for: OnlineClassQuizCompleter
pnorm( predict(finalmodel, data.frame(CoachingEngaged="0" , 
                                      ChallengeCompleter="0" , 
                                      OnlineClassQuizCompleter="1" , 
                                      ConnectedEngaged="0" ,
                                      JobCategory_Laborer="0" ,
                                      JobCategory_Technical="0" ,
                                      JobCategory_Sales="0" ,
                                      JobCategory_Managerial="0" ,
                                      JobCategory_Clerical="0" ,
                                      Education_2_year_degree="0",
                                      Education_4_year_degree="1",
                                      Education_Some_college_or_voc_train="0",
                                      Education_8th_grade_or_less="0",
                                      Education_Post_graduate="0",
                                      EmploymentDuration="More than 5 years" ,
                                      Division="PACIFIC" ,
                                      Group_Fraction_With_One_Or_More_Product_Engagements=med_grp_fraction,
                                      OutcomeIncentiveDesign = "0",
                                      RewardAmount = med_reward,
                                      Gender = "F",
                                      Age = med_Age)) ) /
  pnorm( predict(finalmodel, data.frame(CoachingEngaged="0" , 
                                        ChallengeCompleter="0" , 
                                        OnlineClassQuizCompleter="0" , 
                                        ConnectedEngaged="0" ,
                                        JobCategory_Laborer="0" ,
                                        JobCategory_Technical="0" ,
                                        JobCategory_Sales="0" ,
                                        JobCategory_Managerial="0" ,
                                        JobCategory_Clerical="0" ,
                                        Education_2_year_degree="0",
                                        Education_4_year_degree="1",
                                        Education_Some_college_or_voc_train="0",
                                        Education_8th_grade_or_less="0",
                                        Education_Post_graduate="0",
                                        EmploymentDuration="More than 5 years" ,
                                        Division="PACIFIC" ,
                                        Group_Fraction_With_One_Or_More_Product_Engagements=med_grp_fraction,
                                        OutcomeIncentiveDesign = "0",
                                        RewardAmount = med_reward,
                                        Gender = "F",
                                        Age = med_Age)) )


#'Percent more likely' for: ConnectedEngaged
pnorm( predict(finalmodel, data.frame(CoachingEngaged="0" , 
                                      ChallengeCompleter="0" , 
                                      OnlineClassQuizCompleter="0" , 
                                      ConnectedEngaged="1" ,
                                      JobCategory_Laborer="0" ,
                                      JobCategory_Technical="0" ,
                                      JobCategory_Sales="0" ,
                                      JobCategory_Managerial="0" ,
                                      JobCategory_Clerical="0" ,
                                      Education_2_year_degree="0",
                                      Education_4_year_degree="1",
                                      Education_Some_college_or_voc_train="0",
                                      Education_8th_grade_or_less="0",
                                      Education_Post_graduate="0",
                                      EmploymentDuration="More than 5 years" ,
                                      Division="PACIFIC" ,
                                      Group_Fraction_With_One_Or_More_Product_Engagements=med_grp_fraction,
                                      OutcomeIncentiveDesign = "0",
                                      RewardAmount = med_reward,
                                      Gender = "F",
                                      Age = med_Age)) ) /
  pnorm( predict(finalmodel, data.frame(CoachingEngaged="0" , 
                                        ChallengeCompleter="0" , 
                                        OnlineClassQuizCompleter="0" , 
                                        ConnectedEngaged="0" ,
                                        JobCategory_Laborer="0" ,
                                        JobCategory_Technical="0" ,
                                        JobCategory_Sales="0" ,
                                        JobCategory_Managerial="0" ,
                                        JobCategory_Clerical="0" ,
                                        Education_2_year_degree="0",
                                        Education_4_year_degree="1",
                                        Education_Some_college_or_voc_train="0",
                                        Education_8th_grade_or_less="0",
                                        Education_Post_graduate="0",
                                        EmploymentDuration="More than 5 years" ,
                                        Division="PACIFIC" ,
                                        Group_Fraction_With_One_Or_More_Product_Engagements=med_grp_fraction,
                                        OutcomeIncentiveDesign = "0",
                                        RewardAmount = med_reward,
                                        Gender = "F",
                                        Age = med_Age)) )


#'Percent more likely' for: All 4 products
pnorm( predict(finalmodel, data.frame(CoachingEngaged="1" , 
                                      ChallengeCompleter="1" , 
                                      OnlineClassQuizCompleter="1" , 
                                      ConnectedEngaged="1" ,
                                      JobCategory_Laborer="0" ,
                                      JobCategory_Technical="0" ,
                                      JobCategory_Sales="0" ,
                                      JobCategory_Managerial="0" ,
                                      JobCategory_Clerical="0" ,
                                      Education_2_year_degree="0",
                                      Education_4_year_degree="1",
                                      Education_Some_college_or_voc_train="0",
                                      Education_8th_grade_or_less="0",
                                      Education_Post_graduate="0",
                                      EmploymentDuration="More than 5 years" ,
                                      Division="PACIFIC" ,
                                      Group_Fraction_With_One_Or_More_Product_Engagements=med_grp_fraction,
                                      OutcomeIncentiveDesign = "0",
                                      RewardAmount = med_reward,
                                      Gender = "F",
                                      Age = med_Age)) ) /
  pnorm( predict(finalmodel, data.frame(CoachingEngaged="0" , 
                                        ChallengeCompleter="0" , 
                                        OnlineClassQuizCompleter="0" , 
                                        ConnectedEngaged="0" ,
                                        JobCategory_Laborer="0" ,
                                        JobCategory_Technical="0" ,
                                        JobCategory_Sales="0" ,
                                        JobCategory_Managerial="0" ,
                                        JobCategory_Clerical="0" ,
                                        Education_2_year_degree="0",
                                        Education_4_year_degree="1",
                                        Education_Some_college_or_voc_train="0",
                                        Education_8th_grade_or_less="0",
                                        Education_Post_graduate="0",
                                        EmploymentDuration="More than 5 years" ,
                                        Division="PACIFIC" ,
                                        Group_Fraction_With_One_Or_More_Product_Engagements=med_grp_fraction,
                                        OutcomeIncentiveDesign = "0",
                                        RewardAmount = med_reward,
                                        Gender = "F",
                                        Age = med_Age)) )






#histogram of the Culutre of Health with call-outs for some major clients
names(mydata5)
unique(mydata5[mydata5$GroupName=="AMERICAN SPECIALTY HEALTH, INC.",19])
unique(mydata5[mydata5$GroupName=="ASDF1",19])
unique(mydata5[mydata5$GroupName=="ASDF2",19])
unique(mydata5[mydata5$GroupName=="ASDF3",19])

unique(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements)

mean(unique(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements))
median(unique(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements))

mean(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements)
median(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements) 


#table(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements)
#hist(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements, col = "gray", ylim = c(0,80000), xlab = "", ylab = "Count", main = "Group Fraction with One or More Product Engagements")
hist(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements, yaxt = "n", col = "gray", ylim = c(0,100000), xlim = c(0,1), xlab = "", ylab = "Count", main = "Count of Members by Culture of Health")
axis(2, at=seq(0, 100000, by=20000), labels=c("0", "20000", "40000", "60000", "80000", "100000")) 
abline(v=0.5310219, lwd = 2) + text(x=.49,y=70000,"ASH (53%)", cex = .75)
abline(v=0.08223106, lwd = 2) + text(x=.14,y=69000,"ASDF1 (8%)", cex = .75)
abline(v=0.6025425, lwd = 2) + text(x=.675,y=70000,"ASDF2 (60%)", cex = .75)
abline(v=0.02754836, lwd = 2) + text(x=.085,y=79000,"ASDF3 (3%)", cex = .75)






#simple boxplots for appendix slides
boxplot(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements, main = "Box Plot: Culture of Health")
boxplot(unique(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements), main = "Box Plot: Culture of Health")


mean(unique(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements))
median(unique(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements))

mean(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements)
median(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements) 

length(unique(mydata5$Group_Fraction_With_One_Or_More_Product_Engagements))
