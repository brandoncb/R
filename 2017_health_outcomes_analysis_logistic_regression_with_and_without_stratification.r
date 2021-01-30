
#Variables:
#'achieved' is a binary flag for if the desired outcome was achieved (eg tobacco cessation)
#'engaged' is a binary flag for if the treatment program was completed
#'age_flag' is a binary flag for age 45+
#'male' is a binary flag for male gender


#Read in your dataset
mydata <- read.csv(file="file:///U:/BrandonBo/Brandon/R/Logistic regression stratification data 2.csv", header=T)

#View the first 20 rows and confirm the data import was successful
head(mydata, 20)
dim(mydata)




###Full Model###
fit1 <- glm(achieved ~ engaged*age_flag*male, family=binomial(link='logit'), data=mydata)
#The above model is a less code to get the model below, which shows all possible interaction combinations:
#glm(achieved ~ engaged + age_flag + male + age_flag*male + male*engaged + age_flag*engaged + age_flag*male*engaged, family=binomial(link='logit'), data=mydata)

summary(fit1)
#With a p-value of 0.035605, we see evidence of interaction at the .05 level between age_flag and male gender.
#This may lead some to stratify the data and re-run the logistic regression model for each of the 4 
#groups/'quadrants' between age and gender.

#Create new datasets for each stratum
data_age_flag_male <- mydata[which(mydata$age_flag==1 & mydata$male==1),]
data_age_flag_female <- mydata[which(mydata$age_flag==1 & mydata$male==0),]
data_young_male <-  mydata[which(mydata$age_flag==0 & mydata$male==1),]
data_young_female <-  mydata[which(mydata$age_flag==0 & mydata$male==0),]

#QA check to confirm our stratification was successful
unique(data_age_flag_male$male)


#Re-run the logistic regression models for each stratum
#Note: there is no need to include the interaction terms here because each stratum represents uniform 
#demographics, so there would be no interaction.

#Age_flag Males
fit2a <- glm(achieved ~ engaged, family=binomial(link='logit'), data=data_age_flag_male)
summary(fit2a)
#we see there is a statistically significant engagement-effect for this stratum (p=0.0133)


#Age_flag Females
fit2b <- glm(achieved ~ engaged, family=binomial(link='logit'), data=data_age_flag_female)
summary(fit2b)


#Young Males
fit2c <- glm(achieved ~ engaged, family=binomial(link='logit'), data=data_young_male)
summary(fit2c)


#Young Females
fit2d <- glm(achieved ~ engaged, family=binomial(link='logit'), data=data_young_female)
summary(fit2d)






#Now that the stratified models have been created, we compare the estimates for the odds ratios 
#before and after stratification.


#The following formulas utilize a unique feature of logistic regression models that allow for easy
#calculation of the odds ratio.
#In the simple model with one explanatory variable, the odds ratio is equal to the coefficient of that 
#variable set as the power of e. In the full model, the odds ratio is the sum of the variables' coefficients
#that are unique to the members that are engaged.

#For example, in the case of age_flag males with the full model, engaged age_flag males include all coefficients,
#whereas not-engaged age_flag males only include coefficients 1, 3, 4, and 7. Therefore, the odds ratio
#that the outcome was achieved between engaged and not-engaged age_flag males, is equal to e to the power of
#the sum of coefficients 2, 5, 6, and 8.
#An example interpretation of this odds ratio is: the odds of achieving the desired outcome for engaged 
#age_flag males was 2.63 (1.63 times higher) than age_flag males that were not-engaged.
#Note: the intercept is regarded as the first coefficient.


#List of the coefficients for the full model
summary(fit1)$coefficient


#Below we see the estimates of the odds ratios are identical between the stratified and full models

###Age_flag Males###
exp(summary(fit2a)$coefficient[2,1])

exp(summary(fit1)$coefficient[2,1] +
      summary(fit1)$coefficient[5,1] +
      summary(fit1)$coefficient[6,1] +
      summary(fit1)$coefficient[8,1])


###Age_flag Females###
exp(summary(fit2b)$coefficient[2,1])

exp(summary(fit1)$coefficient[2,1] +
      summary(fit1)$coefficient[5,1])


###Young Males###
exp(summary(fit2c)$coefficient[2,1])

exp(summary(fit1)$coefficient[2,1]+
      summary(fit1)$coefficient[6,1])


###Young Females###
exp(summary(fit2d)$coefficient[2,1])
exp(summary(fit1)$coefficient[2,1])

















#rm(list = ls())


