#This analysis investigates the effect of a clinical treatment program (fully engaged versus not) on a desired
#clinical outcome after controlling for clinically relevant variables in age and gender. 
#I demonstrate the mathematical consistency between a logistic regression model with the whole un-stratified 
#dataset, and the logistic regression models created from specific strata (ie sub-populations within the 
#dataset) in response to evidence of significant interaction effects.
#These techniques, though not this data (see below for disclaimer), has previously been used to evaluate the 
#effects of a telephonic coaching program with the intended goal of tobacco cessation. 
#***All data here is fictitious and was created for the sole purpose of demonstrating the analysis technique.***


#Variables:
#'achieved' is a binary flag for if the desired outcome was achieved (eg tobacco cessation)
#'engaged' is a binary flag for if the treatment program was completed
#'old' is a binary flag for age 45+
#'male' is a binary flag for male gender


#Read in your dataset
mydata <- read.csv(file="file:///U:/Brandon/R/logistic regression interaction test data 2-19-18.csv", header=T)

#View the first 20 rows and confirm the data import was successful
head(mydata, 20)




###Full Model###
fit1 <- glm(achieved ~ engaged*old*male, family=binomial(link='logit'), data=mydata)
#The above model is a less code to get the model below, which shows all possible interaction combinations:
#glm(achieved ~ engaged + old + male + old*male + male*engaged + old*engaged + old*male*engaged, family=binomial(link='logit'), data=mydata)

summary(fit1)
#With a p-value of 0.035605, we see evidence of interaction at the .05 level between old age and male gender.
#This may lead some to stratify the data and re-run the logistic regression model for each of the 4 
#groups/'quadrants' between age and gender.

#Create new datasets for each stratum
data_old_male <- mydata[which(mydata$old==1 & mydata$male==1),]
data_old_female <- mydata[which(mydata$old==1 & mydata$male==0),]
data_young_male <-  mydata[which(mydata$old==0 & mydata$male==1),]
data_young_female <-  mydata[which(mydata$old==0 & mydata$male==0),]

#QA check to confirm our stratification was successful
unique(data_old_male$male)


#Re-run the logistic regression models for each stratum
#Note: there is no need to include the interaction terms here because each stratum represents uniform 
#demographics, so there would be no interaction.

#Old Males
fit2a <- glm(achieved ~ engaged, family=binomial(link='logit'), data=data_old_male)
summary(fit2a)


#Old Females
fit2b <- glm(achieved ~ engaged, family=binomial(link='logit'), data=data_old_female)
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

#For example, in the case of old males with the full model, engaged old males include all coefficients,
#whereas not-engaged old males only include coefficients 1, 3, 4, and 7. Therefore, the odds ratio
#that the outcome was achieved between engaged and not-engaged old males, is equal to e to the power of
#the sum of coefficients 2, 5, 6, and 8.
#An example interpretation of this odds ratio is: the odds of achieving the desired outcome for engaged 
#old males was 2.63 (1.63 times higher) than old males that were not-engaged.
#Note: the intercept is regarded as the first coefficient.


#List of the coefficients for the full model
summary(fit1)$coefficient


#We see the estimates of the odds ratios are identical between the stratified and full models

###Old Males###
exp(summary(fit2a)$coefficient[2,1])

exp(summary(fit1)$coefficient[2,1] +
  summary(fit1)$coefficient[5,1] +
  summary(fit1)$coefficient[6,1] +
  summary(fit1)$coefficient[8,1])


###Old Females###
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


