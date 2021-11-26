getwd()
setwd("C:/Users/olivi/OneDrive/Documents/TCD ASDS/StatsI_Fall2021-main/problemSets/StatsI_PS04.R")

options(scipen = 999) 
library(tidyverse)
#install.packages("broom")
library(broom)

install.packages(car)
library(car)
data(Prestige)
help(Prestige)

#Question 1



#Q1 (a) Create a new variable professional by recoding the variable type so that professionals
#are coded as 1, and blue and white collar workers are coded as 0 (Hint: ifelse.)


professional <- ifelse(Prestige$type == "prof", 1, 0) #dummy variable assigned so that
#if a worker is a professional they get a 1 and if they're a Blue collar or White collar 
#worker they get a 0



#Q1 (b) Run a linear model with prestige as an outcome and income, professional, and the
#interaction of the two as predictors (Note: this is a continuous ×dummy interaction.)

mod1 <- lm(Prestige$prestige ~ Prestige$income + professional + Prestige$income: professional, data = Prestige )
  
mod1


#Q1 (c) Write the prediction equation based on the result.

#y= B0 + BiXi + B2di + B3Xidi

prediction_equation <- 21.142259 + 0.003171*Prestige$income  + 37.781280*professional -0.002326*Prestige$income*professional 
+ D 

prediction_equation

#Q1 (d) Interpret the coefficient for income.
  
  #the coefficient for income that I found is 0.003171. 
  #This means that for every unit increase in income there is a 0.003171 
#unit increase in prestige


#Q1 (e) Interpret the coefficient for professional

#The coefficient I found for professional is 37.781280.
#This means that for every unit increase in professional there is a 37.781280
#unit increase in prestige.



#Q1 (f) What is the effect of a $1,000 increase in income on prestige score for professional
#occupations? In other words, we are interested in the marginal effect of income when
#the variable professional takes the value of 1. Calculate the change in ^y associated
#with a $1,000 increase in income based on your answer for (c).

income_0 <- 21.142259 + 0.003171*0  + 37.781280*1 -0.002326*0*1 + 1

income_0 #answer is 59.92354

income_1000 <- 21.142259 + 0.003171*1000  + 37.781280*1 -0.002326*1000*1 + 1

income_1000 #answer is 60.76854



marginal_effect <- income_1000 - income_0

marginal_effect

#marginal effect = income_1000 - income_0 = 60.76854 - 59.92354 =  0.845




#Q1 (g) What is the effect of changing one's occupations from non-professional to professional
#when her income is $6,000? We are interested in the marginal effect of professional
#jobs when the variable income takes the value of 6,000. Calculate the change in ^y
#based on your answer for (c)

income_professional <- 21.142259 + 0.003171*6000  + 37.781280*1 -0.002326*6000*1 + 1

income_professional #value found = 64.99354

income_nonprofessional <- 21.142259 + 0.003171*6000  + 37.781280*1 -0.002326*6000*0 + 0
  
income_nonprofessional # value found is  77.94954

marginal_effect_2 <- income_professional - income_nonprofessional
  
marginal_effect_2 # marginal effect found for professional
#jobs when the variable income takes the value of 6,000 is -12.956



#########


#Question 2



#Q2 (a) Use the results from a linear regression to determine whether having these yard signs
#in a precinct affects vote share (e.g., conduct a hypothesis test with ?? = .05).

#H0 = There is no impact

#Ha = There is an impact 

#need to get a T-score

#T-score = 

T_score <- 0.042/0.016

T_score #answer is 2.625, look up on the tables

#degrees of freedom 130

#2 sided test so multiply by 2, alpha value 0.05

#The p-value is .009703.

#The result is significant at p < .05.

#We reject the null hypothesis that the signs have no impact.


#Q2 (b) Use the results to determine whether being next to precincts with these yard signs
#affects vote share (e.g., conduct a hypothesis test with ?? = .05).

#H0 = the signs have no impact on neighbouring precincts

#Ha = the signs do have an impact on the neighbouring precincts 




T_score_2 <- 0.042/0.013

T_score_2 # value found = 3.230769

#degrees of freedom 130

#2 sided test so multiply by 2, alpha value 0.05

#The p-value is .001564.

#The result is significant at p < .05.

#We reject the null hypothesis that the signs have no impact
#on neighbouring precincts.





#Q2 (c) Interpret the coefficient for the constant term substantively.


#The constant of 0.302 is the y-intercept, so whenever there are no lawn signs
#at all, the support for Cuccinelli is 30.2%

#The lawn signs increase the votes by 4% but without them he already has 30.2%
#of the vote


#Q1 (d) Evaluate the model fit for this regression. What does this tell us about the importance
#of yard signs versus other factors that are not modeled?

#I am looking at the R squared value to understand the model fit. It is only
#0.094 or 9.4%, so the lawn signs only explain 9.4% of the vote share, so there 
#must be a lot of other variables present affecting the other 90.6%,
#like, maybe actual policies for instance. 
