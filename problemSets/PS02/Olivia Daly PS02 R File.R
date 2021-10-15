getwd()

#setwd(C:olivi/Documents/TCDASDS/StatsI_Fall2021-main/problemSets)

setwd("~/TCD ASDS/StatsI_Fall2021-main/problemSets")



library(ggplot2)
library(tidyverse)


#PS02 Question 1#

#Q1 Part (a)#


F01 <- 14
F02 <- 6
F03 <- 7
F04 <- 7
F05 <- 7
F06 <- 1


row_total_1  <- 27 #(F01 + F02 + F03)
row_total_2 <- 15 #(F04 + F05 + F06)

col_total_1 <- 21
col_total_2 <- 13
col_total_3 <- 8


Grand_total <- 42

#FeN = row total/Grand Total * Column total 

Fe1 <- ((row_total_1/Grand_total)*col_total_1)

Fe1

Fe2 <- ((row_total_1/Grand_total)*col_total_2)
  
Fe3 <- ((row_total_1/Grand_total)*col_total_3)

Fe4 <- ((row_total_2/Grand_total)*col_total_1)

Fe5 <- ((row_total_2/Grand_total)*col_total_2)

Fe6 <- ((row_total_2/Grand_total)*col_total_3)


# xsquared <- sum((F0 - Fe)^2/Fe)

xsquared <- ((F01 - Fe1)^2/Fe1) + ((F02 - Fe2)^2/Fe2) + ((F03 - Fe3)^2/Fe3) + ((F04 - Fe4)^2/Fe4) + ((F05 - Fe5)^2/Fe5) + ((F06 - Fe6)^2/Fe6) 

xsquared


#Q1 Part (b)#


#Example (taken from this website:https://www.statology.org/create-table-in-r/ )


tab <- matrix(c(7, 5, 14, 19, 3, 2, 17, 6, 12), ncol=3, byrow=TRUE)
colnames(tab) <- c('colName1','colName2','colName3')
rownames(tab) <- c('rowName1','rowName2','rowName3')
tab <- as.table(tab)
#print
tab

#applied to this question:

police_experiment <-matrix(c(14, 6, 7, 7, 7, 1 ), nrow = 3, ncol = 2)
colnames(police_experiment) <- c('Not Stopped','Bribe Requested','Stopped/ Given Warning')
rownames(police_experiment) <- c('Upper Class','Lower Class')
police_experiment <- as.table(police_experiment)

#print 

#{police_experiment

#t.test(police_experiment)

#p-value given = 0.001324}- this first attempt was wrong and then I realised 


#to get p value: chisq(xsquaredstat, df, lower.tail= FALSE) (tail you're looking at may vary)


#calculating degrees of freedom: (rows-1)(columns-1)

df <- (2-1)*(3-1)

#print

df

pchisq(3.79, df = 2, lower.tail = F )

#p value found = 0.1503183

#lower tail is false because we are not just looking at the lower tail, we are looking at both tails
#lower tail = TRUE means explicitly looking at lower tail

#p value 

#null hypothesis is that there is no difference between upper class and lower class drivers regarding 
#how police treat them
#alternative hypothesis is that there is a significant difference 

#in our case alpha is 0.1, the p value found is 0.1503183, this p value is slightly higher,
#meaning that we reject the null hypothesis that there is no difference between upper and lower class drivers
#and the p value influences statistical significance

#NB COME BACK TO THIS LATER W REF TO SLIDES- THE ABOVE EXPLANATION MAY BE WRONG- 
# MAY BE THAT WE REJECT THE NULL HYPOTHESIS THAT THE VARIABLES ARE STATISTICALLY INDEPENDENT,
#IE THEY ARE NOT, WE HAVE SHOWN THERE'S AN INDICATION OF A RELATIONSHIP THERE 


#Question 1 part (c)#




#police_experiment_data <- read.csv("PS02 Question 1 Data.csv")


police_experiment_data = read.table("PS02_Question_1_Data.csv", header=TRUE, sep = ',')
View(police_experiment_data)

police_experiment_data$UpperClass <- factor(police_experiment_data$UpperClass)
police_experiment_data$LowerClass <- factor(police_experiment_data$LowerClass)

chisq <- chisq.test(table(police_experiment_data$UpperClass, police_experiment_data$LowerClass))

ls(police_experiment_data)

chisq
chisq$residuals

#note: warning message about incorrectly approximated p values etc

#the above is also wrong, here is a more correct attempt using the matrix I made in the first part of the question:

chisq_test <- chisq.test(police_experiment, correct = FALSE)
chisq_test
#chisq_test$residuals

chisq_test$stdres




#Question 1 Part (d)#

#The lower class offered more bribes and more warnings than what our expected value was
#the further away it is from zero (we don't expect any variation) the more surprised we are. 
#The lower class having bribes requested of them and being stopped/ given a warning were quite farf away
#from our expectations 


#Question 2# 

#Question 2 part (a)#

#Ho = the reservation policy has no effect on the number of new or repaired drinking water facilities in the
#villages.

#Ha = the reservation policy does have an impact on the number of new and repaired drinking water facilities

#Question 2 Part (b) #

install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")

library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)



policy_data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

summary(policy_data)

x <- policy_data$reserved

y <- policy_data$irrigation

lm1 <- lm(policy_data$irrigation~policy_data$reserved)

summary(lm1)

# gives us the p value -- > fail to reject the null hypothesis

# (c) Interpret the coefficient estimate for reservation policy.

# it is the estimate coefficient --> (- 0.36)- this is
#negative so the reservation policy has a negative impact on the irrigation repair systems










#Question 3#

#Question 3 part 1#

#Import the data set and obtain summary statistiscs and examine the distribution of
# the overall lifespan of the fruitflies.

FruitFlies <- read.csv("https://www.zoology.ubc.ca/~bio501/R/data/fruitflies.csv")

summary(FruitFlies)


hist(FruitFlies$longevity.days,
     main = "Scatter Plot of the overall lifespan of FruitFlies",
     xlab = "Number of Days",
     ylab = "Frequency Distribution")

#Question 3 Part 2#

#Plot lifespan vs thorax. Does it look like there is a linear relationship? Provide the
# plot. What is the correlation coefficient between these two variables?


scatter.smooth(FruitFlies$longevity.days, FruitFlies$thorax.mm, xlab = 'Longevity', ylab = 'Thorax', main = 'Scatterplot of linear relationship', col = c("blue", "red", "orange", "black"))

ggplot(aes(longevity.days, thorax.mm), data = FruitFlies) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  ggtitle("Scatterplot of linear relationship") +
  labs(y="Thorax")+
  labs(x = "Longevity")

cor.test(FruitFlies$longevity.days, FruitFlies$thorax.mm)
# 0.63 means a strong positive correlation


#Question 3 Part 3#

#Regress lifespan on thorax. Interpret the slope of the fitted model.


lm <- lm(FruitFlies$longevity.days ~ FruitFlies$thorax.mm, data = FruitFlies)
lm # 144.33 --> for every 0.1 increase in Thorax, Longevity increases with
# one increase in thorax there is a 14,43 increase in days.
summary(lm)



#Question 3 part 4#

#Test for a significant linear relationship between lifespan and thorax. Provide and
# interpret your results of your test.


#we reject the null hypothesis -- > 1.497 e -15 --> Our null
#Hypothesis is Ho= no linear relationship between lifespan and thorax -->
# one variable influences variance in the other, therefore we reject it


#Question 3 Part 5#

#Provide the 90% confidence interval for the slope of the fitted model.

#  Use the formula of confidence interval.

lower_interval <- (144.33 - ((15.77*1.645)))
lower_interval
upper_interval <- (144.33 + ((15.77*1.645)))
upper_interval
# Use the function confint() in R

confint(lm)
confint(lm, level = 0.90)
# the confidence interval for the slope of a regression line

#Question 3 Part 6#

# Use the predict() function in R to (1) predict an individual fruitfly's lifespan when
# thorax=0.8 and (2) the average lifespan of fruitflies when thorax=0.8 by the fitted
# model. This requires that you compute prediction and confidence intervals. What
# are the expected values of lifespan? What are the prediction and confidence intervals
# around the expected values

# lm is the regression line --> we want to use the regression line to make prediction off
FruitFlies_ <- lm(longevity.days ~ thorax.mm, data = FruitFlies)
new_df <- data.frame (thorax.mm = 0.8)

predict(FruitFlies_, newdata = new_df)
# 54.4 days it would live

predict(FruitFlies_, newdata = new_df, interval = 'confidence')

#Question 3 Part 7#


#For a sequence of thorax values, draw a plot with their fitted values for lifespan, as
# well as the prediction intervals and confidence intervals.



model1 <- lm(longevity.days ~ thorax.mm, data=FruitFlies)
summary(model1)

plot(FruitFlies$longevity.days, FruitFlies$longevity.days, ylim=c(100, 200), xlab="QUET", ylab="SBP", main="Regression")
abline(model1, col="lightblue")
lower_interval <- (144.33 - ((15.77*1.645)))
lower_interval
upper_interval <- (144.33 + ((15.77*1.645)))
upper_interval

summary(FruitFlies$thorax.mm)
newx <- seq(0.760, 0.880, by=0.05)
plot(FruitFlies$longevity.days, FruitFlies$thorax.mm, ylim=c(100, 200), xlab="QUET", ylab="SBP", main="Regression")
abline(model1, col="lightblue")