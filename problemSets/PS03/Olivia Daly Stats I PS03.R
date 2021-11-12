#Stats I PS03

setwd("C:\Users\olivi\OneDrive\Documents\TCD ASDS\StatsI_Fall2021-main\problemSets")

options(scipen = 999) 
library(tidyverse)
#install.packages("broom")
library(broom)
?broom


#Question 1


incumbents <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/incumbents_subset.csv")

summary(incumbents)

head(incumbents)

glimpse(incumbents)



#important note on the order of variables: 

#when using the lm function it's lm(y~x)

#when using ggplot it's ggplot(x~y)





#Part 1. running a regression 

#reg1 <- lm(voteshare ~ difflog, data = incumbents)

#reg1 <- lm(data = incumbents, difflog ~ voteshare) 

#as you can see above I got a bit confused about the order
#I'm meant to put things in, but I've figured it out 

reg1 <- lm(data = incumbents, voteshare ~ difflog)


#Part 2. Scatterplot

#I originally did a base R plot, which I have kept as rough work below
#plot(incumbents$difflog, incumbents$voteshare)
#abline(reg1)


#Here is the scatterplot I made using the ggplot function:

ggplot(incumbents, aes(x=difflog, y=voteshare )) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = lm, formula = y~x) +
  ggtitle("Relationship Between Voteshare and Difflog") +
  labs(y="Voteshare") +
  labs(x="Difflog")




#Part 3. Residuals

#resids1 <- reg1$residuals - ended up doing it a different way to this,
#but I'm leaving the first option I was using here for future reference

resids1 <- resid(reg1)


print(resids1)

#Part 4. Write the prediction equation (also = write the regression line)

#this is on last week's slide 7

print(reg1)

#prediction equation is y = mx+c

#prediction equation is therefore:

predict_1 <- 0.04167*difflog + 0.57903



#Question 2

#Part 1. Run a regression.

#reg2 <- lm(data = incumbents, difflog ~ presvote) wrong order

reg2 <- lm(data = incumbents, presvote ~ difflog)


#Part 2. Make a scatterplot and add the regression line.

ggplot(incumbents, aes(x=difflog, y=presvote)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", formula = y~x) +
  ggtitle("Relationship Between Presvote and Difflog") +
  labs(y="Presvote") +
  labs(x="Difflog")
  



#Part 3. Saving the residuals 


resids2 <- resid(reg2)


print(resids2)


#Part 4. Writing the Prediction Equation

#y = mx+c

print(reg2)

predict_2 <- 0.02384*difflog + 0.50758


#Question 3. 

#Part 1. Running a regression

#reg3 <- lm(data = incumbents, presvote ~ voteshare) - wrong order

reg3 <- lm(data = incumbents, voteshare ~ presvote)




#Part 2. Making a scatterplot and adding the regression line

ggplot(incumbents, aes(x=presvote, y=voteshare)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", formula = y~x) +
  ggtitle("Relationship Between Presvote and Voteshare") +
  labs(y="Voteshare") +
  labs(x="Presvote")

#Part 3. Writing the Prediction Equation

#y= mx + c

print(reg3)

predict_3 <- 0.3880*presvote + 0.4413






#Question 4.

#Part 1. Running a regression

#reg4 <- lm(data = incumbents, resids2 ~ resids1) wrong order

reg4 <- lm(data = incumbents, resids1 ~ resids2)


#Part 2. Making a scatterplot and adding the regression line

ggplot(incumbents, aes(x=resids2, y=resids1)) +
  geom_point(alpha = 0.5) + #add a scatterplot
  geom_smooth(method = "lm", formula = y~x) +
  ggtitle("Relationship Between Resids1 and Resids2") +
  labs(y="Resids1") +
  labs(x="Resids2")


#Part 3. Writing the Prediction Equation

#y = mx+c

print(reg4)

predict_4 <- 0.2569e-01*resids2 + -4.860e-18




#Question 5. 

#Part 1. Running a regression

#reg5 <- lm(difflog + presvote ~ voteshare, data = incumbents)



reg5 <- lm(voteshare ~ difflog + presvote, data = incumbents)




#Part 2. Write the prediction equation

#y  = mx1 + mx2 + c

print(reg5)

predict_5 <- 0.03554*difflog + 0.25688*presvote + 0.44864

#Part 3. What in this output is identical to the output
#of Question 4 and why?



summary(reg4)

summary(reg5)

#NB look up the meaning of residuals 


