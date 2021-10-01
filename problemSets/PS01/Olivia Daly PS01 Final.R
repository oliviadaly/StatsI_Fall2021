#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# 
lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory


#setwd("C:/Users/olivi/Downloads/StatsI_Fall2021-main(1).zip/StatsI_Fall2021-main/problemSets/PS01")

#setwd("C:\Users\olivi\Downloads\StatsI_Fall2021-main(1).zip\StatsI_Fall2021-main\problemSets\PS01")

setwd ("c:/Users/olivi/Documents/StatsI_Fall2021/problemSets/PS01")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

t.test(y, conf.level = 0.9)



# Part 1 answer: the 90% confidence interval is between 93.96 and 102.92

#part 2 answer: 

#the null hypothesis is that they are the same or lower- the alternative hypothesis is that they are higher than the average IQ 

#100 is the average IQ score among all the schools in the country 

#the mean of x is 98.44

#t= 37.593 , test statistic

#df = 24, p-value < 2.2e-16

#p value is really low, so we would be very surprised 

sd(y)

#answer is 13.09287

test <- rnorm(n = 25, mean = 98.44, sd = 13.09287)

?Normal

#the school counselor wants to see if school mean is higher than population but only
#has a sample of 25, so needs to somehow get an estimate of the school's entire 
#population to compare to the national mean of 100

#test statistic is the measure of surprise 

t.test(y, mu = 100, alternative = "greater")

#We made the test one-sided by inputting "greater" as the 
#the p value we get from this input is 0.7215 which 
# is really large, meaning we cannot reject the null hypothesis 
# that the mean for the school is less than or equal to 100,
# i.e. her students are not better than the national average as she'd hoped 



#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)

str(expenditure)
hist(expenditure$Y)

str(expenditure)
hist(expenditure$Y)
hist(expenditure$Y, probability = TRUE)
hist(expenditure$X1, probability = TRUE)
hist(expenditure$X2, probability = TRUE)
hist(expenditure$X3, probability = TRUE)

lines(density(expenditure$Y, expenditure$X1, expenditure$X2, expenditure$X3))
plot(density(expenditure$Y,expenditure$X1, expenditure$X2, expenditure$X3), main = "Histogram of states in US")

lines(density(expenditure$Y, expenditure$X1, expenditure$X2, expenditure$X3))
plot(density(expenditure$Y,expenditure$X1, expenditure$X2, expenditure$X3), main = "per capita expenditure on shelters/housing assistance in state")


plot(unlist(biaspl), type = "l", main = "Evolution of Legal Threshold and Effort Levels", xlim = c(1,length(period)), ylim = (0:1),xlab = expression("Time Period (t)"),ylab = expression("Threshold/Effort Level"))
axis(side=2,seq(0,1,0.1))
axis(side=1,seq(0,5,1))
abline(h = xstar, col=("yellow"), lwd=2, lty=1)
mapply(lines, biaspl, col="blue")
legend("bottomright", c("x1","x2","a","b"), fill = c("blue","red","green","green"))

plot(Y, ylim=range(expenditure$Y, expenditure$X1, expenditure$X2, expenditure$X3), col='black')
lines(expenditure$Y, col='red')
lines(expenditure$X1, col='green')
lines(expenditure$X2, col= 'blue')
lines(expenditure$X3, col= 'purple')


plot(expenditure, ylim=range(expenditure$Y, expenditure$X1, expenditure$X2, expenditure$X3), col='black')
lines(expenditure$Y, col='red')
lines(expenditure$X1, col='green')
lines(expenditure$X2, col='pink')
lines(expenditure$X3, col='blue')

# define 3 data sets
xdata <- c(1,2,3,4,5,6,7)
y1 <- c(1,4,9,16,25,36,49)
y2 <- c(1, 5, 12, 21, 34, 51, 72)
y3 <- c(1, 6, 14, 28, 47, 73, 106 )

# plot the first curve by calling plot() function
# First curve is plotted
plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,110), ylab="y" )

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(xdata, y2, col="red", pch="*")
lines(xdata, y2, col="red",lty=2)

# Add Third curve to the same plot by calling points() and lines()
# Use symbol '+' for points.
points(xdata, y3, col="dark red",pch="+")
lines(xdata, y3, col="dark red", lty=3)

# Adding a legend inside box at the location (2,40) in graph coordinates.
# Note that the order of plots are maintained in the vectors of attributes.
legend(1,100,legend=c("y1","y2","y3"), col=c("blue","red","black"),
       pch=c("o","*","+"),lty=c(1,2,3), ncol=1)


#from this website:http://www.countbio.com/web_pages/left_object/R_for_biology/R_fundamentals/multiple_curves_R.html?fbclid=IwAR1qY6nP1DFP8qWFV1LuHB_ceLOpsViHEBvNQVxbt2CbhBwa1C-QbW-KL24
# define 3 data sets
xdata <- c(1,2,3,4,5,6,7)
y1 <- c(1,4,9,16,25,36,49)
y2 <- c(1, 5, 12, 21, 34, 51, 72)
y3 <- c(1, 6, 14, 28, 47, 73, 106 )

# plot the first curve by calling plot() function
# First curve is plotted
plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,110), ylab="y" )

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(xdata, y2, col="red", pch="*")
lines(xdata, y2, col="red",lty=2)

# Add Third curve to the same plot by calling points() and lines()
# Use symbol '+' for points.
points(xdata, y3, col="dark red",pch="+")
lines(xdata, y3, col="dark red", lty=3)

# Adding a legend inside box at the location (2,40) in graph coordinates.
# Note that the order of plots are maintained in the vectors of attributes.
legend(1,100,legend=c("y1","y2","y3"), col=c("blue","red","black"),
       pch=c("o","*","+"),lty=c(1,2,3), ncol=1)

#experimenting with our own expenditure data: 

lines(expenditure$STATE(expenditure$Y))
plot(expenditure$STATE(expenditure$Y), col = 'blue', main = "Graph of states in US")

plot(expenditure$STATE, expenditure$Y, type="o", col="blue", pch="o", lty=1, ylim=c(0,110), ylab="y" )


points(expenditure$STATE, expenditure$X1, col="red", pch="*")
lines(expenditure$STATE, expenditure$X1, col="red",lty=2)


# after trying all of the above I finally got some help from other students because I was really struggling:


#part 1 
str (expenditure)
lines(expenditure$Y)
lines(expenditure$X1)
lines(expenditure$X2)
lines(expenditure$X3)
plot(expenditure, ylim=range(expenditure$Y, expenditure$X1,expenditure$X2,expenditure$X3), col='red', main = "Expenditure of states in US", lower.panel = NULL)


#part 2
install.packages("ggplot2")
library(ggplot2)
data=as.data.frame(expenditure[,c(2,6)])
data$Region = as.factor(data$Region)
mode(data$Region)

#Here I created a box plot graph comparing expenditure and region.
#I wouldn't have thought to do this of my own accord and learned from other students
ggplot(aes(y = Y, x = Region, fill=Region), data = data)+ geom_boxplot()+ggtitle("Box plots of Expenditure by Region")

#According to the students that I learned from, R will read Regions as integers unless you make it into factors because they're labelled 1,2 etc but the numbers are basically just names not actually things you can add
#and then this just creates a data frame out of the two variables you're looking at which is in this case Y(2) and Region(6) based on their position in the list
#mode(data$Region)and then this is to check that you successfully changed the region to factors, if it worked it should give the mode as numeric they said




#part 3

#creating a scatter plot below:

scatter.smooth( expenditure$X1, expenditure$Y, xlab = 'Personal Income', ylab = 'expenditure', main = 'Income and expenditure', col = c("blue", "red", "orange", "black"), pch = c(0,1,2,3))
