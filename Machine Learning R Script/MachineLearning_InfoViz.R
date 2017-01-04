install.packages("bootstrap")
library(bootstrap)
library(MASS)
library(RColorBrewer)
install.packages("hexbin")
library(hexbin)
install.packages("ggplot2")
library(ggplot2)

StudentData <- read.csv("/Users/pratikkamath30/Documents/Big Data 2/main_data.csv", header = T, sep=",")
View(StudentData)
StudentDataDF <- as.data.frame(StudentData)

#Labelling the students vs gdp graph manually for a better understanding.
plot(x = StudentDataDF$gdp, y = StudentDataDF$students, xaxt = "n")
axis(1, at = seq(10, 6e+12, by = 1.5e+11), las=2)

#Plotting no of students vs expenditure
plot(x = StudentDataDF$expenditure, y = StudentDataDF$students)

#Calculate total expenditure by multiplying GDP and percentage of GDP invested on education and plotting it against no of students.
StudentDataDF$expenditure_total = StudentDataDF$expenditure* StudentDataDF$gdp
plot(x = StudentDataDF$expenditure_total, y = StudentDataDF$students)

#Now we are starting Regression analysis here.
fit <- lm(StudentDataDF$students ~ StudentDataDF$gdp + StudentDataDF$expenditure_total)
summary(fit)
# To get all 4 plots in one page I have used the layout command.
layout(matrix(c(1,2,3,4),2,2))
plot(fit)

#Since regression results seem good enough we can do a 10 fold cross validation and check whether R-squared is affected highly or not.
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
X <- as.matrix(StudentDataDF[c("gdp","expenditure_total")])
y <- as.matrix(StudentDataDF[c("students")]) 
results <- crossval(X,y,theta.fit,theta.predict,ngroup=10)
correlation <- cor(y, fit$fitted.values)**2 
correl <- cor(y,results$cv.fit)**2 

#With raw data we get R squared as 0.323 and with fitted data after 10 fold Cross fold validation it gives around 0.30 which shows these are good predictors for number of students and mosaic plot because it has an interesting categorical way of showing things.
correlation
correl

#Lets'do some more interesting info viz. I am using a hexbin plot because it differentiates colorwise for a better understanding.
rf <- colorRampPalette(rev(brewer.pal(40,'Set3')))
hexbinplot(StudentDataDF$students~StudentDataDF$gdp, colramp=rf)
hexbinplot(StudentDataDF$students~StudentDataDF$expenditure_total, colramp=rf)

#For the last important thing I want to plot number of students for particular years, particular GDP's and particular expenditures as % of GDP.We will have to take a head as there are too many values to plot.

StudentDataHead <- head(StudentDataDF, 1000)
year_no <- ggplot(data = StudentDataHead, aes(x = years, y = students)) +
  geom_point(aes(text = paste("Number of students:", students))) +
  stat_summary(fun.y=mean, colour="red", geom="line", size = 3)
year_no

exp_total_no <- ggplot(data = StudentDataHead, aes(x = expenditure_total, y = students)) +
  geom_point(aes(text = paste("Number of students:", students))) +
  stat_summary(fun.y=mean, colour="red", geom="line", size = 3)
exp_total_no

gdp_no <- ggplot(data = StudentDataHead, aes(x = students, y = gdp)) +
  geom_point(aes(text = paste("Number of students:", students))) +
  stat_summary(fun.y=mean, colour="red", geom="line", size = 3)
gdp_no
