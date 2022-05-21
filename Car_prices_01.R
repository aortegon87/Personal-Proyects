



# Description###################################################################

#Title: simple Linear Regression
# version : 1

#description: how to build a simple linear regression

#01 prepare the library

library(dplyr)
library(readxl)
library(ggcorrplot)


#02 PREPARE INFORMATION#########################################################

#import data base
main_data<-read.csv("CarPrice_Assignment.csv")

#descriptive stadistics
summary(main_data)

#type of variables
class_summary <- sapply(main_data,class)

#summary type of variables
table(class_summary)

#select numeric variables
Numeric_variables <- select_if(main_data, is.numeric)

#type of variables
numeric_summary <- sapply(Numeric_variables,class)

#summary numeric variables
table(numeric_summary)

#03 BUILDING CORRELATION MATRIX#################################################


#ploting the correlation matrix
corr <- round(cor(Numeric_variables), 2)
ggcorrplot(corr, type = "lower", lab=TRUE)

#04 define variables ###########################################################

#defining dependend and iindependend variables 

depentcol <- "price"
independentcol <- "enginesize"

colnames(Numeric_variables)[colnames(Numeric_variables)==depentcol]<- "dependent"
colnames(Numeric_variables)[colnames(Numeric_variables)==independentcol]<- "independent"


#subseting the main data base to extract the variables
Simple_linear_DB<-select(Numeric_variables,c("dependent","independent"))

#visualization  distribution of variables
summary(Simple_linear_DB)

hist(Simple_linear_DB$dependent, main = "Histogram", ylab = "Frequency", xlab = depentcol)

hist(Simple_linear_DB$independent, main = "Histogram", ylab = "Frequency" , xlab = independentcol)

plot(Simple_linear_DB$dependent, Simple_linear_DB$independent, main = "Plot",ylab = "Independent", xlab = "Dependent")


#05 BUILDING LINEAR REGRESSION #################################################

Linear_regression <- lm(dependent ~ independent, data = Simple_linear_DB )
Linear_regression
summary(Linear_regression)

plot(Linear_regression)









