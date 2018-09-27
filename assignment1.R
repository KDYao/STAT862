###########################################
#                                         #
#             Assignment 1                #
#           Author: Kundi Yao             #
#           Student ID: 20137469          #
#                                         #
###########################################


############# Question 1 ##################

# (a) Look up the help for state.x77
?state.x77

# (b) Compute the dimension size of state.x77 and display its dimension names
stateData <- as.data.frame(state.x77)
dim(stateData)
dimnames(stateData)

# (c) Write an expression that returns the summary statistics, as given bu summary(), for each of the columns in this matrix
summary(stateData)

# (d) Create a vector named popdense that contains the population density in personsper square mile. Note that the population in state.x77 is given in thousands.
popdense <- as.vector(stateData$Population/stateData$Area * 1000)

# (e) Apply the function pairs() to the matrix to create scatter plots of all columns against one another.
pairs(as.matrix(stateData), pch=20)
#dev.off()

# (g) Select the states that have <1% illiteracy, but a murder rate of>10 per 100,000.
subset(stateData, stateData$Illiteracy<1 & stateData$Murder>10)
print(rownames(subset(stateData, stateData$Illiteracy<1 & stateData$Murder>10)))

# (h) Select the states that have greater than average high school graduate rates, butless than average annual income.
avgGradRates <- mean(stateData$`HS Grad`)
avgAnnualIncome <- mean(stateData$Income)
subset(stateData, stateData$`HS Grad`>avgGradRates & stateData$Income<avgAnnualIncome)

# (i) Create a vector named "Murder" that contains the murder rates.
Murder <- as.vector(stateData$Murder)

# (j) Create a vector named "Illiteracy" that contains the illiteracy rates.
Illiteracy <- as.vector(stateData$Illiteracy)

# (k) Create a design matrix,X, that contains all 1's in the first column and the illiteracy rates in the second column. This will serve as our design matrix in part(m).
secondCol <- stateData$Illiteracy
firstCol <- rep.int(1, length(secondCol))
X <- matrix(firstCol)
X <- cbind(X, secondCol)
colnames(X) <- c("Default", "Illiteracy")


# (l) Assume that there is an approximate linear relationship between Illiteracy and Murder  rates.  Given the design matrix defined above, we can de ne a simple linear model asy=X +εwherey(Murder) is the dependant variable,Xis thedesign matrix of independent variables,is the vector of parameters andεis theerror term. The least squares estimate ofis:^= (XTX)1XTy, whereXTisthe transpose ofX. Write a function which has inputsXandy, and returns^inR.

getLeastSquresEstimate <- function(X, y){
  ### Input
    # X is the design matrix of independant variables
    # y(Murder) is the dependant variable 
  
  ### Output
    # beta
  
  XTrans = t(X)
  beta = (XTrans %*% X ) ^ (-1) %*% XTrans %*% y
  return(beta)
}



# (n) In a single plot, draw the following subplots 
  # (1) a scatter plot of Murder(y) versus Illiteracy(x); 
  # (2) a histogram of Murder; 
  # (3) a Quantile-Quantiale plot of Murder;
  # (4) a boxplot of Murder and Illiteracy.

par(mfrow=c(2,2))
attach(stateData)
plot(Illiteracy, Murder, main = "Scattor plot of Murder versus Illiteracy", pch= 19, xlab = "Illiteracy", ylab = "Murder")
hist(Murder)
qqnorm(Murder, main = "Normal Q-Q Plot of Murder")
qqline(Murder)
boxplot(Illiteracy, Murder, main = "Box plot of Illiteracy and Murder", names = c("Illiteracy", "Murder"))
detach(stateData)

############# Question 2 ##################

# (a)Create a vector called percentile that contains the values 0.75, 0.9, 0.95, 0.975,and 0.99 and 0.999.
percentile <- as.vector(c(0.75, 0.9, 0.95, 0.975, 0.99 , 0.999))

# (b)Create a vector named df that contains the integers 1 to 30 followed by 60 and Infinity. Do this without typing in the integers 1 through 30.
df <- as.vector(c(1:30, 60, Inf))

# (c)Create a matrix named tTable that returns the percentile from at-distributionwhere the rows represent the degrees of freedom as speci ed by the vectordfandthe columns are thequantilesas speci ed by percentile. That is, the cells aretsuch thatP(Tdf<=t) =percentilewhereTdfis at-distribution withdfdegreesof freedom.  Hint: you may need to perform a transpose to get the rows andcolumns as speci ed.
tTable <- matrix(data=NA, nrow = length(df), ncol = length(percentile))

for (i in 1:length(percentile)){
  colDist <- as.vector(qt(percentile[i], df))
  tTable[, i] <- colDist
}
#View(tTable)

# (d) Round the contents of tTable to four decimal places
tTable <- round(tTable, 4)

# (e) Assign the values in df and percentile as row and column names respectively sothat when the matrix is displayed the  rst and last couple of rows look like thefollowing:
colnames(tTable) <- percentile
rownames(tTable) <- df
View(tTable)

############# Question 3 ##################

install.packages("ISLR")
library(ISLR)

# Origin: 1 for American, 2 for European, 3 for Janpanese
# mpg --> dependent variable
# Other variables except "name" --> independent variables

# Data preprocess
depVar <- Auto$mpg
nonIndepNames <- c("mpg", "name")
indepVar <- Auto[!(names(Auto) %in% nonIndepNames)]

# (a) Create a pairwise scatter plot for dependent and independent variables. Show the plot and make comment on the plot.
formula <- as.formula(paste("mpg~", paste(names(indepVar), collapse = "+"))) 

# Color combo from internet
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")

pairs(formula, data = Auto, col = my_cols[Auto$origin], pch=19, main="Scatter plot")

# Add comment at a random place
text(runif(1, .3, .8), runif(1, .1, .8), "Scatter plot", col = 'red', cex=2.5)

# (b) Computer the correlation matrix between the variables usingcor() function.

data_for_cor <- Auto[!(names(Auto) %in% c("name"))]
correlation <- cor(data_for_cor)
View(correlation)

# (c) Fit the multiple linear regression mode. Show the table of the fitted model: coefficients estimation, their standard deviation, t-statistic, and p-values. Show R-squared and the estimation 
multiRegressionModel <- lm(formula = formula, data = Auto)

reg_fit = summary(multiRegressionModel)
print(reg_fit)

# (d) Obtain the prediction of mean response, its associated prediction error and 100(1)% con dence interval based on the  tted model for the new input cylinders=8; displacement= 300; horsepower= 150; weight= 3600; acceleration= 13; year=77; origin= 3
newInput <- data.frame(cylinders=8, displacement= 300, horsepower= 150, weight= 3600, acceleration= 13, year=77, origin= 3)
#predict(multiRegressionModel,newInput, type = "response")
predict.lm(multiRegressionModel,newInput, se.fit=TRUE, interval =  "prediction")

# (e) Is there a relationship between the independant variables and the response variable?
reg_fit$fstatistic
# Use (1-pf) to get the area other than the p-value
# 0 indicate the result is statistical significant, there exists a relationship between the independant variables and the response variable
1- pf(reg_fit$fstatistic[1],reg_fit$fstatistic[2],reg_fit$fstatistic[3])

# (f) Which predictors appear to have a statistically significant relationship to the response?
# Find metrics p-value < 0.05
subset(reg_fit$coefficients[-1,4], reg_fit$coefficients[-1,4] < 0.05)


# (g) Produce the residuals plots
par(mfrow=c(1,2))
qqnorm(reg_fit$residuals)
qqline(reg_fit$residuals)

install.packages("arm")
library(arm)
residual.plot(fitted(multiRegressionModel), resid(multiRegressionModel), sigma.hat(multiRegressionModel))

