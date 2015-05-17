########################################################################################################################################################################
###    Author: Shatrunjai Singh
###
###    Title: Market mix modelling for Altman Vilandrie data challenge
###
###    Date: 05/17/2015
###
###    Dataset: Provided by Altman Vilandrie & Co 
###
###    Purpose: Given a set of observations of realized sales, dollar amounts spent on advertising initiatives, advertising
###    impressions realized, and realized occurrences of non-advertising events, provide insight on any/all of the
###    topics described in the Problem Statement.
###
########################################################################################################################################################################

#reading the csv file into a dataframe called observation

observation <- read.csv("C:/Users/Jai/Desktop/altman vilandrie/test_case/observations1.csv", header=TRUE)

#################################################################################################################################################################################

#Decomposing the sales into a trend, a sessonal and an irregular componenet; We observe a yearly sesonal trend it should not effect our final modelling parameters
install.pacakages("TTR")
library("TTR") #Needed for time series
ts_sales<- ts(observation$Sales, frequency=52,start=c(2013,1)) #Assuming the data is for the last 2 years; Doesnt change the outcome of the analysis 
plot.ts((ts_sales)) #Plot the complete signal
ts_sales_components <- decompose(ts_sales) #Sort of a fourier transformation to decompose signal
plot(ts_sales_components, xlab="time (2 years)",cex=5, frame.plot=TRUE,col.axis="grey",col=c("blue"),axes=FALSE)
seasonally_adjusted_sales <- ts_sales - ts_sales_components$seasonal
plot(seasonally_adjusted_sales) #the signal minus the seasonal componenet

#################################################################################################################################################################################

#Adstock transformation, using adstock variable of 0.15 to account for memory of adversitising since week 1

temp=observation$Sales
for(i in names(observation)){observation[[i]]= as.numeric(filter(x=observation[[i]], filter=.15, method="recursive"))}
observation$Sales=temp

#################################################################################################################################################################################

#Using a multiple regression model to fit the data

model_1=lm(Sales ~ ., data =observation)
model_summary = summary(model_1)
(model_summary$sigma)^2
model_summary$r.squared
model_summary$adj.r.squared

#################################################################################################################################################################################

#Checking for multicolinearity within the independent variables using VIF 

install.packages("car") #recquired for VIF 
library(car)
vif(model_1) 
#we do not see any high (5-10) VIF scores hence we can assume no multicolliniarity exsists between the independent variables

#################################################################################################################################################################################

# calculating the relative importance of different marketing variables and structural components

install.packages("relaimpo")# recquired for calculating relative importance of variables
library(relaimpo)
relative_importance=calc.relimp(model_1, type = c("car"), rela = TRUE) #used 'car' as cost function; rela=TRUE give % that a variable contributed to the adjusted R2
relative_importance
relative_imp=c(0.0172468747,0.0127277338,0.0027447346,0.0542065696,0.0003511832,0.0001619430,0.0001385625,0.0161778978,0.1011683176,0.0011380029,0.0241339708,0.0214259680,0.0017101238,0.0782794183,0.0008752494,0.0248821528,0.0036071786,0.0236321925,0.0150676097,0.0199930583,0.1840546780,0.1022173601,0.0007780532,0.0053315806,0.1408647450,0.0408012486,0.0597265808,0.0036815246,0.0032237803,0.0396517067)
sales_sum=sum(observation$Sales)
relative_contribution_of_each_variable=relative_imp*sales_sum
relative_contribution_of_each_marketing=relative_contribution_of_each_variable[1:20]

#################################################################################################################################################################################

#calculating the Cost/attribute sale and Return on investment (ROI) for each of the variables

marketing_spending <- read.csv("C:/Users/Jai/Desktop/altman vilandrie/test_case/spend_values.csv", header=FALSE)
marketing_spending=t(marketing_spending) #need to transpose the matrix as the data was in rows rather than colloums
marketing_spending[,2]
cost_per_attributed_sale=relative_contribution_of_each_marketing/marketing_spending[,2]
return_on_investment=((relative_contribution_of_each_marketing-marketing_spending[,1])/marketing_spending[,1])

#################################################################################################################################################################################

#optimizing future investment into marketing strategy using linear programming

total_spending=sum(marketing_spending[,1])

instal.packages("linprog")
library(linprog)
ROI <- c(0.34258909,1.45773854,-0.81879653,5.65342571,-0.97887708,-0.97593561,
         -0.98242824,2.43747887,18.11038487,-0.92899161,7.73776766,
         0.44266352,-0.87908270,6.78992897,-0.54373808,1.91995570,-0.25538511,
         3.85398561,-0.05792156,2.75902818)
betaVect <- c(101927176)
AMatrixsolver <- rbind(
  c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) # TOTAL
)
solveLP(ROI, betaVect, AMatrixsolver, maximum=TRUE) #Gives us the optimized investment portfolio for different marketing strategies

# we need more constraints (like minimum amount must be spent on certain media strategies)
# or else the model suggests spending all the money on Media 9!!

#################################################################################################################################################################################

#Variable selection
#Trying a best subset selection to see if the fitting can be improved (AIC criterion used)

install.packages("leaps") #needed for best subset selection function
library(leaps)
subset_result = regsubsets(Sales ~ ., data = observation , nbest = 2, nvmax = 31)
summary(subset_result)
plot(subset_result, scale = "bic") #plotting the results of the variable selection

#Trying a forward selection on the model to see if I can incearese adjusted_R2
nullmodel = lm(Sales ~ 1, data = observation)
fullmodel = lm(Sales ~ ., data = observation)
model.step = step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), 
                  direction = "forward")
model_2=lm(Sales ~ Media.9.Impressions + Media.14.Impressions + Structural.5.Occurrence + 
             Structural.2.Occurrence + Structural.1.Occurrence + Structural.7.Occurrence + 
             Media.4.Impressions + Structural.10.Occurrence + Media.1.Impressions + 
             Media.11.Impressions + Media.12.Impressions + Media.19.Impressions + 
             Structural.6.Occurrence + Media.15.Impressions + Media.20.Impressions + 
             Media.18.Impressions + Media.8.Impressions + Media.3.Impressions + 
             Structural.3.Occurrence + Media.6.Impressions + Media.16.Impressions + 
             Media.2.Impressions + Media.13.Impressions, data=observation)

#Model after forward variable selection
model_summary2 = summary(model_2)
(model_summary2$sigma)^2
model_summary2$r.squared
model_summary2$adj.r.squared

#################################################################################################################################################################################
