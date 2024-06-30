"
****************************************************************
Name: Sparsh Shrestha
Case Study 1
****************************************************************
"

#Reading Personalized data into R
sspd <- read.csv("Shrestha, Sparsh Personalized Data.csv", header = TRUE)

##############################
#Categorical Variable Analysis
##############################

#########Creating frequency for News#########
(news <- table(sspd$News))
(relative.news <- prop.table(news))
#The frequency table shows that I do not spend my time watching news very often
#Out of 33 days I watched news only 2 days(6%) and did not watch news other 31 days(94%).

#Interpreting the data using pie chart
pie(news, main = "Frequency of the days I watched news", col = c('red', 'blue'))
#As the pie chart shows I do not spend my time on watching news.


#########Creating frequency for Fruit#########
(fruit <- table(sspd$Fruit))
(relative.fruit <- prop.table(fruit))
#The frequency table shows that I tried 5 types of fruits in my 33 days recorded data
#While I ate 4 fruits banana, grape, lemon and orange 3 times each, I seem to prefer apple more as I ate it 63% of all the days.

#Interpreting the data using bar plot
barplot(fruit, main = "Frequency of the Fruits I ate", xlab = 'Fruits', ylab = 'Number of Fruits', 
        col = c('red', 'yellow', 'green', 'gold', 'orange'))
#The bar plot shows the I consume apple more than any other fruits.


#########Creating frequency for Dinner_taste(1-3)#########
(dinner <- table(sspd$Dinner_taste.1.3.))
(relative.dinner <- prop.table(dinner))
#The dinner taste frequency is 24.24% for '1', 39.39% for '2' and 36.36% for '3'.

#Interpreting the data using pie chart
barplot(dinner, main = "Dinner Satisfaction", xlab = 'Dinner Satisfaction Rating', ylab = 'Frequency',
        names.arg = c('OK', 'Satisfied', 'Very Satisfied'), col = c('black', 'white', 'gold'))
#The bar plot shows that three variables seems very close.

#########Contingency table for fruit and dinner taste#########
#Importing rpivotTable
library(rpivotTable)
#Creating pivot table
rpivotTable(sspd)
#Creating contingency table for Dinner_taste and Fruit variables
relative.table <- table(sspd$Dinner_taste.1.3., sspd$Fruit)
prop.table(relative.table)
#From the contingency table it can be interpret that dinner tastes and fruits variables do not influence each other.


###############################
#Quantitative Variable Analysis
###############################

#########Zoom Variable#########

#Histogram for zoom variable
hist(sspd$Zoom, main = "Number of hours spent on Zoom Classes", xlab = "Number of Hours", 
     col = "Light Blue", breaks = 10)
#Mode of the distribution is 0
#The data is unimodal and right skew
#There seems to be no outliers when you analyse the histogram

#Calculating the mean, median, standard deviation and IQR for zoom variable
(mean.zoom <- mean(sspd$Zoom))
#Mean = 2.090909
(median.zoom <- median(sspd$Zoom))
#Median = 2
(sd.zoom <- sd(sspd$Zoom))
#Standard deviation = 2.155859
(iqr.zoom <- IQR(sspd$Zoom))
#Inter quartile range = 3

#Since the data is asymmetric creating boxplot to identify outliers
bp.zoom <- boxplot(sspd$Zoom, col = "Light Blue", outcol = "red", main = "Zoom class hours", ylab = "Number of hours")
bp.zoom$out
#There are no outliers in this data because there are no extreme values

#Correlations between zoom and other quantitative variables
cor(sspd$Zoom, sspd$Study)
#Correlation between zoom and study is 0.3933782
cor(sspd$Zoom, sspd$Sleep)
#Correlation between zoom and sleep is 0.1864882
cor(sspd$Zoom, sspd$House)
#Correlation between zoom and house is -0.02483867
cor(sspd$Zoom, sspd$E.mail)
#Correlation between zoom and E.mail is 0.285793


#########Study Variable#########
#Histogram for Study variable
hist(sspd$Study, main = "Number of hours spent Studying", xlab = "Number of Hours", 
     col = "Green")
#Mode of the distribution are 4 and 6
#The data is bimodal and symmetric
#There seems to be no outliers when you analyse the histogram

#Calculating the mean, median, standard deviation and IQR for study variable
(mean.study <- mean(sspd$Study))
#Mean = 5.212121
(median.study <- median(sspd$Study))
#Median = 5
(sd.study <- sd(sspd$Study))
#Standard deviation = 1.634732
(iqr.study <- IQR(sspd$Study))
#Inter quartile range = 2

#Since the mean and median are almost equal data can be considered 
#So calculating the outliers using standardized values
sspd$standard.study<-(sspd$Study - mean.study)/sd.study
(outliers.study<-subset(sspd, standard.study > 3 | standard.study < -3))
#Also creating box plot for the data
bp.study <- boxplot(sspd$Study, col = "Green", outcol = "red", main = "Study hours", ylab = "Number of hours")
#There are no outliers in this data because there are no extreme values

#Correlations between study and other quantitative variables
cor(sspd$Study, sspd$Zoom)
#Correlation between study and zoom is 0.3933782
cor(sspd$Study, sspd$Sleep)
#Correlation between study and sleep is 0.4754795
cor(sspd$Study, sspd$House)
#Correlation between study and house is -0.3867128
cor(sspd$Study, sspd$E.mail)
#Correlation between study and E.mail is -0.06767811


#########Sleep Variable#########
#Histogram for Sleep variable
hist(sspd$Sleep, main = "Number of hours spent Sleeping", xlab = "Number of Hours", 
     col = "Gray")
#Mode of the distribution is 8
#The data is unimodal and left skew
#There seems to be no outliers when you analyse the histogram

#Calculating the mean, median, standard deviation and IQR for sleep variable
(mean.sleep <- mean(sspd$Sleep))
#Mean = 7.666667
(median.sleep <- median(sspd$Sleep))
#Median = 8
(sd.sleep <- sd(sspd$Sleep))
#Standard deviation = 0.7772816
(iqr.sleep <- IQR(sspd$Sleep))
#Inter quartile range = 1

#Since the data is asymmetric creating boxplot to identify outliers
bp.sleep <- boxplot(sspd$Sleep, col = "Gray", outcol = "red", main = "sleep hours", ylab = "Number of hours")
bp.sleep$out
#There are no outliers in this data because there are no extreme values

#Correlations between sleep and other quantitative variables
cor(sspd$Sleep, sspd$Zoom)
#Correlation between sleep and zoom is 0.1864882
cor(sspd$Sleep, sspd$Study)
#Correlation between sleep and study is 0.4754795
cor(sspd$Sleep, sspd$House)
#Correlation between sleep and house is -0.2736553
cor(sspd$Sleep, sspd$E.mail)
#Correlation between sleep and E.mail is 0.1894233


#########House Variable#########
#Histogram for House variable
hist(sspd$House, main = "Number of times I went out of the House", xlab = "Number of outings", 
     col = "Orange")
#Mode of the distribution is 1
#The data is unimodal and right skew
#There seems to be no outliers when you analyse the histogram

#Calculating the mean, median, standard deviation and IQR for house variable
(mean.house <- mean(sspd$House))
#Mean = 0.6969697
(median.house <- median(sspd$House))
#Median = 1
(sd.house <- sd(sspd$House))
#Standard deviation = 0.6366341
(iqr.house <- IQR(sspd$House))
#Inter quartile range = 1

#Since the data is asymmetric creating boxplot to identify outliers
bp.house <- boxplot(sspd$House, col = "Orange", outcol = "red", main = "Number of times I went out of the house", ylab = "Number of Outings")
bp.sleep$out
#There are no outliers in this data because there are no extreme values

#Correlations between house and other quantitative variables
cor(sspd$House, sspd$Zoom)
#Correlation between house and zoom is -0.02483867
cor(sspd$House, sspd$Study)
#Correlation between house and study is -0.3867128
cor(sspd$House, sspd$Sleep)
#Correlation between house and sleep is -0.2736553
cor(sspd$House, sspd$E.mail)
#Correlation between house and E.mail is -0.03515062


#########E.mail Variable#########
#Histogram for E.mail variable
hist(sspd$E.mail, main = "Number of e-mails I recieved everyday", xlab = "Number of emails", 
     col = "pink")
#Mode of the distribution is 10
#The data is unimodal and right skew
#There seems to be no outliers when you analyse the histogram

#Calculating the mean, median, standard deviation and IQR for E.mail variable
(mean.email <- mean(sspd$E.mail))
#Mean = 9.575758
(median.email <- median(sspd$E.mail))
#Median = 9
(sd.email <- sd(sspd$E.mail))
#Standard deviation = 4.527902
(iqr.email <- IQR(sspd$E.mail))
#Inter quartile range = 7

#Since the data is asymmetric creating boxplot to identify outliers
bp.house <- boxplot(sspd$House, col = "pink", outcol = "red", main = "Number of times e-mails I recieved everyday", ylab = "Number of E-mails")
bp.sleep$out
#There are no outliers in this data because there are no extreme values

#Correlations between E.mail and other quantitative variables
cor(sspd$E.mail, sspd$Zoom)
#Correlation between E.mail and zoom is 0.285793
cor(sspd$E.mail, sspd$Study)
#Correlation between E.mail and study is -0.06767811
cor(sspd$E.mail, sspd$Sleep)
#Correlation between E.mail and sleep is 0.1894233
cor(sspd$E.mail, sspd$House)
#Correlation between E.mail and house is -0.03515062


##############################
#Highest Correlation Variables
##############################

#Correlation among all the quantitative variables
sub <- subset(sspd, select = c(Zoom, Study, Sleep, House, E.mail))
cor(sub)
#Highest correlation is between Sleep and Study variables with coefficient equals 0.4754795

#The direction of the relationship is positive 
#The strength of the relationship is moderate

#Checking the condition required for regression
#Quantitative variable condition is met - Both variables are quantitative
regression <- lm(Study~Sleep, sspd)
plot(regression)
#Linearity condition is not met - Since the line is not completely horizontal in Residual vs Fitted plot
#Outlier condition is not met - Since points do not follow the line in Normal Q-Q plot
#Equal Spread condition is not met - In scale-location plot does not have a horizontal and points are not evenly dispersed

#########################################################################
#Remember to Enter 4 times in the console before executing the next line#
#########################################################################


#Determining Equation of the regression line
regression$coefficients
#Slope = 1
#Y-Intercept = -2.454545
#Equation: y = x - 2.454545

#Creating Scatter Plot and regression line
plot(sspd$Sleep, sspd$Study, main = "Relationship between Study and Sleep", xlab = "Sleep (Hours)", 
     ylab = "Study (Hours)", col = "Blue", pch = 20)
abline(regression, col = "red")
text(8, 9, cex = 1, "Study = Sleep - 2.454545", col = "Green")

#Predicting dependent variable Study when independent variable is Sleep = 12 hours
new.sleep <- data.frame(Sleep = 12)
predict.lm(regression, new.sleep)
#If i sleep 12 hours in a day then I will study for 9.54 hours that day.

