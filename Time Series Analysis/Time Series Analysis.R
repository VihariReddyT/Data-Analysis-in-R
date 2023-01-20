#install the packages
install.packages("ggplot2")
install.packages("plotly")
install.packages("corrplot")
install.packages("forecast")
install.packages("tseries")

#Import the dataset
setwd('/Users/Vihari/Desktop/R-LAB-2022')
mydata<-read.csv('NBAPlayerStats.csv',header = TRUE)

#Read the data
head(mydata) # Read the first 4 rows of your dataset
dim(mydata) # Check the dimension of your dataset
summary(mydata) # Check the summary statistics of your dataset
colnames(mydata) # Check for the column names
#attach(mydata) # By attaching the dataset, you can use the variable name directly without specify the dataset

#Visualization
hist(mydata$TWITTER_COUNT_MIL) # histogram
boxplot(mydata$TWITTER_COUNT_MIL~mydata$AGE, ylab = 'Twitter_count',main = 'box plot twitter count for different age') # the box plot
plot(mydata$TWITTER_COUNT_MIL ~ mydata$SALARY_MIL, col = 2) # check scatter plot


# Regression Models
simple_model<-lm(mydata$TWITTER_COUNT_MIL~mydata$SALARY_MIL) # regress your dependent variable on independent variable
abline (simple_model)  # best fit line (regression line)
summary(simple_model) # see the regression results

plot (simple_model)



#library (ggplot2)
#install.packages("plotly")
#library (plotly )
#simple_plot<- ggplot (mydata, aes (SALARY_MIL, TWITTER_COUNT_MIL, colour = PLAYER_NAME ) ) + geom_point ()
#ggplotly (simple_plot )


#Build a correlation heat map 
#install.packages("corrplot")
library (corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")
Corr_Matrix <- mydata [ , c ( 2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17 ) ]
rquery.cormat ( Corr_Matrix, type = "full" )

# Adding more explanatory variables
multiple_model <- lm ( mydata$TWITTER_COUNT_MIL ~ mydata$SALARY_MIL + mydata$OFF_RATING )
summary(multiple_model)

