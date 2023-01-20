# Vihari Reddy Tummuru
# This code imports a CSV file containing data on mobile phone subscribers.
# We added  a new smotefamily package in addition to tidyvverse,corrplot,olsrr
# which is used to deal with class imbalance when doing logistic regression.
# We do logistic regression on the cancelled service variable and plot
# various histograms and correlation plot to understand the distribution and
# relation among the variables.

# Install and load tidyverse package
install.packages("tidyverse")
library(tidyverse)

# Install and load corrplot package
install.packages("corrplot")
library(corrplot)

# Install and load olsrr package
install.packages("olsrr")
library(olsrr)

# Install and load smotefamily package
install.packages("smotefamily")
library(smotefamily)

# Set working directory to Lab05 folder
setwd("C:/Users/ual-laptop/Desktop/Lab06")

# Read CSV file into a tibble and define column types
mobilePhone <- read_csv(file = "MobilePhoneSubscribers.csv",
                       col_types = "lillnininn",
                       col_names = TRUE)

# Display the mobilePhone tibble
print(mobilePhone)

# Display the structure of the tibble
print(str(mobilePhone))

# Display the summary of the tibble
print(summary(mobilePhone))

# Creating a function to display histogram
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value,fill=key),
                              color = "black")+
    facet_wrap( ~ key,scales= "free")+
    theme_minimal()
}

# Display the histogram of the tibble
displayAllHistograms(mobilePhone)

# Display a correlation matrix of mobilePhone that is rounded to two decimals
round(cor(mobilePhone),digits = 2)

# Display of a correlation plot of mobilePhone with certain features
corrplot(cor(mobilePhone),
         method = "number",
         type = "lower")

# Remove data plan and data usage from the mobilePhone data
mobilePhone<- subset(mobilePhone, select = -c(DataPlan,DataUsage))

# Splitting mobilephone data into training and test data sets
set.seed(203)
sampleSet <- sample(nrow(mobilePhone),
                    round(nrow(mobilePhone)*.75),
                    replace = FALSE)

mobilePhoneTraining <- mobilePhone[sampleSet, ]
mobilePhoneTest <- mobilePhone[-sampleSet, ]

# Checking for class imbalance
summary(mobilePhoneTraining$CancelledService)

# Dealing with class imbalance in training set using SMOTE function 
mobilePhoneTrainingSmoted <- tibble(SMOTE(
  X=data.frame(mobilePhoneTraining),
  target = mobilePhoneTraining$CancelledService,
  dup_size = 3)$data)

# Display the tibble after dealing with class imbalance 
summary(mobilePhoneTrainingSmoted)

# Convert cancelled service and recent renewal in logical type
mobilePhoneTrainingSmoted <- mobilePhoneTrainingSmoted %>%
  mutate(CancelledService = as.logical(CancelledService),
         RecentRenewal = as.logical(RecentRenewal))

# Display Display the tibble after converting cancelled service 
# and recent renewal in logical type 
summary(mobilePhoneTrainingSmoted)

# Get rid of "class" column in tibble (added by SMOTE())
mobilePhoneTrainingSmoted <- mobilePhoneTrainingSmoted %>%
  select(-class)

# Check for class imbalance in the training set
summary(mobilePhoneTrainingSmoted)

# Generate logistic regression Model 
mobilePhoneModel<- glm(data=mobilePhoneTrainingSmoted, family=binomial, 
                       formula=CancelledService ~ .)

# Display the logistic model summary
summary(mobilePhoneModel)

# Odds ratios of the independent variables
exp(coef(mobilePhoneModel)["AccountWeeks"])
exp(coef(mobilePhoneModel)["RecentRenewalTRUE"])
exp(coef(mobilePhoneModel)["CustServCalls"])
exp(coef(mobilePhoneModel)["AvgCallMinsPerMonth"])
exp(coef(mobilePhoneModel)["AvgCallsPerMonth"])
exp(coef(mobilePhoneModel)["MonthlyBill"])
exp(coef(mobilePhoneModel)["OverageFee"])

# Use the model to predict outcomes in the testing dataset
mobilePhonePrediction <- predict(mobilePhoneModel,
                                 mobilePhoneTest,
                                 type='response')

# Display the test model
print(mobilePhonePrediction)

# Converting less than 0.5 as 0 and greater than 0.5 as 1
mobilePhonePrediction <- 
  ifelse(mobilePhonePrediction >= 0.5,1,0)

# Creating a mobile phone confusion matrix
mobilePhoneConfusionMatrix <- table(mobilePhoneTest$CancelledService,
                                    mobilePhonePrediction)

# Display confusion matrix
print(mobilePhoneConfusionMatrix)

# Calculating false positive
mobilePhoneConfusionMatrix[1,2]/
  (mobilePhoneConfusionMatrix[1,2]+mobilePhoneConfusionMatrix[1,1])

# Calculating false negative
mobilePhoneConfusionMatrix[2,1]/
  (mobilePhoneConfusionMatrix[2,1]+mobilePhoneConfusionMatrix[2,2])

# Calculating Model Prediction Accuracy
sum(diag(mobilePhoneConfusionMatrix))/ nrow(mobilePhoneTest)
