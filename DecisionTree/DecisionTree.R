# Vihari Reddy Tummuru


# Install and load tidyverse package
# install.packages("tidyverse")
library(tidyverse)

# Install and load rpart.plot ,rpart package
# install.packages("rpart.plot")
library(rpart.plot)
library(rpart)

# Set working directory to Lab09 folder
setwd("C:/Users/ual-laptop/OneDrive - University of Arizona/Desktop/R-545/Lab09")

# Read CSV file into a tibble and define column types
riceFarms <- read_csv(file = "IndonesianRiceFarms.csv",
                       col_types = "fniiinf",
                       col_names = TRUE)

# Display the riceFarms tibble
print(riceFarms)

# Display the structure of the tibble
print(str(riceFarms))

# Display the summary of the tibble
print(summary(riceFarms))

# Randomly splitting the data into a smaple set
set.seed(370)
sampleSet <- sample(nrow(riceFarms),
                    round(nrow(riceFarms)*.75),
                    replace = FALSE)

# Assigning the testing and training tibble
riceFarmsTraining <- riceFarms[sampleSet, ]
riceFarmsTesting <- riceFarms[-sampleSet, ]

# Generating Decision tree model for the training dataset
riceFarmsDecisionTreeModel <- rpart(formula = FarmOwnership ~ .,
                                    method = "class",
                                    cp = 0.01,
                                    data = riceFarmsTraining)

# Display the decision tree
rpart.plot(riceFarmsDecisionTreeModel)

# Predicting classes for each record in the testing dataset and 
# storing them in riceFarmsPrediction
riceFarmsPrediction <- predict(riceFarmsDecisionTreeModel,
                               riceFarmsTesting,
                               type = 'class')

# Display the prediction
print(riceFarmsPrediction)

# Evaluating the model by confusion matrix
riceFarmsConfusionMatrix <- table(riceFarmsTesting$FarmOwnership,
                                  riceFarmsPrediction)

# Display confusion matrix
print(riceFarmsConfusionMatrix)

# Calculating the model predictive accuracy
predictiveAccuracy <- sum(diag(riceFarmsConfusionMatrix)) /
  nrow(riceFarmsTesting)

# Display the predictive accuracy of the model
print(predictiveAccuracy)

# Generate a decision tree model with a complexity parameter of 0.007
riceFarmsDecisionTreeModel <- rpart(formula = FarmOwnership ~ .,
                                    method = "class",
                                    cp = 0.007,
                                    data = riceFarmsTraining)

# Display the decision tree model
rpart.plot(riceFarmsDecisionTreeModel)

# Predicting classes for each record in the testing dataset and 
# storing them in riceFarmsPrediction
riceFarmsPrediction <- predict(riceFarmsDecisionTreeModel,
                               riceFarmsTesting,
                               type = 'class')

# Display the prediction
print(riceFarmsPrediction)

# Evaluating the model by confusion matrix
riceFarmsConfusionMatrix <- table(riceFarmsTesting$FarmOwnership,
                                  riceFarmsPrediction)

# Display confusion matrix
print(riceFarmsConfusionMatrix)

# Calculating the model predictive accuracy
predictiveAccuracy <- sum(diag(riceFarmsConfusionMatrix)) /
  nrow(riceFarmsTesting)

# Display the predictive accuracy of the model
print(predictiveAccuracy)

# Did increasing the complexity of the decision tree 
# improve the model's predictive accuracy? Why do you
# think this is the case?
# No, increasing the complexity did not improve the accuracy since we are
# overfitting the model to a specific data
