# Vihari Reddy Tummuru
# Created a Naive bayes model to predict the type of Dwelling with Age,
# gender, living status and pay type.Model results in a low accuracy of 
# 52%.

# Install and load tidyverse package
# install.packages("tidyverse")
library(tidyverse)

# Install and load e1071 package
install.packages("e1071")
library(e1071)

# Set working directory to Lab08 folder
setwd("C:/Users/ual-laptop/OneDrive - University of Arizona/Desktop/R-545/Lab08")

# Read CSV file into a tibble and define column types
dwellingType  <- read_csv(file = "DwellingType.csv",
                       col_types = "filll",
                       col_names = TRUE)

# Display the dwellingType tibble
print(dwellingType)

# Display the structure of the tibble
print(str(dwellingType))

# Display the summary of the tibble
print(summary(dwellingType))

# Randomly splitting the data into a smaple set
set.seed(154)
sampleSet <- sample(nrow(dwellingType),
                    round(nrow(dwellingType)*.75),
                    replace = FALSE)

# Assigning the testing and training tibble
dwellingTypeTraining <- dwellingType[sampleSet, ]
dwellingTypeTesting <- dwellingType[-sampleSet, ]

# Generating the Naive Bayes Model
dwellingTypeModel <- naiveBayes(formula = DwellingType ~ .,
                                data = dwellingTypeTraining)

# Build probabilities for each record in the testing dataset
dwellingTypeProbability <- predict(dwellingTypeModel,
                                   dwellingTypeTesting,
                                   type = "raw")

# Display the tibble
print(dwellingTypeProbability)

# Predict Classes for each record
dwellingTypePrediction <- predict(dwellingTypeModel,
                                  dwellingTypeTesting,
                                  type = "class")

# Display the dwellingTypePrediction tibble
print(dwellingTypePrediction)

# Evaluate the model by forming a confusion matrix
dwellingConfusionMatrix <- table(dwellingTypeTesting$DwellingType,
                                 dwellingTypePrediction)

# Display the dwellingConfusion Matrix
print(dwellingConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(dwellingConfusionMatrix)) /
  nrow(dwellingTypeTesting)

# Display the predictive accuracy of the model
print(predictiveAccuracy)
