# Vihari Reddy Tummuru
# Importing a dataset of fishing charter and generating neural network to 
# predict chartered boat

# Install the tidyverse and neuralnet packages
# install.packages("tidyverse")
# install.packages("neuralnet")

# Load the tidyverse and neuralnet
library(tidyverse)
library(neuralnet)

# Set the working directory to your Lab11 folder
setwd("C:/Users/ual-laptop/OneDrive - University of Arizona/Desktop/R-545
      /Lab12")

# Read CSV file into a tibble and define column types
fishingCharter <- read_csv(file = "FishingCharter.csv",
                           col_types = "lnn",
                           col_names = TRUE)

# Display fishingCharter tibble
print(fishingCharter)

# Display the structure of fishingCharter tibble
str(fishingCharter)

# Display the summary of fishingCharter tibble
summary(fishingCharter)

# Scale the AnnualIncome feature from 0 to 1
fishingCharter <- fishingCharter %>%
  mutate(AnnualIncomeScaled = (AnnualIncome - min(AnnualIncome)) /
           (max(AnnualIncome) - min(AnnualIncome))) 

# Scale the CatchRate feature from 0 to 1
fishingCharter <- fishingCharter %>%
  mutate(CatchRateScaled = (CatchRate - min(CatchRate)) /
           (max(CatchRate) - min(CatchRate)))

# Set the random seed to 591
set.seed(591)

# Randomly split the dataset into fishingCharterTraining and 
# fishingCharterTesting 
sampleSet <- sample(nrow(fishingCharter),
                    round(nrow(fishingCharter) * 0.75),
                    replace = FALSE)

# Set fishingCharterTraining
fishingCharterTraining <- fishingCharter[sampleSet, ]

# Set fishingCharterTesting
fishingCharterTesting <- fishingCharter[-sampleSet, ]

# Generate the neural network model 
fishingCharterNeuralNet <- neuralnet(
  formula = CharteredBoat ~ AnnualIncomeScaled + CatchRateScaled,
  data = fishingCharterTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE)

# Display the neural network results
print(fishingCharterNeuralNet$result.matrix)

# Visualize the neural network
plot(fishingCharterNeuralNet)

# Use fishingCharterNeuralNet to generate probabilities on the 
# fishingCharterTesting data set and store this in fishingCharterProbability
fishingCharterProbability <- compute(fishingCharterNeuralNet,
                                     fishingCharterTesting)

# Display the probabilities from the testing dataset
print(fishingCharterProbability$net.result)

# Convert probability predictions into 0/1 predictions
fishingCharterPrediction <-
  ifelse(fishingCharterProbability$net.result > 0.5, 1, 0)

# Display the 0/1 predictions
print(fishingCharterPrediction)

# Evaluate the model by forming a confusion matrix
fishingCharterConfusionMatrix <- table(fishingCharterTesting$CharteredBoat,
                                       fishingCharterPrediction)

# Display the confusion matrix
print(fishingCharterConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(fishingCharterConfusionMatrix)) /
  nrow(fishingCharterTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracy)
