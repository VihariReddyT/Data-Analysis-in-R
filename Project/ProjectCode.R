# MIS 545 Project Section 1 Group 12
# Bhargavi Somanahalli Muralidhara, Chace Griffin, Mohammed Almizraq, Mrudang
# Langalia, Vihari Reddy Tummuru

# In this project we are determining which features should be included in the 
# mobile device and then hence optimize manufacturing to increase sales and 
# revenue. We are predicting what features mobile phones should have in order 
# to be sold.

# Install the packages
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("e1071")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("neuralnet")

# Load the packages
library(tidyverse)
library(corrplot)
library(olsrr)
library(class)
library(e1071)
library(rpart)
library(rpart.plot)
library(neuralnet)

# Set working directory to project folder
setwd("C:/Users/ual-laptop/Desktop/MIS545Project")

# Read MobilePhoneSold.csv file into a tibble and define column types
mobilePhone <- read_csv(file = "MobilePhoneSold.csv",
                        col_types = "iinliliniiiiiiiiillll",
                        col_names = TRUE)

# Display the mobilePhone tibble in the console
print(mobilePhone)

# Display the structure of the mobilephone tibble 
print(str(mobilePhone))

# Display the summary of the mobilephone tibble in the console
print(summary(mobilePhone))

# Creating a function to display histogram by converting all columns to numeric
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x = value,fill = key),
                              color = "black")+
    facet_wrap( ~ key,scales = "free")+
    theme_minimal()
}

# Display the histogram of the mobilephone tibble
displayAllHistograms(mobilePhone)

# Display a correlation matrix of mobilePhone tibble and rounding the 
# coefficient to 2 digits after decimal
round(cor(mobilePhone),2)

# Display a correlation plot of mobilePhone with number
corrplot(cor(mobilePhone),
         method = "number",
         type = "lower")

# Three queries that show some interesting insight into the dataset

# Important features of phones sold which are 4G but do not have bluetooth 
print(mobilePhone %>% 
        select(BatteryPower,
               Bluetooth,
               FourG,
               InternalMemory,
               NumberOfCores,
               PrimaryCameraMegaPixels,
               TouchScreen,
               Wifi,
               MobileSold) %>%
        filter(Bluetooth == 0 & FourG == 1 & MobileSold == 1))

# Average battery power for mobile phones sold with different cores 
print(mobilePhone %>%
        filter(MobileSold == 1) %>%
        group_by(NumberOfCores) %>%
        summarize(AverageBatteryPower = mean(BatteryPower)) %>%
        arrange((NumberOfCores)),
      n = Inf)

# There are more than 400 phones that were not sold because they have less talk
# time than 10 
print(mobilePhone %>% 
        select(BatteryPower,
               FrontCameraMegaPixel,
               InternalMemory,
               TalkTime,
               FourG,
               TouchScreen,
               Wifi,
               MobileSold) %>%
        filter(MobileSold == 0 & TalkTime < 10) %>%
        arrange(desc(BatteryPower)))

# ----------------------------------------------------------------------------

# Logistic Regression

# This set of code predicts the probability that a mobile will be sold or 
# unsold based on multiple mobile features. This model also produces odds 
# ratios for each independent variable, and calculates a predictive accuracy 
# for the model as a whole.

# Set seed using 545 as the random seed
set.seed(545)

# Create random sample from original dataset
sampleSet <- sample(nrow(mobilePhone),
                    round(nrow(mobilePhone)*.75),
                    replace = FALSE)

# 75% of randomly split dataset in mobilePhoneTraining tibble
mobilePhoneTraining <- mobilePhone[sampleSet, ]

# 75% of randomly split dataset in mobilePhoneTest tibble
mobilePhoneTesting <- mobilePhone[-sampleSet, ]

# Checking for class imbalance
summary(mobilePhoneTraining$MobileSold)

# Generate logistic regression Model 
mobilePhoneModelLogistic<- glm(data = mobilePhoneTraining, 
                               family = binomial, 
                               formula = MobileSold ~ .)

# Display the logistic model summary
summary(mobilePhoneModelLogistic)

# Odds ratios of the independent variables
exp(coef(mobilePhoneModelLogistic)["BatteryPower"])
exp(coef(mobilePhoneModelLogistic)["Bluetooth"])
exp(coef(mobilePhoneModelLogistic)["ClockSpeed"])
exp(coef(mobilePhoneModelLogistic)["FrontCameraMegaPixel"])
exp(coef(mobilePhoneModelLogistic)["FiveGTRUE"])
exp(coef(mobilePhoneModelLogistic)["InternalMemory"])
exp(coef(mobilePhoneModelLogistic)["MobileDepth"])
exp(coef(mobilePhoneModelLogistic)["MobileWeight"])
exp(coef(mobilePhoneModelLogistic)["NumberOfCores"])
exp(coef(mobilePhoneModelLogistic)["PrimaryCameraMegaPixels"])
exp(coef(mobilePhoneModelLogistic)["PixelResolutionHeight"])
exp(coef(mobilePhoneModelLogistic)["PixelResolutionWidth"])
exp(coef(mobilePhoneModelLogistic)["ScreenHeight"])
exp(coef(mobilePhoneModelLogistic)["ScreenWidth"])
exp(coef(mobilePhoneModelLogistic)["TalkTime"])
exp(coef(mobilePhoneModelLogistic)["FourGTRUE"])
exp(coef(mobilePhoneModelLogistic)["TouchScreenTRUE"])
exp(coef(mobilePhoneModelLogistic)["WifiTRUE"])

# Use the model to predict outcomes in the testing dataset
mobilePhonePredictionLogistic <- predict(mobilePhoneModelLogistic,
                                 mobilePhoneTesting,
                                 type = 'response')

# Display the test model
print(mobilePhonePredictionLogistic)

# Converting less than 0.5 as 0 and greater than 0.5 as 1 using IF Else 
mobilePhonePredictionLogistic <- 
  ifelse(mobilePhonePredictionLogistic >= 0.5,1,0)

# Generating a mobile phone confusion matrix
mobilePhoneConfusionMatrixLogistic <- table(mobilePhoneTesting$MobileSold,
                                            mobilePhonePredictionLogistic)

# Display confusion matrix (mobilePhoneConfusionMatrix)
print(mobilePhoneConfusionMatrixLogistic)

# Calculating false negative for confusion matrix
mobilePhoneConfusionMatrixLogistic[1,2]/
  (mobilePhoneConfusionMatrixLogistic[1,2] + 
     mobilePhoneConfusionMatrixLogistic[1,1])

# Calculating false positive for confusion matrix
mobilePhoneConfusionMatrixLogistic[2,1]/
  (mobilePhoneConfusionMatrixLogistic[2,1]+ 
     mobilePhoneConfusionMatrixLogistic[2,2])

# Calculating Model Prediction Accuracy 
sum(diag(mobilePhoneConfusionMatrixLogistic))/ nrow(mobilePhoneTesting)

# ----------------------------------------------------------------------------
# K-nearest neighbors

# This code used the K-nearest algorithm to group whether mobile phones have 
# been sold or not based on the features of the phones.

# Separate the tibble into two
mobilePhoneLabels <- mobilePhone %>% select(MobileSold)
mobilePhoneKNN <- mobilePhone %>% select(-MobileSold)

# Create a function called displayAllHistograms that takes in a
# tibble parameter and creates histogram
displayAllHistograms <- function(tibbleDataSet) {
  tibbleDataSet %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value, fill=key),
                              color = "Black") +
    facet_wrap(~ key, scales = "free") +
    theme_minimal()
}

# Pass parameter mobilePhoneKNN to get histogram plots
displayAllHistograms(mobilePhoneKNN)

# Split data into training and testing using random seed
set.seed(545)

# Create a vector of 75% randomly sampled rows from the dataset
sampleSetKNN <- sample(nrow(mobilePhoneKNN),
                       round(nrow(mobilePhoneKNN) * 0.75),
                       replace = FALSE)

# Put the record from the 75% sample into mobilePhoneTraining
mobilePhoneTrainingKNN <- mobilePhoneKNN[sampleSet, ]
mobilePhoneTrainingLabels <- mobilePhoneLabels[sampleSet, ]

# Put all other records into mobilePhoneTesting
mobilePhoneTestingKNN <- mobilePhoneKNN[-sampleSet, ]
mobilePhoneTestingLabels <- mobilePhoneLabels[-sampleSet, ]

# Generate k-Nearest Neighbors model
mobilePhonePredictionKNN <- knn(train = mobilePhoneTrainingKNN,
                                test = mobilePhoneTestingKNN,
                                cl = mobilePhoneTrainingLabels$MobileSold,
                                k=7)

# Display the predictions
print(mobilePhonePredictionKNN)

# Display the summary of predictions
print(summary(mobilePhonePredictionKNN))

# Evaluate the model by Confusion Matrix
mobilePhoneConfusionMatrixKNN <- table(mobilePhoneTestingLabels$MobileSold,
                                       mobilePhonePredictionKNN)
# Display Confusion Matrix
print(mobilePhoneConfusionMatrixKNN)

# Calculate the model predictive accuracy
predictiveAccuracyKNN <- sum(diag(mobilePhoneConfusionMatrixKNN)) /
  nrow(mobilePhoneTesting)

# Display predictions
print(predictiveAccuracyKNN)

# Display summary of the predictions
print(summary(predictiveAccuracyKNN))

# Create a matrix of k-values with their predictive accuracy
kValueMatrix <- matrix(data = NA,
                       nrow = 0,
                       ncol = 2)

# Assign column names to matrix
colnames(kValueMatrix) <- c("k value","Predictive Accuracy")

# Loop through with different values of k
for (kValue in 1:100) {
  
  # Only calculate predictive accuracy if the k value is odd
  if(kValue %% 2 != 0) {
    
    # Generate the model
    mobilePhonePredictionKNN <- knn(train = mobilePhoneTraining,
                                    test = mobilePhoneTesting,
                                    cl = mobilePhoneTrainingLabels$MobileSold,
                                    k = kValue) 
    
    # Generate the confusion matrix
    mobilePhoneConfusionMatrixKNN <-
      table(mobilePhoneTestingLabels$MobileSold,
            mobilePhonePredictionKNN)
    
    # Calculate the predictive accuracy
    predictiveAccuracyKNN <- sum(diag(mobilePhoneConfusionMatrixKNN)) /
      nrow(mobilePhoneTesting)
    
    # Add a new row to the kValueMatrix
    kValueMatrix <- rbind(kValueMatrix, c(kValue, predictiveAccuracyKNN))
  }
}

# Display and view the kValueMatrix to determine the best k value
print(kValueMatrix)

# -----------------------------------------------------------------------------

# Naïve Bayes

# This code uses the Naive Bayes algorithm to predict the probability if a 
# mobile phone is sold or not.

# Train the Naive Bayes Model
mobilePhoneModelNaive <- naiveBayes(formula = MobileSold ~ .,
                               data = mobilePhoneTraining,
                               laplace = 1)

# Build probabilities for each record in the testing dataset
mobilePhoneProbabilityNaive <- predict(mobilePhoneModelNaive,
                                  mobilePhoneTesting,
                                  type = "raw")

# Print the probability obtained for each record
print(mobilePhoneProbabilityNaive)

# Predict classes for each record in the testing dataset
mobilePhonePredictionNaive <- predict(mobilePhoneModelNaive,
                                 mobilePhoneTesting,
                                 type = "class")

# Display the predictions on the console
print(mobilePhonePredictionNaive)

# Create a confusion matrix
mobilePhoneConfusionMatrixNaive <- table(mobilePhoneTesting$MobileSold,
                                         mobilePhonePredictionNaive)

# Display the confusion matrix on the console
print(mobilePhoneConfusionMatrixNaive)

# Calculating false negative for Naive confusion matrix
mobilePhoneConfusionMatrixNaive[1,2]/
  (mobilePhoneConfusionMatrixNaive[1,2] + 
     mobilePhoneConfusionMatrixNaive[1,1])

# Calculating false positive for Naive confusion matrix
mobilePhoneConfusionMatrixNaive[2,1]/
  (mobilePhoneConfusionMatrixNaive[2,1]+ 
     mobilePhoneConfusionMatrixNaive[2,2])

# Calculate the model predictive accuracy
predictiveAccuracyNaive <- sum(diag(mobilePhoneConfusionMatrixNaive))/
  nrow(mobilePhoneTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracyNaive)

# ----------------------------------------------------------------------------

# Decision Tree

# This code will use the Decision Tree algorithm and select the most 
# significant feature of mobile phones and then branches out based on the 
# outcomes at each step.

# Generate the decision tree model to predict MobileSold based on the
# other variables in the dataset. Use 0.01 as the complexity parameter.
mobilePhoneDecisionTreeModel1 <- rpart(formula = MobileSold ~ .,
                                       method = "class",
                                       cp = 0.01,
                                       data = mobilePhoneTraining)

# Display the decision tree visualization in R
rpart.plot(mobilePhoneDecisionTreeModel1)

# Predict classes for each record in the testing dataset
# and store them in mobilePhonePrediction1
mobilePhonePredictionDecision1 <- predict(mobilePhoneDecisionTreeModel1,
                                  mobilePhoneTesting,
                                  type = "class")

# Display mobilePhonePrediction1 on the console
print(mobilePhonePredictionDecision1)

# Evaluate the model by forming a confusion matrix
mobilePhoneConfusionMatrixDecision1 <- table(mobilePhoneTesting$MobileSold,
                                     mobilePhonePredictionDecision1)

# Display the confusion matrix on the console
print(mobilePhoneConfusionMatrixDecision1)

# Calculating false negative for Decison 1 confusion matrix
mobilePhoneConfusionMatrixDecision1[1,2]/
  (mobilePhoneConfusionMatrixDecision1[1,2] + 
     mobilePhoneConfusionMatrixDecision1[1,1])

# Calculating false positive for Decison 1 confusion matrix
mobilePhoneConfusionMatrixDecision1[2,1]/
  (mobilePhoneConfusionMatrixDecision1[2,1]+ 
     mobilePhoneConfusionMatrixDecision1[2,2])

# Calculate the model predictive accuracy and
# store it into a variable called predictiveAccuracy1
predictiveAccuracyDecision1 <- sum(diag(mobilePhoneConfusionMatrixDecision1))/
  nrow(mobilePhoneTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracyDecision1)

# Create a new decision tree model using 0.006 as the complexity parameter,
# display the decision tree visualization, and calculate its predictive accuracy
mobilePhoneDecisionTreeModel2 <- rpart(formula = MobileSold ~ .,
                                       method = "class",
                                       cp = 0.006,
                                       data = mobilePhoneTraining)

# Display the decision tree 2 visualization in R
rpart.plot(mobilePhoneDecisionTreeModel2)

# Predict classes for each record in the testing dataset
# and store them in mobilePhonePrediction2 using the new decision tree model
mobilePhonePredictionDecision2 <- predict(mobilePhoneDecisionTreeModel2,
                                  mobilePhoneTesting,
                                  type = "class")

# Display mobilePhonePrediction2 on the console
print(mobilePhonePredictionDecision2)

# Evaluate the model by creating mobilePhoneConfusionMatrix2
mobilePhoneConfusionMatrixDecision2 <- table(mobilePhoneTesting$MobileSold,
                                             mobilePhonePredictionDecision2
                                             )

# Display the confusion matrix on the console
print(mobilePhoneConfusionMatrixDecision2)

# Calculating false negative for Decison 2 confusion matrix
mobilePhoneConfusionMatrixDecision2[1,2]/
  (mobilePhoneConfusionMatrixDecision2[1,2] + 
     mobilePhoneConfusionMatrixDecision2[1,1])

# Calculating false positive for Decison 2 confusion matrix
mobilePhoneConfusionMatrixDecision2[2,1]/
  (mobilePhoneConfusionMatrixDecision2[2,1]+ 
     mobilePhoneConfusionMatrixDecision2[2,2])

# Calculate the 2nd model predictive accuracy and
# store it into a variable called predictiveAccuracy2
predictiveAccuracyDecision2 <- sum(diag(mobilePhoneConfusionMatrixDecision2))/
  nrow(mobilePhoneTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracyDecision2)

# -----------------------------------------------------------------------------

# Neural Network

# This code produces a neural network model for our mobile phone data
# and predicts whether or not we should produce the phone or not, based upon 
# several parameters such as battery power, ram, and screen width.

# Set the working directory
setwd("C:/Users/ual-laptop/OneDrive - University of Arizona/Desktop/Fall 2022/MIS 545 Data Mining/Final")

# Reading the CSV file into an object called mobilePhoneNeural
mobilePhoneNeural <- read_csv(file = "MobilePhoneSold.csv",
                              col_types = "ilnliliniiiiiiiiillll",
                              col_names = TRUE)

# Displays the mobilePhoneNeural tibble on the console using the print() 
# function
print(mobilePhoneNeural)

# Displays the structure of the mobilePhoneNeural tibble using the str() 
# function
str(mobilePhoneNeural)

# Displaying a summary of the mobilePhoneNeural tibble using the summary() 
# function
summary(mobilePhoneNeural)

# Scaling the variables
mobilePhoneNeural <- mobilePhoneNeural %>%
  mutate(BatteryPowerScaled = (BatteryPower - min(BatteryPower))/
           (max(BatteryPower)-min(BatteryPower))) %>%
  mutate(ClockSpeedScaled = (ClockSpeed - min(ClockSpeed))/
           (max(ClockSpeed)-min(ClockSpeed))) %>%
  mutate(FrontCameraMegaPixelScaled = (FrontCameraMegaPixel - 
                                         min(FrontCameraMegaPixel))/
           (max(FrontCameraMegaPixel)-min(FrontCameraMegaPixel))) %>%
  mutate(InternalMemoryScaled = (InternalMemory - min(InternalMemory))/
           (max(InternalMemory)-min(InternalMemory))) %>%
  mutate(MobileDepthScaled = (MobileDepth - min(MobileDepth))/
           (max(MobileDepth)-min(MobileDepth))) %>%
  mutate(MobileWeightScaled = (MobileWeight - min(MobileWeight))/
           (max(MobileWeight)-min(MobileWeight))) %>%
  mutate(NumberOfCoresScaled = (NumberOfCores - min(NumberOfCores))/
           (max(NumberOfCores)-min(NumberOfCores))) %>%
  mutate(PrimaryCameraMegaPixelsScaled = (PrimaryCameraMegaPixels - 
                                            min(PrimaryCameraMegaPixels))/
           (max(PrimaryCameraMegaPixels)-min(PrimaryCameraMegaPixels))) %>%
  mutate(PixelResolutionHeightScaled = (PixelResolutionHeight - 
                                          min(PixelResolutionHeight))/
           (max(PixelResolutionHeight)-min(PixelResolutionHeight))) %>%
  mutate(PixelResolutionWidthScaled = (PixelResolutionWidth - 
                                         min(PixelResolutionWidth))/
           (max(PixelResolutionWidth)-min(PixelResolutionWidth))) %>%
  mutate(RamScaled = (Ram - min(Ram))/
           (max(Ram)-min(Ram))) %>%
  mutate(ScreenHeightScaled = (ScreenHeight - min(ScreenHeight))/
           (max(ScreenHeight)-min(ScreenHeight))) %>%
  mutate(ScreenWidthScaled = (ScreenWidth - min(ScreenWidth))/
           (max(ScreenWidth)-min(ScreenWidth))) %>%
  mutate(TalkTimeScaled = (TalkTime - min(TalkTime))/
           (max(TalkTime)-min(TalkTime))) 

# Setting the random seed to 545
set.seed(545)

# Randomly split the data set into mobilePhoneNeuralTraining (75% of records) 
# and mobilePhoneNeuralTesting (25% of records)
sampleSet <- sample(nrow(mobilePhoneNeural),
                    round(nrow(mobilePhoneNeural) * 0.75),
                    replace = FALSE)
mobilePhoneNeuralTraining <- mobilePhoneNeural[sampleSet, ]
mobilePhoneNeuralTesting <- mobilePhoneNeural[-sampleSet, ]

# Generate a neural network model with one hidden layer to predict MobileSold
# using the input variables
mobilePhoneNeuralNet <- neuralnet(
  formula = MobileSold ~ BatteryPowerScaled + Bluetooth + ClockSpeedScaled + 
    DualSim + FrontCameraMegaPixelScaled + FiveG + InternalMemoryScaled + 
    MobileDepthScaled + MobileWeightScaled + NumberOfCoresScaled + 
    PrimaryCameraMegaPixelsScaled + PixelResolutionHeightScaled + 
    PixelResolutionWidthScaled + RamScaled + ScreenHeightScaled + 
    ScreenWidthScaled + TalkTimeScaled + FourG + TouchScreen + Wifi,
  data = mobilePhoneNeuralTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE
)

# Display the neural network numeric results
print(mobilePhoneNeuralNet$result.matrix)

# Visualize the neural network
plot(mobilePhoneNeuralNet)

# Use mobilePhoneNeuralNet to generate probabilities on the
# mobilePhoneNeuralTesting data set and store this in 
# mobilePhoneProbabilityNeural
mobilePhoneProbabilityNeural <- compute(mobilePhoneNeuralNet,
                                        mobilePhoneNeuralTesting)

# Displaying the probabilities from the testing data set on the console
print(mobilePhoneProbabilityNeural)

# Converting probability predictions into 0/1 predictions and store this into
# mobilePhonePredictionNeural
mobilePhonePredictionNeural <-
  ifelse(mobilePhoneProbabilityNeural$net.result > 0.5, 1, 0)

# Display the 0/1 predictions on the console
print(mobilePhonePredictionNeural)

# Evaluating the model by forming a confusion matrix
mobilePhoneConfusionMatrixNeural <- table(mobilePhoneNeuralTesting$MobileSold,
                                          mobilePhonePredictionNeural)

# Displaying the confusion matrix on the console
print(mobilePhoneConfusionMatrixNeural)

# Calculating false negative for Neural confusion matrix
mobilePhoneConfusionMatrixNeural[1,2]/
  (mobilePhoneConfusionMatrixNeural[1,2] + 
     mobilePhoneConfusionMatrixNeural[1,1])

# Calculating false positive for Neural confusion matrix
FalsePositiveRate <- mobilePhoneConfusionMatrixNeural[2,1]/
  (mobilePhoneConfusionMatrixNeural[2,1]+ 
     mobilePhoneConfusionMatrixNeural[2,2])

# Calculating the model predictive accuracy
predictiveAccuracyNeural <- sum(diag(mobilePhoneConfusionMatrixNeural))/
  nrow(mobilePhoneNeuralTesting)

# Displaying the predictive accuracy on the console
print(predictiveAccuracyNeural)