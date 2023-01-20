# Vihari Reddy Tummuru

# Install and load tidyverse package
# install.packages("tidyverse")
library(tidyverse)

# Load Class package
library(class)

# Set working directory to Lab05 folder
setwd("C:/Users/ual-laptop/OneDrive - University of Arizona/Desktop/R-545/Lab07")

# Read CSV file into a tibble and define column types
sedanSize  <- read_csv(file = "SedanSize.csv",
                        col_types = "cfnii",
                        col_names = TRUE)

# Display the sedanSize tibble
print(sedanSize)

# Display the structure of the tibble
print(str(sedanSize))

# Display the summary of the tibble
print(summary(sedanSize))

# Removing MakeModel Feature
sedanSize <- sedanSize %>% select(-MakeModel)

# Splitting the data into two groups
sedanSizeLabels <- sedanSize %>% select(SedanSize)
sedanSize <- sedanSize %>% select(-SedanSize)

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
displayAllHistograms(sedanSize) 

# Splitting sedanSize data into training and test data sets
set.seed(517)
sampleSet <- sample(nrow(sedanSize),
                    round(nrow(sedanSize)*.75),
                    replace = FALSE)

# Put the records from 75% training into Training tibbles 
sedanSizeTraining <- sedanSize[sampleSet, ]
sedanSizeTrainingLabels <- sedanSizeLabels[sampleSet, ]

# Put the records from 25% into Testing tibbles
sedanSizeTesting <- sedanSize[-sampleSet, ]
sedanSizeTestingLabels <- sedanSizeLabels[-sampleSet, ]

# Generate the K-nearest Model
sedanSizePrediction <- knn(train = sedanSizeTraining,
                           test = sedanSizeTesting,
                           cl = sedanSizeTrainingLabels$SedanSize,
                           k = 7)

# Display the predictions from the testing data on the console
print(sedanSizePrediction)

# Display the summary of prediction from the testing dataset
print(summary(sedanSizePrediction))

# Evaluate the model by forming confusion matrix
sedanSizeConfusionMatrix <- table(sedanSizeTestingLabels$SedanSize,
                                  sedanSizePrediction)

# Display the confusion matrix
print(sedanSizeConfusionMatrix)

# Calculate the predictive accuracy model
predictiveAccuracy <- sum(diag(sedanSizeConfusionMatrix))/
  nrow(sedanSizeTesting)
predictiveAccuracy

# Create a Kvalue matrix along with their predictive accuracy
KValueMatrix <- matrix(data = NA,
                       nrow = 0,
                       ncol= 2)

# Adding column headings
colnames(KValueMatrix) <- c("k value","Predictive Accuracy")

# Looping through different values of k with the training dataset
for (kValue in 1:nrow(sedanSizeTraining)){
  # Calculate prdictive accuracy only if k value is odd
  if (kValue %% 2 !=0) {
    # Generate the Model
    sedanSizePrediction <- knn(train = sedanSizeTraining,
                               test = sedanSizeTesting,
                               cl = sedanSizeTrainingLabels$SedanSize,
                               k = kValue)
    # Generate the confusion matrix
    sedanSizeConfusionMatrix <- table(sedanSizeTestingLabels$SedanSize,
                                      sedanSizePrediction)
    
    # Calculate the predictive accuracy
    predictiveAccuracy <- sum(diag(sedanSizeConfusionMatrix))/
      nrow(sedanSizeTesting)
    
    # Adding a new row
    KValueMatrix <- rbind(KValueMatrix, c(kValue,predictiveAccuracy))
  }
}

# Display the kValue Matrix
print(KValueMatrix)


# Answer 1
# The Best Value for k would be 15 since the more the datasets and classes 
# around the better the k - nearest model is in predicting the correct class.

# Answer 2
# With this model the automobile manufacture can focus on which type of car to
# manufacture, saving him time and money, and also with further analysis we can 
# predict the best price range for that model.