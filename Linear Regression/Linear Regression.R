# Vihari Reddy Tummuru
# This code imports a CSV file containing data on Zoo Spending.We added 
# corrplot and olsrr packages in addition to tidyverse to display a 
# correlation matrix, a correlation plot, and to evaluate multicollinearity
# on the factors that determine the amount of money spent during Zoo visits.
# Three Histograms Of MilesFromZoo, PartySize and VisitSpending are produced
# to visually understand the Zoo Spending data.

# Install and load tidyverse package
install.packages("tidyverse")
library(tidyverse)

# Install and load corrplot package
install.packages("corrplot")
library(corrplot)

# Install and load olsrr package
install.packages("olsrr")
library(olsrr)

# Set working directory to Lab05 folder
setwd("C:/Users/ual-laptop/OneDrive - University of Arizona/Desktop/R-545/Lab05")


# Read CSV file into a tibble and define column types
zooSpending<- read_csv(file = "ZooVisitSpending.csv",
                       col_types = "niil",
                       col_names = TRUE)

# Display the zoo spending tibble
print(zooSpending)

# Display the structure of the tibble
print(str(zooSpending))

# Display the summary of the tibble
print(summary(zooSpending))

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
displayAllHistograms(zooSpending)

# Display a correlation matrix of zoo spending that is rounded to two decimals
round(cor(zooSpending %>% keep(is.numeric)),digits = 2)

# Display of a correlation plot of zoo spending with certain features
corrplot(cor(zooSpending),
         method = "number",
         type = "lower")

# Generating a linear regression model in a new zooSpendingModel tibble 
zooSpendingModel <- lm(data = zooSpending,
                       formula = VisitSpending ~.)

# Display beta coefficients of the zooSpendingModel tibble
print(zooSpendingModel)

# Display a summary of the zooSpendingModel tibble's 
# linear regression model results.
print(summary(zooSpendingModel))

# Test for Multicollinearity of the zooSpendingModel tibble
ols_vif_tol(zooSpendingModel)
      