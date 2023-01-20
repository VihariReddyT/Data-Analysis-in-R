# Vihari Reddy Tummuru
# This code imports a CSV file containing data on grocery transactions. The
# orginal tibble is altered twice, once to subset features, and another time to 
# create a new feature called AveragePricePerUnit. Filters are ran to return
# certain data. A histogram of average price per unit and a boxplot of revenue
# are created

# Install and load tidyverse package
install.packages("tidyverse")
library(tidyverse)

# Set working directory to Lab03 folder
setwd("C:/Users/ual-laptop/OneDrive - University of Arizona/Desktop/R-545/Lab03")

# Read CSV file into a tibble and define column types
groceryTransactions1 <- read_csv(file = "GroceryTransactions.csv",
                                 col_types = "iDffffifffffffin",
                                 col_names = TRUE)

# Display the grocery transactions tibble
print(groceryTransactions1)

# Display the first 20 rows 
print(head(groceryTransactions1, n = 20))

# Display the structure of the tibble
print(str(groceryTransactions1))

# Display the summary of the tibble
print(summary(groceryTransactions1))

# Display mean of revenue, median of units sold, standard deviation of revenue
print(summarize(.data = groceryTransactions1, mean(Revenue)))
print(summarize(.data = groceryTransactions1, median(UnitsSold)))
print(summarize(.data = groceryTransactions1, sd(Revenue)))

# Display the IQR of units sold, minimum of revenue, and maximum of children
print(summarize(.data = groceryTransactions1, IQR(UnitsSold)))
print(summarize(.data = groceryTransactions1, min(Revenue)))
print(summarize(.data = groceryTransactions1, max(Children)))

# New tibble groceryTransactions2 which contains subset of groceryTransaction1
groceryTransactions2 <- select(.data = groceryTransactions1,
                                            PurchaseDate,
                                            Homeowner,
                                            Children,
                                            AnnualIncome,
                                            UnitsSold,
                                            Revenue)

# Features for transactions made by non-homeowners with at least 4 children
print(filter(.data = groceryTransactions2,
             Homeowner == "N"
             & Children >= 4))

# Features for transactions made by either customers in the $150K + annual
# income OR had more than 6 units sold
print(filter(.data = groceryTransactions2,
             AnnualIncome == "$150K +"
             | UnitsSold > 6))

# Average transaction revenue grouped by annual income level from largest to
# smallest
print(groceryTransactions1 %>%
        group_by(AnnualIncome) %>%
        summarize(MeanRevenue = mean(Revenue)) %>%
        arrange(desc(MeanRevenue)),
      n = Inf)

# Create and display new tibble that includes AveragePricePerUnit
groceryTransactions3 <- groceryTransactions2 %>%
  mutate(AveragePricePerUnit = Revenue / UnitsSold)
print(groceryTransactions3)

# Histogram displaying AveragePricePerUnit (binned every 1 unit)
histogramAveragePricePerUnit <- ggplot(data = groceryTransactions3,
                                       aes(x=AveragePricePerUnit))
histogramAveragePricePerUnit + geom_histogram(binwidth = 1,
                                              color = "black",
                                              fill = "orange",
                                              alpha = 0.50) +
  ggtitle("Average Price Per Unit Histogram")

# Boxplot displaying revenue
boxplotRevenue <- ggplot(data = groceryTransactions3,
                         aes(x=Revenue))
boxplotRevenue + geom_boxplot(color = "#0C234B",
                              fill = "#AB0520") + 
  ggtitle("Boxplot of Revenue")