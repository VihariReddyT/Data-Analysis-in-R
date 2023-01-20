# Vihari Reddy Tummuru
# This code generates association rules
# to for items in instacart transactions

# Installing the tidyverse and arules package
install.packages("tidyverse")
install.packages("arules")

# Loading tidyverse and arules packages
library("tidyverse")
library("arules")

# Setting the Working Directory
setwd("C:/Users/ual-laptop/OneDrive - University of Arizona/Desktop/R-545
      /Lab10")

# Import InstaTransactions into an object
instacartTransactions <- read.transactions(file ="InstacartTransactions.csv",
                                           format = "single",
                                           header = TRUE,
                                           sep = ",",
                                           cols = c("OrderID", "ItemID"))

# Display summary of transactions
summary(instacartTransactions)

# Displaying first 3 transacations
inspect(instacartTransactions[1:3])

# Examine the frequency of an item
itemFrequency(instacartTransactions[,"24852"])

# Converting the frequency values in instacartTransactions into a tibble 
instacartTransactionsFrequency <-
  tibble(Items = names(itemFrequency(instacartTransactions)),
         Frequency = itemFrequency(instacartTransactions))

# Displaying item frequencies on console
print(instacartTransactionsFrequency)

# Displaying 10 most frequently purchased items
instacartTransactionsFrequency %>% 
  arrange(desc(Frequency)) %>% slice(1:10)

# Generate association rules model
instacartTransactionsRules <- apriori(instacartTransactions,
                                      parameter = list(
                                        support = 0.005,
                                        confidence = 0.2,
                                        minlen = 2))

# Displaying summary of instacartTransactionsRules
summary(instacartTransactionsRules)

# Sort the association rules by lift and view top 10
instacartTransactionsRules %>%
  sort(by = "lift") %>%
  head(n = 10) %>%
  inspect()
