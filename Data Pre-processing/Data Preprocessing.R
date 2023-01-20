# Vihari Reddy Tummuru
# This code imports a CSV file containing data on Automobile Tire Tread.The
# orginal tibble is altered five times,first to read the csv file ,second to
# impute missing data in UsageMonths with mean,third to normalize the skewed
# distribution using the log method and creating a new column of 
# logUsageMonths,fourth to create a new column with mutate function and using 
# a conditional statement to show treadDepth less than 1.6,and fifth to create
# a data frame so that we can dummy code and divide the position column into 
# four new columns.A scatter plot of Miles and TreadDepth is created and 
# titled as "Tire Miles and Tread Depth Scatter Plot."

# Install and load tidyverse package
install.packages("tidyverse")
library(tidyverse)

# Install and load dummies package from Lab04 folder
install.packages("dummies", repos = NULL, type="source")
library(dummies)

# Set working directory to Lab04 folder
setwd("C:/Users/ual-laptop/OneDrive - University of Arizona/Desktop/R-545/Lab04")

# Read CSV file into a tibble and define column types
tireTread1 <- read_csv(file = "TireTread.csv",
                       col_types = "cfnni",
                       col_names = TRUE)

# Display the tireTread1 tibble
print(tireTread1)

# Display the structure of the tibble
str(tireTread1)

# Display the summary of the tibble
summary(tireTread1)

# Creating a new tireTread2 tibble which imputes the missing data 
# in UsageMonths column with the mean of UsageMonths
tireTread2 <- tireTread1 %>%
  mutate(UsageMonths = ifelse(is.na(UsageMonths),
                              mean(UsageMonths,na.rm = TRUE)
                              ,UsageMonths))

# Display the summary Of Tiretread2
summary(tireTread2)

# Determining minimum value for outlier in TreadDepth and storing it 
# outlierMin tibble
outlierMin <- quantile(tireTread2$TreadDepth,.25) -
  (IQR(tireTread2$TreadDepth)* 1.5)

# Determining maximum value for outlier in TreadDepth and storing it 
# outlierMax tibble
outlierMax <- quantile(tireTread2$TreadDepth,.75) +
  (IQR(tireTread2$TreadDepth)* 1.5)

# Adding the Outliers into a treaddepthOutliers tibble
treadDepthOutliers <- tireTread2 %>%
  filter(TreadDepth < outlierMin | TreadDepth > outlierMax)

# Creating a tireTread3 tibble to store the log of UsageMonths as
# LogUsageMonths and add LogUsageMoths as a new column 
tireTread3<- tireTread2%>%
  mutate(LogUsageMonths = log(UsageMonths))

# Discretizing TreadDepth Column into NeedsReplacing with a condition where
# TreadDepth is less than equal to 1.6
tireTread4 <- tireTread3%>%
  mutate(NeedsReplacing = TreadDepth<= 1.6)

# Converting tireTread4 into a data frame
tireTread4DataFrame <- data.frame(tireTread4)

# Dummy Coding the Position Column into 4 columns of LF,RF,LR,RR
# by using dummies package with dummy.data.frame()located in Lab04 folder
# and storing it into a new tibble tireTread5
tireTread5 <- as_tibble(dummy.data.frame(data = tireTread4DataFrame,
                                         names = "Position"))

# Creating a scatter plot visualization of Miles and TreadDepth
# using ggplot() function and naming the tibble as scatterplotMilesTreadDepth
scatterplotMilesTreadDepth <-  ggplot(data = tireTread5,
                                      aes(x=Miles,
                                          y=TreadDepth))
scatterplotMilesTreadDepth
# Changing the point colour to dark grey using geom_point() ,
# Adding a title using ggtitle() and adding a linear best fit using 
# geom_smooth() and changing the color to red
scatterplotMilesTreadDepth + geom_point(color = "dark grey") + 
  ggtitle("Tire Miles and Tread Depth Scatter Plot.")+
  geom_smooth(method = lm,
              level= 0,
              color = "red")
