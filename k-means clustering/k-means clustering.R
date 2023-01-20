# Vihari Reddy Tummuru
# This code generates clusters using the k-means clustering
# for country-level data.

# Install the tidyverse and factoextra packages.
# install.packages("tidyverse")
# install.packages("factoextra")

# Loading the tidyverse, stats and factoextra libraries.
library(tidyverse)
library(stats)
library(factoextra)

# Loading cluster and gridExtra libraries
library(cluster)
library(gridExtra)

# Set the working directory to your Lab11 folder.
setwd("C:/Users/ual-laptop/OneDrive - University of Arizona/Desktop/R-545/Lab11")

# Read CountryData.csv into an object called countries.
countries <- read_csv(file = "CountryData.csv",
                      col_types = "cnnnnini",
                      col_names = TRUE)

# Display the countries tibble 
print(countries)

# Display the structure of countries tibble.
str(countries)

# Display a summary of countries tibble.
summary(countries)

# Converting the column containing the country name to the row title
countries <- countries %>% column_to_rownames(var = "Country")

# Remove countries from the tibble with missing data
countries <- countries %>% drop_na()

# Summary of the countries tibble
summary(countries)

# Create a new tibble called countriesScaled containing specific features
countriesScaled <- countries %>%
  select(CorruptionIndex, DaysToOpenBusiness)%>% 
  scale()

# Set the random seed
set.seed(679)

# Generate the k-means clusters 
countries4Clusters <- kmeans(x = countriesScaled,
                             centers = 4,
                             nstart = 25)

# Display cluster sizes on the console
print(countries4Clusters$size)

# Display cluster centers (z-scores) on the console
print(countries4Clusters$centers)

# Visualize the clusters
fviz_cluster(object = countries4Clusters,
             data = countriesScaled,
             repel = FALSE)

# Optimize the value for k by evaluating the elbow method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "wss")

# Optimize the value for k by evaluating the average silhouette method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "silhouette")

# Optimize the value for k by evaluating the gap statistic method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "gap_stat")

# Regenerate the cluster analysis using the optimal number of clusters
countries3Clusters <- kmeans(x = countriesScaled,
                             centers = 3,
                             nstart = 25)

# Display cluster sizes on the console
print(countries3Clusters$size)

# Display cluster centers (z-scores) on the console
print(countries3Clusters$centers)

# Visualize the clusters
fviz_cluster(object = countries3Clusters,
             data = countriesScaled,
             repel = FALSE)

# Determine similarities and differences among the clusters using the remaining
# features in the dataset
countries %>% 
  mutate(cluster = countries3Clusters$cluster) %>% 
  select(cluster,GiniCoefficient,GDPPerCapita,
         EduPercGovSpend,EduPercGDP,CompulsoryEducationYears) %>% 
  group_by(cluster) %>% 
  summarise_all("mean")

