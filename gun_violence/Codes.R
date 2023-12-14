# Author: Chiara Lu (261166959)
# Course: MGSC 661 - Mutivariate Statistical Analysis
# Section: 076

#Installations (ignore if you already have them)
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("RColorBrewer")
install.packages("quanteda")
install.packages("quanteda.textstats")
install.packages("quanteda.textplots")

#############################
##### Import libraries ######
#############################
library(ggplot2)
library(tidyverse)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(readr)
require(quanteda)
require(quanteda.textstats)
require(quanteda.textplots)
library(randomForest)
library(rpart)
library(rpart.plot)
library(gbm)


data = read_csv("/Users/chiaralu/Desktop/Courses/MGSC 661/Final Project/Dataset 1 — Gun violence.csv")
density = read_csv("/Users/chiaralu/Desktop/Courses/MGSC 661/Final Project/density2015-2019.csv")
attach(data)


##########################
##### Preprocessing ######
##########################

#1. Create new column with population density for each state
data$pop_density <- density$POP_DENSITY[match(data$state, density$NAME)]

#2. Create a column with the sum of killed and injured people 
data$n_killed[is.na(data$n_killed)] <- 0
data$n_injured[is.na(data$n_injured)] <- 0

data <- data %>%
  mutate(killed_and_injured = n_killed + n_injured)

#3. Create column with percentage of gun violence rate for each state
total_killed_and_injured <- sum(killed_and_injured, na.rm = TRUE)

data <- data %>%
  group_by(state) %>%
  mutate(violence_rate = sum(killed_and_injured) / total_killed_and_injured)

#4. Create month, season, and year columns
data$date <- as.Date(data$date)

data$month <- month(data$date, label = TRUE, abbr = TRUE)

#make 3 season dummies excluding spring
data$season_fall <- ifelse(data$month %in% c("Sep", "Oct", "Nov"), 1, 0)
data$season_winter <- ifelse(data$month %in% c("Dec", "Jan", "Feb"), 1, 0)
data$season_summer <- ifelse(data$month %in% c("Jun", "Jul", "Aug"), 1, 0)

data$year <- year(data$date)

#5. Make a copy of Min max scale pop_density, killed_and_injured, and n_guns_involved
min_max_scale <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

data$copy_pop_density <- data$pop_density
data$copy_killed_and_injured <- data$killed_and_injured
data$copy_n_guns_involved <- data$n_guns_involved

data$scaled_pop_density <- min_max_scale(data$copy_pop_density)
data$scaled_killed_and_injured <- min_max_scale(data$copy_killed_and_injured)
data$scaled_n_guns_involved <- min_max_scale(data$copy_n_guns_involved)

#6. Fill columns with 0 for n_guns_involved and killed_and_injured
data$n_guns_involved[is.na(data$n_guns_involved)] <- 0
data$killed_and_injured[is.na(data$killed_and_injured)] <- 0

#7. Remove outliers based on n_guns_involved and killed_and_injured
data <- data %>%
  filter(n_guns_involved < 50)

data <- data %>%
  filter(killed_and_injured < 100)



#################################
##### Exploratory Analysis ######
#################################

sum(data$n_killed)
sum(data$n_injured)
sum(data$killed_and_injured)

#1. Plot casualties by month and year 

ggplot(data, aes(x = month, y = killed_and_injured, group = 1)) +
  geom_bar(stat = "sum", fill = "brown") +
  labs(title = "Total Casualties by Month", x = "Month", y = "Total Killed and Injured") +
  theme_minimal()+
  theme(legend.position = "none")

ggplot(data, aes(x = year, y = killed_and_injured, group = 1)) +
  geom_line(stat = "summary", fun = sum, color = "brown") +
  labs(title = "Total Casualties by Year", x = "Year", y = "Total Killed and Injured") +
  theme_minimal() +
  ylim(0, 50000) +
  theme(legend.position = "none")


#2. Plot US map with violence incidences by state 

  #group by state
grouped_state <- data %>%
  group_by(state) %>%
  summarise(total_killed_and_injured = sum(killed_and_injured, na.rm = TRUE))

  #draw US map
map_data <- map_data('state')

  #capitalize region names to match names in grouped_state
map_data <- map_data %>%
  mutate(region = str_to_title(region))

  # Merge the injuries data with the map data
merged_data <- left_join(map_data, grouped_state, by = c("region" = "state"))

  # Plot the map using ggplot2
ggplot(merged_data, aes(x = long, y = lat, group = group, fill = total_killed_and_injured)) +
  geom_polygon(color = "white", size = 0.5) +
  scale_fill_viridis_c(name = "Casualties ranges", 
                       breaks = c(0, 4000, 8000, 12000, 16000, 20000),  
                       labels = c("0-4000", "4001-8000", "8001-12000", "12001-16000", "16001-20000", "20000+"),
                       na.value = "grey", guide = "legend") +
                       theme_minimal() +
                        labs(title = "Total Casualties by State") +
                        theme(axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank()) +
                        theme(legend.position = "right") +
                        coord_fixed(ratio = 1.5)

#.3. Text Analytics on most frequent locations for gun violence

toks_location <- tokens(data$location_description, remove_punct = TRUE) %>% 
  tokens_keep(pattern = "^[A-Z]", 
              valuetype = "regex", 
              case_insensitive = FALSE, 
              padding = TRUE)
dfmat_location <- dfm(toks_location)
tstat_freq <- textstat_frequency(dfmat_location, n = 5, groups = data$location_description)

# Create a new data frame with the "group" and "frequency" columns
tstat_freq2 <- data.frame(group = tstat_freq$group, frequency = tstat_freq$frequency)

toks_location <- tokens(data$location_description, remove_punct = TRUE)

dfmat_location <- dfm(toks_location)

tstat_freq <- textstat_frequency(dfmat_location, n = 100)

# Create a new data frame with the "feature" and "frequency" columns
tstat_freq3 <- data.frame(feature = tstat_freq$feature, frequency = tstat_freq$frequency)

#drop those features with less than 3 characters 
tstat_freq3 <- tstat_freq3 %>%
  filter(nchar(feature) > 3)

tstat_freq3 <- tstat_freq3[order(-tstat_freq3$frequency), ]


# Arrange the filtered df by frequency in descending order
tstat_freq_filtered <- tstat_freq_filtered[order(-tstat_freq_filtered$frequency), ]

# Filter out meaningless features ('high', 'west')
tstat_freq_filtered <- tstat_freq3[tstat_freq3$feature != 'high', ]
tstat_freq_filtered <- tstat_freq3[tstat_freq3$feature != 'west', ]

# Select the top 10 features
top_10_features <- head(tstat_freq_filtered, 10)

# Create a vertical bar chart of the top 10 features
ggplot(top_10_features, aes(x =frequency , y = reorder(factor(feature, levels = top_10_features$feature), -frequency))) +
  geom_bar(stat = "identity", fill = "brown") +
  geom_text(aes(label = frequency), hjust = -0.11, vjust = 0.5, color = "black", size = 3) +
  labs(title = "Top 10 Locations by Frequency", x = "Frequency", y = "Locations")+
  theme_minimal() +  
  theme(axis.text.y = element_text(angle = 0, hjust = 1), plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))


#4. Before and after comparison of number of guns and 
plot(data$n_guns_involved, data$killed_and_injured, col = "brown", xlab = "Number of Guns Involved", ylab = "Total Casualties", main = "Before")
plot(data$n_guns_involved, data$killed_and_injured, col = "brown", xlab = "Number of Guns Involved", ylab = "Total Casualties", main = "After removing outliers")


#5. Kmeans clustering of population density and casualties

data$log_pop_density <- log(data$pop_density)
data$log_killed_and_injured <- log(data$killed_and_injured)


#k-means clustering
set.seed(123)

# Replace infinite values with 0 in the log_killed_and_injured column
data$log_killed_and_injured <- replace(data$log_killed_and_injured, is.infinite(data$log_killed_and_injured), 0)

kmeans <- kmeans(data[, c("log_pop_density", "log_killed_and_injured")], centers = 3) #change number to see other clutsers
data$cluster <- as.factor(kmeans$cluster)
attach(data)

#plot clusters
ggplot(data, aes(x = log_pop_density, y = log_killed_and_injured, color = cluster)) +
  geom_point() +
  labs(title = "Population Density vs. Casualties",
       x = "Population Density",
       y = "Number of Casualties") +
  theme_minimal()

#correlation between log_pop_density and log_killed_and_injured
cor(data$log_pop_density, data$log_killed_and_injured)

##########################
##### Models Tested ###### 
##########################

                            #Classification Tree Model

set.seed(123)
train_indices <- sample(nrow(data_copy), nrow(data_copy) * 0.70)
train_tree <- data_copy[train_indices, ]
test_tree <- data_copy[-train_indices, ]

classifiedtree <- rpart(status4 ~ pop_density + violence_rate + season_fall + season_winter + season_summer + year, 
                        data = train_tree,
                        method = "class")

predictions <- predict(classifiedtree, newdata = test_tree, type = "class")

accuracy <- sum(predictions == test_tree$status4) / nrow(test_tree)
cat("Test set accuracy:", accuracy, "\n")

#confusion matrix
conf_matrix_tree <- table(predictions, test_tree$status4)
conf_matrix_tree


                                    # Random Forest with boosting 

data_copy$status4 <- ifelse(data_copy$n_killed > 0 & data_copy$n_injured > 0, "Alert",
                            ifelse(data_copy$n_killed > 0, "Alert",
                                   ifelse(data_copy$n_injured > 0, "Alert", "safe")))

data_copy$status4 <- as.factor(data_copy$status4)

set.seed(123)

train_indices <- sample(nrow(data_copy), nrow(data_copy) * 0.7)

# Create train and test
train_boost <- data_copy[train_indices, ]
test_boost <- data_copy[-train_indices, ]

# Fit the Boosted Random Forest model on the training data
boost <- gbm(status4 ~ pop_density + violence_rate + n_guns_involved + season_fall + season_winter + season_summer + year + longitude + latitude,
             data = train_boost,
             distribution = "multinomial",
             n.trees = 100,
             interaction.depth = 5,
             shrinkage = 0.1,
             verbose = TRUE)

# Summary of the model
summary(boost)

# Make predictions on the test data
predictions <- predict(boost, test_boost, n.trees = 100, type = "response")

# Convert the predicted probabilities to class labels
predicted_classes <- colnames(predictions)[apply(predictions, 1, which.max)]

# Create a confusion matrix
confusion_matrix <- table(test_boost$status4, predicted_classes)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
confusion_matrix

# Calculate the accuracy, precision, and recall
cat("Test set accuracy:", accuracy, "\n")
cat("Test set precision:", confusion_matrix[2, 2] / sum(confusion_matrix[, 2]), "\n")
cat("Test set recall:", confusion_matrix[2, 2] / sum(confusion_matrix[2, ]), "\n")

print(conf_matrix)

########################
##### Final Model ######
########################

data_copy <- data 

data_copy$status4 <- ifelse(data_copy$n_killed > 0 & data_copy$n_injured > 0, "alert",
                            ifelse(data_copy$n_killed > 0, "alert",
                                   ifelse(data_copy$n_injured > 0, "alert", "safe")))

data_copy$status4 <- as.factor(data_copy$status4)

set.seed(123)

train_indices <- sample(nrow(data_copy), nrow(data_copy) * 0.7)

# Create train and test
train <- data_copy[train_indices, ]
test <- data_copy[-train_indices, ]

# Train the random forest model on the training set
myrandomforest <- randomForest(
  status4 ~ pop_density + violence_rate + n_guns_involved + season_fall + season_winter + season_summer + year + latitude + longitude,
  data = train,
  na.action = na.omit,
  ntree = 300,  
  maxdepth = 7,
  importance = TRUE
)

# Predict on the test set
predictions <- predict(myrandomforest, newdata = test)

# Confusion matrix to evaluate accuracy
conf_matrix <- table(predictions, test$status4)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Print accuracy, precision, and recall
cat("Test set accuracy:", accuracy, "\n")
cat("Test set precision:", conf_matrix[2, 2] / sum(conf_matrix[, 2]), "\n")
cat("Test set recall:", conf_matrix[2, 2] / sum(conf_matrix[2, ]), "\n")

# Print confusion matrix
print(conf_matrix)

#Importance of the variables
importance_values <- importance(myrandomforest)
print(importance_values)

# Plot variable importance
varImpPlot(myrandomforest, main = "Variable Importance")



