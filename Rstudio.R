# Load necessary libraries
library(tidyverse)

# Load the dataset
data <- read.csv("D:/medical_cost.csv")

# Display the first few rows of the dataset
head(data)

# Check for missing values
summary(is.na(data))

# Data cleaning
data <- na.omit(data) # Drop rows with missing values
data <- distinct(data) # Remove duplicate rows

# Display the last few rows of the dataset
tail(data)

# 1. How do age and gender affect insurance charges?
# Load necessary libraries
library(ggplot2)

# Read the dataset
data <- read.csv("D:/medical_cost.csv")

# Plot histogram of insurance charges by age and gender
ggplot(data, aes(x=age, y=charges, fill=sex)) +
  geom_histogram(stat="identity", position="dodge") +
  labs(title="Insurance Charges by Age and Gender", x="Age", y="Insurance Charges") +
  theme_minimal()

# 2. What is the relationship between BMI and insurance charges?
# Plot histogram of insurance charges by BMI
ggplot(data, aes(x=bmi, y=charges)) +
  geom_histogram(stat="identity") +
  labs(title="Insurance Charges by BMI", x="BMI", y="Insurance Charges") +
  theme_minimal()

# 3. What is the distribution of the number of children among individuals in the dataset? 
# Plot histogram of the number of children
ggplot(data, aes(x=children)) +
  geom_histogram(binwidth=1, fill="skyblue", color="black") +
  labs(title="Distribution of the Number of Children", x="Number of Children", y="Count") +
  theme_minimal()

# 4. Does the number of children impact insurance charges?
# Plot histogram of insurance charges by number of children
ggplot(data, aes(x=children, y=charges)) +
  geom_histogram(stat="identity") +
  labs(title="Impact of Number of Children on Insurance Charges", x="Number of Children", y="Insurance Charges") +
  theme_minimal()

# 5. Distribution of categorical variables?
# Plot distribution of categorical variables
library(gridExtra)

p1 <- ggplot(data, aes(x=sex)) + geom_bar(fill="skyblue") + labs(title="Distribution of Sex")
p2 <- ggplot(data, aes(x=smoker)) + geom_bar(fill="skyblue") + labs(title="Distribution of Smoker")
p3 <- ggplot(data, aes(x=region)) + geom_bar(fill="skyblue") + labs(title="Distribution of Region")

grid.arrange(p1, p2, p3, ncol=3)

# 6. Compare the distribution of charges between smokers and non-smokers to
# understand the financial impact of smoking on insurance costs?
# Plot distribution of charges for smokers and non-smokers
p1 <- ggplot(data[data$smoker == "yes",], aes(x=charges)) +
  geom_histogram(fill="red", bins=30) +
  labs(title="Insurance Charges for Smokers", x="Charges", y="Count")

p2 <- ggplot(data[data$smoker == "no",], aes(x=charges)) +
  geom_histogram(fill="green", bins=30) +
  labs(title="Insurance Charges for Non-Smokers", x="Charges", y="Count")

grid.arrange(p1, p2, ncol=2)

# 7. What is the distribution of individuals across different regions in the dataset?
# Plot distribution of individuals across regions
ggplot(data, aes(x=region)) +
  geom_bar(fill="skyblue") +
  labs(title="Distribution of Individuals Across Regions", x="Region", y="Count") +
  theme_minimal()

# 8. What insights can we derive from the pairwise relationships between age, BMI,
# number of children, and insurance charges?
# Load necessary library
library(GGally)

# Plot pairwise relationships
ggpairs(data[, c("age", "bmi", "children", "charges")],
        title="Pairwise Relationships Between Age, BMI, Number of Children, and Charges")

# 9. Is there evidence of interactions between variables (e.g., smoking and BMI) that impact charges? 
# Plot boxplot to see interaction between smoking status and BMI on charges
ggplot(data, aes(x=smoker, y=charges, fill=as.factor(bmi))) +
  geom_boxplot() +
  labs(title="Impact of Smoking and BMI on Insurance Charges", x="Smoker", y="Insurance Charges") +
  theme_minimal()
