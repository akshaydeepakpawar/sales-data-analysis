
# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)   # For handling missing values
library(readr)   # For reading CSV files

# Load the CSV file
df <- read.csv("Diwali Sales Data.csv", stringsAsFactors = FALSE)

# Check the dimensions of the dataset (rows and columns)
dim(df)

# View the first few rows of the dataset
head(df)

# Check the structure of the dataset
str(df)

# Get a summary of the dataset
summary(df)

# Remove rows where 'Amount' is missing
df <- df[!is.na(df$Amount), ]

# Remove unnecessary columns if there are any, e.g., unnamed1
df <- df %>% select(-unnamed1)

# Verify if any columns have missing values
colSums(is.na(df))

# Count sales by gender
df %>% 
  group_by(Gender) %>%
  summarize(Total_Amount = sum(Amount, na.rm = TRUE))

# Bar plot of sales by gender
ggplot(df, aes(x = Gender, y = Amount)) + 
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Total Sales by Gender", x = "Gender", y = "Sales Amount") +
  theme_minimal()

# Count sales by age group
df %>% 
  group_by(Age.Group) %>%
  summarize(Total_Amount = sum(Amount, na.rm = TRUE))

# Bar plot of sales by age group
ggplot(df, aes(x = Age.Group, y = Amount)) + 
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Total Sales by Age Group", x = "Age Group", y = "Sales Amount") +
  theme_minimal()

# Count sales by state
df %>% 
  group_by(State) %>%
  summarize(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
  arrange(desc(Total_Amount))

# Bar plot of sales by state
ggplot(df, aes(x = reorder(State, -Total_Amount), y = Total_Amount)) + 
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Total Sales by State", x = "State", y = "Sales Amount") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Count sales by occupation
df %>% 
  group_by(Occupation) %>%
  summarize(Total_Amount = sum(Amount, na.rm = TRUE))

# Bar plot of sales by occupation
ggplot(df, aes(x = Occupation, y = Amount)) + 
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Total Sales by Occupation", x = "Occupation", y = "Sales Amount") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Count sales by product category
df %>% 
  group_by(Product_Category) %>%
  summarize(Total_Amount = sum(Amount, na.rm = TRUE))

# Bar plot of sales by product category
ggplot(df, aes(x = Product_Category, y = Amount)) + 
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Total Sales by Product Category", x = "Product Category", y = "Sales Amount") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Total sales and total number of orders
total_sales <- sum(df$Amount, na.rm = TRUE)
total_orders <- sum(df$Orders, na.rm = TRUE)

cat("Total Sales: ", total_sales, "\nTotal Orders: ", total_orders)

