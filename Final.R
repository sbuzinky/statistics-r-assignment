# Load necessary libraries
library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(car)

# Load the dataset
data <- read_excel('ProjectDataConsolidated.xlsx')

# Summary of missing values per column
colSums(is.na(data))

# Check for exact duplicate rows
duplicates <- data[duplicated(data), ]

# Count duplicates
nrow(duplicates)

data <- data[!duplicated(data), ]

#Outlier Check
boxplot(data$price, main = "Boxplot of Price", xlab = "Price")
boxplot(data$carat, main = "Boxplot of Price", xlab = "Carat")
boxplot(data$depth, main = "Boxplot of Price", xlab = "Depth")
boxplot(data$table, main = "Boxplot of Price", xlab = "Table")

# Function to remove outliers using IQR method
remove_outliers_iqr <- function(df) {
  numeric_cols <- sapply(df, is.numeric)
  df_numeric <- df[, numeric_cols]
  
  for (col_name in names(df_numeric)) {
    Q1 <- quantile(df[[col_name]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col_name]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    df <- df %>% filter(.data[[col_name]] >= lower_bound & .data[[col_name]] <= upper_bound)
  }
  
  return(df)
}

# Apply to your data
data_clean <- remove_outliers_iqr(data)

head(data_clean)

# Convert categorical variables to factors
data_clean$cut <- as.factor(data_clean$cut)
data_clean$color <- as.factor(data_clean$color)
data_clean$clarity <- as.factor(data_clean$clarity)

# Check levels
levels(data_clean$clarity)
levels(data_clean$color)
levels(data_clean$cut)


# Descriptive statistics
summary(data_clean)
describe(data_clean)

# Histograms
ggplot(data_clean, aes(x=carat)) + geom_histogram(bins=30, fill="blue", alpha=0.6)
ggplot(data_clean, aes(x=depth)) + geom_histogram(bins=30, fill="green", alpha=0.6)
ggplot(data_clean, aes(x=table)) + geom_histogram(bins=30, fill="purple", alpha=0.6)
ggplot(data_clean, aes(x=price)) + geom_histogram(bins=30, fill="orange", alpha=0.6)

# Scatter plots
pairs(~ price + carat + depth + table, data=data_clean)

# Correlation matrix
cor(data_clean[sapply(data_clean, is.numeric)])



# Simple Linear Regressions
lm_carat <- lm(price ~ carat, data=data_clean)
summary(lm_carat)

lm_depth <- lm(price ~ depth, data=data_clean)
summary(lm_depth)

lm_table <- lm(price ~ table, data=data_clean)
summary(lm_table)

lm_cut <- lm(price ~ cut, data=data_clean)
summary(lm_cut)

lm_color <- lm(price ~ color, data=data_clean)
summary(lm_color)

lm_clarity <- lm(price ~ clarity, data=data_clean)
summary(lm_clarity)

#Main effects + interaction model
model_interaction <- lm(price ~ carat * cut, data = data_clean)
summary(model_interaction)

#Interaction only model
model_interaction2 <- lm(price ~ carat : cut, data = data_clean)
summary(model_interaction2)


# Multiple regression model
model1 <- lm(price ~ carat + depth + table + cut + color + clarity, data = data_clean)

# Model summary
summary(model1)

# Multicollinearity check
vif_values <- vif(model1)
print(vif_values)

# Filter to keep only significant levels of color and cut
filtered_data <- data_clean %>%
  filter(color %in% c("G", "H", "I", "J"),
         cut %in% c("Ideal", "Premium"))

# Drop unused levels
filtered_data$color <- droplevels(filtered_data$color)
filtered_data$cut <- droplevels(filtered_data$cut)

# Run the refined model
model_final <- lm(price ~ carat + cut + color + clarity, data = filtered_data)
summary(model_final)


# Multicollinearity check
vif_final <- vif(model_final)
print(vif_final)

# Residual analysis
plot(model_final$residuals, main = "Residual Plot", ylab = "Residuals", xlab = "Fitted Values")
qqnorm(model_final$residuals)
qqline(model_final$residuals, col = "red")


ggplot(data, aes(x = cut)) +
  geom_bar(fill = "skyblue") +
  theme_minimal() +
  ggtitle("Distribution of Cut")

ggplot(data, aes(x = cut, y = price)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue") +
  theme_minimal() +
  ggtitle("Average Price by Cut")


ggplot(data, aes(x = color)) +
  geom_bar(fill = "lightgreen") +
  theme_minimal() +
  ggtitle("Distribution of Color")

ggplot(data, aes(x = color, y = price)) +
  stat_summary(fun = mean, geom = "bar", fill = "lightgreen") +
  theme_minimal() +
  ggtitle("Average Price by Color")


ggplot(data, aes(x = clarity)) +
  geom_bar(fill = "salmon") +
  theme_minimal() +
  ggtitle("Distribution of Clarity")

ggplot(data, aes(x = clarity, y = price)) +
  stat_summary(fun = mean, geom = "bar", fill = "salmon") +
  theme_minimal() +
  ggtitle("Average Price by Clarity")


ggplot(data, aes(x = cut)) + geom_bar(fill = "skyblue") + theme_minimal() + ggtitle("Distribution of Cut")
ggplot(data, aes(x = color)) + geom_bar(fill = "lightgreen") + theme_minimal() + ggtitle("Distribution of Color")
ggplot(data, aes(x = clarity)) + geom_bar(fill = "salmon") + theme_minimal() + ggtitle("Distribution of Clarity")

