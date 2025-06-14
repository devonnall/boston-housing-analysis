housing <- read.csv("housing.csv")
head(housing)

summary(housing)

library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

skewness = function(data, column) {
  col_data = as.numeric(data[[column]])
  n = length(col_data)
  mean = mean(col_data)
  std = sd(col_data)
  skew = (n*sum((col_data-mean)^3)) / ((n-1)*(n-2)*std^3)
  return(skew)
}

p1 <- ggplot(data=housing, aes(x=RM)) +
  geom_density(color="lightblue", fill = "lightblue") +
  scale_x_continuous(limits=c(3, 9)) +
  scale_y_continuous(limits=c(0, 1), expand = c(0, 0)) +
  labs(title=paste("RM, skewness is", round(skewness(housing, "RM"), 2))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(data=housing, aes(x=LSTAT)) +
  geom_density(color="lightblue", fill = "lightblue") +
  scale_x_continuous(limits=c(-5, 45)) + 
  scale_y_continuous(limits=c(0, 0.07), expand = c(0, 0)) +
  labs(title=paste("LSTAT, skewness is", round(skewness(housing, "LSTAT"), 2))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p3 <- ggplot(data=housing, aes(x=PTRATIO)) +
  geom_density(color="lightblue", fill = "lightblue") +
  scale_x_continuous(limits = c(10, 25)) +
  labs(title=paste("PTRATIO, skewness is", round(skewness(housing, "PTRATIO"), 2))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p4 <- ggplot(data=housing, aes(x=MEDV)) +
  geom_histogram(bins = 30, color="black", fill="lightblue", linewidth=0.2) +
  labs(title=paste("MEDV, skewness is", round(skewness(housing, "MEDV"), 2))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

(p1 | p2) / (p3 | p4)

## Outlier detection
remove_outliers <- function(data, columns = NULL) {
  # If no columns specified, use all numeric columns
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  }
  
  # Start with the original data
  clean_data <- data
  outlier_info <- list()
  
  # Process each specified column
  for (col in columns) {
    if (!col %in% names(data)) {
      warning(paste("Column", col, "not found in data frame"))
      next
    }
    
    if (!is.numeric(data[[col]])) {
      warning(paste("Column", col, "is not numeric, skipping"))
      next
    }
    
    # Calculate quartiles and IQR
    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    # Define outlier bounds
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Identify outliers
    outliers <- which(data[[col]] < lower_bound | data[[col]] > upper_bound)
    
    # Store outlier information
    outlier_info[[col]] <- list(
      count = length(outliers),
      indices = outliers,
      values = data[[col]][outliers],
      lower_bound = lower_bound,
      upper_bound = upper_bound
    )
    
    # Remove outliers (keep rows where value is within bounds or is NA)
    clean_data <- clean_data[is.na(data[[col]]) | 
                               (data[[col]] >= lower_bound & data[[col]] <= upper_bound), ]
  }
  
  # Print summary
  cat("Outlier Removal Summary:\n")
  cat("Original data shape:", nrow(data), "rows,", ncol(data), "columns\n")
  cat("Clean data shape:", nrow(clean_data), "rows,", ncol(clean_data), "columns\n")
  cat("Total rows removed:", nrow(data) - nrow(clean_data), "\n\n")
  
  for (col in names(outlier_info)) {
    info <- outlier_info[[col]]
    cat("Column", col, ":\n")
    cat("  Outliers found:", info$count, "\n")
    cat("  Bounds: [", round(info$lower_bound, 3), ",", round(info$upper_bound, 3), "]\n")
    if (info$count > 0) {
      cat("  Outlier range: [", round(min(info$values), 3), ",", round(max(info$values), 3), "]\n")
    }
    cat("\n")
  }
  
  # Return clean data with outlier info as attribute
  attr(clean_data, "outlier_info") <- outlier_info
  return(clean_data)
}

df = remove_outliers(housing)

# Train and test split
set.seed(123)
train_indices <- sample(1:nrow(df), size = 0.8*nrow(df))

train_data = df[train_indices, ]
test_data = df[-train_indices, ]

X_train <- train_data[, !names(train_data) %in% "MEDV"]
y_train <- train_data$MEDV
X_test <- test_data[, !names(test_data) %in% "MEDV"]
y_test <- test_data$MEDV

# Linear Regression
lm <- lm(y_train ~ ., data=X_train)
summary(lm)

train_predictions <- predict(lm, X_train)
test_predictions <- predict(lm, X_test)

train_r2 <- summary(lm)$r.squared
test_r2 <- cor(test_predictions, y_test, use = "complete.obs")^2

ggplot(mapping = aes(x = lm$fitted.values, y = lm$residuals)) + 
  geom_point(shape = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Fitted Values",
    y = "Residuals",
    title = "Residuals vs Fitted"
  ) +
  theme_minimal()


layout <- c(
  area(1, 1),
  area(1, 3),
  area(2, 2)
)

# Show the layout to make sure it looks as it should
plot(layout)
