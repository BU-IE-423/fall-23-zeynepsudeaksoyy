
library(tidyverse)
library(lmtest) # For linear regression
library(TTR) # For time-series analysis

# Set the directory where your CSV files are located
data_folder <- "~/Downloads/20180101_20231121_bist30/"
# List all CSV files in the directory
csv_files <- list.files(data_folder, pattern = "*.csv", full.names = TRUE)

# Read and combine all CSV files into a single data frame
combined_data <- lapply(csv_files, read.csv) %>%
  reduce(bind_rows)

# Assuming your combined_data has columns: timestamp, stock_identifier, stock_price
# Pivot the data to have each stock in its own column
reshaped_data <- combined_data %>%
  pivot_wider(names_from = short_name, values_from = price)
head(reshaped_data)

# Calculate correlation matrix for the stocks
correlation_matrix <- cor(reshaped_data[, -1])  # Exclude timestamp column


# Set correlation threshold
correlation_threshold <- 0.99  # Adjust this threshold as needed

# Get the indices where correlation is above the threshold, excluding self-correlations
highly_correlated_pairs <- which(correlation_matrix > correlation_threshold & correlation_matrix < 1, arr.ind = TRUE)

# Create an empty list to store selected pairs
selected_pairs <- list()


# Loop through the correlated pairs to select unique pairs
for (i in 1:nrow(highly_correlated_pairs)) {
  pair <- highly_correlated_pairs[i, ]
  stock1 <- rownames(correlation_matrix)[pair[1]]
  stock2 <- colnames(correlation_matrix)[pair[2]]
  
  # Check if the pair or its reversed version is already selected
  if (!(stock1 %in% unlist(selected_pairs) | stock2 %in% unlist(selected_pairs))) {
    selected_pairs[[length(selected_pairs) + 1]] <- c(stock1, stock2)
  }
}

# Print the selected pairs
selected_pairs

#[[1]]
#[1] "GARAN" "AKBNK"

# Set correlation threshold to different level because we need two pairs
correlation_threshold <- 0.98  # Adjust this threshold as needed

# Get the indices where correlation is above the threshold, excluding self-correlations
highly_correlated_pairs <- which(correlation_matrix > correlation_threshold & correlation_matrix < 1, arr.ind = TRUE)

# Create an empty list to store selected pairs
selected_pairs <- list()

# Loop through the correlated pairs to select unique pairs
for (i in 1:nrow(highly_correlated_pairs)) {
  pair <- highly_correlated_pairs[i, ]
  stock1 <- rownames(correlation_matrix)[pair[1]]
  stock2 <- colnames(correlation_matrix)[pair[2]]
  
  # Check if the pair or its reversed version is already selected
  if (!(stock1 %in% unlist(selected_pairs) | stock2 %in% unlist(selected_pairs))) {
    selected_pairs[[length(selected_pairs) + 1]] <- c(stock1, stock2)
  }
}

# Print the selected pairs
selected_pairs


# Assuming 'combined_data' is your dataset with columns for timestamp, stock_identifier, and stock_price

# Check if values for "KCHOL" or "THYAO" are missing for each timestamp
missing_data_either <- combined_data %>%
  filter(short_name %in% c("KCHOL", "THYAO")) %>%
  group_by(timestamp) %>%
  summarise(missing_count = sum(is.na(price)))

# Filter timestamps where either "KCHOL" or "THYAO" has missing values
timestamps_either_missing <- missing_data_either %>%
  filter(missing_count > 0) %>%
  pull(timestamp)

# Print timestamps where either "KCHOL" or "THYAO" has missing values
if (length(timestamps_either_missing) > 0) {
  cat("Timestamps where either 'KCHOL' or 'THYAO' has missing values:\n")
  print(timestamps_either_missing)
} else {
  cat("There are no timestamps where either 'KCHOL' or 'THYAO' has missing values.\n")
}

# Perform linear regression on the selected pair
lm_model_1 <- lm(KCHOL ~ THYAO, data = reshaped_data)

# Get residuals from the linear regression model
residuals_1 <- residuals(lm_model_1)

# Calculate standard deviation of residuals
residuals_sd_1 <- sd(residuals_1)

# Calculate upper and lower control limits (e.g., 3 standard deviations from mean)
upper_limit <- mean(residuals_1) + 3 * residuals_sd_1
lower_limit <- mean(residuals_1) - 3 * residuals_sd_1

# changing format of timestamp from ‘character’ to “POSIXct" "POSIXt" to plot the residuals.
new_format_timestamp = as.POSIXct(reshaped_data$timestamp, format = "%Y-%m-%d %H:%M:%S")

# Create a data frame for plotting
plot_data <- data.frame(timestamp = new_format_timestamp, residuals = residuals_1)

# Create a ggplot
ggplot(plot_data, aes(x = timestamp, y = residuals)) +
  geom_line() +
  labs(x = "Timestamp", y = "Residuals", title = "Residuals Plot with Control Limits") +
  geom_hline(yintercept = c(upper_limit, lower_limit), linetype = "dashed", color = c("red", "blue")) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  annotate("text", x = as.POSIXct(max(plot_data$timestamp)), y = upper_limit, label = "Upper Limit", hjust = 1.1, color = "red") +
  annotate("text", x = as.POSIXct(max(plot_data$timestamp)), y = lower_limit, label = "Lower Limit", hjust = 1.1, color = "blue") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(title = "Control Limits"))

#For second pair (GARAN and AKBNK)

# Assuming 'combined_data' is your dataset with columns for timestamp, stock_identifier, and stock_price

# Check if values for "GARAN" or "AKBNK" are missing for each timestamp
missing_data_either <- combined_data %>%
  filter(short_name %in% c("GARAN", "AKBNK")) %>%
  group_by(timestamp) %>%
  summarise(missing_count = sum(is.na(price)))

# Print timestamps where either "GARAN" or "AKBNK" has missing values
if (length(timestamps_either_missing) > 0) {
  cat("Timestamps where either 'GARAN' or 'AKBNK' has missing values:\n")
  print(timestamps_either_missing)
} else {
  cat("There are no timestamps where either 'GARAN' or 'AKBNK' has missing values.\n")
}


# Perform linear regression on the selected pair
lm_model_2 <- lm(GARAN ~ AKBNK, data = reshaped_data)

# Get residuals from the linear regression model
residuals_2 <- residuals(lm_model_2)


# Calculate standard deviation of residuals
residuals_sd_2 <- sd(residuals_2)

# Calculate upper and lower control limits (e.g., 2 standard deviations from mean)
upper_limit_2 <- mean(residuals_2) + 3 * residuals_sd_2
lower_limit_2 <- mean(residuals_2) - 3 * residuals_sd_2

# Create a data frame for plotting
plot_data_2 <- data.frame(timestamp = new_format_timestamp, residuals = residuals_2)

# Create a ggplot
ggplot(plot_data_2, aes(x = timestamp, y = residuals)) +
  geom_line() +
  labs(x = "Timestamp", y = "Residuals", title = "Residuals Plot with Control Limits") +
  geom_hline(yintercept = c(upper_limit_2, lower_limit_2), linetype = "dashed", color = c("red", "blue")) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  annotate("text", x = as.POSIXct(max(plot_data$timestamp)), y = upper_limit, label = "Upper Limit", hjust = 1.1, color = "red") +
  annotate("text", x = as.POSIXct(max(plot_data$timestamp)), y = lower_limit, label = "Lower Limit", hjust = 1.1, color = "blue") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(title = "Control Limits"))


# Define initial trading capital
initial_capital <- 1000000  

# Define the amount of shares to buy or sell based on the trading signal
shares_to_trade <- 1  # You can adjust this value

# Create an empty list to store trade information
trade_list <- list()

# Function to execute trades based on control chart signals
execute_trades <- function(timestamp1, stock1, stock2, residuals, upper_limit, lower_limit) {
  trade_info <- NULL  # Define trade_info within the function
  if (residuals > upper_limit) {
    # Sell stock1 and buy stock2
    action <- "SELL/BUY"
    capital_change <- (stock1 - stock2) * shares_to_trade
  } else if (residuals < lower_limit) {
    # Buy stock1 and sell stock2
    action <- "BUY/SELL"
    capital_change <- -(stock1 - stock2) * shares_to_trade
  } else {
    # No trade
    action <- "HOLD"
    capital_change <- 0
  }
  
  # Update trade_info
  initial_capital <<- initial_capital + capital_change
  trade_info <- list(timestamp1, stock1, stock2, action, capital_change)
  # Store trade information in the list
  trade_list[[length(trade_list) + 1]] <<- trade_info
}

# Apply trading simulation for the first pair
for (i in 1:length(residuals_1)) {
  execute_trades(reshaped_data$timestamp[i], reshaped_data$KCHOL[i], reshaped_data$THYAO[i], residuals_1[i], upper_limit, lower_limit)
}


# Create the data frame from the list
trades_df1 <- as.data.frame(do.call(rbind, trade_list))
colnames(trades_df1) <- c("Timestamp", "KCHOL", "THYAO", "Action", "CapitalChange")
# Filter out rows where the action is "HOLD"
trades_df1 <- subset(trades_df1, Action != "HOLD")
trades_df1
final_capital_1 <- initial_capital
final_capital_1

trade_list <- list()
initial_capital <- 1000000
# Apply trading simulation for the second pair
for (i in 1:length(residuals_2)) {
  execute_trades(reshaped_data$timestamp[i], reshaped_data$GARAN[i], reshaped_data$AKBNK[i], residuals_2[i], upper_limit_2, lower_limit_2)
}

# Create the data frame from the list for the second pair
trades_df2 <- as.data.frame(do.call(rbind, trade_list))
colnames(trades_df2) <- c("Timestamp", "GARAN", "AKBNK", "Action", "CapitalChange")
trades_df2 <- subset(trades_df2, Action != "HOLD")
trades_df2
final_capital_2 <- initial_capital
final_capital_2

#TASK 2

#For the pair KCHOL and THYAO:

library(ggplot2)
library(forecast)  # for ARIMA modeling and forecasts


# Function to fit ARIMA model to residuals and calculate new control limits
fit_arima_model <- function(residuals) {
  # Fit ARIMA model
  arima_model <- auto.arima(residuals)  # Adjust order as needed
  # Extract residuals from the ARIMA model
  arima_residuals <- residuals(arima_model)
  
  # Calculate upper and lower control limits (e.g., 3 standard deviations from mean)
  upper_limit <- mean(arima_residuals) + 3 * sd(arima_residuals)
  lower_limit <- mean(arima_residuals) - 3 * sd(arima_residuals)
  
  return(list(arima_model = arima_model, upper_limit = upper_limit, lower_limit = lower_limit))
}

# Apply ARIMA model and redefine control limits for the first pair
arima_results_1 <- fit_arima_model(residuals_1)

# Extract ARIMA model and new control limits
arima_model_1 <- arima_results_1$arima_model
upper_limit_arima_1 <- arima_results_1$upper_limit
lower_limit_arima_1 <- arima_results_1$lower_limit

plot_data_arima1 <- data.frame(
  timestamp = new_format_timestamp,
  residuals = residuals_1,
  upper_limit = upper_limit,
  lower_limit = lower_limit,
  upper_limit_arima = upper_limit_arima_1,
  lower_limit_arima = lower_limit_arima_1
)

ggplot(plot_data_arima1, aes(x = timestamp, y = residuals)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_limit, ymax = upper_limit), fill = "blue", alpha = 0.3, color = NA) +
  geom_ribbon(aes(ymin = lower_limit_arima_1, ymax = upper_limit_arima_1), fill = "red", alpha = 0.3, color = NA) +
  labs(x = "Timestamp", y = "Residuals", title = "Comparative Control Limits") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_manual(values = c("blue", "red"), name = "Limits", labels = c("Basic (Blue)", "ARIMA (Red)")) +
  guides(fill = guide_legend(title = NULL))

trade_list <- list()
initial_capital <- 1000000
# Apply trading simulation for the second pair
for (i in 1:length(residuals_1)) {
  
  execute_trades(reshaped_data$timestamp[i], reshaped_data$KCHOL[i], reshaped_data$THYAO[i], residuals_1[i], upper_limit_arima_1, lower_limit_arima_1)
}

# Create the data frame from the list for the second pair
trades_dfar1 <- as.data.frame(do.call(rbind, trade_list))
colnames(trades_dfar1) <- c("Timestamp", "KCHOL", "THYAO", "Action", "CapitalChange")
trades_dfar1 <- subset(trades_dfar1, Action != "HOLD")
trades_dfar1
final_capital_arima1 <- initial_capital
final_capital_arima1

# Apply ARIMA model and redefine control limits for the second pair
arima_results_2 <- fit_arima_model(residuals_2)

# Extract ARIMA model and new control limits
arima_model_2 <- arima_results_2$arima_model
upper_limit_arima_2 <- arima_results_2$upper_limit
lower_limit_arima_2 <- arima_results_2$lower_limit


#### Create a data frame for plotting using 95% prediction interval
plot_data_arima2 <- data.frame(
  timestamp = new_format_timestamp,
  residuals = residuals_2,
  upper_limit = upper_limit_2,
  lower_limit = lower_limit_2,
  upper_limit_arima = upper_limit_arima_2,
  lower_limit_arima = lower_limit_arima_2
)
ggplot(plot_data_arima2, aes(x = timestamp, y = residuals)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_limit, ymax = upper_limit), fill = "blue", alpha = 0.3, color = NA) +
  geom_ribbon(aes(ymin = lower_limit_arima_2, ymax = upper_limit_arima_2), fill = "red", alpha = 0.3, color = NA) +
  labs(x = "Timestamp", y = "Residuals", title = "Comparative Control Limits") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_manual(values = c("blue", "red"), name = "Limits", labels = c("Basic (Blue)", "ARIMA (Red)")) +
  guides(fill = guide_legend(title = NULL))

trade_list <- list()
initial_capital <- 1000000
# Apply trading simulation for the second pair
for (i in 1:length(residuals_2)) {
  execute_trades(reshaped_data$timestamp[i], reshaped_data$GARAN[i], reshaped_data$AKBNK[i], residuals_2[i], upper_limit_arima_2, lower_limit_arima_2)
}

# Create the data frame from the list for the second pair
trades_dfar2 <- as.data.frame(do.call(rbind, trade_list))
colnames(trades_dfar2) <- c("Timestamp", "GARAN", "AKBNK", "Action", "CapitalChange")
trades_dfar2 <- subset(trades_dfar2, Action != "HOLD")
trades_dfar2
final_capital_arima2 <- initial_capital
final_capital_arima2
