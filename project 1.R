
stocks <- read.csv("C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/ie 423 project/all_ticks_long.csv")
# Load necessary libraries (if not already loaded)
library(dplyr)

# Calculate the total number of unique timestamps for each stock
stock_data <- stocks %>%
  group_by(short_name) %>%
  summarize(total_timestamps = n_distinct(timestamp))

# Define the threshold for 2 years of data (adjust as needed)
threshold <- 365 * 26 *2  # 2 years of data

# Filter the stocks with at least 2 years of data
selected_stocks <- stock_data %>%
  filter(total_timestamps >= threshold)

# Select stocks from at least 3 different sectors
# You will need to know the sectors of the stocks in your data

# Randomly choose 6 stocks from the filtered list
selected_stocks <- selected_stocks %>%
  sample_n(6)

# Print the selected stocks
print(selected_stocks)

################################################################################

# Load necessary libraries (if not already loaded)
library(ggplot2)
library(dplyr)

# Convert the timestamp column to a Date object and extract the month
stocks <- stocks %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%SZ"),
         month = format(as.Date(timestamp), "%Y-%m"))

# Select data for the chosen indices
chosen_indices <- c("ISCTR")
selected_data <- stocks %>% filter(short_name %in% chosen_indices)


# Create boxplots 
ggplot(selected_data, aes(x = month, y = price, group = month)) +
  geom_boxplot() +
  labs(title = paste("Monthly Boxplots for Chosen Indices -", paste(chosen_indices, collapse = ", ")),
       x = "Month",
       y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 3 sigma Rules
# Load necessary libraries (if not already loaded)
library(dplyr)

# Convert the timestamp column to a Date object and extract the month
stocks <- stocks %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%SZ"),
         month = as.Date(timestamp) %>% format("%Y-%m"))

# Select data for the chosen indices
chosen_indices <- "AKBNK"
selected_data <- stocks %>% filter(short_name %in% chosen_indices)

# Calculate monthly mean and standard deviation for each series
outliers <- selected_data %>%
  group_by(short_name, month) %>%
  summarise(
    monthly_mean = mean(price),
    monthly_std = sd(price)
  ) %>%
  ungroup()

# Flag outliers for each series
outliers <- outliers %>%
  left_join(selected_data, by = c("short_name", "month")) %>%
  mutate(
    lower_limit = monthly_mean - 3 * monthly_std,
    upper_limit = monthly_mean + 3 * monthly_std,
    is_outlier = price < lower_limit | price > upper_limit
  )

library(ggplot2)

ggplot(outliers, aes(x = month, y = price, color = is_outlier)) +
  geom_point() +
  labs(title = paste("Outliers Based on 3-Sigma Rule -", chosen_indices),
       x = "Month",
       y = "Price",
       color = "Outlier") +
  scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  theme_minimal() +
  theme(legend.position = "top")




################################################################################





trends <- read.csv("C:/Users/Zeynep Sude Aksoy/Downloads/multiTimeline (4).csv")
# Load necessary libraries (if not already loaded)

# Load necessary libraries (if not already loaded)
library(ggplot2)

ggplot(data = trends, aes(x = Ay, y = IST.AKBNK)) +
  geom_point() +
  labs(x = "Month", y = "Search Volume", title = "Scatter Plot of Search Volume")




stocks <- read.csv("C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/ie 423 project/all_ticks_long.csv")
# Load necessary libraries (if not already loaded)
library(dplyr)
# Convert the timestamp column to a Date object and extract the month
stocks <- stocks %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%SZ"),
         month = format(timestamp, "%Y-%m"))

# Select data for the chosen indices
chosen_indices <- "AKBNK"
selected_data <- stocks %>% filter(short_name %in% chosen_indices)

# Calculate monthly mean for each series
mean_data <- selected_data %>%
  group_by(month) %>%
  summarise(
    monthly_mean = mean(price)
  ) %>%
  ungroup()



# Create a scatter plot of the monthly mean
ggplot(mean_data, aes(x = month, y = monthly_mean)) +
  geom_point() +  # Use geom_point for scatter plot
  labs(
    title = "Monthly Mean Price of AKBNK Stock ",
    x = "Month",
    y = "Mean Stock Price"
  ) +
  theme_minimal()



################################################################################



# Load necessary libraries (if not already loaded)
library(dplyr)
library(ggplot2)

# Load the stocks data
stocks <- read.csv("C:/Users/Zeynep Sude Aksoy/OneDrive/Masaüstü/ie 423 project/all_ticks_long.csv")

# Convert the timestamp column to a Date object and extract the month
stocks <- stocks %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%SZ"),
         month = format(timestamp, "%Y-%m"))

# Select data for the chosen indices
chosen_indices <- "ISCTR"
selected_data <- stocks %>% filter(short_name %in% chosen_indices)

# Calculate monthly mean for ISCTR stock index
outliers <- selected_data %>%
  group_by(month) %>%
  summarise(
    monthly_mean = mean(price)
  ) %>%
  ungroup()

# Load the trends data (modify the file path accordingly)
trends <- read.csv("C:/Users/Zeynep Sude Aksoy/Downloads/multiTimeline (1).csv")

# Merge the datasets based on the "month" column
combined_data <- left_join(outliers, trends, by = c("month" = "Ay"))

scatter_plot <- ggplot(combined_data) +
  geom_point(aes(x = month, y = monthly_mean, color = "Stock Data"), size = 3, na.rm = TRUE) +
  geom_point(aes(x = month, y = IST.ISCTR, color = "Google Trends Data"), size = 3, na.rm = TRUE) +
  labs(
    x = "Months",
    y = "Monthly Mean of Stock Data / Google Trends Data",
    title = "Scatter Plot of Stock Data and Search Volume Data from Google Trends"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Stock Data" = "red", "Google Trends Data" = "blue")) +
  guides(color = guide_legend(title = "Data Source"))

print(scatter_plot)