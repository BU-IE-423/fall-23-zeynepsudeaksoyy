
R version 4.3.1 (2023-06-16) -- "Beagle Scouts"
Copyright (C) 2023 The R Foundation for Statistical Computing
Platform: aarch64-apple-darwin20 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[R.app GUI 1.79 (8238) aarch64-apple-darwin20]

[Workspace restored from /Users/hc/.RData]
[History restored from /Users/hc/.Rapp.history]

> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

> stocks = read.csv(‘~/Downloads/all_ticks_long.csv.gz’)
Error: unexpected input in "stocks = read.csv(‘"
> library(dplyr)
> stocks = read.csv("~/Downloads/all_ticks_long.csv.gz")
> # Calculate the total number of unique timestamps for each 
> stock_data <- stocks %>%
+   group_by(short_name) %>%
+   summarize(total_timestamps = n_distinct(timestamp))
> # Define the threshold for 2 years of data (adjust as needed)
> threshold <- 365 * 26 * 2  # 2 years of data
> 
> 
> # Filter the stocks with at least 2 years of data
> selected_stocks <- stock_data %>%
+   filter(total_timestamps >= threshold)
> 
> 
> # Select stocks from at least 3 different sectors
> # You will need to know the sectors of the stocks in your data
> 
> # Randomly choose 6 stocks from the filtered list
> selected_stocks <- selected_stocks %>%
+   sample_n(6)
> 
> # Print the selected stocks
> print(selected_stocks)
# A tibble: 6 × 2
  short_name total_timestamps
  <chr>                 <int>
1 HALKB                 49071
2 KCHOL                 49093
3 VESTL                 48781
4 TUPRS                 49143
5 ICBCT                 44336
6 ASELS                 48803
> 
> # Install and load necessary libraries if not already installed
> library(ggplot2)
> 
> # Assuming 'stocks' contains your data
> 
> # Convert the timestamp column to a Date object and extract the month
> stocks <- stocks %>%
+   mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%SZ"),
+          month = as.Date(timestamp) %>% format("%Y-%m"))
> 
> # Filter the data for the “BANVT” stock banvt_data <- selected_data[selected_data$short_name == "BANVT", ]
> selected_data <- stocks %>% filter(short_name %in% chosen_indices)
> # Calculate monthly mean and standard deviation for each stock
> outliers <- selected_data %>%
+   group_by(short_name, month) %>%
+   summarise(
+     mean_price = mean(price),
+     sd_price = sd(price)
+   ) %>%
+   ungroup() %>%
+   mutate(
+     lower_bound = mean_price - 3 * sd_price,
+     upper_bound = mean_price + 3 * sd_price
+   )
`summarise()` has grouped output by 'short_name'. You can override using the
`.groups` argument.
> 
> # Identify and flag outliers
> selected_data <- selected_data %>%
+   left_join(outliers, by = c("short_name", "month")) %>%
+   mutate(outlier = price < lower_bound | price > upper_bound)
>  
Error: unexpected input in " "
> # Calculate monthly mean and standard deviation for each series
> outliers <- selected_data %>%
+   group_by(short_name, month) %>%
+   summarise(
+     monthly_mean = mean(price),
+     monthly_std = sd(price)
+   ) %>%
+   ungroup()
`summarise()` has grouped output by 'short_name'. You can override using the
`.groups` argument.
>  
Error: unexpected input in " "
> # Flag outliers for each series
> outliers <- outliers %>%
+   left_join(selected_data, by = c("short_name", "month")) %>%
+   mutate(
+     lower_limit = monthly_mean - 3 * monthly_std,
+     upper_limit = monthly_mean + 3 * monthly_std,
+     is_outlier = price < lower_limit | price > upper_limit
+   )
> 
> library(ggplot2)
> 
> ggplot(outliers, aes(x = month, y = price, color = is_outlier)) +
+   geom_point() +
+   labs(title = paste("Outliers Based on 3-Sigma Rule -", chosen_indices),
+        x = "Month",
+        y = "Price",
+        color = "Outlier") +
+   scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
+   theme_minimal() +
+   theme(legend.position = "top")
Error in UseMethod("depth") : 
  no applicable method for 'depth' applied to an object of class "NULL"
> 
> # Calculate monthly mean and standard deviation for each stock
> outliers <- selected_data %>%
+   group_by(short_name, month) %>%
+   summarise(
+     mean_price = mean(price),
+     sd_price = sd(price)
+   ) %>%
+   ungroup() %>%
+   mutate(
+     lower_bound = mean_price - 3 * sd_price,
+     upper_bound = mean_price + 3 * sd_price
+ )
`summarise()` has grouped output by 'short_name'. You can override using the
`.groups` argument.
> # Identify and flag outliers
> selected_data <- selected_data %>%
+   left_join(outliers, by = c("short_name", "month")) %>%
+   mutate(outlier = price < lower_bound | price > upper_bound)
Error in `mutate()`:
ℹ In argument: `outlier = price < lower_bound | price > upper_bound`.
Caused by error:
! object 'lower_bound' not found
Run `rlang::last_trace()` to see where the error occurred.
>  
Error: unexpected input in " "
> 
> stocks <- stocks %>%
+   mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%SZ"),
+          month = as.Date(timestamp) %>% format("%Y-%m"))
> 
> # Filter the data for the “BANVT” stock banvt_data <- selected_data[selected_data$short_name == "BANVT", ]
> selected_data <- stocks %>% filter(short_name %in% chosen_indices)
> 
> # Calculate monthly mean and standard deviation for each stock
> outliers <- selected_data %>%
+   group_by(short_name, month) %>%
+   summarise(
+     mean_price = mean(price),
+     sd_price = sd(price)
+   ) %>%
+   ungroup() %>%
+   mutate(
+     lower_bound = mean_price - 3 * sd_price,
+     upper_bound = mean_price + 3 * sd_price
+   )
`summarise()` has grouped output by 'short_name'. You can override using the
`.groups` argument.
> # Identify and flag outliers
> selected_data <- selected_data %>%
+   left_join(outliers, by = c("short_name", "month")) %>%
+   mutate(outlier = price < lower_bound | price > upper_bound)
> # Create a scatter plot for “BANVT” with thinner and more distinguishable dots
> p_banvt <- ggplot(banvt_data, aes(x = month, y = price, color = outlier)) +
+   geom_point(size = 1) +  # Adjust the size to make the dots thinner
+   labs(title = "Scatter Plot for BANVT with Outliers Highlighted",
+        x = "Month",
+        y = "Price") +
+   scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
+   theme(axis.text.x = element_text(angle = 45, hjust = 1))
Error: object 'banvt_data' not found
> banvt_data <- selected_data[selected_data$short_name == "BANVT", ]
> # Create a scatter plot for “BANVT” with thinner and more distinguishable dots
> p_banvt <- ggplot(banvt_data, aes(x = month, y = price, color = outlier)) +
+   geom_point(size = 1) +  # Adjust the size to make the dots thinner
+   labs(title = "Scatter Plot for BANVT with Outliers Highlighted",
+        x = "Month",
+        y = "Price") +
+   scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
+   theme(axis.text.x = element_text(angle = 45, hjust = 1))
> 
> 
> print(p_banvt)
Error in UseMethod("depth") : 
  no applicable method for 'depth' applied to an object of class "NULL"
> # Assuming 'selected_data' contains your data with outliers flagged
> 
> # Filter the data for the “BRISA” stock brisa_data <- selected_data[selected_data$short_name == "BRISA", ]
> 
> # Create a scatter plot for “BRISA” with thinner and more distinguishable dots
> p_brisa <- ggplot(brisa_data, aes(x = month, y = price, color = outlier)) +
+   geom_point(size = 1) +  # Adjust the size to make the dots thinner
+   labs(title = "Scatter Plot for BRISA with Outliers Highlighted",
+        x = "Month",
+        y = "Price") +
+   scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
+   theme(axis.text.x = element_text(angle = 45, hjust = 1))
Error: object 'brisa_data' not found
> 
> # Display the plot
> print(p_brisa)
Error: object 'p_brisa' not found
> # Create a scatter plot for “BRISA” with thinner and more distinguishable dots
> p_brisa <- ggplot(brisa_data, aes(x = month, y = price, color = outlier)) +
+   geom_point(size = 1) +  # Adjust the size to make the dots thinner
+   labs(title = "Scatter Plot for BRISA with Outliers Highlighted",
+        x = "Month",
+        y = "Price") +
+   scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
+   theme(axis.text.x = element_text(angle = 45, hjust = 1))
Error: object 'brisa_data' not found
> 
> brisa_data <- selected_data[selected_data$short_name == "BRISA", ]
> # Create a scatter plot for “BRISA” with thinner and more distinguishable dots
> p_brisa <- ggplot(brisa_data, aes(x = month, y = price, color = outlier)) +
+   geom_point(size = 1) +  # Adjust the size to make the dots thinner
+   labs(title = "Scatter Plot for BRISA with Outliers Highlighted",
+        x = "Month",
+        y = "Price") +
+   scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
+   theme(axis.text.x = element_text(angle = 45, hjust = 1))
> # Display the plot
> #print(p_brisa)
> # Display the plot
> print(p_brisa)
Error in UseMethod("depth") : 
  no applicable method for 'depth' applied to an object of class "NULL"
> # Filter the data for the “AKSA” stock
> aksa_data <- selected_data[selected_data$short_name == “AKSA”, ]
Error: unexpected input in "aksa_data <- selected_data[selected_data$short_name == “"
> aksa_data <- selected_data[selected_data$short_name == "AKSA", ]
> # Create a scatter plot for “AKSA” with thinner and more distinguishable dots
> 
> p_aksa <- ggplot(aksa_data, aes(x = month, y = price, color = outlier)) +
+   geom_point(size = 1) +  # Adjust the size to make the dots thinner
+   labs(title = "Scatter Plot for AKSA with Outliers Highlighted",
+        x = "Month",
+        y = "Price") +
+   scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
+   theme(axis.text.x = element_text(angle = 45, hjust = 1))
> # Display the plot
> print(p_aksa)
Error in UseMethod("depth") : 
  no applicable method for 'depth' applied to an object of class "NULL"
> # For tavuk_data 
> tavuk_data = read.csv('~/Downloads/tavuk.csv', sep = ';', header = FALSE, stringsAsFactors = FALSE) 
Error: unexpected input in "tavuk_data = read.csv('~/Downloads/tavuk.csv', sep = ';', header = FALSE, stringsAsFactors = FALSE) "
> tavuk_data = read.csv('~/Downloads/tavuk.csv', sep = ';', header = FALSE, stringsAsFactors = FALSE) 
Error: unexpected input in "tavuk_data = read.csv('~/Downloads/tavuk.csv', sep = ';', header = FALSE, stringsAsFactors = FALSE) "
> tavuk_data = read.csv('~/Downloads/tavuk.csv', sep = ';', header = FALSE, stringsAsFactors = FALSE)
> # remove the unnecessary parts
> 
> tavuk_data <- tavuk_data[-c(1, 2,3), ]
> ggplot(tavuk_data, aes(x = V1, y = V2)) +
+   geom_point() +
+   labs(title = "Scatter Plot of Tavuk Data Over Time", x = "Date", y = "Value") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
> 
> 
> # For banvit_data
> banvit_data = read.csv('~/Downloads/banvit.csv', sep = ';', header = FALSE, stringsAsFactors = FALSE)
> 
> # remove the unnecessary parts
> banvit_data <- banvit_data[-c(1, 2,3), ]
> # check if there is NA values in the data, if there is make them 0
> 
> banvit_data$V2 <- ifelse(is.na(banvit_data$V2), 0, banvit_data$V2)
> # convert categorical data into numerical data
> banvit_data$V2 <- as.numeric(banvit_data$V2)
> ggplot(banvit_data, aes(x = V1, y = V2)) +
+   geom_point() +
+   labs(title = "Banvit Search Over Time", x = "Date", y = "Value") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
> # For IST_BANVT_data
> 
> IST_BANVT_data = read.csv('~/Downloads/IST-BANVT.csv', sep = ';', header = FALSE, stringsAsFactors = FALSE)
> # remove the unnecessary parts
> IST_BANVT_data <- IST_BANVT_data[-c(1, 2,3), ]
> 
> # check if there is NA values in the data, if there is make them 0
> IST_BANVT_data$V2 <- ifelse(is.na(IST_BANVT_data$V2), 0, IST_BANVT_data$V2)
> # convert categorical data into numerical data
> IST_BANVT_data$V2 <- as.numeric(IST_BANVT_data$V2)
> ggplot(IST_BANVT_data, aes(x = V1, y = V2)) +
+   geom_point() +
+   labs(title = “IST:BANVT Search Over Time", x = "Date", y = "Value") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
Error: unexpected input in:
"  geom_point() +
  labs(title = “"
> 
> ggplot(IST_BANVT_data, aes(x = V1, y = V2)) +
+   geom_point() +
+   labs(title = "IST:BANVT Search Over Time", x = "Date", y = "Value") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
> 
> # For BRISA_data
> brisa_search = read.csv('~/Downloads/brisa.csv', sep = ';', header = FALSE, stringsAsFactors = FALSE)
> brisa_search <- brisa_search[-c(1, 2,3), ]
> 
> ggplot(brisa_search, aes(x = V1, y = V2)) +
+   geom_point() +   labs(title = "'Brisa' Search Over Time", x = "Date", y = "Value") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
> 
> # For akrilik_data
> akrilik_data = read.csv('~/Downloads/akrilik.csv', sep = ';', header = FALSE, stringsAsFactors = FALSE)
> # remove the unnecessary parts
> akrilik_data <- akrilik_data[-c(1, 2,3), ]
> ggplot(akrilik_data, aes(x = V1, y = V2)) +
+   geom_point() +   labs(title = "'akrilik' Search Over Time", x = "Date", y = "Value") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
> # For lastik_data
> lastik_data = read.csv('~/Downloads/lastik.csv', sep = ';', header = FALSE, stringsAsFactors = FALSE)
> # remove the unnecessary parts
> lastik_data <- lastik_data[-c(1, 2,3), ]
> ggplot(lastik_data, aes(x = V1, y = V2)) +
+   geom_point() +   labs(title = "'lastik' Search Over Time", x = "Date", y = "Value") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
> 
> # For IST:AKSA_data
> IST_AKSA_data = read.csv('~/Downloads/IST-AKSA.csv’, sep = ';', header = FALSE, stringsAsFactors = FALSE)
Error: unexpected ';' in "IST_AKSA_data = read.csv('~/Downloads/IST-AKSA.csv’, sep = ';"
> 
> # For IST:AKSA_data
> IST_AKSA_data = read.csv('~/Downloads/IST-AKSA.csv', sep = ';', header = FALSE, stringsAsFactors = FALSE)
> # remove the unnecessary parts
> IST_AKSA_data <- IST_AKSA_data[-c(1, 2,3), ]
> # check if there is NA values in the data, if there is make them 0
> IST_AKSA_data$V2 <- ifelse(is.na(IST_AKSA_data$V2), 0, IST_AKSA_data$V2)
> # convert categorical data into numerical data
> IST_AKSA_data$V2 <- as.numeric(IST_AKSA_data$V2)
Warning message:
NAs introduced by coercion 
> IST_AKSA_data$V2 <- ifelse(is.na(IST_AKSA_data$V2), 0, IST_AKSA_data$V2)
> # convert categorical data into numerical data
> IST_AKSA_data$V2 <- as.numeric(IST_AKSA_data$V2)
> 
> ggplot(IST_AKSA_data, aes(x = V1, y = V2)) +
+   geom_point() +   labs(title = "'IST:AKSA' Search Over Time", x = "Date", y = "Value") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

 *** caught segfault ***
address 0x5162dfbf0a60, cause 'invalid permissions'

Possible actions:
1: abort (with core dump, if enabled)
2: normal R exit
3: exit R without saving workspace
4: exit R saving workspace
> 
Selection: 