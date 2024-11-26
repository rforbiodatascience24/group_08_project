library(tidyverse)
library(here)
library(rlang)

# cleaning data
clean_data <- function(dataframe) {
  # create dataframe
  data_clean <- dataframe |>
    # filter out the first 25 rows 
    filter(row_number() > 25) |> 
    # separate the header columns into different headers
    separate(col = `<Header>`, 
             into = c("code_class", "name", "accesion_number", "count"), 
             sep= ",") |> 
    # drop rows containing "NA"
    drop_na()
  
  
  return(data_clean)
}


# Filtering mean for no outliers
calculate_filtered_mean <- function(data) {
  # 1. Calculate quartiles and IQR
  q1 <- quantile(data, 0.25) # First quartile (Q1)
  q3 <- quantile(data, 0.75) # Third quartile (Q3)
  iqr <- q3 - q1             # Interquartile range (IQR)
  
  # 2. Define bounds for outliers
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  # 3. Filter out outliers
  filtered_data <- data[data >= lower_bound & data <= upper_bound]
  
  # 4. Calculate the mean of the filtered data
  mean_filtered <- round(mean(filtered_data, na.rm = TRUE), 2)
  
  # 5. Return results as a list
  return(mean_filtered)
}



# Function to replace outliers with NA for columns starting with a specific string and add bounds
replace_outliers_with_na_old <- function(data, prefix) {
  cols <- names(data)[startsWith(names(data), prefix)]
  
  data %>%
    rowwise() %>%
    mutate(
      lower_bound = quantile(c_across(all_of(cols)), 0.25, na.rm = TRUE) - 1.5 * IQR(c_across(all_of(cols)), na.rm = TRUE),
      upper_bound = quantile(c_across(all_of(cols)), 0.75, na.rm = TRUE) + 1.5 * IQR(c_across(all_of(cols)), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(across(all_of(cols), ~ ifelse(.x < lower_bound | .x > upper_bound, NA, .x)))
}

# Function to replace outliers with NA for columns starting with a specific string and add bounds
replace_outliers_with_na <- function(data, prefix) {
  # Identify columns with the specified prefix
  cols <- names(data)[startsWith(names(data), 
                                 prefix)]
  
  # Generate bound variable names based on the prefix
  lower_bound_name <- paste0("lower_bound_", prefix)
  upper_bound_name <- paste0("upper_bound_", prefix)
  
  # Perform row-wise outlier detection
  data |> 
    rowwise() |>
    mutate(
      # Dynamically create lower and upper bound variables with names including the prefix
      "{lower_bound_name}" := quantile(c_across(all_of(cols)), 
                                       0.25, 
                                       na.rm = TRUE) - 1.5 * IQR(c_across(all_of(cols)), 
                                                                 na.rm = TRUE),
      "{upper_bound_name}" := quantile(c_across(all_of(cols)), 
                                       0.75, 
                                       na.rm = TRUE) + 1.5 * IQR(c_across(all_of(cols)), 
                                                                 na.rm = TRUE),
      
      # Replace outliers with NA for all columns in cols
      across(
        all_of(cols),
        ~ ifelse(. < .data[[lower_bound_name]] | . > .data[[upper_bound_name]], NA, .)
      )
    ) |>
    ungroup()  # Remove rowwise grouping
}

replace_test_pre <- function(data) {
  pre_data <- data |> starts_with("pre_count")
  # 1. Calculate quartiles and IQR
  q1 <- quantile(pre_data, 0.25) # First quartile (Q1)
  q3 <- quantile(pre_data, 0.75) # Third quartile (Q3)
  iqr <- q3 - q1             # Interquartile range (IQR)
  
  # 2. Define bounds for outliers
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  # 3. Filter out outliers
  ifelse(pre_data < lower_bound | pre_data > upper_bound, NA, pre_data)
  
  # 5. Return results as a list
  return(mean_filtered)
}


filter_outliers_test <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  ifelse(x < lower_bound | x > upper_bound, NA, x)
}