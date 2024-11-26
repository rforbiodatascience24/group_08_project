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

