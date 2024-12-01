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
  lower_bound_name <- str_c("lower_bound_", prefix)
  upper_bound_name <- str_c("upper_bound_", prefix)
  
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

# four different plots showing the difference in expression before and after treatment.
# chose four genes with a significant p-value
significant_gene <- function(dataframe, chosen_gene) {
  resulting_df <- data_frame |>
  filter(gene == chosen_gene) |> # filtering for the wanted gene
  ggplot(aes(x = treatment, 
             y = count_value)) +
  geom_point() + # scatter plot
  theme_minimal() +
  ggtitle(chosen_gene) +
  theme(axis.title.x = element_blank(), # remove the x- and y axis legends
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, # move the title of each plot to the middle
                                  size = 10)) # adjust the size of the title of each plot
  return(resulting_df)
}
  

