
############## Downloads data from data aggregator ----
## Load libraries without messages
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(mongolite)
})

## Key parameters which vary according to the project
### Keywords to filter form titles (adjust for each project)
title_keywords <- paste("Project ABC")

### Title of the forms for subsetting (adjust for each project)
form_titles <- "Output|Feedback|Financial"

## Source credentials for KoboToolbox (adjust for each project)
source("scripts/login_credentials.R", encoding = "UTF-8")

# Download data from KoboToolbox
## Create parameters of reference
kobo_api_url <- "https://kc.humanitarianresponse.info/api/v1/"

# Authenticate with KoboToolbox API
auth <- httr::authenticate(user_kobo, password_kobo, type = "basic")

# Remove credentials
rm(user_kobo, password_kobo)

# Get form IDs
## Get list of deployed KoboToolbox forms
response_forms <- httr::GET(
  url = paste0(kobo_api_url, "forms"),
  auth
)

# Check if the request was successful
if (httr::status_code(response_forms) == 200) {
  # Parse JSON data and convert it to a data frame
  forms <- jsonlite::fromJSON(
    httr::content(response_forms, as = "text", encoding = "UTF8"),
    flatten = TRUE
  )
} else {
  cat("Error:", status_code(response_forms), "\n")
  cat("Unable to download form list from KoboToolbox\n")
}

# Filter forms based on keywords (AND condition) using dplyr
relevant_forms <- forms %>%
  # Filter project-relevant forms
  filter(grepl(title_keywords, title)) %>%
  # Filter only using key parameters
  filter(grepl(form_titles, title)) %>%
  # Arrange forms by number
  arrange(title)

# DATA and FORM QUESTIONS -----
# Initialise an empty list to store the dataframes for data and form questions
data_list <- list()
form_list <- list()

# Iterate over relevant forms
for (i in seq_along(relevant_forms$formid)) {
  form_id <- relevant_forms$formid[i]
  
  # Get data from KoboToolbox form
  response_data <- httr::GET(
    url = paste0(kobo_api_url, "data/", form_id),
    auth
  )
  
  # Get forms for KoboToolbox form
  response_form <- httr::GET(
    url = paste0(kobo_api_url, "forms/", form_id, "/form.json"),
    auth
  )
  
  # If the data and form responses are successful, store the data in the lists
  if (
    httr::status_code(response_data) == 200 &&
    httr::status_code(response_form) == 200) {
    data <- jsonlite::fromJSON(
      httr::content(response_data, as = "text", encoding = "UTF8"),
      flatten = TRUE
    )
    data_list[[i]] <- data
    
    form_questions <- jsonlite::fromJSON(
      httr::content(response_form, as = "text", encoding = "UTF8"),
      flatten = TRUE
    )
    form_list[[i]] <- form_questions
  } else {
    cat("Error with form ID", form_id, "\n")
    cat("Data response:", status_code(response_data), "\n")
    cat("Form questions response:", status_code(response_form), "\n")
  }
  # Remove temporary variables
  rm(form_id, response_data, i, form_questions)
}

################################
# Clean raw data
## Function to clean variable names (requires stringr package)
clean_variable_names <- function(df) {
  colnames(df) <- colnames(df) %>%
    stringr::str_replace_all("^_+|_+$", "") %>%
    # Remove starting and ending underscores
    stringr::str_replace_all("/", "_") %>%
    # Replace slashes with underscores
    stringr::str_replace_all("__+", "_")
  # Remove double underscores
  return(df)
}

# Function to convert date and date-time columns
convert_datetime_columns <- function(df) {
  # Define column name patterns for date and date-time columns
  datetime_patterns <- c("today", "start", "end", "\\bdate\\b")
  
  # Loop through each column name pattern
  for (pattern in datetime_patterns) {
    # Find column names matching the current pattern
    matching_cols <- grep(
      pattern, colnames(df), value = TRUE, ignore.case = TRUE
    )
    
    # Loop through each matching column and convert it to date-time
    for (col_name in matching_cols) {
      df[[col_name]] <- anytime::anytime(df[[col_name]])
    }
  }
  return(df)
}

# Iterate over relevant_forms and data_list
for (i in seq_along(relevant_forms$title)) {
  # Clean the form title to create a valid variable name
  form_title <- make.names(relevant_forms$title[i], unique = TRUE) %>% 
    tolower() %>% 
    gsub("\\.", "_", .) %>% 
    gsub("___", "_", .) %>% 
    sub("^([^\\_]+_[^\\_]+_[^\\_]+).*", "\\1", .)
  
  # Assign the dataframe to the global environment with the form_title as the variable name
  assign(form_title, data_list[[i]])
  
  # Get the data.frame from the global environment
  df <- get(form_title)
  
  # Clean variable names
  df <- clean_variable_names(df)
  
  # Assign the cleaned data.frame back to the global environment
  assign(form_title, df)
}

# List the dataframes names to keep in the environment
df <- c(
ls()[grepl(tolower(form_titles), tolower(ls()))],
"forms"
)

all_df <- base::names(
  which
  (unlist
    (eapply
      (.GlobalEnv,is.data.frame)
    )
  )
)

rm_df <- all_df %>%
  base::as.data.frame() %>%
  dplyr::filter(!(all_df %in% df))

base::rm(list=ls()[ls() %in% rm_df$.])
base::rm(rm_df)