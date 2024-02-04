

# Filters only valid submissions ----

if("validation_status_label" %in% names(form_1_output)){
  
  form_1_output <- form_1_output %>%
    filter(
      grepl("^approved", tolower(validation_status_label)) |
        is.na(validation_status_label)
    )
}

if("validation_status_label" %in% names(form_2_feedback)){
  
  form_2_feedback <- form_2_feedback %>%
    filter(
      grepl("^approved", tolower(validation_status_label)) |
        is.na(validation_status_label)
    )
}

if("validation_status_label" %in% names(form_3_financial)){
  
  form_3_financial <- form_3_financial %>%
    filter(
      grepl("^approved", tolower(validation_status_label)) |
        is.na(validation_status_label)
    )
}

# TRANSLATE DATAFRAMES ----

## The xml values are already in English
## Converting all variables into character

form_1 <-  form_1_output %>% 
  mutate_all(., ~as.character(.))

form_2 <-  form_2_feedback %>% 
  mutate_all(., ~as.character(.))

form_3 <-  form_3_financial %>% 
  mutate_all(., ~as.character(.))


# Adjusts date of registartions ----
# replace date of registration by submission date

form_1 <- form_1 %>%
  mutate(filter_date =
           data.table::fifelse(
             lubridate::as_date(today) > 
               lubridate::as_date(submission_time),
             # extract ymd date from submission time
             substr(
               submission_time,
               1,
               nchar(submission_time)-9),
             
             today
           )
  )

form_2 <- form_2 %>%
  mutate(filter_date =
           data.table::fifelse(
             lubridate::as_date(today) > 
               lubridate::as_date(submission_time),
             # extract ymd date from submission time
             substr(
               submission_time,
               1,
               nchar(submission_time)-9),
             
             today
           )
  )

form_3 <- form_3 %>%
  mutate(filter_date =
           data.table::fifelse(
             lubridate::as_date(today) > 
               lubridate::as_date(submission_time),
             # extract ymd date from submission time
             substr(
               submission_time,
               1,
               nchar(submission_time)-9),
             
             today
           )
  )

# Form 3- Currency conversion ----
# Loads data from InforEuro using API for 1EUR to NGN
infoeuro_ngn <- httr::GET("https://ec.europa.eu/budg/inforeuro/api/public/currencies/NGN") %>% 
  fromkobo() %>% 
  type.convert(., as.is = TRUE)
 
infoeuro_ngn <- infoeuro_ngn %>% 
  mutate(dateStart = lubridate::dmy(dateStart),
         dateEnd = lubridate::dmy(dateEnd)) %>% 
  mutate_if(is.Date, as.character)

# Merge currency conversion data
form_3 <-
  merge(
    
    form_3 %>% 
      mutate(date_ym =
               
               # extract ymd date from submission time
               substr(
                 filter_date,
                 1,
                 nchar(filter_date)-3)
      ),
    
    infoeuro_ngn %>% 
      mutate(date_ym =
               
               # extract ymd date from submission time
               substr(
                 dateStart,
                 1,
                 nchar(dateStart)-3)
      ) %>% select(date_ym, amount) %>% 
      rename(euro_ngn = amount),
    
    by = "date_ym",
    all.x = T
  ) %>% 
  select(-date_ym) %>% 
  # Calculates NGN to EUR
  mutate(expend_amount_eur = 
           data.table::fifelse(
             !is.na(expend_amount),
             round(
               as.numeric(expend_amount) / as.numeric(euro_ngn),
               3
             ),
             NA_integer_
             
             
           )
         
  )
  
  
## Import budget file ----

xls_budget_f4p <- readxl::read_excel("raw_data/Budget revision.xlsx")

xls_budget_f4p_table <- xls_budget_f4p %>%
  as_tibble() %>%
  janitor::clean_names()

xls_budget_f4p_table <- xls_budget_f4p_table %>%
  janitor::remove_empty(which = "rows") %>% 
  .[4:140,] %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  rename(expend_budgetline = expenditures,
         expend_category_budgetline = na) %>% 
  filter(!is.na(expend_budgetline) |
           !is.na(expend_category_budgetline)) %>%
  mutate_all(., ~trimws(.)) %>% 
  
  mutate_at(vars(matches("_eur")), ~ gsub(",", "", .))


xls_budget_f4p_category <- xls_budget_f4p_table %>%
  filter(
    is.na(expend_budgetline) &
      grepl("subtotal", tolower(expend_category_budgetline))
  ) %>% 
  mutate(expend_budgetline = as.character(1:nrow(.))) %>% 
  select(1:2, total_costs_eur_2) %>% 
  mutate(expend_category_names =
           data.table::fifelse(
             grepl("human", tolower(expend_category_budgetline)),
             "Human resource",
             data.table::fifelse(
               grepl("travel", tolower(expend_category_budgetline)),
               "Travel",
               data.table::fifelse(
                 grepl("equipment", tolower(expend_category_budgetline)),
                 "Equipment & supplies",
                 data.table::fifelse(
                   grepl("local", tolower(expend_category_budgetline)),
                   "Local office",
                   data.table::fifelse(
                     grepl("services|workshops|visibility", tolower(expend_category_budgetline)),
                     "Services",
                     data.table::fifelse(
                       grepl("subtotal other", tolower(expend_category_budgetline)),
                       "Other",
                       expend_category_budgetline
                     )
                   )
                 )
               )
             )
           )
  ) %>% 
  mutate(total_costs_eur_2 = as.numeric(total_costs_eur_2) %>% round()) %>% 
  select(expend_category_names, expend_budgetline, total_costs_eur_2)
  
form_3 <- form_3 %>% 
  select(
    filter_date, 
         internal_implementing_partner, 
         expend_category, 
         expend_budgetline, 
         contains("amount"), 
         everything()
         ) %>% 
  mutate(expend_category_names =
           data.table::fifelse(
             expend_category == "Personnel_and_perdiem",
             "Human resource",
             data.table::fifelse(
               expend_category == "Travel",
               "Travel",
               data.table::fifelse(
                 grepl("equipment", tolower(expend_category)),
                 "Equipment & supplies",
                 data.table::fifelse(
                   grepl("local", tolower(expend_category)),
                   "Local office",
                   data.table::fifelse(
                     grepl("services|workshops|visibility", tolower(expend_category)),
                     "Services",
                     data.table::fifelse(
                       grepl("subgranting", tolower(expend_category)),
                       "Other",
                       expend_category
                     )
                   )
                 )
               )
             )
           )
  )


form_3 <- form_3 %>% 
  merge(.,
        xls_budget_f4p_category %>% select(expend_category_names, total_costs_eur_2),
        by = "expend_category_names",
        all.x = T)



## Calculates table for each budgetlines 

xls_budget_f4p_budgetline <- xls_budget_f4p_table %>% 
  select(expend_budgetline, expend_category_budgetline, total_costs_eur_2) %>% 
  mutate(total_costs_eur_2 =
           data.table::fifelse(
             is.na(total_costs_eur_2),
             "0",
             total_costs_eur_2
           )
  ) %>% 
  filter(!is.na(expend_budgetline))


## XLSForms data ----

# List the forms with source path directories  
forms <- c("forms/Form_1.xlsx",
           "forms/Form_2.xlsx",
           "forms/Form_3.xlsx")

for (i in 1:length(forms)) {
  
  #### Function to replace NAs 
  replace_na_previous <- function(x, a =! is.na(x)) {
    
    x[which(a)[c(1, 1:sum(a))][cumsum(a) + 1]]
    
  }
  
  koboform_survey <- readxl::read_excel(forms[i], sheet = "survey") %>%   
    select(type,name, contains("label")) %>%
    #janitor::clean_names() %>% 
    rename_with(., 
                ~ tolower(
                  # Removes all the text outside parenthesis and adds prefix label_
                  gsub("^.*?\\((.*)\\)[^)]*$", "label_\\1", .)
                )
    ) %>% 
    #rename(label = label_en) %>% 
    filter(!is.na(name)) %>% 
    
    mutate(name =  
             stringr::str_trim(name)
    ) %>% 
    
    # Creates variable with group name
    mutate(
      group = ifelse(type == "begin group", name, NA)
    ) %>% 
    # Replace empty cells with group names
    mutate(group = replace_na_previous(group) %>% tolower()) %>% 
    # Filters out type "begin group"
    filter(type != "begin group" & 
             type != "begin repeat" & 
             type != "end group" & 
             type != "end repeat" & 
             type != "note" & 
             type != "image" & 
             type != "date" & 
             type != "geopoint") %>%  
    
    # Create column with final variable names
    mutate(
      final_name = paste(group, name, sep="_")) %>% 
    # Keeps only variables present in the final f3_hhsurvey
    # filter(final_name %in% names(f3_hhsurvey)) %>% 
    mutate(
      question_type = (
        type %>% 
          stringr::str_split_fixed(., " ", 2)
      )[,1]) %>% 
    
    mutate(
      options_id = (
        type %>% 
          stringr::str_split_fixed(., " ", 2)
      )[,2]) %>% 
    filter(!is.na(label_en)) %>% 
    # Adjusts duplication in text type labels
    mutate(label_en = trimws(label_en)) %>% 
    # If labels are duplictated then will be appended by its variable name
    mutate(
      label_en = 
        data.table::fifelse( 
          label_en %in% .$label_en[duplicated(.$label_en)], 
          paste0(label_en, " (", gsub("_", " ", name), ")"), 
          label_en)
    )
  
  
  # Saves the form_survery_i with associated dataframe name
  assign(paste("xlsf_survey", 
               tolower(stringr::word(gsub("forms/", "",forms[i]) %>% 
                                       gsub("-", "_",.)
                                     , 1, sep = fixed("."))), 
               #tolower(stringr::word(forms[i], 2)), 
               sep = "_") , 
         koboform_survey) 
  
  rm(koboform_survey)
  
}

# Convert "Not_sure" into "NA" from evaluative questions of Form-2 and Form-4 ----

form_2 <- form_2 %>% 
  
  mutate_at(
    xlsf_survey_form_2 %>%
      filter(grepl("vpoor_vgood|vlow_vhigh", type)) %>% 
      select(final_name) %>%
      unlist(use.names = FALSE)
    , ~ gsub("Not_sure", NA_character_, .)
  )



# Claculates median value of participants to exclude duplication ----

form_1 <- form_1 %>% 
  mutate(outputs_participants = as.numeric(outputs_participants))

# Calculate the median number of participants in reported activities
median_participants <- median(form_1$outputs_participants)

# Create a new column 'unique_beneficiaries' in the form_1 dataset
form_1$unique_participants <- 
  ifelse(
    form_1$outputs_participants > median_participants, 
    median_participants, 
    form_1$outputs_participants
  )


vars <- c("outputs_children", "outputs_youths", "outputs_adults", "outputs_seniors")

form_1 <-
  form_1 %>% 
  mutate_at(vars,
            ~as.numeric(.)) %>% 
  mutate_at(vars,
            ~data.table::fifelse(
              outputs_participants > unique_participants,
              (./outputs_participants * unique_participants),
              .
            )
  ) %>% 
  
  mutate(outputs_women = as.numeric(outputs_women)) %>% 
  mutate(outputs_women =
           data.table::fifelse(
             outputs_women > unique_participants,
             unique_participants ,
             outputs_women
           )
         
  )


# Removes all intermediary dataframes ----

# List the dataframes names to keep in the environment
df <- c(
  "form_1",
  "form_2",
  "form_3",
  "xls_budget_f4p_category",
  "xls_budget_f4p_budgetline",
  "xlsf_survey_form_1",
  "xlsf_survey_form_2",
  "xlsf_survey_form_3"
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