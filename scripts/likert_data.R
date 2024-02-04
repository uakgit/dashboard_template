# Feedback dataframe ----

# Feedback likert ----

form_2_likert <- form_2 %>% 
  # Selects columns
  select(
    
    any_of(
      xlsf_survey_form_2 %>%
        filter(
          grepl("likert", type)
          
        ) %>%
        select(final_name) %>%
        unlist(use.names = FALSE)
    )
    
  ) %>% 
  
  mutate_all(., ~ gsub("_", " ", .)) 

# Replaces the variable names by the values of another dataframe

data.table::setnames(form_2_likert, 
                     as.character(xlsf_survey_form_2$final_name), 
                     as.character(xlsf_survey_form_2$label_en), 
                     skip_absent = TRUE)

form_2_likert <- form_2_likert %>% 
  
  dplyr::mutate_all(
    ., 
    ~ ordered(., levels = c(
      "Totally Agree",
      "Agree",
      "Agree Slightly",
      "Neutral",
      "Disagree Slightly",
      "Disagree",
      "Strongly Disagree"
    )
    )
  )

# Feedback vgood ----

form_2_vgood <- form_2 %>% 
  # Selects columns
  select(
    
    any_of(
      xlsf_survey_form_2 %>%
        filter(
          grepl("vpoor_vgood", type)
          
        ) %>%
        select(final_name) %>%
        unlist(use.names = FALSE)
    )
    
  )  %>% 
  
  mutate_all(., ~ gsub("_", " ", .))

# Replaces the variable names by the values of another dataframe

data.table::setnames(form_2_vgood, 
                     as.character(xlsf_survey_form_2$final_name), 
                     as.character(xlsf_survey_form_2$label_en), 
                     skip_absent = TRUE)

form_2_vgood <- form_2_vgood %>% 
  
  dplyr::mutate_all(
    ., 
    ~ ordered(., levels = c(
      "Very good",
      "Good",
      "Regular",
      "Poor",
      "Very poor"
    )
    )
  )

