# Script with filter per partner

# Filter data by partner names inputs ----

if (length(input$input_partner) == 0  
) {
  
  form_1 <- form_1_filter()
  form_2 <- form_2_filter()
  form_3 <- form_3_filter()
  
}else{
  
  
  if (length(input$input_partner) > 0 
  ) {

    form_1 <- form_1_filter() %>%
      filter(internal_implementing_partner %in% input$input_partner)
    
    form_2 <- form_2_filter() %>%
      filter(internal_implementing_partner %in% input$input_partner)
    
    form_3 <- form_3_filter() %>%
      filter(internal_implementing_partner %in% input$input_partner)
    
    
  }
  
}


# Filter data by date range inputs ----

form_1 <- form_1 %>% 
  # Filter data range
  filter(lubridate::ymd(filter_date) >= 
           lubridate::ymd(input$dateRange[1]),  
         lubridate::ymd(filter_date) <= 
           lubridate::ymd(input$dateRange[2]))

form_2 <- form_2 %>% 
  # Filter data range
  filter(lubridate::ymd(filter_date) >= 
           lubridate::ymd(input$dateRange[1]),  
         lubridate::ymd(filter_date) <= 
           lubridate::ymd(input$dateRange[2]))

form_3 <- form_3 %>% 
  # Filter data range
  filter(lubridate::ymd(filter_date) >= 
           lubridate::ymd(input$dateRange[1]),  
         lubridate::ymd(filter_date) <= 
           lubridate::ymd(input$dateRange[2]))
