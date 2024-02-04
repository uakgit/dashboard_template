# Computates indicators

# Ind. SO1 - Number of people directly benefiting from EU-supported interventions that specifically aim to support civilian post-conflict peace building and/or conflict prevention -----

# - Target value as from approved proposal: 4140 

indSO1_value <- 
  
  form_1 %>% 
  pull(unique_participants) %>% 
  sum(., na.rm = T)



# Ind. SO2 - Policy implementation and monitoring initiatives with CSO and CBOs supported by the action -----

# - Target value as from approved proposal: 16 

indSO2_value <- 
  
  if(TRUE %in% grepl("A1.5|A1.6", form_1$internal_activity)){
    nrow(
      form_1 %>% 
        filter(grepl("A1.5|A1.6", internal_activity))
    )
  }else{
    0
  }


indicatortable <- data.frame()

indicatortable <- 
  data.frame(indicator = c("indSO1_value", "indSO2_value"),
             value = c(indSO1_value, indSO2_value))


# Ind. SO3 - Number of community-based groups, CSOs, and CBOs supported by the action represented in Gombe and Nasarawa (GERF 2.28)  -----

# - Target value as from approved proposal: 30 

indSO3_value <- 
  
  (
    form_1 %>% 
      dplyr::select(internal_participant) %>% 
      unlist() %>% 
      strsplit(., " ") %>% 
      unlist() %>% 
      na.omit() %>%
      table()
  )[c("Community-based_groups", "CBOs", "CSOs")] %>% 
  sum()


indicatortable <- indicatortable %>% 
  rbind(.,
        data.frame(indicator = c("indSO3_value"),
                   value = c(indSO3_value))
  )

# Ind. SO4 - % of participants in surveys with a positive opinion about community-based groups, CBOs, and CSOs’ managerial, organisational, and technical capacities and credibility in the target communities.  -----

# - Target value as from approved proposal: 80% 

indSO4_value <- 
  round(
    (
      form_2 %>% 
        filter(grepl("good", tolower(eval_capacities))) %>% 
        pull(eval_participants) %>% 
        topbayes_mean() *
        nrow(
          form_2 %>% 
            filter(grepl("good", tolower(eval_capacities)))
        )
    ) / 
      
      (
        form_2 %>% 
          filter(!is.na(eval_capacities)) %>% 
          pull(eval_participants) %>% 
          topbayes_mean() *
          nrow(
            form_2 %>% 
              filter(!is.na(eval_capacities))
          )
      ) * 100
    , 1
  )



indicatortable <- indicatortable %>% 
  rbind(.,
        data.frame(indicator = c("indSO4_value"),
                   value = c(indSO4_value))
  )





indicator_table <- indicatortable %>% 
  tibble::as_tibble()

rm(indicatortable)


# Calculates dataframe by picking indicators values from the environment

# indicators_table <- mget(grep('indS|indO', 
#                    names(which(unlist(eapply(.GlobalEnv,is.numeric)))), 
#                    value = TRUE)) %>% 
#   as.data.frame() %>% 
#   t() %>% 
#   as.data.frame() %>% 
#   tibble::rownames_to_column(., "Indicator") %>% 
#   arrange(Indicator)
