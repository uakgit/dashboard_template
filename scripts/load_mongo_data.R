
# Loads credentials ----
source("scripts/login_credentials.R", encoding = "UTF-8")

# Prepares URL for MongoDB Atlas ----
mongo_url <- paste0(
  "mongodb+srv://",
  user_mongodb, ":",
  password_mongodb,
  "@cluster0........mongodb.net/"
)


#----------------------------------------#
# Mongo (form 1) ----

mongo_form_1_output <- mongolite::mongo(
  collection = "form_1_output", 
  db = "production", url = mongo_url)

# Creates dataframe with MongoDB Atlas data
form_1_output <- mongo_form_1_output$find() %>% 
  tibble::as_tibble()

# Mongo (form 2) ----

mongo_form_2_feedback <- mongolite::mongo(
  collection = "form_2_feedback", 
  db = "production", url = mongo_url)

# Creates dataframe with MongoDB Atlas data
form_2_feedback <- mongo_form_2_feedback$find() %>% 
  tibble::as_tibble()


# Mongo (form 3) ----

mongo_form_3_financial <- mongolite::mongo(
  collection = "form_3_financial", 
  db = "production", url = mongo_url)

# Creates dataframe with MongoDB Atlas data
form_3_financial <- mongo_form_3_financial$find() %>% 
  tibble::as_tibble()

# Mongo (form 4) ----

mongo_form_4_baseline <- mongolite::mongo(
  collection = "form_4_baseline", 
  db = "production", url = mongo_url)

# Creates dataframe with MongoDB Atlas data
form_4_baseline <- mongo_form_4_baseline$find() %>% 
  tibble::as_tibble()


# Disconnect from MongoDB Atlas
mongo_form_1_output$disconnect()
mongo_form_2_feedback$disconnect()
mongo_form_3_financial$disconnect()
mongo_form_4_baseline$disconnect()