# Renew MongoDB data

# Prepares URL for MongoDB Atlas ----
mongo_url <- paste0(
  "mongodb+srv://",
  user_mongodb, ":",
  password_mongodb,
  "@cluster0.......mongodb.net/?retryWrites=true&w=majority"
  )


#----------------------------------------#
# Mongo registration ----
# Preparing connection to collection (registration)
mongo_form_1_output <- mongolite::mongo(
  collection = "form_1_output", 
  db = "production", url = mongo_url)

#----------------------------------------#
# Mongo form_2_feedback ----
# Preparing connection to collection (form_2_feedback)
mongo_form_2_feedback <- mongolite::mongo(
  collection = "form_2_feedback", 
  db = "production", url = mongo_url)


#----------------------------------------#
# Mongo item delivery ----
# Preparing connection to collection (item delivery)
mongo_form_3_financial <- mongolite::mongo(
  collection = "form_3_financial", 
  db = "production", url = mongo_url)



#----------------------------------------#
# Drops data from mongoDB cloud

mongo_form_1_output$drop()
mongo_form_2_feedback$drop()
mongo_form_3_financial$drop()


#----------------------------------------#
# Inserts new data in MongoDB ----

# Registrations
mongo_form_1_output$insert(form_1_output)

# form_2_feedback
mongo_form_2_feedback$insert(form_2_feedback)

# Item delivery
mongo_form_3_financial$insert(form_3_financial)


#----------------------------------------#

# Disconnect from MongoDB Atlas
mongo_form_1_output$disconnect()
mongo_form_2_feedback$disconnect()
mongo_form_3_financial$disconnect()
