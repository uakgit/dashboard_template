# STEP 0 - Set working directory (required for AWS only)
setwd("cronjob-ProjectABC/")


paste0("cronjob starts at ", Sys.time())


paste0("runs install_packages.R at ", Sys.time())
# STEP 1 - Load packages ----
source("scripts/packages_cronjob.R", encoding = "UTF-8")

paste0("runs custom_functions.R at ", Sys.time())
# STEP 2 - Load custom functions ----
source("scripts/custom_functions.R", encoding = "UTF-8")

paste0("runs login_credentials.R at ", Sys.time())
# STEP 3 - Load credentials ----
source("scripts/login_credentials.R", encoding = "UTF-8")


# # Run on Ubuntu OS, when developing
# 
# load("raw_data/kobo_data.rda")

paste0("runs downloads_data.R at ", Sys.time())
# STEP 4 - Load data ----
source("scripts/downloads_data.R", encoding = "UTF-8")

paste0("runs renew_mongodb_data.R at ", Sys.time())
# STEP 5 - Update MongoDB data ----
source("scripts/renew_mongodb_data.R", encoding = "UTF-8")

paste0("cronjob completed at ", Sys.time())
# STEP 6 - Cleans workspace ----
source("scripts/clean_workspace.R", encoding = "UTF-8")
