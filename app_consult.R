# Consultation app


#----------------------------------------#
# When we load data from KoboToolbox
kobodata = FALSE
# When we load data from MongoDB
mongodata = FALSE
# When we us local saved data
development_only = TRUE

#----------------------------------------#


#  Packages  ----

if(development_only == FALSE){
  
  source('scripts/packages.R',
         encoding = "UTF-8")
  
}else{
  source('scripts/install_packages.R',
         encoding = "UTF-8")
}


#  Custom functions  ----
source('scripts/custom_functions.R',  
       encoding = "UTF-8")

# ##################### Sources kobo data when deploying the app

# # # Saves and Loads RDA kobo data ---- 


if(kobodata == TRUE &
   mongodata == FALSE &
   development_only == FALSE){
  
  source('scripts/downloads_data.R',
         encoding = "UTF-8")
  
  # For Development purpose
  save(
    form_1_output,
    form_2_feedback,
    form_3_financial,
    file = "raw_data/proj_data.rda")
  
}

# ##################### Sources mongo data when deploying the app ----
if(kobodata == FALSE &
   mongodata == TRUE &
   development_only == FALSE){
  
  # Import data from mongoDB, ff cronjob is already established and the data is backed up in mongoDB cloud 
  
  source("scripts/load_mongo_data.R",
         encoding = "UTF-8")
  
  
  save(
    form_1_output,
    form_2_feedback,
    form_3_financial,
    file = "raw_data/proj_data.rda"
  )
  
}

# Loads local data or when deploying as static app (when app does not download data from kobo Or mongoDB) ----
# OR  (development only)
if(kobodata == FALSE &
   mongodata == FALSE &
   development_only == TRUE){
  
  source("scripts/loads_local.R")
  
}

# Sources script with cleaning code

source("scripts/clean_data.R",
       encoding = "UTF-8")

# Clean names
form_1$outputs_fullname <-
  # Changes to upper case for parsing accents
  toupper(form_1$outputs_fullname) %>%
  gsub("[Á]","A" , .) %>%
  gsub("[Â]","A" , .) %>%
  gsub("[Ã]","A" , .) %>%
  gsub("[É]","E" , .) %>%
  gsub("[Ê]","E" , .) %>%
  gsub("[Í]","I" , .) %>%
  gsub("[Ñ]","N" , .) %>%
  gsub("[Ó]","O" , .) %>%
  gsub("[Ô]","O" , .) %>%
  gsub("[Ú]","U" , .) %>%
  # Remove punctuation from names
  gsub('[[:punct:]]+',' ', .) %>% 
  trimws() %>% 
  str_trim(side="both")

#----------------------------------------#

############## Reads in form definition ----

# Cleans survey form
registration_form <- xlsf_survey_form_1 %>% 
  select(type, final_name, contains("labe"))


############## Defines UI (user interface) for application ----

ui <- fluidPage(
  
  title = "Project ABC",
  
  # Include shinyjs in the UI
  shinyjs::useShinyjs(), 
  
  # Adds tag with CSS for direct print version   
  # Removes image link from printing version
  # Removes header and footer
  tags$head(
    
    tags$style(
    HTML("

  @media screen {
    #htext{
        line-height: 1.5;
    }
  }
    
  @media print {
 
    label , input * {
    display:none;
  }
    
    h2 { font-size: 11pt; 
    text-align: center; 
    font-weight: bold; 
    }
    
    td { 
    text-align: left;
    }

    .sorting { 
    text-align: left;
    }
    
    body { 
    font-size: 7pt;
    text-align: center;
    }
    
    .col-sm-3 * {
    display:none;
    }
    
    .help-block * {
    display:none;
    }
  
  #side-panel {
  display:none; 
  }
  
  h2 {
    display:none; 
    }
  
  .col-sm-8, .col-sm-8 * {
    visibility: visible;
  }
  
  #section-to-print {
    position: absolute;
    left: 0;
    top: 0;
  }

  @page { margin: 1; size: auto; }

      a[href]:after {
       content:'' !important;
    }
  }
                  ")
    )
  ),
  
  # Adds fav icon
  tags$head(
    tags$link(
      rel="icon", 
      href="shiny.png")
  ),
  
  
  # Application title
  
  titlePanel(
    paste(
      "Querying individual records -",
      nrow(form_1),
      "registrations"
    )
  ),
  
  helpText(h5(id="htext", 
              "INSTRUCTIONS: Consult the registration data of a person benefiting from the Project ABC. For security and data protection, to view images you must first log into your program account at https://kobo.humanitarianresponse.info with your program credentials. Images can be enlarged by clicking on them. If you do not have the credentials to authenticate, contact the person responsible for the program in your organization. We suggest not printing to obtain more efficiency from the program and because it is much more environmentally friendly. The registration form is available in the ODK Collect Android app (configured with program credentials) on", a("KoboToolBox", target = "_blank", href = "https://eu.kobotoolbox.org/#/projects/home"), 
              "If you have any questions, comments or suggestions, please send an email to", 
              a("USMAN AHMED", target = "_blank",
                href="https://www.linkedin.com/in/usman-ahmed-87853289/"), 
              "- Data Science:",
              a("uak2604@gmail.com", target="_blank",
                href="mailto:uak2604@gmail.com"
              )
  )
  ),
  
  
  # Sidebar with a slider input for number of participants and %
  sidebarLayout(
    
    sidebarPanel(
      shinyjs::useShinyjs(),
      
      ## to keep space between sidebarpanel and mainpanel
      style = "position:relative;left: 0px;
  border: 1px solid #309c3a;",
      width = 3,
      id = "side-panel",
      
      ## Alternate of img () can be htmlOutput(ui) and renderText(server)
      htmlOutput("sidebar_image"),
      
      tags$hr(),
      
      # Resets form fields due to autocomplete
      uiOutput('resettable_input'),
      
      # Search button
      actionButton("go", 
                   "Search", 
                   icon = icon("Search"),
                   width = "50%"
      ),
      
      # Reset button
      actionButton("reset_input", 
                   "", 
                   icon = icon("refresh"),
                   width = "20%"
      ), 
      
      # Print button
      actionButton("print", 
                   "", 
                   icon = icon("print"),
                   width = "20%"
      ) 
    ),
    
    mainPanel(
      id = "main-panel",
      # Suppresses error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      
      style = "padding-left:1%; padding-right: 0%",
      
      
      # Displays photo of selected beneficiary
     htmlOutput("picture", inline = TRUE),
      

      # Display error messages
      
      h5(textOutput(outputId = "name_error"), style="color:red"),
      h5(textOutput(outputId = "id_error"), style="color:red"),
      h5(textOutput(outputId = "multi_error"), style="color:red"),
      
      
      DT::dataTableOutput("dt", width = "100%"),
      
      #styling using HTML
      
      wellPanel(
        helpText( 
          a(id = "htext2", "Project ABC for Monitoring and Evaluation", 
            href="https://www.linkedin.com/in/usman-ahmed-87853289/", target="_blank"),
          #styling
          tags$style(HTML("#htext2{
                  color: #309c3a;
                  }"))
        )
      )
    ) #mainPanel(
  ) # sidebarLayout(
) # ui <- fluidPage(

############# Defines server ----

server <- function(input, output) {
  
  ############ Renders resettable inputs -----
  # This is required due to the autocomplete option for names
  
  output$resettable_input <- renderUI({
    
    times <- input$reset_input
    
    div(id=letters[(times %% length(letters)) + 1],
        
        # Autocomplete names
        
        selectizeInput(inputId = "name",
                       label = "Full Name:",
                       choices = sort(form_1$outputs_fullname),
                       selected = NULL,
                       options = list(
                         placeholder = 'type or select',
                         onInitialize = I('function() { this.setValue(""); }')
                       )
        ),
        
        # ID number
        textInput(inputId = "origin_id",
                  label = "ID number:",
                  value = NA,
                  placeholder = "only numbers")
        
    )
  })
  
  ######### Print action ---- 
  observeEvent(input$print, {
    shinyjs::runjs("{
                   document.title = 'Registrations Project ABC';
                   window.print();
                   }")
  })
  
  
  
  ######### Outputs input errors ----
  
  # Name error
  output$name_error <- renderText(
    
    if (input$go == 0){
      return()
    }else{
      # Checks if data is in the database
      if (trimws(input$name) != "" & 
          !(trimws(
            toupper(trimws(input$name) %>%
                    # Replaces multiple spaces in name input
                    gsub("\\s+", " ", .)
            )
          ) %in% trimws(toupper(form_1$outputs_fullname) %>%
                        # Replaces multiple spaces in name input
                        gsub("\\s+", " ", .)))){
        
        return("The name entered was not found in the database. Check that the name is complete and does not contain punctuations. If the name is correct, the person may not have been registered yet.")
      }
    }
  )
  
  
  # ID number error
  output$id_error <- renderText(
    
    if (input$go == 0){
      return()
    }else{
      # Checks if data is in the database
      if (trimws(input$origin_id) != "" & 
          !(trimws(input$origin_id) %in% trimws(form_1$id_person))){
        
        return("The identification number entered was not found in the database.")
        
        
      } 
    }
  )
  # Multiple entry error
  output$multi_error <- renderText(
    
    if (input$go == 0){
      return()
    }else{
      # Checks if data is in the database
      if ((
        (trimws(input$name) != "") + 
        (trimws(input$origin_id) != "") 
      ) > 1
      ){
        
        return("An error has occurred! Please use only one search criteria.")
        
        
      }
      
      
    }
  )
  
  
  ########## Outputs reactive table  -----
  
  output$dt <- renderDataTable({
    
    ## Checks search button
    if (input$go == 0)
      return()
    
    
    # Checks if data is in the database
    if ((
      (trimws(input$name) != "") + 
      (trimws(input$origin_id) != "") 
    ) > 1
    )
      return()
    
    # Checks if error exists and if yes stop the script for outputting table
    if (
      # Name error
      ((
        trimws(trimws(input$name)) != "" & 
        !(trimws(
          toupper(trimws(trimws(input$name)) %>% 
                  # Replaces multiple spaces in name input
                  gsub("\\s+", " ", .)
          )
          )  %in% 
          trimws(toupper(form_1$outputs_fullname) %>% 
          # Replaces multiple spaces in name input
          gsub("\\s+", " ", .)
          )
        )
      )) |
      
      # National ID error
      ((trimws(input$origin_id) != "" & 
        !(trimws(input$origin_id) %in% 
          trimws(form_1$id_person))
      ))
    )
    # Action  
    return()
    
    # If Name is available, it looks for the name input
    if ((
        trimws(trimws(input$name)) != "" & 
          (trimws(
            toupper(trimws(trimws(input$name)) %>% 
                      # Replaces multiple spaces in name input
                      gsub("\\s+", " ", .)
            )
          )  %in% 
            trimws(toupper(form_1$outputs_fullname) %>% 
                     # Replaces multiple spaces in name input
                     gsub("\\s+", " ", .)
            )
          )
      )){
      
      
      # Is name is not NA it saves beneficiary data to registration_data
      registration_data <- form_1 %>%
        dplyr::filter(
          (trimws(
            toupper(trimws(trimws(input$name)) %>% 
                      # Replaces multiple spaces in name input
                      gsub("\\s+", " ", .)
            )
          ) == 
            trimws(toupper(form_1$outputs_fullname) %>% 
                     # Replaces multiple spaces in name input
                     gsub("\\s+", " ", .)
            )
          )
        )  %>% 
        filter(row_number()==1)
      
      
    }else{
      
      
      if(
        trimws(input$origin_id) != "" &
        (trimws(input$origin_id) %in% 
         form_1$id_person)) {
        # If id is not NA it saves beneficiary data to registration_data
        registration_data <- form_1 %>%
          dplyr::filter(id_person == 
                          trimws(input$origin_id))  %>% 
          filter(row_number()==1)
        
      }
    }
    
    # Checks if data is in the environment due to reset button
    if (
      "registration_data" %in% ls()
    ) {
      
      ## Preparing data table ----      
      
      ## Adjusts date format of registered data
      registration_data <- registration_data %>%
        mutate(
          filter_date = filter_date %>% 
            lubridate::ymd() %>% 
            as.Date %>% 
            format("%d/%m/%Y")
        ) 
      
      # Creates a vector with names of variables (rows)
      consult_variables <-  c(
        "outputs_fullname",
        "id_person",
        "filter_date", 
        "outputs_gender",
        "outputs_age",
        "outputs_is_pwd",
        "outputs_occupation",
        "outputs_organisation",          
        "outputs_needs",
        "outputs_position", 
        "internal_state",
        "internal_lga", 
        "internal_site",
        "internal_activity",
        "outputs_participants",         
        "outputs_participant_data",     
        "outputs_children",              
        "outputs_youths",               
        "outputs_adults",                
        "outputs_seniors",              
        "outputs_women",                 
        "outputs_representative_data" 
        
      )
      
      
      variables_commas <- c(
        "id_person",
        "outputs_fullname",
        "outputs_position"
        
      )
      
      
      # Converts from tibble to data frame for setting row.names
      registration_form <- data.frame(registration_form) 
      
      
      # Creates line for additional data
      registration_consult <- rbind(
        
        data.frame(
          type = "text", 
          final_name = "outputs_fullname",
          label_en = "Name of the person:",
          label_ha = "Sunan mutumin:"),
        
        data.frame(
          type = "integer", 
          final_name = "id_person",
          label_en = "ID of the person:",
          label_ha = "ID na mutum:"),
        
        # Date of registration
        data.frame(type = "date", 
                   final_name = "filter_date",
                   label_en = "Date of registration:",
                   label_ha = "Ranar rajista:"),
        
        # Form questions
        data.frame(registration_form)
      ) #%>% 
      
      # Reanme variable if different from variable name in form_1----
      ## Change variable names in registration_form if they are different from variable names in form_1 (registration_data) before merging with registration_consult
      
      # Sets rows names to ease merging
      rownames(registration_consult) <- registration_consult$final_name 
      
      # Creates dataframe consult with merged data
      consult <- merge(
        registration_consult, 
        t(registration_data), by=0) %>%
        # Renames variable with merged data - change label names for language selection
        dplyr::rename(Data = V1,
                      Questions = label_en) %>%
        dplyr::select(-Row.names) 
      
      
      # Removes unnecessary rows
      table_consult <- consult %>%
        dplyr::filter(final_name %in% consult_variables) %>%
        # Sets rows for which spaces should not be replaced by commas
        dplyr::mutate(Data = 
                        ifelse(
                          !(final_name %in% variables_commas),
                          gsub(
                            " ", ", ", 
                            as.character(Data)
                          ),
                          as.character(Data)
                        )
        )  %>%
        dplyr::mutate(
          Data = gsub("_", " ",
                      as.character(Data)
          )
        )
      
      # Arranges rows according to vector consult_variables
      table_consult <- 
        table_consult[match(consult_variables, table_consult$final_name),]  %>%
        # Removes questions with NA as answer
        na.omit() 
      
      # Removes row names for printing
      rownames(table_consult) <- c()
      
      # Prints output
      dt <- table_consult %>%
        dplyr::select(Questions, Data) %>%
        dplyr::rename(`Survey` = Questions,
                      `Reported data` = Data) %>%
        # Avoids error binding character to factor
        dplyr::mutate_if(is.factor, as.character)  
      
      # Include other dataframes if any
      dt <- dplyr::bind_rows(
        dt
      )
  
      # Creates variable with question count
      rownames(dt) <- 1:nrow(dt)
      
      # Renders table
      dt 
    } # it closes the check if dataframe %in% ls()
  }, # it closes renderDataTable of output$dt
  
  
  options = list(
    # Only show table and filter
    dom = 'ft',
    # Removes paging
    paging = FALSE, 
    #set width and column index 
    columnDefs = (list(list(width = '15px', targets = c(0)),list(width = '400px', targets = c(1)), list(width = '430px', targets =c(2)))),
    language = list(search = 'Filter:'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#309c3a', 'color': '#fff'});",
      "}")  
    
  )
  
  )  # Closes output$dt renderDataTable 
  
  
  ######## Outputs picture of sidebar-logo -----
  
  output$sidebar_image <- renderText({
    c('<a href="',
      "https://www.linkedin.com/in/usman-ahmed-87853289/",
      '" target="_blank"',
      '>',
      '<img src="',
      "passbild.jpg",
      '", width="', "100%",
      'height="',"100%",
      '"style="display: block; margin-left: auto; margin-right: auto;"',
      " >",
      '</a>')
    
    
  })
  
  ######## Outputs picture of beneficiary -----

  output$picture <- renderText({

    # Checks if at least one of the parameters has data

    ## Checks search button
    if (input$go == 0)
      return()


    # Checks if data is in the database
    if ((
      (trimws(input$name) != "") +
      (trimws(input$origin_id) != "")
    ) > 1
    )
      return()

    # Checks if data is in the database

    if((
        trimws(trimws(input$name)) != "" & 
          !(trimws(
            toupper(trimws(trimws(input$name)) %>% 
                      # Replaces multiple spaces in name input
                      gsub("\\s+", " ", .)
            )
          )  %in% 
            trimws(toupper(form_1$outputs_fullname) %>% 
                     # Replaces multiple spaces in name input
                     gsub("\\s+", " ", .)
            )
          )
      )){
      
      
      return()
    }


    # Checks if data is in the database
    if (trimws(input$origin_id) != "" &
        !(trimws(input$origin_id) %in%
          trimws(form_1$id_person))){
      return()

    }

    # Filters beneficiary data

      # If name is not available, it looks for the name input
        
        if ((
          trimws(trimws(input$name)) != "" & 
          (trimws(
            toupper(trimws(trimws(input$name)) %>% 
                    # Replaces multiple spaces in name input
                    gsub("\\s+", " ", .)
            )
          )  %in% 
          trimws(toupper(form_1$outputs_fullname) %>% 
                 # Replaces multiple spaces in name input
                 gsub("\\s+", " ", .)
          )
          )
        )){
        

        # Is name is not NA it saves beneficiary data to registration_data
        registration_data <- form_1 %>%
          dplyr::filter(
            (trimws(
              toupper(trimws(trimws(input$name)) %>% 
                        # Replaces multiple spaces in name input
                        gsub("\\s+", " ", .)
              )
            )  == 
              trimws(toupper(form_1$outputs_fullname) %>% 
                       # Replaces multiple spaces in name input
                       gsub("\\s+", " ", .)
              )
            )
          ) %>%
          filter(row_number()==1)

      }else{

        # Filters beneficiary based on panacard
        if (trimws(input$origin_id) != "" &
            (trimws(input$origin_id) %in% form_1$id_person)) {
          # If CPF is not NA it saves beneficiary data to registration_data
          registration_data <- form_1 %>%
            dplyr::filter(id_person == trimws(input$origin_id)) %>%
            filter(row_number()==1)
        }
      }
    
    # Setting size of pictures
    photofactor  <-  .4
    photowidth <- 320 * photofactor
    photoheight <- 640 * photofactor

    # Checks if data is in the environment due to reset button
    if (
      "registration_data" %in% ls()
    ) {
      
     ## Use this code if we have url links to the user images
      # if(
      #   !is.na(registration_data$url_photo)
      # ){
      #   # Prints picture
      #   c('<a href="',
      #     registration_data$url_photo,
      #     '" target="_blank"',
      #     '>',
      #     '<img src="',
      #     registration_data$url_photo,
      #     '", width="', photowidth,
      #     'height="',
      #     photoheight,
      #     '"style="display: block; margin-left: auto; margin-right: auto;"',
      #     " >",
      #     '</a>')
      #   
      #   
      # }else{

        if(registration_data$outputs_gender == "Male"){

          # Prints picture
          c('<img src="',
            "user-male.png",
            '", width="', photowidth,
            'height="',
            photoheight,
            '"style="display: block; margin-left: auto; margin-right: auto;"',
            " >",
            '</a>')

        }else{

          if(registration_data$outputs_gender == "Female"){

            # Prints picture
            c('<img src="',
              "user-female.png",
              '", width="', photowidth,
              'height="',
              photoheight,
              '"style="display: block; margin-left: auto; margin-right: auto;"',
              " >",
              '</a>')

          }else{


            # Prints picture
            c('<img src="',
              "gender-neutral.png",
              '", width="', photowidth,
              'height="',
              photoheight,
              '"style="display: block; margin-left: auto; margin-right: auto;"',
              " >",
              '</a>')

          }
        }

      }

  })
  
}

# Runs the application ----
shinyApp(ui = ui, server = server)

