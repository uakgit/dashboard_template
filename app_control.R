# DASHBOARD



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


# CSS for DT tables used in dashboardBody ----

css <- HTML(
  ".dataTables_scrollBody {
        transform:rotateX(180deg);
    }
    .dataTables_scrollBody table {
        transform:rotateX(180deg);
    }"
)

# KEY VALUES ----

## Partner names: which we use to filter data ----
partner_names <- c(
  c(
    form_1 %>%
      select(internal_implementing_partner) %>%
      unlist(use.names = F),
    form_2 %>%
      select(internal_implementing_partner) %>%
      unlist(use.names = F),
    form_3 %>%
      select(internal_implementing_partner) %>%
      unlist(use.names = F)
  ) %>%
    na.omit %>% 
    unique() %>% 
    sort() %>% 
    tibble::as_tibble() %>% 
    unlist(use.names = F)
)

partner_names <- c(
  sort(
    partner_names[partner_names != "Other"]
  ), 
  "Other"
)



#----------------------------------------#
# User interface   ----

dbHeader <- dashboardHeader(
  # Application header
  title = tags$a(
    # Link when clicked
    href = 'https://www.linkedin.com/in/usman-ahmed-87853289/', 
    target = "_blank",
    # Display application name
    'Project ABC',
    '',
    # CSS 
    tags$link(
      rel = "stylesheet", 
      type = "text/css", 
      href = "custom.css"
    )
  ),
  tags$li(
    class = "dropdown",
    
    # Display client logo on the right-upper corner
    tags$a(
      href="https://www.linkedin.com/in/usman-ahmed-87853289/", 
      target="_blank",
      tags$img(height = "20px", 
               alt="SNAP Logo", 
               src="passbild.jpg")
    )
    
  ),
  dropdownMenu(
    type = "messages",
    headerText = "Messages",
    messageItem(
      from = "Usman Ahmed",
      message = "Usman Ahmed is developing the dashboard"
    )
  )
)

# ui ----

ui <- dashboardPage(
  
  
  skin = "black", 
  # Display shiny logo at browser tab
  title = tags$head(
    tags$link(
      rel="icon", 
      type = "image/png",
      #sizes = "32x32",
      href="shiny.png")
  ),
  
  # Header
  dbHeader,
  
  dashboardSidebar(
    
    # Custom CSS to format download button and horizontal lines
    tags$head(
      tags$style(
        HTML('
        #partner_report {
        background-color: #F5F5F5;
        color: #000000;
        }
        #partner_report:hover {
        background-color: #b3b3cc;
        }
        hr {
        border: 1px solid #29293d
        }
        
        /* main sidebar */
        .skin-black .main-sidebar.sidebar-menu a {
                              background-color: #003300;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #003300;
                              }
        /* toggle button when hovered  */
         .skin-black .main-header .navbar .sidebar-toggle:hover{
                              background-color: #003300;
                              }

        '
        )
      )
    ),
    
    # Use shinyjs for disabling button
    shinyjs::useShinyjs(),
    
    # Sidebar   ----
    sidebarMenu(
      # Add space 
      tags$br(),
      
      selectInput("input_partner", 
                  label = "Partner:", 
                  multiple = TRUE, 
                  # Create form choices with implementing partenrs
                  choices = partner_names,
                  selected = partner_names
      ),
      
      # Add space 
      #tags$br(),
      
      # Date range input
      dateRangeInput(
        'dateRange',
        #label = NULL,
        label = "Time interval:", 
        language = "en",
        separator = " - ",
        start = min(
          min(lubridate::ymd(form_1$filter_date)),
          min(lubridate::ymd(form_2$filter_date)),
          min(lubridate::ymd(form_3$filter_date))
        ),
        end = max(
          max(lubridate::ymd(form_1$filter_date)),
          max(lubridate::ymd(form_2$filter_date)),
          max(lubridate::ymd(form_3$filter_date))
        ),
        format = "dd/mm/yyyy"),
      
      tags$br(),
      
      tags$div(
        
        actionButton("generate", "Generate report", icon = icon("file-alt"), 
                     # This is the only button that shows up when the app is loaded
                     style = "color: #fff; background-color: #4d4d4d; border-color: #000000"
        ),
        # Add loading message following condition
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Please wait...",id="loadmessage")
        ),
        
        conditionalPanel(
          condition = "output.reportbuilt",
          downloadButton("partner_report", "Download the report")
        ),
        align = 'center'
      ),
      
      # Forecast date input
      dateInput(
        "future_date", 
        label = "Forecast date:", 
        value = paste0(today() %>% year() + 1, "-12-31"), 
        min = paste0(today() %>% year(), "-", today() %>% month() + 1, "-", today() %>% day()), 
        max = paste0(today() %>% year()+1, "-12-31"),
        format = "dd/mm/yyyy", 
        startview = "month", 
        weekstart = 0,
        language = "en", 
        width = NULL, 
        autoclose = TRUE,
        datesdisabled = NULL, 
        daysofweekdisabled = NULL
      ),
      
      
      # Breaks
      tags$br(),
      tags$br(),
      
      # Dashboard Left sidebar ----
      
      menuItem("Dashboard", 
               tabName = "dashboard", 
               icon = icon("desktop")
      ),
      
      menuItem("Outputs", 
               tabName = "outputs_f1", 
               icon = icon("chart-simple")),
      
      menuItem("Feedback", 
               tabName = "feedback_f2",
               icon = icon("chart-simple"),
               menuSubItem("Demographics", 
                           tabName = "feedback_f2",
                           icon = icon("chart-bar")),
               menuSubItem("Evaluation", 
                           tabName = "evaluation_f2",
                           icon = icon("chart-bar"))
      ),
      
      menuItem("Finances", 
               tabName = "finan_f3", 
               icon = icon("chart-simple"),
               menuSubItem("Demographics", 
                           tabName = "finan_f3", 
                           icon = icon("chart-bar")),
               menuSubItem("Budget", 
                           tabName = "budget_f3", 
                           icon = icon("chart-bar"))
      ),
      
      ## Tab of Tables ----
      menuItem("Data Tables", 
               tabName = "tables",
               icon = icon("database"),
               
               ## F1 Output -----
               menuSubItem("F1 Output", 
                           tabName = "form1_df", 
                           icon = icon("table")),
               
               ## F2 Feedback -----
               menuSubItem("F2 Feedback",
                           tabName = "form2_df",
                           icon = icon("table")),
               
               ## F3 Finances -----
               menuSubItem("F3 Finances",
                           tabName = "form3_df",
                           icon = icon("table"))
      )
      
    ) # sidebarMenu
  ), #dashboardSidebar
  
  # Body content ----
  dashboardBody(
    
    # CSS for data tables
    tags$head(tags$style(css)),
    tabItems(
      # Dashboard tab content
      tabItem(tabName = "dashboard",
              # Top row
              fluidRow(
                
                ## Value Boxs  ----
                shinydashboard::valueBox(
                  textOutput("f1_registrations_vbox"), 
                  "Registrations (F1 Output)", 
                  icon = icon("address-card"), 
                  color="green"
                ),
                
                shinydashboard::valueBox(
                  textOutput("f2_registrations_vbox"), 
                  "Registrations (F2 Feedback)", 
                  icon = icon("address-card"), 
                  color="light-blue"
                ),
                
                shinydashboard::valueBox(
                  textOutput("f3_registrations_vbox"), 
                  "Registrations (F3 Financial)", 
                  icon = icon("address-card"), 
                  color="green"
                ),
                
                shinydashboard::valueBox(
                  textOutput("f1_participants_vbox"),   
                  "Unique beneficiaries (F1 Output)", 
                  icon = icon("people-group"), 
                  color="light-blue"
                ),
                
                shinydashboard::valueBox(
                  textOutput("f1_women_vbox"),   
                  "Women (F1 Output)", 
                  icon = icon("female"), 
                  color="fuchsia"
                ),
                
                
                shinydashboard::valueBoxOutput("valuebox_forecast_f1_benef"),
                
              ),
              
              fluidRow(
                
                column(12, h6(
                  paste0("Updated on: ",
                         format(Sys.Date(), 
                                format="%d/%m/%Y"),
                         " / Most recent registration: ",
                         max(
                           max(lubridate::ymd(form_1$filter_date)),
                           max(lubridate::ymd(form_2$filter_date)),
                           max(lubridate::ymd(form_3$filter_date))
                         ) %>%
                           as.Date() %>%
                           format(.,"%d/%m/%Y"), 
                         " Note: We estimate the number of unique beneficiaries by automatically calculating the median number of participants from reported activities and using it to replace high counts (above the median value) so as to mitigate multiple counting.")
                  
                ), offset=0
                )
              ), 
              
              fluidRow(
                column(12, h3("Indicators")),
                column(12, h6("Source: The calculation of indicators are based on project implementation data. The target values refer logframe for overall project."))
              ),
              
              ## Indicators UI output   ----
              
              uiOutput("ind_comments")
              
              
      ),
      
      
      ## Data tab content- Outputs   ----
      tabItem(tabName = "form1_df",
              column(12, h3(
                paste0(
                  "Outputs and logging activities"))
              ),
              column(3, 
                     downloadButton('f1_downloadData', 'File download XLSX')),
              
              column(12, 
                     tags$br()
              ),
              tags$head(
                tags$style( 
                  type = 'text/css',
                  '.datatable{ overflow-x: scroll; }')
              ),
              ## Dataset 
              fluidRow(dataTableOutput("view_form1_df")
              )
      ),
      
      ## Data tab content- Feedback   ----
      tabItem(tabName = "form2_df",
              column(12, h3(
                paste0(
                  "Feedback form to track quality and beneficiary suggestions continuously"))
              ),
              column(3, 
                     downloadButton('f2_downloadData', 'File download XLSX')),
              
              column(12, 
                     tags$br()
              ),
              tags$head(
                tags$style( 
                  type = 'text/css',
                  '.datatable{ overflow-x: scroll; }')
              ),
              ## Dataset 
              fluidRow(dataTableOutput("view_form2_df")
              )
      ),
      
      ## Data tab content- Financial register   ----
      tabItem(tabName = "form3_df",
              column(12, h3(
                paste0(
                  "Financial register for logging expenditures continuously"))
              ),
              column(3, 
                     downloadButton('f3_downloadData', 'File download XLSX')),
              
              column(12, 
                     tags$br()
              ),
              tags$head(
                tags$style( 
                  type = 'text/css',
                  '.datatable{ overflow-x: scroll; }')
              ),
              ## Dataset 
              fluidRow(dataTableOutput("view_form3_df")
              )
      ),
      
      
      ## Tab output plots  ----
      tabItem(tabName = "outputs_f1",
              fluidRow(
                column(
                  10, h3("Output form for logging activities")
                ),
                column(
                  12, tags$br()
                ),
                ## plot
                box(width = 6,
                    column(
                      12, 
                      plotOutput(
                        "f1_state", 
                        height=360)
                    )
                ),
                ## plot
                box(width = 6,
                    column(
                      12, 
                      plotOutput(
                        "f1_lga", 
                        height=360)
                    )
                ),
                ## plot
                box(width = 6,
                    column(
                      12, 
                      plotOutput(
                        "f1_result", 
                        height=360)
                    )
                ),
                ## plot
                box(width = 6,
                    column(
                      12, 
                      plotOutput(
                        "f1_participant", 
                        height=360)
                    )
                )
              )
      ),
      
      ## Tab feedback plots  ----
      tabItem(tabName = "feedback_f2",
              fluidRow(
                column(10, h3("Feedback form to track quality and beneficiary")
                ),
                column(
                  12, tags$br()
                ),
                ## plot
                box(width = 6,
                    column(
                      12, 
                      plotOutput(
                        "f2_state", 
                        height=360)
                    )
                ),
                ## plot
                box(width = 6,
                    column(
                      12, 
                      plotOutput(
                        "f2_lga", 
                        height=360)
                    )
                ),
                ## plot
                box(width = 6,
                    column(
                      12, 
                      plotOutput(
                        "f2_result", 
                        height=360)
                    )
                ),
                ## plot
                box(width = 6,
                    column(
                      12, 
                      plotOutput(
                        "f2_participant", 
                        height=360)
                    )
                )
              )
      ),
      
      tabItem(tabName = "evaluation_f2",
              fluidRow(
                column(10, h3("Evaluation to track quality")
                ),
                column(
                  12, tags$br()
                ),
                ## plot
                box(width = 12,
                    column(
                      12, 
                      plotOutput(
                        "f2_likert_8", 
                        height=360)
                    )
                ),
                ## plot
                box(width = 12,
                    column(
                      12, 
                      plotOutput(
                        "f2_likert_16", 
                        height=360)
                    )
                ),
                ## plot
                box(width = 12,
                    column(
                      12, 
                      plotOutput(
                        "f2_vgood", 
                        height=360)
                    )
                )
              )
      ),
      
      ## Tab finanaces plots  ----
      tabItem(tabName = "finan_f3",
              fluidRow(
                column(
                  10, 
                  h3("Financial register for logging expenditures")
                ),
                ## plot
                box(width = 6,
                    column(
                      12, 
                      plotOutput(
                        "f3_partner",
                        height=360
                      )
                    )
                ),
                ## plot
                box(width = 6,
                    column(
                      12, 
                      plotOutput(
                        "f3_activity_specific", 
                        height=360
                      )
                    )
                ),
                ## plot
                box(width = 6,
                    column(
                      12, 
                      plotOutput(
                        "f3_category", 
                        height=360
                      )
                    )
                ),
                ## plot
                box(width = 6,
                    column(
                      12, 
                      plotOutput(
                        "f3_payment", 
                        height=360
                      )
                    )
                )
              )
              
      ),
      
      tabItem(tabName = "budget_f3",
              fluidRow(
                column(
                  12, 
                  h3("Financial register for logging expenditures")
                ),
                ## plot
                box(width = 12,
                    column(
                      12, 
                      plotOutput(
                        "f3_budget",
                        height=360
                      )
                    )
                )
              ),
              column(12, h3(
                paste0(
                  "Expenses registered by partners for each budgetline"))
              ),
              column(3, 
                     downloadButton('f3_budgetline_downloadData', 'File download XLSX')),
              
              column(12, 
                     tags$br()
              ),
              tags$head(
                tags$style( 
                  type = 'text/css',
                  '.datatable{ overflow-x: scroll; }')
              ),
              ## Dataset 
              fluidRow(dataTableOutput("view_form3_budgetlines")
              )
              
      )
      
    ) #tabItems( #first one
  ) #dashboardBody
) #ui <- dashboardPage


#----------------------------------------#  
#----------------------------------------#

# Server ----

server <- function(input, output) {
  
  
  # Converts dataframes into reactive values ----
  
  form_1_filter = reactiveVal(form_1)
  form_2_filter = reactiveVal(form_2)
  form_3_filter = reactiveVal(form_3)
  
  
  
  
  reactive_dfs <- reactive({
    
    # Filter the reactive data frames ----
    
    source("scripts/input_filters_reactive.R",
           local = T)
    
    
    source("scripts/computes_indicators.R",
           encoding = "UTF-8",
           local = T)
    
    source("scripts/likert_data.R",
           encoding = "UTF-8",
           local = T)
    
    
    # Data frames from source("scripts/input_filters_reactive.R", local = T)
    
    list(
      form_1 = form_1, 
      form_2 = form_2,
      form_3 = form_3,
      indicator_table = indicator_table,
      form_2_likert = form_2_likert,
      form_2_vgood = form_2_vgood
    )
    
    
  })
  
  
  
  output$valuebox_forecast_f1_benef <- shinydashboard::renderValueBox({
    
    # Value box - forecast of beneficiaries ----
    shinydashboard::valueBox(
      
      textOutput("value_forecast_f1_benef"), 
      paste0("Forecast of beneficiaries on ", 
             lubridate::ymd(input$future_date) %>% 
               format("%d/%m/%Y")),
      icon = icon("chart-line"), 
      color = "light-blue"
    )
    
  })
  
  
  ## Calculates forecast value ----
  
  
  value_forecast_f1_benef <- reactive({
    
    form_1 <- reactive_dfs()[["form_1"]]
    
    
    forecast_form_1 <- form_1 %>% 
      
      select(filter_date, unique_participants) %>% 
      
      rbind(

        data.frame("filter_date" = paste0(today() %>% year()-1, "-01-01"),
                   "unique_participants" = as.numeric(0) )
      ) %>%
      
      na.omit() %>% 
      tibble::as_tibble() %>% 
      group_by(filter_date) %>%
      dplyr::summarise(Beneficiaries = sum(as.numeric(unique_participants)),
                       .groups = "drop_last") %>%
      ungroup() %>% 
      rename(Date = filter_date)
    
    
    if(nrow(forecast_form_1) < 2) {
      
      "Data is not enough"
      
    }else{
      
      forecast_form_1 <- forecast_form_1 %>% 
        mutate(
          Date = as.Date(Date),
          Beneficiaries = as.numeric(Beneficiaries)
        )
      
      # create variables of the week and month of each observation:
      forecast_form_1$Month <- as.Date(cut(forecast_form_1$Date,
                                           breaks = "month"))
      forecast_form_1$Week <- as.Date(cut(forecast_form_1$Date,
                                          breaks = "week",
                                          # changes weekly break point to Sunday
                                          start.on.monday = FALSE)) 
      
      
      # Registrations by week
      forecast_form_1_week <- forecast_form_1 %>%
        group_by(Week) %>%
        dplyr::summarise(Beneficiaries = sum(Beneficiaries),
                         .groups = "drop_last")
      
      actual_value <- sum(forecast_form_1_week$Beneficiaries, na.rm = T)
      
      forecast_value <- lm(cumsum(Beneficiaries) ~ Week, 
                           data = forecast_form_1_week) %>%
        predict(., newdata = 
                  data.frame(Week = as.Date(lubridate::ymd(input$future_date)))
        ) %>%
        as.numeric() %>%
        round(., -1) 
      
      if(forecast_value < actual_value){
        forecast_value %>% sum(., actual_value) %>% 
          format(big.mark = ",", decimal.mark = ".")
        
      }else{
        
        forecast_value %>%
          format(big.mark = ",", decimal.mark = ".")
      }
      
    }
    
    
  })
  
  output$value_forecast_f1_benef <- renderText({
    paste0(value_forecast_f1_benef())
  })
  
  # Value box - registrations ----
  
  f1_registrations_vbox <- reactive({
    
    form_1 <- reactive_dfs()[["form_1"]]
    
    form_1 %>%
      nrow() %>%
      format(big.mark = ",", decimal.mark = ".")
    
  })
  
  output$f1_registrations_vbox <- renderText({
    paste0(f1_registrations_vbox())
  })
  
  
  f2_registrations_vbox <- reactive({
    
    form_2 <- reactive_dfs()[["form_2"]]
    
    form_2 %>%
      nrow() %>%
      format(big.mark = ",", decimal.mark = ".")
    
  })
  
  output$f2_registrations_vbox <- renderText({
    paste0(f2_registrations_vbox())
  })
  
  
  f3_registrations_vbox <- reactive({
    
    form_3 <- reactive_dfs()[["form_3"]]
    
    form_3 %>%
      nrow() %>%
      format(big.mark = ",", decimal.mark = ".")
    
  })
  
  output$f3_registrations_vbox <- renderText({
    paste0(f3_registrations_vbox())
  })
  
  
  f1_participants_vbox <- reactive({
    
    form_1 <- reactive_dfs()[["form_1"]]
    
    form_1 %>% 
      pull(unique_participants) %>% 
      sum(., na.rm = T) %>%
      format(big.mark = ",", decimal.mark = ".")
    
  })
  
  output$f1_participants_vbox <- renderText({
    paste0(f1_participants_vbox())
  })
  
  # Value box - number of women ----
  
  f1_women_vbox <- reactive({
    
    form_1 <- reactive_dfs()[["form_1"]]
    
    paste0(
      round(
        (
          form_1 %>% 
            pull(outputs_women) %>% 
            sum(., na.rm = T) %>% 
            round()  
          
        ) / 
          
          (
            form_1 %>% 
              pull(unique_participants) %>% 
              sum(., na.rm = T) %>% 
              round() 
          ) * 100, 1
      ), "%"
    )
    
  })
  
  output$f1_women_vbox <- renderText({
    paste0(f1_women_vbox())
  })
  
  #------------------------------#
  
  
  # Prints plot timeline (dashboard) ----
  output$timeline <- timevis::renderTimevis({
    timevis::timevis(timeline,
                     fit = TRUE,
                     zoomFactor = 0.5,
                     option = list(zoomable=FALSE))
  })
  
  # Indicators    ----
  
  ## Indicator SO1  ----
  output$kpi_SO1 <- flexdashboard::renderGauge({
    
    indicator_table <- reactive_dfs()[["indicator_table"]]
    
    
    indSO1_value <- indicator_table$value[indicator_table$indicator == "indSO1_value"]
    
    min = 0 
    max = 4140
    
    danger = c(min, floor(max/2))
    warning = c(floor(max/2)+1, max-1)
    success = c(max, max+1)
    
    gauge(
      indSO1_value,
      min = min, max = max,
      symbol = '',
      label = "People", 
      gaugeSectors(
        success = success,
        warning = warning, 
        danger = danger
      )
    )
    
  })
  
  
  ## Indicator SO2   ----
  
  output$kpi_SO2 <- flexdashboard::renderGauge({
    
    indicator_table <- reactive_dfs()[["indicator_table"]]
    
    indSO2_value <- indicator_table$value[indicator_table$indicator == "indSO2_value"]
    
    min = 0 
    max = 16
    
    danger = c(min, floor(max/2))
    warning = c(floor(max/2)+1, max-1)
    success = c(max, max+1)
    
    gauge(
      indSO2_value,
      min = min, max = max,
      symbol = '',
      label = "Advocacy meetings", 
      gaugeSectors(
        success = success,
        warning = warning, 
        danger = danger
      )
    )
    
    
  })
  
  ## Indicator SO3   ----
  
  output$kpi_SO3 <- flexdashboard::renderGauge({
    
    indicator_table <- reactive_dfs()[["indicator_table"]]
    
    indSO3_value <- indicator_table$value[indicator_table$indicator == "indSO3_value"]
    
    min = 0 
    max = 30
    
    danger = c(min, floor(max/2))
    warning = c(floor(max/2)+1, max-1)
    success = c(max, max+1)
    
    gauge(
      indSO3_value,
      min = min, max = max,
      symbol = '',
      label = "Community groups", 
      gaugeSectors(
        success = success,
        warning = warning, 
        danger = danger
      )
    )
    
  })
  
  ## Indicator SO4   ----
  
  output$kpi_SO4 <- flexdashboard::renderGauge({
    
    indicator_table <- reactive_dfs()[["indicator_table"]]
    
    indSO4_value <- indicator_table$value[indicator_table$indicator == "indSO4_value"]
    
    min = 0 
    max = 80
    
    danger = c(min, floor(max/2))
    warning = c(floor(max/2)+1, max-1)
    success = c(max, max+1)
    
    gauge(
      indSO4_value,
      min = min, max = max,
      symbol = '%',
      label = "Percentage",
      gaugeSectors(
        success = success,
        warning = warning, 
        danger = danger
      )
    )
    
    
  })
  
  #------------------------------------# 
  
  # Renders the indicators gauge plots for UI ----
  
  output$ind_comments <- renderUI({
    
    div(
      
      fluidRow(
        box(column(12, h5("Indicator SO1: Number of people directly benefiting from EU-supported interventions that specifically aim to support civilian post-conflict peace building and/or conflict prevention: (target: 4140)")),
            
            column(flexdashboard::gaugeOutput("kpi_SO1"),
                   width=12,
                   title="Indicator SO1"
                   #title= ind_comments(1)
            ),
            height=240
        ),
        
        box(column(12, h5("Indicator SO2: Policy implementation and monitoring initiatives with CSO and CBOs supported by the action: (target: 16)")),
            
            column(flexdashboard::gaugeOutput("kpi_SO2"),
                   width=12,
                   title="Indicator SO2"
            ),
            height=240
        )
      ),
      
      fluidRow(
        box(column(12, h5("Indicator SO3: Number of community-based groups, CSOs, and CBOs supported by the action represented in State_A and State_B: (target: 30)")),
            
            column(flexdashboard::gaugeOutput("kpi_SO3"),
                   width=12,
                   title="Indicator SO3"
                   #title= ind_comments(1)
            ),
            height=240
        ),
        
        box(column(12, h5("Indicator SO4: % of participants in surveys with a positive opinion about community-based groups, CBOs, and CSOs’ managerial, organisational, and technical capacities and credibility in the target communities: (target: 80%)")),
            
            column(flexdashboard::gaugeOutput("kpi_SO4"),
                   width=12,
                   title="Indicator SO4"
            ),
            height=240
        )
      )
    ) #div
    
  })
  
  #------------------------------------#  
  
  # Plots ----
  
  ## Form 1 - State -----
  
  
  f1_state <- reactive({
    
    form_1 <- reactive_dfs()[["form_1"]]
    
    # Plot
    topbarplot(
      form_1$internal_state,
      max.obs = 10,
      title = "State where this task is being implemented:") +
      theme(axis.title.x = element_blank())
    
  })
  
  output$f1_state <- renderPlot({
    print(f1_state())
  }, execOnResize = TRUE)
  
  
  ## Form 1 - LGA -----
  
  f1_lga <- reactive({
    
    form_1 <- reactive_dfs()[["form_1"]]
    # Plot
    topbarplot(
      form_1$internal_lga,
      max.obs = 10,
      title = "LGA (local-government area) where this task is being implemented:") +
      theme(axis.title.x = element_blank())
    
  })
  
  output$f1_lga <- renderPlot({
    print(f1_lga())
  }, execOnResize = TRUE)
  
  
  ## Form 1 - Project results -----
  
  f1_result <- reactive({
    
    form_1 <- reactive_dfs()[["form_1"]]
    
    # Plot
    topbarplot(
      form_1$internal_result,
      max.obs = 10,
      title = "Project result to which the task being reported is related:") +
      theme(axis.title.x = element_blank())
    
  })
  
  output$f1_result <- renderPlot({
    print(f1_result())
  }, execOnResize = TRUE)
  
  ## Form 1 - Project participants -----
  
  f1_participant <- reactive({
    
    form_1 <- reactive_dfs()[["form_1"]]
    
    multibarplot(
      df = form_1, 
      variable = "internal_participant",
      title = "Type of participants / public:", 
      brewerpal = "RdYlGn",
      direction = -1
    )
    
  })
  
  output$f1_participant <- renderPlot({
    print(f1_participant())
  }, execOnResize = TRUE)
  
  
  ## Form 2 - State -----
  
  f2_state <- reactive({
    
    form_2 <- reactive_dfs()[["form_2"]]
    
    # Plot
    topbarplot(
      form_2$internal_state,
      max.obs = 10,
      title = "State where this task is being implemented:") +
      theme(axis.title.x = element_blank())
    
  })
  
  output$f2_state <- renderPlot({
    print(f2_state())
  }, execOnResize = TRUE)
  
  ## Form 2 - LGA -----
  
  f2_lga <- reactive({
    
    form_2 <- reactive_dfs()[["form_2"]]
    
    # Plot
    topbarplot(
      form_2$internal_lga,
      max.obs = 10,
      title = "LGA (local-government area) where this task is being implemented:") +
      theme(axis.title.x = element_blank())
    
  })
  
  output$f2_lga <- renderPlot({
    print(f2_lga())
  }, execOnResize = TRUE)
  
  ## Form 2 - Project results -----
  
  f2_result <- reactive({
    
    form_2 <- reactive_dfs()[["form_2"]]
    
    # Plot
    topbarplot(
      form_2$internal_result,
      max.obs = 10,
      title = "Project result to which the task being reported is related:") +
      theme(axis.title.x = element_blank())
    
  })
  
  output$f2_result <- renderPlot({
    print(f2_result())
  }, execOnResize = TRUE)
  
  ## Form 2 - Project participants -----
  
  f2_participant <- reactive({
    
    form_2 <- reactive_dfs()[["form_2"]]
    
    multibarplot(
      df = form_2, 
      variable = "internal_participant",
      title = "Type of participants / public:", 
      brewerpal = "RdYlGn",
      direction = -1
    )
    
  })
  
  output$f2_participant <- renderPlot({
    print(f2_participant())
  }, execOnResize = TRUE)
  
  
  ## Form 2 - Likert plots ---- 
  output$f2_likert_8 <- renderPlot({
    
    
    form_2_likert <- reactive_dfs()[["form_2_likert"]]
    
    sjPlot::plot_likert(form_2_likert[1:8], 
                        #title = "Participants feedback", 
                        legend.title = "",
                        reverse.colors = TRUE, 
                        values = "sum.outside", 
                        show.prc.sign = TRUE,
                        geom.size = 0.5, 
                        reverse.scale = TRUE,
                        geom.colors = "RdYlGn",
                        show.n = TRUE,
                        # limit the likert count evenly
                        catcount = 6,
                        intercept.line.color = "blue",
                        wrap.labels = 60,
                        # limits of the x-axis-range
                        grid.range = c(1.4, 1.09),
                        expand.grid = TRUE,
                        # breaks down the interval of the x-axis percentages
                        grid.breaks = 0.2,
                        cat.neutral = 4,
                        digits =0
    ) + 
      theme_minimal() +
      theme(axis.text.y = element_text(size = 11))
    
  })
  
  output$f2_likert_16 <- renderPlot({
    
    form_2_likert <- reactive_dfs()[["form_2_likert"]]
    
    sjPlot::plot_likert(form_2_likert[9:16], 
                        #title = "Participants feedback", 
                        legend.title = "",
                        reverse.colors = TRUE, 
                        values = "sum.outside", 
                        show.prc.sign = TRUE,
                        geom.size = 0.5, 
                        reverse.scale = TRUE,
                        geom.colors = "RdYlGn",
                        show.n = TRUE,
                        # limit the likert count evenly
                        catcount = 6,
                        intercept.line.color = "blue",
                        wrap.labels = 60,
                        # limits of the x-axis-range
                        grid.range = c(1.4, 1.09),
                        expand.grid = TRUE,
                        # breaks down the interval of the x-axis percentages
                        grid.breaks = 0.2,
                        cat.neutral = 4,
                        digits =0
    ) + 
      theme_minimal() +
      theme(axis.text.y = element_text(size = 11))
    
  })
  
  output$f2_vgood <- renderPlot({
    
    form_2_vgood <- reactive_dfs()[["form_2_vgood"]]
    
    sjPlot::plot_likert(form_2_vgood, 
                        #title = "Participants feedback", 
                        legend.title = "",
                        reverse.colors = TRUE, 
                        values = "sum.outside", 
                        show.prc.sign = TRUE,
                        geom.size = 0.5, 
                        reverse.scale = TRUE,
                        geom.colors = "RdYlGn",
                        show.n = TRUE,
                        # limit the likert count evenly
                        catcount = 4,
                        intercept.line.color = "blue",
                        wrap.labels = 60,
                        # limits of the x-axis-range
                        grid.range = c(1.4, 1.09),
                        expand.grid = TRUE,
                        # breaks down the interval of the x-axis percentages
                        grid.breaks = 0.2,
                        cat.neutral = 3,
                        digits =0
    ) + 
      theme_minimal() +
      theme(axis.text.y = element_text(size = 11))
    
  })
  
  ## Form 3 - Project partners -----
  output$f3_partner <- renderPlot({
    
    form_3 <- reactive_dfs()[["form_3"]]
    
    # Plot
    topbarplot(
      form_3$internal_implementing_partner,
      max.obs = 10,
      title = "Implementing partner:") +
      theme(axis.title.x = element_blank())
    
  }, execOnResize = TRUE)
  
  ## Form 3 - Project budget -----
  
  
  f3_budget <- reactive({
    
    form_3 <- reactive_dfs()[["form_3"]]
    
    form_3_budget_plot <- form_3 %>%
      select(internal_implementing_partner, expend_category_names, expend_amount_eur)  %>%
      ## Omits NAs
      na.omit() %>% 
      
      ## Groups observations
      group_by(internal_implementing_partner, expend_category_names) %>%
      ## Count observations
      dplyr::summarise(
        expend_amount_eur = sum(as.numeric(expend_amount_eur)) %>% 
          round(., 2)
        
      ) %>%
      
      ## Ungroups variables
      ungroup() %>% 
      
      ## Arranges results in descending order
      arrange(internal_implementing_partner, expend_category_names)
    
    
    ### plot stacked ----
    
    
    totals_disaggregated <- xls_budget_f4p_budgetline %>% 
      
      filter(grepl("^1\\.|^2\\.|^3\\.|^4\\.|^5\\.|^6\\.", expend_budgetline)) %>% 
      
      filter(total_costs_eur_2 != "0") %>% 
      mutate(internal_implementing_partner =
               data.table::fifelse(
                 grepl("Partner B", expend_category_budgetline),
                 "Partner-B",
                 data.table::fifelse(
                   grepl("Partner A", expend_category_budgetline),
                   "Partner-A",
                   data.table::fifelse(
                     grepl("Partner C", expend_category_budgetline),
                     "Partner-C",
                     "Other",
                   )
                 )
               )
      ) %>% 
      mutate(expend_category_names =
               data.table::fifelse(
                 grepl("^1\\.", expend_budgetline),
                 "Human resource",
                 data.table::fifelse(
                   grepl("^2\\.", expend_budgetline),
                   "Travel",
                   data.table::fifelse(
                     grepl("^3\\.", expend_budgetline),
                     "Equipment & supplies",
                     data.table::fifelse(
                       grepl("^4\\.", expend_budgetline),
                       "Local office",
                       data.table::fifelse(
                         grepl("^5\\.", expend_budgetline),
                         "Services",
                         data.table::fifelse(
                           grepl("^6\\.", expend_budgetline),
                           "Other",
                           expend_budgetline
                         )
                       )
                     )
                   )
                 )
               )
      ) %>% 
      
      ## Groups observations
      group_by(internal_implementing_partner, expend_category_names) %>%
      ## Count observations
      dplyr::summarise(
        total_costs_eur_2 = sum(as.numeric(total_costs_eur_2)) %>% 
          round(., 2)
        
      ) %>% 
      ungroup() %>%
      
      ## Arranges results in descending order
      arrange(internal_implementing_partner, expend_category_names)
    
    
    form_3_budget_plot <- form_3_budget_plot %>% 
      merge(.,
            totals_disaggregated,
            by = c("internal_implementing_partner", "expend_category_names"),
            all = T) %>% 
      mutate(total_costs_eur_2 =
               data.table::fifelse(
                 is.na(total_costs_eur_2),
                 0,
                 total_costs_eur_2
               )) %>% 
      mutate(expend_amount_eur =
               data.table::fifelse(
                 is.na(expend_amount_eur),
                 0,
                 expend_amount_eur
               )) %>% 
      arrange(internal_implementing_partner, expend_category_names) %>% 
      
      #### Filters by partners ----
    filter(internal_implementing_partner %in% form_3$internal_implementing_partner) %>% 
      
      group_by(expend_category_names) %>%
      
      dplyr::summarise(internal_implementing_partner = internal_implementing_partner,
                       expend_amount_eur= expend_amount_eur,
                       total_costs_eur_2 = total_costs_eur_2,
                       total_costs_by_category = sum(as.numeric(total_costs_eur_2))
      ) %>%
      
      ungroup() %>% 
      
      group_by(internal_implementing_partner, expend_category_names) %>%
      ## Count observations
      dplyr::summarise(
        expend_amount_eur= expend_amount_eur,
        total_costs_eur_2 = total_costs_eur_2,
        total_costs_by_category=total_costs_by_category,
        percent = round(expend_amount_eur/as.numeric(total_costs_by_category) *100, 2)
      ) %>%
      ungroup() %>% 
      arrange(internal_implementing_partner, expend_category_names) %>% 
      mutate(percent =
               data.table::fifelse(
                 is.infinite(percent),
                 0,
                 percent
               )) %>% 

      
      dplyr::mutate(
        expend_category_names = 
          ordered(expend_category_names, levels = c(
            "Human resource",
            "Travel",
            "Equipment & supplies",
            "Local office",
            "Services",
            "Other"
          )
          )
      )

    form_3_budget_maxperc <- form_3_budget_plot %>% 
      group_by(expend_category_names) %>%
      ## Count observations
      dplyr::summarise(
        percent_category = sum(as.numeric(percent)) %>% 
          round(., 2)
        
      ) %>% 
      ungroup() 

    # Calculates totals
    totals <- form_3_budget_plot %>% 
      select(expend_category_names, total_costs_eur_2) %>% 
      group_by(expend_category_names) %>%
      dplyr::summarise(
        total_costs_eur_2 = sum(as.numeric(total_costs_eur_2)) %>% 
          round(., 2)
        
      ) %>%
      ungroup() %>% 
      mutate(# set percent value (x-axis value) at which total value should be pasted.
        percent = max(form_3_budget_maxperc$percent_category)
      ) 
    
    data = form_3_budget_plot 
    key_variable = "expend_category_names" 
    facet_variable = "internal_implementing_partner"
    ylab = "% spent from allocated budget"
    axis_text_size = 13
    textsize = 4
    angle = 0
    title = "Budget plan and expenditures"
    title_size = 15
    
    p <- ggplot(data = form_3_budget_plot,
                aes(x = forcats::fct_rev(expend_category_names),
                    y = percent,
                    fill = internal_implementing_partner))  +
      geom_bar(
        stat = "identity", 
        position = position_stack(reverse = TRUE),
        width = 0.5
      ) +

      ## shows empty factor level
      scale_x_discrete(drop=FALSE) + 
      
      # Sets plot theme
      theme_bw()  +
      
      # Formats the legends labels
      theme(legend.title=element_text(size=axis_text_size * 1.1), 
            legend.text=element_text(size=axis_text_size * 0.9)) +
      
      #  Prepares plot annotations in % (uses package scales)
      # Prints values
      geom_text(
        
        data = form_3_budget_plot,
        aes(
          label =
            paste0(
              round(percent, 0)
            )
          
        ),
        position = position_stack(vjust=0.6, reverse = TRUE),
        color = "black", size = textsize * 0.9
      ) +
      
      theme(#axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      
      # Sets axis formatting
      theme(
        axis.text.x =
          element_text(
            angle = 0, vjust=.5, hjust = .5
          )
      ) +
      
      # Sets axis formatting
      theme(
        axis.text.y =
          element_text(
            angle = 0
          )
      ) +
      
      # Sets title and subtitle (incl. str_wrap for text wrapping)
      ggtitle(
        label= title
      ) +
      
      # comment the plot.title below if textgrob is used to paste title on ggplot
      theme(
        
        plot.title = element_text(size = title_size, face = "bold"),
        plot.subtitle = element_blank(),
        plot.title.position = "plot"
      ) +
      
      # Adjust scales
      scale_y_continuous(
        limits = c(0, (max(totals$percent) * 1.18)), expand = c(0.01, 0))  +
      # Defines label for y axis (must be defined upon function call)
      labs(
        y = ylab,
        fill = "Partner"
      ) +

      scale_fill_manual("Partner", values = c("Partner-B" = "lightgreen", 
                                              "Partner-A" = "lightblue", 
                                              "Partner-C" = "pink")) +
      
      theme(axis.title.y = element_blank()) +
      
      # # Prints total values of allocated budget
      geom_text(data=totals ,
                aes(
                  x=expend_category_names,
                  y=percent * 1.09,
                  label=as.numeric(total_costs_eur_2) %>% format(big.mark = ","), 
                  fill = NULL
                ),
                #nudge_y = 5,
                color = "blue", 
                size = textsize * 1.1
      ) +
      
      ggplot2::annotate(geom = "text", label = "Allocated Budget (Euro)", 
                        x = Inf, y = max(totals$percent) * 1.05, 
                        color = "blue", 
                        #hjust = 0.8, 
                        vjust = 1.6, 
                        size = textsize * 0.9, 
                        fontface = "italic") +
      
      
      # Flips plot
      coord_flip() +
      # Axis text size
      theme(axis.text = element_text(size = axis_text_size)) +
      
      # Sets options for x axis title
      theme(
        axis.title.x =
          element_text(size = title_size * 0.8,
                       angle = 00)) 
    p

  })
  
  output$f3_budget <- renderPlot({
    print(f3_budget())
  }, execOnResize = TRUE)
  
  
  #------------------------------------#  
  ## Financial budgetline Dataframe ----
  
  
  
  form_3_budgetline_table <- reactive({
    
    form_3 <- reactive_dfs()[["form_3"]]
    
    form_3_df_budgetline <- form_3 %>%
      select(internal_implementing_partner, expend_category_names, expend_budgetline, expend_amount_eur)  %>%
      ## Omits NAs
      na.omit() %>% 
      
      ## Groups observations
      group_by(internal_implementing_partner, expend_category_names, expend_budgetline) %>%
      ## Count observations
      dplyr::summarise(
        expend_amount_eur = sum(as.numeric(expend_amount_eur)) %>% 
          round(., 2)
        
      ) %>%
      
      ## Ungroups variables
      ungroup() %>% 
      
      ## Arranges results in descending order
      arrange(expend_budgetline, expend_category_names, internal_implementing_partner)
    
    
    ### The budgetline
    
    totals_disaggregated <- xls_budget_f4p_budgetline %>% 
      
      filter(grepl("^1\\.|^2\\.|^3\\.|^4\\.|^5\\.|^6\\.", expend_budgetline)) %>% 
      
      filter(total_costs_eur_2 != "0") %>% 
      mutate(total_costs_eur_2 = as.numeric(total_costs_eur_2)) %>% 
      mutate(internal_implementing_partner =
               data.table::fifelse(
                 grepl("Partner B", expend_category_budgetline),
                 "Partner-B",
                 data.table::fifelse(
                   grepl("Partner A", expend_category_budgetline),
                   "Partner-A",
                   data.table::fifelse(
                     grepl("Partner C", expend_category_budgetline),
                     "Partner-C",
                     "Other",
                   )
                 )
               )
      ) %>% 
      
      mutate(expend_category_names =
               data.table::fifelse(
                 grepl("^1\\.", expend_budgetline),
                 "Human resource",
                 data.table::fifelse(
                   grepl("^2\\.", expend_budgetline),
                   "Travel",
                   data.table::fifelse(
                     grepl("^3\\.", expend_budgetline),
                     "Equipment & supplies",
                     data.table::fifelse(
                       grepl("^4\\.", expend_budgetline),
                       "Local office",
                       data.table::fifelse(
                         grepl("^5\\.", expend_budgetline),
                         "Services",
                         data.table::fifelse(
                           grepl("^6\\.", expend_budgetline),
                           "Other",
                           expend_budgetline
                         )
                       )
                     )
                   )
                 )
               )
      ) %>% 
      
      ## Arranges results in descending order
      arrange(internal_implementing_partner, expend_category_names)
    
    
    form_3_df_budgetline <- form_3_df_budgetline %>% 
      merge(.,
            totals_disaggregated,
            by = c("internal_implementing_partner", "expend_category_names", "expend_budgetline"),
            all = T) %>% 
      mutate(total_costs_eur_2 =
               data.table::fifelse(
                 is.na(total_costs_eur_2),
                 0,
                 total_costs_eur_2
               )) %>% 
      mutate(expend_amount_eur =
               data.table::fifelse(
                 is.na(expend_amount_eur),
                 0,
                 expend_amount_eur
               )) %>% 
      mutate(expend_category_budgetline =
               data.table::fifelse(
                 is.na(expend_category_budgetline),
                 "NOT Defined in the Budget Revision",
                 expend_category_budgetline
               )) %>% 
      
      
      mutate(
        remaining = round(total_costs_eur_2-expend_amount_eur, 2)
      ) %>%
      
      mutate(remaining =
               data.table::fifelse(
                 is.infinite(remaining),
                 0,
                 remaining
               )) %>% 
      ### Filters by partners ----
    filter(internal_implementing_partner %in% form_3$internal_implementing_partner) %>% 
      
      select(internal_implementing_partner, 
             expend_category_names,
             expend_budgetline,
             expend_category_budgetline,  
             expend_amount_eur, 
             total_costs_eur_2,
             remaining) %>% 
      
      arrange(internal_implementing_partner, 
              expend_budgetline) %>% 
      
      rename(`Implementing partner` = internal_implementing_partner,
             `Expenditure category` = expend_category_names,
             `Budgetline` = expend_budgetline,
             `Budgetline category` = expend_category_budgetline,
             `Spent (Euro)` = expend_amount_eur,
             `Allocated budget (Euro)` = total_costs_eur_2,
             `Remaining budget (Euro)` = remaining)
    
  })
  
  #------------------------------------#  
  ## Outputs budgetine data ----
  output$view_form3_budgetlines <- DT::renderDataTable({
    
    form_3_budgetline_table <- form_3_budgetline_table() %>% as_tibble()
    
    DT::datatable(
      form_3_budgetline_table, 
      class = 'cell-border stripe',
      options = list(
        scrollX = TRUE,
        pageLength = 50,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/English.json'
        )
      )
    )
  })
  
  ## Form 3 - Project activity specific -----
  output$f3_activity_specific <- renderPlot({
    
    form_3 <- reactive_dfs()[["form_3"]]
    
    # Plot
    topbarplot(
      form_3$expend_activity_specific,
      max.obs = 10,
      title = "Is this an activity-specific expenditure?") +
      theme(axis.title.x = element_blank())
    
  }, execOnResize = TRUE)
  
  ## Form 3 - Project cost category -----
  output$f3_category <- renderPlot({
    
    form_3 <- reactive_dfs()[["form_3"]]
    
    # Plot
    topbarplot(
      form_3$expend_category,
      max.obs = 10,
      title = "Which cost category does the expense belong to?") +
      theme(axis.title.x = element_blank())
    
  }, execOnResize = TRUE)
  
  ## Form 3 - Project cost payment methods -----
  output$f3_payment <- renderPlot({
    
    form_3 <- reactive_dfs()[["form_3"]]
    
    # Plot
    topbarplot(
      form_3$expend_payment,
      max.obs = 10,
      title = "How was the payment made?") +
      theme(axis.title.x = element_blank())
    
  }, execOnResize = TRUE)
  
  
  #------------------------------------#  
  # Outputs dataframe ----
  
  form_1_df <- form_1 %>% 
    # Selects columns
    select(
      filter_date,
      
      
      any_of(
        xlsf_survey_form_1 %>%
          select(final_name) %>%
          unlist(use.names = FALSE)
      )
      
    ) %>% 
    
    # Sorts data by date
    arrange(desc(filter_date))  
  
  
  # # Renames variables
  
  form_1_df <- form_1_df %>% 
    
    rename(
      `Date of registration` = filter_date,
      `Implementing partner` = internal_implementing_partner
    )
  
  # Replaces the variable names by the values of another dataframe
  
  data.table::setnames(form_1_df, 
                       as.character(xlsf_survey_form_1$final_name), 
                       as.character(xlsf_survey_form_1$label_en), 
                       skip_absent = TRUE)
  
  
  
  #------------------------------------#  
  # Shows registration data
  output$view_form1_df <- DT::renderDataTable({
    
    
    if (length(input$input_partner) == 0) {
      
      form_1_df <- form_1_df %>% 
        # Filter data range
        filter(lubridate::ymd(`Date of registration`) >=  
                 lubridate::ymd(input$dateRange[1]),  
               lubridate::ymd(`Date of registration`) <= 
                 lubridate::ymd(input$dateRange[2]))
      
    }else{
      
      if (length(input$input_partner) > 0 
      ) {
        
        form_1_df <- form_1_df %>%
          
          filter(`Implementing partner` %in% input$input_partner) %>% 
          # Filter data range
          filter(lubridate::ymd(`Date of registration`) >= 
                   lubridate::ymd(input$dateRange[1]),  
                 lubridate::ymd(`Date of registration`) <= 
                   lubridate::ymd(input$dateRange[2])) 
      }
    }
    
    
    
    DT::datatable(
      form_1_df, 
      class = 'cell-border stripe',
      options = list(
        scrollX = TRUE,
        pageLength = 50,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/English.json'
        )
      )
    )
  })
  
  #------------------------------------#  
  # Feedback dataframe ----
  
  form_2_df <- form_2 %>% 
    # Selects columns
    select(
      filter_date,
      
      
      any_of(
        xlsf_survey_form_2 %>%
          select(final_name) %>%
          unlist(use.names = FALSE)
      )
      
    ) %>% 
    
    # Sorts data by date
    arrange(desc(filter_date))  
  
  # # Renames variables
  
  form_2_df <- form_2_df %>% 
    
    rename(
      `Date of registration` = filter_date,
      `Implementing partner` = internal_implementing_partner
    )
  
  # Replaces the variable names by the values of another dataframe
  
  data.table::setnames(form_2_df, 
                       as.character(xlsf_survey_form_2$final_name), 
                       as.character(xlsf_survey_form_2$label_en), 
                       skip_absent = TRUE)
  
  
  #------------------------------------#  
  # Shows registration data
  output$view_form2_df <- DT::renderDataTable({
    
    
    if (length(input$input_partner) == 0) {
      
      form_2_df <- form_2_df %>% 
        # Filter data range
        filter(lubridate::ymd(`Date of registration`) >=  
                 lubridate::ymd(input$dateRange[1]),  
               lubridate::ymd(`Date of registration`) <= 
                 lubridate::ymd(input$dateRange[2]))
      
    }else{
      
      if (length(input$input_partner) > 0 
      ) {
        
        form_2_df <- form_2_df %>%
          
          filter(`Implementing partner` %in% input$input_partner) %>% 
          # Filter data range
          filter(lubridate::ymd(`Date of registration`) >= 
                   lubridate::ymd(input$dateRange[1]),  
                 lubridate::ymd(`Date of registration`) <= 
                   lubridate::ymd(input$dateRange[2])) 
      }
    }
    
    
    DT::datatable(
      form_2_df, 
      class = 'cell-border stripe',
      options = list(
        scrollX = TRUE,
        pageLength = 50,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/English.json'
        )
      )
    )
  })
  
  #------------------------------------#  
  # Financial record dataframe ----
  
  form_3_df <- form_3 %>% 
    # Selects columns
    select(
      filter_date,
      
      
      any_of(
        xlsf_survey_form_3 %>%
          select(final_name) %>%
          unlist(use.names = FALSE)
      )
      
    ) %>% 
    
    select(-expend_currency,
           -expend_amount) %>% 
    # Sorts data by date
    arrange(desc(filter_date))  
  
  
  # # Renames variables
  
  form_3_df <- form_3_df %>% 
    
    rename(
      `Date of registration` = filter_date,
      `Implementing partner` = internal_implementing_partner
    )
  
  # Replaces the variable names by the values of another dataframe
  
  data.table::setnames(form_3_df, 
                       as.character(xlsf_survey_form_3$final_name), 
                       as.character(xlsf_survey_form_3$label_en), 
                       skip_absent = TRUE)
  
  
  #------------------------------------#  
  # Shows registration data
  output$view_form3_df <- DT::renderDataTable({
    
    
    if (length(input$input_partner) == 0) {
      
      form_3_df <- form_3_df %>% 
        # Filter data range
        filter(lubridate::ymd(`Date of registration`) >=  
                 lubridate::ymd(input$dateRange[1]),  
               lubridate::ymd(`Date of registration`) <= 
                 lubridate::ymd(input$dateRange[2]))
      
    }else{
      
      if (length(input$input_partner) > 0 
      ) {
        
        form_3_df <- form_3_df %>%
          
          filter(`Implementing partner` %in% input$input_partner) %>% 
          # Filter data range
          filter(lubridate::ymd(`Date of registration`) >= 
                   lubridate::ymd(input$dateRange[1]),  
                 lubridate::ymd(`Date of registration`) <= 
                   lubridate::ymd(input$dateRange[2])) 
      }
    }
    
    
    DT::datatable(
      form_3_df, 
      class = 'cell-border stripe',
      options = list(
        scrollX = TRUE,
        pageLength = 50,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/English.json'
        )
      )
    )
  })
  
  #------------------------------------#  
  # Function to allow users to download data ----
  output$f1_downloadData <- downloadHandler(
    
    filename = function() { paste(
      "Form_1_Output-",
      Sys.Date() %>% format(.,"%d-%b-%Y"),'.xlsx', sep='') },
    content = function(file) {
      writexl::write_xlsx(
        form_1_df, path = file)
    }
  )
  
  output$f2_downloadData <- downloadHandler(
    
    filename = function() { paste(
      "Form_2_Feedback-",
      Sys.Date() %>% format(.,"%d-%b-%Y"),'.xlsx', sep='') },
    content = function(file) {
      writexl::write_xlsx(
        form_2_df, path = file)
    }
  )
  
  output$f3_downloadData <- downloadHandler(
    
    filename = function() { paste(
      "Form_3_Financial-",
      Sys.Date() %>% format(.,"%d-%b-%Y"),'.xlsx', sep='') },
    content = function(file) {
      writexl::write_xlsx(
        form_3_df, path = file)
    }
  )
  
  output$f3_budgetline_downloadData <- downloadHandler(
    
    filename = function() { paste(
      "Form_3_Financial_Budgetline-",
      Sys.Date() %>% format(.,"%d-%b-%Y"),'.xlsx', sep='') },
    content = function(file) {
      writexl::write_xlsx(
        form_3_budgetline_table() %>% as_tibble(), path = file)
    }
  )
  
  
  # #------------------------------------#  
  # # Generate report ----
  
  # This creates a short-term storage location for a filepath
  report <- reactiveValues(filepath = NULL) 
  
  
  observeEvent(input$generate, {
    
    
    shinyjs::disable(id="generate")
    
    # Create a Progress object
    
    ##Insert code to build document
    # Set up parameters to pass to Rmd document
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <-
      file.path(tempdir(),
                "partner_report.Rmd")
    # Copy files to temporary report
    file.copy(
      "partner_report.Rmd",
      tempReport,
      overwrite = TRUE
    )
    params <-
      list(
        partner = input$input_partner,
        start_date = lubridate::ymd(input$dateRange[1]),
        end_date = lubridate::ymd(input$dateRange[2]),
        future_date = lubridate::ymd(input$future_date),
        form_1 = reactive_dfs()[["form_1"]],
        form_2 = reactive_dfs()[["form_2"]],
        form_3 = reactive_dfs()[["form_3"]],
        
        value_boxes = list(
          "f1_registrations_vbox" = f1_registrations_vbox(),
          "f2_registrations_vbox" = f2_registrations_vbox(),
          "f3_registrations_vbox" = f3_registrations_vbox(),
          "f1_participants_vbox" = f1_participants_vbox(),
          "f1_women_vbox" = f1_women_vbox(),
          "value_forecast_f1_benef" = value_forecast_f1_benef()
        ),
        
        plots = list(
          "f1_state" = f1_state(), 
          "f1_lga" = f1_lga(),
          "f3_budget" = f3_budget()
        ),
        
        indicator_table = reactive_dfs()[["indicator_table"]],
        form_2_likert = reactive_dfs()[["form_2_likert"]],
        form_2_vgood = reactive_dfs()[["form_2_vgood"]]
        
      )
    
    
    file <- paste0(tempfile(), ".html")
    
    withProgress(message = "Generating report... Please wait",
                 detail="The download button will appear in the sidebar panel at the end of the progress",
                 value=1,{
                   rmarkdown::render(
                     tempReport, output_file = file,
                     params = params,
                     envir =
                       new.env(parent = globalenv()
                       )
                   )
                   
                 })
    
    report$filepath <- file 
    
  })
  
  ## Creates list of main select inputs
  input_list <- reactive({
    list(input$input_partner,input$dateRange[1], input$dateRange[2])
  })
  
  ## Uses list of inputs to make downloadbutton disappear and enable generate report button
  observeEvent(input_list(), {
    report$filepath <- NULL
    
    shinyjs::enable(id="generate")
  })
  
  # Hide download button until report is generated
  output$reportbuilt <- reactive({
    return(!is.null(report$filepath))
    
  })
  
  outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)
  
  #Download report
  output$partner_report <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    # For PDF output, change this to "report.pdf"
    filename = paste0(
      "Template M&E report - ",
      Sys.Date(),
      ".html"
    ),
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      
      withProgress(message = "Rendering, please wait!", {
        
        file.copy(report$filepath, file)
        
      })
      
    }
    
  )
  
  
}

shinyApp(ui, server)