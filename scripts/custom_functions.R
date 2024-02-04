# Custom functions ----






# Converts data downloaded from Kobotoolbox (JSON) into R dataframes ----
fromkobo <- function(ona_data) {
  # ona_data = any JSON file resulting from an REST API GET request.
  
  # Loads required libraries
  library(httr, dplyr, jsonlite)
  
  
  # Reading data content as text 
  df <- httr::content(
    ona_data, 
    "text", 
    encoding ="UTF-8"
  ) %>% 
    ## Transforming into JSON (Java Script Oriented Notation)
    jsonlite::fromJSON(
      .,
      flatten = TRUE
    )
  
  # Saving data as a R dataframe  
  df <-  as.data.frame(df) %>% 
    ## converts all columns to character
    apply(2, as.character) %>% 
    tibble::as_tibble() 
  
  # Adjusts names of dataframe to valid R names
  names(df) <- make.names(names(df), 
                          unique = FALSE, allow_ = TRUE) 
  
  # Replacing dots in variable names for MongoDB
  names(df) <- gsub("\\.", "_", names(df)) 
  
  # Replacing X_ in variable names for simplification
  names(df) <- gsub("^X_|^_", "", names(df)) 
  
  # Replacing _ in beginning of variable names for simplification
  names(df) <- gsub("^_", "", names(df)) 
  
  # Replacing __ in variable names for simplification
  names(df) <- gsub("__", "", names(df)) 
  
  # Parsing file and transforming in tibble::as_tibble()
  df <- df %>%
    # Mutate all factor variables to character
    dplyr::mutate_if(is.factor, as.character) %>% 
    ## Reordering columns alphabetically
    dplyr::select(
      sort(
        tidyselect::peek_vars()
      )
    )
  
  # Returns dataframe
  df
  
}





# Plots probabilistic bootstrapped confidence intervals ----

topbootplot <- function(var, 
                        seed = 1234, 
                        col = "gold3", 
                        title = "Boot", 
                        font_main=1, 
                        font=1, 
                        xlab = "Mean") {
  ## var = numeric variable e.g. df$income
  ## stat = statistic
  ## seed = numeric input for seet.seed() reproducibility
  ## color = color
  ## title = plot title  
  
  library(bayesboot) # Requiring package
  
  # Checks if variable has available data
  if(
    var %>% 
    na.omit %>% 
    length() > 2
  ){
    
    ## Setting random generation seed to allow for reproducibility
    set.seed(seed)
    
    # Conducting Bayesian bootstrap (requires package "bayesboot")
    bp <- bayesboot::bayesboot(
      as.numeric(var) %>% 
        na.omit(), 
      mean
    )
    
    # Changing default x label of the plot as element of the function
    attr(bp, "statistic.label") <- xlab
    
    ## Ploting results of Bayesian bootstrap
    plot(bp, cex = font, 
         cex.lab = font, 
         cex.axis = font,
         cex.main = font_main, 
         cex.sub = font_main,
         col = col,
         main = title)
    
  }else{
    
    cat(paste("''",title,"''"), fill = TRUE, labels = paste0("No data available for"))
  }
}

# Simple barplots from tables ----
topbarplot <- function(column, 
                       title = "Some plot", 
                       label = "Responses", 
                       units = "responses",
                       brewerpal = "Accent",
                       max.obs = Inf, 
                       titlebreak = 60, 
                       size = 13, 
                       label_size = 4,
                       title_size = 15,
                       vjust = 0.5, 
                       hjust = -5, 
                       direction = 1, 
                       xlab_break = 90, 
                       big.mark = ".", 
                       decimal.mark = ",",
                       decreasing = TRUE,
                       ylable_sort = FALSE,
                       ylab_capitalization = "original"
                       
                       
) {
  ## table = some table
  ## title = string with plot title
  ## label = label of horizontal axis
  ## brewerpal = string with brewer.pal palette (see: https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf)
  ## max.obs = numeric value limiting the number of results to plot
  
  library(ggplot2)
  library(scales)
  library(stringr)
  library(dplyr)
  
  
  # Checks if variable has available data
  if(
    column %>% 
    na.omit %>% 
    length() > 0
  ){
    
    # Creates table from variable
    table <- sapply(
      base::strwrap(
        as.character(
          gsub("_", " ", column %>% na.omit()) 
          
        ), 
        width = xlab_break, 
        simplify=FALSE), 
      paste, collapse="\n")  %>% 
      table()
    
    if (length(table) == 1) {
      # Condition for plotting varibales with one level (e.g. subsets)
      ## Transforming auxiliary dataset
      table <- as.data.frame(table)
      
      
      
    }else{
      
      # Subsets dataset 
      table  <- table %>%
        ## Sorts table in decreasing order
        sort(decreasing = TRUE) %>%
        ## Converts table into a dataframe
        as.data.frame() %>%
        ## Heads max. of desired results
        head(max.obs) 
      
      if(decreasing)
        table[,1] <- factor(
          table[,1], levels=rev(sort(table[,1])))
      
    }
    
    if(ylable_sort){
      table <- table %>% 
        dplyr::mutate_if(is.factor, as.character) 
      names(table) <- c("values", "Freq")
      
      table$values <- as.numeric(table$values)
      
      table<- table %>% arrange(values)
      
      table$values <- as.factor(table$values)
      #names(table) <- c(".", "Freq")
    }else{
      
      table <- table
      
    }
    
    ## capitalization options
    
    if(ylab_capitalization == "all_upper"){
      table[,1] <- toupper(
        table[,1])
    }else{
      
      if(ylab_capitalization == "all_lower"){
        table[,1] <- tolower(
          table[,1])
      }else{
        ## capitalize first letter of first word 
        if(ylab_capitalization == "first_word"){
          table[,1] <- Hmisc::capitalize(
            tolower(
              table[,1]))
        }else{
          ## capitalize first letter of each word 
          if(ylab_capitalization == "first_letter"){
            table[,1] <- sapply(table[,1], simplecapital)
          }else{
            if(ylab_capitalization == "original"){
              ##if ylab_capitalization == "original"
              table <- table
            }
          }
        }
      }
    }
    
    
    p <- ggplot(table, aes(x=table[,1], fill = table[,1],
                           y=table[,2])) + labs(y = label) +
      geom_bar(position = "dodge", 
               stat = "identity") +
      theme_minimal() +
      coord_flip() +
      theme(legend.position="none")  +
      # Includes plot title
      ggtitle(
        stringr::str_wrap(title,
                          width = titlebreak),
        subtitle =
          paste("N =",
                format(sum(table[,2]),
                       big.mark = big.mark,
                       decimal.mark = decimal.mark), units)
      ) +
      # Removes title for y axis
      theme(axis.title.y = element_blank()) +
      # Prints values
      geom_text(
        aes(label =
              paste0(
                scales::percent(
                  table[,2]/table %>%
                    dplyr::select(Freq) %>%
                    unlist() %>%
                    sum(), accuracy = 0.1
                ),
                " (",
                table[,2],
                ")"
              )
        ),
        hjust = -0.1, vjust = 0.2, size = label_size)  +
      # Changes size and format of axis and title
      theme(axis.text=element_text(size = size),
            axis.title=element_text(size = size)) +
      theme(plot.title = element_text(size = title_size, face = "bold")) +
      # Adjust scales
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
                         limits = c(0, max(table[,2]*1.15)))
    
    # Runs condition on number of levels for setting colours
    if(nrow(table) >= RColorBrewer::brewer.pal.info["Accent",]["maxcolors"]){
      
      # Creates expanded Brewer palette
      colourCount = nrow(table)
      getPalette = colorRampPalette(
        brewer.pal(8, brewerpal)
      )
      
      p +  
        scale_fill_manual(
          values = 
            if(direction == -1){
              rev(getPalette(colourCount))
            }else{
              getPalette(colourCount)
            }
        )
      
    }else{ # For cases with over 12 levels due to color palette
      
      p + scale_fill_brewer(palette = brewerpal, 
                            direction = direction)
    }
    
    
  }else{
    
    ## Renders empty ggplot if data is not avaiable
    ggplot(df <- data.frame(), aes(x = "", y = "")) + geom_blank() +
      labs(y = label) +
      # geom_bar(position = "dodge", 
      #          stat = "identity") +
      theme_minimal() +
      coord_flip() +
      theme(legend.position="none")  +
      # Includes plot title
      ggtitle(
        stringr::str_wrap(title,
                          width = titlebreak),
        subtitle =
          paste("N = 0", units)
      ) +
      # Removes title for y axis
      theme(axis.title.y = element_blank()) +
      # Prints values
      geom_text(
        aes(label =
              paste0("No data available for"),
            colour= "red"
        ),
        hjust = -0.1, vjust = 0.2, size = label_size) +
      # Changes size and format of axis and title
      theme(axis.text=element_text(size = size),
            axis.title=element_text(size = size)) +
      theme(plot.title = element_text(size = title_size, face = "bold"))  
    
    #cat(paste("''",title,"''"), fill = TRUE, labels = paste0("No data available for"))
    
  }
  
}

# Custom function for sampling answers ----
topwordcloud <- function(var, 
                         min.freq = 2, 
                         stopwords = "", 
                         brewerpal = "Set3", 
                         brewerpalnr = 13, 
                         seed = 1234, 
                         language = "english",  
                         scale = c(3.5,.5), 
                         title = NA, 
                         cex=1.75, 
                         legend_height=0.25) {
  # Custom function
  ## var = text variable such as df$var
  ## min.freq = minimum frequency of mentions
  ## stopwords = custom words to be excluded from the plot
  ## brewerpal = string with brewer.pal palette in reverted order (see: https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf)
  ## brewerpalnr = numeric value with max. number of colors from palette
  ## seed = random sequence generator 
  ## scale = A vector of length 2 indicating the range of the size of the words.
  
  # Checks if variable has available data
  if(
    var %>% 
    na.omit %>% 
    length() > 1
  ){
    
    # Loads required packages in invisible mode to avoid messages
    suppressPackageStartupMessages(library(dplyr)) ## For data wrangling
    suppressPackageStartupMessages(library(RColorBrewer)) ## For plot colors  
    suppressPackageStartupMessages(library(wordcloud)) ## For plot colors
    suppressPackageStartupMessages(library(tm)) ## For text mining
    suppressPackageStartupMessages(library(stopwords)) ## For parsing stopwords in different langauges
    
    # Manipulating string variable 
    ## Selecting variable/question
    df <- var  %>% 
      ##Replacing puctuation and symbols
      stringr::str_replace_all(pattern = "[[:punct:]]" , "") %>% 
      ## Replacing paragraph signs
      stringr::str_replace_all(pattern = "\n" , " ") %>%
      ## Replacing puctuation numbers generated due to punctuation signs
      stringr::str_replace_all(pattern = "[[:digit:]]" , "") %>%
      ## Replacing multiple spacing
      stringr::str_replace_all(pattern = "\\s+" , " ") %>% 
      ## Setting strings to lower case
      tolower() 
    
    # Converting to corpus and 
    corpus  <- tm::Corpus(VectorSource(df)) %>%  
      ## Removing english stopwords as well as other custom words
      tm::tm_map(removeWords, 
                 c(stopwords::stopwords(language, 
                                        source = "stopwords-iso"),
                   stopwords)
      ) 
    
    # Creating a document term matrix
    dtm <- tm::DocumentTermMatrix(corpus)
    
    # Setting random sequence for reproducibility
    set.seed(seed)
    
    # Creating matrix with corpus
    matrix.cloud <- as.matrix(dtm)
    
    # Computing word frequency
    freq.cloud <- sort(colSums(matrix.cloud), 
                       decreasing = TRUE)
    
    # Defining labels
    label.cloud <- names(freq.cloud)
    
    # Creating dataframe
    data.cloud <- data.frame(word = label.cloud, 
                             freq = freq.cloud) 
    
    if(!is.na(title)){
      # Prepres to print title
      layout(matrix(c(1, 2), nrow=2), heights=c(1, 5))
      par(mar=rep(0, 4))
      plot.new()
      text(x = 0.5, y = legend_height, title, cex = cex)
    }
    
    # Plotting cloud 
    suppressWarnings(
      wordcloud::wordcloud(main = title,
                           toupper(data.cloud$word), 
                           data.cloud$freq, 
                           min.freq = 1, 
                           scale = scale, 
                           rot.per=.15,
                           colo = (
                             rev(brewer.pal(brewerpalnr, brewerpal))
                           ), 
                           random.order = FALSE)
    )
    
  }else{
    
    cat(paste("''",title,"''"), fill = TRUE, labels = paste0("No data available for"))
    
    # p <- ggplot() +
    #   # Includes plot title
    #   ggtitle("Plant growth with\ndifferent treatments",
    #     subtitle = "N = 0"
    #   )
    
    
  }
}



# Random sample of answeres of text type variables ----
randomsanswers <- function(
    df = data_phone, # df = df_kiis
    key_var = "guide_needs", 
    sec_var = NA, 
    terc_var = NA, 
    sec_explanation = NA, 
    terc_explanation = NA,
    title = "title of the text variable", 
    sampleprop = NA, 
    form_path = NA, #  form_path = df_kiis_path
    seed = 1234) {
  
  
  
  # Checks if variable has available data
  if(
    df %>% 
    select(all_of(key_var)) %>% 
    na.omit %>% 
    nrow() > 0
  ){
    
    
    # Custom function for sampling answers
    
    if(
      
      is.na(sec_var) &
      is.na(sec_explanation) &
      !is.na(form_path)
      
    ){
      
      if(grepl("household", tolower(form_path))
      ){
        
        sec_var = "demo_interviewer_participant" 
        sec_explanation = "Participant: " 
        
      }else{
        
        if(grepl("kiis", tolower(form_path))
        ){
          
          sec_var = "demographics_participant" 
          sec_explanation = "Participant: " 
        } else{
          
          if(grepl("online", tolower(form_path))
          ){
            
            sec_var = "demographics_participant" 
            sec_explanation = "Participant: " 
          } 
        }
      }
    }
    
    
    if(
      
      is.na(terc_var) &
      is.na(terc_explanation) &
      !is.na(form_path)
      
    ){
      
      if(grepl("household", tolower(form_path))
      ){
        
        terc_var = "demo_interviewer_district" 
        terc_explanation = "District: " 
        
      } else{
        
        if(grepl("kiis", tolower(form_path))
        ){
          
          terc_var = "internal_site_type" 
          terc_explanation = "District: " 
          
        } else{
          
          if(grepl("online", tolower(form_path))
          ){
            
            terc_var = "demographics_participant_gender" 
            terc_explanation = "Gender: " 
          } 
        }
      }
    }
    
    # Loads required packages
    suppressPackageStartupMessages(library(dplyr)) ## For data wrangling
    suppressPackageStartupMessages(library(stringr)) ## For plot colors  
    
    
    # Custom function for capitalising first letters
    firstcapital = function(string) {
      paste0(
        toupper(
          substr(string, 1, 1)
        ), 
        substring(string, 2)
      )
    }
    
    # Filtering to remove NAs and no/none answers
    df_a <- df[which(!grepl("NA|Nil|nil|NIL|None",
                            df[[key_var]]) & !is.na(df[key_var])
    ),] %>%  
      select(
        all_of(key_var), 
        all_of(sec_var), 
        all_of(terc_var) 
      )
    
    # Creates an auxiliary dataset
    df_a <- df_a %>%
      # Concatenates secondary variable and storing it into a new variable "secondary" 
      mutate(secondary = paste0("\" (", sec_explanation, 
                                trimws(gsub("_", " ", df_a[[sec_var]])), ", ", terc_explanation, 
                                trimws(gsub("_", " ", df_a[[terc_var]]) %>% 
                                         #Remove parentheses and text within ()
                                         gsub("\\s*\\([^\\)]+\\)", "", .)),
                                ")", 
                                sep="")
      ) %>%  
      # Selects variables
      select(all_of(key_var), all_of("secondary")) %>% 
      # Omits NAs
      na.omit() 
    
    
    
    if(is.na(sampleprop)){
      
      
      # If Overall responses are less tha 10 then it replaces sample size to 100%, and if more than 6 then it sets sampleprop to sample 6 observations 
      if(nrow(df_a) <= 10){
        
        df_b <- df_a 
        
        sampleprop <- 1
        
      }else{
        if(nrow(df_a) > 10)
          
          sampleprop <- ceiling(
            10 / 
              nrow(df_a) * 
              100
          ) / 
            100
        
        df_b <- df_a %>%
          # Draws a random sample of answers 
          dplyr::sample_n(
            round(
              nrow(.) *
                as.numeric(sampleprop))) 
        
      }
      
    }else{
      
      df_b <- df_a %>%
        # Draws a random sample of answers
        dplyr::sample_n(
          round(
            nrow(.) *
              as.numeric(sampleprop)))
    }
    
    cat(crayon::bold(
      paste0(
        "\n",
        "**Sample of ", 
        sampleprop * 100, 
        "% of the ", 
        nrow(df_a), 
        " responses: '", 
        title, 
        "'**")
    ), 
    fill = TRUE, "\n")
    
    # Binds strings from two variables into one and transforming data table into a vector
    output <- return(
      paste(df_b[[key_var]], 
            df_b[["secondary"]], 
            sep = ""
      ) %>% 
        ## Removes punctuation in the start of strings
        stringr::str_replace_all(pattern = "^[[:punct:]]" , "") %>% 
        ## Replaces multiple spacing
        stringr::str_replace_all(pattern = "\\s+" , " ") %>% 
        ## Replaces line breaks
        stringr::str_replace_all(pattern = "\\n" , "") %>% 
        # Uses custom sub-function
        firstcapital(.) %>% 
        ## Pastes quotation marks
        paste0("\"", ., "") %>% 
        ## Adds hyphen for bullet points
        paste0('- ', .)  %>% 
        ## Adds end periods
        paste0(., '.') %>% 
        ## Uses cat to print output 
        cat(sep="\n") 
    )
    # Returns answers
    return(output)
    
    
  }else{
    
    cat(paste("''",key_var,"''"), fill = TRUE, labels = base::paste0("No data available for random answers"))
  }
}



# Converts likert scale to evaluative scale ----
translikert <- function(var) {
  # Loads packages
  library(dplyr)
  
  # modifies variable
  var[var == "Discordo_totalmente"] <- "Muito_ruim"
  var[var == "Discordo"] <- "Ruim"
  var[var == "Neutro"] <- "Regular"
  var[var == "Concordo_levemente"] <- "Regular"
  var[var == "Discordo_levemente"] <- "Regular"
  var[var == "Concordo"] <- "Bom"
  var[var == "Concordo_totalmente"] <- "Muito_bom"
  
  var
}



#  Percentage barplot for multiple select questions ----

multibarplot <- function(
    df = baseline, # Data frame
    variable = "demo_needs", # Variable 
    brewerpal = "Set2",
    xlabel  = "Responses",
    max.obs = Inf,
    subtitle = "responses", # Subtitle clarification for barplot
    title = "Plot", # Barplot title
    titlebreak = 60, # Number of characters in barplot title
    hjust = -0.1,
    tree_titlebreak = 90, # Number of characters in tree map 
    heights = c(3, 2), # Height of plot grids
    level_options = NA, # Factor levels if applicable
    direction = 1, # Direction of brewer palette
    xlab_break = 60
) { 
  
  # Checks if variable has available data
  if(
    df %>% 
    dplyr::select(all_of(variable)) %>% 
    na.omit() %>% 
    nrow > 1
  ){
    
    ## df = some data frame
    ## title = string with plot title
    ## label = label of horizontal axis
    ## brewerpal = string with brewer.pal palette (see: https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf)
    ## max.obs = numeric value limiting the number of results to plot
    
    library(ggplot2)
    library(scales)
    library(stringr)
    library(ggpubr)
    
    
    # Plots multiple choice (select_multiple in XLSFom) variables presented as percentages of total obs.
    
    # Creates long auxiliary dataframe with variable of interest (select_multiple)
    data <- df %>% 
      dplyr::select(all_of(variable)) %>% 
      unlist() %>% 
      strsplit(., " ") %>% 
      unlist() %>% 
      na.omit() %>% 
      tibble::as_tibble() 
    
    names(data) <- "value"
    
    data <- data %>% 
      mutate(value = base::gsub("_", " ", value)) 
    
    if(nrow(data) != 0){
      
      # Prepares table with %
      if (!is.na(level_options)) {
        var_table <- data$value %>%
          base::factor(levels = level_opt) %>%
          
          # Breaks line width of each answer 
          sapply(
            base::strwrap(
              as.character(data$value), 
              width = xlab_break, 
              simplify=FALSE), 
            paste, collapse="\n")  %>% 
          table()
        
      }else{
        
        # Breaks line width of each answer 
        var_table <- sapply(
          base::strwrap(
            as.character(data$value), 
            width = xlab_break, 
            simplify=FALSE), 
          paste, collapse="\n")  %>% 
          table()
      }
      
      # Transforms table into dataframe and change names of variables
      var_df <-  as.data.frame(var_table)
      colnames(var_df)<-c("Itens","Value")
      
      # Reorders items
      var_df$Itens <- base::factor(var_df$Itens,
                                   levels = var_df$Itens[order(var_df$`Value`)])
      ## apply max observation condition
      var_df <- var_df %>% 
        as_tibble() %>% 
        arrange(desc(Value)) %>% 
        head(max.obs)
      
      p <- ggplot(data = var_df, aes(x = Itens, y = `Value`, fill = Itens)) +
        # geom_bar(stat = "identity") +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        coord_flip() +
        theme(legend.position="none")  +
        # Includes plot title
        ggtitle(str_wrap(title, width = titlebreak),
                subtitle = paste("N =", df %>%
                                   dplyr::select(all_of(variable)) %>%
                                   na.omit() %>%
                                   nrow(), subtitle)
        ) +
        # Removes title for y axis
        theme(axis.title.y = element_blank()) +
        # Adds text with percentage
        geom_text(
          aes(
            label = 
              base::paste0(
                scales::percent(
                  (`Value`/ (df %>%
                               dplyr::select(all_of(variable)) %>% 
                               na.omit() %>%
                               nrow()
                  )
                  ), accuracy = 0.1
                ),
                " (",
                `Value`,
                ")"
              )
          ),
          hjust = hjust, size = 3.5, col="black") +
        
        # Changes size and format of axis and title
        theme(axis.text=element_text(size = 12),
              axis.title=element_text(size = 12)) +
        theme(plot.title = element_text(size = 14, face = "bold")) +
        # Adjust scales
        scale_y_continuous(
          breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
          limits = c(0, max(var_df[,2]*1.3))) + 
        labs(y = xlabel)
      
      # Runs condition on number of levels for setting colours
      if(nrow(var_df) >= RColorBrewer::brewer.pal.info["Accent",]["maxcolors"]){
        
        # Creates expanded Brewer palette
        colourCount = nrow(var_df)
        getPalette = colorRampPalette(
          brewer.pal(8, brewerpal)
        )
        
        p +  
          scale_fill_manual(
            values = 
              if(direction == -1){
                rev(getPalette(colourCount))
              }else{
                getPalette(colourCount)
              }
          )
        
      }else{ # For cases with over 12 levels due to color palette
        
        p + scale_fill_brewer(
          palette = brewerpal,
          direction = direction
        )
      }
    }
    
  }else{
    
    ## Renders empty ggplot if data is not avaiable
    
    ggplot(df <- data.frame(), aes(x = "", y = "")) + geom_blank() +
      labs(y = xlabel) +
      # geom_bar(position = "dodge", 
      #          stat = "identity") +
      theme_minimal() +
      coord_flip() +
      theme(legend.position="none")  +
      # Includes plot title
      ggtitle(
        stringr::str_wrap(title,
                          width = titlebreak),
        subtitle =
          paste("N = 0", subtitle)
      ) +
      # Removes title for y axis
      theme(axis.title.y = element_blank()) +
      # Prints values
      geom_text(
        aes(label =
              paste0("No data available for"),
            colour= "red"
        ),
        hjust = -0.1, vjust = 0.2, size = 5) +
      
      # Changes size and format of axis and title
      theme(axis.text=element_text(size = 12),
            axis.title=element_text(size = 12)) +
      theme(plot.title = element_text(size = 14, face = "bold"))  
    
    #cat(paste("''",title,"''"), fill = TRUE, labels = paste0("No data available for"))
  }
  
}





# Green to red barplots with evaluation scales -----

evalbarplot <- function(df, 
                        variable, 
                        title, 
                        ylab = "Responses", 
                        brewerpal = "RdYlGn", 
                        varlevel = "vgood_vpoor", 
                        title_break = 60, 
                        units = "responses") {
  ## df = some dataframe
  ## variable = variables of interest for summary
  ## varz must be numeric
  ## maxval = maximum number of summary rows
  ## title = string with plot title
  ## ylab = label for vertical axis
  ## brewerpal = string with brewer.pal palette (see: https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf)
  ## Plots will exclude results if the palette number of colors has less 
  ## colors than bars.
  ## max.obs = numeric value limiting the number of results to plot
  
  # Checks if variable has available data
  if(
    df %>%  
    nrow > 0
  ){
    
    # Loads required packages
    library(ggplot2) ## For plotting
    library(scales) ## For % annotation
    library(dplyr) ## For data wrangling
    library(lazyeval) ## For summarising with the interp function
    library(stringr) ## For text wrapping (e.g. titles)
    library(RColorBrewer) ## For plot colors
    
    
    
    # Creates vector with evaluation scales
    likert <- c(
      "Totally Agree", 
      "Agree", 
      "Agree Slightly", 
      "Neutral", 
      "Disagree Slightly", 
      "Disagree", 
      "Totally Disagree"
    )
    
    likert_es <- c(
      "De acuerdo totalmente",
      "De acuerdo",
      "De acuerdo parcialmente",
      "Neutro",
      "En desacuerdo parcialmente",
      "En desacuerdo",
      "En desacuerdo totalmente"
    ) 
    
    likert_pt <- c(
      
      "Concordo totalmente",
      "Concordo",
      "Neutro",
      "Discordo",
      "Discordo totalmente"
      
    ) 
    
    
    vgood_vpoor <- c("Very good", 
                     "Good", 
                     "Regular", 
                     "Bad", 
                     "Very bad", 
                     "Uncertain", 
                     "Not applicable"
    )
    
    vgood_vpoor_es <- c(
      "Muy bueno",
      "Bueno",
      "Regular",
      "Malo",
      "Muy malo",
      "No estoy seguro(a)",
      "No aplica"
    )
    
    vgood_vpoor_pt <- c(
      "Muito ruim",
      "Ruim",
      "Regular",
      "Bom",
      "Muito bom",
      "Não tenho certeza",
      "Não se aplica"
    )
    
    vhigh_vlow <- c(
      "Very high",
      "High", 
      "Medium",
      "Low", 
      "Very low", 
      "Uncertain", 
      "Not applicable"
    )
    
    vhigh_vlow_pt <- c(
      "Muito baixo",
      "Baixo",
      "Médio",
      "Alto",
      "Muito alto",
      "Não tenho certeza",
      "Não se aplica"
    )
    
    # Checks if variable exists in the dataset
    if (
      variable %in% names(df)
    ){
      
      df_summary <- df  %>% 
        # Groups by key variable
        dplyr::select(tidyselect::all_of(variable)) %>% na.omit()
      
      # Replaces underscores
      df_summary <- data.frame(lapply(df_summary, function(x) {
        gsub("_", " ", x)
      }))
      
      
      df_summary[,1] <- if(
        varlevel == "likert"
      ){
        factor(df_summary[,1] %>% unlist(),
               levels = likert)
      } else {
        
        if(
          varlevel == "likert_es"
        ){
          factor(df_summary[,1] %>% unlist(),
                 levels = likert_es)
        } else {
          
          if(
            varlevel == "likert_pt"
          ){
            factor(df_summary[,1] %>% unlist(),
                   levels = likert_pt)
          } else {
            
            if(
              varlevel == "vgood_vpoor"
            ){
              factor(df_summary[,1] %>% unlist(),
                     levels =  vgood_vpoor )
            } else {
              
              if(
                varlevel == "vgood_vpoor_es"
              ){
                factor(df_summary[,1] %>% unlist(),
                       levels =  vgood_vpoor_es )
              } else {
                
                if(
                  varlevel == "vgood_vpoor_pt"
                ){
                  factor(df_summary[,1] %>% unlist(),
                         levels =  vgood_vpoor_pt )
                } else {
                  
                  if(
                    varlevel == "vhigh_vlow"
                  ){
                    factor(df_summary[,1] %>% unlist(),
                           levels = vhigh_vlow)
                  } else {
                    
                    if(
                      varlevel == "vhigh_vlow_pt"
                    ){
                      factor(df_summary[,1] %>% unlist(),
                             levels = vhigh_vlow_pt)
                    }
                  }
                }
              }
            }
          }
        }
      }
      
      
      # Subsets dataset 
      df_summary <-  df_summary %>% 
        # Groups by key variable
        dplyr::group_by_(tidyselect::all_of(variable))  %>% 
        ## Summarises frequencies by answer option
        dplyr::summarise(freq = n()) %>% 
        ## Omits NAs
        na.omit() 
      
      if (nrow(df_summary) >=1) {
        
        # Colours
        colours = (brewer.pal(name="RdYlGn", n=nlevels(df_summary[,1] %>% unlist())))
        names(colours) = rev(levels(df_summary[,1] %>% unlist()))
        
        # Plots data
        ggplot(data = df_summary, 
               aes(x = df_summary[,1] %>% unlist(), y = freq, 
                   fill = df_summary[,1] %>% unlist()
               ) 
        ) +
          # Determines type of plot
          geom_bar(stat = "identity") + 
          # Sets scale to also show empty levels of factor  
          scale_x_discrete(drop=FALSE) + 
          # Flips plot coordinates  
          coord_flip()  +
          # fills colors
          scale_fill_manual(values=colours) +
          # Uses classic theme 
          theme_classic() +
          # Sets labels and title
          labs(x = "",  y =  ylab, 
               # Wraps plot title to 70 characters
               title = str_wrap(title, width = title_break),
               # Creates subtitle with number of observations
               subtitle =  
                 paste("N =",
                       format(sum(df_summary[,2]), 
                              big.mark=","), units)
          )  + 
          # Sets options for subtitles
          theme(plot.subtitle = element_text(size=11, color="black")) + 
          # Sets options for X axis title
          theme(axis.title.x = element_text(size = 11, angle = 00)) + 
          # Sets options for Y axis title
          theme(axis.title.y = element_text(size = 11, angle = 90)) + 
          # Sets options for X axis text
          theme(axis.text.x = element_text(colour="grey20", size = 11,  
                                           face="plain", angle = 00),
                # Sets options for Y axis text
                axis.text.y = element_text(colour="grey20", size = 11, face="plain"), 
                # Sets options for plot title
                plot.title = element_text(size = 12, face = "bold", vjust=1.2)
          ) +
          # Sets options for X axis line
          theme(axis.line.x = element_line(color = "black", size = 0.5), 
                # Sets options for Y axis
                axis.line.y = element_line(color = "black", size = 0.5)
          ) +
          # Sets options for data labels (text annotations)
          geom_text(
            aes(label = 
                  percent(
                    freq / sum(freq)
                  )
            ), 
            hjust = -0.2, size = 3.5) +
          
          scale_y_continuous(expand = c(.01, .05), # Space between bars and axis
                             # Setting automatic limites based on data
                             limits = c(0, max(df_summary[, 2]*1.10)),
                             # Ensuring only integer breaks                            
                             breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))
          ) +
          # Removes legend 
          theme(legend.position="none")
        
      }
    }
  }else{
    
    cat(paste("''",title,"''"), fill = TRUE, labels = paste0("No data available for"))
  }
}




# Capitalises names / strings ----
simplecapital <- function(x){
  # x = string with people's names
  
  cap <- function(var) {
    
    
    
    s <- strsplit(var, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  gsub("  ", " ", as.character(sapply(tolower(x), cap)))
}





#  Percentage barplot for multiple select questions ----

multipercentplot <- function(
    df = regdata, # Data frame
    variable = "work_profession", # Variable
    xlabel = "Percentage (%)",
    brewerpal = "Set3",
    subtitle = "responses", # Subtitle clarification for barplot
    title = "Plot", # Barplot title
    titlebreak = 60, # Number of characters in barplot title
    hjust = -0.1, 
    heights = c(3, 2)) { # Height of plot grids
  
  # Checks if variable has available data
  if(
    df %>%  
    nrow > 0
  ){
    
    ## df = some data frame
    ## title = string with plot title
    ## label = label of horizontal axis
    ## brewerpal = string with brewer.pal palette (see: https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf)
    ## max.obs = numeric value limiting the number of results to plot
    
    library(ggplot2)
    library(scales)
    library(stringr)
    library(ggpubr)
    
    
    # Plots multiple choice (select_multiple in XLSFom) variables presented as percentages of total obs.
    
    # Creates long auxiliary dataframe with variable of interest (select_multiple)
    data <- df %>% 
      dplyr::select(all_of(variable)) %>% 
      unlist %>% 
      strsplit(., " ") %>% 
      unlist %>% 
      na.omit() %>% 
      tibble::as_tibble()
    
    names(data) <- "value"
    
    data <- data %>% 
      dplyr::mutate(value = gsub("_", " ", value)) 
    
    
    
    # Prepares table with %
    
    var_table <- round(data$value %>% 
                         table() / (df %>%
                                      dplyr::select(contains(variable)) %>% 
                                      na.omit %>% 
                                      nrow())*100,1) 
    
    # Transforms table into dataframe and change names of variables
    var_df <-  as.data.frame(var_table) 
    colnames(var_df)<-c("Itens","Percentage (%)")
    
    # Reorders items
    var_df$Itens <- factor(var_df$Itens, 
                           levels = var_df$Itens[order(var_df$`Percentage (%)`)])
    
    
    
    p <- ggplot(data = var_df, aes(x = Itens, y = `Percentage (%)`, fill = Itens)) + 
      # geom_bar(stat = "identity") +
      geom_bar(position = "dodge", 
               stat = "identity") +
      theme_minimal() + 
      coord_flip() + 
      theme(legend.position="none") + 
      # Includes plot title 
      ggtitle(str_wrap(title, 
                       width = titlebreak), 
              subtitle = paste("N =", df %>% 
                                 dplyr::select(all_of(variable)) %>% 
                                 na.omit() %>% 
                                 nrow(), subtitle)
      ) + 
      # Removes title for y axis
      theme(axis.title.y = element_blank()) +
      geom_text(aes(label=scales::percent(`Percentage (%)`/100)),
                hjust = hjust, size = 3.5, col="black") +
      
      # Changes size and format of axis and title
      theme(axis.text=element_text(size = 12), 
            axis.title=element_text(size = 12)) + 
      theme(plot.title = element_text(size = 14, face = "bold")) +
      # Adjust scales
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
                         limits = c(0, max(var_df[,2]*1.1))) +
      labs(y = xlabel)
    
    # Function to adjust color palette
    if(data$value %>% unique() %>% length <= 8){
      
      p  + 
        scale_fill_brewer(
          palette = brewerpal,
          direction = direction
        )
      
      
    }else{ # For cases with over 12 levels due to color palette
      
      # Creates expanded Brewer palette
      colourCount = nrow(var_df)
      getPalette = colorRampPalette(
        brewer.pal(8, brewerpal)
      )
      
      p +  
        scale_fill_manual(
          values = 
            if(direction == -1){
              rev(getPalette(colourCount))
            }else{
              getPalette(colourCount)
            }
        )
    }
  }else{
    
    cat(paste("''",title,"''"), fill = TRUE, labels = paste0("No data available for"))
  }
}






#  Transforms NAs to zeros ----
natozero <- function(x) {
  # x = variable
  x = ifelse(is.na(x), 0, x)
  
  # Outputs adjusted variable
  x
  
}



#  Barplots with three variables ----
dfbarplot <- function(df, varz, varx, vary, maxval = Inf, title = "Plot", ylab = "Observations", brewerpal = "RdYlGn", dist = 1.2, textsize = 2.5, angle = 30, hjust = 0.2, colour="lightgray", titlebreak = 70, vjust = -1.1, scales = "free_x", nrow = NULL, ncol = NULL, direction = 1) {
  ## df = some two column summary dataframe
  ## varz, varx and varz = variables of interest for summary
  ## varz must be numeric
  ## maxval = maximum number of summary rows
  ## title = string with plot title
  ## ylab = label for vertical axis
  ## brewerpal = string with brewer.pal palette (see: https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf)
  ## Plots will exclude results if the palette number of colors has less 
  ## colors than bars.
  ## max.obs = numeric value limiting the number of results to plot
  ## dist = distance between barplot top and plot frame
  
  # Loads required packages
  library(ggplot2) ## For plotting
  library(scales) ## For % annotation
  library(dplyr) ## For data wrangling
  library(lazyeval) ## For summarising with the interp function
  library(stringr) ## For text wrapping (e.g. titles)
  
  # Subsets dataset 
  df_summary <- df %>% 
    dplyr::select(varz, varx, vary)  %>% 
    ## Omits NAs
    na.omit() %>% 
    ## Groups subset by region
    group_by_(varz, varx) %>% 
    dplyr::summarize_(total = interp(~sum(x), x=as.name(vary))) %>% 
    rename_(x = varz, y = varx)  %>% 
    ## Arranges results in descending order
    arrange(desc(total)) %>%
    ## Heads only top five
    head(maxval)
  
  ggplot(data = df_summary, 
         aes(x = reorder(y, -total, sum), 
             y = total)) + 
    # Sets plot type as bar plot
    geom_bar(aes(
      #reorder(df_summary[,1], -total, sum),
      fill = reorder(y, -total, sum)), 
      stat = "identity", position = "dodge", colour=colour) +
    # Sets color palette
    scale_fill_brewer(palette = brewerpal, name = varz, direction = direction)  +
    # Sets plot theme
    theme_bw() +
    # Prepares plot annotations in % (uses package scales)
    geom_text(aes(label = scales::percent(total/sum(total))
    ),  
    vjust=vjust, hjust=hjust, color="black", size=textsize, angle = angle) +
    facet_wrap(names(df_summary[,1]), 
               # Drops unused levels in facets
               scales = scales, nrow = nrow, ncol = ncol) +
    
    # Removes plot legend
    theme(legend.position="") +
    # Sets axis formatting
    theme(axis.text.x = element_text(angle = angle, vjust=.5, hjust = .5)) +
    theme(axis.title.x = element_blank()) + 
    theme(plot.title = element_text(size = 14, face = "bold")) +
    # Sets title and subtitle (incl. str_wrap for text wrapping)
    ggtitle(stringr::str_wrap(title, width = titlebreak)) +
    # Adjust scales 
    scale_y_continuous(limits = c(0, (max(df_summary$total) * 1.1))) +
    # Defines label for y axis (must be defined upon function call)
    labs(y = ylab) +
    # Adjust scales 
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) *
                                                                      1.1))))) +
    coord_cartesian(ylim = c(0, max(df_summary$total) * dist))
  
}





# Labelling based on results ----
## Scale on five levels (Very good to Very poor) -----

labelfive <- function(value) {
  if(value <= 1.80){
    output <- ("Muito bom")
    return(output)
  } else {
    if(value > 1.80 & value <= 2.6){
      output <- ("Bom")  
      return(output)
    } else { 
      if(value > 2.6 & value <= 3.4){
        output <- ("Regular")  
        return(output)
      } else { 
        if(value > 3.4 & value <= 4.2){
          output <- ("Ruim") 
          return(output)
        } else { 
          if(value > 4.2){
            output <- ("Muito ruim")
            return(output)
          }
          
        }
      }
    }
  }
}

## Scale on six levels (Likert) ----
labelsix <- function(value) {
  if(value <= 1.82){
    output <- ("Muito bom")
    return(output)
  } else {
    if(value >= 1.83 & value < 2.66){
      output <- ("Bom")  
      return(output)
    } else { 
      if(value >= 2.67 & value < 3.49){
        output <- ("Regular")  
        return(output)
      } else { 
        if(value >= 3.5 & value < 4.32){
          output <- ("Ruim") 
          return(output)
        } else { 
          if(value >= 4.33 & value <= 5.16){
            output <- ("Muito ruim")
            return(output)
          } else { 
            if(value >= 5.17 & value <= 6.00){
              output <- ("Sérias deficiências")
              return(output)
            }
          }
          
        }
      }
    }
  }
}



# Parses nested data ------
toparse <- function(
    df = proj_data,
    key_variable = "demo_member_repeat",
    key_id = "uuid"
) {
  
  
  # variable <- variable[variable!="NULL"] 
  df <- df %>%
    filter(!!as.symbol(key_variable) != "NULL") %>% 
    select(
      any_of(key_variable),
      any_of(key_id)
    )
  
  names(df) <- c("key_variable", "key_id")
  
  
  data <- data.frame()
  
  for (row in 1:nrow(df)) {
    
    # Creates preliminary dataset with contents of variable 
    rowdata <- eval(
      parse(
        text=paste('data.frame(', df$key_variable[row], ')')
      )
    ) %>% 
      # Adds a new column with the index (row) 
      dplyr::mutate(
        row = row,
        key_id = df$key_id[row],
        key_variable = df$key_variable[row]
      )
    
    # Binds datasets in rows
    data <- dplyr::bind_rows(
      data, 
      rowdata
    )
    
  }
  
  data <- data %>% 
    mutate(index = 1:n()) %>% 
    select(
      index,
      row,
      key_id,
      key_variable,
      everything()
    )
  
  string <- gsub("\\..*", "_", names(data)[5])
  # Remove prefixes of variable names
  names(data) <- gsub("^.*\\.", "", names(data)) 
  
  names(data)[!(names(data) %in% c("index", "row", "key_id", "key_variable"))] <-
    paste0(string, 
           c(names(data)[!(names(data) %in% c("index", "row", "key_id", "key_variable"))]))
  
  # Returns dataset as tibble::as_tibble()
  data %>% 
    tibble::as_tibble() 
  
}

# Parses nested data wit additional variables ------
toparse_extra <- function(
    data = proj_data,
    key_variable = "demo_member_repeat",
    facet_variable = "interno_municipality",
    add_variables = c(names(proj_data %>% select(contains("demo_")))),
    key_id = "uuid"
) {
  
  library(dplyr)
  
  df <- data %>% 
    mutate(row_id = 1:nrow(data)) %>%
    select(
      id,
      uuid,
      row_id,
      any_of(c(key_variable)),
      any_of(
        facet_variable
      ),
      any_of(
        add_variables
      )
    )  %>%
    filter(!!as.symbol(key_variable) != "NULL") %>% 
    mutate(
      key_id = !!as.symbol(key_id)
    ) 
  
  # Parses nested data ------
  toparse <- function(
    df = proj_data,
    key_variable = "demo_member_repeat",
    key_id = "uuid"
  ) {
    
    
    # variable <- variable[variable!="NULL"] 
    df <- df %>%
      filter(!!as.symbol(key_variable) != "NULL") %>% 
      select(
        any_of(key_variable),
        any_of(key_id)
      )
    
    names(df) <- c("key_variable", "key_id")
    
    
    data <- data.frame()
    
    for (row in 1:nrow(df)) {
      
      # Creates preliminary dataset with contents of variable 
      rowdata <- eval(
        parse(
          text=paste('data.frame(', df$key_variable[row], ')')
        )
      ) %>% 
        # Adds a new column with the index (row) 
        dplyr::mutate(
          row = row,
          key_id = df$key_id[row],
          key_variable = df$key_variable[row]
        )
      
      # Binds datasets in rows
      data <- dplyr::bind_rows(
        data, 
        rowdata
      )
      
    }
    
    data <- data %>% 
      mutate(index = 1:n()) %>% 
      select(
        index,
        row,
        key_id,
        key_variable,
        everything()
      )
    
    string <- gsub("\\..*", "_", names(data)[5])
    # Remove prefixes of variable names
    names(data) <- gsub("^.*\\.", "", names(data)) 
    
    names(data)[!(names(data) %in% c("index", "row", "key_id", "key_variable"))] <-
      paste0(string, 
             c(names(data)[!(names(data) %in% c("index", "row", "key_id", "key_variable"))]))
    
    # Returns dataset as tibble::as_tibble()
    data %>% 
      tibble::as_tibble() 
    
  }
  
  data <- toparse(
    df = df,
    key_variable = key_variable,
    key_id = key_id
  )
  
  
  df_merged <- merge(
    data,
    df,
    by = c("key_id")
  )   %>%
    filter(
      !is.na(
        !!as.symbol(key_variable)
      )
    ) %>%
    filter(
      !!as.symbol(key_variable) != "NULL"
    )
  
  names(df_merged) <- gsub(
    "$.x", "",
    names(df_merged)
  )
  
  df_merged <- df_merged %>% 
    select(any_of(names(data)), 
           everything())
  
  df_merged
  
}

## Network plot of co-occurrences of text data ----
topnetworkplot <- function(
    # Defines variable (needs to be a text variable)
  
  data = df_kiis,
  key_variable = "guide_implementation",
  facet_variable = "internal_site_type",
  
  
  # Defines language for stopwords function
  language = "en",
  
  # Number of desired features
  features = 15,
  
  # Sets random seed generator
  seed = 1234,
  
  # Sets scale of labels
  scale = c(0.1),
  
  # Adds stop words
  stopwords = c("NIL", "nil", "NA", "None", "program", "programm", "programs",
                "kind", "put", "lack", "low", "high", "TEKAN"),
  
  # Title
  title = "Some title of some variable",
  
  # Title break
  titlebreak = 110,
  
  # Unit of measurement
  units = "responses",
  
  # Size of edges
  edge_size = 2.5,
  
  remove_numbers = TRUE,
  
  keep_acronyms = TRUE,
  
  title_size = 10,
  
  facet_plot = FALSE
  
) {
  
  
  # To analyse the open-ended answers from the household interviews, Text Network Plots were used. 
  # This plot shows relationships between words which appear together in the same answers (co-occurrence).
  # The words with higher frequency of co-occurrence (number of times the word occurs with other words) 
  # appear with larger font size. The thickness of the blue line shows the extent of co-occurrence with 
  # other words. 
  
  # Checks if variable has available data
  if(
    data %>%
    select(any_of(key_variable)) %>%
    unlist(., use.names = FALSE) %>%
    na.omit %>%
    length() > 0
  ){
    
    
    # # Subsets dataset
    
    ## Setting random generation seed to allow for reproducibility
    set.seed(seed)
    
    # List the names of facet_levels for plot disaggregartion
    
    
    if(facet_variable %in% names(data)){
      
      if(is.factor(data %>% pull(facet_variable))){
        facet_levels <- levels(data %>% pull(facet_variable)) %>% gsub("_", " ", .)
      }else{
        facet_levels <- data %>% pull(facet_variable) %>% unique() %>% as.character() %>% sort() %>% gsub("_", " ", .)
      }
      
      facet_levels <- facet_levels %>% 
        unique() 
      
      
      facet_levels <-
        c(
          "Total responses",
          facet_levels
        )
      
      # Adds line break for number of observations
      #subtitle = paste(subtitle, "\n")
      
      
      df_summary <- data %>%
        select(any_of(c(facet_variable, key_variable)))  %>%
        rename_(x = facet_variable,
                y = key_variable) %>%
        ## Omits NAs
        na.omit() %>%
        mutate(y= gsub("_"," ",y),
               x= gsub("_"," ",x) 
        )
      
    }else{
      
      facet_levels <- "Total responses"
      
      df_summary <- data %>%
        select(any_of(key_variable))  %>%
        mutate(x = "Total responses") %>% 
        rename_(y = key_variable) %>%
        ## Omits NAs
        na.omit() %>%
        mutate(y= gsub("_"," ",y),
               x= gsub("_"," ",x) 
        )
      
    }
    
    
    if(facet_plot == FALSE){
      facet_levels <- facet_levels[1]
    }
    
    scale_i <- scale
    
    for (i in 1:length(facet_levels)){
      
      
      if(!is.na(scale[i])){
        scale_i <- scale[i]
      }
      
      
      title_textplot = facet_levels[i]
      subtitle = ""
      title_size_textplot = title_size * 0.8
      label_size = 8
      
      # Filter the data per country and adjust the title and subtitle text and size
      if(facet_levels[i] != "Total responses"){
        
        
        df_textplot <- df_summary %>%
          filter(x %in% facet_levels[i]) %>%
          select(y)
        
      }else{
        
        
        df_textplot <- df_summary %>%
          select(y)
      }
      
      variable <- df_textplot %>% 
        pull(y)
      
      # Loads packages
      library(quanteda)
      
      # Cleans variable
      variable <- variable %>% 
        na.omit %>% 
        base::gsub("_", " ", .) %>% 
        stringr::str_replace_all(
          pattern = "[[:punct:]]" , "") %>% 
        stringr::str_replace_all(
          pattern = "[[:digit:]]" , "")
      
      # The function below checks one single word in the dictionary and replace it to singulaR
      # SemNetCleaner::singularize(dictionary = TRUE)
      
      # Creates tokens from a corpus
      toks <- quanteda::corpus_subset(quanteda::corpus(variable)) %>%
        # Removes punctuation
        quanteda::tokens(remove_punct = TRUE, 
                         remove_numbers = remove_numbers,
                         remove_symbols = TRUE,
                         remove_url = TRUE) %>%
        # Sets to lower case
        quanteda::tokens_tolower(keep_acronyms = keep_acronyms) %>%
        # Removes stopwords
        quanteda::tokens_remove(pattern = c(
          stopwords::stopwords(language),
          stopwords::stopwords(source = "smart"), # Only for english
          stopwords,
          strsplit(title, " ") %>% unlist %>% 
            stringr::str_replace_all(
              pattern = "[[:punct:]]" , "") %>% 
            stringr::str_replace_all(
              pattern = "[[:digit:]]" , "")
        ) %>% unique(), 
        padding = FALSE)
      
      # Sets seed
      set.seed(seed)
      
      # Creates feature co-occurence matrix
      fcmat <- quanteda::fcm(toks, context = "window", tri = FALSE)
      
      # Sets number of features
      feat <- names(quanteda::topfeatures(fcmat, features))
      
      # Subsets fcmat based on the number of features
      fcm <- quanteda::fcm_select(fcmat, pattern = feat)
      
      # p_network <- quanteda::textplot_network(fcm,  edge_size = edge_size,
      #                                         vertex_labelsize = scale * 3) +
      
      
      if(min(rowSums(fcm)) < 1){
        min_rowsums <- 1
      }else{
        min_rowsums <- min(rowSums(fcm))
      }
      
      p_network <- quanteda.textplots::textplot_network(fcm,  edge_size = edge_size, 
                                                        vertex_labelsize = scale_i * rowSums(fcm)/min_rowsums) +
        
        # Includes plot title 
        ggtitle(str_wrap(title_textplot, width = titlebreak),
                subtitle =  
                  paste("n =",
                        length(variable %>% 
                                 na.omit), units) 
        ) +
        theme(plot.title = element_text(size = title_size_textplot, face = "bold"),
              plot.subtitle = element_text(size = title_size * 0.8)) 
      
      #options(warn=1)
      #print(p_network)
      #print(i)
      #print(p_network)
      #warnings(print(p_network))
      
      #options(warn=-1)
      
      ## Assign the index to name of each plot
      
      assign(paste("p_network",
                   i,
                   sep = "_") ,
             p_network)
      
      rm(p_network)
      
      
    }
    
    
    ## Adds plot title to ggplot without any margin space from left
    
    library(gridExtra)
    library(grid)
    
    
    title.grob <- textGrob(
      label = stringr::str_wrap(title,
                                width = titlebreak),
      x = unit(0, "lines"),
      y = unit(0, "lines"),
      hjust = -0.00, vjust = -0.5,
      gp = gpar(fontsize = title_size , fontface = "bold"
      )
    )  
    
    
    if(facet_plot == TRUE){
      # Binds the disaggregated plots
      # Checks if any 2 level facet plot is created 
      if("p_network_2" %in% ls()){
        
        # Extract names of textplots from global environment
        p_network <- ls()[grepl("p_network_", ls())]
        
        # Combine all the plots in 1 grob
        pl <- lapply(1:length(p_network), function(.x) 
          get(paste0("p_network_", .x))
        )
        
        # Adjusts widths of odd number of facet plots 
        factors_n <- length(p_network)
        
        # If facet levels are in even number or 1
        if((factors_n %% 2) == 0){
          
          p_fct <- gridExtra::arrangeGrob(
            grobs=pl,
            nrow= ceiling(factors_n/2), 
            ncol=2
          )
          
          
        }else{
          
          # If facet levels are in odd number then arrange Total_responses plot at the top
          # then build the bottom row
          
          bottom_row  <- gridExtra::arrangeGrob(
            grobs=pl[2:factors_n],
            nrow= floor(factors_n/2), 
            ncol=2
          )
          
          nrow_odd_facets <- if(factors_n > 3){ceiling(factors_n/2)}else{floor(factors_n/2)}
          
          p_fct <- cowplot::plot_grid(
            pl[[1]] ,
            bottom_row,
            nrow = 2 ,
            rel_heights = if(factors_n > 3){
              c(1/nrow_odd_facets,((nrow_odd_facets-1)/nrow_odd_facets))
            }else{
              c(0.5, 0.5)
            }
          )
          
        }
        
        # Binds the title
        
        ddpcr::quiet(
          gridExtra::grid.arrange(
            p_fct,
            top= title.grob,
            padding = unit(1.5, "line")
          )
        )  
        
        # Deletes the intermediary plots
        rm(list=ls()[ls() %in% c(p_network, "p_fct")])
        
      }else{
        
        ddpcr::quiet(
          gridExtra::grid.arrange(
            p_network_1,
            top= title.grob,
            padding = unit(1.5, "line")
          )
        )  
        
      }
      
    }else{
      
      if(facet_plot == FALSE){
        
        # Binds the title
        p_network_1 <- p_network_1 +
          theme(plot.title = element_blank())
        
        ddpcr::quiet(
          gridExtra::grid.arrange(
            p_network_1,
            top= title.grob,
            padding = unit(1.5, "line")
          )
        )  
        
      }
    }
    
  }else{
    
    cat(paste("''",title,"''"), fill = TRUE, labels = base::paste0("No data available for text-network"))
  }
}



# Line plots based on dates -----

toplineplot <- function(
    df = baseline,
    time_variable = "submission_time",
    y_label = "Numbers of Submissions",
    date_breaks  ="1 day",
    date_labels = "%d-%m-%Y",
    title = "Submissions by date",
    units = " submissions",
    point_colour = "darkred",
    line_colour="pink",
    line_size = 1.5,
    title_break = 60,
    simplify_dates = FALSE
) {
  # Checks if variable has available data
  if(
    df %>%  
    nrow > 0
  ){
    
    
    if(simplify_dates == TRUE){
      
      df <- df %>% 
        dplyr::select(time_variable)  %>% 
        na.omit() %>% 
        unlist() %>% 
        as.Date(.) %>% 
        format("%Y-%m-01") %>% lubridate::ymd() %>%
        tibble::as_tibble() %>% 
        dplyr::group_by(value) %>%
        dplyr::summarise(Submissions = dplyr::n()) %>%
        ungroup() %>% 
        rename(Date = value)
      
    }else{
      
      ## Prepares dataset
      df <- df %>% 
        dplyr::select(time_variable)  %>% 
        na.omit() %>% 
        unlist() %>% 
        as.Date(.) %>% lubridate::ymd() %>%
        tibble::as_tibble() %>% 
        dplyr::group_by(value) %>%
        dplyr::summarise(Submissions = dplyr::n()) %>%
        ungroup() %>% 
        rename(Date = value)
      
    }
    
    ## Prepares plot
    ggplot(df, aes(Date,  Submissions))  +
      ## Sets theme / design
      theme_linedraw() +
      ## Defines geometry line
      geom_line(colour = line_colour, size = line_size) +
      ## Defines geometry points
      geom_point(colour=point_colour) +
      ## Removes label of x axix
      xlab("") +
      ## Sets label for y axis
      ylab(y_label)   +
      # Chnages format of dates in the plot display
      scale_x_date(date_labels = date_labels, date_breaks  = date_breaks) +
      # Sets options font formatting options
      theme(
        axis.text.x = 
          element_text(
            colour="grey20", size=9, angle=70, hjust=.5, vjust=.5, face="plain"),
        axis.text.y = 
          element_text(
            colour="grey20",size=9,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.y = 
          element_text(
            colour="grey20",size=10,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(size = 14, face = "bold", vjust=1.2)) +
      ## Sets title and subtitle
      ggtitle(stringr::str_wrap(title, width = title_break),
              subtitle =  paste("N = ",
                                format(sum(df$Submissions)), units,
                                " from ",
                                min(df$Date) %>% format(date_labels),
                                " to ",
                                max(df$Date) %>% format(date_labels),
                                sep = "")) +
      # Adjust scales to integer numbers
      scale_y_continuous(
        breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))
      ) +
      theme(panel.grid.minor =   element_blank(),
            panel.grid.major =   element_line(colour = "grey", size=0.25)
      )
    
  }else{
    
    cat(paste("''",title,"''"), fill = TRUE, labels = paste0("No data available for"))
  }
}

# Density map plot ----

# Plots density maps 

topdensitymap <- function(
    # Basemap with ggmap::get_map
  base_map = baselinemap,
  # Dataframe used for basemap
  df = baseline,
  # Number of bins for density map
  bins = 20,
  # Transparency of the density map layer
  alpha = 0.3,
  # Title of plot
  title = "Density of interviews with GPS data",
  # Number of characters of title line break
  title_break = 80,
  # Size of points for density plot
  size = 0.05
) {
  
  # Required packages
  library(ggmap)
  library(dplyr)
  
  ggmap::ggmap(base_map, extent = "panel", 
               maprange=FALSE) %+% baseline +
    aes(x = lon, y = lat) +
    geom_density2d(
      data = df %>% 
        dplyr::select(lat, lon) %>%
        dplyr::mutate(lat = as.numeric(lat),
                      lon = as.numeric(lon)
        ) %>% 
        na.omit(),
      aes(x = lon, y = lat)) +
    stat_density2d(
      data = df, 
      aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
      size = size, bins = bins, geom = 'polygon') +
    scale_fill_gradient(low = "green", high = "red") +
    scale_alpha(range = c(0.00, alpha), guide = FALSE) +
    theme(
      legend.position = "none", 
      axis.title = element_blank(), 
      text = element_text(size = 12)
    ) +
    # Adds plot decoration
    # Includes border
    theme(
      panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    # Sets title and subtitle (incl. str_wrap for text wrapping)
    ggtitle(stringr::str_wrap(title, width = title_break),
            subtitle =  
              paste("N = ",
                    format(
                      nrow(
                        df %>% 
                          dplyr::select(lon, lat) %>% 
                          na.omit()
                      ), 
                      big.mark=","
                    ), 
                    " obs.", sep = ""
              )
    ) + 
    # Sets options for subtitles
    theme(
      plot.subtitle = element_text(size=12, color="black")) + 
    # Sets options for X axis text
    theme(
      axis.text.x = element_text(colour="grey20", size = 10,  
                                 face="plain", angle = 00),
      # Sets options for Y axis text
      axis.text.y = element_text(colour="grey20", size = 10, 
                                 face="plain"), 
      # Sets options for plot title
      plot.title = element_text(size = 14, face = "bold", 
                                vjust=1.2)
    ) 
  
}

# Extract age from birth date ----
#' 
#' Returns age, decimal or not, from single value or vector of strings
#' or dates, compared to a reference date defaulting to now. Note that
#' default is NOT the rounded value of decimal age.
#' @param from_date vector or single value of dates or characters
#' @param to_date date when age is to be computed
#' @param dec return decimal age or not
#' @examples
#' get_age("2000-01-01")
#' get_age(lubridate::as_date("2000-01-01"))
#' get_age("2000-01-01","2015-06-15")
#' get_age("2000-01-01",dec = TRUE)
#' get_age(c("2000-01-01","2003-04-12"))
#' get_age(c("2000-01-01","2003-04-12"),dec = TRUE)

get_age <- function(from_date,to_date = lubridate::now(),dec = FALSE){
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (dec) { age <- lubridate::interval(start = from_date, end = to_date)/(lubridate::days(365)+lubridate::hours(6))
  } else   { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))}
  age
}


# Merge dataframes ----

topmerge <- function(
    df1 = support, # Primary dataframe 
    df2 = regdata, # Secondary dataframe with complementing information
    by = "cpf_nr", # Key variable (must be the same in both dataframes)
    all = TRUE, # Sets if all rows should be kept
    filter_duplicates = "uuid", # Variable to filter duplicated observations after mergning
    filter_na = "nome", # Variable for performing second merge in case of remaining NAs
    df2_columns = c("nome", "cpf_nr", "id_origem_nr", "id_pana_num", "nascimento") # Columns from secondary dataframe that should be added to output
) {
  
  # Load packages
  require(dplyr)
  
  # Merge dataframe
  df <- merge(
    df1, 
    df2, 
    by = by,
    all.x = T
  ) 
  
  # Save vector with df names which are duplicated
  names_without_suffixes <- df %>% 
    dplyr::select(
      dplyr::contains(".x")
    ) %>% 
    names() %>% 
    gsub("\\.x.*", "",.)
  
  # Save vector with df names which are duplicated with suffixes
  names_x_suffixes <- df %>% 
    dplyr::select(
      dplyr::contains(".x")
    ) %>% 
    names() 
  
  
  # Loops through variables with suffixes .x for replacing .x NA values them by .y values 
  for (i in 1:length(names_x_suffixes)) {
    
    # # Replace variables by x suffixed variable
    df[names_without_suffixes[i]] <- df[names_x_suffixes[i]] 
    
    # Replace NAs by y suffixed variable
    df[names_without_suffixes[i]][is.na(df[names_without_suffixes[i]])]  <- df[paste0(names_without_suffixes[i], ".y")][is.na(df[names_without_suffixes[i]])] 
    
  }
  
  # Remove merge byproducts
  df <- df %>%
    dplyr::select(
      -contains(".x"),
      -contains(".y")
    )  %>% 
    # Select pre-existing columns in df1
    dplyr::select(c(tidyselect::all_of(df2_columns), names(df1)))  %>% 
    # Remove duplicated rows
    dplyr::filter(!!sym(filter_duplicates) %in% 
                    (df1 %>% dplyr::select(tidyselect::all_of(filter_duplicates)) %>% unlist) &
                    !duplicated(!!sym(filter_duplicates)))
  
  
  # SECONDARY MERGE ROUND
  # Create intermediary dataframe without NAs
  df_without_nas <- df2 %>%
    dplyr::filter(!is.na(!!sym(filter_na)))
  
  
  # Merge dataframe
  df <- merge(
    df,
    df_without_nas,
    by = filter_na,
    all = T
  )
  
  # Save vector with df names which are duplicated
  names_without_suffixes <- df %>%
    dplyr::select(
      dplyr::contains(".x")
    ) %>%
    names() %>%
    gsub("\\.x.*", "",.)
  
  # Save vector with df names which are duplicated with suffixes
  names_x_suffixes <- df %>%
    dplyr::select(
      dplyr::contains(".x")
    ) %>%
    names()
  
  # Loops through variables with suffixes .x for replacing .x NA values them by .y values
  for (i in 1:length(names_x_suffixes)) {
    
    # # Replace variables by x suffixed variable
    df[names_without_suffixes[i]] <- df[names_x_suffixes[i]]
    
    # Replace NAs by y suffixed variable
    df[names_without_suffixes[i]][is.na(df[names_without_suffixes[i]])]  <- df[paste0(names_without_suffixes[i], ".y")][is.na(df[names_without_suffixes[i]])]
    
  }
  
  # Remove merge byproducts
  df <- df %>%
    dplyr::select(
      -contains(".x"),
      -contains(".y")
    )  %>% 
    # Select pre-existing columns in df1
    dplyr::select(c(tidyselect::all_of(df2_columns), names(df1)))  %>% 
    # Remove duplicated rows
    dplyr::filter(!!sym(filter_duplicates) %in% 
                    (df1 %>% dplyr::select(tidyselect::all_of(filter_duplicates)) %>% unlist) &
                    !duplicated(!!sym(filter_duplicates)))
  
  # Return merged dataframe
  return(df) 
  
}


# Function to translate datasets ----

## the function translates all the variables of panabase to English
## the function also translate multi-answers 
## the form should be corresponing to data e.g Formulario 0 corresponds to baseline data, Formulario 1 corresponds to regdata 
## the output variables contains begin_group prefixes such as demo_ , O1_ etc.


translate_df <- function(
    # A dataframe imported from an ODK aggregator
  df = regdata_xml,
  # XLSForm used to collect data
  form = "forms/Form_4.xlsx",
  # Unique identifier of each row. 
  key_filter = "uuid",
  # Desired language for the translation (it must exist in the XLSForm)
  language = "Português"
  
) {
  
  
  ## arranges the data in decreasing submission time, In case if we have duplicated uuid but different submission time then the latest submission will be considered
  
  if("submission_time" %in% names(df)){
    
    df <- df %>% 
      arrange(desc(submission_time))
  }
  
  
  
  # convert to lowercase 
  language <- tolower(language)
  
  
  # Load required packages 
  library(tidyr)
  
  #### Function to replace NAs 
  replace_na_previous <- function(x, a =! is.na(x)) {
    
    x[which(a)[c(1, 1:sum(a))][cumsum(a) + 1]]
    
  }
  
  ## XLSForm - Survey 
  # Import worksheet "survey"
  form_survey <- readxl::read_excel(form, sheet = "survey") %>% 
    select(type,name, contains("label")) %>%
    rename_with(., 
                ~ tolower(
                  # removes parathesis and text within
                  gsub("\\s*\\([^\\)]+\\)","", .) %>% 
                    # Removes all the text before the matching character
                    gsub(".*:", "", .) %>% 
                    stringr::str_trim()
                )
    ) 
  
  # Removes duplicated columns
  form_survey <- form_survey[, 
                             !duplicated(colnames(form_survey), 
                                         fromLast = TRUE)] 
  
  # Cleans Survey form data
  form_survey <- form_survey %>% 
    dplyr::select(type, name, everything()) %>% 
    # Removes NAs
    filter(!is.na(name))  %>%
    dplyr::mutate(type = stringr::str_trim(type) %>% 
                    gsub(" or_other", "", .)
    ) %>%  
    # Creates variable with group name
    dplyr::mutate(group = ifelse(type == "begin group", name, NA)) %>% 
    # Replace empty cells with group names
    dplyr::mutate(group = replace_na_previous(group)) %>% 
    # Filters out type "begin group"
    dplyr::filter(type != "begin group") %>%
    # Create column with final variable names
    dplyr::mutate(name = paste(group, stringr::str_trim(name), sep="_")) %>% 
    
    # Removes NAs
    #vdplyr::filter(!is.na(label_es) & !is.na(label_pt)) %>%
    # Removes unnecessary rows and extra headers
    dplyr::filter(
      !(type %in% 
          c("note", "begin repeat", "image",
            "text", "integer", "date", "geopoint")
      )
    ) %>% 
    # Keeps only variables present in the final dataset
    dplyr::filter(name %in% names(df)) %>%
    # Selects columns of interest
    dplyr::select(type, name, group, everything()) 
  
  ## Removing words before middle whitespaces to create variable type
  form_survey <- form_survey %>% 
    ## removes the letter after middle white spaces (to get question_type) and to seperate them from variabale names
    mutate(
      options_id = 
        
        data.table::fifelse(
          # Returns logical if the value is one single word
          str_count(trimws(type) ,"\\W+") == 0,
          type,
          (
            type %>% 
              stringr::str_split_fixed(., " ", 2)
          )[,2]
          
        )
    ) %>%     mutate(
      type = (
        type %>% 
          stringr::str_split_fixed(., " ", 2)
      )[,1]) %>% 
    dplyr::select(type, name, options_id, all_of(language)) 
  
  # Renames language variable
  names(form_survey) <- c("type", "name", "options_id", "label")
  
  ## XLSForm - Choices 
  # Import worksheet "choices"
  form_choices <- 
    readxl::read_excel(
      form, 
      sheet = "choices"
    ) %>% 
    
    ## Remove NA rows 
    dplyr::filter(!is.na(list_name)) %>% 
    rename_with(., 
                ~ tolower(
                  # removes parathesis and text within
                  gsub("\\s*\\([^\\)]+\\)","", .) %>% 
                    # Removes all the text before the matching character
                    gsub(".*:", "", .) %>% 
                    stringr::str_trim()
                )
    ) %>% 
    # Converts to lowecase the list of choices; the XML answers will also be converted to lower case to avoid error due to unmatching letters and make the merging process efficient
    mutate(name = name) %>% 
    dplyr::select(list_name, name, all_of(language)) 
  
  # Renames language variable
  names(form_choices) <- c("list_name", "name", "label")
  
  # Removes duplicated columns
  form_choices <- form_choices[, 
                               !duplicated(colnames(form_choices), 
                                           fromLast = TRUE)] 
  
  
  # Make a list of variable names of df 
  variable_list <- data.frame(name = names(df))
  
  # Make a list of variable names from form_Survey$name that also exist in df 
  variable_names <- form_survey$name[form_survey$name %in% variable_list$name]
  
  ###  Use a loop to translate each variable in variable_names
  
  ## introduces _ in answers
  
  form_choices <- form_choices %>%
    mutate(label = 
             gsub(" ","_", stringr::str_trim(label)) 
           
    )
  
  
  # Create intermediary dataframe with variables which have select_multiple answers
  
  multi_var <- form_survey %>% 
    filter(type == "select_multiple") %>% 
    dplyr::select(name, options_id)
  
  
  for (i in 1:length(variable_names)){
    
    ## Get question types of variables
    question_type <- form_survey %>%
      dplyr::filter(form_survey$name == variable_names[i]) %>% 
      dplyr::select(options_id) 
    
    # Create a column of question_type in df to be used as id for joining
    df <- df %>% 
      dplyr::mutate(list_name = "") %>% 
      select(list_name, everything())
    
    df$list_name <- question_type$options_id
    
    
    # Create a column of variable names in df to be used as id for joining
    df <- df %>% 
      mutate(survey_name_var = "") %>% 
      select(survey_name_var, everything())
    
    df$survey_name_var <- variable_names[i]
    
    ## Rename form_choices$name to variable_names to be used as id for joining
    names(form_choices)[names(form_choices) == "name"] <- variable_names[i]
    
    ## If the variabels have select_multiple answers ----
    
    if (variable_names[i] %in% multi_var$name){
      
      # Replace white spaces by "_" in label_en same as they are in form_choices$name
      form_choices_multi <- form_choices %>% 
        filter(form_choices$list_name %in% multi_var$options_id) 
      
      
      # Split the multi_answer into single row answer
      multianswer_split <- df %>% 
        dplyr::select(all_of(key_filter), list_name, variable_names[i]) %>% 
        mutate(!!sym(variable_names[i]) := 
                 strsplit(
                   as.character(
                     !!sym(variable_names[i])
                   ),# %>% tolower(), 
                   " ")
        ) %>% 
        #unnest(base::paste0(variable_names[i]))
        unnest(!!sym(variable_names[i]))
      
      ## Start replacing label values by joining 
      ## Join by list_name(question type) and variable_names
      multianswer_split <- left_join(multianswer_split, form_choices_multi, 
                                     by = c("list_name", paste0(variable_names[i])))  
      
      
      ## Sets language (select_multiple) -
      
      # replaces the varible values by the translated label values
      multianswer_split <- multianswer_split %>% 
        mutate(!!sym(variable_names[i]) := 
                 ifelse(!is.na(label), label, !!sym(variable_names[i]))) %>%
        # removes intermediarary variables
        dplyr::select(-list_name, 
                      -contains("label"), 
                      -contains("survey_name_var"))
      
      
      # Concatenate them back to their original position
      # Summarize 
      df_grouped <- multianswer_split %>% 
        group_by(!!sym(key_filter)) %>% 
        dplyr::summarise( 
          ## collaps them by "space" if original data also have spaces among multi-values
          !!sym(variable_names[i]) := paste(!!sym(variable_names[i]), 
                                            collapse=" ")
        ) %>% 
        ungroup() %>% 
        # replaces all charachter NA by logical NA
        mutate_all(
          ~ replace(., . == "NA", NA)
        )
      
      ## Join df to df_grouped
      df <- left_join(df, df_grouped, by = key_filter) %>%
        ## Remove original answers variable from df that contains .x
        dplyr::select(
          -contains("list_name"), 
          -contains("survey_name_var"), 
          -contains(".x")
        ) 
      
      ## Renames translated variable joined from df_grouped that contains .y
      colnames(df) <- colnames(df) %>%
        base::gsub("\\.y","",.)
      
      ## Remove duplications produced during each iteration of loop
      df <- df %>% 
        filter(!duplicated(!!sym(key_filter)))
      
      
    }else{
      
      ## If variable is select_one type ----
      # Convert variable to character class so as to avoid error
      df <- df %>% 
        mutate(!!sym(variable_names[i]) := !!sym(variable_names[i]) %>% 
                 as.character() #%>% 
               #tolower() %>% 
               # If any xml value has space then adds "_"
               #gsub(" ","_", .)
        ) 
      
      # Join df to form_choices
      df <- left_join(df, form_choices, 
                      by = c("list_name", base::paste0(variable_names[i])))  
      
      ## Sets language (selectone) -
      
      
      df <- df %>% 
        # replaces all charachter NA by logical NA
        mutate_all(
          ~ replace(., . == "NA", NA)
        ) %>% 
        mutate(!!sym(variable_names[i]) := 
                 ifelse(!is.na(label), 
                        # For select_one type answer replace _ by space
                        gsub("_"," ", label), 
                        !!sym(variable_names[i]))) %>%
        dplyr::select(
          -list_name, 
          -contains("label"), 
          -contains("survey_name_var")
        )
      
      ## Remove duplications produced during each iterartion of loop
      df <- df %>% 
        filter(!duplicated(!!sym(key_filter)))
      
    }
    
    # Rename form_choices variable back to its original name # should be in for loop but outside if-else
    names(form_choices)[names(form_choices) == variable_names[i]] <- "name"
    
    
  } # Close for loop
  
  
  ## Reorders the translated variables at the start of the data frame *(multi_vari first and then others)
  df <- df %>% 
    dplyr::select(
      all_of(key_filter), 
      any_of(multi_var$name), 
      any_of(variable_names), 
      everything()
    ) %>% 
    # Filter only distinct observations based on the keyfilter
    distinct(!!sym(key_filter), .keep_all = T) %>% 
    dplyr::select(
      -starts_with("label")
    )
  
  # Print output
  return(df)
  
}

# Translate individual variables based on XLSForm ----
toptranslate_variable <- function(
    # Variable of interest
  variable = regdata$education, 
  # XLSForm choices imported 
  xlsform = form, 
  # List name as from XLSForm choices
  choices = "escolaridade",
  # Output language
  language = "es",
  # Inform if varibale is select_multiple
  select_multiple = FALSE) {
  
  # Require packages
  require(dplyr)
  
  # Check language
  if(language == "es")
    translation <- xlsform %>%
      dplyr::filter(list_name == choices) %>%
      dplyr::select(name, `label::Español`) %>% 
      rename(label_trans = `label::Español`) %>% 
      na.omit() %>% 
      dplyr::mutate_if(is.factor, as.character)
  
  if(language == "en")
    translation <- xlsform %>%
      dplyr::filter(list_name == choices) %>%
      dplyr::select(name, `label::English`) %>% 
      rename(label_trans = `label::English`) %>% 
      na.omit() %>% 
      dplyr::mutate_if(is.factor, as.character)
  
  if(language == "pt")
    translation <- xlsform %>%
      dplyr::filter(list_name == choices) %>%
      dplyr::select(name, `label::Português`) %>% 
      rename(label_trans = `label::Português`) %>% 
      na.omit() %>% 
      dplyr::mutate_if(is.factor, as.character)
  
  # Translate selectone variables
  if(select_multiple == FALSE){
    variable <- data.frame(name = variable) %>% 
      dplyr::mutate_if(is.factor, as.character)
    
    variable <- variable %>%
      left_join(translation, by = c("name")) 
  }
  
  # Translate select_multiple variables
  if(select_multiple == TRUE){
    
    variable <- stringr::str_split(variable, " ") %>% 
      unlist %>% 
      gsub("Clothers", "Clothes", .) %>% 
      tibble::as_tibble() %>% 
      rename(name = value) %>% 
      na.omit()
    
    variable <- variable %>%
      left_join(translation, by = c("name"))
    
  }
  
  return(variable$label_trans)
  
}

# Parse support event participants nested data ----

parse_support_participants <- function(
    column = "activrep_part_repeat",
    df = support # data frame 
) {
  
  #if column of support (df) is NA then returns ()
  if(df %>% dplyr::select(all_of(column)) %>% na.omit %>% nrow == 0)
    return("Aún no hay apoyo.")
  else
    #empty data frame
    data <- data.frame() 
  # Creates index with numbering of delivery i.e. item number by auto increament
  df <- df %>% 
    dplyr::mutate(partnr = 1:nrow(.)) 
  
  # Selects column of interest
  variable <- df %>% 
    dplyr::select(all_of(column)) %>% unlist()# %>% View
  
  
  for (row in 1:length(variable)) {
    rowdata <- data.frame()
    # Creates preliminary dataset with contents of variable
    rowdata <- eval(
      parse(
        text = paste(
          'data.frame(', 
          variable[row], 
          ')'
        )
      )
    ) %>%
      # Adds a new column with the index (row numbers) of the beneficiary in the full dataset
      dplyr::mutate(partnr = row) %>% 
      mutate_all(., as.character)
    
    # Binds datasets in rows
    data <- dplyr::bind_rows(data, rowdata) %>% 
      dplyr::select(partnr, everything()) 
  }
  
  
  # Adjusts names automatically (removes wordes before the last point)
  names(data) <- data %>% 
    names( ) %>%
    sub(".*\\.", "", .) %>%
    gsub("part_", "benef_", .)
  
  
  
  return(data)  
  
}

# Parse nested data ----
parse_item_date <- function(
    column = "hygiene_sanitation_num_items_hygiene_sanitation", # list of items
    df = item_delivery # data frame of all items (listed by columns) received by a person
) {
  
  
  #if column of received item (df) is NA then returns ()
  if(df %>% dplyr::select(tidyselect::all_of(column)) %>% na.omit %>% nrow == 0)
    return("Ainda não existem entregas registradas")
  else
    #empty data frame
    data <- data.frame() 
  # Creates index with numbering of delivery i.e. item number by auto increament
  df <- df %>% 
    dplyr::mutate(itemnr = 1:nrow(.)) 
  
  # Selects column of interest
  variable <- df %>% 
    dplyr::select(tidyselect::all_of(column)) %>% unlist()
  
  
  for (row in 1:length(variable)) {
    rowdata <- data.frame()
    # Creates preliminary dataset with contents of variable
    rowdata <- eval(parse(text=paste('data.frame(', variable[row], ')'))) %>%
      # Adds a new column with the index (row numbers) of the beneficiary in the full dataset
      dplyr::mutate(itemnr = row) %>% 
      mutate_all(., as.character)
    
    # Binds datasets in rows
    data <- dplyr::bind_rows(data, rowdata) %>% # empty data is filled with rowdata
      dplyr::select(-contains("itemnr_current"), -contains("NA")) %>% # removing the variables with "prefixes" and "NA"
      # keeping itemnrnr at column index 1 and then rest as following
      dplyr::select(itemnr, everything()) 
  }
  
  # Adjusts names automatically
  names(data) <- data %>% 
    names( ) %>%
    sub(".*\\.", "", .) %>% 
    sub("\\_.*", "", .)
  
  # Removes duplicated columns
  data <- data %>% 
    setNames(make.names(names(.), unique = TRUE)) %>% 
    dplyr::select(-matches("*\\.[1-9]+$"))
  
  # merging data by itemnr_nr
  data <- merge(
    data, df %>%
      dplyr::select(itemnr), by="itemnr") %>%
    dplyr::mutate(item = ifelse(item == "Other", other, item)) #%>% 
  #  dplyr::select(-other)
  
  # Filters out NAs
  # this filters the rows which contain NA in variables with name "units" in it
  # data <- data[!Reduce("&", lapply(data[grep("^units", names(data))], is.na)),]
  if("units" %in% colnames(data)){
    data <- data[!Reduce("&", lapply(data[grep("^units", names(data))], is.na)),]
  }else{
    data <- data %>% dplyr::mutate(units = NA)
    data <- data[!Reduce("&", lapply(data[grep("^units", names(data))], is.na)),]
  }
  
  # Translates names of variables
  names(data) <- names(data) %>% 
    gsub("outro", "other", .) %>% 
    gsub("peso", "weight", .) 
  
  return(data)  
  
}

# Highcharts for multi variables -----

multi_highchart <- function(
    variable = regdata$needs,
    title = "¿Cuáles són tus necesidades más urgentes?",
    name = "Necesidades",
    colorByPoint = TRUE,
    credit_text = "Programa Europana",
    type = "bar",
    units = "registros",
    brewerpal = "Set3",
    max.obs = Inf
) {
  # Required packages
  invisible(library(RColorBrewer))
  invisible(library(dplyr))
  
  # Split variables
  data <- variable %>% 
    unlist %>% 
    strsplit(., " ") %>% 
    unlist %>% 
    na.omit() %>% 
    gsub("_", " ", .) 
  
  # Order observations by frequency
  tb <- table(data) %>% 
    # Set maximum number of observations 
    head(max.obs)
  
  data <- factor(data,
                 levels = names(tb[order(tb, decreasing = TRUE)]))
  
  # Save intermediary highchart
  hcplot <- hchart(
    data,
    type = type,
    colorByPoint = colorByPoint,
    name = name
  ) %>%
    # Add credit box
    hc_credits(enabled = TRUE,
               text = credit_text,
               style = list(fontSize = "10px")) %>%
    
    # Add tooltip (box when hoovering the mouse)
    hc_tooltip(crosshairs = TRUE, borderWidth = 1,
               sort = TRUE, shared = TRUE, table = TRUE
    ) %>%
    
    #  Add title
    hc_title(text = title) %>% 
    
    # Add subtitle
    hc_subtitle(text =
                  paste(
                    "N = ", length(variable %>% na.omit), units)
    ) 
  
  # Check if Brewer palette will meet the variable size    
  if(
    length(
      unique(
        variable
      )
    ) <= RColorBrewer::brewer.pal.info[brewerpal,]["maxcolors"]
    
  ){
    # Return plot with brewer palette
    hcplot %>% 
      # Add theme and colors
      hc_add_theme(
        hc_theme_smpl(
          colors = RColorBrewer::brewer.pal(
            # Number of colours
            n = length(
              unique(
                variable
              )
            ), 
            # Palette name
            name = brewerpal
          )
        )
      )
    
  }else{
    
    # Print plot without Brewer palette
    colourCount = length(tb)
    getPalette = colorRampPalette(
      brewer.pal(9, brewerpal)
    )
    
    hcplot %>% 
      # Add theme and colors
      hc_add_theme(
        hc_theme_smpl(
          colors = getPalette(colourCount)
        )
      )
  }
}


# Highcharts for single variables -----

top_highchart <- function(
    variable = regdata$education,
    title = "Educación de las personas registradas",
    name = "Niveles educacionales",
    colorByPoint = TRUE,
    credit_text = "Programa Europana",
    type = "bar",
    units = "registros",
    brewerpal = "Set3",
    max.obs = Inf
) {
  
  # Required packages
  invisible(library(RColorBrewer))
  invisible(library(dplyr))
  
  # Split variables
  data <- variable %>% 
    na.omit() %>% 
    gsub("_", " ", .) 
  
  # Order observations by frequency
  tb <- table(data) %>% 
    # Set maximum number of observations 
    head(max.obs)
  
  data <- factor(data,
                 levels = names(tb[order(tb, decreasing = TRUE)]))
  
  # Save intermediary highchart
  hcplot <- # Plot highchart
    hchart(
      data,
      type = type,
      colorByPoint = colorByPoint,
      name = name
    ) %>%
    # Add credit box
    hc_credits(enabled = TRUE,
               text = credit_text,
               style = list(fontSize = "10px")) %>%
    
    # Add tooltip (box when hoovering the mouse)
    hc_tooltip(crosshairs = TRUE, borderWidth = 1,
               sort = TRUE, shared = TRUE, table = TRUE
    ) %>%
    
    #  Add title
    hc_title(text = title) %>% 
    
    # Add subtitle
    hc_subtitle(text =
                  paste(
                    "N = ", length(variable %>% na.omit), units)
    ) 
  
  # Check if Brewer palette will meet the variable size    
  if(
    length(
      unique(
        variable
      )
    ) <= RColorBrewer::brewer.pal.info[brewerpal,]["maxcolors"]
    
  ){
    # Return plot with brewer palette
    hcplot %>% 
      # Add theme and colors
      hc_add_theme(
        hc_theme_smpl(
          colors = RColorBrewer::brewer.pal(
            # Number of colours
            n = length(
              unique(
                variable
              )
            ), 
            # Palette name
            name = brewerpal
          )
        )
      )
    
  }else{
    
    # Print plot without Brewer palette
    colourCount = length(tb)
    getPalette = colorRampPalette(
      brewer.pal(8, brewerpal)
    )
    
    hcplot %>% 
      # Add theme and colors
      hc_add_theme(
        hc_theme_smpl(
          colors = getPalette(colourCount)
        )
      )
  }
  
}




# Ordered barplots ----
topbarplot_ordered <- function(column = baseline$demo_age, # Factor variable 
                               levels = c("0-6", "7-14", "15-17",
                                          "18-24", "25-29", "30-40",   
                                          "41-50", "51-60", "61_or_more"), # Levels
                               title = "Some plot", 
                               label = "Responses", 
                               units = "responses",
                               brewerpal = "Spectral",
                               titlebreak = 60, 
                               size = 13, 
                               label_size = 4,
                               title_size = 15,
                               vjust = 0.5, 
                               hjust = -5, 
                               direction = 1, 
                               xlab_break = 90, 
                               big.mark = ".", 
                               decimal.mark = ",",
                               decreasing = TRUE
) {
  
  ## brewerpal = string with brewer.pal palette (see: https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf)
  ## max.obs = numeric value limiting the number of results to plot
  
  library(ggplot2)
  library(scales)
  library(stringr)
  library(dplyr)
  library(RColorBrewer)
  
  
  # Checks if variable has available data
  if(
    column %>% 
    na.omit %>% 
    length() > 0
  ){
    
    
    # Remove underscores from provided levels  
    levels <- levels %>% 
      base::gsub("_", " ", .) %>% 
      # Sets to lower and capitalise first letter
      tolower() %>% 
      Hmisc::capitalize()
    
    
    # Transform into dataframe with counts and remove underscores
    table <- column %>% 
      na.omit() %>% 
      as.character() %>% 
      base::gsub("_", " ", .) %>% 
      # Sets to lower and capitalise first letter
      tolower() %>% 
      Hmisc::capitalize() %>% 
      base::factor(., levels = levels) %>% 
      table(.) %>% 
      as.data.frame()
    
    p <- ggplot(table, aes(x=table[,1], 
                           fill = table[,1],
                           y=table[,2])
    ) + 
      labs(y = label) +
      geom_bar(position = "dodge", 
               stat = "identity") +
      theme_minimal() +
      coord_flip() +
      theme(legend.position="none")  +
      # Includes plot title
      ggtitle(
        stringr::str_wrap(title,
                          width = titlebreak),
        subtitle =
          paste("N =",
                format(sum(table[,2]),
                       big.mark = big.mark,
                       decimal.mark = decimal.mark), units)
      ) +
      # Removes title for y axis
      theme(axis.title.y = element_blank()) +
      # Prints values
      geom_text(
        aes(label =
              base::paste0(
                scales::percent(
                  table[,2]/table %>%
                    dplyr::select(Freq) %>%
                    unlist() %>%
                    sum(), accuracy = 0.1
                ),
                " (",
                table[,2],
                ")"
              )
        ),
        hjust = -0.1, vjust = 0.2, size = label_size)  +
      # Changes size and format of axis and title
      theme(axis.text=element_text(size = size),
            axis.title=element_text(size = size)) +
      theme(plot.title = element_text(size = title_size, face = "bold")) +
      # Adjust scales
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
                         limits = c(0, max(table[,2]*1.15)))
    
    
    # Runs condition on number of levels for setting colours
    if(nrow(table) >= RColorBrewer::brewer.pal.info["Accent",]["maxcolors"]){
      
      # Creates expanded Brewer palette
      colourCount = nrow(table)
      getPalette = colorRampPalette(
        brewer.pal(8, brewerpal)
      )
      
      p +  
        scale_fill_manual(
          values = 
            if(direction == -1){
              rev(getPalette(colourCount))
            }else{
              getPalette(colourCount)
            }
        )
      
    }else{ # For cases with over 12 levels due to color palette
      
      p + scale_fill_brewer(palette = brewerpal, 
                            direction = direction)
    }
    
  }else{
    
    ## Renders empty ggplot if data is not avaiable
    
    ggplot(df <- data.frame(), aes(x = "", y = "")) + geom_blank() +
      
      # geom_bar(position = "dodge", 
      #          stat = "identity") +
      theme_minimal() +
      coord_flip() +
      theme(legend.position="none")  +
      # Includes plot title
      ggtitle(
        stringr::str_wrap(title,
                          width = titlebreak),
        subtitle =
          paste("N = 0", units)
      ) +
      # Removes title for y axis
      theme(axis.title.y = element_blank()) +
      # Prints values
      geom_text(
        aes(label =
              paste0("No data available for"),
            colour= "red"
        ),
        hjust = -0.1, vjust = 0.2, size = 5) +
      
      # Changes size and format of axis and title
      theme(axis.text=element_text(size = 12),
            axis.title=element_text(size = 12)) +
      theme(plot.title = element_text(size = title_size, face = "bold"))  
    
    #cat(paste("''",title,"''"), fill = TRUE, labels = paste0("No data available for"))
  }
  
}


######### Extract items (simple) in dataframe format for Consulta -----
# "women_hygiene_num_items_women_hygiene",
# "hygiene_sanitation_num_items_hygiene_sanitation",
# "kitchen_num_items_kitchen",
# "bed_table_bath_num_items_bed_table_bath",
# "foodsec_num_foodpacks",
# "foodsec_num_vouchers",
# "foodsec_num_foodportions",
# "foodsec_num_chicken",
# "foodsec_num_pigs",
# "foodsec_num_fish",
# "foodsec_num_farm_inputs",
# "foodsec_num_other_itens"

extract_items_simple <- function(
    column = "women_hygiene_num_items_women_hygiene",
    df = item_delivery,
    benef_id = NA,#"12345",
    benef_birthcertif_nr = NA, #"71332578152",#"70544580281", #
    benef_name = NA # "0160" extr
    
) {
  
  # Check if the item of interest is related to food security
  
  # Extract name of item category
  item_category <- 
    gsub("_num_items_.*","", column) 
  
  
  ## Extract name of related addinfo variable 
  addinfo_variable <- gsub("_num_items_","_addinfo_", 
                           column)
  
  
  # Extract data based on input
  
  if(
    !is.na(benef_id)
  ){
    
    registered_item <- df %>%
      # Filter for beneficiary ID
      dplyr::filter(demo_id_nr == benef_id)  
    
  }else{
    
    if(
      !is.na(benef_name)
    ){
      
      registered_item <- df %>%
        # Filter for beneficiary ID
        dplyr::filter(demo_fullname == benef_name) 
      
    }else{
      
      if(
        !is.na(benef_birthcertif_nr)
      ){
        
        registered_item <- df %>%
          # Filter for beneficiary ID
          dplyr::filter(demo_birthcertif_nr == benef_birthcertif_nr)  
      }
    }
  }
  
  # If addinfo variable is not avaialable then copies the item category 
  
  if(registered_item %>% dplyr::select(contains(addinfo_variable)) %>% ncol == 0
     
  ){
    
    #item_category <- gsub("kits de ", "", tolower(item_category))
    
    registered_item[addinfo_variable] <- paste0(item_category)
  }
  
  # Renames variable with additional information 
  registered_item <- registered_item %>%
    
    
    dplyr::select(
      contains(item_category), 
      interno_municipality, 
      addinfo_variable,
      filter_date, 
      interno_implementing_partner#,
      #food_addinfo_food
    ) %>%
    dplyr::filter(
      !is.na(
        !!sym(column) 
      ) &
        !!sym(column) != 0
    ) 
  
  ## If there is no item_delivery !!
  
  if(registered_item %>% nrow == 0){
    # For cases where the person did not receive anything
    registered_item <- data.frame(
      info = paste(
        "Artículos - ", 
        gsub("_", " ", item_category), 
        ": ", sep ="") %>% 
        gsub("women hygiene",
             "kits de higiene para mulheres", .) %>% 
        gsub("hygiene sanitation", 
             "kits de higiene e limpeza", .) %>% 
        gsub("kitchen",
             "kits de cozinha", .) %>% 
        gsub("bed table bath", 
             "kits domésticos (roupas de cama, lençóis, cobertores)", .) %>% 
        gsub("food",
             "alimentos", .), 
      items_list = "Ainda não existem entregas registradas")
    
  }else{
    
    ## renames unit column and addinfo 
    names(registered_item)[names(registered_item) == column] <- "units"
    names(registered_item)[names(registered_item) == addinfo_variable] <- "item"
    
    ## Trim whitespaces and capitalizes
    registered_item <- 
      registered_item %>%
      dplyr::mutate_if(is.factor, as.character) %>%
      dplyr::mutate(
        item = 
          trimws(Hmisc::capitalize(item))
      ) %>%
      dplyr::mutate(units = trimws(units)) 
    
    # Start concatenation
    registered_item <- 
      registered_item %>%
      dplyr::select(filter_date, interno_municipality, item, interno_implementing_partner, units
      ) %>%
      tibble::as_tibble() %>%
      
      dplyr::mutate(filter_date = as.Date(filter_date) %>% 
                      format("%d/%m/%Y")
      ) %>% 
      # Concatenates items with units
      dplyr::mutate(
        items_list =
          paste0(
            item,
            " [",
            units,
            " unid.", 
            "]"
          )
      ) %>%
      dplyr::select(filter_date, interno_municipality, interno_implementing_partner, 
                    item, items_list, units) %>%
      # remove the _ in names of items
      dplyr::mutate(
        items_list = gsub("_", " ", tolower(items_list))
      )
    
    # Summarize items_list by date (filter_date)
    registered_item <- registered_item %>%
      dplyr::select(filter_date, interno_municipality, interno_implementing_partner, items_list) %>%
      dplyr::group_by(filter_date, interno_municipality, interno_implementing_partner) %>%
      dplyr::summarise(
        items_list = paste(items_list, collapse=", "))
    # Concatenates date (filter_date) to summarized date_item
    registered_item <- registered_item %>%
      tibble::as_tibble() %>%
      # arrange dates in decreasing order
      arrange(
        desc(
          as.Date(filter_date, format("%d/%m/%Y")
          )
        )
      ) %>%
      # Concatenates data into one variable (items_list)
      dplyr::mutate(items_list =
                      paste0(
                        "(",
                        filter_date, 
                        " - Parceiro: ", 
                        interno_implementing_partner, ", Município: ",
                        interno_municipality,
                        ", Itens: ", 
                        items_list,
                        ")"
                      )
      ) %>%
      dplyr::select(items_list)
    # Add info if the items are delivered
    registered_item <- registered_item %>%
      dplyr::mutate(
        info = paste(
          "Artículos - ", 
          gsub("_", " ", item_category), 
          ": ", sep ="") %>% 
          gsub("women hygiene",
               "kits de higiene para mulheres", .) %>% 
          gsub("hygiene sanitation", 
               "kits de higiene e limpeza", .) %>% 
          gsub("kitchen",
               "kits de cozinha", .) %>% 
          gsub("bed table bath", 
               "kits domésticos (roupas de cama, lençóis, cobertores)", .) %>% 
          gsub("food",
               "alimentos", .) 
      ) %>%
      dplyr::select(info, everything())
    
    # Summarize items_list by info (item_category)
    registered_item <- registered_item %>%
      dplyr::group_by(info) %>%
      dplyr::summarise(
        items_list = paste(items_list, collapse="; ")) %>% 
      dplyr::mutate(
        items_list =
          items_list %>% 
          gsub("women hygiene",
               "kits de higiene para mulheres", .) %>% 
          gsub("hygiene sanitation", 
               "kits de higiene e limpeza", .) %>% 
          gsub("kitchen",
               "kits de cozinha", .) %>% 
          gsub("bed table bath", 
               "kits domésticos (roupas de cama, lençóis, cobertores)", .) %>% 
          gsub("food",
               "alimentos", .) 
      ) 
    
  }
  
  return(registered_item)
}

######### Extract items in dataframe format for Consulta -----
# "materials_items_materials",
# "office_items_office",
# "others_items_others",
# "equipment_items_equipment",
# "clothing_items_clothing"



extract_items <- function(
    column = "others_items_others",
    df = item_delivery,
    form = "forms/Form_2_itemdelivery_ADRA.xlsx",
    language = "portuguese",
    benef_id = NA,
    benef_birthcertif_nr = NA, 
    benef_name = NA
    
) {
  
  # Loads packages
  library(janitor)
  library(dplyr)
  
  # Prepares intermediary dataN
  item_category <- gsub("_items_.*","", paste0(column))
  
  # If addinfo variable is not avaialable then copies the item category 
  
  if(
    !(column %in% colnames(df))
    
  ){
    
    df[tidyselect::all_of(column)] <- NA
  }
  
  # Subfunction to parse items by date ----
  
  parse_item_date <- function(
    column = "materials_items_materials", # list of items
    df = received_items # data frame of all items (listed by columns) received by a person
  ) {
    
    #if column of received item (df) is NA then returns ()
    if(df %>% dplyr::select(tidyselect::all_of(column)) %>% na.omit %>% nrow == 0)
      return("Ainda não há entregas.")
    else
      #empty data frame
      data <- data.frame() 
    # Creates index with numbering of delivery i.e. item number by auto increament
    df <- df %>% 
      dplyr::mutate(itemnr = 1:nrow(.)) 
    
    # Selects column of interest
    variable <- df %>% 
      dplyr::select(tidyselect::all_of(column)) %>% unlist()
    
    
    for (row in 1:length(variable)) {
      rowdata <- data.frame()
      # Creates preliminary dataset with contents of variable
      rowdata <- eval(
        parse(
          text = paste(
            'data.frame(', 
            variable[row], 
            ')'
          )
        )
      ) %>%
        # Adds a new column with the index (row numbers) of the beneficiary in the full dataset
        dplyr::mutate(itemnr = row) %>% 
        mutate_all(., as.character)
      
      # Binds datasets in rows
      data <- dplyr::bind_rows(data, rowdata) %>% # empty data is filled with rowdata
        dplyr::select(
          -contains("itemnr_current"), 
          -contains("NA")
        ) %>% # removing the variables with "prefixes" and "NA"
        # keeping itemnrnr at column index 1 and then rest as following
        dplyr::select(itemnr, everything()) 
    }
    
    # Adjusts names automatically
    names(data) <- data %>% 
      names( ) %>%
      sub(".*\\.", "", .) %>% 
      sub("\\_.*", "", .)
    
    # Removes duplicated columns
    data <- data %>% 
      setNames(
        make.names(
          names(.), 
          unique = TRUE
        )
      ) %>% 
      dplyr::select(
        -matches("*\\.[1-9]+$"
        )
      )
    
    # merging data by itemnr_nr
    data <- merge(
      data, 
      df %>%
        dplyr::select(filter_date, itemnr), 
      by="itemnr") %>%
      dplyr::mutate(
        item = 
          ifelse(item == "Other", other, item)
      ) #%>% 
    #  dplyr::select(-other)
    
    # Filters out NAs
    # this filters the rows which contain NA in variables with name "units" in it
    # data <- data[!Reduce("&", lapply(data[grep("^units", names(data))], is.na)),]
    if("units" %in% colnames(data)){
      data <- 
        data[!Reduce("&", 
                     lapply(data[grep("^units", 
                                      names(data))],
                            is.na)),]
    }else{
      
      data <- data %>% 
        dplyr::mutate(units = NA)
      
      data <-
        data[!Reduce("&", 
                     lapply(data[grep("^units", 
                                      names(data))], 
                            is.na)),]
    }
    
    # Translates names of variables
    names(data) <- names(data) %>% 
      gsub("outro", "other", .) %>% 
      gsub("peso", "weight", .) 
    
    return(data)  
    
  }
  
  # Extract data based on input 
  
  if(
    !is.na(benef_id)
  ){
    
    received_items <- df %>%
      # Filter for beneficiary ID
      dplyr::filter(demo_id_nr == benef_id)  
    
  }else{
    
    if(
      !is.na(benef_name)
    ){
      
      received_items <- df %>%
        # Filter for beneficiary ID
        dplyr::filter(demo_fullname == benef_name) 
      
    }else{
      
      if(
        !is.na(benef_birthcertif_nr)
      ){
        
        received_items <- df %>%
          # Filter for beneficiary ID
          dplyr::filter(demo_birthcertif_nr == benef_birthcertif_nr)  
      }
    }
  }
  
  # Selects variable from extracted data
  
  received_items <- received_items %>%
    # Selects columns of interest
    dplyr::select(filter_date, interno_implementing_partner,
                  demo_item_categ_delivery,
                  comments_team_member,
                  contains("_items_"),
                  contains("municipality"),
                  contains(item_category),
                  -contains("_count")
    ) %>%
    
    dplyr::mutate(itemnr = 1:nrow(.)) %>%
    # Arranges by date
    arrange(desc(as.Date(filter_date)))
  
  # Replaces values with "NULL" by NAs
  received_items[received_items == "NULL"] = NA
  
  # Returns dataframe if there is no data (no delivery)
  
  # Return NA when there is no data of items delivery
  if(
    received_items %>%  
    dplyr::select(tidyselect::all_of(column)) %>% 
    na.omit %>% 
    nrow == 0
  ){
    # For cases where the person did not receive anything
    registered_item <- data.frame(
      info = paste(
        "Artículos - ", 
        gsub("_", " ", item_category), 
        ": ", sep ="") %>% 
        
        gsub("livestock",
             "pecuário", .) %>% 
        gsub("seeds",
             "sementes", .) %>% 
        gsub("vehicles",
             "veículos", .) %>% 
        gsub("equipment agri",
             "equipamento agrícola", .) %>% 
        gsub("equipment",
             "equipamento", .) %>% 
        gsub("agric inputs",
             "Insumos para hortas familiares / comunitárias (ferramentas, sementes)  ", .) %>%
        gsub("office", 
             "escritório", .) %>% 
        gsub("materials", 
             "materiales", .) %>%
        gsub("clothing", 
             "roupas", .) %>% 
        gsub("health",
             "saúde", .) %>% 
        gsub("others", 
             "outras", .),
      items_list = "Ainda não existem entregas registradas")
    
    
  }else{
    
    
    
    # Uses parse_item function 
    # The parse_item_date: parse the list and group by items with date
    registered_item <- parse_item_date(
      df = received_items,
      column = column)
    
    #### Translating registered_item ----
    # Rename item to its original variable name as it is in Form 2 also name unit back to its original variable name
    
    names(registered_item)[names(registered_item) == "item"] <- 
      gsub("_items_","_item_", paste0(column))
    
    names(registered_item)[names(registered_item) == "unit"] <- 
      gsub("_items_","_unit_type_items_", paste0(column))
    
    # Load required packages 
    library(tidyr)
    
    #### Function to replace NAs 
    replace_na_previous <- function(x, a =! is.na(x)) {
      
      x[which(a)[c(1, 1:sum(a))][cumsum(a) + 1]]
      
    }
    
    
    ## XLSForm - Survey 
    # Import worksheet "survey"
    form_survey <- readxl::read_excel(form, sheet = "survey")
    
    ##clean form-survey variables
    names(form_survey) <-  names(form_survey) %>% 
      # Removes signs needed in XLSForms
      gsub("::", "_",.) %>% 
      # Simplifies names
      gsub("Português ", "", .) %>% 
      gsub("English ", "", .) %>% 
      gsub("Umbundu ", "", .) %>% 
      gsub("[()]", "", .) 
    
    form_survey <- form_survey[, 
                               !duplicated(colnames(form_survey), 
                                           fromLast = TRUE)] 
    
    # Cleans Survey form data
    form_survey <- form_survey %>% 
      dplyr::select(type, name, starts_with("label")) %>% 
      # Removes NAs
      na.omit()  %>%
      # Creates variable with group name
      dplyr::mutate(group = ifelse(type == "begin group", name, NA)) %>% 
      # Replace empty cells with group names
      dplyr::mutate(group = replace_na_previous(group)) %>% 
      # Filters out type "begin group"
      dplyr::filter(type != "begin group") %>%
      
      # Create column with final variable names
      dplyr::mutate(name = paste(group, name, sep="_")) %>% 
      # Removes NAs
      #vdplyr::filter(!is.na(label_umb) & !is.na(label_pt)) %>%
      # Removes unnecessary rows and extra headers
      dplyr::filter(
        !(type %in% 
            c("note", "begin_repeat", "image",
              "text", "integer", "date")
        )
      ) %>% 
      # Keeps only variables present in the final dataset
      dplyr::filter(name %in% names(registered_item)) %>%
      # Selects columns of interest
      dplyr::select(type, name, contains("label"))
    
    ## Removing words before middle whitespaces to create variable type
    form_survey <- form_survey %>% 
      #dplyr::mutate(type = sub(".*? ", "", form_survey$type)) %>% 
      ## removes the letter after middle white spaces (to get question_type) and to seperate them from variabale names
      dplyr::mutate(type = sub(" .*", "", form_survey$type)) %>% 
      ## removes the letter before middle white spaces to make them identical to form_choices$list_name
      dplyr::mutate(varname_type = sub(".* ", "", form_survey$type)) %>% 
      dplyr::select(type, varname_type, everything())
    
    
    
    ## XLSForm - Choices 
    # Import worksheet "choices"
    form_choices <- readxl::read_excel(form, sheet = "choices")
    
    ## Remove NA rows 
    form_choices <- form_choices %>% 
      dplyr::filter(!is.na(list_name))
    
    # Adjusts names of dataframe
    names(form_choices) <- names(form_choices) %>% 
      # Removes signs needed in XLSForms
      gsub("::", "_",.) %>% 
      # Simplifies names
      gsub("Português ", "", .) %>% 
      gsub("English ", "", .) %>% 
      gsub("Umbundu ", "", .) %>% 
      gsub("[()]", "", .)  
    
    form_choices <- form_choices[, !duplicated(colnames(form_choices), fromLast = TRUE)] 
    
    # Checks for duplicated columns
    if(form_choices %>% names %>% duplicated() %>% sum != 0) {
      
      # Removing unnecessary columns
      form_choices <- form_choices %>% 
        dplyr::mutate(
          label_en = gsub(" ","_", label_en)
        ) %>% 
        dplyr::select(contains("name") -contains("name_"), contains("label")) 
      
    }else{
      
      form_choices <- form_choices %>%  
        dplyr::select(contains("name"), -contains("name_"), contains("label")) 
      
    }
    
    # Make a list of variable names of registered_item 
    variable_list <- data.frame(name = names(registered_item))
    
    # Make a list of variable to be translated from registered_item that exist in  form_Survey$name 
    variable_names <- form_survey$name[form_survey$name %in% variable_list$name]
    
    
    # Start translating variable  
    for (i in 1:length(variable_names)){
      
      ## Get question types of variables
      question_type <- form_survey %>%
        dplyr::filter(form_survey$name == variable_names[i]) %>% 
        dplyr::select(varname_type) 
      
      # Create a column of question_type in registered_item to be used as id for joining
      registered_item <- registered_item %>% 
        dplyr::mutate(list_name = "")
      
      registered_item$list_name <- question_type$varname_type
      
      # Add names from form_survey to form_choices
      form_choices$survey_name_var <- form_survey$name[match(form_choices$list_name,
                                                             form_survey$varname_type)]
      
      # Reoder columns for visualisation (Development only)
      form_choices <- form_choices %>% 
        dplyr::select(list_name, name, survey_name_var, everything())
      
      # Create a column of variable names in registered_item to be used as id for joining
      registered_item <- registered_item %>% 
        dplyr::mutate(survey_name_var = "")
      
      registered_item$survey_name_var <- variable_names[i]
      
      ## Rename form_choices$name to variable_names to be used as id for joining
      names(form_choices)[names(form_choices) == "name"] <- variable_names[i]
      
      
      # Convert variable to character class so as to avoid error
      registered_item <- registered_item %>%
        dplyr::mutate(!!sym(variable_names[i]) := !!sym(variable_names[i]) %>%
                        as.character())
      
      # Join registered_item to form_choices
      registered_item <- left_join(registered_item, form_choices, 
                                   by = c("list_name", paste0(variable_names[i])))
      
      
      ## Sets language (selectone) and translate by replacing the label -
      
      if(language == "english")
        registered_item <- registered_item %>% 
        dplyr::mutate(!!sym(variable_names[i]) := 
                        ifelse(!is.na(label_en), label_en, !!sym(variable_names[i]))) %>%
        dplyr::select(-list_name, -contains("label_"), -contains("survey_name_var"))
      
      if(language == "umbundu")
        registered_item <- registered_item %>% 
        dplyr::mutate(!!sym(variable_names[i]) := 
                        ifelse(!is.na(label_umb), label_umb, !!sym(variable_names[i]))) %>%
        dplyr::select(-list_name, -contains("label_"), -contains("survey_name_var"))
      
      if(language == "portuguese")
        registered_item <- registered_item %>% 
        dplyr::mutate(!!sym(variable_names[i]) := 
                        ifelse(!is.na(label_pt), label_pt, !!sym(variable_names[i]))) %>%
        dplyr::select(-list_name, -contains("label_"), -contains("survey_name_var"))
      
      
      # Rename form_choices variable back to its original name # should be or not in for loop but outside if-else
      names(form_choices)[names(form_choices) == variable_names[i]] <- "name"
      
      
      # Rename registered_items variable back to its original name
      names(registered_item)[names(registered_item) == variable_names[i] & grepl("_unit_type_items_", variable_names[i])] <- "unit"
      names(registered_item)[names(registered_item) == variable_names[i] & grepl("_item_", variable_names[i])] <- "item"
      
      
      
      # Remove label variables
      registered_item <- registered_item %>% 
        
        dplyr::select(-starts_with("label"))
      
    }
    
    
    # if there is no delivery data
    
    if(length(registered_item)==1){
      
      # For cases where the person did not receive anything
      registered_item <- data.frame(
        info = paste(
          "Artículos - ", 
          gsub("_", " ", item_category), 
          ": ", sep ="") %>% 
          
          gsub("livestock",
               "pecuário", .) %>% 
          gsub("seeds",
               "sementes", .) %>% 
          gsub("vehicles",
               "veículos", .) %>% 
          gsub("equipment agri",
               "equipamento agrícola", .) %>% 
          gsub("equipment",
               "equipamento", .) %>% 
          gsub("agric inputs",
               "Insumos para hortas familiares / comunitárias (ferramentas, sementes)  ", .) %>%
          gsub("office", 
               "escritório", .) %>% 
          gsub("materials", 
               "materiales", .) %>%
          gsub("clothing", 
               "roupas", .) %>% 
          gsub("health",
               "saúde", .) %>% 
          gsub("others", 
               "outras", .),
        items_list = "Ainda não existem entregas registradas")
      
      # if there is delivery data
      
    }else{
      
      
      # Subsets intermediary dataframe with interno_municipality names and date of delivery to merge into registered item
      municipality_df <- received_items %>%
        select (interno_municipality, filter_date) %>%
        dplyr::mutate(
          interno_municipality = gsub("_", " ", interno_municipality) 
        )
      
      # list items list awardin
      df_ngo <- received_items %>%
        select (ngo = "interno_implementing_partner" , filter_date) %>%
        dplyr::mutate(
          ngo = gsub("_", " ", ngo) 
        )
      
      
      # Merges cities to regi stered items dataframe
      registered_item <- merge(
        registered_item, municipality_df , by="filter_date")
      
      registered_item <- merge(
        registered_item, df_ngo , by="filter_date")
      
      # Simplified names of variables
      names(registered_item) <- 
        gsub("_.*","", names(registered_item))
      
      if(item_category %in% 
         c("hygiene_sanitation", "others") & 
         length(registered_item)==5){
        
        # when length is 5, we have description
        registered_item <- registered_item %>%
          dplyr::group_by(itemnr, interno_municipality, item, ngo, filter_date) %>%
          dplyr::summarise(
            units = sum(as.numeric(units))) %>%
          ungroup() %>%
          #Change the date formate
          dplyr::mutate(
            filter_date = as.Date(filter_date) %>% 
              format("%d/%m/%Y")
          )
        
      }else{
        
        if(item_category %in% 
           c("hygiene_sanitation", "others") & 
           length(registered_item)==3){
          
          #when length is 3 or <5, we have dont have description
          registered_item <- registered_item %>%
            dplyr::group_by(itemnr, interno_municipality, item, ngo, filter_date) %>%
            dplyr::summarise(
              units = sum(as.numeric(units))) %>%
            ungroup() %>%
            dplyr::mutate(filter_date = as.Date(filter_date) %>% 
                            format("%d/%m/%Y"))
          
        }else{
          
          registered_item <- registered_item %>%
            dplyr::group_by(itemnr, interno_municipality, item, ngo, filter_date) %>%
            dplyr::summarise(
              units = sum(as.numeric(units))
            ) %>%
            ungroup() %>%
            # Change the date formate
            dplyr::mutate(filter_date = as.Date(filter_date) %>% 
                            format("%d/%m/%Y"))
        }
      }
      
      # Start concatenation
      registered_item <- registered_item %>%
        dplyr::select(filter_date, interno_municipality, item, ngo, units) %>%
        tibble::as_tibble() %>%
        # Concatenates items with units
        dplyr::mutate(items_list =
                        paste0(
                          tolower(item), " [", units," unid.", "]")) %>%
        dplyr::select(filter_date, interno_municipality, ngo, item, 
                      items_list, units) %>%
        # remove the _ in names of items
        dplyr::mutate(
          items_list = gsub("_", " ", items_list)
        )
      
      # Summarize items_list by date (filter_date)
      registered_item <- registered_item %>%
        dplyr::select(filter_date, interno_municipality, ngo, items_list) %>%
        dplyr::group_by(filter_date, interno_municipality, ngo) %>%
        dplyr::summarise(
          items_list = paste(items_list, collapse=", "))
      # Concatenates date (filter_date) to summarized date_item
      registered_item <- registered_item %>%
        tibble::as_tibble() %>%
        # arrange dates in decreasing order
        arrange(
          desc(
            as.Date(
              filter_date, 
              format("%d/%m/%Y")
            )
          )
        ) %>%
        # Concatenates data into one variable (items_list)
        dplyr::mutate(items_list =
                        paste0(
                          "(",
                          filter_date, 
                          " - Parceiro: ", 
                          ngo, ", Município: ",
                          interno_municipality,
                          ", Itens: ", 
                          items_list,
                          ")"
                        )
        ) %>%
        dplyr::select(items_list)
      
      # Add info if the items are delivered
      registered_item <- registered_item %>%
        dplyr::mutate(
          info = paste(
            "Artículos - ", 
            gsub("_", " ", item_category), 
            ": ", sep ="") %>% 
            
            gsub("livestock",
                 "pecuário", .) %>% 
            gsub("seeds",
                 "sementes", .) %>% 
            gsub("vehicles",
                 "veículos", .) %>% 
            gsub("equipment agri",
                 "equipamento agrícola", .) %>% 
            gsub("equipment",
                 "equipamento", .) %>% 
            gsub("agric inputs",
                 "Insumos para hortas familiares / comunitárias (ferramentas, sementes)  ", .) %>%
            gsub("office", 
                 "escritório", .) %>% 
            gsub("materials", 
                 "materiales", .) %>%
            gsub("clothing", 
                 "roupas", .) %>% 
            gsub("health",
                 "saúde", .) %>% 
            gsub("others", 
                 "outras", .),
        ) %>%
        dplyr::select(
          info, 
          everything()
        )
      
      # Summarize items_list by info (item_category)
      registered_item <- registered_item %>%
        dplyr::group_by(info) %>%
        dplyr::summarise(
          items_list = 
            paste(
              items_list, 
              collapse = "; "
            )
        )
      
      return(registered_item)
      
    }
  }
}




############## Function to extract rendered support ----



extract_support <- function(
    benef_id = NA,
    benef_birthcertif_nr = NA, 
    benef_name = NA,
    df = support
) { 
  
  # Sets inputs as character
  benef_birthcertif_nr <- as.character(benef_birthcertif_nr)
  benef_name  <- as.character(benef_name)
  benef_id  <- as.character(benef_id)
  
  
  if(
    !is.na(benef_id) & 
    benef_id != "Número não disponível ou inválido"
  ){
    
    received_support <- df %>%
      # Filter for beneficiary ID
      dplyr::filter(demo_id_nr == benef_id)
    
    
  }else{
    # Filters based on name
    if(!is.na(benef_name)){     
      
      received_support <- df %>%
        # Filter for beneficiary ID
        dplyr::filter(demo_fullname == benef_name)
      
      
    }else{
      
      # Filters based on Birthcerti. number
      if(!is.na(benef_birthcertif_nr) & 
         benef_birthcertif_nr != "Número não disponível ou inválido"
      ){  
        
        received_support <- df %>%
          # Filter for beneficiary ID
          dplyr::filter(demo_birthcertif_nr == benef_birthcertif_nr)
        
      }else{
        received_support <- data.frame()
        
      }
    }
  }
  
  ## If there is no support !!
  
  if(received_support %>% nrow == 0){
    # For cases where the person did not receive anything
    received_support <- data.frame(
      info = paste(
        "Apoio fornecido"),
      support_list = "ainda não há apoio registrado"
    ) #%>% tibble::as_tibble()
    return(received_support)
    
  }else{
    
    received_support <- received_support %>% 
      # Replaces activity by description after answering "other", if any !
      dplyr::mutate(interno_activity = 
                      ifelse(
                        interno_activity == "Outro(s)",
                        atividade_other,
                        interno_activity
                      )
      ) %>% 
      
      dplyr::select(demo_id_nr, 
                    demo_fullname,
                    filter_date, 
                    interno_implementing_partner, 
                    interno_activity, 
                    activrep_hours,
                    activrep_description,
                    interno_municipality
                    
      ) %>%
      
      dplyr::mutate(interno_activity = trimws(interno_activity),
                    activrep_hours = trimws(activrep_hours)) %>% 
      
      
      
      # Formats data and changes description from singular to plural when applicable
      dplyr::mutate(support_list = 
                      
                      ifelse(
                        
                        as.numeric(activrep_hours) > 1,
                        
                        paste0("(",
                               as.Date(filter_date) %>%
                                 format("%d/%m/%Y"),
                               " - Parceiro: ", interno_implementing_partner, 
                               ", Município: ", interno_municipality,
                               ", Duração: ", activrep_hours," horas", ", ",
                               "Comentários: ", trimws(activrep_description), ")"),
                        
                        
                        paste0("(",
                               as.Date(filter_date) %>%
                                 format("%d/%m/%Y"),
                               " - Parceiro: ", interno_implementing_partner, 
                               ", Município: ", interno_municipality,
                               ", Duração: ", activrep_hours," hora", ", ",
                               "Comentários: ", trimws(activrep_description), ")")
                      )
      ) %>% 
      
      dplyr::group_by(interno_activity) %>% 
      dplyr::summarise( 
        support_list = paste(support_list, collapse="; ")) %>% 
      ungroup %>% 
      dplyr::mutate(info = paste0("Apoio fornecido - ",  tolower(interno_activity), ":")) %>% 
      dplyr::select(info, support_list)  
    
    return(received_support)
    
    
  }
}


# Extract age from birth date ----
#' 
#' Returns age, decimal or not, from single value or vector of strings
#' or dates, compared to a reference date defaulting to now. Note that
#' default is NOT the rounded value of decimal age.
#' @param from_date vector or single value of dates or characters
#' @param to_date date when age is to be computed
#' @param dec return decimal age or not
#' @examples
#' get_age("2000-01-01")
#' get_age(lubridate::as_date("2000-01-01"))
#' get_age("2000-01-01","2015-06-15")
#' get_age("2000-01-01",dec = TRUE)
#' get_age(c("2000-01-01","2003-04-12"))
#' get_age(c("2000-01-01","2003-04-12"),dec = TRUE)

get_age <- function(from_date,to_date = lubridate::now(),dec = FALSE){
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (dec) { age <- lubridate::interval(start = from_date, end = to_date)/(lubridate::days(365)+lubridate::hours(6))
  } else   { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))}
  age
}


# Merge dataframes ----

topmerge <- function(
    df1 = support, # Primary dataframe 
    df2 = regdata, # Secondary dataframe with complementing information
    by = "cpf_nr", # Key variable (must be the same in both dataframes)
    all = TRUE, # Sets if all rows should be kept
    filter_duplicates = "uuid", # Variable to filter duplicated observations after mergning
    filter_na = "fullname", # Variable for performing second merge in case of remaining NAs
    df2_columns = c("fullname", "cpf_nr", "id_origin", "birth_date") # Columns from secondary dataframe that should be added to output
) {
  
  # Load packages
  require(dplyr)
  
  # Merge dataframe
  df <- merge(
    df1, 
    df2, 
    by = by,
    all.x = T
  ) 
  
  # Save vector with df names which are duplicated
  names_without_suffixes <- df %>% 
    dplyr::select(
      dplyr::contains(".x")
    ) %>% 
    names() %>% 
    gsub("\\.x.*", "",.)
  
  # Save vector with df names which are duplicated with suffixes
  names_x_suffixes <- df %>% 
    dplyr::select(
      dplyr::contains(".x")
    ) %>% 
    names() 
  
  
  # Loops through variables with suffixes .x for replacing .x NA values them by .y values 
  for (i in 1:length(names_x_suffixes)) {
    
    # # Replace variables by x suffixed variable
    df[names_without_suffixes[i]] <- df[names_x_suffixes[i]] 
    
    # Replace NAs by y suffixed variable
    df[names_without_suffixes[i]][is.na(df[names_without_suffixes[i]])]  <- df[paste0(names_without_suffixes[i], ".y")][is.na(df[names_without_suffixes[i]])] 
    
  }
  
  # Remove merge byproducts
  df <- df %>%
    dplyr::select(
      -contains(".x"),
      -contains(".y")
    )  %>% 
    # Select pre-existing columns in df1
    dplyr::select(c(tidyselect::all_of(df2_columns), names(df1)))  %>% 
    # Remove duplicated rows
    dplyr::filter(!!sym(filter_duplicates) %in% 
                    (df1 %>% dplyr::select(tidyselect::all_of(filter_duplicates)) %>% unlist) &
                    !duplicated(!!sym(filter_duplicates)))
  
  
  # SECONDARY MERGE ROUND
  # Create intermediary dataframe without NAs
  df_without_nas <- df2 %>%
    dplyr::filter(!is.na(!!sym(filter_na)))
  
  
  # Merge dataframe
  df <- merge(
    df,
    df_without_nas,
    by = filter_na,
    all = T
  )
  
  # Save vector with df names which are duplicated
  names_without_suffixes <- df %>%
    dplyr::select(
      dplyr::contains(".x")
    ) %>%
    names() %>%
    gsub("\\.x.*", "",.)
  
  # Save vector with df names which are duplicated with suffixes
  names_x_suffixes <- df %>%
    dplyr::select(
      dplyr::contains(".x")
    ) %>%
    names()
  
  # Loops through variables with suffixes .x for replacing .x NA values them by .y values
  for (i in 1:length(names_x_suffixes)) {
    
    # # Replace variables by x suffixed variable
    df[names_without_suffixes[i]] <- df[names_x_suffixes[i]]
    
    # Replace NAs by y suffixed variable
    df[names_without_suffixes[i]][is.na(df[names_without_suffixes[i]])]  <- df[paste0(names_without_suffixes[i], ".y")][is.na(df[names_without_suffixes[i]])]
    
  }
  
  # Remove merge byproducts
  df <- df %>%
    dplyr::select(
      -contains(".x"),
      -contains(".y")
    )  %>% 
    # Select pre-existing columns in df1
    dplyr::select(c(tidyselect::all_of(df2_columns), names(df1)))  %>% 
    # Remove duplicated rows
    dplyr::filter(!!sym(filter_duplicates) %in% 
                    (df1 %>% dplyr::select(tidyselect::all_of(filter_duplicates)) %>% unlist) &
                    !duplicated(!!sym(filter_duplicates)))
  
  # Return merged dataframe
  return(df) 
  
}


# Translate individual variables based on XLSForm ----
toptranslate_variable <- function(
    # Variable of interest
  variable = regdata$education, 
  # XLSForm choices imported 
  xlsform = form, 
  # List name as from XLSForm choices
  choices = "escolaridade",
  # Output language
  language = "es",
  # Inform if varibale is select_multiple
  select_multiple = FALSE) {
  
  # Require packages
  require(dplyr)
  
  # Check language
  if(language == "es")
    translation <- xlsform %>%
      dplyr::filter(list_name == choices) %>%
      dplyr::select(name, `label::Español`) %>% 
      rename(label_trans = `label::Español`) %>% 
      na.omit() %>% 
      dplyr::mutate_if(is.factor, as.character)
  
  if(language == "en")
    translation <- xlsform %>%
      dplyr::filter(list_name == choices) %>%
      dplyr::select(name, `label::English`) %>% 
      rename(label_trans = `label::English`) %>% 
      na.omit() %>% 
      dplyr::mutate_if(is.factor, as.character)
  
  if(language == "pt")
    translation <- xlsform %>%
      dplyr::filter(list_name == choices) %>%
      dplyr::select(name, `label::Português`) %>% 
      rename(label_trans = `label::Português`) %>% 
      na.omit() %>% 
      dplyr::mutate_if(is.factor, as.character)
  
  # Translate select_one variables
  if(select_multiple == FALSE){
    variable <- data.frame(name = variable) %>% 
      dplyr::mutate_if(is.factor, as.character)
    
    variable <- variable %>%
      left_join(translation, by = c("name")) 
  }
  
  # Translate select_multiple variables
  if(select_multiple == TRUE){
    
    variable <- stringr::str_split(variable, " ") %>% 
      unlist %>% 
      gsub("Clothers", "Clothes", .) %>% 
      tibble::as_tibble() %>% 
      rename(name = value) %>% 
      na.omit()
    
    variable <- variable %>%
      left_join(translation, by = c("name"))
    
  }
  
  return(variable$label_trans)
  
}

# Plot sex against age ----

topsexage <- function(
    df, # Dataframe
    age_variable, # Age variable
    sex_variable, # Sex variable
    title = "Some plot", # Plot main title
    x_lab = "Age", # Label of x axis
    y_lab = "Number of responses", # Label of y axis
    brewerpal = "Paired",
    x_angle = 20, # Angle of x_lab
    titlebreak = 50, # Characters for title break
    size = 13,
    title_size = 15,
    units = "responses",
    label_size = 4
) {
  
  # Requires package
  library(ggplot2)
  library(stringr)
  
  
  
  table <- df %>% 
    table() %>% 
    as.data.frame() %>% 
    mutate(
      age_number = (
        .[,1] %>% 
          stringr::str_split_fixed(., "-", 2)
      )[,1] %>% gsub("([0-9]+).*$", "\\1", .) %>% 
        as.numeric()
    ) %>% 
    arrange(.[,4], .[,2])
  
  # Ploting respondents by age and sex
  plot.age_sex  <-  ggplot(table, aes(x=reorder(table[,1], table[,4]) , fill = table[,2],
                                      y=table[,3])) +
    geom_bar(position = "dodge", 
             stat = "identity") +
    
    # Prints values
    geom_text(
      aes(label =
            base::paste0(
              scales::percent(
                table[,3]/table %>%
                  dplyr::select(Freq) %>%
                  unlist() %>%
                  sum(), accuracy = 0.1
              )
            )
      ), position=position_dodge(width=0.9), vjust=-0.55) 
  # 
  plot.age_sex + 
    theme_classic() + 
    # Includes plot title 
    ggtitle(stringr::str_wrap(title, width = titlebreak),
            subtitle =  
              paste("N =",
                    nrow(df), units)
    )  +
    
    # Sets options for subtitles
    theme(plot.subtitle = element_text(size = size, color="black")) + 
    theme(legend.title = element_blank(),
          axis.text.x = element_text(colour = "grey20", 
                                     size = size, face="plain", angle = x_angle),
          axis.text.y = element_text(colour = "grey20", size = size, face = "plain"), 
          legend.position = c(0.15, 0.87), 
          plot.title = element_text(size = title_size, face = "bold", 
                                    hjust = 0, vjust = 1.2)) + 
    labs(x = x_lab, y = y_lab, title = title) + 
    
    
    
    
    scale_fill_brewer(palette = brewerpal) + 
    theme(axis.title.x = element_text(size = size, angle = 00)) +    
    theme(axis.title.y = element_text(size = size, angle = 90)) + 
    theme(legend.text = element_text(size = size))
}


### Retrieves uuid from attachemets to be used to render photo in consulta ----
toparse_list <- function(variable) {
  
  if (is.na(variable) | 
      variable == "list()"){ 
    
    variable = NA 
    
  }else{
    
    # Loads required package
    library(dplyr)
    
    data <- data.frame() %>% 
      # Mutates factor to character to avoid merging problems
      dplyr::mutate_if(is.factor, as.character)
    
    # Creates preliminary dataset with contents of variable 
    rowdata <- eval(parse(text=paste('data.frame(', variable, ')'))) %>% 
      
      dplyr::mutate_if(is.factor, as.character)
    
    # Binds datasets in rows
    variable <- dplyr::bind_rows(data, rowdata)
    
    #}
    ## converts %2F to / as it should be in URL
    variable <- variable %>% 
      dplyr::mutate(uuid_photo = 
                      gsub("%2F", "/", download_url))  
    ## remove image id by removing all letters after last / 
    variable <- variable %>% 
      dplyr::mutate(uuid_photo = 
                      gsub("/[^/]*$", "", uuid_photo))
    ## removes all letters before / to get only uuid
    variable <- variable %>% 
      dplyr::mutate(uuid_photo = 
                      gsub(".*\\/", "", uuid_photo)) %>%
      distinct(uuid_photo) %>% 
      unlist
    
    
  }
  
}

# Function to round values while preserving the total sum ----
smart.round <- function(x) {
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y
}

# Remove outliers from numeric variables ----
remove.outliers <- function(num_variable) {
  
  
  # Prepare variable
  variable <- num_variable %>% 
    unlist() %>% 
    as.double() %>% 
    na.omit()
  
  # Identify outliers
  outliers <- boxplot(variable, plot=FALSE)$out
  
  # Remove outliers 
  variable[-which(variable %in% outliers)] 
}

# Disentangling many strings in single levels ----
# Function to take multiple columns that should be evaluated as one aspect
multicols_in_one <- function(df, 
                             cut_str = "noinfo", 
                             varlist = "noinfo", 
                             df_type = "noinfo", 
                             remove_outliers = FALSE){
  dfmod <- c()
  cut_str <- rlang::quo_name(rlang::enquo(cut_str))
  if(cut_str == "noinfo"){
    warning("use cut_str arg. with the common strings prefix to be removed")}
  if(df_type == "noinfo"){
    warning("use 'df_type' arg. with 'numeric' or 'factor'")}
  
  
  for(i in 1:length(varlist)){
    col1 <-  varlist[i]
    
    if(remove_outliers == FALSE){
      list_df  <- df %>% 
        select(matches(col1)) %>%
        na.omit %>%  
        gather(key = "vars", value = "answer")
      
    }else{
      
      if(remove_outliers == TRUE){
        
        list_df  <- df %>% select(matches(col1)) %>%
          na.omit %>% 
          remove.outliers() %>% 
          as_tibble() %>% 
          gather(key = "vars", value = "answer") %>% 
          mutate(vars = paste(col1))
      }
      
    }
    
    dfmod <- bind_rows(dfmod, list_df) 
  }
  if(df_type == "factor"){
    dfmod <- table(dfmod) %>% tibble::as_tibble() %>%
      mutate(vars = gsub("_", " ", vars))
  }
  else{
    dfmod <- dfmod %>% 
      mutate(vars = gsub(cut_str, "", vars),
             answer = as.numeric(as.character(answer)),
             vars = gsub("_", " ", vars))
    
  }
  
  
  return(dfmod)
}

# Calculates bayesian mean of variable

topbayes_mean <- function(
    var = needsdata$demo_community_size,
    seed = 1234,
    remove_outliers = FALSE,
    round_digit = 0
) {
  
  set.seed(seed)
  
  if(is.logical(remove_outliers)){
    if(remove_outliers == TRUE){
      var <- remove.outliers(var)
    }
  }
  
  
  
  if(length(var) > 1){
    
    bayesboot::bayesboot(var %>%
                           as.character() %>%
                           as.numeric() %>%
                           na.omit(), mean) %>%  summary() %>% 
      as.list() %>% 
      as.data.frame() %>% 
      select(-statistic) %>%
      tidyr::spread(measure, value) %>% 
      pull(mean) %>% 
      round(., round_digit)
    
  }else{
    as.numeric(var)
  }
  
  
}

## Sets auxiliary plotting functions ----

# Bootstrap plot with disaggregation -----
auto_topbootplot_facet <- function(
    data = f1_obs,
    key_variable = "market_price_chlorinegranules",
    facet_variable = "interno_municipality",
    seed = 1234,
    bins = 60,
    fill = c("steelblue", "#00e600", "#cccc00", "darkgreen", "darkorange1", "darkorchid1", "darksalmon"),
    col = "white",
    title = "Example tile",
    titlebreak_topbootplot = 80,
    title_size = 11,
    subtitle =
      "As linhas pontilhadas representam o intervalo de confiança de 95% (intervalo de alta densidade).",
    label_size = 10,
    remove_outliers_topbootplot = FALSE,
    facet_plot = TRUE
){
  
  # The graphs represent the average estimates using a bayesian bootstrap (re-sampling) method with 4,000 
  # replicates. This allows us to estimate the average and 95% confidence intervals for each one of these
  # indicators. Bootstrapping is an useful alternative to the traditional method of hypothesis testing as 
  # it mitigates some of the pitfalls encountered within the traditional approach, mainly in terms of 
  # assumption violations. This ensures that the results are data driven and do not need to assume 
  # theoretical distributions.
  
  # https://towardsdatascience.com/bootstrapping-statistics-what-it-is-and-why-its-used-e2fa29577307
  
  library(bayesboot) # Requiring package
  library(gridExtra) # For plot grids
  library(cowplot) # For plot grids
  library(tidyr) # For function spread
  
  # Remove outliers from numeric variables
  remove.outliers <- function(num_variable) {
    
    # Prepare variable
    variable <- num_variable %>%
      unlist() %>%
      as.double() %>%
      na.omit()
    
    # Identify outliers
    outliers <- boxplot(variable, plot=FALSE)$out
    
    # Remove outliers
    #variable <-  variable[-which(variable %in% outliers)]
    
    variable <-  variable[!(variable %in% outliers)]
  }
  
  
  # Checks if variable has available data
  if(
    data %>%
    select(any_of(key_variable)) %>%
    unlist(., use.names = FALSE) %>%
    na.omit %>%
    length() > 0
  ){
    
    # # Subsets dataset
    
    ## Setting random generation seed to allow for reproducibility
    set.seed(seed)
    
    # List the names of facet_levels for plot disaggregartion
    
    
    if(is.factor(data %>% pull(facet_variable))){
      facet_levels <- levels(data %>% pull(facet_variable)) %>% gsub("_", " ", .)
    }else{
      facet_levels <- data %>% pull(facet_variable) %>% unique() %>% as.character() %>% sort() %>% gsub("_", " ", .)
    }
    
    facet_levels <- facet_levels %>% 
      unique() 
    
    
    facet_levels <-
      c(
        "Overall",
        facet_levels
      )
    
    # Adds line break for number of observations
    subtitle = paste(subtitle, "\n")
    
    
    df_summary <- data %>%
      select(any_of(c(facet_variable, key_variable)))  %>%
      rename_(x = facet_variable,
              y = key_variable) %>%
      ## Omits NAs
      na.omit() %>%
      mutate(y= gsub("_"," ",y),
             x= gsub("_"," ",x) 
      )
    
    
    # Uses for loop to plot for each country
    
    for (i in 1:length(facet_levels)){
      
      
      title_bootplot = facet_levels[i]
      subtitle = ""
      title_size_bootplot = title_size * 0.8
      label_size = 8
      
      # Filter the data per country and adjust the title and subtitle text and size
      if(facet_levels[i] != "Overall"){
        
        # title = facet_levels[i]
        # subtitle = ""
        # title_size_bootplot = 10
        # label_size = 8
        
        
        df_bootplot <- df_summary %>%
          filter(x %in% facet_levels[i]) %>%
          select(y)
        
      }else{
        
        
        df_bootplot <- df_summary %>%
          select(y)
      }
      
      #Starts plotting
      
      if(df_bootplot %>% nrow > 1 &
         #df_bootplot %>% unique %>% nrow > 1 & # FALSE when the values are univariate 
         sum(as.numeric(df_bootplot$y)) != 0){
        
        if(remove_outliers_topbootplot == TRUE){
          ## Setting random generation seed to allow for reproducibility
          set.seed(seed)
          
          bp0 <- bayesboot::bayesboot(
            df_bootplot %>%
              pull(y) %>%
              as.character() %>%
              as.numeric() %>%
              na.omit() %>%
              remove.outliers(),
            mean
          )
          
          # Adjusts title
          # if(facet_levels[i] == "Overall"){
          #   subtitle = paste0("Os outliers são automaticamente removidos")
          # }
          
        }else{
          ## Setting random generation seed to allow for reproducibility
          set.seed(seed)
          # Conducting Bayesian bootstrap (requires package "bayesboot")
          bp0 <- bayesboot::bayesboot(
            df_bootplot %>%
              pull(y) %>%
              as.character() %>%
              as.numeric() %>%
              na.omit(),
            mean
          )
          
        }
        
        # Create summary of bootstrap (median, mean, sd, hdi.low, hdi.high, etc)
        bp0_summary <- bp0 %>%
          summary() %>%
          as.list() %>%
          as.data.frame() %>%
          select(-statistic) %>%
          tidyr::spread(measure, value)
        
        # Draws main plot
        p_topboot <- ggplot(data=bp0, aes(x= V1)) +
          geom_histogram(bins = bins, fill = fill[i], col = col)
        
        # Extract maximum counted value (ymax) of bin ----
        max_count <- ggplot_build(p_topboot)$data[[1]]$ymax %>% max()
        
        p_topboot <- p_topboot +
          # Sets title and subtitle (incl. str_wrap for text wrapping)
          ggtitle(
            stringr::str_wrap(title_bootplot, width = titlebreak_topbootplot),
            subtitle =
              paste0("n = ", df_bootplot %>% nrow," ", subtitle)
          ) +
          theme_grey() +
          theme(
            plot.subtitle = element_text(size = title_size_bootplot * 0.8, color="black"),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(size = title_size_bootplot, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text = element_text(size = label_size)
          ) +
          # Adjust scales to increase plot space
          scale_y_continuous(
            limits = c(0, max_count * 1.2)
          ) +
          geom_vline(
            xintercept = bp0_summary$mean,
            show.legend = TRUE,
            col = "black",
            linetype = "dotted"
          ) +
          # Annotation with the mean
          ggplot2::annotate(
            geom = "text",
            x = bp0_summary$mean,
            y = max_count * 1.12,
            label = round(bp0_summary$mean, 2),
            size = label_size * 0.4,
            fontface = "bold"
          ) +
          # Lower confidence interval
          geom_vline(
            xintercept = bp0_summary$hdi.low,
            show.legend = TRUE,
            linetype = "dotted"
          ) +
          ggplot2::annotate(
            geom = "text",
            x = bp0_summary$hdi.low,
            y = max_count * 0.6,
            label = round(bp0_summary$hdi.low, 2),
            size = label_size * 0.3
          ) +
          # Upper confidence interval
          geom_vline(
            xintercept = bp0_summary$hdi.high,
            show.legend = TRUE,
            linetype = "dotted"
          )  +
          ggplot2::annotate(
            geom = "text",
            x = bp0_summary$hdi.high,
            y = max_count * 0.6,
            label = round(bp0_summary$hdi.high, 2),
            size = label_size * 0.3
          )
        
      }else{
        
        # if(df_bootplot %>% nrow < 2){
        #   label_mes <- 
        #     "O comprimento dos dados é inferior a 2"
        #   
        # }else{
        #   if(df_bootplot %>% unique %>% nrow == 1 &
        #      sum(as.numeric(df_bootplot$y)) != 0){ # TRUE when the values are univariate 
        #     label_mes <- paste0(
        #       "Todos os valores registados são os mesmos = ",
        #       df_bootplot %>% pull(y) %>% unique()
        #     )
        #   }else{
        #     if(sum(as.numeric(df_bootplot$y)) == 0){ # TRUE when all the values are zeros
        #       label_mes <- 
        #         "A soma dos valores é 0"
        #       
        #     }else{
        #       label_mes <- 
        #         "A soma dos valores é 0"
        #       
        #     }
        #   }
        # }
        
        label_mes <-
          if(df_bootplot %>% nrow < 2){
            paste0("The data length is less than 2")
          }else{
            paste0("The sum of the values is 0")
          }
        
        ## Renders empty ggplot if data is not avaiable
        p_topboot <- ggplot(df <- data.frame(), aes(x = "", y = "")) + geom_blank() +
          
          theme_minimal() +
          coord_flip() +
          theme(legend.position="none")  +
          # Removes title for y axis
          theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
          
          ggtitle(
            stringr::str_wrap(title_bootplot,
                              width = titlebreak_topbootplot),
            subtitle =
              paste0("n = ", df_bootplot %>% nrow)
          ) +
          # Prints values
          geom_text(
            aes(label = label_mes,
                colour= "red"
            ),
            hjust = 0.5, vjust = 0.0, size = title_size_bootplot * 0.5) +
          
          theme(
            plot.subtitle = element_text(size = title_size_bootplot * 0.8, color="black"),
            plot.title = element_text(size = title_size_bootplot, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text = element_text(size = label_size)
          )
        
        
        
      }
      
      
      ## Assign the index to name of each plot
      
      assign(paste("p_topboot",
                   i,
                   sep = "_") ,
             p_topboot)
      
      rm(p_topboot)
      
    }
    
    
    ## Adds plot title to ggplot without any margin space from left
    
    library(gridExtra)
    library(grid)
    
    if(remove_outliers_topbootplot == TRUE){
      title = paste0(title, " (Outliers are automatically removed)")
    }
    
    title.grob <- textGrob(
      label = stringr::str_wrap(title,
                                width = titlebreak_topbootplot),
      x = unit(0, "lines"),
      y = unit(0, "lines"),
      hjust = -0.00, vjust = -0.5,
      gp = gpar(fontsize = title_size , fontface = "bold")
    )  
    
    
    if(facet_plot == TRUE){
      # Binds the disaggregated plots
      # Checks if any 2 level facet plot is created 
      if("p_topboot_2" %in% ls()){
        
        # Extract names of bootplots from global environment
        p_topboot <- ls()[grepl("p_topboot_", ls())]
        
        # Combine all the plots in 1 grob
        pl <- lapply(1:length(p_topboot), function(.x) 
          get(paste0("p_topboot_", .x))
        )
        
        # Adjusts widths of odd number of facet plots 
        factors_n <- length(p_topboot)
        
        # If facet levels are in even number or 1
        if((factors_n %% 2) == 0){
          
          p_fct <- gridExtra::arrangeGrob(
            grobs=pl,
            nrow= ceiling(factors_n/2), 
            ncol=2
          )
          
          
        }else{
          
          # If facet levels are in odd number then arrange Overall plot at the top
          # then build the bottom row
          
          bottom_row  <- gridExtra::arrangeGrob(
            grobs=pl[2:factors_n],
            nrow= floor(factors_n/2), 
            ncol=2
          )
          
          nrow_odd_facets <- if(factors_n > 3){ceiling(factors_n/2)}else{floor(factors_n/2)}
          
          p_fct <- cowplot::plot_grid(
            pl[[1]] ,
            bottom_row,
            nrow = 2 ,
            rel_heights = if(factors_n > 3){
              c(1/nrow_odd_facets,((nrow_odd_facets-1)/nrow_odd_facets))
            }else{
              c(0.5, 0.5)
            }
          )
          
        }
        
        # Binds the title
        
        ddpcr::quiet(
          gridExtra::grid.arrange(
            p_fct,
            top= title.grob,
            padding = unit(1.5, "line")
          )
        )  
        
        # Deletes the intermediary plots
        rm(list=ls()[ls() %in% c(p_topboot, "p_fct")])
        
      }else{
        
        ddpcr::quiet(
          gridExtra::grid.arrange(
            p_topboot_1,
            top= title.grob,
            padding = unit(1.5, "line")
          )
        )  
        
      }
      
      
    }else{
      
      if(facet_plot == FALSE){
        
        # Binds the title
        
        ddpcr::quiet(
          gridExtra::grid.arrange(
            p_topboot_1,
            top= title.grob,
            padding = unit(1.5, "line")
          )
        )  
        
      }
    }
    
    
  }else{
    
    # label_mes <- paste0("O comprimento dos dados é inferior a 2")
    #   
    # 
    # ## Renders empty ggplot if data is not avaiable
    # p_topboot <- ggplot(df <- data.frame(), aes(x = "", y = "")) + geom_blank() +
    #   
    #   theme_minimal() +
    #   coord_flip() +
    #   theme(legend.position="none")  +
    #   # Removes title for y axis
    #   theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
    #   
    #   ggtitle(
    #     stringr::str_wrap("Overall",
    #                       width = titlebreak_topbootplot),
    #     subtitle =
    #       paste0("n = ", data %>%
    #                select(any_of(key_variable)) %>%
    #                unlist(., use.names = FALSE) %>%
    #                na.omit %>%
    #                length())
    #   ) +
    #   # Prints values
    #   geom_text(
    #     aes(label = label_mes,
    #         colour= "red"
    #     ),
    #     hjust = 0.5, vjust = 0.0, size = title_size_bootplot * 0.5) +
    #   
    #   theme(
    #     plot.subtitle = element_text(size = title_size_bootplot * 0.8, color="black"),
    #     plot.title = element_text(size = title_size_bootplot, face = "bold"),
    #     axis.title.x = element_blank(),
    #     axis.title.y = element_blank(),
    #     axis.text.y = element_blank(),
    #     axis.text = element_text(size = label_size)
    #   )
  }
  
}



empty_ggplot <- function(
    title = "There is no data",
    label_mes = "No data available for",
    titlebreak = 100,
    title_size= 15,
    label_size = 5
){
  
  ggplot(df <- data.frame(), aes(x = "", y = "")) + geom_blank() +
    
    theme_minimal() +
    coord_flip() +
    theme(legend.position="none")  +
    # Removes title for y axis
    theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
    
    ggtitle(
      stringr::str_wrap(title,
                        width = titlebreak)
    ) +
    
    # Prints values
    geom_text(
      aes(label = label_mes,
          colour= "red"
      ),
      hjust = -0.1, vjust = 0.2, size = label_size) +
    
    theme(
      plot.title = element_text(size = title_size, face = "bold"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    )
}

# Gauge plot ----

topgauge <- function(
    ind_value = 80,
    breaks=c(
      0, # Danger (min value)
      30, # Warning
      70, # Sucess
      100 # success margin (max value)
    ),
    label = "Percentage",
    symbol = "",
    image_name = "ind_1.png",
    size_min = 12,
    size_max = 12,
    size_ind_value = 15,
    size_label = 12
){
  
  require(ggplot2)
  library(grid)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=10000)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  
  # The bar color based on target value
  bar_color <- ifelse(ind_value < breaks[2], "red",
                      ifelse(ind_value>=breaks[2] & ind_value< breaks[3], "#e6ac00","#66cc00") # yellow, green
  )
  
  # If the value is greater than the max value of gauge plot
  if(ind_value > breaks[4]){
    
    bar_value <- breaks[4]
  }else{
    
    bar_value <- ind_value
  }
  
  # Minumum and maximum values
  
  min_value <- breaks[1]
  
  max_value <- breaks[4]
  
  
  # Normalize the breaks
  if(breaks[1] != 0 | breaks[4] != 100){
    
    
    bar_value  <- (bar_value - min(breaks)) / (max(breaks) - min(breaks)) *100
    
    normalize <- function(x){
      
      x <- 
        (x - min(x)) / (max(x) - min(x)) *100
      
      
      return(x)
    }
    
    breaks  <- normalize(breaks)
    
    
  }
  
  
  ggplot() + 
    geom_polygon(data=get.poly(breaks[1],breaks[4]),aes(x,y),fill="grey", alpha = 0.5) +
    geom_polygon(data=get.poly(breaks[1], bar_value),aes(x,y),fill= bar_color) +
    geom_path(data=get.poly(breaks[1],breaks[4]),aes(x,y),colour='darkgrey', size = 1.5, alpha = 0.8) + 
    geom_line(data=data.frame(x= c(-1,-0.5), y=c(0,0)),aes(x,y),colour='darkgrey', size = 1.5, alpha = 0.8) + 
    #geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="green") +
    # geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y)) +
    # geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
    #           aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%"))) +
    geom_text(aes(x = -0.75, y = -0.065, 
                  label = paste0(format(min_value, big.mark = ","))), size= size_min, fontface = "bold") +
    geom_text(aes(x = 0.75, y = -0.065, 
                  label = paste0(format(max_value, big.mark = ","))), size= size_max, fontface = "bold") +
    
    geom_text(aes(x = 0, y = 0.17, 
                  label = paste0(format(ind_value, big.mark = ","), symbol)), size= size_ind_value, fontface = "bold") +
    geom_text(aes(x = 0, y = -0.065, 
                  label = label), size= size_label, fontface = "bold") +
    coord_fixed() +
    theme_bw() +
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) +
    theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
  
  # ggsave("p.png", width = 12, height = 7, units = "cm")
  
  #ggsave(paste0("gaugeplots/", image_name), width = 12, height = 7, units = "cm")
  
}

# Function to round values while preserving the total sum ----
smart.round <- function(x) {
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y
}

# Topbarplot with facets  -----

# The stacked bar plot does not show percentage labels if the percentage is less than 3% to avoid overlapping of the such labels percentages when they are adjusants to eachother.   


auto_topbarplot_facet <-function(
    data = df_hh, 
    key_variable = "demo_ethnic_group", 
    facet_variable = "demo_interviewer_participant", 
    title = NA, 
    form_path = "forms/Household interviews (source file in XLSForm) - UNICEF AZE.xlsx", 
    language = "English",
    facet_title = "Disaggregated by municipality",
    levels = FALSE,
    facet_plot = TRUE,
    heights=c(0.55, 0.45),
    title_size = 11, # Text size of title
    ylab = "Responses", 
    units = "responses",
    labels_count = Inf,
    brewerpal = "Set3", 
    dist = 1.4, # Creates distance between bar value and scale limit 
    textsize = 3,  # text size of bar value
    axis_text_size = 9, # text size of axis label (answer options and x axis scale)
    angle = 0, 
    hjust = -0.07, 
    titlebreak_topbarplot = 90, 
    vjust = 0.29, 
    scales = "free_x", 
    nrow = NULL, 
    ncol = NULL, 
    direction = 1,
    xlab_break = 25,
    big.mark = ",",
    decimal.mark = ".",
    fix_ylab_names = NA,
    fix_ylab_color = NA,
    stacked = FALSE
    
) {
  
  if(is.factor(data %>% pull(facet_variable))){
    facet_levels <- levels(data %>% pull(facet_variable)) %>% gsub("_", " ", .)
  }else{
    facet_levels <- data %>% pull(facet_variable) %>% unique() %>% as.character() %>% sort() %>% gsub("_", " ", .)
  }
  
  
  # Loads required packages
  library(ggplot2) ## For plotting
  library(scales) ## For % annotation
  library(dplyr) ## For data wrangling
  library(lazyeval) ## For summarising with the interp function
  library(stringr) ## For text wrapping (e.g. titles)
  library(splitstackshape) ## For splitting select multiple variables
  
  
  ## Loads options labels using form survey
  library(tidyr)
  
  
  
  # remove the list of answer labels if exists in global environment
  if("choices_list" %in% ls()){
    rm(choices_list)
  } 
  
  ## If we have form path then we can extract anser options and title of the variable from the XLSFOrm
  if(!is.na(form_path)){
    
    
    # convert to lowercase 
    language <- tolower(language)
    
    #### Function to replace NAs 
    replace_na_previous <- function(x, a =! is.na(x)) {
      
      x[which(a)[c(1, 1:sum(a))][cumsum(a) + 1]]
      
    }
    
    # Imports form data (sheet choices) ----
    koboform_choices <- 
      suppressWarnings(
        readxl::read_xlsx(
          path = form_path,
          sheet = "choices")
      ) %>% 
      
      select(list_name,name, contains("label")) %>%
      rename_with(., 
                  ~ tolower(
                    # removes parathesis and text within
                    gsub("\\s*\\([^\\)]+\\)","", .) %>% 
                      # Removes all the text before the matching character
                      gsub(".*:", "", .) %>% 
                      stringr::str_trim()
                  )
      ) %>% 
      #rename(label = label) %>% 
      filter(!is.na(list_name)) %>% 
      dplyr::select(list_name, name, all_of(language)) 
    
    # Renames language variable
    names(koboform_choices) <- c("list_name", "name", "label")
    
    # Imports form data (sheet survey) and pick option id used with the variable
    koboform_survey <- 
      suppressWarnings(
        readxl::read_xlsx(
          path = form_path, 
          sheet = "survey"
        )
      ) %>% 
      select(type,name, contains("label")) %>%  
      #janitor::clean_names() %>% 
      rename_with(., 
                  ~ tolower(
                    # removes parathesis and text within
                    gsub("\\s*\\([^\\)]+\\)","", .) %>% 
                      # Removes all the text before the matching character
                      gsub(".*:", "", .) %>% 
                      stringr::str_trim()
                  )
      )  %>% 
      filter(!is.na(name)) %>% 
      
      # Creates variable with group name
      mutate(
        group = ifelse(type == "begin group", name, NA)
      ) %>% 
      # Replace empty cells with group names
      mutate(group = replace_na_previous(group)) %>% 
      filter(grepl("select_", type)) %>% 
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
      
      
      distinct(final_name, .keep_all = T) %>% 
      # Create column with final variable names
      mutate(
        name = stringr::str_trim(name)
      ) %>% 
      mutate(
        options_id = (
          type %>% 
            stringr::str_split_fixed(., " ", 2)
        )[,2]) %>% 
      dplyr::select(type, name, options_id, final_name, all_of(language)) 
    
    # Renames language variable
    names(koboform_survey) <- c("type", "name", "options_id", "final_name", "label")
    
    
    
    
    # IF the variable exists in the XLSForm
    
    if(key_variable %in% koboform_survey$final_name){
      
      
      kobosurvey_id <- koboform_survey %>% 
        filter(final_name %in% key_variable) %>% 
        pull(options_id) 
      
      # Uses options id to extract the labels
      
      choices_list <- 
        koboform_choices %>%
        filter(list_name %in% gsub(" or_other", "", kobosurvey_id)) %>% 
        select(label) %>% 
        na.omit() %>% 
        unique() 
      
      # if variable has or_other option in XLSForm then we need to add option "Other" in the answer list
      
      if(grepl("or_other", kobosurvey_id)){
        choices_list <- bind_rows(choices_list,
                                  data.frame(label = "Other"))
      }
      
      # Apply xlabel_breaks to extracted option labels from form choices sheet
      choices_list <- 
        
        sapply(
          base::strwrap(
            as.character(
              base::gsub("_", " ", choices_list$label %>% na.omit() %>% unique())
              
            ),
            width = xlab_break,
            simplify=FALSE),
          paste, collapse="\n")
      
    }else{
      
      choices_list <- 
        
        sapply(
          base::strwrap(
            as.character(
              base::gsub("_", " ", data %>%
                           select(any_of(c(key_variable)))  %>%
                           ## Omits NAs
                           na.omit() %>% 
                           ## Split variables from select_multiple questions
                           splitstackshape::cSplit(
                             ., splitCols = key_variable,
                             sep = " ", 
                             direction = "long") %>%
                           unique() %>% 
                           pull())
              
            ),
            width = xlab_break,
            simplify=FALSE),
          paste, collapse="\n")
      
    }
    
    # Replaces labels for variables having "_other" by following variable
    if(is.na(title)){
      if(grepl("_other", key_variable)){
        title <- 
          paste0("Other? - ",
                 koboform_survey$label[koboform_survey$final_name == gsub("_other", "", key_variable)]
          ) 
        
      }else{
        
        title <- 
          koboform_survey$label[koboform_survey$final_name == key_variable]
      }
      
    }
    
  }
  
  
  
  # Subsets dataset ----
  df_summary <- data %>%
    select(any_of(c(facet_variable, key_variable)))  %>%
    ## Omits NAs
    na.omit() %>% 
    ## Split variables from select_multiple questions
    splitstackshape::cSplit(
      ., splitCols = key_variable,
      sep = " ", 
      direction = "long") %>%
    
    
    rename_(x = facet_variable, y = key_variable) %>%
    ## Groups observations
    group_by(x, y) %>%
    ## Count observations
    dplyr::summarise(n=n()) %>%
    ## Remove underscore
    mutate(
      y = gsub("_", " ", y),
      x = gsub("_", " ", x)
      
    ) %>%
    ## Arranges results in descending order
    arrange(desc(n)) %>% 
    
    ## Ungroups variables
    ungroup() 
  
  ## Calculates total number of responses
  
  total_n <- sum(df_summary$n)
  
  
  # Adds factor level "Overall"
  df_summary <- df_summary %>% 
    
    bind_rows(
      .,
      df_summary %>% group_by(y) %>% dplyr::summarise(
        x = "Overall",
        n = sum(n)
      ) %>% 
        select(x, y, n) 
    ) %>% 
    
    # Arranges facet to factor levels
    
    mutate(x = 
             factor(x, 
                    levels = c("Overall",  facet_levels
                    )
             )
    ) %>% 
    
    ungroup() %>% 
    
    ## Group by variable Y
    group_by(x) %>%
    ## Count observations
    dplyr::summarise(y=y,
                     n=n,
                     percent = round(n/sum(n) *100, 1)
    ) %>%
    
    ungroup()
  
  
  
  # Calculates sum of the observations for each facet factor and paste with facet title
  df_summary <- df_summary %>% 
    group_by(x) %>%
    dplyr::summarise(x = 
                       as.factor(paste0(x, " (", sum(n), ")")
                                 
                       ),
                     y= y,
                     n = n,
                     percent = percent
    ) %>% 
    ungroup() %>% 
    arrange(x, desc(n))
  
  # checks if facet facor is missing in data
  
  missing_facet <- 
    facet_levels[!(facet_levels %in% gsub("\\s*\\([^\\)]+\\)","", levels(df_summary$x)))] %>% 
    paste0(., " (0)")
  
  
  if(missing_facet != " (0)"){
    df_summary <- df_summary %>% 
      mutate(x = 
               factor(x, 
                      levels = c(
                        levels(df_summary$x)[1],
                        c(levels(df_summary$x)[2:length(df_summary$x %>% unique())],
                          missing_facet) %>% sort()
                      )
               )
      ) 
  }
  
  
  # Applies label characters breakdown
  df_summary <- df_summary %>% 
    
    mutate(y= sapply(
      base::strwrap(
        as.character(
          base::gsub("_", " ", df_summary$y %>% na.omit()) 
          
        ), 
        width = xlab_break, 
        simplify=FALSE), 
      paste, collapse="\n") 
    ) 
  
  
  # Adjusts widths of odd number of facet plots 
  factors_n <- levels(df_summary$x) %>% length()
  
  
  ## Calculates total number of labels count in data
  labels_n <- unique(df_summary$y) %>% length()
  
  
  # Checks if this is a select_multiple question  
  select_multiple <- data %>%
    select(any_of(key_variable)) %>% 
    rename_(y = key_variable) %>%
    mutate(y = trimws(y)) %>% 
    unlist(., use.names = FALSE) %>% 
    grepl(" ", .) 
  
  
  if (TRUE %in% select_multiple) {
    select_multiple <-  TRUE
    
    ## Calculates total number of interviews
    total_n <- nrow(
      data %>%
        select(any_of(key_variable))  %>%
        na.omit()
    )
    
    # Change the unit and x axis title
    
    # Change the unit and x axis title
    
    ylab = "Interviews" 
    units = "interviews"
    
    
    
    if(
      labels_n > 10 & 
      is.infinite(labels_count)
    ){
      
      labels_count <- 10
      
      # Adjust titles to inform that it refers to a multiple selection question
      title <- paste0(
        title, 
        " (Multiple selection - Top 10/",
        labels_n,
        ")"
      )
      
    }else{
      
      
      if(
        (
          labels_n <= 10 & 
          is.infinite(labels_count)
          
        )
      ){
        
        title <- paste0(
          title, 
          " (Multiple selection)"
          
        )
        
        
      }else{
        
        if(
          !is.infinite(labels_count)
        ){
          
          if(labels_count >= labels_n){
            title <- paste0(
              title, 
              " (Multiple selection)"
              
            )
          }else{
            
            # Adjust titles to inform that it refers to a multiple selection question
            title <- paste0(
              title, 
              " (Multiple selection - Top ",
              labels_count,
              "/",
              labels_n,
              ")"
            ) 
          }
          
          
        }
      }
      
    }
    
  }
  
  # Adjust title if labels_count is applied to select_one type variable
  
  if (!(TRUE %in% select_multiple)) {
    
    ## Calculates total number of interviews
    total_n <- nrow(
      data %>%
        select(any_of(key_variable))  %>%
        na.omit()
    )
    
    if(
      labels_n > 10 & 
      is.infinite(labels_count)
    ){
      
      labels_count <- 10
      
      # Adjust titles to inform that it refers to a multiple selection question
      title <- paste0(
        title, 
        " (Top 10/",
        labels_n,
        ")"
      )
      
    }else{
      
      
      if(
        (
          labels_n <= 10 & 
          is.infinite(labels_count)
          
        )
      ){
        
        title <- title
        
        
      }else{
        
        if(
          !is.infinite(labels_count)
        ){
          
          if(labels_count >= labels_n){
            title <- title
          }else{
            
            # Adjust titles to inform that it refers to a multiple selection question
            title <- paste0(
              title, 
              " (Top ",
              labels_count,
              "/",
              labels_n,
              ")"
            )
          }
          
        }
        
        
      }
    }
    
  }
  
  # Extracts the label names that will be used in facetplot to extract matching labels 
  
  label_names <- df_summary %>% 
    distinct(y, .keep_all = T) %>% 
    pull(y) %>% 
    head(labels_count) %>%
    unlist
  
  # Extract only the filtered labels
  df_summary <- df_summary %>% 
    filter(y %in% label_names) %>% 
    # Arranges facet to factor levels
    
    mutate(y = 
             factor(y, 
                    levels = label_names
             )
    ) 
  
  if(exists("choices_list") & !is.logical(levels)){
    ## If any of the variable option does not exist in form choices label than takes variable choices
    if(FALSE %in% (choices_list %in% label_names)){
      choices_list <- c(choices_list[choices_list %in% label_names], label_names[!(label_names %in% choices_list)])
      
    }else{
      
      # If levels are provided in input
      choices_list <- levels
      
    }
    
  }
  
  
  # If levels are given then arrange the plot in factor level otherwise arrange the labels as arranged in form_choices sheet
  if(TRUE %in% (levels == FALSE | is.na(levels[1]))){
    
    df_summary <- df_summary
    
    choices_list <- label_names
    
  }else{
    # If levels are true then picks the choices from the XLSForm
    if(TRUE %in% (levels == TRUE |
                  key_variable %in% levels)
    ){
      
      # Remove underscores from provided levels
      # Arranges y axis according to factor levels
      df_summary <- df_summary %>%
        mutate (y = factor(y, levels = rev(choices_list)))
      
      if(stacked == TRUE){
        df_summary <- df_summary %>%
          mutate(y = factor(y, levels = choices_list))
      }
      
      
    }else{
      # If levels are provided
      if(TRUE %in% (length(levels) > 1 & !is.na(levels))){
        
        levels <- 
          
          sapply(
            base::strwrap(
              as.character(
                base::gsub("_", " ", levels)
                
              ),
              width = xlab_break,
              simplify=FALSE),
            paste, collapse="\n")
        
        # Arranges y axis according to factor levels
        df_summary <- df_summary %>%
          mutate (y = factor(y, levels = levels))
        
      } 
    }
  }
  
  
  
  # Runs condition on number of levels for setting colors when levels exceeds palette
  if(choices_list %>%
     length() >
     RColorBrewer::brewer.pal.info[brewerpal,]["maxcolors"]){
    
    # Creates expanded Brewer palette
    colourCount = choices_list %>%
      length()
    getPalette = colorRampPalette(
      RColorBrewer::brewer.pal(
        as.numeric(
          RColorBrewer::brewer.pal.info[brewerpal,]["maxcolors"]),
        brewerpal
      )
    )
    
    # Create custom
    scale_fill_custom <- function(...){
      ggplot2:::manual_scale(
        'fill',
        values = setNames(
          # Colours
          if(direction == -1){
            rev(getPalette(colourCount))
          }else{
            getPalette(colourCount)
          },
          choices_list
        )
      )
    }
    
  }else{
    
    # Creates count of colours
    colourCount = choices_list %>%
      length()
    
    # Create palette for cases when levels do not exceed palettes
    
    # Create custom
    scale_fill_custom <- function(...){
      ggplot2:::manual_scale(
        'fill',
        values = setNames(
          # Colours
          if(direction == -1){
            rev(RColorBrewer::brewer.pal(
              colourCount, brewerpal))
          }else{
            RColorBrewer::brewer.pal(
              colourCount, brewerpal)
          },
          choices_list
        )
      )
    }
  }
  
  if(stacked == FALSE){
    # plot multibar ----
    p <- ggplot(data = df_summary,
                aes(x = y,
                    y = n))  +
      
      geom_bar(aes(
        fill = y),
        stat = "identity", position = "dodge") +
      
      
      ## shows empty factor level
      scale_x_discrete(drop=FALSE) + 
      # Sets plot theme
      theme_bw()  +
      
      theme(#axis.line = element_line(color='black'),
        plot.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      
      
      
      # Creates subplots
      facet_wrap(names(df_summary[,1]),
                 # Drops unused levels in facets
                 scales = scales,
                 drop = FALSE,
                 nrow = nrow, 
                 ncol = ncol
      )  +
      
      
      theme(strip.text.x = element_text(size = title_size * 0.7, face = "bold")) +
      
      # Prepares plot annotations in % (uses package scales)
      # # Prints values
      geom_text(
        aes(label =
              base::paste0(
                percent,
                "% (",
                n,
                ")"
              )
        ),
        vjust=vjust, hjust=hjust,
        color = "black", size = textsize, angle = angle
      ) +
      
      # Removes plot legend
      theme(legend.position="") +
      # Sets axis formatting
      theme(
        axis.text.x =
          element_text(
            angle = angle, vjust=.5, hjust = .5)
      ) +
      
      # Sets title and subtitle (incl. str_wrap for text wrapping)
      ggtitle(
        
        ## Comments the titlebreak to test element_textbox_simple()
        # stringr::str_wrap(title,
        #                   width = titlebreak_topbarplot),
        label= title,
        
        subtitle =
          paste("n =",
                format(total_n,
                       big.mark = big.mark,
                       decimal.mark = decimal.mark), units)
      ) +
      
      # comment the plot.title below if textgrob is used to paste title on ggplot
      theme(
        
        ## Comments the titlebreak to test element_textbox_simple()
        
        #plot.title = element_text(size = title_size, face = "bold"),
        
        plot.title = ggtext::element_textbox_simple(
          margin = ggplot2::margin(0, 10, 5, 0),
          size = title_size, face = "bold"
        ),
        
        plot.subtitle = element_text(size = title_size * 0.75),
        plot.title.position = "plot"
      ) +
      
      # Adjust scales
      scale_y_continuous(
        limits = c(0, (max(df_summary$n) * dist)), 
        expand = c(0.01, 0)
      )  +
      # Defines label for y axis (must be defined upon function call)
      labs(y = ylab) +
      theme(axis.title.y = element_blank()) +
      
      
      # Flips plot
      coord_flip() +
      # Axis text size
      theme(axis.text = element_text(size = axis_text_size)) +
      
      # Sets options for x axis title
      theme(
        axis.title.x =
          element_text(size = title_size * 0.8,
                       angle = 00)) 
    
    
    if(
      
      (
        exists("choices_list") & 
        "No" %in% levels(df_summary$y) &
        "Yes" %in% levels(df_summary$y)
      ) |
      
      
      ("No" %in% df_summary$y) &
      ("Yes" %in% df_summary$y) &
      is.na(fix_ylab_names[1]) &
      is.na(fix_ylab_color[1])
    ){
      
      colours_yes_no =
        setNames(
          c('#ff9999', '#9999ff',
            "#808080", "#bfbfbf", '#d966ff',
            "#ffc266"),
          sapply(
            base::strwrap(
              as.character(
                c(
                  "No", "Yes", 
                  "Not applicable", "Not sure", "Sometimes",
                  "No response"
                )
                
              ),
              width = xlab_break, 
              simplify=FALSE), 
            paste, collapse="\n")
        )
      
      p <- p + scale_fill_manual(values = colours_yes_no)
      
      
      
    }else{
      
      # Fixes color to the input list of names
      
      if(
        !is.na(fix_ylab_names[1]) &
        !is.na(fix_ylab_color[1])
      ){
        colours = setNames(
          fix_ylab_color,
          fix_ylab_names
        )
        
        
        p <- p + scale_fill_manual(values = colours)
        
        
      }else{
        
        
        
        p <- p + scale_fill_custom()
        
        
        
      }
      
    }
    
    
    
    # Removes the x axis title if fact plots are included
    if(facet_plot == FALSE){ 
      # Sets options for X axis title
      p <-  p  %+% (subset(df_summary, x %in% levels(df_summary$x)[1]) %>%   
                      mutate(x = 
                               
                               factor(x, 
                                      
                                      levels = levels(df_summary$x)[1]
                               )
                             
                      )
      )
      
      
    }else{
      
      if((factors_n %% 2) == 0 | factors_n <= 2){
        
        p <- p
        
      }else{
        
        
        
        p <- cowplot::plot_grid(
          p  %+% (subset(df_summary, x %in% levels(df_summary$x)[1]) %>%   
                    mutate(x = 
                             
                             factor(x, 
                                    
                                    levels = levels(df_summary$x)[1]
                             )
                           
                    )
          ) + 
            theme(axis.title.x = element_blank()), 
          p %+% (subset(df_summary, x %in% levels(df_summary$x)[2:factors_n])%>%   
                   mutate(x = 
                            
                            factor(x, 
                                   
                                   levels = levels(df_summary$x)[2:factors_n]
                            )
                          
                   )
          )  +
            # Axis text size
            theme(plot.title = element_blank(),
                  plot.subtitle = element_blank()), 
          nrow = 2,
          rel_heights = heights
        )
        
        
      }
      
    }
    
    p
    
  }else{
    
    # plot stacked ----
    
    ylab = paste0(ylab, " (%)")  
    
    df_summary <- df_summary %>%  
      rename(Options = y) %>% 
      mutate(x = 
               factor(
                 
                 sapply(
                   base::strwrap(
                     gsub("\\s*\\([^\\)]+\\)","", x),
                     width = 20,
                     simplify=FALSE),
                   paste, collapse="\n"),
                 
                 levels = 
                   
                   sapply(
                     base::strwrap(
                       levels(df_summary$x) %>% 
                         gsub("\\s*\\([^\\)]+\\)","", .),
                       width = 20,
                       simplify=FALSE),
                     paste, collapse="\n")
               )
      )
    
    # Calculates sum of the observations for each facet factor
    totals <- df_summary %>% 
      group_by(x) %>%
      dplyr::summarise(n = sum(n),
                       # set percent value (x-axis value) at which total value should be pasted.
                       percent = as.numeric(105)
      ) %>% 
      ungroup() 
    
    
    # subsets the lebels greater than 3 % responses to avoid overlapping valvues on stacked bars
    df_summary <- df_summary %>%
      mutate(percent_label = replace(
        percent, 
        percent < 3, 
        "")
      )
    
    p <- ggplot(data = df_summary,
                aes(x = forcats::fct_rev(x),
                    y = percent,
                    fill = Options))  +
      geom_bar(
        stat = "identity", 
        position = position_stack(reverse = TRUE),
        width = 0.7
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
        # subsets the lebels greater than 3 % responses to avoid overlapping valvues on stacked bars 
        data = df_summary,
        aes(
          label =
            percent_label
          
        ),
        position = position_stack(vjust=0.5, reverse = TRUE),
        color = "black", size = textsize * 0.8
      ) +
      
      
      # # Prints total values
      geom_text(data=totals ,
                aes(
                  x=x,
                  y=percent,
                  label=n, fill = NULL
                ),
                #nudge_y = 5,
                color = "black", 
                size = textsize * 0.8
      ) +
      
      ggplot2::annotate(geom = "text", label = "(n)", 
                        x = Inf, y = max(totals$percent), 
                        color = "blue", 
                        #hjust = 0.8, 
                        vjust = 1.2, 
                        size = textsize * 1.1, 
                        fontface = "italic") +
      
      
      
      theme(#axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      
      # Sets axis formatting
      theme(
        axis.text.x =
          element_text(
            angle = angle, vjust=.5, hjust = .5
          )
      ) +
      
      # Sets axis formatting
      theme(
        axis.text.y =
          element_text(
            face = "bold"
          )
      ) +
      
      # Sets title and subtitle (incl. str_wrap for text wrapping)
      ggtitle(
        ## Comments the titlebreak to test element_textbox_simple()
        # stringr::str_wrap(title,
        #                   width = titlebreak_topbarplot),
        label= title
      )  +
      
      
      # comment the plot.title below if textgrob is used to paste title on ggplot
      theme(
        
        ## Comments the titlebreak to test element_textbox_simple()
        
        #plot.title = element_text(size = title_size, face = "bold"),
        
        plot.title = ggtext::element_textbox_simple(
          margin = ggplot2::margin(0, 10, 5, 0),
          size = title_size, face = "bold"
        ),
        
        plot.subtitle = element_blank(),
        plot.title.position = "plot"
      ) +
      
      # Adjust scales
      scale_y_continuous(
        limits = c(0, (100 * 1.08)), expand = c(0.01, 0))  +
      # Defines label for y axis (must be defined upon function call)
      labs(
        y = ylab,
        fill = "Options"
      ) +
      theme(axis.title.y = element_blank()) +
      
      
      # Flips plot
      coord_flip() +
      # Axis text size
      theme(axis.text = element_text(size = axis_text_size)) +
      
      # Sets options for x axis title
      theme(
        axis.title.x =
          element_text(size = title_size * 0.8,
                       angle = 00)) 
    
    # Customises colors of the labels
    
    if(
      
      (
        exists("choices_list") & 
        "No" %in% levels(df_summary$y) &
        "Yes" %in% levels(df_summary$y)
      ) |
      
      
      ("No" %in% df_summary$y) &
      ("Yes" %in% df_summary$y) &
      is.na(fix_ylab_names[1]) &
      is.na(fix_ylab_color[1])
    ){
      
      colours_yes_no =
        setNames(
          c('#ff9999', '#9999ff',
            "#808080", "#bfbfbf", '#d966ff',
            "#ffc266"),
          sapply(
            base::strwrap(
              as.character(
                c(
                  "No", "Yes", 
                  "Not applicable", "Not sure", "Sometimes",
                  "No response"
                )
                
              ),
              width = xlab_break, 
              simplify=FALSE), 
            paste, collapse="\n")
        )
      
      
      p <- p + scale_fill_manual(values = colours_yes_no)
      
      
      
    }else{
      
      # Fixes color to the input list of names
      
      if(
        !is.na(fix_ylab_names[1]) &
        !is.na(fix_ylab_color[1])
      ){
        colours = setNames(
          fix_ylab_color,
          fix_ylab_names
        )
        
        
        p <- p + scale_fill_manual(values = colours)
        
        
      }else{
        
        
        
        p <- p + scale_fill_custom()
        
        
        
      }
      
    }
    
    
    p
    
  }
  
}


# Evalbar plots  -----
auto_evalbarplot_facet <-function(
    data = form_4, 
    variable = "religious_safety_felling", 
    facet_variable = "internal_site_type", 
    title = "Some title",
    facet_title = "Disaggregated by country",
    title_size = 10,
    ylab = "Responses", 
    units = "responses", 
    brewerpal = "RdYlGn", 
    varlevel = "vpoor_vgood", 
    titlebreak_evalbarplot = 70, 
    scales = "free_x",
    nrow = NULL,
    ncol = NULL,
    rev_colors = FALSE,
    axis_text_size = 8,
    textsize = 3,
    heights=c(0.55, 0.45),
    facet_plot = TRUE,
    dist = 1.6
    
){
  
  # Creates vector with evaluation scales
  
  vpoor_vgood <- c(
    "Very good",
    "Good",
    "Regular",
    "Poor",
    "Very poor",
    "Not sure",
    "Not applicable"
  )
  
  vlow_vhigh <- c(
    "Very high",
    "High",
    "Average",
    "Low",
    "Very low",
    "Not sure",
    "Not applicable"
  )
  
  vgood_vbad <- c(
    "Very good",
    "Good",
    "Average",
    "Bad",
    "Very bad",
    "Not sure",
    "Not applicable"
  )
  
  likert <- c(
    "Totally Agree",
    "Agree",
    "Agree Slightly",
    "Neutral",
    "Disagree Slightly",
    "Disagree",
    "Strongly Disagree"
  )
  
  agree_disagree <- c(
    "Strongly agree",
    "Agree",
    "Slightly agree",
    "Slightly disagree",
    "Disagree",
    "Strongly disagree"
  )
  
  # Evaluation facet plots with evaluation scales -----
  
  # Checks if variable has available data
  if(
    variable %in% names(data)
  ){
    
    # Loads required packages
    library(ggplot2) ## For plotting
    library(scales) ## For % annotation
    library(dplyr) ## For data wrangling
    library(lazyeval) ## For summarising with the interp function
    library(stringr) ## For text wrapping (e.g. titles)
    library(RColorBrewer) ## For plot colors
    
    ## Calculates total number of responses
    
    total_n <- data %>% 
      select(all_of(variable)) %>% 
      ## Omits NAs
      na.omit() %>% 
      nrow
    
    # Checks if variable exists in the dataset
    if (
      total_n > 0
    ){
      
      
      
      if(is.factor(data %>% pull(facet_variable))){
        facet_levels <- levels(data %>% pull(facet_variable)) %>% gsub("_", " ", .)
      }else{
        facet_levels <- data %>% pull(facet_variable) %>% unique() %>% as.character() %>% sort() %>% gsub("_", " ", .)
      }
      
      facet_levels <- facet_levels %>% 
        unique() %>% 
        gsub("_", " ", .) %>% 
        sort()
      
      
      # Subsets dataset
      df_summary <- data %>%
        select(any_of(c(facet_variable, variable)))  %>%
        ## Omits NAs
        na.omit() %>% 
        ## Group by variable Y
        group_by_(facet_variable, variable) %>%
        ## Count observations
        dplyr::summarise(n=n()) %>%
        rename_(x = facet_variable, y = variable) %>%
        ## Remove underscore
        mutate(
          y = gsub("_", " ", y),
          x = gsub("_", " ", x) 
          
        ) %>%
        ## Arranges results in descending order
        arrange(desc(n)) %>% 
        
        ## Ungroups variables
        ungroup()
      
      
      # Adds factor level "Overall"
      df_summary <- df_summary %>% 
        
        bind_rows(
          .,
          df_summary %>% group_by(y) %>% dplyr::summarise(
            x = "Overall",
            n = sum(n)
          ) %>% 
            select(x, y, n) 
        ) %>% 
        
        # Arranges facet to factor levels
        
        mutate(x = 
                 factor(x, 
                        levels = c("Overall",  facet_levels
                        )
                 )
        ) %>%  
        
        ungroup() %>% 
        
        ## Group by variable Y
        group_by(x) %>%
        ## Count observations
        dplyr::summarise(y=y,
                         n=n,
                         percent = round(n/sum(n) *100, 1)
        ) %>%
        
        ungroup()
      
      
      
      # Calculates sum of the observations for each facet factor and paste with facet title
      df_summary <- df_summary %>% 
        group_by(x) %>%
        dplyr::summarise(x = as.factor(paste0(x, " (", sum(n), ")")),
                         y= y,
                         n = n,
                         percent = percent
        ) %>% 
        ungroup() 
      
      missing_facet <- facet_levels[!(facet_levels %in% gsub("\\s*\\([^\\)]+\\)","", levels(df_summary$x)))] %>% 
        paste0(., " (0)")
      
      
      if(missing_facet != " (0)"){
        df_summary <- df_summary %>% 
          mutate(x = 
                   factor(x, 
                          levels = c(
                            levels(df_summary$x)[1],
                            c(levels(df_summary$x)[2:length(df_summary$x %>% unique())],
                              missing_facet) %>% sort()
                          )
                   )
          ) 
      }
      
      
      df_summary$y <- if(
        varlevel == "vpoor_vgood"
      ){
        base::factor(df_summary$y %>% unlist(),
                     levels = rev(vpoor_vgood))
      } else {
        if(
          varlevel == "vlow_vhigh"
        ){
          base::factor(df_summary$y %>% unlist(),
                       levels = rev(vlow_vhigh))
        } else {
          if(
            varlevel == "vgood_vbad"
          ){
            base::factor(df_summary$y %>% unlist(),
                         levels = rev(vgood_vbad))
          } else {
            if(
              varlevel == "likert"
            ){
              base::factor(df_summary$y %>% unlist(),
                           levels = rev(likert))
            } else {
              if(
                varlevel == "agree_disagree"
              ){
                base::factor(df_summary$y %>% unlist(),
                             levels = rev(agree_disagree))
              } else {
                if(
                  varlevel == "high_low"
                ){
                  base::factor(df_summary$y %>% unlist(),
                               levels = rev(high_low))
                }
                
              }
            }
          }
        }
      }
      
      
      # Colours
      if("Not sure" %in% levels(df_summary[,2] %>% 
                                unlist()) &
         "Not applicable" %in% levels(df_summary[,2] %>% 
                                      unlist())
      ){
        colours = 
          c(
            
            "#808080", # Not applicable
            "#bfbfbf", # Not sure
            
            RColorBrewer::brewer.pal(
              name="RdYlGn", 
              n=nlevels(df_summary[,2] %>%
                          unlist()) -2
            )
          )
        
      }else{
        
        # Colours
        colours = (
          RColorBrewer::brewer.pal(
            name="RdYlGn", 
            n=nlevels(df_summary[,2] %>%
                        unlist())))
        
      }
      ##
      if(rev_colors == FALSE){
        names(colours) = 
          
          levels(df_summary[,2] %>% 
                   unlist())
        
      }else{
        if(rev_colors == TRUE){
          
          names(colours) = 
            rev(
              levels(df_summary[,2] %>% 
                       unlist())
            )
          
          if("Not sure" %in% levels(df_summary[,2] %>% 
                                    unlist()) &
             "Not applicable" %in% levels(df_summary[,2] %>% 
                                          unlist())
          ){
            
            names(colours) =  c(
              levels(df_summary[,2] %>% 
                       unlist())[1:2],
              rev(levels(df_summary[,2] %>% 
                           unlist())[3:7])
            )
          }
          
          
          
        }
        
      }
      ##
      
      # Plots data
      p <- ggplot(data = df_summary, 
                  aes(x = y , y = n, 
                      fill = y
                  ) 
      ) +
        # Determines type of plot
        geom_bar(stat = "identity") + 
        # Sets scale to also show empty levels of factor  
        scale_x_discrete(drop=FALSE) + 
        # Flips plot coordinates  
        coord_flip()  +
        # fills colors
        scale_fill_manual(values=colours) +
        # Sets plot theme
        theme_bw()  +
        
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        # Uses classic theme 
        #theme_classic() +
        # Creates subplots
        facet_wrap(names(df_summary[,1]),
                   # Drops unused levels in facets
                   scales = scales,
                   nrow = nrow, 
                   ncol = ncol,
                   drop = FALSE) +
        
        theme(strip.text.x = element_text(size =  title_size * 0.7, face = "bold")) +
        
        # Sets labels and title
        labs(x = "",  y =  ylab, 
             
             
             ## Comments the titlebreak to test element_textbox_simple()
             # Wraps plot title to 70 characters
             #title = str_wrap(title, width = titlebreak_evalbarplot),
             
             title = title,
             
             
             # Creates subtitle with number of observations
             subtitle =  
               paste("n =",
                     format(total_n, 
                            big.mark=","), units)
        )  + 
        
        
        theme(
          
          ## Comments the titlebreak to test element_textbox_simple()
          
          #plot.title = element_text(size = title_size, face = "bold"),
          
          plot.title = ggtext::element_textbox_simple(
            margin = ggplot2::margin(0, 10, 5, 0),
            size = title_size, face = "bold"
          ),
          
          plot.title.position = "plot",
          # Sets options for subtitles
          plot.subtitle = #element_blank()
            element_text(size = title_size * 0.75,
                         color="black")
        ) + 
        
        # # Sets labels and title
        # labs(x = "",  y =  ylab#, 
        #      # Wraps plot title to 70 characters
        #      # title = str_wrap(facet_title, width = titlebreak_evalbarplot)
        #      
        # )  + 
        # Sets options for X axis title
        theme(
          axis.title.x = 
            element_text(size = title_size * 0.8, 
                         angle = 00)) + 
        
        # Sets options for X axis text
        theme(axis.text.x = 
                element_text(colour="grey20",
                             #size = 10,
                             size =  axis_text_size,
                             face="plain", 
                             angle = 00),
              # Sets options for Y axis text
              axis.text.y = 
                element_text(colour="grey20",
                             #size = 10, 
                             size =  axis_text_size,
                             face="plain")
        ) +
        # Sets options for X axis line
        theme(axis.line.x = element_line(
          color = "black", 
          size = 0.5), 
          # Sets options for Y axis
          axis.line.y = element_line(
            color = "black", 
            size = 0.5)
        ) +
        # Sets options for data labels (text annotations)
        geom_text(
          aes(label = 
                paste0(
                  percent,
                  
                  "% (",
                  n,
                  ")"
                )
          ), 
          hjust = -0.07, vjust = 0.29, size = textsize) +
        
        scale_y_continuous(
          expand = c(.01, .05), # Space between bars and axis
          # Setting automatic limits based on data
          limits = c(
            0, max(df_summary[, 3] * dist)),
          # Ensuring only integer breaks                            
          breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))
        ) +
        # Removes legend 
        theme(legend.position="none")
      
      
      # Adjusts widths of odd number of facet plots 
      #factors_n <- df_summary$x %>% unique() %>% length()
      
      # Adjusts widths of odd number of facet plots 
      factors_n <- levels(df_summary$x) %>% length()
      
      # Removes the x axis title if fact plots are included
      if(facet_plot == FALSE){ 
        # Sets options for X axis title
        p <- p  %+% (subset(df_summary, x %in% levels(df_summary$x)[1]) %>% 
                       mutate(x = as.character(x)))
        
        
      }else{
        
        if((factors_n %% 2) == 0 | factors_n <= 2){
          
          p <- p
          
        }else{
          
          
          p <- cowplot::plot_grid(
            p  %+% (subset(df_summary, x %in% levels(df_summary$x)[1]) %>% 
                      mutate(x = as.character(x))) + 
              theme(axis.title.x = element_blank()) , 
            p %+% (subset(df_summary, x %in% levels(df_summary$x)[2:factors_n]) %>% 
                     mutate(x = as.character(x)))  +
              # Axis text size
              theme(plot.title = element_blank(),
                    plot.subtitle = element_blank()) , 
            nrow = 2,
            rel_heights = heights
          )
          
          
        }
        
      }
      
      print(p)
      
      
    }else{
      
      cat(paste("''",facet_title,"''"), fill = TRUE, labels = base::paste0("No data available for"))
    }
    
    
  }else{
    
    cat(paste("''",facet_title,"''"), fill = TRUE, labels = base::paste0("The variable does not exist"))
  }
  
  
}


# Autoplot Disaggregated based on varible type ----

autoplot_facets <- function(
    # Main variable
  var = "mentalhealth_ph3_interest_pleasure",
  # Dataset
  dataset = df_hh,
  # Path to XLSForm
  form_path = NA, 
  # selects language labels of the form
  label_language = "English",
  # If NA, automatically picks the facet variable by looking into form_path
  auto_facet_variable = NA,
  
  auto_title_size = 10,
  # Label of x axis
  x_label = "responses",
  # Mininum frequency for word clouds
  auto_minfreq = 2, 
  # Language for wordclouds - topnetwork plot
  lang = "en",
  # Stop words for wordclouds and topnetwork cloud
  auto_stopwords = c("NIL", "nil", "NA", "None", "program", "programm", "programs",
                     "kind", "put", "lack", "low", "high", "TEKAN"),
  
  titlebreak_topbarplot = 100,
  
  titlebreak_topbootplot = 97,
  
  titlebreak_evalbarplot = 100,
  
  labels_count_topbar = Inf,
  
  remove_outliers_topbootplot = FALSE,
  # If levels_topbarplot = FALSE, arranges the labels in decreasing order.
  # If TRUE, arranges the labels as the options are in XLSForm.
  # Also accepts labels inputs e.g levels_topbarplot = c(A, B, C).
  # Also accepts variable names inputs e.g levels_topbarplot = c("area_country", "wash_handwashing").
  levels_topbarplot = NA_character_,
  # Breaks the answer labels
  xlab_break = 33,
  # If NA, then prints max 10 responses, or prints the responses by provided input (in fraction)
  ranswers_sampleprop = NA,
  # Size of wordcloud (width, height), to be adjusted (width) if words are dropped
  autoword_scale = c(2.2, .12),
  # To reverse color order of evalbar plot
  eval_rev_colors = FALSE,
  # Adjusts the heights of topbar and evalbar facet plots
  autoplot_heights = c(0.55, 0.45),
  # Acronyms in network wordcloud
  topnetwork_keep_acronyms = TRUE,
  
  # if True will not plot facet plots
  
  auto_facet_plot = TRUE,
  
  # Scale value of the words size ratio in networkplot
  
  networkplot_scale = c(1.3),
  
  barplot_dist = 1.6
  
) {
  
  
  # Checks if variable has available data
  if(
    dataset %>%
    select(all_of(var)) %>%
    na.omit %>%
    nrow() > 0
  ){
    
    
    # Automats the form path by looking into the dataframes variable names ----
    if(is.na(form_path) & "form" %in% names(dataset)){
      
      if("kiis" %in% dataset$form){
        
        form_path = "forms/Key-informant interviews (source file in XLSForm) - UNICEF AZE.xlsx"
        
      }else{
        
        if("household" %in% dataset$form){
          
          form_path = "forms/Household interviews (source file in XLSForm) - UNICEF AZE.xlsx"
          
        }else{
          
          if("online" %in% dataset$form){
            
            form_path = "forms/Online survey form (source file in XLSForm) - UNICEF AZE.xlsx"
            
          }
        }
      }
    }
    
    
    # Adjust facet variable names based on form_path ----
    
    if(
      
      is.na(auto_facet_variable)
      
    ){
      
      if(
        grepl("household", tolower(form_path))
      ){
        
        auto_facet_variable <- "demo_interviewer_participant" 
        auto_facet_title <- "Disaggregated by governorate"
        
      }else{
        
        if(
          grepl("online", tolower(form_path))
        ){
          
          auto_facet_variable <- "demographics_participant_gender" 
          auto_facet_title <- "Disaggregated by gender"
        }else{
          
          if(
            grepl("informant", tolower(form_path))
          ){
            
            auto_facet_variable <- "demographics_participant"
            auto_facet_title <- "Disaggregated by participant"
          } 
        }
      }
    }
    
    
    ## If list of selected variables is provided to remove the outliers
    if(
      !is.logical(remove_outliers_topbootplot)
    ){
      
      if(
        var %in% remove_outliers_topbootplot
      ){
        
        remove_outliers_topbootplot = TRUE
        
      }else{
        remove_outliers_topbootplot = FALSE
      }
    }
    
    
    ## Loads form survey to extract variable title and options type  -----
    # Function to replace NA with previous observation  
    replace_na_previous <- function(x, a=!is.na(x)) {
      
      x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
      
    }
    
    # Imports form data (sheet survey)
    koboform_survey <- 
      suppressWarnings(
        readxl::read_xlsx(
          path = form_path, 
          sheet = "survey"
        )
      ) %>% 
      select(type,name, contains("label")) %>%  
      #janitor::clean_names() %>% 
      rename_with(., 
                  ~ tolower(
                    # removes parathesis and text within
                    gsub("\\s*\\([^\\)]+\\)","", .) %>% 
                      # Removes all the text before the matching character
                      gsub(".*:", "", .) %>% 
                      stringr::str_trim()
                  )
      )  %>% 
      filter(!is.na(name)) %>% 
      dplyr::mutate(type = stringr::str_trim(type) %>% 
                      gsub(" or_other", "", .)
      ) %>% 
      # Creates variable with group name
      mutate(
        group = ifelse(type == "begin group", name, NA)
      ) %>% 
      # Replace empty cells with group names
      mutate(group = replace_na_previous(group)) %>% 
      
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
        final_name = paste(group, name, sep="_")
      ) %>% 
      
      distinct(final_name, .keep_all = T) %>% 
      
      # Keeps only variables present in the final dataset
      # filter(name %in% names(needsdata)) %>% 
      mutate(question_type = (type %>% stringr::str_split_fixed(., " ", 2))[,1]) %>% 
      
      mutate(
        options_id = 
          
          data.table::fifelse(
            # Returns logical if the value is one single word
            str_count(trimws(type) ,"\\W+") == 0,
            question_type,
            (
              type %>% 
                stringr::str_split_fixed(., " ", 2)
            )[,2]
            
          )
      ) %>% 
      dplyr::select(type, 
                    name, 
                    group, 
                    final_name, 
                    question_type, 
                    options_id, 
                    all_of(tolower(label_language))
      ) 
    
    # Renames language variable
    names(koboform_survey) <- c("type", 
                                "name", 
                                "group", 
                                "final_name", 
                                "question_type", 
                                "options_id",  
                                "label")
    
    # # Extracts option id
    # 
    options_id <- koboform_survey %>%
      filter(final_name == var) %>%
      pull(options_id)
    
    # Imports form data (sheet choices) ----
    koboform_choices <-
      suppressWarnings(
        readxl::read_xlsx(
          path = form_path,
          sheet = "choices")
      ) %>%
      select(list_name, name, contains("label")) %>%  
      #janitor::clean_names() %>% 
      rename_with(., 
                  ~ tolower(
                    # removes parathesis and text within
                    gsub("\\s*\\([^\\)]+\\)","", .) %>% 
                      # Removes all the text before the matching character
                      gsub(".*:", "", .) %>% 
                      stringr::str_trim()
                  )
      )  %>% 
      select(list_name, all_of(tolower(label_language))) %>%
      filter(!is.na(list_name))
    
    # Renames language variable
    names(koboform_choices) <- c("list_name",  
                                 "label")
    
    # Extract count of options
    labels_options <- koboform_choices %>%
      filter(list_name %in% options_id) 
    
    
    labels_count <- nrow(labels_options)
    
    # # Extract count of options
    # suppressWarnings(
    #   labels_count <- dataset %>%
    #     select(all_of(var))  %>%
    #     ## Omits NAs
    #     na.omit() %>%
    #     ## Split variables from select_multiple questions
    #     splitstackshape::cSplit(
    #       ., splitCols = var,
    #       sep = " ",
    #       direction = "long") %>%
    #     pull() %>%
    #     unique() %>%
    #     length()
    # )
    
    
    
    
    ## If list of selected variables is provided to sort the labels according to XLSForm
    if(
      !is.logical(levels_topbarplot)
    ){
      
      if(
        var %in% levels_topbarplot |
        
        (
          "No" %in% (labels_options$label) &
          "Yes" %in% (labels_options$label) 
        ) |
        
        (
          "Never" %in% (labels_options$label) &
          "Rarely" %in% (labels_options$label) 
        )
        
        #"Other" %in% (dataset %>% pull(var) %>% unique())
        
      ){
        
        levels_topbarplot = TRUE
        
      }else{
        
        levels_topbarplot = FALSE
      }
    }
    
    ## If list of selected variables is provided to sort the labels according to XLSForm
    if(
      !is.logical(eval_rev_colors)
    ){
      
      if(
        var %in% eval_rev_colors 
        
      ){
        
        eval_rev_colors = TRUE
        
      }else{
        eval_rev_colors = FALSE
      }
    }
    
    
    ## Performs checks of variable type -----
    
    
    if (
      grepl("likert", koboform_survey$type[koboform_survey$final_name == var])
    ) {
      
      ### Prints evalbarplot for evaluation questions
      suppressMessages(
        invisible(
          auto_evalbarplot_facet(
            data = dataset,
            variable = var,
            varlevel = "likert",
            title = koboform_survey$label[koboform_survey$final_name == var],
            title_size = auto_title_size,
            titlebreak_evalbarplot = titlebreak_evalbarplot,
            rev_colors = eval_rev_colors,
            facet_variable = auto_facet_variable,
            facet_title = auto_facet_title,
            facet_plot = auto_facet_plot,
            heights = autoplot_heights,
            dist = barplot_dist
          )
        )
      )
      
      
      
    }else{
      
      if (
        grepl("vpoor_vgood", koboform_survey$type[koboform_survey$final_name == var])
      ) {
        
        ### Prints evalbarplot for evaluation questions
        suppressMessages(
          invisible(
            auto_evalbarplot_facet(
              data = dataset,
              variable = var,
              varlevel = "vpoor_vgood",
              title = koboform_survey$label[koboform_survey$final_name == var],
              title_size = auto_title_size,
              titlebreak_evalbarplot = titlebreak_evalbarplot,
              rev_colors = eval_rev_colors,
              facet_variable = auto_facet_variable,
              facet_title = auto_facet_title,
              facet_plot = auto_facet_plot,
              heights = autoplot_heights,
              dist = barplot_dist
            )
          )
        )
        
        
      }else{
        
        if (
          grepl("vlow_vhigh", koboform_survey$type[koboform_survey$final_name == var])
        ) {
          
          ### Prints evalbarplot for evaluation questions
          suppressMessages(
            invisible(
              auto_evalbarplot_facet(
                data = dataset,
                variable = var,
                varlevel = "vlow_vhigh",
                title = koboform_survey$label[koboform_survey$final_name == var],
                title_size = auto_title_size,
                titlebreak_evalbarplot = titlebreak_evalbarplot,
                rev_colors = eval_rev_colors,
                facet_variable = auto_facet_variable,
                facet_title = auto_facet_title,
                facet_plot = auto_facet_plot,
                heights = autoplot_heights,
                dist = barplot_dist
              )
            )
          )
          
          
          
        }else{
          
          if (
            grepl("vgood_vbad", koboform_survey$type[koboform_survey$final_name == var])
          ) {
            
            ### Prints evalbarplot for evaluation questions
            suppressMessages(
              invisible(
                auto_evalbarplot_facet(
                  data = dataset,
                  variable = var,
                  varlevel = "vgood_vbad",
                  title = koboform_survey$label[koboform_survey$final_name == var],
                  title_size = auto_title_size,
                  titlebreak_evalbarplot = titlebreak_evalbarplot,
                  rev_colors = eval_rev_colors,
                  facet_variable = auto_facet_variable,
                  facet_title = auto_facet_title,
                  facet_plot = auto_facet_plot,
                  heights = autoplot_heights,
                  dist = barplot_dist
                )
              )
            )
            
            
            
          }else{
            
            if (
              grepl("agree_disagree", koboform_survey$type[koboform_survey$final_name == var])
            ) {
              
              ### Prints topbarplot for evaluation questions
              suppressMessages(
                invisible(
                  auto_evalbarplot_facet(
                    data = dataset,
                    variable = var,
                    varlevel = "agree_disagree",
                    title = koboform_survey$label[koboform_survey$final_name == var],
                    title_size = auto_title_size,
                    titlebreak_evalbarplot = titlebreak_evalbarplot,
                    rev_colors = eval_rev_colors,
                    facet_variable = auto_facet_variable,
                    facet_title = auto_facet_title,
                    facet_plot = auto_facet_plot,
                    heights = autoplot_heights,
                    dist = barplot_dist
                  )
                )
              )
              
            }else{
              
              # Select one and select multiple
              if ((koboform_survey$question_type[koboform_survey$final_name == var] == "select_one" |
                   koboform_survey$question_type[koboform_survey$final_name == var] == "select_multiple")  &
                  var != auto_facet_variable  
              ) {
                
                if(
                  labels_count > 4 &
                  auto_facet_plot == TRUE &
                  !("No" %in% (labels_options$label) &
                    "Yes" %in% (labels_options$label)) &
                  
                  !("Never" %in% (labels_options$label) &
                    "Rarely" %in% (labels_options$label))
                  
                ){
                  stacked = TRUE
                }else{
                  stacked = FALSE
                }
                
                ### Prints topbarplot for select_one questions ----
                ddpcr::quiet( 
                  auto_topbarplot_facet(
                    data = dataset,
                    key_variable = var,
                    title = koboform_survey$label[koboform_survey$final_name == var],
                    title_size = auto_title_size,
                    titlebreak_topbarplot = titlebreak_topbarplot,
                    levels = levels_topbarplot,
                    xlab_break = xlab_break,
                    form_path = form_path,
                    facet_variable = auto_facet_variable,
                    facet_title = auto_facet_title,
                    heights = autoplot_heights,
                    facet_plot = auto_facet_plot,
                    language = label_language,
                    labels_count = labels_count_topbar,
                    dist = barplot_dist,
                    stacked = stacked
                  )
                )
                
              } else {
                
                if (koboform_survey$question_type[koboform_survey$final_name == var] == "text") {
                  
                  # Replaces labels for variables having "_other" by following variable
                  if(grepl("_other", var)){
                    title_other <- 
                      paste0("Other? - ",
                             # The code selects the prior variable and extracts its label text
                             koboform_survey$label[which(koboform_survey$final_name ==var)-1]
                             #koboform_survey$label[koboform_survey$final_name == gsub("_other", "", var)]
                      ) 
                    
                  }else{
                    
                    title_other <- 
                      koboform_survey$label[koboform_survey$final_name == var]
                  }
                  
                  
                  # ### Prints wordclouds for text questions ----
                  # suppressMessages(
                  #   invisible(
                  #     topwordcloud(
                  #       var = dataset %>% 
                  #         pull(all_of(var)), 
                  #       min.freq = auto_minfreq, 
                  #       language = lang, 
                  #       stopwords = auto_stopwords, 
                  #       title = title_other,
                  #       scale= autoword_scale
                  #     )
                  #   )
                  # )
                  
                  
                  ### Prints Network wordcloud for text questions ----
                  suppressMessages(
                    invisible(
                      topnetworkplot(
                        # Defines variable (needs to be a text variable)
                        data = dataset,
                        key_variable = var,
                        language = lang,
                        stopwords= auto_stopwords,
                        title = title_other,
                        titlebreak = titlebreak_topbarplot,
                        title_size = auto_title_size,
                        keep_acronyms = topnetwork_keep_acronyms,
                        scale = networkplot_scale,
                        facet_plot = auto_facet_plot
                        
                      )
                    )
                  )
                  
                  
                  
                  
                  # # Prints multiple plots using custom function
                  # suppressMessages(
                  #   invisible(
                  #     top_sentiment_barplot(
                  #       
                  #       data = dataset,
                  #       key_variable = var,
                  #       min.freq = 1,   
                  #       title = title_other,
                  #       facet_plot = auto_facet_plot
                  #       
                  #     )
                  #   )
                  # )
                  # cat(sep="\n")
                  # cat("<br> ",sep="\n")
                  # cat(sep="\n")
                  # 
                  # ### Prints randomanswers for text questions ----
                  # suppressMessages(
                  #   invisible(
                  #     randomsanswers(
                  #       df = dataset, 
                  #       key_var = var,  
                  #       title = title_other,
                  #       sampleprop = ranswers_sampleprop,
                  #       form_path = form_path)
                  #   )
                  # )
                  
                  
                  
                } else {
                  
                  if (koboform_survey$question_type[koboform_survey$final_name == var] == "integer" |
                      koboform_survey$question_type[koboform_survey$final_name == var] == "decimal") {
                    
                    if (dataset %>% select(all_of(var)) %>% unlist() %>% na.omit() %>% length() > 0) {
                      
                      if(var != auto_facet_variable){
                        suppressMessages(
                          invisible(
                            auto_topbootplot_facet(
                              data = dataset,
                              key_variable = var,
                              title = koboform_survey$label[koboform_survey$final_name == var],
                              title_size = auto_title_size,
                              titlebreak_topbootplot = titlebreak_topbootplot,
                              remove_outliers_topbootplot = remove_outliers_topbootplot,
                              facet_variable = auto_facet_variable,
                              facet_plot = auto_facet_plot
                            )
                          )
                        )
                        
                        
                        
                      }
                      
                    }
                    
                  } else {
                    
                    ddpcr::quiet(
                      print(
                        paste(
                          var,
                          "is of unsupported type")
                      )
                    )
                    
                  }
                }
              }
            }
          }
        } 
      }
    }
    
  }else{
    
    cat(paste("''",var,"''"), fill = TRUE, labels = base::paste0("No data available for"))
  }
  
}

replace_na_previous <- function(x, a =! is.na(x)) {
  
  x[which(a)[c(1, 1:sum(a))][cumsum(a) + 1]]
  
}

# Prints label names with percentages ----

toplabel <- function(
    variable = "org_function_fiscal_council",
    df = proj_data,
    unit = "", # Representing word for calculated percentages
    and = "and ",
    labels_count = Inf, # Count of labels 
    labels_count_rest = Inf, # Count of labels
    stopwords = c("NIL", "nil", "NA", "None", "program", "programm", "programs",
                  "kind", "put", "lack", "low", "high", "TEKAN"),
    stopwords_stemmed = c(""),
    language = "en",
    lowercase = TRUE,
    # Answer options to match using | and &
    label_grepl = "",
    label_grepl_middle = "",
    label_grepl_not = "",
    quo_marks = FALSE,
    form_path = NA,
    slice = 1:100,
    Only_pretext = TRUE,
    reorder_label = FALSE,
    only_perc = FALSE # Only when label count is 1
){
  
  # Stemming words to their core parts allows for aggregation of different words with similar structure 
  # (e.g., singular and plurals or the words with the same root or radical). This is a common technique 
  # in natural language processing and should be seen together with the network co-occurrence plot 
  # as well as the random sample below. The frequency of the stemmed words are shown in percentages. 
  # Percentages exceeding 100% indicate that the word count is more than the number of respondents. 
  # The percentages of the words are irrelevant of co-occurrence with other words unlike the text network plots.
  
  
  if(variable %in% names(df)){
    
    
    # Creats string vector
    
    vector <- df %>% 
      pull(variable) %>% 
      as.character() %>% 
      na.omit
    
    # Split words into list
    vector_list <- gsub(
      "_", " ", 
      strsplit(vector, " ") %>% 
        unlist %>% 
        gsub("\n", "", .)# %>% 
      # Removes . from the texts
      #gsub("\\.+","", .)
    ) %>% 
      trimws() %>%  
      stringi::stri_enc_toutf8() 
    
    
    if(!is.na(form_path)){
      # Imports form data (sheet survey) to check if the variable is text type.
      koboform_survey <- 
        suppressWarnings(
          readxl::read_xlsx(
            path = form_path, 
            sheet = "survey"
          )
        ) %>% 
        suppressMessages() %>% 
        select(type,name, contains("label")) %>%
        rename_with(., 
                    ~ tolower(
                      # removes parathesis and text within
                      gsub("^.*?\\((.*)\\)[^)]*$", "\\1", .) %>% 
                        stringr::str_trim()
                    )
        ) %>% 
        #rename(label = label_en) %>% 
        filter(!is.na(name)) %>% 
        dplyr::mutate(type = stringr::str_trim(type) %>% 
                        gsub(" or_other", "", .)
        ) %>% 
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
        
        ## After removing 1, 2 and 3 from watersources and the container assessment, we keep the distinct variables 
        distinct(final_name, .keep_all = T) %>% 
        # Keeps only variables present in the final dataset
        # filter(name %in% names(needsdata)) %>% 
        mutate(question_type = (type %>% stringr::str_split_fixed(., " ", 2))[,1]) %>% 
        
        mutate(
          options_id = 
            
            data.table::fifelse(
              # Returns logical if the value is one single word
              str_count(trimws(type) ,"\\W+") == 0,
              question_type,
              (
                type %>% 
                  stringr::str_split_fixed(., " ", 2)
              )[,2]
              
            )
        ) %>% 
        select(type, name, final_name, question_type, options_id, all_of(language))
      
      #### Extract options label from survey form ----
      # Extracts option id
      
      options_id <- koboform_survey %>%
        filter(final_name == variable) %>%
        pull(options_id)
      
      # Imports form data (sheet choices) ----
      koboform_choices <-
        suppressWarnings(
          readxl::read_xlsx(
            path = form_path,
            sheet = "choices")
        ) %>%
        select(list_name, name, contains("label")) %>%  
        #janitor::clean_names() %>% 
        rename_with(., 
                    ~ tolower(
                      # removes parathesis and text within
                      gsub("^.*?\\((.*)\\)[^)]*$", "\\1", .) %>% 
                        stringr::str_trim()
                    )
        ) %>% 
        select(list_name, all_of(tolower(language))) %>%
        filter(!is.na(list_name))
      
      # Renames language variable
      names(koboform_choices) <- c("list_name",  
                                   "label")
      
      # Extract count of options
      labels_options <- koboform_choices %>%
        filter(list_name %in% options_id) %>% 
        select(label)
      
      
      
      if(variable %in% koboform_survey$final_name){
        #  Removes stop words if variable is"text" type
        if(koboform_survey$question_type[koboform_survey$final_name == variable] == "text"){
          
          
          # Extract text of Question text to be used in the list of the stopwords
          title <- koboform_survey %>% 
            filter(final_name == variable) %>% 
            select(all_of(language)) %>% 
            pull()
          
          # Removing stopwords as well as other custom words
          vector_list <- vector_list %>%
            gsub("[[:punct:]]+","", .) %>% 
            stringr::str_trim() %>% 
            ## Replacing paragraph signs
            stringr::str_replace_all(
              pattern = "\n" , " ") %>%
            ## Replacing puctuation numbers generated due to punctuation signs
            stringr::str_replace_all(
              pattern = "[[:digit:]]" , "") %>%
            ## Replacing multiple spacing
            stringr::str_replace_all(
              pattern = "\\s+" , " ") %>%  
            ## Replaces line breaks
            stringr::str_replace_all(pattern = "\\n" , "") %>%
            # Removes all empty strings from a character vector
            stringi::stri_remove_empty() %>% 
            tolower() %>% 
            tibble(col = .) %>% 
            filter(
              !(col %in% c(stopwords::stopwords(language),
                           stopwords::stopwords(source = "smart"), # Only for english
                           stopwords,
                           strsplit(title, " ") %>% unlist %>% 
                             stringr::str_replace_all(
                               pattern = "[[:punct:]]" , "") %>% 
                             stringr::str_replace_all(
                               pattern = "[[:digit:]]" , "")
              )
              )
            ) %>%
            # Coverts to stemmed words
            mutate(col = textstem::stem_strings(col)) %>% 
            # Filter out the stemmed stopwords
            filter(
              !(col %in% c(
                stopwords_stemmed,
                stopwords
              )
              )
            ) %>%
            unlist %>% 
            as.character()
          
          ## Calculates total number of interviews
          total_n <- nrow(
            df %>%
              select(any_of(variable))  %>%
              na.omit()
          )
          
        }
        
      }
      
      
    }
    

    

    #   }
    #   
    # }
    
    # Apply input of lowercase parameter
    if(lowercase == TRUE){
      vector_list <- 
        vector_list %>%
        tolower() %>%
        gsub("i ", "I ", .) %>% 
        tibble(col = .) %>% 
        na.omit %>% 
        count(col) %>% 
        arrange(desc(n))
      
    }else{
      
      if(lowercase == FALSE){
        
        vector_list <- vector_list %>%
          tibble(col = .) %>% 
          na.omit %>% 
          count(col) %>% 
          arrange(desc(n))
      }
    }
    
    
    if(quo_marks == TRUE){
      quo_marks <- "'"
    }else{
      quo_marks <- ""
    }
    
    
    ## Calculates total number of responses
    
    if(is.na(form_path)){
      
      total_n <- sum(vector_list$n)
    }else{
      #  Removes stop words if variable is"text" type
      if(koboform_survey$question_type[koboform_survey$final_name == variable] != "text"){
        total_n <- sum(vector_list$n)
    
      }
    }
    
    # Create a string to search for 
    label_grepl <- 
      label_grepl %>% 
      paste(.,collapse="|") %>% 
      tolower()
    
    label_grepl_middle <- 
      label_grepl_middle %>% 
      paste(.,collapse="|") %>% 
      tolower()
    
    label_grepl_not <- 
      label_grepl_not %>% 
      paste(.,collapse="|") %>% 
      tolower()
    
    if(label_grepl_not != ""){
      
      vector_list <- vector_list %>% 
        filter(
          !grepl(
            label_grepl_not, 
            tolower(
              col
            )
          )
        ) %>% 
        arrange(desc(col))
    }
    
    ## Extract only searched labels
    vector_list_pre <- vector_list %>% 
      filter(
        grepl(
          # If label_grepl is empty than filter every row
          label_grepl, 
          tolower(
            col
          )
        )
      ) %>% 
      dplyr::slice(slice)
    
    
    if(label_grepl_middle != ""){
      ## Extract only searched labels
      vector_list_middle <- vector_list %>% 
        filter(
          grepl(
            label_grepl_middle, 
            tolower(
              col
            )
          )
        ) %>% 
        dplyr::slice(slice) %>% 
        arrange(desc(col))
    }else{
      
      vector_list_middle <- data.frame()
    } 
    
    
    if(label_grepl != ""){
      # Extract the renmaining
      vector_list_rest <- vector_list %>% 
        filter(
          !grepl(
            label_grepl, 
            tolower(
              col
            )
          )
        ) %>% 
        dplyr::slice(slice) %>% 
        arrange(desc(col))
      
      if(label_grepl_middle != ""){
        # Extract the renmaining
        vector_list_rest <- vector_list_rest %>% 
          filter(
            !grepl(
              label_grepl_middle, 
              tolower(
                col
              )
            )
          ) %>% 
          dplyr::slice(slice) %>% 
          arrange(desc(col))
        
      }
      
    }else{
      
      vector_list_rest <- data.frame()
    }       
    
    # if(variable %in% koboform_survey$final_name){
    #   #  Removes stop words if variable is the variable is  "text" type
    #   if(#koboform_survey$question_type[koboform_survey$final_name == variable] == "select_multiple" |
    #     koboform_survey$question_type[koboform_survey$final_name == variable] == "text") {
    #     
    #     ## Calculates total number of interviews
    #     total_n <- nrow(
    #       df %>%
    #         select(any_of(variable))  %>%
    #         na.omit()
    #     )
    #   }
    # }
    
    
    if(only_perc == TRUE | is.na(only_perc)){
      text_perc <- ""
      text_perc <-  paste0(
        
        round(sum(vector_list_pre$n) / 
                total_n  *
                100),
        "%"
      )
      
      
      
      text_perc_middle <- ""
      text_perc_middle <-  paste0(
        
        round(sum(vector_list_middle$n) / 
                total_n  *
                100),
        "%"
      )
      
      
      text_perc_rest <- ""
      text_perc_rest <-  paste0(
        
        round(sum(vector_list_rest$n) / 
                total_n  *
                100),
        "%"
      )
      
    }
    
    
    # Store words list length
    vector_length_pre <- nrow(vector_list_pre)
    vector_length_middle <- nrow(vector_list_middle)
    vector_length_rest <- nrow(vector_list_rest)
    
    
    if(reorder_label == TRUE){
      
      labels_list <-
        labels_options %>% 
        pull(label)
      
      if(lowercase == TRUE){
        labels_list %>% 
          tolower()
      }
      
      
      if(vector_length_pre > 0){
        reorder_vector_list_pre <- match(labels_list, vector_list_pre$col, nomatch = 0)
        
        vector_list_pre <- vector_list_pre[reorder_vector_list_pre,]
      }
      
      if(vector_length_middle > 0){
        reorder_vector_list_middle <- match(labels_list, vector_list_middle$col, nomatch = 0)
        
        vector_list_middle <- vector_list_middle[reorder_vector_list_pre,]
      }
      
      
      if(vector_length_rest > 0){
        reorder_vector_list_rest <- match(labels_list, vector_list_rest$col, nomatch = 0)
        
        vector_list_rest <- vector_list_rest[reorder_vector_list_rest,]
      }
      
    }
    
    
    
    if(Only_pretext == TRUE){
      
      vector_length_middle = 0
      vector_length_rest = 0
    }
    
    
    # Apply input of labels_count parameter
    if(!is.infinite(labels_count)){
      vector_length_pre <- labels_count
    }
    
    # Apply input of labels_count parameter
    if(!is.infinite(labels_count_rest)){
      vector_length_rest <- labels_count_rest
    }
    
    
    # sets loop length when rest of the labels do not exist
    if(
      vector_length_rest  != 0 & 
      vector_length_middle != 0
    ){
      
      length = 3
      
    }else{
      
      if(
        (vector_length_rest  == 0 & vector_length_middle != 0) |
        (vector_length_rest  != 0 & vector_length_middle == 0)
      ){
        
        length = 2
        
      }else{
        
        if(
          vector_length_rest  == 0 & 
          vector_length_middle == 0
        ){
          
          length = 1
        }
      }
    }
    
    
    
    for(n in 1:length){
      
      if(n == 1){
        vector_length <- vector_length_pre
        vector_list <- vector_list_pre
        
      }else{
        
        if(n == 2){
          
          
          if(
            vector_length_middle != 0
          ){
            
            vector_length <- vector_length_middle
            vector_list <- vector_list_middle
            
          }else{
            
            
            vector_length <- vector_length_rest
            vector_list <- vector_list_rest
            
          }
          
        }else{
          if(n == 3){
            vector_length <- vector_length_rest
            vector_list <- vector_list_rest
          }
        }
      }
      
      
      # Concatenate labels with responses percentages
      
      if(vector_length > 0 & (only_perc == FALSE | is.na(only_perc))){
        
        if(!is.na(form_path)){
        if(variable %in% koboform_survey$final_name){
          if(koboform_survey$question_type[koboform_survey$final_name == variable] == "text") {
            
            text <- paste("the", vector_length, "most frequent stemmed words after processing are ")
            
          }else{
            text <- ""
          }
        }
          }else{
          text <- ""
        }
        
        
        
        for (i in 1:vector_length) {
          
          # Rounds the values 
          
          value <- vector_list$n[i] / 
            total_n  *
            100
          
          if(!is.na(value)){
            if(value < 0.1){
              
              value <- round(value, 2)
            }else{
              if(value < 1){
                
                value <- round(value, 1)
              }else{
                
                value <- round(value)
              }
              
            }
          }
          
          # if i is smaller than the vector_length
          if(i < vector_length){
            text <- paste0(
              text,
              quo_marks,
              vector_list %>%  dplyr::slice(i:i) %>% 
                pull(col),
              quo_marks,
              " (",
              value,
              "%",
              unit,
              "), "
            )
            
          }else{
            
            # if i is equal to vector_length but not equal to one
            if (i == vector_length &
                i != 1) {
              
              text <-  paste0(
                text,
                and,
                quo_marks,
                vector_list %>%  dplyr::slice(i:i) %>% 
                  pull(col),
                quo_marks,
                " (",
                value,
                "%",
                unit,
                ")"
              )
              
            }else{
              
              # if i is equal to one (For single word list)
              
              if(i == 1){
                text <-  paste0(
                  quo_marks,
                  vector_list %>%  dplyr::slice(i:i) %>% 
                    pull(col),
                  quo_marks,
                  " (",
                  value,
                  "%",
                  unit,
                  ")"
                )
                
              }
              
            }
            
          }
        }
        
      }
      
      # Assign the name to the created object
      assign(paste("text", 
                   n,
                   sep = "_") , 
             text) 
      
    }
    
    ###  Print the results
    
    if(is.na(only_perc)){
      
      
      if("text_1" %in% ls()){
        if(!grepl(and, text_1))
          text_1 <- gsub("\\s*\\([^\\)]+\\)","", text_1)
      }
      
      if("text_2" %in% ls()){
        if(!grepl(and, text_2))
          text_2 <- gsub("\\s*\\([^\\)]+\\)","", text_2)
      }
      
      if("text_3" %in% ls()){
        if(!grepl(and, text_3))
          text_3 <- gsub("\\s*\\([^\\)]+\\)","", text_3)
      }
      
      
      if(vector_length_rest  == 0 & vector_length_middle == 0){
        # if there is no rest list of results than it doesnt print while
        paste0(text_perc, " assessed as ", text_1)
      } else{ 
        
        if(vector_length_rest  == 0 & vector_length_middle != 0){
          # if there is no rest list of results than it doesnt print while
          paste0(text_perc, " assessed as ", text_1, ", while ", text_perc_middle, " assessed as ", text_2)
          
        } else{ 
          
          if(vector_length_rest  != 0 & vector_length_middle == 0){
            # if there is no rest list of results than it doesnt print while
            paste0(text_perc, " assessed as ", text_1, ", while ", text_perc_rest, " assessed as ", text_2)
            
          } else{ 
            
            # if there is rest label/results to be printed "... while X% said x(%) and y(%)"
            paste0(text_perc, " assessed as ", text_1, ", while ", text_perc_middle, " assessed as ", text_2,
                   ", and ", text_perc_rest, " assessed as ", text_3)
          }
        }
      }
      
      
    }else{
      if(only_perc == TRUE){
        
        paste(text_perc)
        
      }else{
        if(only_perc == FALSE){
          
          paste(text_1)
          
        }else{
          if(vector_length_pre == 0){
            cat(
              paste0("(",variable,")'"), 
              fill = TRUE, 
              labels = paste0("'Data is not available/enough")
            )
          }
        }
        
      }
    }
    
    ##
    
  }else{
    
    cat(
      paste0("(",variable,")'"), 
      fill = TRUE, 
      labels = paste0("'Variable is not available")
    )
    
  }
}

# Prints label names with percentages ----

toprint_question <- function(
    variable = "demo_physically_disabled",
    df = proj_data,
    language = "English",
    quo_marks = FALSE,
    form_path = NA
){
  
  
  # Automats the form path by looking into the dataframes variable names
  if(is.na(form_path) & "form" %in% names(df)){
    
    if("kiis" %in% df$form){
      
      form_path = "forms/Key-informant interviews (source file in XLSForm) - UNICEF AZE.xlsx"
      
    }else{
      
      if("household" %in% df$form){
        
        form_path = "forms/Household interviews (source file in XLSForm) - UNICEF AZE.xlsx"
        
      }else{
        
        if("online" %in% df$form){
          
          form_path = "forms/Online survey form (source file in XLSForm) - UNICEF AZE.xlsx"
          
        }
      }
    }
  }
  
  
  # Imports form data (sheet survey) to check if the variable is text type.
  koboform_survey <- 
    suppressWarnings(
      readxl::read_xlsx(
        path = form_path, 
        sheet = "survey"
      )
    ) %>% 
    suppressMessages() %>% 
    select(type,name, contains("label")) %>%
    rename_with(., 
                ~ tolower(
                  # removes parathesis and text within
                  gsub("\\s*\\([^\\)]+\\)","", .) %>% 
                    # Removes all the text before the matching character
                    gsub(".*:", "", .) %>% 
                    stringr::str_trim()
                )
    ) %>% 
    
    mutate(name =  
             stringr::str_trim(name)
    ) %>% 
    
    # Creates variable with group name
    mutate(
      group = ifelse(type == "begin group", name, NA)
    ) %>% 
    # Replace empty cells with group names
    mutate(group = replace_na_previous(group) %>% tolower()) %>% 
    
    # Create column with final variable names
    mutate(
      final_name = paste(group, name, sep="_")) %>% 
    
    #rename(label = label_en) %>% 
    filter(final_name %in% variable) %>% 
    
    
    select(all_of(tolower(language))) 
  
  
  
  if(nrow(koboform_survey) > 0){
    
    names(koboform_survey) <- "label"
    # Reduce labels 
    koboform_survey <- koboform_survey %>% 
      mutate(
        label = gsub(
          "  ", " ", stringr::str_trim(label)
        ) %>%
          # Erases all non-letter characters before first letter
          gsub("^[^a-zA-Z]*","", .) %>% 
          Hmisc::capitalize()
      )
    
    
    if(quo_marks == TRUE){
      quo_marks <- "'"
    }else{
      quo_marks <- ""
    }
    
    text <- paste0(
      quo_marks,
      koboform_survey %>%
        pull(label),
      quo_marks
    )
    
    
    paste(text)
    
  }else{
    
    cat(
      paste0("(",variable,")'"), 
      fill = TRUE, 
      labels = paste0("'Data is not available/enough")
    )
    
  }
  
}

# Custom function for sampling answers ----
top_sentiment_cloud <- function(
    
  var = df_kiis$guide_needs, 
  min.freq = 1, 
  stopwords = c("NIL", "nil", "NA", "None", "program", "programm", "programs",
                "kind", "put", "lack", "low", "high", "TEKAN"),
  seed = 1234, 
  language = "en",  
  scale = c(4.0, .65), 
  title = "Sentiment analysis",
  title_size = 1.5, 
  legend_height=0.30
  
) {
  
  library("tidytext")
  
  # Checks if variable has available data
  if(
    var %>% 
    na.omit %>% 
    length() > 0
  ){
    
    # Loads required packages in invisible mode to avoid messages
    suppressPackageStartupMessages(library(dplyr)) ## For data wrangling
    suppressPackageStartupMessages(library(RColorBrewer)) ## For plot colors  
    suppressPackageStartupMessages(library(wordcloud)) ## For plot colors
    suppressPackageStartupMessages(library(tm)) ## For text mining
    suppressPackageStartupMessages(library(stopwords)) ## For parsing stopwords in different langauges
    
    # Manipulating string variable 
    ## Selecting variable/question
    df <- var  %>% 
      ##Replacing puctuation and symbols
      stringr::str_replace_all(
        pattern = "[[:punct:]]" , "") %>% 
      ## Replacing paragraph signs
      stringr::str_replace_all(
        pattern = "\n" , " ") %>%
      ## Replacing puctuation numbers generated due to punctuation signs
      stringr::str_replace_all(
        pattern = "[[:digit:]]" , "") %>%
      ## Replacing multiple spacing
      stringr::str_replace_all(
        pattern = "\\s+" , " ") %>% 
      ## Setting strings to lower case
      tolower() 
    
    # Converting to corpus and 
    corpus  <- Corpus(VectorSource(df)) %>%  
      ## Removing stopwords as well as other custom words
      tm::tm_map(removeWords, 
                 c(stopwords::stopwords(language),
                   stopwords::stopwords(source = "smart"), # Only for english
                   stopwords,
                   strsplit(title, " ") %>% unlist %>% 
                     stringr::str_replace_all(
                       pattern = "[[:punct:]]" , "") %>% 
                     stringr::str_replace_all(
                       pattern = "[[:digit:]]" , "")
                 ) %>% unique()
      ) 
    
    # Creating a document term matrix
    dtm <- tm::DocumentTermMatrix(corpus)
    
    # Setting random sequence for reproducibility
    set.seed(seed)
    
    # Creating matrix with corpus
    matrix.cloud <- as.matrix(dtm)
    
    # Computing word frequency
    freq.cloud <- sort(colSums(matrix.cloud), 
                       decreasing = TRUE)
    
    # Defining labels
    label.cloud <- names(freq.cloud)
    
    # Creating dataframe
    data.cloud <- 
      data.frame(word = label.cloud,
                 freq = freq.cloud)
    
    data.cloud <-
      data.cloud %>%
      filter(freq >= min.freq) %>% 
      inner_join(get_sentiments("bing") %>%
                   filter(sentiment %in% c("positive",
                                           "negative"))) %>%
      
      acast(word ~ sentiment, value.var = "freq", fill = 0)
    
    wrap_strings  <- function(vector_of_strings, width)
    {as.character(sapply(vector_of_strings,FUN=function(x)
    {paste(strwrap(x,width=width), collapse="\n")}))}
    
    # if(!is.na(title)| title == "" ){
    #   ## Prepare to print title
    #   layout(matrix(c(1, 2), nrow=2),
    #          heights=c(1, 5))
    #   par(mar=rep(0, 4))
    #   plot.new()
    #   text(x = 0.5, y = legend_height,
    #        labels=wrap_strings(
    #          str_wrap(title),
    #          width = 1 * getOption("width")),
    #        cex = title_size
    #   )
    # }
    
    
    p_word <-  
      comparison.cloud(term.matrix = data.cloud,
                       colors = c("red","blue"),
                       max.words = 100,
                       scale = scale,
                       rot.per=0,
                       title.size=2.5,
                       match.colors=T
      )
    
    ddpcr::quiet(p_word)
    
  }else{
    
    cat(paste("''",title,"''"), fill = TRUE, labels = base::paste0("No data available for"))
    
    
    
  }
}


## Network plot of co-occurrences of text data ----
top_sentiment_barplot <- function(
    # Defines variable (needs to be a text variable)
  
  data = df_kiis,
  key_variable = "guide_implementation",
  facet_variable = "internal_site_type",
  
  
  # Defines language for stopwords function
  language = "en",
  
  min.freq = 1,
  
  seed = 1234,
  
  # Adds stop words
  stopwords = c("NIL", "nil", "NA", "None", "program", "programm", "programs",
                "kind", "put", "lack", "low", "high", "TEKAN"),
  
  # Title
  title = "Some title",
  
  # Title break
  titlebreak = 90,
  
  title_size = 10,
  
  textsize = 3,
  
  angle = 0,
  
  big.mark = ",",
  
  decimal.mark = ".",
  
  # Unit of measurement
  units = "responses",
  
  ylab = "Counts",
  
  dist = 1.4,
  
  axis_text_size = 9,
  
  hjust = -0.07, 
  
  vjust = 0.29, 
  
  facet_plot = TRUE
  
) {
  
  
  # To analyse the open-ended answers from the household interviews, Text Network Plots were used. 
  # This plot shows relationships between words which appear together in the same answers (co-occurrence).
  # The words with higher frequency of co-occurrence (number of times the word occurs with other words) 
  # appear with larger font size. The thickness of the blue line shows the extent of co-occurrence with 
  # other words. 
  
  # Checks if variable has available data
  if(
    data %>%
    select(any_of(key_variable)) %>%
    unlist(., use.names = FALSE) %>%
    na.omit %>%
    length() > 0
  ){
    
    
    # # Subsets dataset
    
    ## Setting random generation seed to allow for reproducibility
    set.seed(seed)
    
    # List the names of facet_levels for plot disaggregartion
    
    
    if(is.factor(data %>% pull(facet_variable))){
      facet_levels <- levels(data %>% pull(facet_variable)) %>% gsub("_", " ", .)
    }else{
      facet_levels <- data %>% pull(facet_variable) %>% unique() %>% as.character() %>% sort() %>% gsub("_", " ", .)
    }
    
    facet_levels <- facet_levels %>% 
      unique() 
    
    
    facet_levels <-
      c(
        "Total responses",
        facet_levels
      )
    
    # Adds line break for number of observations
    #subtitle = paste(subtitle, "\n")
    
    
    df_summary <- data %>%
      select(any_of(c(facet_variable, key_variable)))  %>%
      rename_(x = facet_variable,
              y = key_variable) %>%
      ## Omits NAs
      na.omit() %>%
      mutate(y= gsub("_"," ",y),
             x= gsub("_"," ",x) 
      )
    
    
    if(facet_plot == FALSE | df_summary$x %>% unique() %>% length() < 2){
      facet_levels <- facet_levels[1]
    }
    
    
    for (i in 1:length(facet_levels)){
      
      
      
      
      title_textplot = facet_levels[i]
      subtitle = ""
      title_size_textplot = title_size * 0.7
      label_size = 8
      
      # Filter the data per country and adjust the title and subtitle text and size
      if(facet_levels[i] != "Total responses"){
        
        
        df_textplot <- df_summary %>%
          filter(x %in% facet_levels[i]) %>%
          select(y)
        
        total_n <- nrow(df_textplot)
        
      }else{
        
        
        df_textplot <- df_summary %>%
          select(y)
        
        total_n <- nrow(df_textplot)
      }
      
      
      # Loads packages
      library(quanteda)
      # Loads required packages in invisible mode to avoid messages
      suppressPackageStartupMessages(library(dplyr)) ## For data wrangling
      suppressPackageStartupMessages(library(RColorBrewer)) ## For plot colors  
      suppressPackageStartupMessages(library(wordcloud)) ## For plot colors
      suppressPackageStartupMessages(library(tm)) ## For text mining
      suppressPackageStartupMessages(library(stopwords)) ## For parsing stopwords in different langauges
      
      
      ## Selecting variable/question
      df <- df_textplot$y  %>% 
        ##Replacing puctuation and symbols
        stringr::str_replace_all(
          pattern = "[[:punct:]]" , "") %>% 
        ## Replacing paragraph signs
        stringr::str_replace_all(
          pattern = "\n" , " ") %>%
        ## Replacing puctuation numbers generated due to punctuation signs
        stringr::str_replace_all(
          pattern = "[[:digit:]]" , "") %>%
        ## Replacing multiple spacing
        stringr::str_replace_all(
          pattern = "\\s+" , " ") %>% 
        ## Setting strings to lower case
        tolower() 
      
      # Converting to corpus and 
      corpus  <- Corpus(VectorSource(df)) %>%  
        ## Removing stopwords as well as other custom words
        tm::tm_map(removeWords, 
                   c(stopwords::stopwords(language),
                     stopwords::stopwords(source = "smart"), # Only for english
                     stopwords,
                     strsplit(title, " ") %>% unlist %>% 
                       stringr::str_replace_all(
                         pattern = "[[:punct:]]" , "") %>% 
                       stringr::str_replace_all(
                         pattern = "[[:digit:]]" , "")
                   ) %>% unique()
        ) 
      
      # Creating a document term matrix
      dtm <- tm::DocumentTermMatrix(corpus)
      
      # Setting random sequence for reproducibility
      set.seed(seed)
      
      # Creating matrix with corpus
      matrix.cloud <- as.matrix(dtm)
      
      # Computing word frequency
      freq.cloud <- sort(colSums(matrix.cloud), 
                         decreasing = TRUE)
      
      # Defining labels
      label.cloud <- names(freq.cloud)
      
      # Creating dataframe
      data.cloud <- 
        data.frame(word = label.cloud,
                   freq = freq.cloud)
      
      data.cloud <-
        data.cloud %>%
        filter(freq >= min.freq) %>% 
        inner_join(get_sentiments("bing") %>%
                     filter(sentiment %in% c("positive",
                                             "negative"))) %>%
        
        group_by(sentiment) %>% 
        dplyr::summarise(freq = sum(freq, na.rm = T)) %>% 
        mutate(percent = round(freq/sum(freq) *100, 1)) %>% 
        mutate(sentiment = Hmisc::capitalize(sentiment))
      
      
      p_sentiment <-  
        # plot multibar ----
      ggplot(data = data.cloud,
             aes(x = sentiment,
                 y = freq))  +
        
        geom_bar(aes(
          fill = sentiment),
          stat = "identity", position = "dodge",
          width = 0.7) +
        
        
        ## shows empty factor level
        scale_x_discrete(drop=FALSE) + 
        # Sets plot theme
        theme_bw()  +
        
        theme(#axis.line = element_line(color='black'),
          plot.background = element_blank(),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        ) +
        
        # Prepares plot annotations in % (uses package scales)
        # # Prints values
        geom_text(
          aes(label =
                base::paste0(
                  percent,
                  "% (",
                  freq,
                  ")"
                )
          ),
          vjust=vjust, hjust=hjust,
          color = "black", size = textsize, angle = angle
        ) +
        
        # Removes plot legend
        theme(legend.position="") +
        # Sets axis formatting
        theme(
          axis.text.x =
            element_text(
              angle = angle, vjust=.5, hjust = .5)
        ) +
        
        # Sets title and subtitle (incl. str_wrap for text wrapping)
        ggtitle(
          
          label= title_textplot,
          
          subtitle =
            paste("n =",
                  format(total_n,
                         big.mark = big.mark,
                         decimal.mark = decimal.mark), units)
        ) +
        
        # comment the plot.title below if textgrob is used to paste title on ggplot
        theme(
          
          plot.title = ggtext::element_textbox_simple(
            margin = ggplot2::margin(0, 10, 5, 0),
            size = title_size_textplot, face = "bold"
          ),
          
          plot.subtitle = element_text(size = title_size * 0.7),
          plot.title.position = "plot"
        ) +
        
        # Adjust scales
        scale_y_continuous(
          limits = c(0, (max(data.cloud$freq) * dist)), 
          expand = c(0.01, 0)
        )  +
        # Defines label for y axis (must be defined upon function call)
        labs(y = ylab) +
        theme(axis.title.y = element_blank()) +
        
        
        # Flips plot
        coord_flip() +
        # Axis text size
        theme(axis.text = element_text(size = axis_text_size)) +
        
        # Sets options for x axis title
        theme(
          axis.title.x =
            element_text(size = title_size * 0.7,
                         angle = 00))
      
      
      
      
      
      colours_sentiments =
        setNames(
          c('#ff9999', '#9999ff'),
          
          c(
            "Negative", "Positive"
          )
        )
      
      p_sentiment <- p_sentiment + scale_fill_manual(values = colours_sentiments)
      
      ## Assign the index to name of each plot
      
      assign(paste("p_sentiment",
                   i,
                   sep = "_") ,
             p_sentiment)
      
      rm(p_sentiment)
      
      
    }
    
    
    ## Adds plot title to ggplot without any margin space from left
    
    library(gridExtra)
    library(grid)
    
    if(facet_plot == TRUE){
      # Binds the disaggregated plots
      # Checks if any 2 level facet plot is created 
      if("p_sentiment_2" %in% ls()){
        
        # Extract names of textplots from global environment
        p_sentiment <- ls()[grepl("p_sentiment_", ls())]
        
        # Combine all the plots in 1 grob
        pl <- lapply(1:length(p_sentiment), function(.x) 
          get(paste0("p_sentiment_", .x))
        )
        
        # Adjusts widths of odd number of facet plots 
        factors_n <- length(p_sentiment)
        
        # If facet levels are in even number or 1
        if((factors_n %% 2) == 0){
          
          p_fct <- gridExtra::arrangeGrob(
            grobs=pl,
            nrow= ceiling(factors_n/2), 
            ncol=2
          )
          
          
          
          p_fct <- cowplot::plot_grid(
            p_fct
          ) 
          
          
        }else{
          
          # If facet levels are in odd number then arrange Total_responses plot at the top
          # then build the bottom row
          
          bottom_row  <- gridExtra::arrangeGrob(
            grobs=pl[2:factors_n],
            nrow= floor(factors_n/2), 
            ncol=2
          )
          
          nrow_odd_facets <- if(factors_n > 3){ceiling(factors_n/2)}else{floor(factors_n/2)}
          
          p_fct <- cowplot::plot_grid(
            pl[[1]] ,
            bottom_row,
            nrow = 2 ,
            rel_heights = if(factors_n > 3){
              c(1/nrow_odd_facets,((nrow_odd_facets-1)/nrow_odd_facets))
            }else{
              c(0.5, 0.5)
            }
          ) 
          
        }
        
        # comment the plot.title below if textgrob is used to paste title on ggplot
        p_fct <- p_fct + 
          ggtitle(
            
            label= title
          ) +
          
          # comment the plot.title below if textgrob is used to paste title on ggplot
          theme(
            
            plot.title = ggtext::element_textbox_simple(
              margin = ggplot2::margin(0, 10, 5, 0),
              size = title_size, face = "bold"
            )
          )
        ddpcr::quiet(
          p_fct
        )
        
        
        # Deletes the intermediary plots
        rm(list=ls()[ls() %in% c(p_sentiment, "p_fct")])
        
      }else{
        
        
        p_sentiment_1 <- p_sentiment_1 + 
          ggtitle(
            
            label = title
          ) +
          
          # comment the plot.title below if textgrob is used to paste title on ggplot
          theme(
            
            plot.title = ggtext::element_textbox_simple(
              margin = ggplot2::margin(0, 10, 5, 0),
              size = title_size, face = "bold"
            )
          )
        
        
        ddpcr::quiet(
          p_sentiment_1
        )  
        
      }
      
    }else{
      
      if(facet_plot == FALSE){
        
        # Binds the title
        p_sentiment_1 <- p_sentiment_1 + 
          ggtitle(
            
            label = title
          ) +
          
          # comment the plot.title below if textgrob is used to paste title on ggplot
          theme(
            
            plot.title = ggtext::element_textbox_simple(
              margin = ggplot2::margin(0, 10, 5, 0),
              size = title_size, face = "bold"
            )
          )
        
        ddpcr::quiet(
          p_sentiment_1
        )  
        
      }
    }
    
  }else{
    
    cat(paste("''",title,"''"), fill = TRUE, labels = base::paste0("No data available for text-network"))
  }
}



# ≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈ ----#
# ≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈ ----#
# Function to plot Boruta results
boruta_vimp_plot <- function(
    mod = NA,         # Boruta model
    del_less_Imp = 1,  # Delete some less important vars to be plotted
    Confirmed = "Confirmed", # rename labels
    Tentative = "Tentative", # rename labels
    Rejected = "Rejected", # rename labels
    title = NA, # plot title
    # collors for C-confirmed; T_entative; R_ejected; Sh_adows
    collor4_CTRSh = c("#4E9F3D", "#FFBD69", "#BDBDD7", "#F05454") #C;T;R;Sh colors
){
  brta_plot <- mod[2][[1]] %>% # scoring results from boruta model
    as.data.frame %>% # as df
    gather("vars", "value") %>% # cols to rows
    tibble() %>% # as tibble format
    mutate(value = ifelse(is.infinite(value), NA, value)) %>% # -Inf to NA
    # Below: decision col based on results of Boruta alg. (Confirmed, Tentative, Rejection)
    mutate(decision = ifelse(vars %in% c(mod[1][[1]][mod[1][[1]] == "Confirmed"] %>% names), Confirmed,
                             ifelse(vars %in% c(mod[1][[1]][mod[1][[1]] == "Tentative"] %>% names), Tentative,
                                    ifelse(vars %in% c(mod[1][[1]][mod[1][[1]] == "Rejected"] %>% names), Rejected, 
                                           vars)))) %>%
    mutate(collor = ifelse(decision == Confirmed, collor4_CTRSh[1], # new col for collors
                           ifelse(decision == Tentative, collor4_CTRSh[2],
                                  ifelse(decision == Rejected, collor4_CTRSh[3], 
                                         collor4_CTRSh[4]))))
  
  # reordering the level increasing based on the scores collor
  varss <- reorder(brta_plot$vars,brta_plot$value,na.rm = TRUE) %>% levels
  # Reducing the name of displayed vars by removing the least important vars.
  brta_plot <- brta_plot %>% filter(vars %in% varss[del_less_Imp:length(varss)]) %>%
    mutate(across(c(vars, decision, collor), factor))
  
  my_collors <- list(); my_dec <- levels(brta_plot$decision)
  for(i in 1:length(my_dec)){ # extracting collors corresponding to the decision levels
    my_collors[[i]] <- brta_plot$collor[brta_plot$decision == my_dec[i]][1]
    # than use it to collor correctly in plot
  }
  
  my_collors <- unlist(my_collors)
  
  plot_out <- ggplot(data = na.omit(brta_plot), # using data to plot
                     aes(x = reorder(vars,value,na.rm = TRUE), # vars sorted by values
                         y=value, color = decision)) + # color by decision col
    geom_boxplot() + # barplot
    scale_color_manual(values = as.character(my_collors)) +
    theme_bw() + # plot style
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          plot.title = element_textbox_simple(size = 11, face = "bold", 
                                              margin = ggplot2::margin(0, 15, 10, 0)))+
    xlab("") + # remove x lab
    coord_flip() # flip plot
  return(plot_out)
}


# ≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈ ----

##############################################
rf_plot_res <- function(rfmdfit, df, pred_nr = 1:4,  xlab_len = 25, form = NA){
  list0 <- list(); idx = 0
  for(i in rownames(rfmdfit$importance)[rownames(rfmdfit$importance) %in% pred_nr]){
    idx <- idx +1
    if(!df[, i] %>% is.numeric){ #!df[, i] %>% is.numeric
      list0[[idx]] <- pdp::partial(rfmdfit, pred.var = i, plot = TRUE,
                                   plot.engine = "ggplot2") +
        geom_point(size = 3, color = "black", shape = 22, fill = "#4E9F3D")+
        scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),
                                                       width = xlab_len))+
        theme_bw() + 
        labs(title = paste0(form %>% filter(name %in% i) %>% pull(label_pt), " (", i, ")")) +#ggtitle(i)+ ggtitle(i)+  
        theme(axis.title.y=element_blank(),
              plot.title = element_textbox_simple(size = 10, face = "bold", 
                                                  margin = ggplot2::margin(0, 15, 10, 0))) +
        coord_flip() + ylab(rfmdfit$call$formula[[2]])
    } else{
      list0[[idx]] <- pdp::partial(rfmdfit, pred.var = i, plot = TRUE,
                                   plot.engine = "ggplot2") +
        geom_line(size = 1.5, color = "#4E9F3D")+
        theme_bw() + 
        labs(title = form %>% filter(name %in% i) %>% pull(label_pt),
             caption = i) +#ggtitle(i)+ 
        theme(axis.title.y=element_blank(),
              plot.title = element_text(color = "darkgray", size = 10, face = "bold")) +
        coord_flip() + ylab(rfmdfit$call$formula[[2]])
      
      
      
      
    }
  }
  return(list0)
}




# PLOTTING SENTIMENT ANALIYS BARPLOT AND CLOUD PLOTS COMBINED
sentim_barcloud_comb <- function(
    df, # data frame
    var, # main variable
    colors = c("red", "blue"), # collors
    ylim_adj = NA, # adjusting y limits to display bar labels
    title = "Add title here", # title
    title_size = 5, # title size
    subtitle_size = 5, # subtitle size
    facet_title_size = 20, # facet title size
    scale_size_cloud = 10 # # adjust the scale of cloud
){
  # variable info as vector
  text_var <-  df[, var] %>% na.omit %>% # del NAs
    filter(grepl("^[A-Z]*", var)) %>% # del empty
    filter(!!sym(var) != "N/A") %>% #del N/As
    pull(var) # variable info as vector
  
  # LIBRARIES
  # Loading instaling and loading pckages if not installed and loaded
  requiredPackages <- c("tidytext", "dplyr", "ggwordcloud",
                        "ggtext", "ggpubr", "ggh4x", "rlang")
  ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  ipak(requiredPackages)
  
  
  # data for the cloud sentimental analysis
  tokens_cloud <- tibble(text_var) %>% # as data frame
    unnest_tokens(word, text_var) %>%  # generating tokens
    # filtering words matching "BING" positive/negative
    inner_join(get_sentiments("bing")) %>% # positive/negative 
    count(sentiment, word, sort = TRUE) # freq of words
  
  # if there only positive or negative use the correct color
  if(length(unique(tokens_cloud$sentiment)) < 2){# posit or negat only?
    if(unique(tokens_cloud$sentiment) == "positive") { # if posit...
      colors = rep(colors[2], 2) # color blue
    } else if(unique(tokens_cloud$sentiment) == "negative"){ # if negat...
      colors = rep(colors[1], 2) # color red
    }} else {colors <- colors} # if posit & negat red/blue
  
  # data for the barplot sentimental analysis
  tokens_barplot <- tokens_cloud %>% # use tokens_cloud data created
    group_by(sentiment) %>% # grouping
    summarise(Responses = sum(n)) %>% # suming up responses (freq words)
    ungroup %>% # ungroup
    # creating col percentage
    mutate(perc = round(Responses/sum(Responses)*100,2))
  
  # If statement to automate the label on top of bars
  if(is.na(ylim_adj)){ # if function arg "ylim_adj" not NA....
    # so y limm plot will be the highst value among positive/negative plus 15% this value
    ylim_adj <- max(tokens_barplot$Responses) + max(tokens_barplot$Responses)*.15
    # if function arg "ylim_adj" is NA adjust by using the function arg
  } else{ ylim_adj <- max(tokens_barplot$Responses) + ylim_adj}
  
  # used to change the color in facet
  strip <- strip_themed(text_x = elem_list_text(colour = colors,
                                                face = c("bold", "bold"),
                                                size=facet_title_size))
  
  # plotting cloud sentiment analysis
  sent_cplot <- ggplot(tokens_cloud,
                       aes(label = word, 
                           size = n, 
                           x = sentiment, 
                           color = sentiment)) +
    scale_color_manual(values = colors) + # colors using arg
    geom_text_wordcloud_area() + # ploting cloud
    scale_size_area(max_size = scale_size_cloud) + # changing scale of cloud
    scale_x_discrete(breaks = NULL) + # removing labels
    theme_bw() + # ready theme
    coord_flip() + # flipping plot
    xlab("")  + # removing x lab
    facet_wrap2(~sentiment, nrow = 1, strip = strip)  # using facet and strip
  
  # plotting clud sent analysis
  sent_bar <- ggplot(tokens_barplot, aes(x = sentiment, y = Responses, fill = sentiment)) + 
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(perc, "% (", Responses, ")")), hjust = -0.2, size = 3) +
    scale_fill_manual(values = colors) +
    
    theme(legend.position = "none")  +
    labs(title = title,
         subtitle = paste0("n = ", length(text_var), " responses")) +
    guides(fill = "none") +
    scale_x_discrete(labels = NULL, breaks = NULL) +
    # xlab("Responses")+
    theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      plot.subtitle=element_text(face='italic', color='black')) +
    scale_y_continuous(limits = c(0, ylim_adj), expand = c(0, 0)) +
    theme_bw() +
    coord_flip()+
    theme( plot.title = element_textbox_simple(
      size = title_size, lineheight = 1, padding = margin(0, 10, 10, 0)),
      plot.subtitle = element_textbox_simple(face='italic', size = subtitle_size)) 
  
  # plotting all plot together
  library("ggtext")
  ggarrange(sent_bar, sent_cplot, ncol = 1, nrow = 2)
}

# Scaling data ----
scaling_min_max <- function(x) {
  
  # The resultant values range between zero(0) and one(1).
  
  return (
    (
      x - min(x, na.rm = TRUE)) /
      (
        max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
      )
  )
}

#' Read XLSForm and generate dummy data automatically randomly.
#'
#' Takes in an XLSForm and creates a dataframe with random data from its options.
#' @param path # XLSForm path as character input
#' @param rows # Desired number of rows as integer input
#' @param seed # Integer to pass to set.seed function
#' @param integer_range # Integer range for sampling
#' @param decimal_range # Decimal range for sampling
#' @param start_date # Start date for sampling
#' @param text_example # Text to populate the database
#' @return A R dataframe in tbl_df format
#' @export

create_data <- function(
    # XLSForm path (character)
  path = "forms/example_xlsform.xlsx",
  # Number of rows to create
  rows = 20,
  # Pseudo-random seed for reproducibility
  seed = "10101",
  # Range of integers for sampling
  integer_range = 1:2000,
  # Range of decimals for sampling
  decimal_range = 0.50:100.00,
  # Date of start for random dates
  start_date = "2020-01-01",
  # Text
  text_example = "At auctor urna nunc id cursus metus aliquam eleifend mi in nulla posuere sollicitudin aliquam ultrices sagittis orci a scelerisque purus semper eget duis at tellus at urna condimentum mattis pellentesque id nibh tortor id aliquet lectus proin nibh nisl condimentum id venenatis a condimentum vitae sapien pellentesque habitant morbi"
) {
  
  # Packages ----
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(readxl))
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Custom function ----
  # Replace NA with previous observation
  replace_na_previous <- function(x, a=!is.na(x)) {
    
    x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
    
  }
  
  # Read questions ----
  questions <- 
    readxl::read_excel(
      path, sheet = "survey") %>%   
    select(
      type,name, contains("label")
    ) %>%
    rename_with(
      .,
      ~ tolower(
        # Remove text outside parenthesis and adds prefix label_
        gsub(
          "^.*?\\((.*)\\)[^)]*$", 
          "label_\\1", .)
      )
    ) %>% 
    # Required to drop empty lines 
    filter(!is.na(name)) %>% 
    mutate(name =  
             stringr::str_trim(name)
    ) %>% 
    # Create variable with group name
    mutate(
      group = ifelse(type == "begin group", name, NA)
    ) %>% 
    # Replace empty cells with group names
    mutate(group = replace_na_previous(group) %>%
             tolower()
    ) %>% 
    # Filters out type "begin group"
    filter(
      type != "begin group" & 
        type != "begin repeat" & 
        type != "end group" & 
        type != "end repeat" & 
        type != "note") %>%  
    
    # Create column with final variable names
    mutate(
      question_name = paste(group, name, sep="_")) %>% 
    mutate(
      question_type = (
        type %>% 
          stringr::str_split_fixed(., " ", 2)
      )[,1]) %>% 
    
    mutate(
      options_id = (
        type %>% 
          stringr::str_split_fixed(., " ", 2)
      )[,2] %>% 
        # Get only first word before space
        # Needed for options with "or_other"
        gsub( " .*$", "", .) %>% 
        # Remove trailing white spaces
        trimws()
    ) %>% 
    rename(list_name = name)
  
  
  
  # Subset by type ----
  
  # Get question
  question_types <- 
    unique(
      questions$question_type
    )
  
  ## THIS PROCESS SHOULD BE AUTOMATED
  ## IMPORTANT IN CASE OTHER TYPES COME UP!
  
  ## Text questions
  questions_text <- 
    questions %>% 
    filter(
      question_type == "text"
    ) %>% 
    pull(question_name)
  
  ## Integer questions
  questions_integer <- 
    questions %>% 
    filter(
      question_type == "integer"
    ) %>% 
    pull(question_name)
  
  ## Decimal questions
  questions_decimal <- 
    questions %>% 
    filter(
      question_type == "decimal"
    ) %>% 
    pull(question_name)
  
  ## Date questions
  questions_date <- 
    questions %>% 
    filter(
      question_type == "date"
    ) %>% 
    pull(question_name)
  
  ## Image questions
  questions_image <- 
    questions %>% 
    filter(
      question_type == "image"
    ) %>% 
    pull(question_name)
  
  ## Geopoint questions
  questions_geopoint <- 
    questions %>% 
    filter(
      question_type == "geopoint"
    ) %>% 
    pull(question_name)
  
  ## Select_one questions
  questions_select_one <- 
    questions %>% 
    filter(
      question_type == "select_one"
    ) %>% 
    pull(question_name)
  
  ## Select_multiple questions
  questions_select_multiple <- 
    questions %>% 
    filter(
      question_type == "select_multiple"
    ) %>% 
    select(question_name)
  
  
  
  
  # Read choices ---- 
  # Import worksheet "choices"
  choices <- 
    readxl::read_excel(path, sheet = "choices") %>%   
    select(list_name, name, contains("label")) %>%
    rename_with(.,
                ~ tolower(
                  # Remove text outside parenthesis and adds prefix label_
                  gsub("^.*?\\((.*)\\)[^)]*$", "label_\\1", .)
                )
    ) %>% 
    # Required to drop empty lines 
    filter(!is.na(name)) %>% 
    mutate(name =  
             stringr::str_trim(name)
    ) 
  
  # Subset questions before merging ----
  questions_subset <- 
    questions %>% 
    select(
      question_name, 
      options_id,
      question_type
    ) %>%
    rename(
      question_name = question_name
    ) %>% 
    mutate(
      options_id =
        ifelse(
          options_id == "",
          question_type,
          options_id
        )
    )
  
  # Subset choices before merging -----
  choices_subset <- choices %>% 
    select(list_name, name) %>% 
    na.omit() %>% 
    rename(
      options_id = list_name,
      options = name
    )
  
  # Merge subsets -----
  ## To have all questions (incl. text and integer) and associated choices
  ## in one place.
  merged_form <- merge(
    questions_subset, 
    choices_subset,
    all.x = T
  ) %>% 
    # Reorder columns to ease development
    select(
      question_type,
      question_name,
      options_id,
      options
    )
  
  
  ## Get unique questions
  questions = 
    merged_form["question_name"] %>% 
    unique() 
  
  # Empty dataframe  ----
  output <- matrix(
    ncol = nrow(questions), 
    nrow = rows
  ) %>% 
    data.frame() %>%
    tibble::tibble()
  
  # Name variables
  names(output) <- questions$question_name
  
  
  ## Fill text ----
  output_data <- output %>% 
    mutate_at(
      questions_text,
      function(x) ifelse(
        is.na(x), 
        text_example, 
        x
      )
    )
  
  ## Fill integer ----
  output_data <- output_data %>% 
    mutate_at(
      questions_integer,
      function(x) ifelse(
        is.na(x), 
        sample(integer_range, rows), 
        x
      )
    )
  
  ## Fill decimal ----
  output_data <- output_data %>% 
    mutate_at(
      questions_decimal,
      function(x) ifelse(
        is.na(x), 
        sample(decimal_range, rows), 
        x
      )
    )
  
  ## Fill dates ----
  output_data <- output_data %>% 
    mutate_at(
      questions_date,
      as.Date
    ) %>% 
    mutate_at(
      questions_date,
      function(x) ifelse(
        is.na(x), 
        seq.Date(
          as.Date(start_date),
          lubridate::today(), # Rows minus 1 day
          by="day") %>% 
          # Random sample
          sample(
            size = rows, 
            replace = TRUE
          ) %>% 
          format("%Y-%m-%d"),
        x
      )
    )
  
  ## Fill images ----
  output_data <- output_data %>% 
    mutate_at(
      questions_image,
      function(x) ifelse(
        is.na(x), 
        "https://logo.jpg", 
        x
      )
    )
  
  ## Fill geopoints ----
  output_data <- output_data %>% 
    mutate_at(
      questions_geopoint,
      function(x) ifelse(
        is.na(x),
        paste0(
          "c(", 
          runif(
            rows,
            -10.021484375, 2.021484375
          ), ", ", 
          runif(
            rows,
            49.021484375, 53.021484375
          ), 
          ")"
        ), 
        x
      )
    )
  
  for (i in 1:nrow(questions)) {
    
    # Get options
    subset_choices <- merged_form$options[
      merged_form$question_name == questions[i,]] 
    
    ## Fill select_one ----
    if (
      unique(merged_form$question_type[
        merged_form$question_name == questions[i,]]) ==
      "select_one"
    ) {
      # Store created data
      created_data <- subset_choices %>% 
        sample(rows, replace = T) 
    }
    
    # Fill select_multiple ----
    if (
      unique(merged_form$question_type[
        merged_form$question_name == questions[i,]]) ==
      "select_multiple"
    ) {
      
      # Randomise number of selected options
      mutilselect_sample <- function(
    x = subset_choices,
    sizes = multiselected
      ) {
        
        # Set number of samples per row
        multiselected <- 
          sample(
            1:4,
            rows,
            replace = TRUE
          )
        
        # Sample options 
        result = character()
        
        for (i in 1:length(sizes)) {
          result[i] = sample(
            x,
            sizes[i]
          ) %>% 
            paste(collapse = " ")
        }
        return(result)
      }
      
      # Store created data
      created_data <- mutilselect_sample()
    }
    
    
    
    if (
      output_data[questions[i,]] %>% 
      na.omit %>% 
      nrow() == 0
    )  {
      output_data[i] <- created_data
    }
  }
  
  return(output_data)
  
}


