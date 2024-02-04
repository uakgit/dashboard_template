packages <- c(
  "dplyr", "DT", "lubridate", "shinyjs", "data.table", "flexdashboard", 
  "forcats", "ggplot2", "rmarkdown", "shinydashboard", "sjPlot", 
  "tibble", "timevis", "writexl", "shiny", "knitr", "emojifont", 
  "base", "httr", "janitor", "readxl", "stringr", "bayesboot", 
  "cowplot", "crayon", "ddpcr", "ggmap", "ggpubr", "ggtext", "grid", 
  "gridExtra", "Hmisc", "jsonlite", "lazyeval", "pdp", "quanteda", 
  "quanteda.textplots", "RColorBrewer", "rlang", "scales", "splitstackshape", 
  "stopwords", "stringi", "textstem", "tidyr", "tidyselect", "tidytext", 
  "tm", "wordcloud", "anytime", "mongolite", "dygraphs", "leaflet", 
  "plotly", "reshape2", "shinythemes", "syuzhet"
)


# # Install packages if not already installed
suppressPackageStartupMessages(
  invisible(
    newPkgs <- packages[!(packages %in%
                            installed.packages()[,"Package"])]))

suppressPackageStartupMessages(
  invisible(
    if(
      length(newPkgs) > 0
    ){ install.packages(
      newPkgs
    )
    }
  )
)


# # Loads packages silently
suppressPackageStartupMessages(
  invisible(
    lapply(packages, library, character.only = TRUE)
  )
)