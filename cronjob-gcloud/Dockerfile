FROM rocker/r-ver:4.1.1

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libxml2-dev \
  libcurl4-openssl-dev \
  libssl-dev

RUN R -e "install.packages(c('httr', 'jsonlite', 'dplyr', 'janitor', 'stringr', 'anytime', 'mongolite'))"

# Copy your R scripts into the Docker image
COPY update_mongodb.R /app/ 

# Set the working directory
WORKDIR /app

# Run the R scripts
CMD Rscript update_mongodb.R && echo "update_mongodb.R ran successfully" || echo "update_mongodb.R encountered an error" 