# Use the official R base image
FROM rocker/r-base:4.3.2

# Set the working directory
WORKDIR /app

# Install system dependencies (including PostgreSQL client libraries)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages (including database packages)
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'plotly', 'dplyr', 'lubridate', 'ggplot2', 'DBI', 'RPostgres'), repos='https://cran.rstudio.com/')"

# Copy the Shiny app to the container
COPY app.R /app/

# Use Railway's dynamic port (Railway provides PORT environment variable)
EXPOSE $PORT

# Command to run the Shiny app with Railway's dynamic port
CMD ["sh", "-c", "R -e \"shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', '3838')))\""]
