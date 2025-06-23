# Use rocker/shiny which already has Shiny pre-installed
FROM rocker/shiny:4.3.2

# Set the working directory
WORKDIR /srv/shiny-server

# Install system dependencies (including PostgreSQL client libraries)
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages from binary (faster than source)
RUN install2.r --error --skipinstalled \
    shinydashboard \
    DT \
    plotly \
    dplyr \
    lubridate \
    ggplot2 \
    DBI \
    RPostgres \
    scales

# Copy the Shiny app to the container
COPY app.R /srv/shiny-server/

# Make sure the app is executable
RUN chmod -R 755 /srv/shiny-server/

# Use Railway's dynamic port
EXPOSE $PORT

# Command to run the Shiny app with Railway's dynamic port
CMD ["sh", "-c", "R -e \"shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', '3838')))\""]
