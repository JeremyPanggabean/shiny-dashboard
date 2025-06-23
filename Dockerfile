# Use the official R base image
FROM rocker/r-base:4.3.2

# Set the working directory
WORKDIR /app

# Install system dependencies
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
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'plotly', 'dplyr', 'lubridate', 'ggplot2'), repos='https://cran.rstudio.com/')"

# Copy the Shiny app to the container
COPY app.R /app/

# Expose the port that Shiny will run on
EXPOSE 3838

# Command to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]