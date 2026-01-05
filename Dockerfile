FROM rocker/shiny:4.3.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libssl-dev \
    libcurl4-openssl-dev \
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
RUN R -e "install.packages(c('shiny', 'shinyjs', 'DBI', 'RPostgres', 'dplyr', 'lubridate', 'httr', 'jsonlite', 'plotly', 'shinyWidgets', 'bcrypt'), repos='https://cloud.r-project.org/', dependencies=TRUE)"

# Set working directory
WORKDIR /app

# Copy app files
COPY app.R /app/
COPY www /app/www

# Expose port
EXPOSE 8080

# Run app
CMD ["R", "-e", "shiny::runApp('/app/app.R', host='0.0.0.0', port=8080)"]