FROM rocker/shiny:4.3.2

# Install PostgreSQL dependencies and R packages
RUN apt-get update && apt-get install -y libpq-dev && \
    R -e "install.packages(c('shiny', 'shinyjs', 'DBI', 'RPostgres', 'dplyr', 'lubridate', 'httr', 'jsonlite', 'plotly', 'shinyWidgets', 'bcrypt'), repos='https://cloud.r-project.org/')"

# Copy app file
COPY app.R /app.R

# Expose port
EXPOSE 8080

# Run app
CMD ["R", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=8080)"]