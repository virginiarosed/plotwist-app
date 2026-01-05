FROM rocker/shiny:4.3.2

# Install PostgreSQL support
RUN apt-get update && apt-get install -y libpq-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinyjs', 'DBI', 'RPostgres', 'dplyr', 'lubridate', 'httr', 'jsonlite', 'plotly', 'shinyWidgets', 'bcrypt'), repos='https://cloud.r-project.org/')"

# Copy ALL files
COPY . .

EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=8080)"]