# Shiny on Cloud Run
FROM rocker/r-ver:4.4.1

# System libraries the BigQuery and HTTP packages link against
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
 && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled \
    shiny bslib bigrquery gargle dplyr tidyr ggplot2 scales DT

WORKDIR /app
COPY app.R /app/app.R

# Run the app directly (not via Shiny Server) so Cloud Run's environment
# variables reach R, and listen on the port Cloud Run provides.
EXPOSE 8080
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', '8080')))"]
