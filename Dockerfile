# Shiny on Cloud Run
FROM rocker/r-ver:4.4.1

# System libraries the BigQuery and HTTP packages link against
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev libuv1-dev \
 && rm -rf /var/lib/apt/lists/*

# Install current package versions (prebuilt Linux binaries from Posit
# Package Manager). The base image pins CRAN to an older snapshot, which
# mixed old and new shiny/bslib versions and broke page_navbar().
RUN . /etc/os-release && install2.r --error \
    -r "https://packagemanager.posit.co/cran/__linux__/${VERSION_CODENAME}/latest" \
    shiny bslib bigrquery gargle dplyr tidyr ggplot2 scales DT \
 && Rscript -e 'cat("shiny", as.character(packageVersion("shiny")), "| bslib", as.character(packageVersion("bslib")), "\n")'

# Fail the BUILD (not the deploy) if any package can't load, e.g. a
# missing system library. Errors show up in the Cloud Build log.
RUN Rscript -e 'for (p in c("shiny","bslib","bigrquery","gargle","dplyr","tidyr","ggplot2","scales","DT")) library(p, character.only = TRUE); cat("All packages load\n")'

WORKDIR /app
COPY app.R /app/app.R

# Run the app directly (not via Shiny Server) so Cloud Run's environment
# variables reach R, and listen on the port Cloud Run provides.
EXPOSE 8080
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', '8080')))"]