# Shiny on Cloud Run
FROM rocker/r-ver:4.4.1

# System libraries for BigQuery/HTTP packages and for ragg/systemfonts/textshaping
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev libuv1-dev \
    libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
    libpng-dev libjpeg-dev libtiff-dev libwebp-dev \
 && rm -rf /var/lib/apt/lists/*

# Current package versions as prebuilt Linux binaries (Posit Package Manager).
# The base image pins CRAN to an old snapshot, which mixes shiny/bslib versions.
RUN . /etc/os-release && install2.r --error \
    -r "https://packagemanager.posit.co/cran/__linux__/${VERSION_CODENAME}/latest" \
    shiny bslib bigrquery gargle dplyr tidyr ggplot2 scales DT systemfonts ragg \
 && Rscript -e 'cat("shiny", as.character(packageVersion("shiny")), "| bslib", as.character(packageVersion("bslib")), "\n")'

# Fail the BUILD (not the deploy) if any package can't load.
RUN Rscript -e 'for (p in c("shiny","bslib","bigrquery","gargle","dplyr","tidyr","ggplot2","scales","DT","systemfonts","ragg")) library(p, character.only = TRUE); cat("All packages load\n")'

WORKDIR /app
COPY app.R /app/app.R
COPY www /app/www

# Run the app directly (not via Shiny Server) so Cloud Run's environment
# variables reach R, and listen on the port Cloud Run provides.
EXPOSE 8080
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', '8080')))"]