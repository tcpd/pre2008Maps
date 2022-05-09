FROM rocker/shiny:latest
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    gdal-bin \
    libgdal-dev \
    libudunits2-dev

RUN R -e 'install.packages(c(\
              "shiny", \
              "data.table", \
              "dplyr", \
              "plotly", \
              "rgdal", \
              "leaflet", \
              "randomcoloR", \
              "htmltools", \
              "mapview", \
              "DT" \
            ) \
          )'

COPY ./app.R  /srv/shiny-server/
RUN mkdir -p /srv/shiny-server/data
RUN mkdir -p /srv/shiny-server/Maps
CMD ["/usr/bin/shiny-server"]
