# https://hub.docker.com/r/rocker/tidyverse/tags
FROM rocker/geospatial:4.2.3
MAINTAINER Jonathan Callahan <jonathan@mazamascience.com>

# Install from CRAN to get all dependencies
RUN install2.r --error -r "https://cran.rstudio.com" \
    AirMonitor

# Uupdate to latest version from Github
RUN installGithub.r \
    MazamaScience/AirMonitor@0.3.10

