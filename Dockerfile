# Use the official rocker geospatial image as a base
# Includes R, RStudio, tidyverse, GDAL, PROJ, GEOS, and key spatial packages
FROM rocker/geospatial:latest

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive

# Install additional system dependencies if needed (e.g., for specialized R packages)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libnetcdf-dev \
    libnode-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install renv using install2.r
RUN install2.r --error renv

RUN mkdir /home/rstudio/project

# WORKDIR /home/rstudio/project
# COPY renv.lock renv.lock

# # Restore the project library
# RUN R -e 'renv::restore()'


# # Install specific R packages for spatial analysis
# RUN install2.r --error \
#     renv \
#     sf \
#     terra \
#     raster \
#     stars \
#     leaflet \
#     mapview \
#     rayshader \
#     spatial \
#     spatstat \
#     gstat \
#     tidyverse \
#     && rm -rf /tmp/downloaded_packages

# Default RStudio username/password is set by the rocker base image
# (usually rstudio / password in the console)