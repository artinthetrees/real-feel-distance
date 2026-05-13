**Prompt**: dockerfile to use docker to do spatial analysis using rstudio gdal

Using Docker for R-based spatial analysis is best accomplished by leveraging the rocker project, specifically the rocker/geospatial image. This image comes pre-configured with RStudio, GDAL, GEOS, PROJ, and many common spatial R packages (sf, raster, terra).

1. Dockerfile to Setup Spatial Analysis Environment. This Dockerfile extends rocker/geospatial to include additional commonly used spatial libraries and set up RStudio.

```
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
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install specific R packages for spatial analysis
RUN install2.r --error \
    sf \
    terra \
    raster \
    stars \
    leaflet \
    mapview \
    rayshader \
    spatial \
    spatstat \
    gstat \
    tidyverse \
    && rm -rf /tmp/downloaded_packages

# Default RStudio username/password is set by the rocker base image
# (usually rstudio / password in the console)
```
2. How to Build and Run

- Build the Image:
    - Save the code above as Dockerfile. 
    - Run the following command in the same directory:
    ```docker build -t rstudio-spatial .```
- Run the Container:
    - Run the container to start RStudio Server, mapping your current directory (data folder) to the container:
    ```docker run --rm -p 8787:8787 -e PASSWORD=yourpassword -v $(pwd):/home/rstudio/data rstudio-spatial```
    **ART NOTE**: replace $(pwd) with %cd%

3. Access RStudio
- Open your web browser and navigate to http://localhost:8787.Login: 
- Username is rstudio, Password is yourpassword (whatever password you set up in run command).
- Your local project files will be accessible in the /home/rstudio/data folder within RStudio.

4. Also check out: https://rocker-project.org/images/versioned/rstudio.html#how-to-use
