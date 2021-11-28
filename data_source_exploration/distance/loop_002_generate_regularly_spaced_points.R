# generate randomly spaced points on a map
# https://romanabashin.com/how-to-generate-regularly-spaced-points-on-a-map-inside-a-polygon/
# https://stackoverflow.com/questions/25547826/generate-regularly-spaced-points-in-polygon
# https://gis.stackexchange.com/questions/346712/choosing-crs-for-chicago-illinois

# NAD83 UTM Zone 16N (EPSG 26916)


#library(raster)
#library(rgdal)

# p <- 
#   raster::shapefile("C:/Users/Andrea/Desktop/repositories/real-feel-distance/chicago_shp_files/Boundaries - Census Tracts - 2010/geo_export_627832c0-3120-4238-92f1-c6c4c755baa9.shp")[1,]

# p_whole <- 
#   raster::shapefile("C:/Users/Andrea/Desktop/repositories/real-feel-distance/chicago_shp_files/Boundaries - Census Tracts - 2010/geo_export_627832c0-3120-4238-92f1-c6c4c755baa9.shp")
# p <- p_whole[1,]
# p.utm <- sp::spTransform(p, "+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80")

p.utm <- one_tract.utm

r <- raster::raster(p.utm, res=100)
r <- raster::rasterize(p.utm, r)
pts.utm.spdf <- raster::rasterToPoints(r, spatial=TRUE)


pts_lonlat.spdf <- sp::spTransform(pts.utm.spdf, "+proj=longlat +datum=NAD83")
pts_lonlat.array <- raster::coordinates(pts_lonlat.spdf)
pts_lonlat.df <- data.frame(pts_lonlat.array)

# plot(p)
# points(result, pch="+", cex=.5)

# set a crs for the lat/long coordinates
pts_lonlat.sf <- 
  sf::st_as_sf(pts_lonlat.df, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")

# check that the crs was set and check the units (will be null for longlat projection)
sf::st_crs(pts_lonlat.sf)$proj4string
sf::st_crs(pts_lonlat.sf)$units

# convert crs to use utm which measures distance in meters, use zone 16N for chicago
pts_lonlat.sf.utm <- 
  sf::st_transform(pts_lonlat.sf,
                   crs = "+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80") 

# check that the crs was set and check the units (will be meters for utm projection)
sf::st_crs(pts_lonlat.sf.utm)$proj4string
sf::st_crs(pts_lonlat.sf.utm)$units

