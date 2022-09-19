########################################################
#  EXTRACTION CONTOURS COMMUNAUX, SEINE, PARCS ET CIMETIERES
#                    OSM
#  R. YSEBAERT, JUILLET 2022
#######################################################
library(osmdata)
library(sf)
library(mapsf)

com <- st_read("com.gpkg")
com <- com[com$LIB_EPCI == "Métropole du Grand Paris",]
com <- st_transform(com, 4326)
bb <- st_bbox(com)


head(com)

## Téléchargement contours communaux (OSM)
bb <- opq(bbox = bb)


q <- add_osm_feature(opq = bb, key = 'man_made', value = "beehive")
q <- osmdata_sf(q)
ruche1 <- q$osm_points[,c("osm_id", "man_made")]
ruche2 <- q$osm_polygons[,c("osm_id", "man_made")]
ruche2 <- st_centroid(ruche2)
ruche <- rbind(ruche1, ruche2)
ruche$osm_tag <- "man_made = beehive"
ruche$date <- "2022_09_07"
ruche$man_made <- NULL

q <- add_osm_feature(opq = bb, key = 'craft', value = "beekeeper")
q <- osmdata_sf(q)
ruche1 <- q$osm_points[,"osm_id"]
ruche2 <- q$osm_polygons[,"osm_id"]
ruche2 <- st_centroid(ruche2)
ruche3 <- rbind(ruche1, ruche2)
ruche3$osm_tag <- "craft = beekeeper"
ruche3$date <- "2022_09_07"
ruche <- rbind(ruche, ruche3)
st_write(ruche, "export/ruche_osm.shp",  layer_options = "ENCODING=UTF-8", delete_layer = TRUE)

q <- add_osm_feature(opq = bb, key = 'man_made', value = "insect_hotel")
q <- osmdata_sf(q)
insect <- q$osm_points[,"osm_id"]
insect$tag <- "man_made = insect_hotel"
insect$date <- "2022_09_07"
insect <- insect[,c("osm_id", "tag", "date")]
st_write(insect, "export/insect_osm.shp",  layer_options = "ENCODING=UTF-8", delete_layer = TRUE)


q <- add_osm_feature(opq = bb, key = 'leisure', value = "bird_hide")
q <- osmdata_sf(q)
bird1 <- q$osm_points[,"osm_id"]
bird2 <- q$osm_polygons[,"osm_id"]
bird2 <- st_centroid(bird2)
bird <- rbind(bird1, bird2)
bird$tag <- "leisure = bird_hide"
bird$date <- "2022_09_07"

st_write(bird, "export/bird_osm.shp",  layer_options = "ENCODING=UTF-8", delete_layer = TRUE)

mf_map(ruche, add = TRUE)
mf_map(insect, col = "red", add = TRUE)
mf_map(bird, col = "blue", add = TRUE)



# extract Parks and Cemetaries
q3 <- add_osm_feature(opq = bb, key = 'leisure', value = "park")
res3 <- osmdata_sf(q3)
parc1 <- res3$osm_polygons
parc2 <- res3$osm_multipolygons
parc1 <- parc1[,c("osm_id", "name")]
parc2 <- parc2[,c("osm_id", "name")]
parc2 <- st_cast(parc2, "POLYGON")

parc <- rbind(parc1, parc2)
parc <- st_transform(parc, 2154)
parc$tag <- "leisure = park"


q4 <- add_osm_feature(opq = bb, key = 'landuse', value = "cemetery")
res4 <- osmdata_sf(q4)
parc3 <- res4$osm_polygons
parc3 <- st_transform(parc3, 2154)
parc3 <- parc3[,c("osm_id", "name")]
parc3$tag <- "landuse = cemetery"
parc <- rbind(parc, parc3)
parc$area <- st_area(parc)
parc <- st_transform(parc, 4326)
parc$date <- "2022_09_07"

st_write(parc, "export/parc_osm.shp",  layer_options = "ENCODING=UTF-8", delete_layer = TRUE)


# Main roads
q <- add_osm_feature(opq = bb, key = 'highway', value = "primary")
q <- osmdata_sf(q)
line1 <- q$osm_lines
line1 <- line1[,c("osm_id", "name")]
line1$tag <- "highway = primary"
line1$date <- "2022_09_07"

q <- add_osm_feature(opq = bb, key = 'highway', value = "motorway")
q <- osmdata_sf(q)
line2 <- q$osm_lines
line2 <- line2[,c("osm_id", "name")]
line2$tag <- "highway = motorway"
line2$date <- "2022_09_07"

q <- add_osm_feature(opq = bb, key = 'highway', value = "secondary")
q <- osmdata_sf(q)
line3 <- q$osm_lines
line3 <- line3[,c("osm_id", "name")]
line3$tag <- "highway = secondary"
line3$date <- "2022_09_07"

line <- rbind(line1, line2, line3)

st_write(line, "export/highway_osm.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)


# Railways
q <- add_osm_feature(opq = bb, key = 'railway', value = "rail")
q <- osmdata_sf(q)
line1 <- q$osm_lines
line1 <- line1[,c("osm_id", "name")]
line1$tag <- "railway = rail"
line1$date <- "2022_09_07"
st_write(line1, "export/railway_osm.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)

#
q <- add_osm_feature(opq = bb, key = 'landuse', value = "flowerbed")
q <- osmdata_sf(q)
flower <- q$osm_polygons
flower <- flower[,c("osm_id")]
flower$tag <- "landuse = flowerbed"
flower$date <- "2022_09_07"
st_write(flower, "export/landuse_flower_osm.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)

q <- add_osm_feature(opq = bb, key = 'landuse', value = "forest")
q <- osmdata_sf(q)
forest <- q$osm_polygons
forest <- forest[,c("osm_id", "name")]
forest$tag <- "landuse = forest"
forest$date <- "2022_09_07"
st_write(forest, "export/landuse_forest_osm.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)

q <- add_osm_feature(opq = bb, key = 'landuse', value = "orchard")
q <- osmdata_sf(q)
orchard <- q$osm_polygons
orchard <- orchard[,c("osm_id")]
orchard$tag <- "landuse = orchard"
orchard$date <- "2022_09_07"
st_write(orchard, "export/landuse_orchard_osm.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)
