library(sf)

st_layers("../Decoupages_territoriaux/ADE_4-0_GPKG_LAMB93_FXX-ED2025-09-16.gpkg")

epci <- st_read("../Decoupages_territoriaux/ADE_4-0_GPKG_LAMB93_FXX-ED2025-09-16.gpkg",
               layer = "epci")

epci <- epci[epci$nature == "Etablissement public territorial" |
               epci$nom_officiel == "Métropole du Grand Paris",]
epci <- epci[, c(1,3,4,7,8)]
st_write(epci, "../Decoupages_territoriaux/EPCI.shp")

com <- st_read("../Decoupages_territoriaux/ADE_4-0_GPKG_LAMB93_FXX-ED2025-09-16.gpkg",
               layer = "commune")
com <- com[substr(com$code_insee,1,2) %in% c("75", "77", "78", "91", "92", "93", "94", "95") ,]

dep <- st_sf(
  DEP = tapply(com$code_insee_du_departement, com$code_insee_du_departement, head, 1),
  geometry = tapply(st_geometry(com), com$code_insee_du_departement, st_union),
  crs = st_crs(com)
)

st_write(dep, "../Decoupages_territoriaux/DEPARTEMENTS.shp")


com$codes_siren_des_epci <- substr(com$codes_siren_des_epci,1,9)
com <- com[com$codes_siren_des_epci %in% epci$code_siren ,]
com <- com[, c(3:5,14)]
colnames(com)[1] <- "nom"
st_write(com, "../Decoupages_territoriaux/COM.shp")

iris <- st_read("../Decoupages_territoriaux/iris.gpkg")
iris <- iris[iris$code_insee %in% com$code_insee ,]

iris <- iris[, c(1,3,5:7)]
st_write(iris, "../Decoupages_territoriaux/IRIS.shp")








library(osmdata)
library(tidygeocoder)


mgp <- st_read("../Decoupages_territoriaux/EPCI.shp")
mgp_bbox <- st_union(mgp) |>
  st_transform(crs = 4326) |>
  st_bbox()

q <- opq(bbox = mgp_bbox, osm_types = "node") # seulement les points

# Extraction des hôtels à insectes
insect <- add_osm_feature(opq = q,
                          key = "leisure",
                          value = "bird_hide")
insect <- osmdata_sf(insect)
insect <- insect$osm_points

st_write(insect, "insectes.shp")

# Extraction des ruches
ruches1 <- add_osm_feature(opq = q,
                          key = "man_made",
                          value = "beehive")
ruches1 <- osmdata_sf(ruches1)
ruches1 <- ruches1$osm_points

ruches2 <- add_osm_feature(opq = q,
                           key = "craft",
                           value = "beekeeper")
ruches2 <- osmdata_sf(ruches2)
ruches2 <- ruches2$osm_points

ruches <- rbind(ruches1[, c(1,2)], ruches2[, c(1,2)])

st_write(ruches, "ruches.shp")


# Extraction des pointsd'écoute à oiseaux
birds <- add_osm_feature(opq = q,
                         key = "leisure",
                         value = "bird_hide")
birds <- osmdata_sf(birds)
birds <- birds$osm_points

st_write(birds, "birds.shp")
