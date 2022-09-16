# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# EXTRACTION GRILLE INSEE 200M
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  

# La grille Insee brute mesure 2,2 Go. Ce script extrait les tuiles d'IDF et
# retient les données pertinentes pour les exercices afin de réduire la taille
# du jeu de données <100Mo (limite sur gitHub).

library(tidyverse)
library(sf)

# Chemin d'accès vers le dossier local à changer pour faire tourner ce script
# sur un autre ordinateur
racineGit = "/home/maxime/Documents/Cours/2021-22 MECI SIG et Carto/infogeo/"
setwd(paste0(racineGit, "data/Infogeo_data/Donnees_INSEE/Grille 200m/"))

grille = read_sf("Filosofi2015_carreaux_200m_metropole.shp")

# On récupère les contours du Grand Paris (la couche s'appelle "dépatements" ^^)
gdParis = read_sf(paste0(racineGit, "data/Infogeo_data/Decoupages_territoriaux/Departement/Depatements_Metropole_Grand_Paris.shp"))

# On vérifie que les CRS sont compatibles
if (st_crs(grille) != st_crs(gdParis)) { stop("CRS incompatibles !") }

# Quels carreaux intersectent avec le Grand Paris ?
grille$gdParis = lengths(st_intersects(grille, gdParis)) > 0

# On ne retient que ceux-là
grille = filter(grille, gdParis == TRUE) %>% select(-gdParis)

# On calcule le revenu moyen (somme des niveaux de vie winsorisés / nombre d'ind)
grille$RevenuMoy = grille$Ind_snv / grille$Ind
grille$SurfMoy = grille$Men_surf / grille$Men

# On enlève les colonnes qu'on n'utilisera pas
grille = select(grille, -Ind_snv, -Men_surf)

# Enregistrement de la grille
write_sf(grille, "grille200m.shp")
