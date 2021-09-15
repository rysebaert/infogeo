library(sf)
library(cartography)

commune <- st_read(dsn ="Infogeo_data/Decoupages_territoriaux/Communes", layer ="COM_Metropole_Grand_Paris", quiet=TRUE)
dep <- st_read(dsn ="Infogeo_data/Decoupages_territoriaux/Departement", layer ="Depatements_Metropole_Grand_Paris", quiet=TRUE)


typoLayer(x = commune, var = "EPT_LIBEPT",
           col = paste0(carto.pal(pal1 = "multi.pal", n1 = 12),"50"),
           border = "grey40",
           legend.pos = "topleft",
           legend.values.order = sort(unique(commune$EPT_LIBEPT)),
           legend.title.txt = "Établissement public territorial")
plot(st_geometry(dep), add=TRUE)          



biblio <- st_read(dsn ="Infogeo_data/Equipements/carte-des-bibliotheques", layer ="carte-des-bibliotheques", quiet=TRUE)
retraite <- st_read(dsn ="Infogeo_data/Equipements/etablissements_et_services_pour_personnes_agees", layer ="etablissements_et_services_pour_personnes_agees", quiet=TRUE)
conserva <- st_read(dsn ="Infogeo_data/Equipements/les-conservatoires-et-ecoles-de-musique-en-ile-de-france", layer ="les-conservatoires-et-ecoles-de-musique-en-ile-de-france", quiet=TRUE)
mission <- st_read(dsn ="Infogeo_data/Equipements/missions-locales", layer ="missions-locales", quiet=TRUE)
cine <- st_read(dsn ="Infogeo_data/Equipements/salle_de_cinema_ile-de-france", layer ="etablissements-cinematographiques", quiet=TRUE)

biblio <-st_transform(biblio, crs=st_crs(commune))
retraite <-st_transform(retraite, crs=st_crs(commune))
conserva <-st_transform(conserva, crs=st_crs(commune))
mission <-st_transform(mission, crs=st_crs(commune))
cine <-st_transform(cine, crs=st_crs(commune))

par(mfrow=c(2,3))
plot(st_geometry(commune), lwd=0.5, col="grey76")
plot(st_geometry(biblio), add=TRUE, pch = 16, cex=0.4 , col="red" )
title("Biblio")
plot(st_geometry(commune), lwd=0.5, col="grey76")
plot(st_geometry(retraite), add=TRUE, pch = 16, cex=0.4 , col="blue" )
title("retraite")
plot(st_geometry(commune), lwd=0.5, col="grey76")
plot(st_geometry(conserva), add=TRUE, pch = 16, cex=0.4 , col="green" )
title("Conservatoire")
plot(st_geometry(commune), lwd=0.5, col="grey76")
plot(st_geometry(mission), add=TRUE, pch = 16, cex=0.4 , col="yellow" )
title("mission")
plot(st_geometry(commune), lwd=0.5, col="grey76")
plot(st_geometry(cine), add=TRUE, pch = 16, cex=0.4 , col="purple" )
title("cinéma")
