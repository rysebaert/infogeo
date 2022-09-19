#########
# Prépare IRIS and Geo
####

library(sf)
library(readxl)
library(mapsf)
library(openxlsx)

# Une fonction pour exporter les données et métadonnées au format xlsx
export_xls <- function(x_iris, x_com, x_meta, file){
  # IRIS
  OUT <- createWorkbook()
  addWorksheet(OUT, "IRIS")
  addWorksheet(OUT, "META")
  writeData(OUT, sheet = "IRIS", x = x_iris)
  writeData(OUT, sheet = "META", x = x_meta)
  saveWorkbook(OUT, paste0("export/stats/IRIS/", file,".xlsx"))

  # COMMUNES
  OUT <- createWorkbook()
  addWorksheet(OUT, "COMMUNES")
  addWorksheet(OUT, "META")
  writeData(OUT, sheet = "COMMUNES", x = x_com)
  writeData(OUT, sheet = "META", x = x_meta)
  saveWorkbook(OUT, paste0("export/stats/COMMUNES/", file, ".xlsx"))
}


com <- st_read("com.gpkg")
com <- com[com$LIB_EPCI == "Métropole du Grand Paris",]
com <- st_set_geometry(com, NULL)
lib <- unique(com$INSEE_COM)

epci <- aggregate(com[,c("INSEE_COM", "LIB_EPCI_SUB")],
                  by = list(com$INSEE_COM),
                  FUN = head, 1)

# Départements
iris <- st_read("CONTOURS-IRIS_2-1__SHP__FRA_2022-01-01/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2022-06-00180/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2022/CONTOURS-IRIS.shp")
dep <- iris
dep$DEP <- substr(dep$INSEE_COM, 1, 2)
dep <- aggregate(dep[,"DEP"],
                 by = list(dep$DEP),
                 FUN = head, 1)

dep <- dep[,2]
dep <- dep[dep$DEP %in% c("75", "91", "92", "93", "94", "77", "78", "95"),]
st_write(dep, dsn = "export/Territories/DEPARTEMENT.shp", delete_layer = TRUE)


# IRIS MGP
iris <- iris[iris$INSEE_COM %in% lib,]
iris <- merge(iris, epci[,c(2:3)], by = "INSEE_COM", all.x = TRUE)
colnames(iris)[7] <- "LIB_EPCI"
st_write(iris, dsn = "export/Territories/IRIS.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)

# Communes
com <- aggregate(iris[,c("INSEE_COM", "NOM_COM")],
                 by = list(iris$INSEE_COM),
                 FUN = head, 1)
com <- com[,c(2,3)]
com <- merge(com, epci[,c(2:3)], by = "INSEE_COM", all.x = TRUE)
st_write(com, dsn = "export/Territories/COMMUNES.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)


# EPCI
epci <- aggregate(iris[,"LIB_EPCI"],
                 by = list(iris$LIB_EPCI),
                 FUN = head, 1)
st_write(epci, dsn = "export/Territories/EPCI.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)



# Stats ----
## Démo ----
df <- read_xlsx("base-ic-evol-struct-pop-2018.xlsx", sheet = "IRIS", skip = 5)
df <- data.frame(df[df$COM %in% lib,])

var <- c("IRIS", "LIBIRIS",  "COM", "LIBCOM",  "P18_POP", "P18_POP0002", "P18_POP0305",
         "P18_POP0610", "P18_POP1117", "P18_POP1824", "P18_POP2539", "P18_POP4054",
         "P18_POP5564", "P18_POP6579","P18_POP80P")

iris_sel <- df[,var]
iris_sel <- merge(iris_sel, st_set_geometry(com[,c(1,3)], NULL),  by.x = "COM",
                  by.y = "INSEE_COM", all.x = TRUE)

iris_sel <- iris_sel[,c(c(1:4), ncol(iris_sel), c(5:(ncol(iris_sel)-1)))]

com_sel <- aggregate(iris_sel[,c(6:length(iris_sel))],
                     by = list("INSEE_COM" = iris_sel$COM),
                     FUN = sum)

com_sel <- merge(com_sel, st_set_geometry(com, NULL), by = "INSEE_COM", all.x = TRUE)
com_sel <- com_sel[,c(1, c((ncol(com_sel) -1) : ncol(com_sel)), c(2:(ncol(com_sel)-2)))]

meta1 <- data.frame(read_xlsx("base-ic-evol-struct-pop-2018.xlsx", sheet = "Variables", skip = 1))
meta <- data.frame(read_xlsx("base-ic-evol-struct-pop-2018.xlsx", sheet = "Variables", skip = 5))
meta_sel <- meta[meta$VAR_ID %in% var,]
meta_sel$DATE <- meta1[1,1]
meta_sel$SOURCE <- meta1[2,1]
export_xls(x_iris = iris_sel, x_com = com_sel, x_meta = meta_sel, file = "demo")


## Socio éco ----
# Activité des résidents
var <- c("IRIS", "LIBIRIS",  "COM", "LIBCOM",  "C18_POP15P_CS1", "C18_POP15P_CS2",
         "C18_POP15P_CS3", "C18_POP15P_CS4", "C18_POP15P_CS5", "C18_POP15P_CS6",
         "C18_POP15P_CS7", "C18_POP15P_CS8")

iris_sel <- df[,var]
iris_sel <- merge(iris_sel, st_set_geometry(com[,c(1,3)], NULL),  by.x = "COM",
                  by.y = "INSEE_COM", all.x = TRUE)
iris_sel <- iris_sel[,c(c(1:4), ncol(iris_sel), c(5:(ncol(iris_sel)-1)))]

meta_sel <- meta[meta$VAR_ID %in% var,]
meta_sel$DATE <- meta1[1,1]
meta_sel$SOURCE <- meta1[2,1]


df <- read_xlsx("base-ic-activite-residents-2018.xlsx", sheet = "IRIS",
                skip = 5)
df <- data.frame(df[df$COM %in% lib,])
var <- c("IRIS", "P18_POP1564", "P18_POP1524", "P18_POP2554", "P18_POP5564",
         "P18_ACT1564", "P18_ACT1524", "P18_ACT2554", "P18_ACT5564",
         "P18_CHOM1564", "P18_CHOM1524", "P18_CHOM2554", "P18_CHOM5564",
         "P18_ETUD1564", "P18_RETR1564", "P18_SAL15P", "P18_SAL15P_CDD",
         "P18_SAL15P_INTERIM", "P18_SAL15P_EMPAID", "P18_SAL15P_APPR", "P18_NSAL15P_INDEP",
         "P18_NSAL15P_EMPLOY")

df <- df[,var]
iris_sel <- merge(iris_sel, df,  by = "IRIS", all.x = TRUE)

meta1 <- data.frame(read_xlsx("base-ic-activite-residents-2018.xlsx", sheet = "Variables", skip = 1))
meta <- data.frame(read_xlsx("base-ic-activite-residents-2018.xlsx", sheet = "Variables", skip = 5))
meta_tmp <- meta[meta$VAR_ID %in% var,]
meta_tmp$DATE <- meta1[1,1]
meta_tmp$SOURCE <- meta1[2,1]
meta_tmp <- meta_tmp[-1,]
meta_sel <- rbind(meta_sel, meta_tmp)


# Revenu https://www.insee.fr/fr/statistiques/6049648
df <- read_xlsx("BASE_TD_FILO_DEC_IRIS_2019.xlsx", sheet = "IRIS_DEC",
                skip = 5)
df <- data.frame(df[df$COM %in% lib,])
var <- c("IRIS", "DEC_TP6019", "DEC_Q119", "DEC_MED19", "DEC_Q319")
df <- df[,var]
iris_sel <- merge(iris_sel, df,  by = "IRIS", all.x = TRUE)

meta1 <- data.frame(read_xlsx("BASE_TD_FILO_DEC_IRIS_2019.xlsx", sheet = "Variables", skip = 1))
meta <- data.frame(read_xlsx("BASE_TD_FILO_DEC_IRIS_2019.xlsx", sheet = "Variables", skip = 5))
meta_tmp <- meta[meta$VAR_ID %in% var,]
meta_tmp$DATE <- meta1[1,1]
meta_tmp$SOURCE <- meta1[2,1]
meta_tmp <- meta_tmp[-1,]
meta_sel <- rbind(meta_sel, meta_tmp)

# Agrégation communes
com_sel <- aggregate(iris_sel[,c(6: c(length(iris_sel)-4))],
                     by = list("INSEE_COM" = iris_sel$COM),
                     FUN = sum)

df <- read_xlsx("FILO2019_DEC_COM.xlsx", sheet = "ENSEMBLE", skip = 5)
df <- df[,c("CODGEO", "Q119", "Q219", "Q319")]
df2 <- read_xlsx("FILO2019_DEC_Pauvres_COM.xlsx", sheet = "ENSEMBLE", skip = 5)
df2 <- df2[,c("CODGEO", "TP6019")]
df <- merge(df, df2, by = "CODGEO")
colnames(df) <- c("CODGEO", "DEC_Q119", "DEC_MED19", "DEC_Q319", "DEC_TP6019")
com_sel <- merge(com_sel, df, by.x = "INSEE_COM", by.y = "CODGEO", all.x = TRUE)
com_sel <- merge(com_sel, st_set_geometry(com, NULL), by = "INSEE_COM", all.x = TRUE)
com_sel <- com_sel[,c(1, c((ncol(com_sel) -1) : ncol(com_sel)), c(2:(ncol(com_sel)-2)))]

export_xls(x_iris = iris_sel, x_com = com_sel, x_meta = meta_sel, file = "socio_eco")



# Equipements ----
bpe <- read.csv("bpe21_ensemble_xy.csv", sep = ";")
bpe <- bpe[,c("LAMBERT_X", "LAMBERT_Y", "DCIRIS", "QUALI_IRIS", "TYPEQU")]
bpe <- bpe[bpe$QUALI_IRIS %in% c("1","2"),]


sel_eq <- c("A203", "B101", "B102", "B201", "B202", "B203", "C101", "C102",
            "C103", "C104", "C105", "C201", "C301", "C302", "C303", "C304",
            "C305", "C701", "C702", "D101", "D102", "D103", "D201", "D401",
            "D402", "D403", "F101", "F102", "F103", "F104", "F107", "F109",
            "F111", "F112", "F114", "F117", "F120", "F121", "F203", "F303",
            "F305", "F307", "F312", "G102", "G104")

bpe <- bpe[bpe$TYPEQU %in% sel_eq,]
bpe <- bpe[,c(1:3,5)]

bpe_sf <- st_as_sf(bpe, coords = c("LAMBERT_X", "LAMBERT_Y"), crs = 2154)
metro <- st_union(com)
bbox <- st_as_sfc(st_bbox(metro + c(-5000,-5000,5000,5000), crs = 2154))
st_crs(bbox) <- 2154
head(bbox)

mf_map(bbox)
mf_map(metro, add = TRUE)
inter <- st_intersects(bpe_sf, bbox, sparse = FALSE)
bpe_sf_sel <- bpe_sf[inter, ]
bpe_sf_sel <- bpe_sf_sel[,"TYPEQU"]

st_write(bpe_sf_sel, dsn = "export/BPE.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)


# Agrégation à l'IRIS
bpe <- read.csv("bpe21_ensemble.csv", sep = ";")
bpe <- bpe[bpe$DEPCOM %in% lib,]
bpe <- bpe[,c("DEPCOM", "DCIRIS", "TYPEQU", "QUALI_IRIS", "NB_EQUIP")]
bpe <- bpe[bpe$QUALI_IRIS %in% c("1","2"),]
bpe <- bpe[bpe$TYPEQU %in% sel_eq,]

agg <- aggregate(bpe[,c("NB_EQUIP")], by = list(bpe$DCIRIS, bpe$TYPEQU),
                 FUN = sum)

iris_sel <- iris
for (i in 1:length(sel_eq)){
  tmp <- agg[agg$Group.2 == sel_eq[i],]
  tmp <- tmp[,c(1,3)]
  colnames(tmp)[2] <- sel_eq[i]
  iris_sel <- merge(iris_sel, tmp, by.x = "CODE_IRIS", by.y = "Group.1", all.x = TRUE)
}

iris_sel[is.na(iris_sel)] <- 0
iris_sel <- st_set_geometry(iris_sel, NULL)

meta <- read.csv("Varmod_bpe21_ensemble.csv", sep = ";")
meta <- meta[meta$COD_MOD %in% sel_eq,]
meta$SOURCE <- "INSEE, Base Permanente des équipements. Dénombrement des équipements (commerce, services, santé...) en 2021"
meta$DATE <- "Paru le 08/07/2022"

com_sel <- aggregate(iris_sel[,c(8:length(iris_sel))],
                     by = list("INSEE_COM" = iris_sel$INSEE_COM),
                     FUN = sum)

com_sel <- merge(com_sel, st_set_geometry(com, NULL), by = "INSEE_COM", all.x = TRUE)
com_sel <- com_sel[,c(1, c((ncol(com_sel) -1) : ncol(com_sel)), c(2:(ncol(com_sel)-2)))]
export_xls(x_iris = iris_sel, x_com = com_sel, x_meta = meta, file = "BPE_extrait")
