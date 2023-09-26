
# Transformation des fichiers htlm en pdf pour Moodle

## Package
install.packages("renderthis")

# install.packages("remotes")
remotes::install_github("jhelvy/renderthis")

library(renderthis)

remotes::install_github('rstudio/chromote') #nécessaire?

renderthis::to_pdf("IG_1_Introduction_cours.html")
renderthis::to_pdf("IG_2_Defintion_methodes_outils.html")
renderthis::to_pdf("IG_3_Coordonnees_Projections.html")
renderthis::to_pdf("IG_4_Introduction_SIG.html")

