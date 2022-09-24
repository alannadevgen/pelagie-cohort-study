# Set-up ------------------------------------------------------------------------------------

# working directory
setwd("../1_StatistiquesDescriptives")
setwd("1_StatistiquesDescriptives")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# packages
library(dplyr)

# Calcul du nombre de mesures pour la taille ---------------------------------------------

# filles
for (an in 1:13){
  nb <- croiss_taille %>% filter(sexe == "F") %>% filter(age_an >= an-1 & age_an < an) %>% nrow()
  if (an == 1){
    print(paste0("De ", an-1, " a ", an, " an : ", nb, " mesures de taille pour les filles"))
  } else {
    print(paste0("De ", an-1, " a ", an, " ans : ", nb, " mesures de taille pour les filles")) 
  }
}

# garcons
for (an in 1:13){
  nb <- croiss_taille %>% filter(sexe == "M") %>% filter(age_an >= an-1 & age_an < an) %>% nrow()
  if (an == 1){
    print(paste0("De ", an-1, " a ", an, " an : ", nb, " mesures de taille pour les garcons"))
  } else {
    print(paste0("De ", an-1, " a ", an, " ans : ", nb, " mesures de taille pour les garcons")) 
  }
}

# Calcul du nombre de mesures pour le poids ---------------------------------------------

# filles
for (an in 1:13){
  nb <- croiss_poids %>% filter(sexe == "F") %>% filter(age_an >= an-1 & age_an < an) %>% nrow()
  if (an == 1){
    print(paste0("De ", an-1, " a ", an, " an : ", nb, " mesures de poids pour les filles"))
  } else {
    print(paste0("De ", an-1, " a ", an, " ans : ", nb, " mesures de poids pour les filles")) 
  }
}

# garcons
for (an in 1:13){
  nb <- croiss_poids %>% filter(sexe == "M") %>% filter(age_an >= an-1 & age_an < an) %>% nrow()
  if (an == 1){
    print(paste0("De ", an-1, " a ", an, " an : ", nb, " mesures de poids pour les garcons"))
  } else {
    print(paste0("De ", an-1, " a ", an, " ans : ", nb, " mesures de poids pour les garcons")) 
  }
}
