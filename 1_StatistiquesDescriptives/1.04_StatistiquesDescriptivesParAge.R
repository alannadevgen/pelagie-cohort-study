# Set-up ------------------------------------------------------------------------------------

# working directory
setwd("../1_StatistiquesDescriptives")
setwd("1_StatistiquesDescriptives")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# packages
library(ggplot2)
library(plyr)

# Summary ----------------------------------------------------------------------------------------

# résume statistique par age
for (an in 1:13){
  cat("\n\n----------------------------------", an - 1, "à", an, "ans ----------------------------------\n\n")
  sous_base <- croiss %>% filter(age_an >= an - 1 & age_an < an)
  print(summary(sous_base, na.rm = T))
}

# ecart-type poids/taille
for (an in 1:13){
  cat("\n\n----------------------------------", an - 1, "à", an, "ans ----------------------------------\n\n")
  sous_base <- croiss %>% filter(age_an >= an - 1 & age_an < an)
  cat("Std taille : ", round(sd(sous_base$taille, na.rm = T), digits = 2), "\n")
  cat("Std poids : ", round(sd(sous_base$poids, na.rm = T), digits = 2))
}
