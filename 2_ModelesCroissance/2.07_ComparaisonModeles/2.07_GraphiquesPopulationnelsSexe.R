# Description ----------------------------------------------------------------------------

# Graphiques populationnels de tous les modèles

# author : @alannagenin

# Set-up ---------------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")
setwd("2_ModelesCroissance")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")
# --------------------- TAILLE ---------------------
# Jenss Bayley taille
load("RData/Taille/JenssBayleyTailleFilles.RData")
load("RData/Taille/JenssBayleyTailleGarcons.RData")
# Jenss adapte taille
load("RData/Taille/JenssModifieTailleFilles.RData")
load("RData/Taille/JenssModifieTailleGarcons.RData")
# Reed 1er ordre taille
load("RData/Taille/Reed1erOrdreTailleFilles.RData")
load("RData/Taille/Reed1erOrdreTailleGarcons.RData")
# Reed 2nd ordre taille
load("RData/Taille/Reed2ndOrdreTailleFilles.RData")
load("RData/Taille/Reed2ndOrdreTailleGarcons.RData")

# --------------------- POIDS ---------------------
# Jenss Bayley poids
load("RData/Poids/JenssBayleyPoidsFilles.RData")
load("RData/Poids/JenssBayleyPoidsGarcons.RData")
# Jenss adapte poids
load("RData/Poids/JenssModifiePoidsFilles.RData")
load("RData/Poids/JenssModifiePoidsGarcons.RData")
# Reed 1er ordre poids
load("RData/Poids/Reed1erOrdrePoidsFilles.RData")
load("RData/Poids/Reed1erOrdrePoidsGarcons.RData")
# Reed 2nd ordre poids
load("RData/Poids/Reed2ndOrdrePoidsFilles.RData")
load("RData/Poids/Reed2ndOrdrePoidsGarcons.RData")

# charger multiplot
source("../Fonctions/multiplot.R")

# creer dossier pour sauvegarder les resultats
newpath <- file.path("Graphiques")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")

# packages
library(dplyr)
library(ggplot2)

# définition globale des thèmes pour ggplot2 -------------------------------------------------

my_theme <- theme(
  legend.position = "bottom",
  legend.key.width = unit(3, "cm"),
  legend.title = element_text(size = 18, face = "bold"),
  legend.text = element_text(size = 16, margin = margin(r = 1, unit = "pt")),
  legend.margin = margin(rep(0, 4)),
  legend.spacing.x = unit(0, "cm"),
  plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
  axis.title.x = element_text(face = "bold", size = 18, hjust = 0.5),
  axis.title.y = element_text(face = "bold", size = 18, hjust = 0.5),
  axis.text.x = element_text(face = "plain", size = 18, hjust = 0),
  axis.text.y = element_text(face = "plain", size = 18, hjust = 0),
  plot.caption = element_text(size = 14),
  strip.text.x = element_text(size = 18)
)

# Graphique populationnel taille filles ------------------------------------------------------

# data
croiss_modele <- croiss_taille %>% filter(sexe == "F")
n <- croiss_modele %>% select(num) %>% distinct() %>% nrow()
# prédictions
new_data <- data.frame(num = rep(1, 157), age_mois = 0:156)
new_data_no_zero <- data.frame(num = rep(1, 157), age_mois_no_zero = 0:156)
prediction <- data.frame(new_data, 
                         age_an = new_data$age_mois/12,
                         predictionJenssBayley = predict(JenssBayleyTailleFilles, new_data),
                         predictionJenssModifie = predict(JenssModifieTailleFilles, new_data),
                         predictionReed1erOrdre = predict(Reed1erOrdreTailleFilles, new_data_no_zero),
                         predictionReed2ndOrdre = predict(Reed2ndOrdreTailleFilles, new_data_no_zero)
)
# plot
taille_filles <- ggplot(croiss_modele, aes(x = age_an, y = taille)) + 
  theme_minimal() +
  geom_point(colour = "#999997", pch = 1) +
  geom_line(data = prediction,
            aes(x = age_an, y = predictionReed2ndOrdre, 
                colour = "R2", linetype = "R2"),
            size = 2) +
  geom_line(data = prediction,
            aes(x = age_an, y = predictionReed1erOrdre, 
                colour = "R1", linetype = "R1"),
            size = 2) +
  geom_line(data = prediction, 
            aes(x = age_an, y = predictionJenssBayley, 
                colour = "JB", linetype = "JB"),
            size = 2) +
  geom_line(data = prediction,
            aes(x = age_an, y = predictionJenssModifie, 
                colour = "JA", linetype = "JA"),
            size = 2) +
  scale_color_manual(name = "",
                     values = c("JB" = "#941757", "JA" = "#AB5676",
                                "R1" = "#1F78B4", "R2" = "#A6CEE3"),
                     labels = c("Jenss-Bayley",
                                "Jenss adapté",
                                bquote('Reed ' ~ 1^er ~'ordre'),
                                bquote('Reed ' ~ 2^nd ~'ordre'))
  ) +
  scale_linetype_manual(name = "",
                        values = c("JB" = "11",
                                   "JA" = "twodash",
                                   "R1" = "dashed",
                                   "R2" = "solid"),
                        labels = c("Jenss-Bayley",
                                   "Jenss adapté",
                                   bquote('Reed ' ~ 1^er ~'ordre'),
                                   bquote('Reed ' ~ 2^nd ~'ordre'))
  ) +
  labs(
    title = paste0("Modélisation de la taille sur les données filles (n = ", n,")"),
    x = "Âge (années)",
    y = "Taille (cm)",
    caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
  ) +
  my_theme

# Graphique populationnel taille filles ----------------------------------------------------------------------------

# data
croiss_modele <- croiss_taille %>% filter(sexe == "M")
n <- croiss_modele %>% select(num) %>% distinct() %>% nrow()
# prédictions
new_data <- data.frame(num = rep(1, 157), age_mois = 0:156)
new_data_no_zero <- data.frame(num = rep(1, 157), age_mois_no_zero = 0:156)
prediction <- data.frame(new_data, 
                         age_an = new_data$age_mois/12,
                         predictionJenssBayley = predict(JenssBayleyTailleGarcons, new_data),
                         predictionJenssModifie = predict(JenssModifieTailleGarcons, new_data),
                         predictionReed1erOrdre = predict(Reed1erOrdreTailleGarcons, new_data_no_zero),
                         predictionReed2ndOrdre = predict(Reed2ndOrdreTailleGarcons, new_data_no_zero)
)
# plot
taille_garcons <- ggplot(croiss_modele, aes(x = age_an, y = taille)) + 
  theme_minimal() +
  geom_point(colour = "#999997", pch = 1) +
  geom_line(data = prediction,
            aes(x = age_an, y = predictionReed2ndOrdre, 
                colour = "R2", linetype = "R2"),
            size = 2) +
  geom_line(data = prediction,
            aes(x = age_an, y = predictionReed1erOrdre, 
                colour = "R1", linetype = "R1"),
            size = 2) +
  geom_line(data = prediction, 
            aes(x = age_an, y = predictionJenssBayley, 
                colour = "JB", linetype = "JB"),
            size = 2) +
  geom_line(data = prediction,
            aes(x = age_an, y = predictionJenssModifie, 
                colour = "JA", linetype = "JA"),
            size = 2) +
  scale_color_manual(name = "",
                     values = c("JB" = "#941757", "JA" = "#AB5676",
                                "R1" = "#1F78B4", "R2" = "#A6CEE3"),
                     labels = c("Jenss-Bayley",
                                "Jenss adapté",
                                bquote('Reed ' ~ 1^er ~'ordre'),
                                bquote('Reed ' ~ 2^nd ~'ordre'))
  ) +
  scale_linetype_manual(name = "",
                        values = c("JB" = "11",
                                   "JA" = "twodash",
                                   "R1" = "dashed",
                                   "R2" = "solid"),
                        labels = c("Jenss-Bayley",
                                   "Jenss adapté",
                                   bquote('Reed ' ~ 1^er ~'ordre'),
                                   bquote('Reed ' ~ 2^nd ~'ordre'))
  ) +
  labs(
    title = paste0("Modélisation de la taille sur les données garçons (n = ", n,")"),
    x = "Âge (années)",
    y = "Taille (cm)",
    caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
  ) +
  my_theme

# Graphique populationnel taille garcons ----------------------------------------------------------------------------

# data
croiss_modele <- croiss_poids %>% filter(sexe == "F")
n <- croiss_modele %>% select(num) %>% distinct() %>% nrow()
# prédictions
new_data <- data.frame(num = rep(1, 157), age_mois = 0:156)
new_data_no_zero <- data.frame(num = rep(1, 157), age_mois_no_zero = 0:156)
prediction <- data.frame(new_data, 
                         age_an = new_data$age_mois/12,
                         predictionJenssBayley = predict(JenssBayleyPoidsFilles, new_data),
                         predictionJenssModifie = predict(JenssModifiePoidsFilles, new_data),
                         predictionReed1erOrdre = predict(Reed1erOrdrePoidsFilles, new_data_no_zero),
                         predictionReed2ndOrdre = predict(Reed2ndOrdrePoidsFilles, new_data_no_zero)
)
# plot
poids_filles <- ggplot(croiss_modele, aes(x = age_an, y = poids)) + 
  theme_minimal() +
  geom_point(colour = "#999997", pch = 1) +
  geom_line(data = prediction,
            aes(x = age_an, y = predictionReed2ndOrdre, 
                colour = "R2", linetype = "R2"),
            size = 2) +
  geom_line(data = prediction,
            aes(x = age_an, y = predictionReed1erOrdre, 
                colour = "R1", linetype = "R1"),
            size = 2) +
  geom_line(data = prediction, 
            aes(x = age_an, y = predictionJenssBayley, 
                colour = "JB", linetype = "JB"),
            size = 2) +
  geom_line(data = prediction,
            aes(x = age_an, y = predictionJenssModifie, 
                colour = "JA", linetype = "JA"),
            size = 2) +
  scale_color_manual(name = "",
                     values = c("JB" = "#941757", "JA" = "#AB5676",
                                "R1" = "#1F78B4", "R2" = "#A6CEE3"),
                     labels = c("Jenss-Bayley",
                                "Jenss adapté",
                                bquote('Reed ' ~ 1^er ~'ordre'),
                                bquote('Reed ' ~ 2^nd ~'ordre'))
  ) +
  scale_linetype_manual(name = "",
                        values = c("JB" = "11",
                                   "JA" = "twodash",
                                   "R1" = "dashed",
                                   "R2" = "solid"),
                        labels = c("Jenss-Bayley",
                                   "Jenss adapté",
                                   bquote('Reed ' ~ 1^er ~'ordre'),
                                   bquote('Reed ' ~ 2^nd ~'ordre'))
  ) +
  labs(
    title = paste0("Modélisation du poids sur les données filles (n = ", n,")"),
    x = "Âge (années)",
    y = "Poids (kg)",
    caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
  ) +
  my_theme

# Graphique populationnel poids garcons ----------------------------------------------------------------------------

# data
croiss_modele <- croiss_poids %>% filter(sexe == "M")
n <- croiss_modele %>% select(num) %>% distinct() %>% nrow()
# prédictions
new_data <- data.frame(num = rep(1, 157), age_mois = 0:156)
new_data_no_zero <- data.frame(num = rep(1, 157), age_mois_no_zero = 0:156)
prediction <- data.frame(new_data, 
                         age_an = new_data$age_mois/12,
                         predictionJenssBayley = predict(JenssBayleyPoidsGarcons, new_data),
                         predictionJenssModifie = predict(JenssModifiePoidsGarcons, new_data),
                         predictionReed1erOrdre = predict(Reed1erOrdrePoidsGarcons, new_data_no_zero),
                         predictionReed2ndOrdre = predict(Reed2ndOrdrePoidsGarcons, new_data_no_zero)
)
# plot
poids_garcons <- ggplot(croiss_modele, aes(x = age_an, y = poids)) + 
  theme_minimal() +
  geom_point(colour = "#999997", pch = 1) +
  geom_line(data = prediction,
            aes(x = age_an, y = predictionReed2ndOrdre, 
                colour = "R2", linetype = "R2"),
            size = 2) +
  geom_line(data = prediction,
            aes(x = age_an, y = predictionReed1erOrdre, 
                colour = "R1", linetype = "R1"),
            size = 2) +
  geom_line(data = prediction, 
            aes(x = age_an, y = predictionJenssBayley, 
                colour = "JB", linetype = "JB"),
            size = 2) +
  geom_line(data = prediction,
            aes(x = age_an, y = predictionJenssModifie, 
                colour = "JA", linetype = "JA"),
            size = 2) +
  scale_color_manual(name = "",
                     values = c("JB" = "#941757", "JA" = "#AB5676",
                                "R1" = "#1F78B4", "R2" = "#A6CEE3"),
                     labels = c("Jenss-Bayley",
                                "Jenss adapté",
                                bquote('Reed ' ~ 1^er ~'ordre'),
                                bquote('Reed ' ~ 2^nd ~'ordre'))
  ) +
  scale_linetype_manual(name = "",
                        values = c("JB" = "11",
                                   "JA" = "twodash",
                                   "R1" = "dashed",
                                   "R2" = "solid"),
                        labels = c("Jenss-Bayley",
                                   "Jenss adapté",
                                   bquote('Reed ' ~ 1^er ~'ordre'),
                                   bquote('Reed ' ~ 2^nd ~'ordre'))
  ) +
  labs(
    title = paste0("Modélisation du poids sur les données garçons (n = ", n,")"),
    x = "Âge (années)",
    y = "Poids (kg)",
    caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
  ) +
  my_theme

# Exporter tous les graphes -------------------------------------------------------------

pdf("Graphiques/Taille/ModelisationPopulationnelleTailleFilles.pdf", width = 12, height = 10)
taille_filles
dev.off()

pdf("Graphiques/Taille/ModelisationPopulationnelleTailleGarcons.pdf", width = 12, height = 10)
taille_garcons
dev.off()

pdf("Graphiques/Poids/ModelisationPopulationnellePoidsFilles.pdf", width = 12, height = 10)
poids_filles
dev.off()

pdf("Graphiques/Poids/ModelisationPopulationnellePoidsGarcons.pdf", width = 12, height = 10)
poids_garcons
dev.off()
