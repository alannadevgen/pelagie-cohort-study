# Description --------------------------------------------------------------------

# Graphique populationnel de tous les modeles poids

# author : @alannagenin

# Set-up -------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")
setwd("2_ModelesCroissance")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")
load("RData/Poids/JenssModifiePoidsPop.RData")
load("RData/Poids/JenssBayleyPoidsPop.RData")
load("RData/Poids/Reed1erOrdrePoidsPop.RData")
load("RData/Poids/Reed2ndOrdrePoidsPop.RData")

# creer dossier pour sauvegarder les resultats
newpath <- file.path("Graphiques/Poids")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")

# packages
library(dplyr)
library(ggplot2)

# Graphique populationnel Jenss-Bayley et Jenss modifié -------------------------------------------

new_data <- data.frame(num = rep(1, 157), age_mois = 0:156, age_an = 0:156/12)
new_data_no_zero <- data.frame(num = rep(1, 157), age_mois_no_zero = 0:156)
prediction <- data.frame(new_data, 
                         predictionJenssBayleyPoidsPop = predict(JenssBayleyPoidsPop, new_data),
                         predictionJenssModifiePoidsPop = predict(JenssModifiePoidsPop, new_data),
                         predictionReed1erOrdrePoidsPop = predict(Reed1erOrdrePoidsPop, new_data_no_zero),
                         predictionReed2ndOrdrePoidsPop = predict(Reed2ndOrdrePoidsPop, new_data_no_zero)
)


plot_populationnel <- ggplot(croiss, aes(x = age_an, y = poids)) + 
  theme_minimal() +
  geom_point(colour = "#999997", pch = 1) +
  geom_line(data = prediction,
            aes(x = age_an, y = predictionReed2ndOrdrePoidsPop, 
                colour = "R2",
                linetype = "R2"),
            size = 1.5) +
  geom_line(data = prediction, 
            aes(x = age_an, y = predictionJenssBayleyPoidsPop,
                colour = "JB",
                linetype = "JB"),
            size = 1.5) +
  geom_line(data = prediction,
            aes(x = age_an, y = predictionReed1erOrdrePoidsPop,
                colour = "R1",
                linetype = "R1"),
            size = 1.5) +
  geom_line(data = prediction,
            aes(x = age_an, y = predictionJenssModifiePoidsPop,
                colour = "JA",
                linetype = "JA"),
            size = 1.5) +
  scale_color_manual(name = "",
                    values = c("JB" = "#941757",
                               "JA" = "#AB5676",
                               "R1" = "#1F78B4",
                               "R2" = "#A6CEE3"),
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
    title = "Modélisation populationnelle des effets fixes de quatre modèles structuraux pour le poids",
    x = "Âge (années)",
    y = "Poids (kg)",
    caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
  ) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(3, "cm"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15, face = "bold"),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(face = "plain", size = 16, hjust = 0),
    axis.text.y = element_text(face = "plain", size = 16, hjust = 0),
    plot.caption = element_text(size = 14),
    strip.text.x = element_text(size = 16)
  )

plot_populationnel

pdf("Graphiques/Poids/GraphiquePopulationnelPoids.pdf", width = 12, height = 10)
plot_populationnel
dev.off()
