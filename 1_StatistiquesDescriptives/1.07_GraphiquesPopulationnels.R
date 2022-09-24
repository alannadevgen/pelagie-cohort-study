# Set-up ------------------------------------------------------------------------------------

# working directory
setwd("../1_StatistiquesDescriptives")
setwd("1_StatistiquesDescriptives")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# creer dossier pour sauvegarder les resultats
newpath <- file.path("Graphiques/Poids")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")
newpath <- file.path("Graphiques/Taille")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")

# packages
library(ggplot2)
library(ggpubr)
library(tidyverse)

# Plot populationnel poids ----------------------------------------------------------------

plot_poids <- ggplot(croiss, aes(x = age_an, y = poids)) + 
  theme_minimal() +
  geom_point(colour = "#999997", pch = 1) +
  labs(
    title = "Poids en fonction de l'âge",
    x = "Âge (années)",
    y = "Poids (kg)",
    caption = ""
    # caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
  ) + 
  theme(
    plot.background = element_rect(fill = "transparent", colour = "transparent"),
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    panel.border = element_rect(fill = "transparent", colour = "transparent"),
    axis.text = element_text(color = "black"),
    strip.background = element_rect(fill = "transparent", colour = "transparent"),
    panel.spacing.x = unit(4, "mm"),
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text.x = element_text(face = "plain", size = 18, hjust = 0),
    axis.text.y = element_text(face = "plain", size = 18, hjust = 0),
    plot.caption = element_text(size = 14),
    strip.text.x = element_text(size = 18)
  )

# export
pdf(file = "Graphiques/Poids/PoidsEnFonctionAge.pdf", paper = "a4r", width = 12, height = 10)
plot_poids
dev.off()

# Plot populationnel taille ----------------------------------------------------------------

plot_taille <- ggplot(croiss, aes(x = age_an, y = taille)) + 
  theme_minimal() +
  geom_point(colour = "#999997", pch = 1) +
  labs(
    title = "Taille en fonction de l'âge",
    x = "Âge (années)",
    y = "Taille (cm)",
    caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
  ) +
  theme(
    plot.background = element_rect(fill = "transparent", colour = "transparent"),
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    panel.border = element_rect(fill = "transparent", colour = "transparent"),
    axis.text = element_text(color = "black"),
    strip.background = element_rect(fill = "transparent", colour = "transparent"),
    panel.spacing.x = unit(4, "mm"),
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text.x = element_text(face = "plain", size = 18, hjust = 0),
    axis.text.y = element_text(face = "plain", size = 18, hjust = 0),
    plot.caption = element_text(size = 14),
    strip.text.x = element_text(size = 18)
  )

# export
pdf(file = "Graphiques/Taille/TailleEnFonctionAge.pdf", paper = "a4r", width = 12, height = 10)
plot_taille
dev.off()

# plot final ----------------------------------------------------------------------------

# combiner les deux plots
plot_final <- ggarrange(plot_poids, plot_taille, nrow = 1, ncol = 2)

# export
pdf(file = "Graphiques/PoidsTailleEnFonctionAge.pdf", width = 16, height = 9)
plot_final
dev.off()
