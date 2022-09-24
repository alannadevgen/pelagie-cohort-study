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
library(forcats)
library(plyr)
library(gridExtra)

# Graphiques pour le poids et la taille a la naissance ---------------------------------------------

naiss <- croiss %>% filter(age_mois == 0)

# frequence
pdf("Graphiques/Poids/HistogrammePoidsNaissance.pdf", width = 9, height = 7)
ggplot(naiss, aes(x = poids)) + 
  theme_minimal() +
  geom_histogram(bins = 25, aes(y = stat(count) / sum(count)), 
                 color="MediumTurquoise", fill="LightSeaGreen") +
  labs(title = "Poids à la naissance des enfants de la cohorte PÉLAGIE",
       x = "Poids (kg)",
       y = "Fréquence (%)",
       caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
       ) +
  scale_y_continuous(labels = scales::percent) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8))
dev.off()

# frequence
pdf("Graphiques/Taille/HistogrammeTailleNaissance.pdf", width = 9, height = 7)
ggplot(naiss, aes(x = taille)) +
  theme_minimal() +
  geom_histogram(bins = 25, aes(y = stat(count) / sum(count)),
                 fill = "#5d2a69", color = "#4a2254") +
  labs(title="Taille à la naissance des enfants de la cohorte PÉLAGIE",
       x="Taille (cm)",
       y="Fréquence (%)",
       caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
       ) +
  scale_y_continuous(labels = scales::percent) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8))
dev.off()
