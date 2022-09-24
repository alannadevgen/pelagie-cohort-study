# Description ----------------------------------------------------------------------------

# Graphiques diagnostiques pour le meilleur modèle poids : Jenss adapte

# author : @alannagenin

# Set-up ---------------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")
setwd("2_ModelesCroissance")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")
load("RData/Poids/JenssModifiePoidsFilles.RData")

# creer dossier pour sauvegarder les resultats
newpath <- file.path("Graphiques/Poids")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")

# packages
library(dplyr)
library(ggplot2)

# Data ------------------------------------------------------------------------------------------------

# selectionner les donnees filles
croiss_modele <- croiss_poids %>% filter(sexe == "F")
best.fit <- JenssModifiePoidsFilles

# Graphique observations vs predictions ---------------------------------------------------------------------------------

# fusionner avec les predictions
croiss_modele <- croiss_modele %>%
  bind_cols(best.fit["results"]["predictions"] %>%
              select(icpred) %>% dplyr::rename(., prediction = icpred)
  ) %>%
  relocate(prediction, .after = poids)

# graphique des predictions individuelles
pdf("Graphiques/Poids/PredictionsIndividuellesJenssAdaptePoidsFilles.pdf", paper = "a4r", width = 11, height = 10)
ggplot(croiss_modele, aes(x = prediction, y = poids)) +
  theme_minimal() +
  geom_point(pch = 1, colour = "#999997") +
  geom_abline(intercept = 0, slope = 1, colour = "#941757",
              size = 1.2, linetype = "dashed") +
  labs(title = "Prédiction individuelle (MAP)",
       x = "Prédiction individuelle",
       y = "Observation",
       caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
  ) +
  xlim(0, 80) +
  ylim(0, 80) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
        axis.title.x = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 13),
        plot.caption = element_text(size = 8)
  )
dev.off()

# Graphique des résidus ---------------------------------------------------------------------------------

residus <- best.fit@results@icpred %>% 
  bind_cols(best.fit@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Poids/ScatterResidualsJenssAdaptePoidsFilles.pdf", width = 12, height = 10)
ggplot(data = residus) +
  theme_minimal() +
  geom_point(aes(x = icpred, y = icwres), pch = 1, colour = "#999997") +
  ylim(c(-6.5, 6.5)) +
  geom_hline(yintercept = 4, colour = "#941757", size = 0.9, linetype = "dashed") +
  geom_hline(yintercept = -4, colour = "#941757", size = 0.9, linetype = "dashed") +
  labs(title = "Résidus du modèle Jenss adapté pour les filles",
       x = "Prédiction individuelle",
       y = "Résidu individuel pondéré",
       caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
  ) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
        axis.title.x = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 13),
        plot.caption = element_text(size = 8)
  )
dev.off()

