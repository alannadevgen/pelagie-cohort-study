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
library(tidyverse)

# fixation de la graine
set.seed(12345678)

# Spaghetti plot des donnees poids ----------------------------------------------------------

# Selection d'un echantillon parmi toutes les donnees
selection <- croiss_poids %>% filter(n_obs > 20) %>% select(num) %>% distinct() %>% pull() %>% sample(., size = 30)
# filtrage des donnees selon cet echantillon
selection_data <- croiss_poids %>% filter(num %in% selection)
# repartition filles/garcons
selection_data %>% group_by(num) %>% distinct(num, sexe)  %>% as.data.frame() %>% select(sexe) %>% table()

# sample de 30 enfants
pdf("Graphiques/Poids/SpaghettiPlotPoids.pdf", paper = "A4r", width = 13, height = 12)
ggplot(data = selection_data) +
  theme_minimal() +
  geom_line(aes(x = age_an, y = poids, group = num, col = sexe, linetype = sexe), size = 0.7) +
  scale_colour_manual(values = c("#BD3977", "#F97A5D"),
                      labels = c("Filles", "Garçons"),
                      name = "Sexe") +
  scale_linetype_manual(values = c("longdash", "solid"),
                        labels = c("Filles", "Garçons"),
                        name = "Sexe") +
  labs(title = "Courbes de croissance de poids",
       x = "Âge (années)",
       y = "Poids (kg)",
       caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
       ) +
  theme(legend.position = c(0.92, 0.1),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(color = "black"),
        strip.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.spacing.x = unit(4, "mm"),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.width = unit(1.2, "cm"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18, face = "bold"),
        plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.y = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.text.x = element_text(face = "plain", size = 18, hjust = 0),
        axis.text.y = element_text(face = "plain", size = 18, hjust = 0),
        plot.caption = element_text(size = 14),
        strip.text.x = element_text(size = 18)
        )
dev.off()

# tous les enfants
pdf("Graphiques/Poids/SpaghettiPlotPoidsEchantillon.pdf", paper = "A4r", width = 13, height = 12)
ggplot(data = croiss_poids) +
  theme_minimal() +
  geom_line(aes(x = age_an, y = poids, group = num, col = sexe, linetype = sexe), size = 0.7) +
  scale_colour_manual(values = c("#BD3977", "#F97A5D"),
                      labels = c("Filles", "Garçons"),
                      name = "Sexe") +
  scale_linetype_manual(values = c("longdash", "solid"),
                        labels = c("Filles", "Garçons"),
                        name = "Sexe") +
  labs(title = "Courbes de croissance de poids",
       x = "Âge (années)",
       y = "Poids (kg)",
       caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
  ) +
  theme(legend.position = c(0.92, 0.1),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(color = "black"),
        strip.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.spacing.x = unit(4, "mm"),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.width = unit(1.2, "cm"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18, face = "bold"),
        plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.y = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.text.x = element_text(face = "plain", size = 18, hjust = 0),
        axis.text.y = element_text(face = "plain", size = 18, hjust = 0),
        plot.caption = element_text(size = 14),
        strip.text.x = element_text(size = 18)
        )
dev.off()

# Spaghetti plot des donnees taille ----------------------------------------------------------

# Selection d'un echantillon parmi toutes les donnees
selection <- croiss_taille %>% filter(n_obs > 20) %>% select(num) %>% distinct() %>% pull() %>% sample(., size = 30)
# filtrage des donnees selon cet echantillon
selection_data <- croiss_taille %>% filter(num %in% selection)
# repartition filles/garcons
selection_data %>% group_by(num) %>% distinct(num, sexe)  %>% as.data.frame() %>% select(sexe) %>% table()

# sample de 30 enfants
pdf("Graphiques/Taille/SpaghettiPlotTaille.pdf", paper = "A4r", width = 13, height = 12)
ggplot(data = selection_data) +
  theme_minimal() +
  geom_line(aes(x = age_an, y = taille, group = num, col = sexe, linetype = sexe), size = 0.7) +
  scale_colour_manual(values = c("#BD3977", "#F97A5D"),
                      labels = c("Filles", "Garçons"),
                      name = "Sexe") +
  scale_linetype_manual(values = c("longdash", "solid"),
                      labels = c("Filles", "Garçons"),
                      name = "Sexe") +
  labs(title = "Courbes de croissance de taille",
       x = "Âge (années)",
       y = "Taille (cm)",
       caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
       ) +
  theme(legend.position = c(0.92, 0.1),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(color = "black"),
        strip.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.spacing.x = unit(4, "mm"),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.width = unit(1.2, "cm"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18, face = "bold"),
        plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.y = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.text.x = element_text(face = "plain", size = 18, hjust = 0),
        axis.text.y = element_text(face = "plain", size = 18, hjust = 0),
        plot.caption = element_text(size = 14),
        strip.text.x = element_text(size = 18)
        )
dev.off()

# tous les enfants
pdf("Graphiques/Taille/SpaghettiPlotTailleEchantillon.pdf", paper = "A4r", width = 13, height = 12)
ggplot(data = croiss_taille) +
  theme_minimal() +
  geom_line(aes(x = age_an, y = taille, group = num, col = sexe, linetype = sexe), size = 0.7) +
  scale_colour_manual(values = c("#BD3977", "#F97A5D"),
                      labels = c("Filles", "Garçons"),
                      name = "Sexe") +
  scale_linetype_manual(values = c("longdash", "solid"),
                        labels = c("Filles", "Garçons"),
                        name = "Sexe") +
  labs(title = "Courbes de croissance de taille",
       x = "Âge (années)",
       y = "Taille (cm)",
       caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
  ) +
  theme(legend.position = c(0.92, 0.1),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(color = "black"),
        strip.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.spacing.x = unit(4, "mm"),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.width = unit(1.2, "cm"),
        legend.spacing.x = unit(1.0, "mm"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18, face = "bold"),
        plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.y = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.text.x = element_text(face = "plain", size = 18, hjust = 0),
        axis.text.y = element_text(face = "plain", size = 18, hjust = 0),
        plot.caption = element_text(size = 14),
        strip.text.x = element_text(size = 18)
  )
dev.off()
