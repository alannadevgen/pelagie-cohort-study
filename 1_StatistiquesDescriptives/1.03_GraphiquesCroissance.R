# Set-up ------------------------------------------------------------------------------------

# working directory
setwd("../1_StatistiquesDescriptives")
setwd("1_StatistiquesDescriptives")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# charger la fonction multiplot
source("../Fonctions/multiplot.R")

# creer dossier pour sauvegarder les resultats
newpath <- file.path("Graphiques/Poids")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")
newpath <- file.path("Graphiques/Taille")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")
newpath <- file.path("Graphiques/IMC")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")
newpath <- file.path("Graphiques/Age")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")

# packages
library(ggplot2)
library(forcats)
library(plyr)
library(gridExtra)

# Renommage sexe pour affichage -------------------------------------------------------

croiss$sexe <- as.factor(croiss$sexe) %>% fct_recode("Filles" = "F", "Garçons" = "M")

# Graphiques poids ----------------------------------------------------------------------

# Boxplot du poids en fonction du sexe
pdf("Graphiques/Poids/BoxplotPoidsSexe.pdf", width = 15, height = 10)
ggplot(croiss, aes(x = sexe, y = poids,  fill = sexe)) + 
  theme_minimal() +
  geom_boxplot() +
  labs(title="Distribution du poids des enfants", x="Sexe", y="Poids (kg)") +
  stat_summary(fun.y = "mean", geom = "point", shape = 16, size = 2, color = "white") +
  scale_fill_manual(values = c("#BD3977", "#F97A5D"))+
  theme(legend.position = "none")
dev.off()

# Boxplot du poids en fonction de l'age
boxplot_poids_age <- ggplot(data = croiss, aes(x = factor(age_an_ent), y = poids, group = interaction(sexe, age_an_ent))) + 
  theme_bw() +
  geom_boxplot(aes(fill = factor(sexe), color = factor(sexe))) +
  scale_fill_manual("Sexe", values = c("#BD3977", "#F97A5D")) + 
  scale_color_manual("Sexe", values = c("#BD3977", "#F97A5D")) + 
  stat_summary(geom = "crossbar", width = 1, fatten = 1.5, color = "white",
               position = position_dodge(1),
               fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))}) +
  labs(title = "Poids des enfants en fonction de l'âge",
       x = "Âge (années)",
       y = "Poids (kg)",
       legend = "",
       caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
  ) +
  theme(legend.position = c(.15,.85),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(color = "black"),
        strip.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.spacing.x = unit(4, "mm"),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(1.5, "cm"),
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
pdf("Graphiques/Poids/BoxplotPoidsAge.pdf", width = 15, height = 10)
boxplot_poids_age
dev.off()

# Histogramme du poids en fonction du sexe
pdf("Graphiques/Poids/HistogrammePoids.pdf", width = 15, height = 10)
ggplot(croiss, scale="density", aes(x=poids)) + 
  theme_minimal() +
  geom_histogram(binwidth=1, aes(y=..density..), color="MediumTurquoise", fill="LightSeaGreen") + 
  labs(title="Distribution du poids des enfants", x="Poids (kg)", y="Densité") +
  theme_bw()
dev.off()

# Graphiques taille --------------------------------------------------------------------

# Boxplot de la taille en fonction du sexe
pdf("Graphiques/Taille/BoxplotTailleSexe.pdf", width = 15, height = 10)
ggplot(croiss, aes(x = sexe, y = taille, fill = sexe)) + 
  theme_minimal() +
  geom_boxplot() +
  labs(title="Distribution de la taille des enfants", x="Sexe", y="Taille (cm)") +
  stat_summary(fun.y = "mean", geom = "point", shape = 16, size = 2, color = "white") +
  scale_fill_manual(values = c("#BD3977", "#F97A5D")) +
  theme(legend.position = "none")
dev.off()

# Boxplot de la taille en fonction de l'age
boxplot_taille_age <- ggplot(data = croiss, aes(x = factor(age_an_ent), y = taille, group = interaction(sexe, age_an_ent))) + 
  theme_bw() +
  geom_boxplot(aes(fill = factor(sexe), color = factor(sexe))) +
  scale_fill_manual("Sexe", values = c("#BD3977", "#F97A5D")) + 
  scale_color_manual("Sexe", values = c("#BD3977", "#F97A5D")) + 
  stat_summary(geom = "crossbar", width = 1, fatten = 1.5, color = "white",
               position = position_dodge(1),
               fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))}) +
  labs(title = "Taille des enfants en fonction de l'âge",
       x = "Âge (années)",
       y = "Taille (cm)",
       legend = "",
       caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
  ) +
  theme(legend.position = c(.15,.85),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.border = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(color = "black"),
        strip.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.spacing.x = unit(4, "mm"),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(1.5, "cm"),
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
pdf("Graphiques/Taille/BoxplotTailleAge.pdf", width = 15, height = 10)
boxplot_taille_age
dev.off()

# Histogramme de la taille
pdf("Graphiques/Taille/HistogrammeTaille.pdf", width = 15, height = 10)
ggplot(croiss, scale="density", aes(x=taille)) + 
  theme_minimal() +
  geom_histogram(binwidth=2, aes(y=..density..),
                 fill = "#5d2a69", color = "#4a2254")+ 
  labs(title="Distribution de la taille des enfants", x="Taille (cm)", y="Densité")+
  theme_bw()
dev.off()

# Graphiques IMC ------------------------------------------------------------------------

# Boxplot de l'IMC
pdf("Graphiques/IMC/BoxplotIMCSexe.pdf", width = 15, height = 10)
ggplot(croiss, aes(x = sexe, y = IMC,  fill = sexe)) + 
  theme_minimal() +
  geom_boxplot() +
  labs(title="Distribution de l'IMC des enfants", x="Sexe", y="IMC") +
  stat_summary(fun.y = "mean", geom = "point", shape = 16, size = 2, color = "white")+
  scale_fill_manual(values = c("#BD3977", "#F97A5D"))+
  theme(legend.position="none")
dev.off()

# Histogramme de l'IMC
pdf("Graphiques/IMC/HistogrammeIMC.pdf", width = 15, height = 10)
ggplot(croiss, scale="density", aes(x=IMC)) + 
  theme_minimal() +
  geom_histogram(aes(y=..density..), color="SlateBlue", fill="MediumPurple")+ 
  labs(title="Distribution de l'IMC des enfants", x="IMC", y="Densité")+
  theme_bw()
dev.off()

# Age maximal et nb d'obs ------------------------------------------------------------------------

pdf("Graphiques/Age/HistogrammeAge.pdf")
ggplot(croiss, scale="density", aes(x=age_an_max)) + 
  theme_minimal() +
  geom_histogram(binwidth=0.1, color="Black", fill="RoyalBlue")+ 
  labs(title="Distribution de l'age maximal des enfants", x="Age maximal (années)", y="Frequence")
dev.off()

ggplot(croiss, scale="density", aes(x=n_obs)) + 
  theme_minimal() +
  geom_histogram(binwidth=1, color="Black", fill="SteelBlue")+ 
  labs(title="Distribution du nombre d'observations par enfant", x="Nombre de mesures par enfant", y="Fréquence")

# combiner les deux boxplot poids/taille en fonction de l'age --------------------------------------

pdf("Graphiques/BoxplotPoidsTailleAge.pdf", width = 16)
multiplot(boxplot_poids_age, boxplot_taille_age, cols = 2)
dev.off()
