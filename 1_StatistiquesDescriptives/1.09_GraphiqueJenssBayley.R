# Set-up -------------------------------------------------------------------------------

# working directory
setwd("../1_StatistiquesDescriptives")
setwd("1_StatistiquesDescriptives")

# vider l'espace de travail et charger les donnees
rm(list = ls())

source("../Modeles/ModelesJenss.R")

# packages
library(dplyr)
library(ggplot2)
library(scales)

# Fake data ----------------------------------------------------------------------------

age = seq(0, 15, 0.5)
JenssBayley = modJenssBayley(age, Ap = 3.2282940, Bp = 0.2373537, Cp = 2.8923920, Dp = 0.4268003)
ModeleLineaire = 8 + age*0.2445
Exponentielle = JenssBayley + log(age)/1.5
Exponentielle[1:3] = JenssBayley[1:3]

data <- data.frame(
  age,
  JenssBayley,
  ModeleLineaire,
  Exponentielle
)

# Graphique Jenss-Bayley ---------------------------------------------------------------

pdf("Graphiques/InterpretationCoefficientsJenssBayley.pdf", width = 14, height = 10)
ggplot(data) +
  theme_minimal() +
  geom_line(aes(age, JenssBayley, colour = "JB", linetype = "JB"), size = 1.8) +
  geom_line(aes(age, Exponentielle, colour = "Exp", linetype = "Exp"), size = 1.8) +
  geom_line(aes(age, ModeleLineaire, colour = "ML", linetype = "ML"), size = 1.8) +
  geom_point(aes(age[1], JenssBayley[1]), colour = "grey30", size = 2.5) +
  scale_color_manual(name = "",
                     values = c("JB" = "#3497A9",
                                "Exp" = "#d81159",
                                "ML" = "#55a630"),
                     labels = c("Jenss-Bayley",
                                "Mouvement exponentiel",
                                "Modèle linéaire")
  ) +
  scale_linetype_manual(name = "",
                     values = c("JB" = "solid",
                                "Exp" = "dashed",
                                "ML" = "11"),
                     labels = c("Jenss-Bayley",
                                "Mouvement exponentiel",
                                "Modèle linéaire")
  ) +
  labs(
    title = "Interprétation des coefficients du modèle de Jenss-Bayley",
    x = "Âge (mois)",
    y = "Poids (kg)",
    caption = "Source : Jérémie BOTTON et al. (2014) | Graphique : Alanna GENIN"
  ) +
  annotate("text", x = age[1], y = JenssBayley[1]-0.6, label = "a", size = 10, hjust = 0, vjust = 0, 
           col = "grey30", lineheight = 0.8) +
  annotate("text", x = age[17], y = ModeleLineaire[17]+0.3, label = "b", size = 10, hjust = 0, vjust = 0, 
           col = "grey30", lineheight = 0.8) +
  annotate("text", x = 0.1, y = (JenssBayley[1] + ModeleLineaire[1])/2, 
           label = "e^c", size = 10, hjust = 0, vjust = 0, parse=TRUE,
           col = "grey30", lineheight = 0.8) +
  annotate("text", x = 6.1, y = (JenssBayley[13] + Exponentielle[13])/2, 
           label = "d", size = 10, hjust = 0, vjust = 0, 
           col = "grey30", lineheight = 0.8) +
  annotate(
    geom = "curve", x = 0, xend = 0,  y = JenssBayley[1]+0.15, yend = ModeleLineaire[1]-0.1,
    colour = "grey30", curvature = -.0, arrow = arrow(length = unit(2, "mm"), ends = "both")) +
  annotate(
    geom = "curve", x = 6, xend = 6,  y = JenssBayley[13]+0.1, yend = Exponentielle[13]-0.1,
    colour = "grey30", curvature = -.0, arrow = arrow(length = unit(2, "mm"), ends = "both")) +
  scale_y_continuous(limits = c(0, 12),
                     labels = label_number(suffix = " kg", big.mark = " ", decimal.mark = ",")) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2.2, "cm"),
    legend.text = element_text(face = "plain", size = 18),
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text.x = element_text(face = "plain", size = 18, hjust = 0),
    axis.text.y = element_text(face = "plain", size = 18, hjust = 0),
    plot.caption = element_text(size = 14)
  )
dev.off()
