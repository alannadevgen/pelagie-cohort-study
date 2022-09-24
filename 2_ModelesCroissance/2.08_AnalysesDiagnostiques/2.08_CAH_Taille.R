# Description --------------------------------------------------------------------------

# classification pour la taille avec le modele Jenss adapte

# author : @camillepliquet @alannagenin

## Settings ----------------------------------------------------------------------------

# working directory
setwd("2_ModelesCroissance")
setwd("../2_ModelesCroissance")

# packages
library(dplyr)
library(ggplot2)
require(FactoMineR)
library(rstatix)
library(factoextra)


# vider l'espace de travail et charger les donnees
rm(list = ls())

load("../0_Datamanagement/DataDatamanagement.RData")
load("RData/Taille/JenssModifieTailleFilles.RData")
load("RData/Taille/JenssModifieTailleGarcons.RData")

# creer dossier pour sauvegarder les resultats
newpath <- file.path("Graphiques/Taille")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")
newpath <- file.path("Graphiques/Taille/CAH")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")

# Fusion des donnees filles et garcons ---------------------------------------------

# selection des donnees taille
croiss_modele <- croiss_taille

# recuperer les identifiants des filles et des garcons
num_filles <- croiss_modele %>% filter(sexe == "F") %>% select(num) %>% distinct()
num_garcons <- croiss_modele %>% filter(sexe == "M") %>% select(num) %>% distinct()

# fusionner les identifiants avec les resultats de la modelisation R2
param_filles <-  num_filles %>% 
  bind_cols(psi(JenssModifieTailleFilles) %>% as.data.frame()) %>% 
  mutate(sexe = "F")
param_garcons <- num_garcons %>%
  bind_cols(psi(JenssModifieTailleGarcons) %>% as.data.frame())%>% 
  mutate(sexe = "M")

# fusionner les params filles et garcons ensemble
param <- bind_rows(param_filles, param_garcons) %>% as.data.frame() 
colnames(param) <- c("num", "Ap", "Bp", "Cp", "Dp", "Ep", "sexe")

# suppression des donnees qui ne vont pas dnas la CAH
param <- param %>% select(-num, -sexe)
# centrer et reduire les donnees
param.cr <- scale(param, center = T, scale = T)

# calcul de la matrice des distances entre individus
d.param <- dist(param.cr)


# Classification ascendante hierarchique - critère de Ward -----------------------------------
cah.ward <- hclust(d.param, method = "ward.D2")

# Inertie -----------------------------------
# preparation des donnees
inertie <- sort(cah.ward$height, decreasing = TRUE)
sauts_inertie <- data.frame(
  nb_classes = 1:20,
  inertie = inertie %>% head(20)
)
# plot
pdf("Graphiques/Taille/CAH/SautsInertieTaille.pdf", width = 14, height = 10)
ggplot(sauts_inertie, aes(x = nb_classes, y = inertie)) +
  theme_minimal() +
  geom_step(size = 2.5) +
  labs(
    title = "Sauts d'inertie",
    x = "Nombre de classes",
    y = "Inertie",
    caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Camille PLIQUET et Alanna GENIN"
    
  ) +
  geom_point(data = sauts_inertie %>% filter(nb_classes %in% c(2, 3)),
             shape = 21, colour = c("#359DAAFF", "#73D4ADFF"),
             fill = "transparent", size = 12, stroke = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.text.x = element_text(face = "plain", size = 24, hjust = 0),
    axis.text.y = element_text(face = "plain", size = 24, hjust = 0),
    plot.caption = element_text(size = 16),
    strip.text.x = element_text(size = 18)
  )
dev.off()

# Dendrogramme ------------------------------------------------------------------------------

# definition du theme
my_theme <- theme(
  plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
  axis.title.x = element_text(face = "bold", size = 18, hjust = 0.5),
  axis.title.y = element_text(face = "bold", size = 18, hjust = 0.5),
  axis.text.x = element_text(face = "plain", size = 18, hjust = 0, colour = "white"),
  axis.text.y = element_text(face = "plain", size = 18, hjust = 0),
  plot.caption = element_text(size = 14),
  strip.text.x = element_text(size = 18),
  axis.line.x = element_line(colour = 'white', size = 0),
  axis.line.y = element_line(colour = 'white', size = 0),
  panel.background = element_rect(fill = NA, colour = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank()
)


# CAH a 2 groupes
pdf("Graphiques/Taille/CAH/CAH_taille2groupes.pdf", width = 12, height = 10)
fviz_dend(cah.ward, k = 2, show_labels = FALSE,
          k_colors = c("#97A934", "#3497a9"),
          ggtheme = theme_minimal(),
          ylab = "Distance", xlab = "Individus",
          main = "Dendrogramme pour le poids en 2 classes",
          rect = FALSE, rect_fill = FALSE, lwd = 2) + my_theme
dev.off()


# CAH a 3 groupes
pdf("Graphiques/Taille/CAH/CAH_taille3groupes.pdf", width = 12, height = 10)
fviz_dend(cah.ward, k = 3, show_labels = FALSE,
          k_colors = c("#5D9C39", "#395D9C", "#9C395D"),
          ggtheme = theme_minimal(),
          ylab = "Distance", xlab = "Individus",
          main = "Dendrogramme pour le poids en 3 classes",
          rect = FALSE, rect_fill = FALSE, lwd = 2) + my_theme
dev.off()

# decoupage en 3 groupes
groupes.cah <- cutree(cah.ward, k = 3)

# Caracterisation des groupes ----------------------------------
# ajouter le num de groupes dans les param :
param <- param %>%
  mutate(groupe = as.factor(groupes.cah))

# effectifs par groupe
table(param$groupe)

# stats descriptives
catdes(param, num.var=6)

# Tests ANOVA
param %>% anova_test(Ap ~ groupe)
param %>% anova_test(Bp ~ groupe)
param %>% anova_test(Cp ~ groupe)
param %>% anova_test(Dp ~ groupe)
param %>% anova_test(Ep ~ groupe)

# visualisation des groupes --------------------------------------------------------------

param_gp1 <- param %>% 
  filter(groupe == "1")%>%
  select("Ap", "Bp", "Cp", "Dp", "Ep")%>%
  colMeans()

param_gp2 <- param %>% 
  filter(groupe == "2")%>%
  select("Ap", "Bp", "Cp", "Dp", "Ep")%>%
  colMeans()

param_gp3 <- param %>% 
  filter(groupe == "3")%>%
  select("Ap", "Bp", "Cp", "Dp", "Ep")%>%
  colMeans()

eq <-function(time){
  param_gp1[1] + param_gp1[2]*time + param_gp1[3] - param_gp1[3]*exp(-time*param_gp1[4]) + param_gp1[5]*time^2
}
eq2 <- function(time){
  param_gp2[1] + param_gp2[2]*time + param_gp2[3] - param_gp2[3]*exp(-time*param_gp2[4]) + param_gp2[5]*time^2
}
eq3 <- function(time){
  param_gp3[1] + param_gp3[2]*time + param_gp3[3] - param_gp3[3]*exp(-time*param_gp3[4]) + param_gp3[5]*time^2
}

pdf("Graphiques/Taille/CAH/CourbesTailleCAH.pdf", width = 11, height = 10)
croiss_plot <- croiss_taille %>%
  filter(age_mois_no_zero <= 12)
ggplot(croiss_taille, aes(x = age_mois_no_zero, y = taille)) + 
  theme_minimal() +
  geom_point(colour = "#999997", pch = 1) +
  geom_function(aes(x = age_mois_no_zero, colour = "g1", linetype = "g1"),
                fun = eq, size = 1.2, show.legend = TRUE) +
  geom_function(aes(x = age_mois_no_zero, colour = "g2", linetype = "g2"),
                fun = eq2, size = 1.2, show.legend = TRUE) +
  geom_function(aes(x = age_mois_no_zero, colour = "g3", linetype = "g3"),
                fun = eq3, size = 1.2, show.legend = TRUE) +
  scale_color_manual(name = "",
                     values = c("g1" = "#5D9C39", #3497A9 #440154
                                "g2" = "#395D9C", #395D9C #1F968B
                                "g3" = "#9C395D"),#582A71 #86c239
                     labels = c("Groupe 1",
                                "Groupe 2",
                                "Groupe 3")
  ) +
  scale_linetype_manual(name = "",
                        values = c("g1" = "solid",
                                   "g2" = "dashed",
                                   "g3" = "11"
                        ),
                        labels = c("Groupe 1",
                                   "Groupe 2",
                                   "Groupe 3")
  ) + 
  labs(
    title = "Courbes de croissance moyenne de la taille pour chaque groupe de la CAH",
    x = "Âge (mois)",
    y = "Taille (cm)",
    caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Camille PLIQUET"
  ) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2.2, "cm"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    axis.title.x = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.text = element_text(size = 13)
  ) +
  ggeasy::easy_center_title()
dev.off()

# boxplots ----------------------------------
# boxplot sur la taille de naissance (Ap) par groupe : 
boxplot <- ggplot(param, aes(x=groupe, y = Ap, group = groupe)) +
  theme_minimal() +
  geom_boxplot(color = c("#3497A9", "#395D9C", "#3A2C58")) +
  labs(
    title = "Distribution de la taille de naissance dans chacun des groupes de la CAH",
    x = "Groupes",
    y = "Taille (cm)",
    caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Camille PLIQUET"
  ) +
  theme(
    legend.position = "none",
    legend.key.width = unit(2.2, "cm"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    axis.title.x = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.text = element_text(size = 13)
  ) + 
  ggeasy::easy_center_title()

pdf("Graphiques/Taille/CAH/BoxplotTailleGroupesCAH.pdf", width = 11, height = 10)
boxplot
dev.off()

# boxplots : distribution des 4 autres parametres
ggplot(param, aes(x=groupe, y = Bp, group = groupe)) +
  theme_minimal() +
  geom_boxplot(color = "#941757") +
  ggtitle("Distribution du paramètre Bp dans chacun des groupes") +
  xlab("Groupes") +
  ylab("Taille en cm") +
  theme(plot.title = element_text(face = "bold")) +
  ggeasy::easy_center_title()


ggplot(param, aes(x=groupe, y = Cp, group = groupe)) +
  theme_minimal() +
  geom_boxplot(color = "#941757") +
  ggtitle("Distribution du paramètre Cp dans chacun des groupes") +
  xlab("Groupes") +
  ylab("Taille en cm") +
  theme(plot.title = element_text(face = "bold")) +
  ggeasy::easy_center_title()


ggplot(param, aes(x=groupe, y = Dp, group = groupe)) +
  theme_minimal() +
  geom_boxplot(color = "#941757") +
  ggtitle("Distribution du paramètre Dp dans chacun des groupes") +
  xlab("Groupes") +
  ylab("Taille en cm") +
  theme(plot.title = element_text(face = "bold")) +
  ggeasy::easy_center_title()


ggplot(param, aes(x=groupe, y = Ep, group = groupe)) +
  theme_minimal() +
  geom_boxplot(color = "#941757") +
  ggtitle("Distribution du paramètre Ep dans chacun des groupes") +
  xlab("Groupes") +
  ylab("Taille en cm") +
  theme(plot.title = element_text(face = "bold")) +
  ggeasy::easy_center_title()