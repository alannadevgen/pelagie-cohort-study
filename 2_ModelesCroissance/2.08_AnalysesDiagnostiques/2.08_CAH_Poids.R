# Description -----------------------------------------------------------------------------

# classification sur le modele Jenss adapte pour le poids

# author : @camillepliquet @alannagenin

## Settings -------------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")
setwd("2_ModelesCroissance")

# packages
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(rstatix)

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")
load("RData/Poids/JenssModifiePoidsFilles.RData")
load("RData/Poids/JenssModifiePoidsGarcons.RData")

# creer dossier pour sauvegarder les resultats
newpath <- file.path("Graphiques/Poids")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")
newpath <- file.path("Graphiques/Poids/CAH")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")

# Fusion des donnees filles et garcons -------------------------------------------------

croiss_modele <- croiss_poids

# recuperer les identifiants des filles et des garcons
num_filles <- croiss_modele %>% filter(sexe == "F") %>% select(num) %>% distinct()
num_garcons <- croiss_modele %>% filter(sexe == "M") %>% select(num) %>% distinct()

# fusionner les identifiants avec les resultats de la modelisation R2
param_filles <-  num_filles %>% 
  bind_cols(psi(JenssModifiePoidsFilles) %>% as.data.frame()) %>% 
  mutate(sexe = "F")
param_garcons <- num_garcons %>%
  bind_cols(psi(JenssModifiePoidsGarcons) %>% as.data.frame())%>% 
  mutate(sexe = "M")

# fusionner les params filles et garcons ensemble
param <- bind_rows(param_filles, param_garcons) %>% as.data.frame() 
colnames(param) <- c("num", "Ap", "Bp", "Cp", "Dp", "Ep", "sexe")

# suppression des parametres identifiant et sexe + centrage des donnees
param <- param %>% select(-num, -sexe)
param.cr <- scale(param, center=T, scale=T)

# Mise en place de la CAH ------------------------------------------------

# matrice des distances entre individus
d.param <- dist(param.cr)
# CAH - critère de Ward
# method = « ward.D2 » correspond au vrai critère de Ward
# utilisant le carré de la distance
cah.ward <- hclust(d.param,method="ward.D2")

# Dendrogramme ------------------------------------------------

## avec plot ----
plot(cah.ward, labels=FALSE, hang=-1,
     ylab="Distance", xlab= "Individus", main = "Dendrogramme pour le poids") # on visualise mieux comme ca
plot(cah.ward, labels=FALSE)

# dendrogramme avec matérialisation des groupes
rect.hclust(cah.ward,k = 5, border = "#941757")

#découpage en 5 groupes
groupes.cah <- cutree(cah.ward, k = 5)

## avec ggplot ----
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

# pour 5 classes 
pdf("Graphiques/Poids/CAH/CAH_poids5groupes.pdf", width = 12, height = 10)
fviz_dend(cah.ward, k = 5, show_labels = FALSE,
          k_colors = c("#540d6e", "#1982c4", "#8ac926", "#f86624", "#d81159"),
          ggtheme = theme_minimal(),
          ylab = "Distance", xlab = "Individus",
          main = "Dendrogramme pour le poids en 5 classes",
          rect = FALSE, rect_fill = FALSE, lwd = 2
) + my_theme
dev.off()

# pour 3 classes
pdf("Graphiques/Poids/CAH/CAH_poids3groupes.pdf", width = 12, height = 10)
fviz_dend(cah.ward, k = 3, show_labels = FALSE,
          k_colors = c("#440154", "#1F968B", "#86c239"),
          ggtheme = theme_minimal(),
          ylab = "Distance", xlab = "Individus",
          main = "Dendrogramme pour le poids en 3 classes",
          rect = FALSE, rect_fill = FALSE, lwd = 2
) + my_theme
dev.off()

# pour 2 classes
pdf("Graphiques/Poids/CAH/CAH_poids2groupes.pdf", width = 12, height = 10)
fviz_dend(cah.ward, k = 2, show_labels = FALSE,
          k_colors = c("#170C3A", "#BB3754"),
          ggtheme = theme_minimal(),
          ylab = "Distance", xlab = "Individus",
          main = "Dendrogramme pour le poids en 2 classes",
          rect = FALSE, rect_fill = FALSE, lwd = 2
) + my_theme
dev.off()

# Inertie ------------------------------------------------
inertie <- sort(cah.ward$height, decreasing = TRUE)

## avec plot ----
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie", main = "Sauts d'inertie")
points(c(2,3,5), inertie[c(2, 3, 5)], col = c("red3", "blue3", "green3"), cex = 2, lwd = 3)

## avec ggplot ----
# preparation des donnees
sauts_inertie <- data.frame(
  nb_classes = 1:20,
  inertie = inertie %>% head(20)
)
# graphique d'inertie
pdf("Graphiques/Poids/CAH/SautsInertiePoids.pdf", width = 14, height = 10)
ggplot(sauts_inertie, aes(x = nb_classes, y = inertie)) +
  theme_minimal() +
  geom_step(size = 2.5) +
  labs(
    title = "Sauts d'inertie",
    x = "Nombre de classes",
    y = "Inertie",
    caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Camille PLIQUET et Alanna GENIN"
    
  ) +
  geom_point(data = sauts_inertie %>% filter(nb_classes %in% c(2, 3, 5)),
             shape = 21, colour = c("#501D4CFF", "#A7185AFF", "#EB493EFF"),
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

# Caracterisation des groupes ------------------------------------------------

# ajouter le num de groupes dans les param :
param <- param %>%
  mutate(groupe = as.factor(groupes.cah))

# effectifs par groupe :
table(param$groupe)

# stats descriptives des groupes
catdes(param, num.var=6)

# Tests ANOVA
param %>% anova_test(Ap ~ groupe)
param %>% anova_test(Bp ~ groupe)
param %>% anova_test(Cp ~ groupe)
param %>% anova_test(Dp ~ groupe)
param %>% anova_test(Ep ~ groupe)

# Test du chi-2 ----------------------------------------------------------------------

# table intermediaire
num <- rbind(num_filles, num_garcons) %>% mutate(groupe = groupes.cah)
# fusionner avec la table d'interet
croiss_modele <- croiss_modele %>% left_join(num)

# test du chi-2
# region de rejet : stat de test >= quantile chi-2(K-1, 1-alpha)

# test groupe CAH - IMC mere
addmargins(table(croiss_modele$groupe, croiss_modele$IMC_mere))
(test_IMC_mere <- chisq.test(croiss_modele$groupe, croiss_modele$IMC_mere))
test_IMC_mere$statistic >= qchisq(0.95, df = 4)
# on rejette HO car X >= quantile

# test groupe CAH - statut tabagique mere
addmargins(table(croiss_modele$groupe, croiss_modele$tabac))
(test_tabac <- chisq.test(croiss_modele$groupe, croiss_modele$tabac))
test_tabac$statistic >= qchisq(0.95, df = 4)
# on rejette HO car X >= quantile


# Visualisation des courbes de croissance de chaque groupe --------------------------------

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

param_gp4 <- param %>% 
  filter(groupe == "4")%>%
  select("Ap", "Bp", "Cp", "Dp", "Ep")%>%
  colMeans()

param_gp5 <- param %>% 
  filter(groupe == "5")%>%
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
eq4 <- function(time){
  param_gp4[1] + param_gp4[2]*time + param_gp4[3] - param_gp4[3]*exp(-time*param_gp4[4]) + param_gp4[5]*time^2
}
eq5 <- function(time){
  param_gp5[1] + param_gp5[2]*time + param_gp5[3] - param_gp5[3]*exp(-time*param_gp5[4]) + param_gp5[5]*time^2
}

new_data <- data.frame(num = rep(1, 157), age_mois = 0:156, age_an = 0:156/12)
prediction <- data.frame(new_data, 
                         groupe1 = eq(new_data$age_mois),
                         groupe2 = eq2(new_data$age_mois),
                         groupe3 = eq3(new_data$age_mois),
                         groupe4 = eq4(new_data$age_mois),
                         groupe5 = eq5(new_data$age_mois)
)

groupes <- data.frame(num = unique(croiss_poids$num),
                      groupe = groupes.cah
)

croiss_poids <- croiss_poids %>% left_join(groupes)

pdf("Graphiques/Poids/CAH/CourbesPoidsCAH5groupes.pdf", width = 14, height = 10)
ggplot(data = croiss_poids, aes(x = age_an, y = poids)) + 
  theme_minimal() +
  geom_point(colour = "#bfbfbd", pch = 1) +
  geom_line(data = prediction, 
                aes(x = age_an, y = groupe1, colour = "g1", linetype = "g1"),
                size = 2)+
  geom_line(data = prediction, 
                aes(x = age_an, y = groupe2, colour = "g2", linetype = "g2"),
                size = 2,
                show.legend = TRUE)+
  geom_line(data = prediction, 
                aes(x = age_an, y = groupe3, colour = "g3", linetype = "g3"),
                size = 2,
                show.legend = TRUE)+
  geom_line(data = prediction, 
                aes(x = age_an, y = groupe4, colour = "g4", linetype = "g4"),
                size = 2,
                show.legend = TRUE)+
  geom_line(data = prediction, 
                aes(x = age_an, y = groupe5, colour = "g5", linetype = "g5"),
                size = 2,
                show.legend = TRUE) +
  scale_color_manual(name = "",
                     values = c("g1" = "#540d6e",
                                "g2" = "#1982c4",
                                "g3" = "#8ac926",
                                "g4" = "#f86624",
                                "g5" = "#d81159"),

                     labels = c("Groupe 1",
                                "Groupe 2",
                                "Groupe 3",
                                "Groupe 4",
                                "Groupe 5")
  ) +
  scale_linetype_manual(name = "",
                        values = c("g1" = "solid",
                                   "g2" = "longdash",
                                   "g3" = "dashed",
                                   "g4" = "dotdash",
                                   "g5" = "11"
                        ),
                        labels = c("Groupe 1",
                                   "Groupe 2",
                                   "Groupe 3",
                                   "Groupe 4",
                                   "Groupe 5")
  )  + 
  labs(
    title = "Courbes de croissance moyenne du poids pour chaque groupe de la CAH",
    x = "Âge (années)",
    y = "Poids (kg)",
    caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Camille PLIQUET et Alanna GENIN"
  ) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(0, unit = "pt"),
    legend.key.width = unit(3.5, "cm"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.text.x = element_text(face = "plain", size = 20, hjust = 0),
    axis.text.y = element_text(face = "plain", size = 20, hjust = 0),
    plot.caption = element_text(size = 14),
    strip.text.x = element_text(size = 20)
  )
dev.off()


ggplot() + 
  theme_minimal() +
  geom_point(data = croiss_poids %>% filter(groupe == 1), 
             aes(x = age_an, y = poids),
             colour = "#30203E", pch = 16) +
  geom_point(data = croiss_poids %>% filter(groupe == 2), 
             aes(x = age_an, y = poids),
             colour = "#40478B", pch = 16) +
  geom_point(data = croiss_poids %>% filter(groupe == 3), 
             aes(x = age_an, y = poids),
             colour = "#357BA2", pch = 16) +
  geom_point(data = croiss_poids %>% filter(groupe == 4), 
             aes(x = age_an, y = poids),
             colour = "#39ACAC", pch = 16) +
  geom_point(data = croiss_poids %>% filter(groupe == 5), 
             aes(x = age_an, y = poids),
             colour = "#4fc98f", pch = 16) +
  labs(
    title = "",
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

# boxplot sur la poids de naissance (Ap) par groupe : 
boxplot <- ggplot(param, aes(x=groupe, y = Ap, group = groupe)) +
  theme_minimal() +
  geom_boxplot(color = c("#440154", "#1F968B", "#86c239")) +
  labs(
    title = "Distribution du poids de naissance dans chacun des groupes de la CAH",
    x = "Groupes",
    y = "Poids (kg)",
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

pdf("Graphiques/Poids/CAH/BoxplotPoidsGroupesCAH.pdf", width = 11, height = 10)
boxplot
dev.off()