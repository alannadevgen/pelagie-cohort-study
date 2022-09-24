# Description --------------------------------------------------------------------------------

# Boxplots des residus des meilleurs modeles pour la taille
# --> permet de verifier l'adequation du modele aux donnees
# --> on veut des boxplots centrees sur l'axe des abscisses (en 0)

# author : @camillepliquet @alannagenin

# Set-up ------------------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")
setwd("2_ModelesCroissance")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")
load("RData/Taille/JenssModifieTaillePop.RData")
load("RData/Taille/JenssBayleyTaillePop.RData")
load("RData/Taille/Reed1erOrdreTaillePop.RData")
load("RData/Taille/Reed2ndOrdreTaillePop.RData")

# importer la fonction multiplot
source("../Fonctions/multiplot.R")

# creer dossier pour sauvegarder les resultats
newpath <- file.path("Graphiques/Taille")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")

# packages
library(ggplot2)
library(grid)

# Preparation des donnees --------------------------------------------------

# extraction des residus dans un vecteur
ResidusJenssBayley <- JenssBayleyTaillePop@results@icwres
ResidusJenssModifie <- JenssModifieTaillePop@results@icwres
ResidusReed1erOrdre <- Reed1erOrdreTaillePop@results@icwres
ResidusReed2ndOrdre <- Reed2ndOrdreTaillePop@results@icwres

# creation d'un dataframe contenant les differents age ainsi que les residus
data_boxplot <- data.frame(age_mois = round(croiss_taille$age_mois_no_zero),
                  age_an = croiss_taille$age_an_ent,
                  ResidusJenssBayley,
                  ResidusJenssModifie,
                  ResidusReed1erOrdre,
                  ResidusReed2ndOrdre)

## En utilisant l'age en mois -------------------------------------------------------

# plotJenssBayleyAgeMois <- ggplot(data_boxplot, aes(x=age_mois, y = ResidusJenssBayley, group = age_mois)) +
#   theme_minimal() +
#   geom_boxplot(color = "#941757") +
#   ggtitle("Jenss-Bayley") +
#   xlab("Âge (mois)") +
#   ylab("Résidus standardisés pour la taille") +
#   geom_hline(yintercept =  0) +
#   ggeasy::easy_center_title()
# plotJenssBayleyAgeMois 
# 
# plotJenssModifieAgeMois <- ggplot(data_boxplot, aes(x=age_mois, y = ResidusJenssModifie, group = age_mois)) +
#   theme_minimal() +
#   geom_boxplot(color = "#AB5676") +
#   ggtitle("Jenss modifié") +
#   xlab("Âge (mois)") +
#   ylab("Résidus standardisés pour la taille") +
#   geom_hline(yintercept =  0) +
#   ggeasy::easy_center_title()
# plotJenssModifieAgeMois
# 
# plotReed1erOrdreAgeMois <- ggplot(data_boxplot, aes(x=age_mois, y = ResidusReed1erOrdre, group = age_mois)) +
#   theme_minimal() +
#   geom_boxplot(color = "#1F78B4") +
#   ggtitle(bquote('Reed ' ~ 1^er ~'ordre')) +
#   xlab("Âge (mois)") +
#   ylab("Résidus standardisés pour la taille") +
#   geom_hline(yintercept =  0) +
#   ggeasy::easy_center_title()
# plotReed1erOrdreAgeMois
# 
# plotReed2ndOrdreAgeMois <- ggplot(data_boxplot, aes(x=age_mois, y = ResidusReed2ndOrdre, group = age_mois)) +
#   theme_minimal() +
#   geom_boxplot(color = "#A6CEE3") +
#   ggtitle(bquote('Reed' ~ 2^nd ~'ordre')) +
#   xlab("Âge (mois)") +
#   ylab("Résidus standardisés pour la taille") +
#   geom_hline(yintercept =  0) +
#   ggeasy::easy_center_title()
# plotReed2ndOrdreAgeMois
# 
# pdf("2.08_AnalysesDiagnostiques/BoxplotResidusTailleMois.pdf")
# multiplot(plotJenssBayleyAgeMois, plotReed1erOrdreAgeMois, plotJenssModifieAgeMois, plotReed2ndOrdreAgeMois, cols=2)
# dev.off()

## En utilisant l'age en annees  ---------------------------------------------------------------------

plotJenssBayleyAgeAnnees <- ggplot(data_boxplot, aes(x=age_an, y = ResidusJenssBayley, group = age_an)) +
  theme_minimal() +
  geom_boxplot(color = "#941757") +
  ggtitle("Jenss-Bayley") +
  xlab("Âge (années)") +
  ylab("Résidus standardisés pour la taille") +
  geom_hline(yintercept =  0) +
  theme(plot.title = element_text(face = "bold")) +
  ggeasy::easy_center_title()
plotJenssBayleyAgeAnnees

plotJenssModifieAgeAnnees <- ggplot(data_boxplot, aes(x=age_an, y = ResidusJenssModifie, group = age_an)) +
  theme_minimal() +
  geom_boxplot(color = "#AB5676") +
  ggtitle("Jenss adapté") +
  xlab("Âge (années)") +
  ylab("Résidus standardisés pour la taille") +
  geom_hline(yintercept =  0) +
  theme(plot.title = element_text(face = "bold")) +
  ggeasy::easy_center_title()
plotJenssModifieAgeAnnees

plotReed1erOrdreAgeAnnees <- ggplot(data_boxplot, aes(x=age_an, y = ResidusReed1erOrdre, group = age_an)) +
  theme_minimal() +
  geom_boxplot(color = "#1F78B4") +
  ggtitle(bquote(bold('Reed ' ~ 1^er ~'ordre'))) +
  xlab("Âge (années)") +
  ylab("Résidus standardisés pour la taille") +
  geom_hline(yintercept =  0) +
  theme(plot.title = element_text(face = "bold")) +
  ggeasy::easy_center_title()
plotReed1erOrdreAgeAnnees

plotReed2ndOrdreAgeAnnees <- ggplot(data_boxplot, aes(x=age_an, y = ResidusReed2ndOrdre, group = age_an)) +
  theme_minimal() +
  geom_boxplot(color = "#A6CEE3") +
  ggtitle(bquote(bold('Reed' ~ 2^nd ~'ordre'))) +
  xlab("Âge (années)") +
  ylab("Résidus standardisés pour la taille") +
  geom_hline(yintercept =  0) +
  ggeasy::easy_center_title() +
  theme(plot.title = element_text(face = "bold"))
plotReed2ndOrdreAgeAnnees

pdf("Graphiques/Taille/BoxplotResidusTailleAnnees.pdf", paper = "a4r", width = 11, height = 10)
multiplot(title = "Résidus standardisés des quatre modèles candidats pour la taille",
          plotJenssBayleyAgeAnnees,
          plotReed1erOrdreAgeAnnees,
          plotJenssModifieAgeAnnees,
          plotReed2ndOrdreAgeAnnees,
          fontsize = 16,
          cols=2)
dev.off()
