# Description --------------------------------------------------------------------

# Modèles de croissance taille chez les filles selon le modele de Jenss modifie :
# taille ~ Ap + Bp*age + Cp*exp(-age*Dp) + Ep*age^2

# author : @alannagenin

# Set-up -------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")
setwd("2_ModelesCroissance")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# importer les modeles qui sont stockees dans les dossiers "Modeles" et "Fonctions"
source("../Modeles/ModelesJenss.R")
source("../Fonctions/FonctionJenssModifieNls.R")
source("../Fonctions/FonctionJenssModifieSaemix.R")
source("../Fonctions/GraphiqueSaemix.R")

# creer dossier pour sauvegarder les resultats
newpath <- file.path("Graphiques/Taille")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")
newpath <- file.path("RData/Taille")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")

# packages
library(dplyr)
library(plyr)
library(ggplot2)
library(gridExtra)
library(nlme)
library(saemix)
library(matlab)

# ---------------------------------------------------------------------------------------------
# ------------------------- MODELE SAEMIX SUR LES FILLES --------------------------------------
# ---------------------------------------------------------------------------------------------

# table d'interet 
croiss_modele <- croiss_taille %>% filter(sexe == "F")

# identifiants de tous les enfants
enfants_filles <- unique(croiss_modele$num)

# modele de Jenss modifie en appellant la fonction nlsJenssModifieTaille du fichier Fonctions/FonctionJenssModifieNls.R
nlsJenssModifieFilles <- nlsJenssModifieTaille(data = croiss_modele,
                                            param = list(Ap = 50, Bp = .9, Cp = 20, Dp = .1, Ep = .001))
(coef_nlsJenssModifieFilles <- coef(nlsJenssModifieFilles))
# Ap            Bp            Cp            Dp            Ep 
# 50.2820663081  0.6394863159 23.1127919532  0.1119354356 -0.0006566095

# summary
nlsJenssModifieFilles ; summary(nlsJenssModifieFilles)
cat("AIC : ", AIC(nlsJenssModifieFilles), "\nBIC : ", BIC(nlsJenssModifieFilles))
# AIC :  65353.63 
# BIC :  65397.49

# Get the sum of the squared residuals.
sum(resid(nlsJenssModifieFilles)^2)
# 238581.8

# Get the confidence intervals on the chosen values of the coefficients.
confint(nlsJenssModifieFilles)
#         2.5%      97.5%
# Ap 50.0029349246 50.5591385587
# Bp  0.6148998260  0.6627518950
# Cp 22.2174319291 24.0742911232
# Dp  0.1045081905  0.1198410679
# Ep -0.0007764291 -0.0005315252

# ------------------------------- MEILLEUR MODELE ---------------------------------------
# meilleur modele : erreurs combined - avec covariances (pour les filles) ---------------

saemix.data <- saemixData(
  name.data = croiss_modele,
  header = TRUE,
  sep = ";",
  na = NA,
  name.group = c("num"),
  name.predictors = c("age_mois"),
  name.response = c("taille"),
  units=list(x = "mois", y = "cm", covariates = c("-")),
  name.X = "age_mois")

saemix.best.fit.filles <- saemixJenssModifie(
  sexe = "Filles",
  variable_anthropometrique = "taille",
  coefficients = coef_nlsJenssModifieFilles,
  distrib_param = "normal",
  estimation = "restimation",
  covariance = TRUE,
  covariable = FALSE,
  erreur = "combined",
  save.diag = TRUE,
  save.graph = TRUE
)

# AIC / BIC
aic.best.fit.filles <- AIC(saemix.best.fit.filles) ; bic.best.fit.filles <- BIC(saemix.best.fit.filles);
cat("AIC : ", aic.best.fit.filles, "\nBIC : ", bic.best.fit.filles)
# AIC :  46604.9 
# BIC :  46705.71

# log-vraisemblance
logLik(saemix.best.fit.filles)
# 'log Lik.' -23280.45 (df=22)

# ecart-type des residus
saemix.best.fit.filles@results@icwres %>% sd() %>% round(2) # 0.89

# sauvegarder dans un RData -------------------------------------------------------------------

JenssModifieTailleFilles <- saemix.best.fit.filles
save(JenssModifieTailleFilles, file = "RData/Taille/JenssModifieTailleFilles.RData")

# plot diagnostiques -------------------------------------------------------------------

# distribution des résidus
pdf("Graphiques/Taille/GraphiquesJenssModifieFilles/DistriResiduals.pdf")
saemix.plot.distribresiduals(saemix.best.fit.filles, level=1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit.filles@results@icpred %>% 
  bind_cols(saemix.best.fit.filles@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Taille/GraphiquesJenssModifieFilles/ScatterResiduals.pdf")
ggplot(data = residus) +
  theme_minimal() +
  geom_point(aes(x = icpred, y = icwres), pch = 1, colour = "#999997") +
  ylim(c(-6.5, 6.5)) +
  geom_hline(yintercept = 4, colour = "#941757", size = 0.9, linetype = "dashed") +
  geom_hline(yintercept = -4, colour = "#941757", size = 0.9, linetype = "dashed") +
  labs(title = "Résidus",
       x = "Prédiction individuelle",
       y = "Résidu individuel pondéré") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
dev.off()

# observation vs predictions
pdf("Graphiques/Taille/GraphiquesJenssModifieFilles/ObservationsVsPredictions.pdf", paper = "a4r", width = 11, height = 10)
plot(saemix.best.fit.filles, plot.type="observations.vs.predictions")
dev.off()

# VPC (visual predictive check)
pdf("Graphiques/Taille/GraphiquesJenssModifieFilles/VPC.pdf", paper = "a4r", width = 11, height = 10)
plot(saemix.best.fit.filles, plot.type = "vpc", pch = 1, col.pobs = "#999997",
     vpc.obs = T, vpc.method = "user", vpc.bin = 20,
     xlab = "Âge (mois)", ylab = "Poids (kg)",
     col.fillmed = "#c98ea1", col.lmed = "#941757",
     col.lobs = "#6A3D9A", col.lpi = "#6A3D9A",
     col.fillpi = "#CAB2D6")
dev.off()

# courbe de croissance et prediction
plot(saemix.best.fit.filles, plot.type = "both.fit")

# boxplot des parametres
plot(saemix.best.fit.filles, plot.type = "random.effects")

# correlation entre les parametres
plot(saemix.best.fit.filles, plot.type = "correlations")

# densite de chaque parametre
plot(saemix.best.fit.filles, plot.type = "marginal.distribution")

# plot du meilleur modele pour les filles -----------------------------------------------------

# ajout des predictions dans la base de donnees
croiss_modele <- croiss_modele %>%
  bind_cols(saemix.best.fit.filles["results"]["predictions"] %>%
              select(ipred) %>% dplyr::rename(., prediction = ipred)
  ) %>%
  relocate(prediction, .after = taille)

pdf(file = "Graphiques/Taille/GraphiquesJenssModifieFilles/GraphiqueJenssModifieSaemixFilles.pdf")
graphiqueSaemixTaille(nom = "JM",
                     data = croiss_modele,
                     identifiants = enfants_filles)
dev.off()
