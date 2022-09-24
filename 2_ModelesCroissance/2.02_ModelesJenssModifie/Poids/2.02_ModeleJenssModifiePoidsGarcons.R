# Description --------------------------------------------------------------------

# Modèles de croissance poids chez les garcons selon le modele de Jenss modifie :
# poids ~ Ap + Bp*age + Cp*exp(-age*Dp) + Ep*age^2

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
newpath <- file.path("Graphiques/Poids")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")
newpath <- file.path("RData/Poids")
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
croiss_modele <- croiss_poids %>% filter(sexe == "M")

# identifiants de tous les enfants
enfants_garcons <- unique(croiss_modele$num)

# modele de Jenss modifie en appellant la fonction nlsJenssModifiePoids du fichier Fonctions/FonctionJenssModifieNls.R
nlsJenssModifieGarcons <- nlsJenssModifiePoids(data = croiss_modele,
                                            param = list(Ap = 3, Bp = 0, Cp = 5, Dp = 0.1, Ep = 0))
(coef_nlsJenssModifieGarcons <- coef(nlsJenssModifieGarcons))
# Ap           Bp           Cp           Dp           Ep 
# 3.4621886177 0.0977427159 6.5814096308 0.1393439955 0.0008037047

# summary
nlsJenssModifieGarcons ; summary(nlsJenssModifieGarcons)
cat("AIC : ", AIC(nlsJenssModifieGarcons), "\nBIC : ", BIC(nlsJenssModifieGarcons))
# AIC :  51552.99 
# BIC :  51596.28

# Get the sum of the squared residuals.
# print(sum(resid(nlsJenssModifieGarcons)^2))
# 167832.6

# Get the confidence intervals on the chosen values of the coefficients.
# print(confint(nlsJenssModifieGarcons))
#         2.5%      97.5%
# Ap  2.9406482  3.4519418
# Bp -0.2500806 -0.2471268
# Cp  2.0203450  2.5957574
# Dp  0.4096901  0.6410665

# -------------------------------- MEILLEUR MODELE ---------------------------------------
# meilleur modele : erreurs combined - avec covariances (pour les garcons) ---------------

saemix.data <- saemixData(
  name.data = croiss_modele,
  header = TRUE,
  sep = ";",
  na = NA,
  name.group = c("num"),
  name.predictors = c("age_mois"),
  name.response = c("poids"),
  units=list(x = "mois", y = "kg", covariates = c("-")),
  name.X = "age_mois")

saemix.best.fit.garcons <- saemixJenssModifie(
  sexe = "Garcons",
  variable_anthropometrique = "poids",
  coefficients = coef_nlsJenssModifieGarcons,
  distrib_param = "log-normal",
  estimation = "restimation",
  covariance = TRUE,
  covariable = FALSE,
  erreur = "combined",
  save.diag = TRUE,
  save.graph = TRUE
)

# AIC / BIC
aic.best.fit.garcons <- AIC(saemix.best.fit.garcons) ; bic.best.fit.garcons <- BIC(saemix.best.fit.garcons);
cat("AIC : ", aic.best.fit.garcons, "\nBIC : ", bic.best.fit.garcons)
# AIC :  22026.66 
# BIC :  22127.95

# log-vraisemblance
logLik(saemix.best.fit.garcons)
# 'log Lik.' -10991.33 (df=22)

# ecart-type des residus
saemix.best.fit.garcons@results@icwres %>% sd() %>% round(2) # 0.90

# sauvegarder dans un RData -------------------------------------------------------------------

JenssModifiePoidsGarcons <- saemix.best.fit.garcons
save(JenssModifiePoidsGarcons, file = "RData/Poids/JenssModifiePoidsGarcons.RData")

# plot diagnostiques -------------------------------------------------------------------

# distribution des résidus
pdf("Graphiques/Poids/GraphiquesJenssModifieGarcons/DistriResiduals.pdf")
saemix.plot.distribresiduals(saemix.best.fit.garcons, level=1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit.garcons@results@icpred %>% 
  bind_cols(saemix.best.fit.garcons@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Poids/GraphiquesJenssModifieGarcons/ScatterResiduals.pdf")
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
pdf("Graphiques/Poids/GraphiquesJenssModifieGarcons/ObservationsVsPredictions.pdf")
plot(saemix.best.fit.garcons, plot.type="observations.vs.predictions")
dev.off()

# VPC (visual predictive check)
pdf("Graphiques/Poids/GraphiquesJenssModifieGarcons/VPC.pdf", paper = "a4r", width = 11, height = 10)
plot(saemix.best.fit.garcons, plot.type="vpc", pch = 1, col.pobs = "#999997",
     vpc.obs = T, vpc.method = "user", vpc.bin = 20,
     xlab = "Âge (mois)", ylab = "Poids (kg)",
     col.fillmed = "#c98ea1", col.lmed = "#941757",
     col.lobs = "#6A3D9A", col.lpi = "#6A3D9A",
     col.fillpi = "#CAB2D6")
dev.off()

# courbe de croissance et prediction
plot(saemix.best.fit.garcons, plot.type = "both.fit")

# boxplot des parametres
plot(saemix.best.fit.garcons, plot.type = "random.effects")

# correlation entre les parametres
plot(saemix.best.fit.garcons, plot.type = "correlations")

# densite de chaque parametre
plot(saemix.best.fit.garcons, plot.type = "marginal.distribution")

# plot du meilleur modele pour les garcons -----------------------------------------------------

# ajout des predictions dans la base de donnees
croiss_modele <- croiss_modele %>%
  bind_cols(saemix.best.fit.garcons["results"]["predictions"] %>%
              select(ipred) %>% dplyr::rename(., prediction = ipred)
  ) %>%
  relocate(prediction, .after = poids)

pdf(file = "Graphiques/Poids/GraphiquesJenssModifieGarcons/GraphiqueJenssModifieSaemixGarcons.pdf")
graphiqueSaemixPoids(nom = "JM",
                     data = croiss_modele,
                     identifiants = enfants_garcons)
dev.off()
