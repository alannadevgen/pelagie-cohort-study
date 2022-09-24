# Description --------------------------------------------------------------------

# Modèles de croissance poids chez les garcons selon le modele de Jenss-Bayley :
# poids ~ Ap + Bp*age + Cp*exp(-age*Dp)

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
source("../Fonctions/FonctionJenssBayleyNls.R")
source("../Fonctions/FonctionJenssBayleySaemix.R")
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
# ------------------------- MODELE SAEMIX SUR LES GARCONS -------------------------------------
# ---------------------------------------------------------------------------------------------

# table d'interet 
croiss_modele <- croiss_poids %>% filter(sexe == "M")

# identifiants de tous les enfants
enfants_garcons <- unique(croiss_modele$num)

# modele de Jenss-Bayley en appellant la fonction nlsJenssBayleyPoids du fichier Fonctions/FonctionJenssBayleyNls.R
nlsJenssBayleyGarcons <- nlsJenssBayleyPoids(data = croiss_modele,
                                             param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 1))
(coef_nlsJenssBayleyGarcons <- coef(nlsJenssBayleyGarcons))
# Ap        Bp        Cp        Dp 
# 3.2680520 0.2325936 3.2737989 0.4080625 

# summary
nlsJenssBayleyGarcons ; summary(nlsJenssBayleyGarcons)
cat("AIC : ", AIC(nlsJenssBayleyGarcons), "\nBIC : ", BIC(nlsJenssBayleyGarcons))
# AIC :  52306.17 
# BIC :  52342.25

# Get the sum of the squared residuals.
sum(resid(nlsJenssBayleyGarcons)^2)
# 107319.1

# Get the confidence intervals on the chosen values of the coefficients.
confint(nlsJenssBayleyGarcons)
#       2.5%     97.5%
# Ap 3.0608375 3.4737679
# Bp 0.2310123 0.2341656
# Cp 3.0371355 3.5105302
# Dp 0.3573116 0.4686317

# ------------------------------- MEILLEUR MODELE ----------------------------------------
# meilleur modele : erreurs combined - avec covariances - normal -------------------------

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

saemix.best.fit.garcons <- saemixJenssBayley(
  sexe = "Garcons",
  variable_anthropometrique = "poids",
  coefficients = coef_nlsJenssBayleyGarcons,
  distrib_param = "normal",
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
# normal
# AIC :  27670.95 
# BIC :  27744.61

# log-vraisemblance
logLik(saemix.best.fit.garcons)
# 'log Lik.' -13819.48 (df=16)

# ecart-type des residus
saemix.best.fit.garcons@results@icwres %>% sd() %>% round(2) # 0.89

# sauvegarder dans un RData -------------------------------------------------------------------

JenssBayleyPoidsGarcons <- saemix.best.fit.garcons
save(JenssBayleyPoidsGarcons, file = "RData/Poids/JenssBayleyPoidsGarcons.RData")

# plot diagnostiques -------------------------------------------------------------------

# distribution des résidus
pdf("Graphiques/Poids/GraphiquesJenssBayleyGarcons/DistriResiduals.pdf")
saemix.plot.distribresiduals(saemix.best.fit.garcons, level=1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit.garcons@results@icpred %>% 
  bind_cols(saemix.best.fit.garcons@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Poids/GraphiquesJenssBayleyGarcons/ScatterResiduals.pdf")
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
pdf("Graphiques/Poids/GraphiquesJenssBayleyGarcons/ObservationsVsPredictions.pdf")
plot(saemix.best.fit.garcons, plot.type="observations.vs.predictions")
dev.off()

# VPC (visual predictive check)
pdf("Graphiques/Poids/GraphiquesJenssBayleyGarcons/VPC.pdf", paper = "a4r", width = 11, height = 10)
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

pdf(file = "Graphiques/Poids/GraphiquesJenssBayleyGarcons/GraphiqueJenssBayleySaemixGarcons.pdf")
graphiqueSaemixPoids(nom = "JB",
                     data = croiss_modele,
                     identifiants = enfants_garcons)
dev.off()
