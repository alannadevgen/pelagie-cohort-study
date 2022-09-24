# Description --------------------------------------------------------------------

# Modèles de croissance poids chez les garcons selon le modele Reed du 1er ordre :
# poids ~ Ap + Bp*age + Cp*log(age) + Dp/age

# author : @camillepliquet, @alannagenin

# Set-up -------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")
setwd("2_ModelesCroissance")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# importer les modeles qui sont stockees dans les dossiers "Modeles" et "Fonctions"
source("../Modeles/ModelesReed.R")
source("../Fonctions/FonctionReed1erOrdreNls.R")
source("../Fonctions/FonctionReed1erOrdreSaemix.R")
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
# ------------------------- MODELE SAEMIX SUR LES GARCONS --------------------------------------
# ---------------------------------------------------------------------------------------------

# table d'interet 
croiss_modele <- croiss_poids %>% filter(sexe == "M")

# identifiants de tous les enfants
enfants_garcons <- unique(croiss_modele$num)

# modele de Reed 1er ordre en appellant la fonction nlsReed1erOrdrePoids du fichier Fonctions/FonctionReed1erOrdreNls.R
nlsReed1Garcons <- nlsReed1Poids(data = croiss_modele,
                                       param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 1))
coef_nlsReed1Garcons <- coef(nlsReed1Garcons)
# Ap         Bp         Cp         Dp 
# 6.7374371  0.2722867 -0.5888545 -0.4879297 

# summary
nlsReed1Garcons ; summary(nlsReed1Garcons)
cat("AIC : ", AIC(nlsReed1Garcons), "\nBIC : ", BIC(nlsReed1Garcons))
# AIC :  52532.74 
# BIC :  52568.82

# Get the sum of the squared residuals.
sum(resid(nlsReed1Garcons)^2)
# 109767.5

# Get the confidence intervals on the chosen values of the coefficients.
confint(nlsReed1Garcons)
#         2.5%      97.5%
# Ap  5.3206614  5.7519026
# Bp  0.2268441  0.2325215
# Cp  0.1944538  0.3962699
# Dp -0.2059881 -0.1026454

# ------------------------------- MEILLEUR MODELE ---------------------------------------
# meilleur modele : erreurs combined - avec covariances (pour les garcons) ---------------

saemix.data <- saemixData(
  name.data = croiss_modele,
  header = TRUE,
  sep = ";",
  na = NA,
  name.group = c("num"),
  name.predictors = c("age_mois_no_zero"),
  name.response = c("poids"),
  units=list(x = "mois", y = "kg", covariates = c("-")),
  name.X = "age_mois_no_zero")

saemix.best.fit.garcons <- saemixReed1(
  sexe = "Garcons",
  variable_anthropometrique = "poids",
  coefficients = coef_nlsReed1Garcons,
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
# AIC :  29810.85 
# BIC :  29884.52

# log-vraisemblance
logLik(saemix.best.fit.garcons)
# 'log Lik.' -14889.43 (df=16)

# ecart-type des residus
saemix.best.fit.garcons@results@icwres %>% sd() %>% round(2) # 0.90

# sauvegarder dans un RData -------------------------------------------------------------------

Reed1erOrdrePoidsGarcons <- saemix.best.fit.garcons
save(Reed1erOrdrePoidsGarcons, file = "RData/Poids/Reed1erOrdrePoidsGarcons.RData")

# plot diagnostics -------------------------------------------------------------------

# distribution des residus
pdf("Graphiques/Poids/GraphiquesReed1erOrdreGarcons/DistriResiduals.pdf")
saemix.plot.distribresiduals(saemix.best.fit.garcons, level=1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit.garcons@results@icpred %>% 
  bind_cols(saemix.best.fit.garcons@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Poids/GraphiquesReed1erOrdreGarcons/ScatterResiduals.pdf")
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
pdf("Graphiques/Poids/GraphiquesReed1erOrdreGarcons/ObservationsVsPredictions.pdf")
plot(saemix.best.fit.garcons, plot.type="observations.vs.predictions")
dev.off()

# VPC (visual predictive check)
pdf("Graphiques/Poids/GraphiquesReed1erOrdreGarcons/VPC.pdf", paper = "a4r", width = 11, height = 10)
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

pdf(file = "Graphiques/Poids/GraphiquesReed1erOrdreGarcons/GraphiqueReed1erOrdreSaemixGarcons.pdf")
graphiqueSaemixPoids(nom = "R1",
                     data = croiss_modele,
                     identifiants = enfants_garcons)
dev.off()
