# Description --------------------------------------------------------------------

# Modèles de croissance poids chez les garcons selon le modele Reed du 2nd ordre :
# poids ~ Ap + Bp*age + Cp*log(age) + Dp/age + Ep/age^2

# author : @corentindaumont, @alannagenin

# Set-up -------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")
setwd("2_ModelesCroissance")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# importer les modeles qui sont stockees dans les dossiers "Modeles" et "Fonctions"
source("../Modeles/ModelesReed.R")
source("../Fonctions/FonctionReed2ndOrdreNls.R")
source("../Fonctions/FonctionReed2ndOrdreSaemix.R")
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

# modele de Reed 2nd ordre en appellant la fonction nlsReed2ndOrdrePoids du fichier Fonctions/FonctionReed2ndOrdreNls.R
nlsReed2Garcons <- nlsReed2Poids(data = croiss_modele,
                                       param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 1, Ep = 1))
(coef_nlsReed2Garcons <- coef(nlsReed2Garcons))
# Ap         Bp         Cp         Dp         Ep 
# 7.4967441  0.2420398 -0.4233814 -2.8788784  0.2372424 

# summary
nlsReed2Garcons ; summary(nlsReed2Garcons)
cat("AIC : ", AIC(nlsReed2Garcons), "\nBIC : ", BIC(nlsReed2Garcons))
# AIC :  52370.69 
# BIC :  52413.98

# Get the sum of the squared residuals.
sum(resid(nlsReed2Garcons)^2)
# 107989.2

# Get the confidence intervals on the chosen values of the coefficients.
confint(nlsReed2Garcons)
#         2.5%      97.5%
# Ap  7.1292184  7.8642700
# Bp  0.2386519  0.2454276
# Cp -0.5717914 -0.2749715
# Dp -3.2974041 -2.4603530
# Ep  0.2010734  0.2734114

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

saemix.best.fit.garcons <- saemixReed2(
  sexe = "Garcons",
  variable_anthropometrique = "poids",
  coefficients = coef_nlsReed2Garcons,
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
# AIC :  30491.65 
# BIC :  30592.94

# log-vraisemblance
logLik(saemix.best.fit.garcons)
# 'log Lik.' -15223.83 (df=22)

# ecart-type des residus
saemix.best.fit.garcons@results@icwres %>% sd() %>% round(2) # 0.89

# sauvegarder dans un RData -------------------------------------------------------------------

Reed2ndOrdrePoidsGarcons <- saemix.best.fit.garcons
save(Reed2ndOrdrePoidsGarcons, file = "RData/Poids/Reed2ndOrdrePoidsGarcons.RData")

# plot diagnostiques -------------------------------------------------------------------

# distribution des résidus
pdf("Graphiques/Poids/GraphiquesReed2ndOrdreGarcons/DistriResiduals.pdf")
saemix.plot.distribresiduals(saemix.best.fit.garcons, level=1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit.garcons@results@icpred %>% 
  bind_cols(saemix.best.fit.garcons@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Poids/GraphiquesReed2ndOrdreGarcons/ScatterResiduals.pdf")
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
pdf("Graphiques/Poids/GraphiquesReed2ndOrdreGarcons/ObservationsVsPredictions.pdf")
plot(saemix.best.fit.garcons, plot.type="observations.vs.predictions")
dev.off()

# VPC (visual predictive check)
pdf("Graphiques/Poids/GraphiquesReed2ndOrdreGarcons/VPC.pdf", paper = "a4r", width = 11, height = 10)
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

pdf(file = "Graphiques/Poids/GraphiquesReed2ndOrdreGarcons/GraphiqueReed2ndOrdreSaemixGarcons.pdf")
graphiqueSaemixPoids(nom = "R2",
                     data = croiss_modele,
                     identifiants = enfants_garcons)
dev.off()
