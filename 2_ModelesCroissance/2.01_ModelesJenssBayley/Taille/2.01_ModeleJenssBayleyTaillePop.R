# Description --------------------------------------------------------------------

# Modèles de croissance taille population entiere selon le modele de Jenss-Bayley :
# taille ~ Ap + Bp*age + Cp*exp(-age*Dp)

# author : @alannagenin

# Set-up -------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")
setwd("2_ModelesCroissance")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# importer les modeles qui sont stockees dans le dossier "Modeles"
source("../Modeles/ModelesJenss.R")
source("../Fonctions/FonctionJenssBayleyNls.R")
source("../Fonctions/FonctionJenssBayleySaemix.R")
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

# Modele de Jenss-Bayley --------------------------------------------------------------

# table d'interet
croiss_modele <- croiss_taille

# identifiants de tous les enfants
enfants <- unique(croiss_modele$num)

# modele de Jenss-Bayley en appellant la fonction nlsJenssBayleyTaille du fichier Fonctions/FonctionJenssBayleyNls.R
nlsJenssBayley <- nlsJenssBayleyTaille(data = croiss_taille)
(coef_nlsJenssBayley <- coef(nlsJenssBayley))
# Ap         Bp         Cp         Dp 
# 51.1528798  0.5032610 28.3728994  0.0881055 

# summary
nlsJenssBayley ; summary(nlsJenssBayley)
cat("AIC : ", AIC(nlsJenssBayley), "\nBIC : ", BIC(nlsJenssBayley))
# AIC :  131752.3 
# BIC :  131792.4

# Get the sum of the squared residuals.
sum(resid(nlsJenssBayley)^2)
# 483285.4

# Get the confidence intervals on the chosen values of the coefficients.
confint(nlsJenssBayley)
#         2.5%       97.5%
# Ap 50.97243687 51.33268279
# Bp  0.50065594  0.50583696
# Cp 28.07632537 28.67127836
# Dp  0.08589341  0.09037064

# -------------------------------- MEILLEUR MODELE ----------------------------------------
# meilleur modele : erreurs combined - avec covariances et covariable sexe ----------------

saemix.data <- saemixData(
  name.data = croiss_modele,
  header = TRUE,
  sep = ";",
  na = NA,
  name.X = "age_mois",
  name.group = "num",
  name.predictors = "age_mois",
  name.response = "taille",
  name.covariates = "sexe",
  units=list(x = "mois", y = "cm", covariates = c("-")),
)

saemix.best.fit <- saemixJenssBayley(
  variable_anthropometrique = "taille",
  data = saemix.data,
  coefficients = coef_nlsJenssBayley,
  distrib_param = "normal",
  estimation = "restimation",
  covariance = TRUE,
  covariable = TRUE,
  erreur = "combined",
  save.diag = TRUE,
  save.graph = TRUE
)

# AIC / BIC
aic.best.fit <- AIC(saemix.best.fit) ; bic.best.fit <- BIC(saemix.best.fit);
cat("AIC : ", aic.best.fit, "\nBIC : ", bic.best.fit)
# AIC :  95313.06 
# BIC :  95418.78

# log-vraisemblance
logLik(saemix.best.fit)
# 'log Lik.' -47636.53 (df=20)

# ecart-type des residus
saemix.best.fit@results@icwres %>% sd() # 0.89

# sauvegarder dans un RData -------------------------------------------------------------------

JenssBayleyTaillePop <- saemix.best.fit
save(JenssBayleyTaillePop, file = "RData/Taille/JenssBayleyTaillePop.RData")

# plot diagnostiques -------------------------------------------------------------------

# distribution des résidus
pdf("Graphiques/Taille/GraphiquesJenssBayley/DistriResiduals.pdf")
saemix.plot.distribresiduals(saemix.best.fit, level = 1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit@results@ipred %>% 
  bind_cols(saemix.best.fit@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Taille/GraphiquesJenssBayley/ScatterResiduals.pdf")
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
pdf("Graphiques/Taille/GraphiquesJenssBayley/ObservationsVsPredictions.pdf", paper = "a4r", width = 11, height = 10)
plot(saemix.best.fit, plot.type = "observations.vs.predictions")
dev.off()

# VPC (visual predictive check)
pdf("Graphiques/Taille/GraphiquesJenssBayley/VPC.pdf", paper = "a4r", width = 11, height = 10)
plot(saemix.best.fit, plot.type = "vpc", pch = 1, col.pobs = "#999997",
     vpc.obs = T, vpc.method = "user", vpc.bin = 20,
     xlab = "Âge (mois)", ylab = "Poids (kg)",
     col.fillmed = "#c98ea1", col.lmed = "#941757",
     col.lobs = "#6A3D9A", col.lpi = "#6A3D9A",
     col.fillpi = "#CAB2D6")
dev.off()

# courbe de croissance et prediction
plot(saemix.best.fit, plot.type = "both.fit")

# boxplot des parametres
plot(saemix.best.fit, plot.type = "random.effects")

# correlation entre les parametres
plot(saemix.best.fit, plot.type = "correlations")

# densite de chaque parametre
plot(saemix.best.fit, plot.type = "marginal.distribution")

# distribution des parametres selon la/les covariable(s) 
plot(saemix.best.fit, plot.type = "parameters.versus.covariates")

# distribution des effets aleatoires selon la/les covariable(s) 
plot(saemix.best.fit, plot.type = "randeff.versus.covariates")

# prediction du modele ----------------------------------------------------------------

# ajout des predictions dans la base de donnees
croiss_modele <- croiss_modele %>%
  bind_cols(saemix.best.fit["results"]["predictions"] %>%
              select(icpred) %>% dplyr::rename(., prediction = icpred)
  ) %>%
  relocate(prediction, .after = taille)

# graphiques de tous les enfants ---------------------------------------------------------

pdf(file = "Graphiques/Taille/GraphiquesJenssBayley/GraphiqueJenssBayleySaemix.pdf")
graphiqueSaemixTaille(nom = "JB",
                      data = croiss_modele,
                      identifiants = enfants)
dev.off()
