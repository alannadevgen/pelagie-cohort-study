# Description --------------------------------------------------------------------

# Modèles de croissance poids chez les filles selon le modele de Jenss-Bayley :
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
# ------------------------- MODELE SAEMIX SUR LES FILLES --------------------------------------
# ---------------------------------------------------------------------------------------------

# table d'interet 
croiss_modele <- croiss_poids %>% filter(sexe == "F")

# identifiants de tous les enfants
enfants_filles <- unique(croiss_modele$num)

# modele de Jenss-Bayley en appellant la fonction nlsJenssBayleyPoids du fichier Fonctions/FonctionJenssBayleyNls.R
nlsJenssBayleyFilles <- nlsJenssBayleyPoids(data = croiss_modele,
                                            param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 1))
(coef_nlsJenssBayleyFilles <- coef(nlsJenssBayleyFilles))
# Ap        Bp        Cp        Dp 
# 3.1894565 0.2420348 2.5133457 0.4487598 

# summary
nlsJenssBayleyFilles ; summary(nlsJenssBayleyFilles)
cat("AIC : ", AIC(nlsJenssBayleyFilles), "\nBIC : ", BIC(nlsJenssBayleyFilles))
# AIC :  52673.05 
# BIC :  52709.1

# Get the sum of the squared residuals.
sum(resid(nlsJenssBayleyFilles)^2)
# 113265.4

# Get the confidence intervals on the chosen values of the coefficients.
confint(nlsJenssBayleyFilles)
#         2.5%      97.5%
# Ap 2.9731589 3.4038718
# Bp 0.2404498 0.2436100
# Cp 2.2677340 2.7590781
# Dp 0.3779863 0.5388745

# ------------------------------- MEILLEUR MODELE ---------------------------------------
# meilleur modele : erreurs combined - avec covariances (pour les filles) ---------------

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

saemix.best.fit.filles <- saemixJenssBayley(
  sexe = "Filles",
  variable_anthropometrique = "poids",
  coefficients = coef_nlsJenssBayleyFilles,
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
# normal
# AIC :  27377.3 
# BIC :  27450.61

# log-vraisemblance
logLik(saemix.best.fit.filles)
# 'log Lik.' -13672.65 (df=16)

# ecart-type des residus
saemix.best.fit.filles@results@icwres %>% sd() %>% round(2) # 0.90

# sauvegarder dans un RData -------------------------------------------------------------------

JenssBayleyPoidsFilles <- saemix.best.fit.filles
save(JenssBayleyPoidsFilles, file = "RData/Poids/JenssBayleyPoidsFilles.RData")

# plot diagnostiques -------------------------------------------------------------------

# distribution des résidus
pdf("Graphiques/Poids/GraphiquesJenssBayleyFilles/DistriResiduals.pdf", paper = "A4")
saemix.plot.distribresiduals(saemix.best.fit.filles, level=1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit.filles@results@icpred %>% 
  bind_cols(saemix.best.fit.filles@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Poids/GraphiquesJenssBayleyFilles/ScatterResiduals.pdf")
ggplot(data = residus) +
  theme_minimal() +
  geom_point(aes(x = icpred, y = icwres), pch = 1, colour = "#999997") +
  ylim(c(-6.5, 6.5)) +
  geom_hline(yintercept = 4, colour = "#941757", size = 0.9, linetype = "dashed") +
  geom_hline(yintercept = -4, colour = "#941757", size = 0.9, linetype = "dashed") +
  labs(title = "Résidus",
       x = "Prédiction individuelle",
       y = "Résidu individuel pondéré",
       caption = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN"
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
dev.off()

# observation vs predictions
pdf("Graphiques/Poids/GraphiquesJenssBayleyFilles/ObservationsVsPredictions.pdf", paper = "a4r", width = 11, height = 10)
plot(saemix.best.fit.filles, plot.type="observations.vs.predictions")
dev.off()

# VPC (visual predictive check)
pdf("Graphiques/Poids/GraphiquesJenssBayleyFilles/VPC.pdf", paper = "a4r", width = 11, height = 10)
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
  relocate(prediction, .after = poids)

pdf(file = "Graphiques/Poids/GraphiquesJenssBayleyFilles/GraphiqueJenssBayleySaemixFilles.pdf")
graphiqueSaemixPoids(nom = "JB",
                     data = croiss_modele,
                     identifiants = enfants_filles)
dev.off()
