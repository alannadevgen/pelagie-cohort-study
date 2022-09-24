# Description --------------------------------------------------------------------

# Modèles de croissance taille chez les filles selon le modele de Jenss-Bayley :
# taille ~ Ap + Bp*age + Cp*exp(-age*Dp)

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

# modele de Jenss-Bayley en appellant la fonction nlsJenssBayleyTaille du fichier Fonctions/FonctionJenssBayleyNls.R
nlsJenssBayleyFilles <- nlsJenssBayleyTaille(data = croiss_modele,
                                            param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 0.1))
(coef_nlsJenssBayleyFilles <- coef(nlsJenssBayleyFilles))
# Ap          Bp          Cp          Dp 
# 50.77543318  0.50797185 28.20491222  0.08413015 

# summary
nlsJenssBayleyFilles ; summary(nlsJenssBayleyFilles)
cat("AIC : ", AIC(nlsJenssBayleyFilles), "\nBIC : ", BIC(nlsJenssBayleyFilles))
# AIC :  65438.95 
# BIC :  65475.51

# Get the sum of the squared residuals.
sum(resid(nlsJenssBayleyFilles)^2)
# 240473.6

# Get the confidence intervals on the chosen values of the coefficients.
confint(nlsJenssBayleyFilles)
#         2.5%      97.5%
# Ap 50.52310636 51.02654904
# Bp  0.50417628  0.51170353
# Cp 27.77373837 28.64042883
# Dp  0.08112775  0.08723424

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

saemix.best.fit.filles <- saemixJenssBayley(
  sexe = "Filles",
  variable_anthropometrique = "taille",
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
# AIC :  46740.67 
# BIC :  46813.98

# log-vraisemblance
logLik(saemix.best.fit.filles)
# 'log Lik.' -23354.33 (df=16)

# ecart-type des residus
saemix.best.fit.filles@results@icwres %>% sd() %>% round(2) # 0.90

# sauvegarder dans un RData -------------------------------------------------------------------

JenssBayleyTailleFilles <- saemix.best.fit.filles
save(JenssBayleyTailleFilles, file = "RData/Taille/JenssBayleyTailleFilles.RData")

# plot diagnostiques -------------------------------------------------------------------

# distribution des résidus
pdf("Graphiques/Taille/GraphiquesJenssBayleyFilles/DistriResiduals.pdf")
saemix.plot.distribresiduals(saemix.best.fit.filles, level=1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit.filles@results@icpred %>% 
  bind_cols(saemix.best.fit.filles@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Taille/GraphiquesJenssBayleyFilles/ScatterResiduals.pdf")
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
pdf("Graphiques/Taille/GraphiquesJenssBayleyFilles/ObservationsVsPredictions.pdf", paper = "a4r", width = 11, height = 10)
plot(saemix.best.fit.filles, plot.type="observations.vs.predictions")
dev.off()

# VPC (visual predictive check)
pdf("Graphiques/Taille/GraphiquesJenssBayleyFilles/VPC.pdf", paper = "a4r", width = 11, height = 10)
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

pdf(file = "Graphiques/Taille/GraphiquesJenssBayleyFilles/GraphiqueJenssBayleySaemixFilles.pdf")
graphiqueSaemixTaille(nom = "JB",
                     data = croiss_modele,
                     identifiants = enfants_filles)
dev.off()
