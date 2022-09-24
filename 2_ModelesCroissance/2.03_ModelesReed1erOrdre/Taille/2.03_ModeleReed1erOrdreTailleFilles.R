# Description --------------------------------------------------------------------

# Modeles de croissance taille chez les filles selon le modele Reed du 1er ordre :
# taille ~ Ap + Bp*age + Cp*log(age) + Dp/age

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

# modele de Reed 1er ordre en appellant la fonction nlsReed1erOrdreTaille du fichier Fonctions/FonctionReed1erOrdreNls.R
nlsReed1Filles <- nlsReed1Taille(data = croiss_modele,
                                       param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 1))
(coef_nlsReed1Filles <- coef(nlsReed1Filles))
# Ap         Bp         Cp         Dp 
# 48.6874845  0.4257127  8.4791702  2.0451931

# summary
nlsReed1Filles ; summary(nlsReed1Filles)
cat("AIC : ", AIC(nlsReed1Filles), "\nBIC : ", BIC(nlsReed1Filles))
# AIC :  65585.94 
# BIC :  65622.49

# Get the sum of the squared residuals.
sum(resid(nlsReed1Filles)^2)
# 243691.7

# Get the confidence intervals on the chosen values of the coefficients.
confint(nlsReed1Filles)
#         2.5%      97.5%
# Ap 48.3708459 49.0041231
# Bp  0.4219016  0.4295239
# Cp  8.3321650  8.6261753
# Dp  1.9699672  2.1204190

# ------------------------------- MEILLEUR MODELE ---------------------------------------
# meilleur modele : erreurs combined - avec covariances (pour les filles) ---------------

saemix.data <- saemixData(
  name.data = croiss_modele,
  header = TRUE,
  sep = ";",
  na = NA,
  name.group = c("num"),
  name.predictors = c("age_mois_no_zero"),
  name.response = c("taille"),
  units=list(x = "mois", y = "cm", covariates = c("-")),
  name.X = "age_mois_no_zero")

saemix.best.fit.filles <- saemixReed1(
  sexe = "Filles",
  variable_anthropometrique = "taille",
  coefficients = coef_nlsReed1Filles,
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
# AIC :  48237.2 
# BIC :  48310.51

# log-vraisemblance
logLik(saemix.best.fit.filles)
# 'log Lik.' -24102.6 (df=16)

# ecart-type des residus
saemix.best.fit.filles@results@icwres %>% sd() %>% round(2) # 0.92

# sauvegarder dans un RData -------------------------------------------------------------------

Reed1erOrdreTailleFilles <- saemix.best.fit.filles
save(Reed1erOrdreTailleFilles, file = "RData/Taille/Reed1erOrdreTailleFilles.RData")

# plot diagnostics -------------------------------------------------------------------

# distribution des residus
pdf("Graphiques/Taille/GraphiquesReed1erOrdreFilles/DistriResiduals.pdf")
saemix.plot.distribresiduals(saemix.best.fit.filles, level=1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit.filles@results@icpred %>% 
  bind_cols(saemix.best.fit.filles@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Taille/GraphiquesReed1erOrdreFilles/ScatterResiduals.pdf")
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
pdf("Graphiques/Taille/GraphiquesReed1erOrdreFilles/ObservationsVsPredictions.pdf", paper = "a4r", width = 11, height = 10)
plot(saemix.best.fit.filles, plot.type="observations.vs.predictions")
dev.off()

# VPC (visual predictive check)
pdf("Graphiques/Taille/GraphiquesReed1erOrdreFilles/VPC.pdf", paper = "a4r", width = 11, height = 10)
plot(saemix.best.fit.filles, plot.type = "vpc", pch = 1, col.pobs = "#999997",
     vpc.obs = T, vpc.method = "user", vpc.bin = 20,
     xlab = "Âge (mois)", ylab = "Taille (cm)",
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

pdf(file = "Graphiques/Taille/GraphiquesReed1erOrdreFilles/GraphiqueReed1erOrdreSaemixFilles.pdf")
graphiqueSaemixTaille(nom = "R1",
                     data = croiss_modele,
                     identifiants = enfants_filles)
dev.off()
