# Description --------------------------------------------------------------------

# Modèles de croissance taille population entiere selon le modele Reed du 1er ordre :
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
# ------------------------- MODELE SAEMIX POP ENTIERE --------------------------------------
# ---------------------------------------------------------------------------------------------

# table d'interet 
croiss_modele <- croiss_taille

# identifiants de tous les enfants
enfants <- unique(croiss_modele$num)

# modele de Reed 1er ordre en appellant la fonction nlsReed1erOrdreTaille du fichier Fonctions/FonctionReed1erOrdreNls.R
nlsReed1 <- nlsReed1Taille(data = croiss_modele,
                                 param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 1))
(coef_nlsReed1 <- coef(nlsReed1))
# Ap         Bp         Cp         Dp 
# 49.324845  0.416485  8.578735  2.036614 

# summary
nlsReed1 ; summary(nlsReed1)
cat("AIC : ", AIC(nlsReed1), "\nBIC : ", BIC(nlsReed1))
# AIC :  131791.4 
# BIC :  131831.4

# Get the sum of the squared residuals.
sum(resid(nlsReed1)^2)
# 484134.1

# Get the confidence intervals on the chosen values of the coefficients.
confint(nlsReed1)
#         2.5%      97.5%
# Ap 49.1033946 49.546296
# Bp  0.4138199  0.419150
# Cp  8.4759738  8.681496
# Dp  1.9840243  2.089204

# ------------------------------- MEILLEUR MODELE ---------------------------------------
# meilleur modele : erreurs combined - avec covariances ---------------------------------

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

saemix.best.fit <- saemixReed1(
  sexe = "Filles",
  variable_anthropometrique = "taille",
  coefficients = coef_nlsReed1,
  distrib_param = "normal",
  estimation = "restimation",
  covariance = TRUE,
  covariable = FALSE,
  erreur = "combined",
  save.diag = TRUE,
  save.graph = TRUE
)

# AIC / BIC
aic.best.fit <- AIC(saemix.best.fit) ; bic.best.fit <- BIC(saemix.best.fit);
cat("AIC : ", aic.best.fit, "\nBIC : ", bic.best.fit)
# AIC :  96837.98 
# BIC :  96922.56

# log-vraisemblance
logLik(saemix.best.fit)
# 'log Lik.' -48402.99 (df=16)

# ecart-type des residus
saemix.best.fit@results@icwres %>% sd() %>% round(2) # 0.92

# sauvegarder dans un RData -------------------------------------------------------------------

Reed1erOrdreTaillePop <- saemix.best.fit
save(Reed1erOrdreTaillePop, file = "RData/Taille/Reed1erOrdreTaillePop.RData")

# plot diagnostics -------------------------------------------------------------------

# distribution des residus
pdf("Graphiques/Taille/GraphiquesReed1erOrdre/DistriResiduals.pdf")
saemix.plot.distribresiduals(saemix.best.fit, level=1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit@results@icpred %>% 
  bind_cols(saemix.best.fit@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Taille/GraphiquesReed1erOrdre/ScatterResiduals.pdf")
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
pdf("Graphiques/Taille/GraphiquesReed1erOrdre/ObservationsVsPredictions.pdf", paper = "a4r", width = 11, height = 10)
plot(saemix.best.fit, plot.type="observations.vs.predictions")
dev.off()

# VPC (visual predictive check)
pdf("Graphiques/Taille/GraphiquesReed1erOrdre/VPC.pdf", paper = "a4r", width = 11, height = 10)
plot(saemix.best.fit, plot.type = "vpc", pch = 1, col.pobs = "#999997",
     vpc.obs = T, vpc.method = "user", vpc.bin = 20,
     xlab = "Âge (mois)", ylab = "Taille (cm)",
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

# plot du meilleur modele --------------------------------------------------------------

# ajout des predictions dans la base de donnees
croiss_modele <- croiss_modele %>%
  bind_cols(saemix.best.fit["results"]["predictions"] %>%
              select(ipred) %>% dplyr::rename(., prediction = ipred)
  ) %>%
  relocate(prediction, .after = taille)

pdf(file = "Graphiques/Taille/GraphiquesReed1erOrdre/GraphiqueReed1erOrdreSaemix.pdf")
graphiqueSaemixTaille(nom = "R1",
                      data = croiss_modele,
                      identifiants = enfants)
dev.off()
