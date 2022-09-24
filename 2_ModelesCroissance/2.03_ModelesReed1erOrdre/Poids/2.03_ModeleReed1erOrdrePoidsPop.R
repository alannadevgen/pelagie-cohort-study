# Description --------------------------------------------------------------------

# Modèles de croissance poids population entiere selon le modele Reed du 1er ordre :
# poids ~ Ap + Bp*age + Cp*log(age) + Dp/age

# author : @camillepliquet, @alannagenin

# Set-up -------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")
setwd("2_ModelesCroissance")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# importer les modeles qui sont stockees dans le dossier "Modeles"
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

# Modele de Reed 1er ordre --------------------------------------------------------------

# table d'interet
croiss_modele <- croiss_poids

# identifiants de tous les enfants
enfants <- unique(croiss_modele$num)

# modele de Reed 1er ordre en appellant la fonction nlsReed1Poids
nlsReed1 <- nlsReed1Poids(data = croiss_poids,
                          param = list(Ap = 0.5, Bp = 1, Cp = 1, Dp = 1))
(coef_nlsReed1 <- coef(nlsReed1))
# Ap         Bp         Cp         Dp 
# 5.4256734  0.2367738  0.1714025 -0.1775238

# summary
nlsReed1 ; summary(nlsReed1)
cat("AIC : ", AIC(nlsReed1), "\nBIC : ", BIC(nlsReed1))
# AIC :  105468.1 
# BIC :  105507.6

# Get the sum of the squared residuals.
sum(resid(nlsReed1)^2)
# 226047

# Get the confidence intervals on the chosen values of the coefficients.
confint(nlsReed1)
#       2.5%      97.5%
# Ap  5.27053595  5.5808108
# Bp  0.23473943  0.2388082
# Cp  0.09882975  0.2439753
# Dp -0.21477861 -0.1402690

# ---------------------------------------------------------------------------------------------
# -------------------------- MODELE SAEMIX AVEC COVARIABLE SEXE -------------------------------
# ---------------------------------------------------------------------------------------------

saemix.data <- saemixData(
  name.data = croiss_modele,
  header = TRUE,
  sep = ";",
  na = NA,
  name.X = "age_mois_no_zero",
  name.group = "num",
  name.predictors = "age_mois_no_zero",
  name.response = "poids",
  name.covariates = "sexe",
  units=list(x = "mois", y = "kg", covariates = c("-")),
)

# meilleur modele : erreurs combined - avec covariances et covariable sexe --------------------

saemix.best.fit <- saemixReed1(
  variable_anthropometrique = "poids",
  data = saemix.data,
  coefficients = coef_nlsReed1,
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
# AIC :  58901.51 
# BIC :  59007.23

# log-vraisemblance
logLik(saemix.best.fit)
# 'log Lik.' -29430.75 (df=20)

# ecart-type des residus
saemix.best.fit@results@icwres %>% sd() %>% round(2) # 0.91

# sauvegarder dans un RData -------------------------------------------------------------------

Reed1erOrdrePoidsPop <- saemix.best.fit
save(Reed1erOrdrePoidsPop, file = "RData/Poids/Reed1erOrdrePoidsPop.RData")

# plot diagnostics -------------------------------------------------------------------

# distribution des residus
pdf("Graphiques/Poids/GraphiquesReed1erOrdre/DistriResiduals.pdf")
saemix.plot.distribresiduals(saemix.best.fit, level=1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit@results@icpred %>% 
  bind_cols(saemix.best.fit@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Poids/GraphiquesReed1erOrdre/ScatterResiduals.pdf")
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
pdf("Graphiques/Poids/GraphiquesReed1erOrdre/ObservationsVsPredictions.pdf")
plot(saemix.best.fit, plot.type="observations.vs.predictions")
dev.off()

# VPC (visual predictive check)
pdf("Graphiques/Poids/GraphiquesReed1erOrdre/VPC.pdf", paper = "a4r", width = 11, height = 10)
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
  relocate(prediction, .after = poids)

pdf(file = "Graphiques/Poids/GraphiquesReed1erOrdre/GraphiqueReed1erOrdreSaemix.pdf")
graphiqueSaemixPoids(nom = "R1",
                     data = croiss_modele,
                     identifiants = enfants)
dev.off()
