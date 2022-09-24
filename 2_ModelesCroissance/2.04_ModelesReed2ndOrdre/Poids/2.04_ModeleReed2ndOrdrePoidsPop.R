# Description --------------------------------------------------------------------

# Modèles de croissance poids population entiere selon le modele Reed du 2nd ordre :
# poids ~ Ap + Bp*age + Cp*log(age) + Dp/age + Ep/age^2

# author : @corentindaumont, @alannagenin

# Set-up -------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")
setwd("2_ModelesCroissance")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# importer les modeles qui sont stockees dans le dossier "Modeles"
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

# Modele de Reed 2nd ordre --------------------------------------------------------------

# table d'interet
croiss_modele <- croiss_poids

# identifiants de tous les enfants
enfants <- unique(croiss_modele$num)

# modele de Reed 2nd ordre en appellant la fonction nlsReed2Poids
nlsReed2 <- nlsReed2Poids(data = croiss_poids,
                          param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 1, Ep = 1))
(coef_nlsReed2 <- coef(nlsReed2))
# Ap         Bp         Cp         Dp         Ep 
# 7.4732505  0.2496883 -0.5800115 -3.0123818  0.2470581

# summary
nlsReed2 ; summary(nlsReed2)
cat("AIC : ", AIC(nlsReed2), "\nBIC : ", BIC(nlsReed2))
# AIC :  105136.4 
# BIC :  105183.8

# Get the sum of the squared residuals.
sum(resid(nlsReed2)^2)
# 222316

# Get the confidence intervals on the chosen values of the coefficients.
confint(nlsReed2)
#        2.5%      97.5%
# Ap  7.2057442  7.7407567
# Bp  0.2472438  0.2521328
# Cp -0.6878508 -0.4721721
# Dp -3.3175997 -2.7071638
# Ep  0.2206539  0.2734622

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

saemix.best.fit <- saemixReed2(
  variable_anthropometrique = "poids",
  data = saemix.data,
  coefficients = coef_nlsReed2,
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
# AIC :  60693.61 
# BIC :  60836.34

# log-vraisemblance
logLik(saemix.best.fit)
# 'log Lik.' -30319.81 (df=27)

# ecart-type des residus
saemix.best.fit@results@icwres %>% sd() %>% round(2) # 0.89

# sauvegarder dans un RData -------------------------------------------------------------------

Reed2ndOrdrePoidsPop <- saemix.best.fit
save(Reed2ndOrdrePoidsPop, file = "RData/Poids/Reed2ndOrdrePoidsPop.RData")

# plot diagnostiques -------------------------------------------------------------------

# distribution des résidus
pdf("Graphiques/Poids/GraphiquesReed2ndOrdre/DistriResiduals.pdf")
saemix.plot.distribresiduals(saemix.best.fit, level=1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit@results@icpred %>% 
  bind_cols(saemix.best.fit@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Poids/GraphiquesReed2ndOrdre/ScatterResiduals.pdf")
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
pdf("Graphiques/Poids/GraphiquesReed2ndOrdre/ObservationsVsPredictions.pdf")
plot(saemix.best.fit, plot.type="observations.vs.predictions")
dev.off()

# VPC (visual predictive check)
pdf("Graphiques/Poids/GraphiquesReed2ndOrdre/VPC.pdf", paper = "a4r", width = 11, height = 10)
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

pdf(file = "Graphiques/Poids/GraphiquesReed2ndOrdre/GraphiqueReed2ndOrdreSaemix.pdf")
graphiqueSaemixPoids(nom = "R2",
                     data = croiss_modele,
                     identifiants = enfants)
dev.off()
