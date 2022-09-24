# Description --------------------------------------------------------------------

# Modèles de croissance taille population entiere selon le modele Reed du 2nd ordre :
# taille ~ Ap + Bp*age + Cp*log(age) + Dp/age + Ep/age^2

# author : @corentindaumont, @alannagenin

# Set-up -------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")
setwd("2_ModelesCroissance")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# importer les modeles qui sont stockees dans le dossier "Modeles" et "Fonctions"
source("../Modeles/ModelesReed.R")
source("../Fonctions/FonctionReed2ndOrdreNls.R")
source("../Fonctions/FonctionReed2ndOrdreSaemix.R")
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
library(stringr)

# Modele de Reed 2nd ordre  --------------------------------------------------------------

# table d'interet
croiss_modele <- croiss_taille
# filtrer les données manquantes pour le tabac et l'imc de la mère le cas échéant
croiss_modele_tabac_imc_mere <- croiss_taille %>% filter((IMC_mere == 1 | IMC_mere == 2) & (tabac == 0 | tabac == 1))
croiss_modele_tabac <- croiss_taille %>% filter(tabac == 1 | tabac == 0)
croiss_modele_IMC_mere <- croiss_taille %>% filter(IMC_mere == 2 | IMC_mere == 1)

# identifiants de tous les enfants
enfants <- unique(croiss_modele$num)

# modele de Reed 2nd ordre en appellant la fonction nlsReed2Taille
nlsReed2 <- nlsReed2Taille(data = croiss_taille,
                          param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 1, Ep = 1))
(coef_nlsReed2 <- coef(nlsReed2))
# Ap         Bp         Cp         Dp         Ep 
# 45.3558474  0.3931604 10.0002481  7.9472762 -0.5207755 

# summary
nlsReed2 ; summary(nlsReed2)
cat("AIC : ", AIC(nlsReed2), "\nBIC : ", BIC(nlsReed2))
# AIC :  131309.6 
# BIC :  131357.7

# Get the sum of the squared residuals.
sum(resid(nlsReed2)^2)
# 473731.9

# Get the confidence intervals on the chosen values of the coefficients.
confint(nlsReed2)
#       2.5%      97.5%
# Ap 44.9413966 45.7702977
# Bp  0.3898100  0.3965108
# Cp  9.8383493 10.1621472
# Dp  7.4207606  8.4737928
# Ep -0.5669387 -0.4746123

# ---------------------------------------------------------------------------------------------
# -------------------------- MODELE SAEMIX AVEC COVARIABLE SEXE -------------------------------
# ---------------------------------------------------------------------------------------------

# meilleur modele : erreurs combined - avec covariances et covariable sexe --------------------

saemix.data <- saemixData(
  name.data = croiss_modele,
  header = TRUE,
  sep = ";",
  na = NA,
  name.X = "age_mois_no_zero",
  name.group = "num",
  name.predictors = "age_mois_no_zero",
  name.response = "taille",
  name.covariates = "sexe", 
  units=list(x = "mois", y = "cm", covariates = "-"),
)

saemix.best.fit <- saemixReed2(
  variable_anthropometrique = "taille",
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
# AIC :  93767.47 
# BIC :  93910.2

# log-vraisemblance
logLik(saemix.best.fit)
# 'log Lik.' -46856.74 (df=27)

# ecart-type des residus
saemix.best.fit@results@icwres %>% sd() %>% round(2) # 0.89

# sauvegarder dans un RData -------------------------------------------------------------------

Reed2ndOrdreTaillePop <- saemix.best.fit
save(Reed2ndOrdreTaillePop, file = "RData/Taille/Reed2ndOrdreTaillePop.RData")

# avec les covariables tabac et IMC_mere -----------------------------------------------------

# Sans surprises, le modèle le plus signifiant au sens de l'AIC/BIC est le modèle intégrant les trois covariables (sexe, tabac et IMC_mère)

saemix.data <- saemixData(
  name.data = croiss_modele_tabac_IMC_mere,
  header = TRUE,
  sep = ";",
  na = NA,
  name.X = "age_mois_no_zero",
  name.group = "num",
  name.predictors = "age_mois_no_zero",
  name.response = "taille",
  name.covariates = c("sexe", "tabac", "IMC_mere"), 
  units=list(x = "mois", y = "cm", covariates = c("-", "-", "-")),
)

saemix.best.fit <- saemixReed2(
  variable_anthropometrique = "taille",
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

# plot diagnostiques -------------------------------------------------------------------

# distribution des résidus
pdf("Graphiques/Taille/GraphiquesReed2ndOrdre/DistriResiduals.pdf")
saemix.plot.distribresiduals(saemix.best.fit, level=1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit@results@icpred %>% 
  bind_cols(saemix.best.fit@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Taille/GraphiquesReed2ndOrdre/ScatterResiduals.pdf")
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
pdf("Graphiques/Taille/GraphiquesReed2ndOrdre/ObservationsVsPredictions.pdf")
plot(saemix.best.fit, plot.type="observations.vs.predictions")
dev.off()

# VPC (visual predictive check)
pdf("Graphiques/Taille/GraphiquesReed2ndOrdre/VPC.pdf", paper = "a4r", width = 11, height = 10)
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

# prediction du modele ----------------------------------------------------------------

# ajout des predictions dans la base de donnees
croiss_modele <- croiss_modele %>%
  bind_cols(saemix.best.fit["results"]["predictions"] %>%
              select(icpred) %>% dplyr::rename(., prediction = icpred)
  ) %>%
  relocate(prediction, .after = taille)

pdf(file = "Graphiques/Taille/GraphiquesReed2ndOrdre/GraphiqueReed2ndOrdreSaemix.pdf")
graphiqueSaemixTaille(nom = "R2",
                     data = croiss_modele,
                     identifiants = enfants)
dev.off()
