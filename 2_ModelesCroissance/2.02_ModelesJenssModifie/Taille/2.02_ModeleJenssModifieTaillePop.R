# Description --------------------------------------------------------------------

# Modèles de croissance taille population entiere selon le modele Jenss modifie :
# taille ~ Ap + Bp*age + Cp*exp(-age*Dp) + Ep*age^2

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
source("../Fonctions/FonctionJenssModifieNls.R")
source("../Fonctions/FonctionJenssModifieSaemix.R")
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

# modele de Jenss-Bayley en appellant la fonction nlsJenssModifieTaille du fichier Fonctions/FonctionJenssModifieNls.R
nlsJenssModifie <- nlsJenssModifieTaille(data = croiss_taille,
                                         param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 1, Ep = .1))
(coef_nlsJenssModifie <- coef(nlsJenssModifie))
# Ap           Bp           Cp           Dp           Ep 
# 50.519238144  0.657083441 22.636581220  0.124374156 -0.000777117

# summary
nlsJenssModifie ; summary(nlsJenssModifie)
cat("AIC : ", AIC(nlsJenssModifie), "\nBIC : ", BIC(nlsJenssModifie))
# AIC :  131470.6 
# BIC :  131518.7

# Get the sum of the squared residuals.
sum(resid(nlsJenssModifie)^2)
# 477168.1

# Get the confidence intervals on the chosen values of the coefficients.
confint(nlsJenssModifie)
#         2.5%       97.5%
# Ap 50.3202929021 50.7172680618
# Bp  0.6417038386  0.6719413507
# Cp 22.0806888369 23.2164848594
# Dp  0.1187910591  0.1301886003
# Ep -0.0008548386 -0.0006972976

# ------------------------------- MEILLEUR MODELE ---------------------------------------
# meilleur modele : erreurs proportionnal - avec covariances et covariable sexe ---------

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

saemix.best.fit <- saemixJenssModifie(
  variable_anthropometrique = "taille",
  data = saemix.data,
  coefficients = coef_nlsJenssModifie,
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
# AIC :  93208.48 
# BIC :  93351.2

# log-vraisemblance
logLik(saemix.best.fit)
# 'log Lik.' -46577.24 (df=27)

# ecart-type des residus
saemix.best.fit@results@icwres %>% sd() %>% round(2) # 0.89

# sauvegarder dans un RData -------------------------------------------------------------------

JenssModifieTaillePop <- saemix.best.fit
save(JenssModifieTaillePop, file = "RData/Taille/JenssModifieTaillePop.RData")

# plot diagnostiques -------------------------------------------------------------------

# distribution des résidus
pdf("Graphiques/Taille/GraphiquesJenssModifie/DistriResiduals.pdf")
saemix.plot.distribresiduals(saemix.best.fit, level=1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit@results@icpred %>% 
  bind_cols(saemix.best.fit@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Taille/GraphiquesJenssModifie/ScatterResiduals.pdf")
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
pdf("Graphiques/Taille/GraphiquesJenssModifie/ObservationsVsPredictions.pdf")
plot(saemix.best.fit, plot.type="observations.vs.predictions")
dev.off()

# VPC (visual predictive check)
pdf("Graphiques/Taille/GraphiquesJenssModifie/VPC.pdf", paper = "a4r", width = 11, height = 10)
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

pdf(file = "Graphiques/Taille/GraphiquesJenssModifie/GraphiqueJenssModifieSaemix.pdf")
graphiqueSaemixTaille(nom = "JM",
                      data = croiss_modele,
                      identifiants = enfants)
dev.off()
