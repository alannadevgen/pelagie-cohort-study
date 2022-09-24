# Description --------------------------------------------------------------------

# Modèles de croissance poids chez les filles selon le modele de Jenss modifie :
# poids ~ Ap + Bp*age + Cp*exp(-age*Dp) + Ep*age^2

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
source("../Fonctions/FonctionJenssModifieNls.R")
source("../Fonctions/FonctionJenssModifieSaemix.R")
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

# modele de Jenss modifie en appellant la fonction nlsJenssModifiePoids du fichier Fonctions/FonctionJenssModifieNls.R
nlsJenssModifieFilles <- nlsJenssModifiePoids(data = croiss_modele,
                                            param = list(Ap = 3, Bp = 0.1, Cp = 3, Dp = .1, Ep = .01))
(coef_nlsJenssModifieFilles <- coef(nlsJenssModifieFilles))
# Ap          Bp          Cp          Dp          Ep 
# 3.502797430 0.043559426 8.067895418 0.085943496 0.001135472 

# summary
nlsJenssModifieFilles ; summary(nlsJenssModifieFilles)
cat("AIC : ", AIC(nlsJenssModifieFilles), "\nBIC : ", BIC(nlsJenssModifieFilles))
# AIC :  51682.85 
# BIC :  51726.12

# Get the sum of the squared residuals.
sum(resid(nlsJenssModifieFilles)^2)
# 102569.8

# Get the confidence intervals on the chosen values of the coefficients.
confint(nlsJenssModifieFilles)
#         2.5%      97.5%
# Ap 3.314585670 3.687945338
# Bp 0.009555848 0.069687077
# Cp 7.032130419 9.531087454
# Dp 0.069870382 0.104336860
# Ep 0.001005698 0.001297749

# ------------------------------- MEILLEUR MODELE ---------------------------------------
# meilleur modele : erreurs combined - sans covariances (pour les filles) ---------------

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

saemix.best.fit.filles <- saemixJenssModifie(
  sexe = "Filles",
  variable_anthropometrique = "poids",
  coefficients = coef_nlsJenssModifieFilles,
  distrib_param = "log-normal",
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
# AIC :  21418.29 
# BIC :  21519.1

# log-vraisemblance
logLik(saemix.best.fit.filles)
# 'log Lik.' -10687.15 (df=22)

# ecart-type des residus
saemix.best.fit.filles@results@icwres %>% sd() %>% round(2) # 0.90

# sauvegarder dans un RData -------------------------------------------------------------------

JenssModifiePoidsFilles <- saemix.best.fit.filles
save(JenssModifiePoidsFilles, file = "RData/Poids/JenssModifiePoidsFilles.RData")

# plot diagnostiques -------------------------------------------------------------------

# distribution des résidus
pdf("Graphiques/Poids/GraphiquesJenssModifieFilles/DistriResiduals.pdf")
saemix.plot.distribresiduals(saemix.best.fit.filles, level=1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit.filles@results@icpred %>% 
  bind_cols(saemix.best.fit.filles@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Poids/GraphiquesJenssModifieFilles/ScatterResiduals.pdf")
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
pdf("Graphiques/Poids/GraphiquesJenssModifieFilles/ObservationsVsPredictions.pdf", paper = "a4r", width = 11, height = 10)
plot(saemix.best.fit.filles, plot.type="observations.vs.predictions")
dev.off()
saemix.best.fit.filles <- JenssModifiePoidsFilles
# VPC (visual predictive check)
pdf("Graphiques/Poids/GraphiquesJenssModifieFilles/VPC.pdf", paper = "a4r", width = 11, height = 10)
plot(saemix.best.fit.filles, plot.type = "vpc", pch = 1, col.pobs = "#999997",
     vpc.obs = T, vpc.method = "user", vpc.bin = 20,
     xlab = "Âge (mois)", ylab = "Poids (kg)",
     col.fillmed = "#c98ea1", col.lmed = "#941757",
     col.lobs = "#6A3D9A", col.lpi = "#6A3D9A",
     col.fillpi = "#CAB2D6",
     main = bquote(bold("Visual Predictive Check pour les données filles avec le modèle Jenss adapté pour le poids"))
)
mtext(side = 3, line = -33, at = 1, adj = 1, padj = 1, cex = 0.7, 
      text = "Données : Cohorte PÉLAGIE - INSERM | Graphique : Alanna GENIN")
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

pdf(file = "Graphiques/Poids/GraphiquesJenssModifieFilles/GraphiqueJenssModifieSaemixFilles.pdf")
graphiqueSaemixPoids(nom = "JM",
                      data = croiss_modele,
                      identifiants = enfants_filles)
dev.off()
