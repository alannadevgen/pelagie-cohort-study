# Description --------------------------------------------------------------------

# Modèles de croissance taille chez les filles selon le modele Reed du 2nd ordre :
# taille ~ Ap + Bp*age + Cp*log(age) + Dp/age + Ep/age^2

# author : @corentindaumont, @alannagenin

# Set-up -------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")
setwd("2_ModelesCroissance")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# importer les modeles qui sont stockees dans les dossiers "Modeles" et "Fonctions"
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

# ---------------------------------------------------------------------------------------------
# ------------------------- MODELE SAEMIX SUR LES FILLES --------------------------------------
# ---------------------------------------------------------------------------------------------

# table d'interet 
croiss_modele <- croiss_taille %>% filter(sexe == "F")

# identifiants de tous les enfants
enfants_filles <- unique(croiss_modele$num)

# modele de Reed 2nd ordre en appellant la fonction nlsReed2ndOrdreTaille du fichier Fonctions/FonctionReed2ndOrdreNls.R
nlsReed2Filles <- nlsReed2Taille(data = croiss_modele,
                                       param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 1, Ep = 1))
(coef_nlsReed2Filles <- abs(coef(nlsReed2Filles)))
# Ap         Bp         Cp         Dp         Ep 
# 44.4437602  0.4006187 10.0032249  8.3211402 -0.5525379

# summary
nlsReed2Filles ; summary(nlsReed2Filles)
cat("AIC : ", AIC(nlsReed2Filles), "\nBIC : ", BIC(nlsReed2Filles))
# AIC :  65303.93 
# BIC :  65347.79

# Get the sum of the squared residuals.
sum(resid(nlsReed2Filles)^2)
# 237511.9

# Get the confidence intervals on the chosen values of the coefficients.
confint(nlsReed2Filles)
#         2.5%      97.5%
# Ap 43.8620749 45.0254464
# Bp  0.3958677  0.4053696
# Cp  9.7749705 10.2314790
# Dp  7.5918955  9.0503834
# Ep -0.6164070 -0.4886686

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
  units=list(x = "mois", y = "kg", covariates = c("-")),
  name.X = "age_mois_no_zero")

saemix.best.fit.filles <- saemixReed2(
  sexe = "Filles",
  variable_anthropometrique = "taille",
  coefficients = coef_nlsReed2Filles,
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
# AIC :  46551.08 
# BIC :  46651.88

# log-vraisemblance
logLik(saemix.best.fit.filles)
# 'log Lik.' -23253.54 (df=22)

# ecart-type des residus
saemix.best.fit.filles@results@icwres %>% sd() %>% round(2) # 0.89

# sauvegarder dans un RData -------------------------------------------------------------------

Reed2ndOrdreTailleFilles <- saemix.best.fit.filles
save(Reed2ndOrdreTailleFilles, file = "RData/Taille/Reed2ndOrdreTailleFilles.RData")

# plot diagnostiques -------------------------------------------------------------------

# distribution des résidus
pdf("Graphiques/Taille/GraphiquesReed2ndOrdreFilles/DistriResiduals.pdf")
saemix.plot.distribresiduals(saemix.best.fit.filles, level=1)
dev.off()

# scatterplot des residus
residus <- saemix.best.fit.filles@results@icpred %>% 
  bind_cols(saemix.best.fit.filles@results@icwres)
colnames(residus) <- c("icpred", "icwres")

pdf("Graphiques/Taille/GraphiquesReed2ndOrdreFilles/ScatterResiduals.pdf")
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
pdf("Graphiques/Taille/GraphiquesReed2ndOrdreFilles/ObservationsVsPredictions.pdf")
plot(saemix.best.fit.filles, plot.type="observations.vs.predictions")
dev.off()

# VPC (visual predictive check)
pdf("Graphiques/Taille/GraphiquesReed2ndOrdreFilles/VPC.pdf", paper = "a4r", width = 11, height = 10)
plot(saemix.best.fit.filles, plot.type = "vpc", pch = 1, col.pobs = "#999997",
     vpc.obs = T, vpc.method = "user", vpc.bin = 20,
     xlab = "Âge (mois)", ylab = "Taille (cm)",
     col.fillmed = "#c98ea1", col.lmed = "#941757",
     col.lobs = "#6A3D9A", col.lpi = "#6A3D9A",
     col.fillpi = "#CAB2D6",
     main = bquote(bold("Visual Predictive Check pour les données filles avec le modèle Reed" ~ 2^nd ~"ordre pour la taille"))
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
  relocate(prediction, .after = taille)

pdf(file = "Graphiques/Taille/GraphiquesReed2ndOrdreFilles/GraphiqueReed2ndOrdreSaemixFilles.pdf")
graphiqueSaemixTaille(nom = "R2",
                     data = croiss_modele,
                     identifiants = enfants_filles)
dev.off()
