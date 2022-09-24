# Description --------------------------------------------------------------------

# Comparaison de tous les modeles poids

# author : @alannagenin

# Set-up -------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")
setwd("2_ModelesCroissance")

# vider l'espace de travail et charger les donnees
rm(list = ls())
# Poids population entiere
load("RData/Poids/JenssBayleyPoidsPop.RData")
load("RData/Poids/JenssModifiePoidsPop.RData")
# Poids filles
load("RData/Poids/JenssBayleyPoidsFilles.RData")
load("RData/Poids/JenssModifiePoidsFilles.RData")

# packages
library(dplyr)
library(lmtest)
library(saemix)

# Comparaison de deux modeles emboites grace au test du rapport de vraisemblance ----------

# on utilise la fonction lrtest du package lmtest
# lrtest(model1, model0) # ou model1 est le plus grand modele entre les deux

# Modeles Jenss-Bayley et Jenss modifie ---------------------------------------------------

# population entiere
LR <- 2*(JenssModifiePoidsPop@results@ll.is - JenssBayleyPoidsPop@results@ll.is)
logLik(JenssBayleyPoidsPop)
# log Lik.' -36023.79 (df=20)
logLik(JenssModifiePoidsPop)
# 'log Lik.' -28145.31 (df=27)
qchisq(0.95, 7) # 14.06714
LR > qchisq(0.95, 7) # TRUE
1 - pchisq(LR, 7) # p-value
# comparer les AIC et BIC
compare.saemix(JenssModifiePoidsPop, JenssBayleyPoidsPop)

# pour les filles
LR <- 2*(saemixJenssModifiePoidsFilles@results@ll.is - saemixJenssBayleyPoidsFilles@results@ll.is)
logLik(saemixJenssBayleyPoidsFilles)
# 'log Lik.' -36023.59 (df=20)
logLik(saemixJenssModifiePoidsFilles)
# 'log Lik.' -28708.3 (df=27)
qchisq(0.95, 7) # 14.06714
LR > qchisq(0.95, 7) # TRUE
1 - pchisq(LR, 7) # p-value

# pour les garcons
lrtest(saemixJenssModifiePoidsGarcons, saemixJenssBayleyPoidsGarcons)