# Set-up ------------------------------------------------------------------------------------

# working directory
setwd("../0_Datamanagement")
setwd("0_Datamanagement")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("DataDatamanagement.RData")

# packages
library(dplyr)
library(stringr)
library(readxl)
library(readr)

# Import data ------------------------------------------------------------------------------------

### import du fichier CSV de covariables
covariables <- read.csv2("../Data/BDD_nvlls_vars.csv", row.names = 1)

### renommage des colonnes
colnames(covariables) = c("num", "IMC_mere", "tabac")

### extraction du numero de l'enfant dans l'identifiant
covariables$num <- as.integer(str_sub(covariables$num, start = 6))

### fusion des deux tables
BDD_croissance <- BDD_croissance %>% left_join(covariables, by = "num")

# Export as RData ------------------------------------------------------------------------------------

remove(covariables)
save.image("DataDatamanagement.RData")
