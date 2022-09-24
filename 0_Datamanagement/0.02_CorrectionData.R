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

### import du fichier CSV de correction des donnees
correction <- read_xlsx("../Data/Correction.xlsx", col_names = T)

### renommage des colonnes
colnames(correction) = c("num", "age_mois", "taille_old", "taille_new",
                         "IMC_old", "IMC_new", "zscore_imc_old", "zscore_imc_new")

# convertir l'id en variable numerique
correction <- correction %>% mutate(num = as.integer(str_sub(num, start = 6)))

# convertir toute la table (strings) en variable numerique et trier par id
correction <- correction %>% type_convert() %>% arrange(num)

# selectionner les identifiants des enfants concernes
identifiants <- correction %>% pull(num)
age_correction <- correction %>% pull(age_mois)

# localiser les enfants problematiques et replacer leurs donnees par les nvlles
BDD_croissance <- BDD_croissance %>% 
  mutate(taille = replace(x = taille,
                          list = num %in% identifiants & age_mois %in% age_correction,
                          values = correction %>% pull(taille_new)
                          ),
         IMC = replace(x = IMC,
                       list = num %in% identifiants & age_mois %in% age_correction,
                       values = correction %>% pull(IMC_new)
                       ),
         zscore_imc = replace(x = zscore_imc,
                       list = num %in% identifiants & age_mois %in% age_correction,
                       values = correction %>% pull(zscore_imc_new)
         )
  )


# Export as RData ------------------------------------------------------------------------------------

remove(correction, age_correction, identifiants)
save.image("DataDatamanagement.RData")
