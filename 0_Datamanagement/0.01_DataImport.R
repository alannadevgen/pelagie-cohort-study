# Set-up ------------------------------------------------------------------------------------

# working directory
setwd("../0_Datamanagement")
setwd("0_Datamanagement")

# vider l'espace de travail
rm(list = ls())

# packages
library(dplyr)
library(stringr)

# Import data ------------------------------------------------------------------------------------

### import du fichier CSV
BDD_croissance <- read.csv("../Data/BDD_croissance_V2.csv",
                   row.names = 1, stringsAsFactors = TRUE,
                   sep = ";", dec = ".", header = TRUE,
                   encoding="UTF-8", fileEncoding="UTF-8")

### renommage de certaines colonnes
# localisation des colonnes a renommer (au cas où les colonnes bougent)
index_id <- which(colnames(BDD_croissance)  == "identifiant_ENSAI")
index_imc <- which(colnames(BDD_croissance)  == "imc")
# changement des colnames
colnames(BDD_croissance)[index_id] = "num"
colnames(BDD_croissance)[index_imc] = "IMC"

# reordonnancement des colonnes
BDD_croissance <- BDD_croissance %>% 
  relocate(age_mois, .after = num)

### calcul de l'age en annees
BDD_croissance <- BDD_croissance %>% 
  mutate(age_an = round(age_mois/12, digits = 2),
         age_an_ent = round(age_mois/12, digits = 0)) %>% 
  relocate(age_an, .after = age_mois) %>% 
  relocate(age_an_ent, .after = age_an)

# ajouter artificiellement 0.1 aux enfants qui ont une mesure à la naissance
# (ie 0 mois) pour permettre les divisions (divisions par 0 impossibles)
BDD_croissance <- BDD_croissance %>% 
  mutate(age_mois_no_zero = case_when(age_mois <= 0.05 ~ 0.1,
                                      age_mois > 0 ~ age_mois)) %>%
  relocate(age_mois_no_zero, .after = age_mois)

### extraction du numero de l'enfant dans l'identifiant
BDD_croissance$num <- as.integer(str_sub(BDD_croissance$num, start = 6))

### mettre les modalites du sexe (f ou m) en majuscules (F ou M)
BDD_croissance$sexe <- BDD_croissance$sexe %>% str_to_upper()

# Export as RData ------------------------------------------------------------------------------------

remove(index_id, index_imc)
save.image("DataDatamanagement.RData")
