# Set-up --------------------------------------------------------------------------

# working directory
setwd("../0_Datamanagement")
setwd("0_Datamanagement")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("DataDatamanagement.RData")

# packages
library(dplyr)

# Nombre d'observations dans la base de données initiale ----------------------------------------------------------------------------------

### Nombre d'enfants
(nb_enf <- length(unique(BDD_croissance$num)))
# 2189 enfants

### Nombre de garcons
(nb_garcons <- length(unique(BDD_croissance$num[BDD_croissance$sexe=="M"])))
# 1099 garcons

### Nombre de filles
(nb_filles <- length(unique(BDD_croissance$num[BDD_croissance$sexe=="F"])))
# 1090 filles

# Calcul du nombre d'observations et de l'age maximal -----------------------------

# calcul du nombre d'obs par enfant
# parfois count veut "num" d'autres fois num donc test if pour régler le probleme
if (BDD_croissance %>% count(num) %>% nrow() == nb_enf){
  n_obs <- BDD_croissance %>% count(num)
} else {
  n_obs <- BDD_croissance %>% count("num")
}
names(n_obs)[2] <- "n_obs"
# merge et met la colonne "n_obs" apres "num" en 1 etape
BDD_croissance <- BDD_croissance %>% left_join(n_obs, by = "num") %>% relocate(n_obs, .after = num)

# age maximal par enfant
age_max <- aggregate(formula = age_an ~ num, data = BDD_croissance, FUN = function(x){max(x)})
names(age_max)[2] <- "age_an_max"
# merge et met la colonne "age_max" apres "age_an" en 1 etape
BDD_croissance <- BDD_croissance %>% left_join(age_max, by = "num") %>% 
  # relocate(age_mois_max, .after = age_mois_no_zero) %>% 
  relocate(age_an_max, .after = age_an)
# trier par identifiant et par age
BDD_croissance <- BDD_croissance %>% arrange(num, age_mois)

# Filtrage des donnees en fonction du nb d'obs et de l'age maximal ----------------
# on ne garde que les enfants qui ont plus de 5 observations et 8 ans et plus
croiss <- BDD_croissance %>% filter(n_obs > 5 & age_an_max > 8 & age_an < 13) %>% 
  select(-zscore_imc, -questionnaire_taille:-Q6_Q12ouQ6_GC)

# Filtrage des donnees en fonction de la variable d'interet -----------------------------------------

# taille complete
croiss_taille <- croiss %>% filter(!is.na(taille))

# poids complet
croiss_poids <- croiss %>% filter(!is.na(poids))

# IMC complet
# croiss_IMC <- croiss %>% filter(!is.na(IMC))

# Nombre d'observations dans la base de données finale ----------------------------------------------------------------------------------

### Nombre d'enfants
(nb_enf <- length(unique(croiss$num)))
# 1460 enfants

### Nombre de garcons
(nb_garcons <- length(unique(croiss$num[croiss$sexe == "M"])))
# 738 garcons

### Nombre de filles
(nb_filles <- length(unique(croiss$num[croiss$sexe == "F"])))
# 722 filles

# Export as RData ---------------------------------------------------------------

remove(BDD_croissance, age_max, n_obs)
save.image("DataDatamanagement.RData")
