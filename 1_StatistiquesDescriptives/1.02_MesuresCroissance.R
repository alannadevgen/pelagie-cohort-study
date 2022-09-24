# Set-up ------------------------------------------------------------------------------------

# working directory
setwd("../1_StatistiquesDescriptives")
setwd("1_StatistiquesDescriptives")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# packages
library(dplyr)

# Summary filles ------------------------------------------------------------------------

# nombre de mesures pour le poids (filles)
croiss_poids %>% 
  filter(sexe == "F") %>%
  # distinct(num) %>% 
  nrow()

# nombre de mesures pour la taille (filles)
croiss_taille %>% 
  filter(sexe == "F") %>%
  # distinct(num) %>% 
  nrow()

croiss_poids %>% 
  filter(sexe == "F") %>%
  select(poids) %>% 
  summary()

croiss_taille %>% 
  filter(sexe == "F") %>%
  select(taille) %>% 
  summary()

# Summary garcons ------------------------------------------------------------------------

# nombre de mesures pour le poids (garcons)
croiss_poids %>% 
  filter(sexe == "M") %>%
  # distinct(num) %>% 
  nrow()

# nombre de mesures pour la taille (garcons)
croiss_taille %>% 
  filter(sexe == "M") %>%
  # distinct(num) %>% 
  nrow()

croiss_poids %>% 
  filter(sexe == "M") %>%
  select(poids) %>% 
  summary()

croiss_taille %>% 
  filter(sexe == "M") %>%
  select(taille) %>% 
  summary()

# Nombre de mesures par age -------------------------------------------------------------

# Enfants ayant des croissance dans chaque tranche d'age
for (an in 1:17){
  sous_base <- croiss[croiss$age_mois>=((an-1)*12) & croiss$age_mois<(an*12),]
  nb <- length(unique(sous_base$num))
  if (an == 1){
    print(paste0("De ", an-1, " a ", an, " an : ", nb, " enfants ont une mesure soit un pourcentage de ", round(nb/nb_enf*100), "%."))
  } else {
    print(paste0("De ", an-1, " a ", an, " ans : ", nb, " enfants ont une mesure soit un pourcentage de ", round(nb/nb_enf*100), "%.")) 
  }
}

# Garcons
for (an in 1:17){
  sous_base <- croiss[croiss$age_mois>=((an-1)*12) & croiss$age_mois<(an*12) & croiss$sexe=="M",]
  nb <- length(unique(sous_base$num))
  if (an == 1){
    print(paste0("De ", an-1, " a ", an, " an : ", nb, " garcons ont une mesure soit un pourcentage de ", round(nb/nb_garcons*100), "%.")) 
  } else {
    print(paste0("De ", an-1, " a ", an, " ans : ", nb, " garcons ont une mesure soit un pourcentage de ", round(nb/nb_garcons*100), "%.")) 
  }
}

# Filles
for (an in 1:17){
  sous_base <- croiss[croiss$age_mois>=((an-1)*12) & croiss$age_mois<(an*12) & croiss$sexe=="F",]
  nb <- length(unique(sous_base$num))
  if (an == 1){
    print(paste0("De ", an-1, " a ", an, " an : ", nb, " filles ont une mesure soit un pourcentage de ", round(nb/nb_filles*100), "%."))
  } else {
    print(paste0("De ", an-1, " a ", an, " ans : ", nb, " filles ont une mesure soit un pourcentage de ", round(nb/nb_filles*100), "%."))
  }
}

# Nombre de mesures par age maximal ------------------------------------------------------

# Nombre de croiss selon l'age maximal
length(croiss$num[croiss$age_an_max<1])
length(croiss$num[croiss$age_an_max>1 & croiss$age_an_max<=2]) # 0
length(croiss$num[croiss$age_an_max>2 & croiss$age_an_max<=3]) # 0
length(croiss$num[croiss$age_an_max>3 & croiss$age_an_max<=4]) # 0
length(croiss$num[croiss$age_an_max>4 & croiss$age_an_max<=5]) # 0
length(croiss$num[croiss$age_an_max>5 & croiss$age_an_max<=6]) # 0
length(croiss$num[croiss$age_an_max>6 & croiss$age_an_max<=7]) # 0
length(croiss$num[croiss$age_an_max>7 & croiss$age_an_max<=8]) # 0
length(croiss$num[croiss$age_an_max>8 & croiss$age_an_max<=9]) # 0
length(croiss$num[croiss$age_an_max>9 & croiss$age_an_max<=10]) # 54
length(croiss$num[croiss$age_an_max>10 & croiss$age_an_max<=11]) # 451
length(croiss$num[croiss$age_an_max>11 & croiss$age_an_max<=12]) # 1 512
length(croiss$num[croiss$age_an_max>12 & croiss$age_an_max<=13]) # 4 568
length(croiss$num[croiss$age_an_max>13 & croiss$age_an_max<=14]) # 5 053
length(croiss$num[croiss$age_an_max>14 & croiss$age_an_max<=15]) # 6 961
length(croiss$num[croiss$age_an_max>15 & croiss$age_an_max<=16]) # 4 437
length(croiss$num[croiss$age_an_max>16 & croiss$age_an_max<=17]) # 757
