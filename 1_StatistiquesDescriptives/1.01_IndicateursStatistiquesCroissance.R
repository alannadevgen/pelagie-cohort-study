# Set-up ------------------------------------------------------------------------------------

# working directory
setwd("../1_StatistiquesDescriptives")
setwd("1_StatistiquesDescriptives")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# packages
library(ggplot2)
library(plyr)

# Summary ----------------------------------------------------------------------------------------

str(croiss)
# 'data.frame':	32342 obs. of  14 variables:
#   $ num                 : int  1 1 1 1 1 1 1 1 1 1 ...
# $ n_obs               : int  24 24 24 24 24 24 24 24 24 24 ...
# $ taille              : num  49.5 57.8 135 74.5 144 ...
# $ poids               : num  3.42 5.05 NA 9.8 NA 39 39 NA NA 51 ...
# $ IMC                 : num  14 15.1 NA 17.7 NA ...
# $ zscore_imc          : num  0.419 -0.521 NA 0.627 NA ...
# $ age_mois            : num  0 1.56 110.99 12.14 122.99 ...
# $ age_mois_max        : num  174 174 174 174 174 ...
# $ age_an              : num  0 0.13 9.25 1.01 10.25 ...
# $ age_an_max          : num  14.5 14.5 14.5 14.5 14.5 ...
# $ sexe                : chr  "M" "M" "M" "M" ...
# $ questionnaire_taille: int  2 2 10 2 10 10 10 10 10 12 ...
# $ questionnaire_poids : int  2 2 NA 2 NA 10 30 NA NA 30 ...
# $ Q6_Q12ouQ6_GC       : int  NA NA NA NA NA NA NA NA NA NA ...

summary(croiss)
#    num             n_obs          age_mois      age_mois_no_zero  
# Min.   :   1   Min.   : 6.00   Min.   :  0.00   Min.   :  0.0658  
# 1st Qu.: 503   1st Qu.:16.00   1st Qu.:  5.56   1st Qu.:  5.5601  
# Median :1046   Median :21.00   Median : 22.93   Median : 22.9313  
# Mean   :1065   Mean   :20.45   Mean   : 51.41   Mean   : 51.4118  
# 3rd Qu.:1641   3rd Qu.:25.00   3rd Qu.:110.50   3rd Qu.:110.4986  
# Max.   :2195   Max.   :35.00   Max.   :155.93   Max.   :155.9342  
# 
#    age_an           age_an_max      age_an_ent        taille      
# Min.   : 0.000   Min.   : 9.88   Min.   : 0.000   Min.   : 34.00  
# 1st Qu.: 0.460   1st Qu.:12.92   1st Qu.: 0.000   1st Qu.: 66.00  
# Median : 1.910   Median :14.04   Median : 2.000   Median : 85.00  
# Mean   : 4.284   Mean   :13.77   Mean   : 4.264   Mean   : 95.85  
# 3rd Qu.: 9.210   3rd Qu.:14.49   3rd Qu.: 9.000   3rd Qu.:131.00  
# Max.   :12.990   Max.   :16.62   Max.   :13.000   Max.   :182.50  
# NA's   :1521    
#      poids            IMC             sexe              IMC_mere    
#  Min.   : 1.05   Min.   : 7.67   Length:23793       Min.   :1.000  
#  1st Qu.: 6.70   1st Qu.:14.93   Class :character   1st Qu.:1.000  
#  Median :10.10   Median :16.10   Mode  :character   Median :1.000  
#  Mean   :14.49   Mean   :16.17                      Mean   :1.165  
#  3rd Qu.:17.40   3rd Qu.:17.28                      3rd Qu.:1.000  
#  Max.   :92.30   Max.   :33.72                      Max.   :2.000  
#  NA's   :3746    NA's   :5266                       NA's   :77     
# 
#      tabac        
# Min.   :0.00000  
# 1st Qu.:0.00000  
# Median :0.00000  
# Mean   :0.08532  
# 3rd Qu.:0.00000  
# Max.   :1.00000  
# NA's   :164    

# ecart-type
sd(croiss$taille, na.rm = T) # 35.93544
sd(croiss$poids, na.rm = T) # 12.14108
sd(croiss$IMC, na.rm = T) # 1.907682

# Donnees manquantes pour poids et taille ----------------------------------------

# poids
summary(as.vector(table(croiss$num[is.na(croiss$poids)])))
sd(as.vector(table(croiss$num[is.na(croiss$poids)])))
length(table(croiss$num[is.na(croiss$poids)]))

# taille
summary(as.vector(table(croiss$num[is.na(croiss$taille)])))
sd(as.vector(table(croiss$num[is.na(croiss$taille)])))
length(table(croiss$num[is.na(croiss$taille)]))

# IMC
summary(as.vector(table(croiss$num[is.na(croiss$IMC)])))
sd(as.vector(table(croiss$num[is.na(croiss$IMC)])))
length(table(croiss$num[is.na(croiss$IMC)]))
