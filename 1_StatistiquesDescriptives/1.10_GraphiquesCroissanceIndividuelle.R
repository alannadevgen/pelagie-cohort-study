# Set-up ------------------------------------------------------------------------------------

# working directory
setwd("../1_StatistiquesDescriptives")
setwd("1_StatistiquesDescriptives")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# creer dossier pour sauvegarder les resultats
newpath <- file.path("Graphiques/Poids")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")
newpath <- file.path("Graphiques/Taille")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")
newpath <- file.path("Graphiques/IMC")
dir.create(newpath, showWarnings = TRUE, recursive = TRUE, mode = "0777")

# packages
library(ggplot2)
library(forcats)
library(plyr)
library(gridExtra)

## Graphiques individuels du poids, de la taille et de l'IMC -------------------------------------

# vecteur contenant tous les identifiants des enfants
enfants <- unique(croiss$num)

# Poids individuel (toutes obs) -----------------------------------------------------------------------

pdf("Graphiques/Poids/PoidsIndividuel.pdf", onefile = TRUE)
i=1
while (i <= nb_enf){
  p<-list()
  for (j in 1:9){
    
    if(!is.na(enfants[i])){
      sous_croiss<-croiss[croiss$num == enfants[i],]
      
      p[[j]]<-ggplot(sous_croiss, aes(x = age_mois, y = poids)) + 
        labs(title=paste0("Enfant ", enfants[i]),
             x="Age (mois)", y="Poids(kg)") +
        geom_line(size = 0.5, color = "MediumTurquoise") +
        geom_point(size = 1.5, color = "LightSeaGreen") +
        xlim(0, max(croiss$age_mois)) +
        ylim(0, max(croiss$poids, na.rm = TRUE)) +
        theme_bw() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5)
        )
      i<-i+1
    }
  }
  do.call("grid.arrange", c(p, ncol = 3, nrow = 3))
}
dev.off()

# Taille individuelle ------------------------------------------------------------------------------

enfants <- unique(croiss$num)
pdf("Graphiques/Taille/TailleIndividuelle.pdf", onefile = TRUE)
i=1
while (i <= nb_enf){
  p<-list()
  for (j in 1:9){
    
    if(!is.na(enfants[i])){
      sous_croiss<-croiss[croiss$num==enfants[i],]
      
      p[[j]]<-ggplot(sous_croiss, aes(x = age_mois, y = taille)) + 
        labs(title=paste0("Enfant ", enfants[i]), 
             x="Age (mois)", y="Taille (cm)") +
        geom_line(size = 0.5, color = "#5d2a69") +
        geom_point(size = 1.5, color = "#4a2254") +
        xlim(0, max(croiss$age_mois)) +
        ylim(0, max(croiss$taille, na.rm = TRUE)) +
        theme_bw() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5)
        )
      i<-i+1
    }
  }
  do.call("grid.arrange", c(p, ncol = 3, nrow = 3))
}
dev.off()

# IMC individuel --------------------------------------------------------------------------------

pdf("Graphiques/IMC/IMC_individuel.pdf", onefile = TRUE)
i=1
while (i <= nb_enf){
  p<-list()
  for (j in 1:9){
    
    if(!is.na(enfants[i])){
      sous_croiss<-croiss[croiss$num==enfants[i],]
      
      p[[j]]<-ggplot(sous_croiss, aes(x = age_mois, y = IMC)) + 
        labs(title=paste0("Enfant ", enfants[i]), 
             x="Age (mois)", y="IMC") +
        geom_line(size = 0.5, color = "SlateBlue") +
        geom_point(size = 1.5, color = "MediumPurple") +
        xlim(0, max(croiss$age_mois)) +
        ylim(0, max(croiss$IMC, na.rm = TRUE)) +
        theme_bw() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5)
        )
      i<-i+1
    }
  }
  do.call("grid.arrange", c(p, ncol = 3, nrow = 3))
}
dev.off()
