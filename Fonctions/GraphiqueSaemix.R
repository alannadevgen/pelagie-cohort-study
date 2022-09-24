graphiqueSaemixPoids <- function(
  nom_modele,
  data,
  identifiants
){
  if (nom_modele == "JB"){
    titre <- "Modèle Jenss-Bayley"
    couleur <- "#941757"
  } else if (nom_modele == "JM"){
    titre <- "Modèle Jenss modifié"
    couleur <- "#AB5676"
  } else if (nom_modele == "R1"){
    titre <- "Modèle Reed du 1er ordre"
    couleur <- "#1F78B4"
  } else if (nom_modele == "R2"){
    titre <- "Modèle Reed du 2nd ordre"
    couleur <- "#A6CEE3"
  }
  
  i=1
  while (i <= length(unique(data$num))){
    p<-list()
    for (j in 1:9){
      
      if(!is.na(identifiants[i])){
        sous_croiss <- data %>% filter(num == identifiants[i])
        
        p[[j]] <- ggplot(data = sous_croiss, aes(age_mois, poids)) +
          theme_minimal(base_size = 10) +
          geom_point() +
          labs(title = paste0("Enfant ", identifiants[i]),
               subtitle = titre,
               x = "Age (mois)",
               y = "Poids (kg)") +
          geom_line(aes(age_mois, prediction),
                    colour = couleur) +
          theme(
            # formater le titre
            plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
            # formater le sous-titre
            plot.subtitle = element_text(hjust = 0.5, face = "italic")
          ) +
          xlim(0, max(data$age_mois)) +
          ylim(0, max(data$poids, na.rm = TRUE))
        
        i<-i+1
      }
    }
    do.call("grid.arrange", c(p, ncol = 3, nrow = 3))
  }
}

graphiqueSaemixTaille <- function(
  data,
  nom_modele,
  identifiants
){
  if (nom_modele == "JB"){
    titre <- "Modèle Jenss-Bayley"
    couleur <- "#941757"
  } else if (nom_modele == "JM"){
    titre <- "Modèle Jenss modifié"
    couleur <- "#AB5676"
  } else if (nom_modele == "R1"){
    titre <- "Modèle Reed du 1er ordre"
    couleur <- "#1F78B4"
  } else if (nom_modele == "R2"){
    titre <- "Modèle Reed du 2nd ordre"
    couleur <- "#A6CEE3"
  }
  i=1
  while (i <= length(unique(data$num))){
    p<-list()
    for (j in 1:9){
      
      if(!is.na(identifiants[i])){
        sous_croiss <- data %>% filter(num == identifiants[i])
        
        p[[j]] <- ggplot(data = sous_croiss, aes(age_mois, taille)) +
          theme_minimal(base_size = 10) +
          geom_point() +
          labs(title = paste0("Enfant ", identifiants[i]),
               subtitle = titre,
               x = "Age (mois)",
               y = "Taille (cm)") +
          geom_line(aes(age_mois, prediction),
                    colour = couleur) +
          theme(
            # formater le titre
            plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
            # formater le sous-titre
            plot.subtitle = element_text(hjust = 0.5, face = "italic")
          ) +
          xlim(0, max(data$age_mois)) +
          ylim(0, max(data$taille, na.rm = TRUE))
        
        i<-i+1
      }
    }
    do.call("grid.arrange", c(p, ncol = 3, nrow = 3))
  }
}
