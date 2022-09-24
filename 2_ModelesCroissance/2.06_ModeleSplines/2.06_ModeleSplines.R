# Description --------------------------------------------------------------------

# Tentative de modele spline lineaire, pas termine

# author : @camillepliquet

# Set-up -------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")
source("../0_Datamanagement/0.02_BDDsans2et4ans.R")
#install.packages("nlme")
require(nlme)
require(ggplot2)
require(gridExtra)

###################################################################
########## Modele mixte lineaire pour tous les individus ##########
###################################################################

# modele mixte en supposant une relation lineaire
mod1<- lme(taille ~age_mois, data = croiss_taille, random=~1+age_mois|num, control=list(maxIter=1000, opt="optim"), method="ML")
summary(mod1)
print(mod1)
plot(mod1, taille ~ fitted(.) | num==1, abline = c(0,1))

# coefficients pour tracer la droite de regression
effetalea <- mod1$coefficients$random$num
effetfixe <- mod1$coefficients$fixed
coef <- matrix(NA, ncol=2, nrow=nrow(effetalea))
coef[,1] <- effetalea[,1]+effetfixe[1]
coef[,2] <- effetalea[,2]+effetfixe[2]

enfants <- unique(croiss$num)

pdf("Graphiques/modelemixte_taille.pdf", onefile = TRUE)
i = 1
while (i <= 9){
  p<-list()
  for (j in 1:9){
    sous_croiss_taille<-croiss_taille[croiss_taille$num==enfants[i],]
      
    p[[j]]<- ggplot(sous_croiss_taille, aes(x = age_mois, y = taille)) + 
      labs(title=paste0("Enfant ", enfants[i]), x="Age (mois)", y="Taille(cm)") +
      geom_point(size=1.5, color="Purple")+
      geom_abline(intercept = round(coef[i,1],2), slope = round(coef[i,2],2), color="Medium Purple") +
      xlim(0, max(croiss$age_mois))+ylim(0, max(croiss$taille))+
      theme_bw()
    
    i<- i+1
  }
  do.call("grid.arrange",c(p,ncol=3,nrow=3))
}
dev.off()

##########################################################
#############"" Modele mixte avec splines ################
##########################################################

# 1er spline : entre 0 et 1 an

croiss_taille_0_1 <- croiss_taille[croiss_taille$age_mois <=13,]
mod2<- lme(taille ~age_mois, data = croiss_taille_0_1, random=~1+age_mois|num, control=list(maxIter=1000, opt="optim"), method="ML")
summary(mod2)
alea0_1 <- mod2$coefficients$random$num
fixe0_1 <- mod2$coefficients$fixed
coef0_1 <- matrix(NA, ncol=2, nrow=length(unique(croiss_taille_0_1$num)))
coef0_1[,1] <- alea0_1[,1]+fixe0_1[1]
coef0_1[,2] <- alea0_1[,2]+fixe0_1[2]


i <- 1
#while (i <= length(unique(croiss_taille_0_2$num))){
while(i<=9){
  p<-list()
  for (j in 1:9){
    #sous_croiss_taille<-croiss_taille[croiss_taille$num==enfants[i],]
    sous_croiss<-croiss[croiss$num==enfants[i],]
    sous_croiss_taille2<-croiss_taille_0_2[croiss_taille_0_2$num==enfants[i],]
    xmin <- min(sous_croiss_taille2$age_mois)
    xmax <- max(sous_croiss_taille2$age_mois)
    y <- round(coef0_2[i,1],2)
    
    p[[j]]<- ggplot(sous_croiss, aes(x = age_mois, y = taille)) + 
      labs(title=paste0("Enfant ", enfants[i]), x="Age (mois)", y="Taille(cm)") +
      geom_point(size=1.5, color="LightSkyBlue")+
      geom_abline(intercept = round(coef[i,1],2), slope = round(coef[i,2],2), color="Medium Purple") +
      geom_segment(aes(x=xmin, y=round(coef0_2[i,1],2), xend=xmax, yend=y+xmax*round(coef0_2[i,2],2)), color="green", size=1)+
      xlim(0, max(croiss$age_mois))+ylim(0, max(croiss$taille, na.rm = TRUE))+
      theme_bw()
    
    i<- i+1
  }
  do.call("grid.arrange",c(p,ncol=3,nrow=3))
}



####### 2e  spline : au-dessus de 2 ans ################
croiss_taille_2_4 <- croiss_taille[croiss_taille$age_mois >20,]
mod3<- lme(taille ~age_mois, data = croiss_taille_2_4, random=~1+age_mois|num, control=list(maxIter=1000, opt="optim"), method="ML")
summary(mod2)
alea2_4 <- mod3$coefficients$random$num
fixe2_4 <- mod3$coefficients$fixed
coef2_4 <- matrix(NA, ncol=2, nrow=length(unique(croiss_taille_2_4$num)))
coef2_4[,1] <- alea2_4[,1]+fixe2_4[1]
coef2_4[,2] <- alea2_4[,2]+fixe2_4[2]


i <- 1
#while (i <= length(unique(croiss_taille_2_4$num))){
while(i<=9){
  p<-list()
  for (j in 1:9){
    sous_croiss<-croiss[croiss$num==enfants[i],]
    sous_croiss_taille3<-croiss_taille_2_4[croiss_taille_2_4$num==enfants[i],]
    xmin <- min(sous_croiss_taille3$age_mois)
    xmax <- max(sous_croiss_taille3$age_mois)
    y <- round(coef2_4[i,1],2)
    
    p[[j]]<- ggplot(sous_croiss, aes(x = age_mois, y = taille)) + 
      labs(title=paste0("Enfant ", enfants[i]), x="Age (mois)", y="Taille(cm)") +
      geom_point(size=1.5, color="LightSkyBlue")+
      geom_abline(intercept = round(coef[i,1],2), slope = round(coef[i,2],2), color="Medium Purple") +
      geom_segment(aes(x=xmin, y=round(coef2_4[i,1],2), xend=xmax, yend=y+xmax*round(coef2_4[i,2],2)), color="green", size=1)+
      xlim(0, max(croiss$age_mois))+ylim(0, max(croiss$taille, na.rm = TRUE))+
      theme_bw()
    
    i<- i+1
  }
  do.call("grid.arrange",c(p,ncol=3,nrow=3))
}


sous_croiss<-croiss[croiss$num==enfants[1],]
sous_croiss_taille3<-croiss_taille_2_4[croiss_taille_2_4$num==enfants[1],]
xmin2 <- min(sous_croiss_taille3$age_mois)
xmax2 <- max(sous_croiss_taille3$age_mois)
y2 <- round(coef2_4[1,1],2)
sous_croiss_taille2<-croiss_taille_0_2[croiss_taille_0_2$num==enfants[1],]
xmin <- min(sous_croiss_taille2$age_mois)
xmax <- max(sous_croiss_taille2$age_mois)
y <- round(coef0_2[1,1],2)

ggplot(sous_croiss, aes(x = age_mois, y = taille)) + 
  labs(title=paste0("Enfant ", enfants[1]), x="Age (mois)", y="Taille(cm)") +
  geom_point(size=1.5, color="LightSkyBlue")+
  geom_abline(intercept = round(coef[1,1],2), slope = round(coef[1,2],2), color="Medium Purple") +
  geom_segment(aes(x=xmin, y=round(coef0_2[1,1],2), xend=xmax, yend=y+xmax*round(coef0_2[1,2],2)), color="orange", size=1)+
  geom_segment(aes(x=xmax, y=y+xmax*round(coef0_2[1,2],2), xend=xmax2, yend=y2+xmax2*round(coef2_4[1,2],2)), color="green", size=1)+
  xlim(0, max(croiss$age_mois))+ylim(0, max(croiss$taille, na.rm = TRUE))+
  theme_bw()
