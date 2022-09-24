# Description --------------------------------------------------------------------

# base de code pour le modele SITAR, pas poursuivi

# author : @alannagenin, @camillepliquet

# Set-up -------------------------------------------------------------------------

# working directory
setwd("../2_ModelesCroissance")
setwd("2_ModelesCroissance")

# vider l'espace de travail et charger les donnees
rm(list = ls())
load("../0_Datamanagement/DataDatamanagement.RData")

# packages
library(sitar)
library(plyr)
library(nlme)
library(lattice)

# Modele SITAR --------------------------------------------------------------------

croiss <- croiss_taille
model <- sitar(x=age_mois,
               y=taille,
               id=num,
               data=croiss,
               df=5)
summary(model)

outliers <- velout(age_mois, taille,  num,  croiss, limit=2)
codeplot(outliers, icode=c(4,6))
# on voit que les valeurs aberrantes sont dans le code 6
croiss2 <- zapvelout(outliers, icode=6) %>%
  filter(!is.na(taille))


plot(model, y2par=list(col="blue"), apv=TRUE)

plot(x = croiss$age_mois, y = croiss$taille)
new.data <- data.frame(age_mois = seq(min(croiss$age_mois),
                                      max(croiss$age_mois),
                                      length.out = nrow(croiss)))
lines(x = new.data$age_mois, y = predict(object = model, newdata = new.data), 
      col = "green", lwd = 2, lty = 2)
lines(x = new.data$age_mois, y = predict(object = modelReed, newdata = new.data), 
      col = "purple", lwd = 2, lty = 2)
lines(x = new.data$age_mois, y = predict(object = modelReed2, newdata = new.data), 
      col = "orange", lwd = 2, lty = 2)

