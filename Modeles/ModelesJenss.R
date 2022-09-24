# Modele de Jenss-Bayley --------------------------------------------------------

# ecriture du modele de Jenss-Bayley
# modJenssBayley <- function(time, Ap, Bp, Cp, Dp){
#   Ap + Bp*time - exp(Cp + Dp*time)
# }

# seconde parametrisation
modJenssBayley <- function(time, Ap, Bp, Cp, Dp){
  Ap + Bp*time + Cp*(1 - exp(-Dp*time))
}

# ecriture du modele de Jenss-Bayley avec les parametres a l'exponentielle
# modJenssBayleyExp <- function(time, Ap, Bp, Cp, Dp){
#   exp(Ap) + exp(-Bp)*time + exp(Cp)*(1 - exp(-exp(-Dp)*time))
# }

# ecriture du modele pour saemix
modSaemixJenssBayley <- function(psi, id, xidep) {
  time   <- xidep[,1]
  Ap     <- psi[id,1]
  Bp     <- psi[id,2]
  Cp     <- psi[id,3]
  Dp     <- psi[id,4]
  ypred <- Ap - Bp*time + Cp*(1 - exp(-Dp*time))
  return(ypred)
}

# modele de Jenss-Bayley pour le modele saemix avec les parametres a l'exponentielle
# modSaemixJenssBayleyExp <- function(psi, id, xidep) {
#   time   <- xidep[,1]
#   Ap     <- psi[id,1]
#   Bp     <- psi[id,2]
#   Cp     <- psi[id,3]
#   Dp     <- psi[id,4]
#   ypred <- exp(Ap) + exp(-Bp)*time + exp(Cp)*(1 - exp(-exp(-Dp)*time))
#   return(ypred)
# }

# Modele Jenss modifie ----------------------------------------------------------

# ecriture du modele Jenss modifie
modJenssModifie <- function(time, Ap, Bp, Cp, Dp, Ep){
  Ap + Bp*time + Cp - Cp*exp(-time*Dp) + Ep*time^2
}

# Equation du modele de Jenss adapte (modele de base, sans covariable)
modSaemixJenssModifie <-function(psi, id, xidep) {
  time   <- xidep[,1]
  Ap     <- psi[id,1]
  Bp     <- psi[id,2]
  Cp     <- psi[id,3]
  Dp     <- psi[id,4]
  Ep     <- psi[id,5]
  ypred <- Ap + Bp*time + Cp - Cp*exp(-time*Dp) + Ep*time^2
  return(ypred)
}