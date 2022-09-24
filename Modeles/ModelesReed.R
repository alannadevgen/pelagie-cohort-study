# Modele de Reed (1er ordre) --------------------------------------------------------

# ecriture du modele de Reed (1er ordre)
modReed1 <- function(time, Ap, Bp, Cp, Dp){
  Ap + Bp*time + Cp*log(time) + Dp/time
}

# ecriture du modele pour saemix
modSaemixReed1 <- function(psi, id, xidep) {
  time   <- xidep[,1]
  Ap     <- psi[id,1]
  Bp     <- psi[id,2]
  Cp     <- psi[id,3]
  Dp     <- psi[id,4]
  ypred <- Ap + Bp*time + Cp*log(time) + Dp/time
  return(ypred)
}

# Modele Reed (2eme ordre) ----------------------------------------------------------

# ecriture du modele Reed (2eme ordre)
modReed2 <- function(time, Ap, Bp, Cp, Dp, Ep){
  Ap + Bp*time + Cp*log(time) + Dp/time + Ep/(time^2)
}

# Equation du modele Reed (2eme ordre)
modSaemixReed2 <-function(psi, id, xidep) {
  time   <-xidep[,1]
  Ap     <- psi[id,1]
  Bp     <- psi[id,2]
  Cp     <- psi[id,3]
  Dp     <- psi[id,4]
  Ep     <- psi[id,5]
  ypred <- Ap + Bp*time + Cp*log(time) + Dp/time + Ep/(time^2)
  return(ypred)
}
