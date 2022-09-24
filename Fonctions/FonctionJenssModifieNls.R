# modele nls Jenss modifie pour la taille ----------------------------------------------------
nlsJenssModifieTaille <- function(data = croiss_modele,
                                 param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 0.1, Ep = 1)){
  
  nlsJenssModifie <- nls(taille ~ modJenssModifie(age_mois, Ap, Bp, Cp, Dp, Ep),
                        data = data,
                        start = param,
                        trace = TRUE,
                        # lower = rep(0, 5),
                        algorithm = "port",
                        control = nls.control(maxiter = 10000, minFactor = 1e-10))
  return(nlsJenssModifie)
}
 # modele nls Jenss modifie pour le poids ----------------------------------------------------
nlsJenssModifiePoids <- function(data = croiss_modele,
                                param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 1, Ep = 1)){
  nlsJenssModifie <- nls(poids ~ modJenssModifie(age_mois, Ap, Bp, Cp, Dp, Ep),
                        data = data,
                        start = param,
                        trace = TRUE,
                        # lower = rep(0, 5),
                        algorithm = "port",
                        control = nls.control(maxiter = 10000, minFactor = 1e-10))
  return(nlsJenssModifie)
}