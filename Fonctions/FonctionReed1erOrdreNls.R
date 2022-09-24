# modele nls Reed du 1er ordre pour la taille ----------------------------------------------------
nlsReed1Taille <- function(data = croiss_modele,
                                 param = list(Ap = 0.5, Bp = 1, Cp = 1, Dp = 0.1)){
  nlsReed1 <- nls(taille ~ modReed1(age_mois_no_zero, Ap, Bp, Cp, Dp),
                        data = data,
                        start = param,
                        trace = TRUE,
                        # lower = c(0, 0, 0, 0),
                        algorithm = "port",
                        control = nls.control(maxiter = 10000, minFactor = 1e-10))
  return(nlsReed1)
}
 # modele nls Reed du 1er ordre pour le poids ----------------------------------------------------
nlsReed1Poids <- function(data = croiss_modele,
                                param = list(Ap = 0.5, Bp = 1, Cp = 1, Dp = 1)){
  nlsReed1 <- nls(poids ~ modReed1(age_mois_no_zero, Ap, Bp, Cp, Dp),
                        data = data,
                        start = param,
                        trace = TRUE,
                        # lower = c(0, 0, 0, 0),
                        algorithm = "port",
                        control = nls.control(maxiter = 10000, minFactor = 1e-10))
  return(nlsReed1)
}