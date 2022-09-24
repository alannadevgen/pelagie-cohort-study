# modele nls Reed du 2nd ordre pour la taille ----------------------------------------------------
nlsReed2Taille <- function(data = croiss_modele,
                                 param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 1, Ep = 1)){
  nlsReed2 <- nls(taille ~ modReed2(age_mois_no_zero, Ap, Bp, Cp, Dp, Ep),
                        data = data,
                        start = param,
                        trace = TRUE,
                        # lower = c(0, 0, 0, 0, 0),
                        algorithm = "port",
                        control = nls.control(maxiter = 10000, minFactor = 1e-10))
  return(nlsReed2)
}
 # modele nls Reed du 2nd ordre pour le poids ----------------------------------------------------
nlsReed2Poids <- function(data = croiss_modele,
                                param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 1, Ep = 1)){
  nlsReed2 <- nls(poids ~ modReed2(age_mois_no_zero, Ap, Bp, Cp, Dp, Ep),
                        data = data,
                        start = param,
                        trace = TRUE,
                        # lower = c(0, 0, 0, 0, 0),
                        algorithm = "port",
                        control = nls.control(maxiter = 10000, minFactor = 1e-10))
  return(nlsReed2)
}