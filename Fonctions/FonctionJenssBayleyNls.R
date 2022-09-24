# modele nls Jenss Bayley pour la taille ----------------------------------------------------
nlsJenssBayleyTaille <- function(data = croiss_modele,
                                 param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 0.1)){
  nlsJenssBayley <- nls(taille ~ modJenssBayley(age_mois, Ap, Bp, Cp, Dp),
                        data = data,
                        start = param,
                        trace = TRUE,
                        lower = c(0, 0, 0, 0),
                        algorithm = "port",
                        control = nls.control(maxiter = 10000, minFactor = 1e-10))
  return(nlsJenssBayley)
}
 # modele nls Jenss Bayley pour le poids ----------------------------------------------------
nlsJenssBayleyPoids <- function(data = croiss_modele,
                                param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 0.1)){
  nlsJenssBayley <- nls(poids ~ modJenssBayley(age_mois, Ap, Bp, Cp, Dp),
                        data = data,
                        start = param,
                        trace = TRUE,
                        lower = c(0, 0, 0, 0),
                        algorithm = "port",
                        control = nls.control(maxiter = 10000, minFactor = 1e-10))
  return(nlsJenssBayley)
}

# modele nls Jenss Bayley à l'exponentielle pour la taille ----------------------------------------------------
nlsJenssBayleyTailleExp <- function(data = croiss_modele,
                                 param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 1)){
  nlsJenssBayley <- nls(taille ~ modJenssBayleyExp(age_mois, Ap, Bp, Cp, Dp),
                        data = data,
                        start = param,
                        trace = TRUE,
                        lower = c(0, 0, 0, 0),
                        algorithm = "port",
                        control = nls.control(maxiter = 10000, minFactor = 1e-10))
  return(nlsJenssBayley)
}
# modele nls Jenss Bayley à l'exponentielle  pour le poids ----------------------------------------------------
nlsJenssBayleyPoidsExp <- function(data = croiss_modele,
                                param = list(Ap = 1, Bp = 1, Cp = 1, Dp = 1)){
  nlsJenssBayley <- nls(poids ~ modJenssBayleyExp(age_mois, Ap, Bp, Cp, Dp),
                        data = data,
                        start = param,
                        trace = TRUE,
                        lower = c(0, 0, 0, 0),
                        algorithm = "port",
                        control = nls.control(maxiter = 10000, minFactor = 1e-10))
  return(nlsJenssBayley)
}