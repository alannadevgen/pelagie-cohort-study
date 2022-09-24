saemixJenssBayley <- function(
  variable_anthropometrique = c("poids", "taille"),
  data = saemix.data,
  sexe = NULL,
  nb_param = 4,
  coefficients,
  distrib_param = c("normal", "log-normal"),
  estimation = c("restimation", "initial"),
  covariable = c(TRUE, FALSE),
  covariance = c(TRUE, FALSE),
  erreur = c("constant", "proportional", "combined"),
  save.diag = FALSE,
  save.graph = FALSE
){
  # distribution des estimateurs
  distrib_param <- ifelse(distrib_param == "normal", 0, 1)
  
  # estimation des parametres
  estimation <- ifelse(estimation == "restimation", 1, 0)
  
  # autorisation des covariances
  if (covariance == TRUE){
    covariance.model <- ones(nb_param)
  } else {
    covariance.model <- diag(nb_param)
  }
  
  # modele des covariables
  if (covariable == TRUE){
    covariate.model <- matrix(1, ncol = nb_param, byrow = TRUE)
  } else {
    covariate.model <- matrix(0, ncol = nb_param, byrow = TRUE)
  }
  
  model <- saemixModel(
    model = modSaemixJenssBayley,
    description = paste0("Modele Jenss Bayley - ", str_to_title(variable_anthropometrique)),
    psi0 = matrix(coefficients, ncol = nb_param, byrow = TRUE,
                  dimnames = list(NULL, c("Ap","Bp","Cp","Dp"))),
    transform.par = rep(distrib_param, nb_param),
    fixed.estim = rep(estimation, nb_param),
    covariance.model = covariance.model,
    covariate.model = covariate.model,
    error.model = erreur)
  
  saemix.fit <- saemix(
    model = model,
    data = data,
    control = list(seed = 632545,
                   save = save.diag,
                   save.graphs = save.graph,
                   directory = paste0("Graphiques/", str_to_title(variable_anthropometrique), "/GraphiquesJenssBayley", str_to_title(sexe))
                   )
  )
  
  # # Prints a summary of the results
  # print(saemix.fit)
  # 
  # # Outputs the estimates of individual parameters
  # psi(saemix.fit)
  # 
  # # Shows some diagnostic plots to evaluate the fit
  # plot(saemix.fit)
}