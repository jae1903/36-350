generate_data = function(n,p) {
  covariates = matrix(rnorm(n*p, 0, 1), nrow = n, ncol = p)
  responses = rnorm(n)
  return(list(covariates = covariates, responses = responses))
}

