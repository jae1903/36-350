generate_data = function(n,p) {
  covariates = matrix(rnorm(n), nrow = n, ncol = p)
  responses = rnorm(n)
  return(list(covariates = covariates, responses = responses))
}

generate_data(3, 5)
