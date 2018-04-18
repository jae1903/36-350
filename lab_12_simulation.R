generate_data = function(n,p) {
  covariates = matrix(rnorm(n*p, 0, 1), nrow = n, ncol = p)
  responses = rnorm(n)
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses, cutoff) {
  lm.res.cov = lm(responses ~ covariates)
  p.value = (summary(lm.res.cov)$coefficients)[-1, 4]
  na.vals = which(p.value <= cutoff)
  if (length(na.vals) == 0) {
    return(c())
  }
  lm.res.cov.2 = lm(responses ~ covariates[, na.vals])
  p.value.2 = (summary(lm.res.cov.2)$coefficients)[-1, 4]
  return(p.value.2)
}
