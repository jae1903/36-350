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

run_simulation = function(n_trials, n, p, cutoff) {
  p.value = vector()
  for (i in 1:n_trials) {
    data = generate_data(n, p)
    cov = data$covariates
    res = data$responses
    model = model_select(cov, res, cutoff)
    p.value = c(p.value, model)
  }
  save(p.value, file = "p_value.RData")
}

run_simulation(100, 100, 10, 0.05)
run_simulation(1000, 1000, 20, 0.05)
run_simulation(10000, 10000, 50, 0.05)
