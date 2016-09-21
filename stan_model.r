library (rstan)
stan.data <- list (x = c(0.1, 0.5, 0.6), y = c(0.35, 0.9, 1.1), sigma_e = 0.25, new_x = 0.75)
my.stan.model <- stan (file="model.stan", data=stan.data, iter=2000, chains=4)
svg ("stan-model-posterior-samples.svg")
traceplot (my.stan.model, pars = c("alpha"), inc_warmup = T)
dev.off ()

y_pred_struct <- extract (my.stan.model, 'y_pred')
y_pred <- unlist (y_pred_struct, use.names=F)
svg ("y-given-x-via-stan.svg")
hist (y_pred, freq=F, main="Density of y|x via Stan")
dev.off ()
