library( "loo" )
library( "bayesplot" )
library( "rethinking" )
# Question 1
# Load data
data( "salamanders" )
data <- salamanders; rm( salamanders )
data$FORESTAGE <- ( data$FORESTAGE - mean( data$FORESTAGE ))/sd( data$FORESTAGE )
str( data )

# Create data list for the stan model
data.model <- list(
  N = NROW( data ),
  salaman = data$SALAMAN,
  pct_cover = data$PCTCOVER,
  forest_age = data$FORESTAGE
)

# Model code
model.stan <- "
data{
  int<lower=1> N;
  int salaman[N];
  int pct_cover[N];
}
parameters{
  real a;
  real b_pct_cover;
}
model{
  vector[N] lamda;
  b_pct_cover ~ uniform( -15 , 15 );
  a ~ uniform( -30, 30 );
  for ( i in 1:N ) {
    lamda[i] = a + b_pct_cover * pct_cover[i];
    lamda[i] = exp( lamda[i] );
  }
  salaman ~ poisson( lamda );
}
generated quantities{
  vector[N] log_lik;
  vector[N] lamda;
  for ( i in 1:N ) {
    lamda[i] = a + b_pct_cover * pct_cover[i];
    lamda[i] = exp( lamda[i] );
    log_lik[i] = poisson_lpmf( salaman[i] | lamda[i] );
  }
}
"

# Fit stan model with model code and model data
fit.stan.1 <- stan( model_code=model.stan , data=data.model , 
              chains=4 , cores=4 , iter=1000 )

# Show results
print( fit.stan.1 , probs=c(0.055,0.945) , pars=c("a","b_pct_cover") )

# Plot coeftab
plot( fit.stan.1 , pars=c("a","b_pct_cover") )

# Paits plot
pairs( fit.stan.1 , pars=c("a","b_pct_cover") )

# Traceplot
extract_post <- rstan::extract( fit.stan.1 , inc_warmup=TRUE , permuted=FALSE)
post_traceplot <- mcmc_trace( extract_post , n_warmup=0 , pars=c("a","b_pct_cover") ,
                facet_args=list( nrow=2 , labeller=label_parsed ) )
post_traceplot

# The predicted results of each row of data and the 89% interval are as follows:
p <- as.data.frame(fit.stan.1)[,50:96]
p.mean <- apply( p , 2 , mean )
p.PI <- apply( p, 2, PI , prob=0.89 )
plot(SALAMAN ~ PCTCOVER, data=data, col=50, pch=16)
points(data$PCTCOVER , p.mean)
for ( i in 1:47 ) lines( c(data$PCTCOVER[i], data$PCTCOVER[i]), p.PI[,i] )

# From all the plots and results above, we could see that the model did not predict so well, maybe some more variables should be added to the model.

# Question 2
model.stan <- "
data{
  int<lower=1> N;
  int salaman[N];
  int pct_cover[N];
  real forest_age[N];
}
parameters{
  real a;
  real b_pct_cover;
  real b_forest_age;
}
model{
  vector[N] lamda;
  b_forest_age ~ uniform( -15 , 15 );
  b_pct_cover ~ uniform( -15 , 15 );
  a ~ uniform( -30, 30 );
  for ( i in 1:N ) {
    lamda[i] = a + b_pct_cover * pct_cover[i] + b_forest_age * forest_age[i];
    lamda[i] = exp( lamda[i] );
  }
    salaman ~ poisson( lamda );
}
generated quantities{
  vector[N] log_lik;
  vector[N] lamda;
  for ( i in 1:N ) {
    lamda[i] = a + b_pct_cover * pct_cover[i] + b_forest_age * forest_age[i];
    lamda[i] = exp( lamda[i] );
    log_lik[i] = poisson_lpmf( salaman[i] | lamda[i] );
  }
}
"

# Fit stan model with model code and model data
fit.stan.2 <- stan( model_code=model.stan , data=data.model , 
               chains=4 , cores=4 , iter=1000 )

# Show results
print( fit.stan.2 , probs=c(0.055,0.945) , pars=c("a","b_pct_cover","b_forest_age") )

# Plot coeftab
plot( fit.stan.2 , pars=c("a","b_pct_cover","b_forest_age") )

# Pairs plot
pairs( fit.stan.2 , pars=c("a","b_pct_cover","b_forest_age") )

# Traceplot
extract_post <- rstan::extract( fit.stan.2 , inc_warmup=TRUE , permuted=FALSE)
post_traceplot <- mcmc_trace( extract_post , n_warmup=0 , pars=c("a","b_pct_cover","b_forest_age"),
                              facet_args=list( nrow=2 , labeller=label_parsed ) )
post_traceplot

# Compute WAIC for both stan models
ll.stan.1 <- extract_log_lik(fit.stan.1, merge_chains=FALSE)
reff.stan.1 <- relative_eff(exp(ll.stan.1))
waic.stan.1 <- waic(ll.stan.1, r_eff=reff.a.stan, cores=4)
ll.stan.2 <- extract_log_lik(fit.stan.2, merge_chains=FALSE)
reff.stan.2 <- relative_eff(exp(ll.stan.2))
waic.stan.2 <- waic(ll.stan.2, r_eff=reff.c.stan, cores=4)

# Compare WAICs
loo::compare(waic.stan.1, waic.stan.2)

# By comparing the second model to the first one, there isn't much improvement since the elpd_diff value is negative. The reason that FORESTAGE did not help enhance prediction may because it is not relative to the outcome variable.


###########

data("salamanders")
d <- salamanders

f <- alist(
  SALAMAN ~ dpois(lambda),
  log(lambda) <- alpha + beta_pct_cover*PCTCOVER,
  alpha ~ dnorm(0, 10),
  beta_pct_cover ~ dnorm(0, 5)
)

m10H4.map <- map(f, data = d)
m10H4.stan <- map2stan(f, data = d, warmup = 1e3, iter = 3e3, chains = 2)
stancode(m10H4.stan)
# compare models
precis(m10H4.map)
precis(m10H4.stan)

# plot model predictions
pctcover.seq <- seq(from=0,to=100,length.out=30)
lambda <- link(m10H4.stan, data = list(PCTCOVER = pctcover.seq))
lambda.mean <- apply(X = lambda, MARGIN = 2, FUN = mean)
lambda.PI <- apply(X = lambda, MARGIN = 2, FUN = PI)

counts <- sim(m10H4.stan, data = list(PCTCOVER = pctcover.seq))
counts.mean <- apply(X = counts, MARGIN = 2, FUN = mean)
counts.PI <- apply(X = counts, MARGIN = 2, FUN = PI)

plot(SALAMAN ~ PCTCOVER, data = d)
lines(pctcover.seq, lambda.mean)
shade(lambda.PI, pctcover.seq)
shade(counts.PI, pctcover.seq)

