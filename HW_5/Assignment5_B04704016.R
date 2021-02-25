library( "MASS" )
library( "dplyr" )
library( "loo" )
library( "bayesplot" )
library( "GGally" )
library( "rethinking" )

# Load data
data( "eagles" )
data <- eagles; rm( eagles )

# Set dummy variables
data$P <- ifelse( data$P=="L" , 1 , 0 )
data$A <- ifelse( data$A=="A" , 1 , 0 )
data$V <- ifelse( data$V=="L" , 1 , 0 )

# Create data list for the stan model
data.model <- list(
  N = NROW( data ),
  y = data$y,
  n = data$n,
  P = data$P,
  A = data$A,
  V = data$V
)
str( data.model )

# Model code
model.a.stan <- "
data{
  int<lower=1> N;
  int y[N];
  int n[N];
  real P[N];
  real A[N];
  real V[N];
}
parameters{
  real a;
  real bP;
  real bA;
  real bV;
}
model{
  vector[N] p;
  bV ~ normal( 0 , 5 );
  bA ~ normal( 0 , 5 );
  bP ~ normal( 0 , 5 );
  a ~ normal( 0 , 10 );
  for ( i in 1:N ) {
    p[i] = a + bP * P[i] + bA * A[i] + bV * V[i];
    p[i] = inv_logit(p[i]);
  }
  y ~ binomial( n , p );
}
generated quantities{
  vector[N] log_lik;
  vector[N] p;
  vector[N] c;
  for ( i in 1:N ) {
    p[i] = a + bP * P[i] + bA * A[i] + bV * V[i];
    p[i] = inv_logit(p[i]);
    c[i] = binomial_rng( n[i] , p[i] );
    log_lik[i] = binomial_lpmf( y[i] | n[i] , p[i] );
  }
}
"

# Fit stan model with model code and model data
fit.a.stan <- stan( model_code=model.a.stan , data=data.model , 
              chains=4 , cores=4 , iter=1000 )

# Show results
print( fit.a.stan , probs=c(0.055,0.945) , pars=c("a","bP","bA","bV") )

# Plot coeftab
plot( fit.a.stan , pars=c("a","bP","bA","bV") )

# Paits plot
pairs( fit.a.stan , pars=c("a","bP","bA","bV") )
ggpairs( data=as.data.frame( fit.a.stan ), columns=c(1:4))

# Traceplot
extract_post <- rstan::extract( fit.a.stan , inc_warmup=TRUE , permuted=FALSE)
post_traceplot <- mcmc_trace( extract_post , n_warmup=0 , pars=c("a","bP","bA","bV"),
                facet_args=list( nrow=2 , labeller=label_parsed ) )
post_traceplot

# Fit quadratic approximation model
fit.a.quap <- map(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- a + bP*P + bA*A + bV*V,
    a ~ dnorm(0, 10),
    bP ~ dnorm(0, 5),
    bA ~ dnorm(0, 5),
    bV ~ dnorm(0, 5)
  ),
  data=data
)

# Show results
precis( fit.a.quap )

# Plot coeftab
plot( coeftab( fit.a.quap ) )

# Pairs plot
pairs( fit.a.quap )

# From the pairs plot for the stan model, we can see the skewed distribution of both bP and bV. On the other hand, for the same parameters, the quadratic qpproximation model shows symmetric distribution. We would prefer the stan model as it is closer to what we expect.

# (b)
# The predicted probability of success and the 89% interval for each row of data is as follows:
p <- as.data.frame(fit.a.stan)[,13:20]
p.mean <- apply( p , 2 , mean )
p.PI <- apply( p, 2, PI , prob=0.89 )
data$prob_success <- data$y / data$n
plot( data$prob_success , col=50 , ylab="probability of success" , xlab="each case of data (P.A.V)" , xlim=c(0,9) , ylim=c(0,1) , pch=16 , xaxt="n" )
axis( 1 , at=1:8 , labels=c("L.A.L","L.A.S","L.I.L","L.I.S","S.A.L","S.A.S","S.I.L","S.I.S") )
points( 1:8 , p.mean )
for ( i in 1:8 ) lines( c(i, i), p.PI[,i] )

# The predicted success count and the 89% interval for each row of data is as follows:
c <- as.data.frame(fit.a.stan)[,21:28]
c.mean <- apply( c , 2 , mean )
c.PI <- apply( c, 2, PI , prob=0.89 )
plot( data$y , col=50 , ylab="count of success" , xlab="each case of data (P.A.V)" , xlim=c(0,9) , ylim=c(0,30) , pch=16 , xaxt="n" )
axis( 1 , at=1:8 , labels=c("L.A.L","L.A.S","L.I.L","L.I.S","S.A.L","S.A.S","S.I.L","S.I.S") )
points( 1:8 , c.mean )
for ( i in 1:8 ) lines( c(i, i), c.PI[,i] )

# From the results, it shows that the probability of succes could depend on pirate eagle size, victim eagle size and the age of pirate eagle. Besides, the success count is even more relative to the age of pirate eagle.

# (c)
model.c.stan <- "
data{
  int<lower=1> N;
  int y[N];
  int n[N];
  real P[N];
  real A[N];
  real V[N];
}
parameters{
  real a;
  real bP;
  real bA;
  real bV;
  real bPA;
}
model{
  vector[N] p;
  bPA ~ normal( 0 , 5 );
  bV ~ normal( 0 , 5 );
  bA ~ normal( 0 , 5 );
  bP ~ normal( 0 , 5 );
  a ~ normal( 0 , 10 );
  for ( i in 1:N ) {
    p[i] = a + bP * P[i] + bA * A[i] + bV * V[i] + bPA * P[i] * A[i];
    p[i] = inv_logit(p[i]);
  }
  y ~ binomial( n , p );
}
generated quantities{
  vector[N] log_lik;
  vector[N] p;
  for ( i in 1:N ) {
    p[i] = a + bP * P[i] + bA * A[i] + bV * V[i] + bPA * P[i] * A[i];
    p[i] = inv_logit(p[i]);
    log_lik[i] = binomial_lpmf( y[i] | n[i] , p[i] );
  }
}
"

fit.c.stan <- stan( model_code=model.c.stan , data=data.model , 
               chains=4 , cores=4 , iter=1000 )
print( fit.c.stan , probs=c(0.055,0.945) , pars=c("a","bP","bA","bV","bPA") )
plot( fit.c.stan , pars=c("a","bP","bA","bV","bPA") )

# Pairs plot
pairs( fit.c.stan , pars=c("a","bP","bA","bV","bPA") )
ggpairs( data=as.data.frame( fit.c.stan ), columns=c(1:5))

# Traceplot
extract_post <- rstan::extract( fit.c.stan , inc_warmup=TRUE , permuted=FALSE)
post_traceplot <- mcmc_trace( extract_post , n_warmup=0 , pars=c("a","bP","bA","bV","bPA"),
                              facet_args=list( nrow=2 , labeller=label_parsed ) )
post_traceplot

# Compute WAIC for both stan models
ll.a.stan <- extract_log_lik(fit.a.stan, merge_chains=FALSE)
reff.a.stan <- relative_eff(exp(ll.a.stan))
waic.a.stan <- waic(ll.a.stan, r_eff=reff.a.stan, cores=4)
ll.c.stan <- extract_log_lik(fit.c.stan, merge_chains=FALSE)
reff.c.stan <- relative_eff(exp(ll.c.stan))
waic.c.stan <- waic(ll.c.stan, r_eff=reff.c.stan, cores=4)

# Compare WAICs
loo::compare(waic.a.stan, waic.c.stan)

# Since the elpd_diff and se values are both positive, it implies that the latter model, which considers the interaction betewwn variables P and A, predicts better than the other model.

# (b)????
post <- as.data.frame(fit.a.stan)
post_cov <- post %>%
  select(a, bP, bA, bV) %>% 
  cov() 
post_coef_mean <- post %>%
  head() %>%
  select(a, bP, bA, bV) %>%
  summarise_all(mean) %>%
  as.numeric()
post <- MASS::mvrnorm(n = 1e4, mu = post_coef_mean, Sigma = post_cov)

mu <- apply(as.data.frame(data), 1, function(x){
  apply(post, 1, function(y){
    inv_logit(y[1]+y[2]*x[3]+y[3]*x[4]+y[4]*x[5])
  })
})
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )

##########
extract.sample <- as.data.frame(extract(fit.c.stan, permuted = TRUE)) # return a list of arrays 
mu <- as.data.frame( sapply( extract.sample , function(x) mean(x) ) )
mu.PI.lower <- as.array(sapply( extract.sample , function(x) quantile( x , probs=0.055 ) ) )
mu.PI.upper <- as.array(sapply( extract.sample , function(x) quantile( x , probs=0.945 ) ) )
colnames(mu)[1] <- "mean"
mu$lower_0.89 <- mu.PI.lower
mu$upper_0.89 <- mu.PI.upper
print(mu[6:13,])
###########
library(MASS)
library(rethinking)
data(eagles)
d <- eagles
d$V <- ifelse(test = d$V == "L", yes = 1, no = 0)
d$A <- ifelse(test = d$A == "A", yes = 1, no = 0)
d$P <- ifelse(test = d$P == "L", yes = 1, no = 0)

m10H3.stan <- map2stan(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- alpha + beta_P*P + beta_A*A + beta_V*V,
    alpha ~ dnorm(0, 10),
    beta_P ~ dnorm(0, 5),
    beta_A ~ dnorm(0, 5),
    beta_V ~ dnorm(0, 5)
  ),
  data = d, warmup = 500, iter = 2500, chains = 2
)

m10H3.map <- map(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- alpha + beta_P*P + beta_A*A + beta_V*V,
    alpha ~ dnorm(0, 10),
    beta_P ~ dnorm(0, 5),
    beta_A ~ dnorm(0, 5),
    beta_V ~ dnorm(0, 5)
  ),
  data = d
)

# compare
precis(m10H3.stan)
precis(m10H3.map)

pairs(m10H3.stan)
pairs(m10H3.map)

# When looking at the pairs plot for the model fit with MCMC, we can see the same long-tail effect in both beta_P and beta_V as we saw in 10H1. 
# In the model fit with MAP, we see symmetric posteriors for these parameters. The former is what we'd expect to see, as our logistic functions allows
# for a long tail of valid parameters for a given outcome.

# b)

# Just looking at the MAP values for each of the parameters:

# V: A value of "large" (i.e. the variable "V" is "on") would cause a sigmoid(-5.04) = 0.0064% increase in the probability of the positive event.
# P: A value of "large" would cause a sigmoid(4.64) = 0.9904% increase in the probability of the positive event.
# A: A value of "adult" would cause a sigmoid(4.64) = 0.753% increase in the probability of the positive event.
#
# Of course, in real-value terms (the actual value of `p`), these marginal changes all depends on what the values of the other variables are.

p <- link(m10H3.stan)
y <- sim(m10H3.stan)

p.mean <- apply(X = p, MARGIN = 2, FUN = mean)
p.PI <- apply(X = p, MARGIN = 2, FUN = PI)
y.mean <- apply(X = y, MARGIN = 2, FUN = mean)
y.PI <- apply(X = y, MARGIN = 2, FUN = PI)

# plot the model predictions for `p` vs. the actual proportion of successes for each case
d$success.proportion <- d$y / d$n
plot(d$success.proportion, col=rangi2, ylab="successful proportion", xlab="case", xaxt="n", xlim=c(0.75,8.25) , ylim = c(0, 1), pch=16)
axis(1, at=1:8, labels=c( "LAL","LAS","LIL","LIS","SAL","SAS","SIL","SIS" ))
points( 1:8 , p.mean )
for ( i in 1:8 ) lines( c(i, i), p.PI[,i] )

# plot the model predictions for `y` vs. the actual number of successes for each case
plot(d$y, col=rangi2, ylab="number of successes successful", xlab="case", xaxt="n", xlim=c(0.75,8.25) , ylim = c(0, 30), pch=16)
axis(1, at=1:8, labels=c( "LAL","LAS","LIL","LIS","SAL","SAS","SIL","SIS" ))
points( 1:8 , y.mean )
for ( i in 1:8 ) lines( c(i, i), y.PI[,i] )

# c)

m10H3.stan.interaction <- map2stan(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- alpha + beta_P*P + beta_A*A + beta_V*V + beta_P_A*P*A,
    alpha ~ dnorm(0, 10),
    beta_P ~ dnorm(0, 5),
    beta_A ~ dnorm(0, 5),
    beta_V ~ dnorm(0, 5),
    beta_P_A ~ dnorm(0, 5)
  ),
  data = d, warmup = 500, iter = 2500, chains = 2
)

compare(m10H3.stan, m10H3.stan.interaction)
precis(m10H3.stan.interaction)
