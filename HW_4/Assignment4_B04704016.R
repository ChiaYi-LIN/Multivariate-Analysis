# Question 1
# (a)
rm(list=ls())
library(rethinking)
data(rugged)
data_rugged <- rugged

# Drop incomplete rows
data_rugged <- data_rugged[ complete.cases( data_rugged$rgdppc_2000 ) , ]

# Get log GDP
data_rugged$log_gdp <- log( data_rugged$rgdppc_2000 )

# Rescale variables
data_rugged$log_gdp_std <- data_rugged$log_gdp / mean(data_rugged$log_gdp)
data_rugged$rugged_std <- data_rugged$rugged / max(data_rugged$rugged)
mean(data_rugged$rugged_std)
# data_rugged$cent <- data_rugged$rugged_std - mean(data_rugged$rugged_std)

# make variable to index Africa (1) or not (2) 8.8
# data_rugged$cid <- ifelse( data_rugged$cont_africa==1 , 1 , 2 )

# Drop Seychelles
data_rugged_without_seychelles <- data_rugged[ data_rugged$country!="Seychelles" , ]
mean(data_rugged_without_seychelles$rugged_std)

# Split countries into Africa and not-Africa
# data_rugged.A1 <- data_rugged[ data_rugged$cont_africa==1 , ] # Africa
# data_rugged.A0 <- data_rugged[ data_rugged$cont_africa==0 , ] # not Africa

# Set input data
rugged_seq <- seq( from=0 , to=1 , length.out=30 )
data_rugged.tomodel <- list(
  N = NROW(data_rugged),
  cont_africa = as.integer(data_rugged$cont_africa),
  rugged_std = data_rugged$rugged_std,
  log_gdp_std = data_rugged$log_gdp_std
)
str(data_rugged.tomodel)

# Modeling
model.rugged <- ulam(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ),
    mu <- a + bA*cont_africa + bR*(rugged_std - 0.2149601) + bAR*cont_africa*(rugged_std - 0.2149601),
    a ~ dnorm( 1 , 0.1 ),
    bA ~ dnorm( 0 , 0.3 ),
    bR ~ dnorm( 0 , 0.3 ),
    bAR ~ dnorm( 0 , 0.3 ),
    sigma ~ dexp( 1 )
  ),
  data=data_rugged.tomodel, chains=4 , cores=4 , iter=1000
)
precis( model.rugged , 2 )

par(mfrow=c(1,2))
for ( s in 0:1 ) {
  idx <- which( data_rugged$cont_africa==s )
  plot( data_rugged$rugged_std[idx] , data_rugged$log_gdp_std[idx] , xlim=c(0,1) , ylim=c(0.7,1.3) ,
        xlab="ruggness" , ylab="log GDP" , pch=16 , col=rangi2 )
  mu <- link( model.rugged , data=data.frame( cont_africa=s , rugged_std=rugged_seq ))
  mu_mean <- apply( mu , 2 , mean )
  mu_PI <- apply( mu , 2 , PI , prob=0.97 )
  lines( rugged_seq , mu_mean , lwd=2 )
  shade( mu_PI , rugged_seq , col=col.alpha(rangi2,0.3) )
  if ( s==0 ) {
    mtext("Non-African nations")
  } else {
    mtext("African nations")
  }
}

data_rugged_without_seychelles.tomodel <- list(
  N = NROW(data_rugged_without_seychelles),
  cont_africa = as.integer(data_rugged_without_seychelles$cont_africa),
  rugged_std = data_rugged_without_seychelles$rugged_std,
  log_gdp_std = data_rugged_without_seychelles$log_gdp_std
)
str(data_rugged_without_seychelles.tomodel)

model.without.seychelles <- ulam(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ),
    mu <- a + bA*cont_africa + bR*(rugged_std - 0.2115714) + bAR*cont_africa*(rugged_std - 0.2115714),
    a ~ dnorm( 1 , 0.1 ),
    bA ~ dnorm( 0 , 0.3 ),
    bR ~ dnorm( 0 , 0.3 ),
    bAR ~ dnorm( 0 , 0.3 ),
    sigma ~ dexp( 1 )
  ),
  data=data_rugged_without_seychelles.tomodel, chains=4 , cores=4 , iter=1000
)
precis( model.without.seychelles , 2 )

par(mfrow=c(1,2))
for ( s in 0:1 ) {
  idx <- which( data_rugged_without_seychelles.tomodel$cont_africa==s )
  plot( data_rugged_without_seychelles.tomodel$rugged_std[idx] , data_rugged_without_seychelles.tomodel$log_gdp_std[idx] , xlim=c(0,1) , ylim=c(0.7,1.3) ,
        xlab="ruggness" , ylab="log GDP" , pch=16 , col=rangi2 )
  mu <- link( model.without.seychelles , data=data.frame( cont_africa=s , rugged_std=rugged_seq ))
  # for ( i in 1:20 ) lines( -1:1 , mu[i,] , col=col.alpha("black",0.3) )
  mu_mean <- apply( mu , 2 , mean )
  mu_PI <- apply( mu , 2 , PI , prob=0.97 )
  lines( rugged_seq , mu_mean , lwd=2 )
  shade( mu_PI , rugged_seq , col=col.alpha(rangi2,0.3) )
  if ( s==0 ) {
    mtext("Non-African nations")
  } else {
    mtext("African nations")
  }
}

# Conclusion:
# According to the graphs above, we could find out that no matter Seychelles is included in the model or not, ruggness is positively related to log GDP of African nations. On the other hand, ruggness is negatively related to log GDP of non-African nations in both models. However, the difference between two models is that for the one that does not include Seychelles, the slope of ruggness and log GDP of African nations seems to be flatter, compared to the original model.

# (b)
# Plot the predictions
par(mfrow=c(2,2))
for ( s in 0:1 ) {
  idx <- which( data_rugged$cont_africa==s )
  plot( data_rugged$rugged_std[idx] , data_rugged$log_gdp_std[idx] , xlim=c(0,1) , ylim=c(0.7,1.3) ,
        xlab="ruggness" , ylab="log GDP" , pch=16 , col=rangi2 )
  mu <- link( model.rugged , data=data.frame( cont_africa=s , rugged_std=rugged_seq ))
  for ( i in 1:20 ) lines( rugged_seq , mu[i,] , col=col.alpha("black",0.3) )
  if ( s==0 ) {
    mtext("With Seychelles, Non-African nations")
  } else {
    mtext("With Seychelles, African nations")
  }
}
for ( s in 0:1 ) {
  idx <- which( data_rugged_without_seychelles$cont_africa==s )
  plot( data_rugged_without_seychelles$rugged_std[idx] , data_rugged_without_seychelles$log_gdp_std[idx] , xlim=c(0,1) , ylim=c(0.7,1.3) ,
        xlab="ruggness" , ylab="log GDP" , pch=16 , col=rangi2 )
  mu <- link( model.without.seychelles , data=data.frame( cont_africa=s , rugged_std=rugged_seq ))
  for ( i in 1:20 ) lines( rugged_seq , mu[i,] , col=col.alpha("black",0.3) )
  if ( s==0 ) {
    mtext("Without Seychelles, Non-African nations")
  } else {
    mtext("Without Seychelles, African nations")
  }
}

# Conclusion:
# As for the predictions, we could find out that for each model, the predictions on log GDP of non-African nations are quite the same. However, as for predicting log GDP of African nations, the model not including Seychelles generates predictions of more flatter slopes than the other one.

# (c)
model.c1 <- map2stan(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ),
    mu <- a + bR*(rugged_std - 0.2115714),
    a ~ dnorm( 1 , 0.1 ),
    bR ~ dnorm( 0 , 0.3 ),
    sigma ~ dexp( 1 )
  ),
  data=data_rugged_without_seychelles.tomodel, chains=4 , cores=4 , iter=1000
)
precis( model.c1 , 2 )

model.c2 <- map2stan(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ),
    mu <- a + bA*cont_africa + bR*(rugged_std - 0.2115714),
    a ~ dnorm( 1 , 0.1 ),
    bA ~ dnorm( 0 , 0.3 ),
    bR ~ dnorm( 0 , 0.3 ),
    sigma ~ dexp( 1 )
  ),
  data=data_rugged_without_seychelles.tomodel, chains=4 , cores=4 , iter=1000
)
precis( model.c2 , 2 )

model.c3 <- map2stan(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ),
    mu <- a + bA*cont_africa + bR*(rugged_std - 0.2115714) + bAR*cont_africa*(rugged_std - 0.2115714),
    a ~ dnorm( 1 , 0.1 ),
    bA ~ dnorm( 0 , 0.3 ),
    bR ~ dnorm( 0 , 0.3 ),
    bAR ~ dnorm( 0 , 0.3 ),
    sigma ~ dexp( 1 )
  ),
  data=data_rugged_without_seychelles.tomodel, chains=4 , cores=4 , iter=1000
)
precis( model.c3 , 2 )

compare( model.c1 , model.c2 , model.c3 )

par(mfrow=c(1,1))
prediction.data <- data.frame( rugged_std = rugged_seq, cont_africa = 1 )
mu.ensemble <- ensemble( model.c1 , model.c2 , model.c3 , data = prediction.data )
mu.mean <- apply( mu.ensemble$link , 2 , mean )
mu.PI <- apply( mu.ensemble$link , 2 , PI , prob=0.89 )
data.plot <- data_rugged_without_seychelles[(data_rugged_without_seychelles$cont_africa == 1) , ]
plot( log_gdp_std ~ rugged_std , data=data.plot )
lines( rugged_seq , mu.mean )
lines( rugged_seq , mu.PI[1,] )
lines( rugged_seq , mu.PI[2,] )

# Conclusion:
# The inferences are quite similar to the results in part(b), because both predictions drop Seychelles out of the models. Also, both models consider the interactions between variables.

# Question 2
# (a)
data(nettle)
data_nettle <- nettle
data_nettle <- data_nettle[ complete.cases( data_nettle$num.lang , data_nettle$k.pop , data_nettle$area ) , ]
data_nettle$lang.per.cap <- data_nettle$num.lang/data_nettle$k.pop
data_nettle$log.lang.per.cap <- log(data_nettle$lang.per.cap)
data_nettle$log.area <- log(data_nettle$area)
data_nettle$log.k.pop <- log(data_nettle$k.pop)
data_nettle$pop.dens <- data_nettle$log.k.pop/data_nettle$log.area

# Standardize variables
data_nettle$log_lang_per_cap_std <- (data_nettle$log.lang.per.cap - mean(data_nettle$log.lang.per.cap))/sd(data_nettle$log.lang.per.cap)
min_lang <- min(data_nettle$log_lang_per_cap_std)
max_lang <- max(data_nettle$log_lang_per_cap_std)
data_nettle$mean_growing_season_std <- (data_nettle$mean.growing.season - mean(data_nettle$mean.growing.season))/sd(data_nettle$mean.growing.season)
min_mean_grow <- min(data_nettle$mean_growing_season_std)
max_mean_grow <- max(data_nettle$mean_growing_season_std)
data_nettle$sd_growing_season_std <- (data_nettle$sd.growing.season - mean(data_nettle$sd.growing.season))/sd(data_nettle$sd.growing.season)
min_sd_grow <- min(data_nettle$sd_growing_season_std)
max_sd_grow <- max(data_nettle$sd_growing_season_std)
data_nettle$area_std <- (data_nettle$area - mean(data_nettle$area))/sd(data_nettle$area)
# data_nettle$pop_dens_std <- (data_nettle$pop.dens - mean(data_nettle$pop.dens))/sd(data_nettle$pop.dens)

# Modeling
model.2a <- map2stan(
  alist(
    log_lang_per_cap_std ~ dnorm( mu , sigma ),
    mu <- a + bM*mean_growing_season_std + bA*area_std,
    a ~ dnorm( 0 , 0.5 ),
    bM ~ dnorm( 0 , 0.5 ),
    bA ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 )
  ),
  data=data_nettle, chains=4 , cores=4 , iter=1000
)

# Inspect prior predictions
par(mfrow=c(1,1))
prior <- extract.prior( model.2a )
plot( NULL , xlim=c(floor(min_mean_grow),ceiling(max_mean_grow)) , ylim=c(floor(min_lang),ceiling(max_lang)) ,
      xlab="standardized mean growing season" , ylab="standardized log language per capita" )
abline( h=min_lang , lty=2 )
abline( h=max_lang , lty=2 )
growing_season_seq <- seq( from=floor(min_mean_grow) , to=ceiling(max_mean_grow) , length.out=50 )
mu <- link( model.2a , post=prior , data=data.frame( mean_growing_season_std=growing_season_seq, area_std=0 ))
for ( i in 1:50 ) lines( growing_season_seq , mu[i,] , col=col.alpha("black",0.3) )

precis( model.2a )
plot( coeftab( model.2a ) )

# Conclusion:
# The results seem to support the hypothesis. As the bM coefficient is positive, we could consider that language diversity is positively associated with the average length of the growing season.

# (b)
model.2b <- map2stan(
  alist(
    log_lang_per_cap_std ~ dnorm( mu , sigma ),
    mu <- a + bS*sd_growing_season_std + bA*area_std,
    a ~ dnorm( 0 , 0.5 ),
    bS ~ dnorm( 0 , 0.5 ),
    bA ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 )
  ),
  data=data_nettle, chains=4 , cores=4 , iter=1000
)

# Inspect prior predictions
par(mfrow=c(1,1))
prior <- extract.prior( model.2b )
plot( NULL , xlim=c(floor(min_sd_grow),ceiling(max_sd_grow)) , ylim=c(floor(min_lang),ceiling(max_lang)) ,
      xlab="standardized standard deviation of growing season" , ylab="standardized log language per capita" )
abline( h=min_lang , lty=2 )
abline( h=max_lang , lty=2 )
growing_season_seq <- seq( from=floor(min_sd_grow) , to=ceiling(max_sd_grow) , length.out=50 )
mu <- link( model.2b , post=prior , data=data.frame( sd_growing_season_std=growing_season_seq, area_std=0 ))
for ( i in 1:50 ) lines( growing_season_seq , mu[i,] , col=col.alpha("black",0.3) )

precis( model.2b )
plot( coeftab( model.2b ) )

# Conclusion:
# The results seem support the hypothesis. As the bS coefficient is negative, we could consider that language diversity is negatively associated with the standard deviation of length of the growing season.

# (c)
model.2c <- map2stan(
  alist(
    log_lang_per_cap_std ~ dnorm( mu , sigma ),
    mu <- a + bM*mean_growing_season_std + bS*sd_growing_season_std + bMS*mean_growing_season_std*sd_growing_season_std + bA*area_std,
    a ~ dnorm( 0 , 0.5 ),
    bM ~ dnorm( 0 , 0.5 ),
    bS ~ dnorm( 0 , 0.5 ),
    bMS ~ dnorm( 0 , 0.5 ),
    bA ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 )
  ),
  data=data_nettle, chains=4 , cores=4 , iter=1000
)

# Inspect prior predictions
par(mfrow=c(2,3))
for ( s in -1:1 ) {
  idx <- which( data_nettle$sd_growing_season_std==s )
  plot( data_nettle$mean_growing_season_std[idx] , data_nettle$log_lang_per_cap_std[idx] , xlim=c(-1,1) , ylim=c(-2,2) ,
        xlab="standardized mean growing season" , ylab="standardized log language per capita" , pch=16 , col=rangi2 )
  mu <- link( model.2c , data=data.frame( sd_growing_season_std=s , mean_growing_season_std=-1:1 , area_std=0 ) )
  for ( i in 1:20 ) lines( -1:1 , mu[i,] , col=col.alpha("black",0.3) )
  if ( s == -1 ) {
    mtext("sd_growing_season_std = -1")
  } else if ( s == 0 ) {
    mtext("sd_growing_season_std = 0")
  } else if ( s == 1 ) {
    mtext("sd_growing_season_std = 1")
  }
}
for ( s in -1:1 ) {
  idx <- which( data_nettle$mean_growing_season_std==s )
  plot( data_nettle$sd_growing_season_std[idx] , data_nettle$log_lang_per_cap_std[idx] , xlim=c(-1,1) , ylim=c(-2,2) ,
        xlab="standardized standard deviation of growing season" , ylab="standardized log language per capita" , pch=16 , col=rangi2 )
  mu <- link( model.2c , data=data.frame( mean_growing_season_std=s , sd_growing_season_std=-1:1 , area_std=0 ) )
  for ( i in 1:20 ) lines( -1:1 , mu[i,] , col=col.alpha("black",0.3) )
  if ( s == -1 ) {
    mtext("mean_growing_season_std = -1")
  } else if ( s == 0 ) {
    mtext("mean_growing_season_std = 0")
  } else if ( s == 1 ) {
    mtext("mean_growing_season_std = 1")
  }
}

par(mfrow=c(1,1))
plot( coeftab( model.2c ) )
plot( coeftab( model.2a , model.2b , model.2c ) )

# Conclusion:
# The results seem to support the hypothesis that nations with longer average and higher variance of growing seasons might lead to greater social integration and fewer languages per capita.

#
model1.1 <- "
data {
int N;
int cid[N];
real rugged_cent[N];
real log_gdp_std[N];
}
parameters {
vector[2] a;
vector[2] bA;
real bR;
vector[2] bAR;
real<lower=0> sigma;
}
model {
vector[N] mu;
a ~ normal( 1 , 0.1 );
bA ~ normal( 0 , 0.3 );
bR ~ normal( 0 , 0.3 );
bAR ~ normal( 0 , 0.3 );
sigma ~ exponential( 1 );
for ( i in 1:N ) {
mu[i] = a[cid[i]] + bA[cid[i]] + bR*rugged_cent[i] + bAR[cid[i]]*rugged_cent[i];
}
log_gdp_std ~ normal( mu , sigma ); 
}"

fit1.1 <- stan( model_code=model1.1 , data=data_rugged.tomodel , 
                chains=4 , cores=4 , iter=1000 )

class(fit1.1)
print( fit1.1 )
plot( fit1.1 )
