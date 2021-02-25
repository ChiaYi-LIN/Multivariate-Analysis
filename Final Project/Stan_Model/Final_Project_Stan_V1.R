# Packages
library( "rstudioapi" )
library( "dplyr" )
library( "stringr" )
library( "rstan" )
library( "rethinking" )

# Working directory
current_path <- getActiveDocumentContext()$path
setwd( dirname( current_path ) )
getwd()

# Stan config
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

# Data
data <- read.csv( "./panel_rent.csv" , header=TRUE )
data_2 <- read.csv( "./panel_return.csv" , header=TRUE )
data$StationIndex <- as.integer(as.factor(data$Station))
data$HourIndex <- as.integer(as.factor(str_sub(data$ObsTime,12,13)))
data <- data[ , c(1:7, 63:64) ]
data$Return <- data_2$Return; rm( data_2 )
data$NetReturn <- data$Return - data$Rent
data[ , c(3:7,10:11) ] <- sapply( data[ , c(3:7,10:11) ], scale)
head(data)
model.data <- list(
  N = NROW( data ),
  R = data$NetReturn,
  T = data$Temperature,
  P = data$Precp,
  Min = data$MinDisMRT,
  Num = data$NumMRT,
  S = data$StationIndex,
  H = data$HourIndex
)
str( model.data )

# Model 1
model.1 = "
data{
  int<lower=1> N;
  real R[N];
  real T[N];
  real P[N];
  real Min[N];
  real Num[N];
  int S[N];
  int H[N];
}
parameters{
  vector[31] a_S;
  vector[24] a_H;
  vector[24] b_TH;
  vector[24] b_PH;
  real<lower=0> sigma;
}
model{
  vector[N] mu;
  sigma ~ exponential(1);
  for ( i in 1:31 ) {
    a_S[i] ~ normal( 0 , 1 );
  }

  for ( i in 1:24 ) {
    a_H[i] ~ normal( 0 , 1 );
    b_TH[i] ~ normal( 0 , 1 );
    b_PH[i] ~ normal( 0 , 1 );
  }

  for ( i in 1:N ) {
    mu[i] =  
    a_S[S[i]] +
    a_H[H[i]] + b_TH[H[i]] * T[i] + b_PH[H[i]] * P[i];
  }

  R ~ normal( mu , sigma );
}
"
fit.1 <- stan( model_code=model.1 , data=model.data , 
               chains=3 , cores=3 , iter=3000 )

# traceplot( fit.1 , pars=c("a_S[1]","a_H[1]","b_TH[1]","b_PH[1]") )
# pairs( fit.1 , pars=c("a_S[1]","a_H[1]","b_TH[1]","b_PH[1]") )
print( fit.1 , pars=c("a_S[1]","a_H[1]","b_TH[1]","b_PH[1]") , probs=c(.1, .5, .9) )
# fit.1@stanmodel@dso <- new("cxxdso")
# saveRDS(fit.1, file = "fit_1.rds")

# Model 2
model.2 = "
data{
  int<lower=1> N;
  real R[N];
  real T[N];
  real P[N];
  real Min[N];
  real Num[N];
  int S[N];
  int H[N];
}
parameters{
  real a;
  real b_M;
  real b_N;
  vector[24] a_H;
  vector[24] b_TH;
  vector[24] b_PH;
  real<lower=0> sigma;
}
model{
  vector[N] mu;
  sigma ~ exponential(1);
  a ~ normal( 0 , 1 );
  b_M ~ normal( 0 , 1 );
  b_N ~ normal( 0 , 1 );

  for ( i in 1:24 ) {
    a_H[i] ~ normal( 0 , 1 );
    b_TH[i] ~ normal( 0 , 1 );
    b_PH[i] ~ normal( 0 , 1 );
  }

  for ( i in 1:N ) {
    mu[i] =  
    a + b_M * Min[i] + b_N * Num[i] +
    a_H[H[i]] + b_TH[H[i]] * T[i] + b_PH[H[i]] * P[i];
  }

  R ~ normal( mu , sigma );
}
"
fit.2 <- stan( model_code=model.2 , data=model.data , 
               chains=3 , cores=3 , iter=3000 )

# traceplot( fit.2 , pars=c("a","b_M","b_N","a_H[1]","b_TH[1]","b_PH[1]") )
# pairs( fit.2 , pars=c("a","b_M","b_N") )
print( fit.2 , pars=c("a","b_M","b_N","a_H[1]","b_TH[1]","b_PH[1]") , probs=c(.1, .5, .9) )
# fit.2@stanmodel@dso <- new("cxxdso")
# saveRDS(fit.2, file = "fit_2.rds")