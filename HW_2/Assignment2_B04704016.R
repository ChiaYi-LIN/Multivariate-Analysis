library(rethinking)
rm(list=ls())
##Question 1
#Create answer sheet
answer.sheet <- data.frame(
  list(
  "Individual" = c( 1 , 2 , 3 , 4 , 5 ),
  "Weight" = c( 46.95 , 43.72 , 64.78 , 32.59 , 54.63 ),
  "Expected height" = rep( "NA" , 5 ), 
  "89% interval" = rep( "NA" , 5 )
  ) , check.names=FALSE
)
answer.sheet

#Load data
data(Howell1)
Howell1.data <- Howell1
d <- Howell1

#Inspect data
str(d)
precis(d)

#Use cubic regression to fit the data
#Standardize predictors
d$weight_s <- ( d$weight - mean(d$weight) )/sd( d$weight )
d$weight_s2 <- d$weight_s^2
d$weight_s3 <- d$weight_s^3

#Create model using quadratic approximation
model.fit <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight_s + b2*weight_s2 + b3*weight_s3 ,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm( 0 , 10 ) ,
    b3 ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d )

#Inspect the model
#Create a sequence of standardized weights
weight.seq <- seq( from=-2.2 , to=2 , length.out=30 )

#Create a list of predictors
pred_dat <- list( weight_s=weight.seq , weight_s2=weight.seq^2 , weight_s3=weight.seq^3 )

#Sampling from posterior distribution, and compute mu for each pred_dat
mu <- link( model.fit , data=pred_dat )

#Calculate the mean of mu and 89% interval of mean
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )

#Simulate 10000 predicted heights for each pred_dat
sim.height <- sim( model.fit , data=pred_dat, n=1e4 )

#Calculate the 89% interval of predicted heights
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

#Plot the results of fitted models with raw data
plot( height ~ weight_s , d , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq )
#The model fits quite well to the observated data, so the next step would be to predict heights from given weight data.

#Standardize the weight data in answer.sheet
weight_predict_s <- ( answer.sheet$Weight - mean(d$weight) )/sd( d$weight )

#Create the predictors of the cube regression
weight_predictors <- list( weight_s=weight_predict_s , weight_s2=weight_predict_s^2 , weight_s3=weight_predict_s^3 )

#Sampling from posterior distribution, and compute mu for each weight_predictors
mu.predict <- link( model.fit , data=weight_predictors )

#Calculate the mean of predicted heights and 89% interval of mean
mu.predict.mean <- apply( mu.predict , 2 , mean )
mu.predict.PI <- apply( mu.predict , 2 , PI , prob=0.89 )

#Simulate predicted heights for each weight_predictors
sim.height.predict <- sim( model.fit , data=weight_predictors )

#Calculate the 89% interval of predicted heights
height.predict.PI <- apply( sim.height.predict , 2 , PI , prob=0.89 )

#Add mean heights into answer.sheet
height.mean <- round( mu.predict.mean , 3 )
answer.sheet$`Expected height` <- height.mean

#Add 89% intervals into answer.sheet
height.HPDI <- round( height.predict.PI , 3 )
answer.sheet$`89% interval` <- sapply( 1:5 , function(i)
  paste( "( " , height.HPDI[ 1 , i ] , " , " , height.HPDI[ 2 , i ] , " )" )
)

#Answer
answer.sheet

##Question 2
#2-a
#Filter - children
d2 <- Howell1.data[ d$age < 18 , ]

#Inspect d2
precis(d2)

#Create data list for stan model
data_list <- list(
  children_height = d2$height,
  children_weight = d2$weight,
  children_age = d2$age,
  children_male = d2$male
) 

#Inspect data list
str(data_list)

#Compute mean weight of data in d2
children_mean_weight <- mean( d2$weight )
children_mean_weight
#The mean weight of all of the children is 18.41419 kg.

model2 <- ulam(
  alist(
    children_height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( children_weight - 18.41419 ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=data_list , chains=4 , cores=4 , iter=1000 )

precis( model2 )
#Since the mean slope of the regression model is 2.72, we could expect that for every 10 units of increase in weight, the model would predict the child to be 27.2 units taller.

pairs( model2 )

#2-b
#Create a sequence as each weight
children.weight.seq <- seq( from=0 , to=50 , by=1 )

#Create a list to be applies to link function with posterior model 
children.to.predict <- list( children_weight=children.weight.seq )

#Sample from posterior to calculate mu
children.mu.predict <- link( model2 , data=children.to.predict )

#Calculate the mean and 89% interval of mu
children.mu.predict.mean <- apply( children.mu.predict , 2 , mean )
children.mu.predict.PI <- apply( children.mu.predict , 2 , PI , prob=0.89 )

#Simulate predicted heights regarding each weight
children.height.predict <- sim( model2 , data=children.to.predict )

#Calculate the 89% interval of predicted heights
children.height.predict.PI <- apply( children.height.predict , 2 , PI , prob=0.89 )

#Plot the results of fitted models with raw data
plot( height ~ weight , d2 , col=col.alpha(rangi2,0.5) )
lines( children.weight.seq , children.mu.predict.mean )
shade( children.mu.predict.PI , children.weight.seq )
shade( children.height.predict.PI , children.weight.seq )

#2-c
#From the graph above, we could find that although the 89% interval of predicted heights contained most of the raw data, the linear regression model itself did not fit the data well. It is possibly better to use polynomial regression model in this case, since the raw data seemed to be like a curve.

#Question 3
#3-a
#Create a data list for the model
data_list_3 <- list(
  data_height = Howell1.data$height,
  data_weight = Howell1.data$weight,
  data_age = Howell1.data$age,
  data_male = Howell1.data$male
)

#Inspect the data list
str(data_list_3)

#Apply stan model
model3 <- ulam(
  alist(
    data_height ~ dnorm( mu , sigma ) ,
    mu <- a + b*log( data_weight ) ,
    a ~ dnorm( 178 , 100 ) ,
    b ~ dnorm( 0 , 100 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=data_list_3 , chains=4 , cores=4 , iter=1000 )

#Inspect results of the model
precis( model3 )
round( vcov( model3 ) , 3 )
#show( model3 )
pairs( model3 )

#Interpret:
#Since we are using a logarithm regression model to predict mu, it is quite diffucult to interpret the results. Because the logarithm is a non-linear regression model, the mean and standard deviation of a and b did not tell us a lot about the relationship between weights and heights.   

#3-b
#Beginning plot
plot( height ~ weight , data=Howell1 , col=col.alpha(rangi2, 0.4) )

#Create sequence of weights and a list of them
weight.seq3 <- seq( from=0 , to=70 , by=1 )
weight.list <- list( data_weight=weight.seq3 )

#Apply link function to generate values of mu
predict.mu <- link( model3 , data=weight.list )

#Calculate the mean and 97% interval of mu
predict.mu.mean <- apply( predict.mu , 2 , mean )
predict.mu.PI <- apply ( predict.mu , 2 , HPDI , prob=0.97 )

#Apply sim function to predict heights of weight.list
predict.height <- sim( model3 , data=weight.list )

#Calculate the 97% interval of predicted heights
predict.height.PI <- apply( predict.height , 2 , HPDI , prob=0.97 )

#Add the lines and shades to the plot
lines( weight.seq3 , predict.mu.mean )
shade( predict.mu.PI , weight.seq3 )
shade( predict.height.PI , weight.seq3 )

#####################

#plot( height ~ weight , data=d2 , col=rangi2 )
post <- extract.samples( m4.3 )
mu_at_50 <- post$a + post$b * ( 50 - xbar )
HPDI( mu_at_50 , prob=0.89 )
#a_map <- mean(post$a)
#b_map <- mean(post$b)
curve( a_map + b_map*(x - xbar) , add=TRUE )
#
#str(d)
#precis(d)
#d$height
#d[[1]]
d2 <- d[ d$age >= 18 , ]
#dens(d2$height)
#curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )
#curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )
xbar <- mean(d2$weight)
flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm( 178 , 20 ) ,
  sigma ~ dunif( 0 , 50 )
)
m4.1 <- quap( flist , data=d2 )
precis(m4.1)

m4.2 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu ~ dnorm( 178 , 0.1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )
precis( m4.2 )
vcov( m4.1 )
diag( vcov( m4.1 ) )
cov2cor( vcov( m4.1 ) )
post <- extract.samples( m4.1 , n=1e4 )
head(post)













sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
#dens( prior_h )

mu.list <- seq( from=140, to=160 , length.out=200 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum( dnorm(
  d2$height ,
  mean=post$mu[i] ,
  sd=post$sigma[i] ,
  log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )
#contour_xyz( post$mu , post$sigma , post$prob )
#image_xyz( post$mu , post$sigma , post$prob )

sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]
plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )
dens( sample.mu )
dens( sample.sigma )
HPDI( sample.mu )
HPDI( sample.sigma )



pos <- replicate( 1000 , sum( runif(16,-1,1) ) )
hist(pos)
plot(density(pos))
log.big <- replicate( 10000 , log(prod( 1 + runif(12,0,0.5) ) ) )
dens( log.big , norm.comp=TRUE )
dens( small , norm.comp=TRUE )


#################
#Old version of Q1
#Filter - adults
d2 <- d[ d$age >= 18 , ]
precis(d2)

#Average weight
xbar <- mean(d2$weight)

#Quadratic approximation
model <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - xbar ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d2 )

#Simulation and prediction
sim.height <- sim( model , list( weight=answer.sheet$Weight ))

#Calculate mean height
height.mean <- round( apply( sim.height , 2 , mean ) , 3 )
answer.sheet$`Expected height` <- height.mean

#Calculate 89% interval
height.HPDI <- round( apply( sim.height , 2 , HPDI , prob=0.89) , 3 )
answer.sheet$`89% interval` <- sapply( 1:5 , function(i)
  paste( "( " , height.HPDI[ 1 , i ] , " , " , height.HPDI[ 2 , i ] , " )" )
)

#Answer
answer.sheet


#post <- extract.samples( model2 )
#a_map <- mean(post$a)
#b_map <- mean(post$b)
#curve( a_map + b_map*(x - children_mean_weight) , add=TRUE )

weight.seq <- seq( from=0 , to=ceiling( max( d3$weight ) ) , by=1 )
mu <- link( model2 , data=data.frame(children_weight=weight.seq) )
str(mu)

#for ( i in 1:100 ) points( weight.seq , mu[i,] , pch=16 , col=col.alpha(rangi2,0.1) )
#Calculate stan predicted regression line and 89% HPDI for the mean.
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )
#plot( height ~ weight , data=d3 , col=col.alpha(rangi2,0.5) )
#Plot the calculated values
lines( weight.seq , mu.mean )
shade( mu.HPDI , weight.seq )

#Calculate 89% HPDI for predicted heights.
sim.height <- sim( model2 , data=list(children_weight=weight.seq) )
str(sim.height)
height.HPDI <- apply( sim.height , 2 , HPDI , prob=0.89 )
#Plot the calculated values
shade( height.HPDI , weight.seq )

######################
