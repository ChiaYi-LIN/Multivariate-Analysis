#Question 1
library(tidyverse)
library(tidybayes)
library(rethinking)
birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,
            0,0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,
            1,0,0,0,0,0,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,
            0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,
            0,1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,
            1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,
            0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,0,0,0,1,1,1,0,0,0,0)
p_grid <- seq( from = 0, to = 1, length.out = 1000 )
prior <- rep( 1, 1000 )
prob_data <- dbinom(( sum( birth1 ) + sum( birth2 )), size = 200, prob = p_grid )
posterior <- prob_data * prior
posterior <- posterior / sum(posterior)
plot( posterior ~ p_grid )
answer_1 <- round( p_grid[ which.max( posterior ) ], 3)
cat("The parameter value which maximizes the posterior probability: p =", answer_1)

#Question 2
set.seed(100)
samples <- sample( p_grid, prob = posterior, size = 10000, replace = TRUE)
answer_2 <- mode_hdi( samples, .width = c( 0.5, 0.89, 0.97 ))
answer_2

#Question 3
predict_boys_1 <- rbinom( 1e4, size = 200, prob = samples )
plot( predict_boys_1 )
dens( predict_boys_1 )
abline( v = sum( birth1 ) + sum( birth2 ) , col = "red", lwd = 3, lty = 2 )

#Question 4
predict_boys_2 <- rbinom( 1e4, size = 100, prob = samples )
plot( predict_boys_2 )
dens( predict_boys_2 )
abline( v = sum( birth1 ) , col = "red", lwd = 3, lty = 2 )

#Question 5
female_first_borns <- c( which( birth1 == 0 ))
cat("The following families have a girl as the first child:", female_first_borns)
cat("Number of families:", length(female_first_borns) )
second_birth_following_female <- birth2[ female_first_borns ]
cat("The second child of these families have the following genders:", second_birth_following_female)
second_male_simulation <- rbinom(n = 1e4, size = length(second_birth_following_female), prob = samples)
dens(second_male_simulation)
abline(v = sum(second_birth_following_female), col = "red", lwd = 3, lty = 2 )
