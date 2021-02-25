# Packages
rm(list=ls())
suppressMessages( library( "ggplot2" ) )
suppressMessages( library( "ggmap") )
suppressMessages( library( "plotly") )
suppressMessages( library( "rgl") )
suppressMessages( library( "rstudioapi" ) )
suppressMessages( library( "plyr" ) )
suppressMessages( library( "dplyr" ) )
suppressMessages( library( "tidyr" ) )
suppressMessages( library( "gridExtra" ) )
suppressMessages( library( "stringr" ) )
suppressMessages( library( "loo" ) )
suppressMessages( library( "rstan" ) )
suppressMessages( library( "rethinking" ) )

# Working directory
current_path <- getActiveDocumentContext()$path
setwd( dirname( current_path ) )
getwd()

countD_rent = plyr::count(dat$rent_sta_sarea)
countD_return = plyr::count(dat$return_sta_sarea)
par(mfrow = c(1, 2))
hist(dat$rent_sta_sarea, col = "cyan",breaks = 30, xlab = "District", main = "Frequency of rent")
hist(dat$return_sta_sarea, col = "cyan", xlab = "District", main = "Frequency of return")
dat = read.table("./dataset/201601.txt", header = T)
dat12rent = dat[(dat$rent_sta_sarea == "12"), ]
dat12return = dat[(dat$return_sta_sarea == "12"), ]
dat12rent$rent_sta = as.character(dat12rent$rent_sta)
dat12return$rent_sta = as.character(dat12return$rent_sta)
count_12_rent = plyr::count(dat12rent$rent_sta)
count_12_return = plyr::count(dat12return$return_sta)
rm(dat)
#rent
count12_rent_hr = list()
for(i in 1:nrow(count_12_rent)){
  dat12_hr = dat12rent[dat12rent$rent_sta == as.character(count_12_rent[i, 1]), ]
  dat12_hr$rent_time = str_sub(dat12_hr$rent_time, 1, 13)
  dat12_hr = dat12_hr[(str_sub(dat12_hr$rent_time, 1, 4) !="2014"),]
  dat12_hr = dat12_hr[(str_sub(dat12_hr$rent_time, 1, 4) !="2015"),]
  count_temp = plyr::count(dat12_hr$rent_time)
  count12_rent_hr[[i]] = as.data.frame(count_temp)
}
rent_12_hr = matrix(0, 31, 744)
A1 = paste("2016/01/01", sep = " ", c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A2 = paste("2016/01/02", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A3 = paste("2016/01/03", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A4 = paste("2016/01/04", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A5 = paste("2016/01/05", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A6 = paste("2016/01/06", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A7 = paste("2016/01/07", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A8 = paste("2016/01/08", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A9 = paste("2016/01/09", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A10 = paste("2016/01/10", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A11 = paste("2016/01/11", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A12 = paste("2016/01/12", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A13 = paste("2016/01/13", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A14 = paste("2016/01/14", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A15 = paste("2016/01/15", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A16 = paste("2016/01/16", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A17 = paste("2016/01/17", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A18 = paste("2016/01/18", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A19 = paste("2016/01/19", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A20 = paste("2016/01/20", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A21 = paste("2016/01/21", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A22 = paste("2016/01/22", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A23 = paste("2016/01/23", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A24 = paste("2016/01/24", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A25 = paste("2016/01/25", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A26 = paste("2016/01/26", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A27 = paste("2016/01/27", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A28 = paste("2016/01/28", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A29 = paste("2016/01/29", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A30 = paste("2016/01/30", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A31 = paste("2016/01/31", sep = " ", c("00","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
A = c(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31) 
colnames(rent_12_hr) = A
rownames(rent_12_hr) = as.character(count_12_rent[, 1])  
for(i in 1:31){
  for(n in 1:nrow(count12_rent_hr[[1]])){
    rent_12_hr[i, which(colnames(rent_12_hr) == count12_rent_hr[[i]][n, 1])] = count12_rent_hr[[i]][n, 2]
  }
}
#return
count12_return_hr = list()
for(i in 1:31){
  dat12_hr = dat12return[dat12return$return_sta == as.character(count_12_return[i, 1]), ]
  dat12_hr$return_time = str_sub(dat12_hr$return_time, 1, 13)
  dat12_hr = dat12_hr[(str_sub(dat12_hr$return_time, 1, 4) !="2014"),]
  dat12_hr = dat12_hr[(str_sub(dat12_hr$return_time, 1, 4) !="2015"),]
  count_temp = plyr::count(dat12_hr$return_time)
  count12_return_hr[[i]] = as.data.frame(count_temp)
}
return_12_hr = matrix(0, 31, 744)
colnames(return_12_hr) = A
rownames(return_12_hr) = as.character(count_12_rent[, 1])  
for(i in 1:31){
  for(n in 1:nrow(count12_return_hr[[1]])){
    return_12_hr[i, which(colnames(return_12_hr) == count12_return_hr[[i]][n, 1])] = count12_return_hr[[i]][n, 2]
  }
}
#panel data
#weather
weather = read.csv("weather-2016-01.csv", header = T)
weather_t_p = data.frame(weather$Temperature, weather$Precp)
weather_t_p$weather.Precp = as.character(weather_t_p$weather.Precp)
weather_t_p$weather.Precp[which(weather_t_p$weather.Precp == "T")] = "0"
weather_t_p$weather.Precp = as.numeric(weather_t_p$weather.Precp)
weathr_2016 = weather_t_p
for(i in 1:30){
  weathr_2016 = rbind(weathr_2016, weather_t_p)
}
#MRT
mrt = read.csv("distance_between_youbike_and_metro_station.csv", header = T)
mrt_min = min(mrt$Distance_Meters[which(mrt$Youbike_Station == rownames(rent_12_hr)[1])])
for(i in 2:nrow(rent_12_hr)){
  mrt_min = c(mrt_min, min(mrt$Distance_Meters[which(mrt$Youbike_Station == rownames(rent_12_hr)[i])]))
}
mrt_num = sum(as.numeric(mrt$Distance_Meters[which(mrt$Youbike_Station == rownames(rent_12_hr)[1])] <= 1000))
for(i in 2:nrow(rent_12_hr)){
  mrt_num = c(mrt_num, sum(as.numeric(mrt$Distance_Meters[which(mrt$Youbike_Station == rownames(rent_12_hr)[i])] <= 1000)))
}
#panel rent
panel_rent = matrix(rent_12_hr[1,], ncol(rent_12_hr), 1)
for(i in 2:nrow(rent_12_hr)){
  panel_rent = rbind(panel_rent,as.matrix(rent_12_hr[i,]))
}
panel_rent = as.data.frame(panel_rent)
panel_rent = cbind(rep(rownames(rent_12_hr), each = ncol(rent_12_hr)), rep(colnames(rent_12_hr), 31), panel_rent)
panel_rent = cbind(panel_rent, weathr_2016, rep(mrt_min, each = ncol(rent_12_hr)), rep(mrt_num, each = ncol(rent_12_hr)))
colnames(panel_rent) = c("Station", "ObsTime", "Rent", "Temperature", "Precp", "MinDisMRT", "NumMRT")
#panel return
panel_return = matrix(return_12_hr[1,], ncol(return_12_hr), 1)
for(i in 2:nrow(rent_12_hr)){
  panel_return = rbind(panel_return,as.matrix(return_12_hr[i,]))
}
panel_return = as.data.frame(panel_return)
panel_return = cbind(rep(rownames(return_12_hr), each = ncol(return_12_hr)), rep(colnames(return_12_hr), 31), panel_return)
panel_return = cbind(panel_return, weathr_2016, rep(mrt_min, each = ncol(rent_12_hr)), rep(mrt_num, each = ncol(rent_12_hr)))
colnames(panel_return) = c("Station", "ObsTime", "Return", "Temperature", "Precp", "MinDisMRT", "NumMRT")
write.csv(panel_rent, "panel_rent_new.csv")
write.csv(panel_return, "panel_return_new.csv")
#Net return
panel_net = cbind(panel_rent, panel_return$Return, (panel_return$Return-panel_rent$Rent))
colnames(panel_net)[c(9, 10)] = c("Return", "Net Return")
#plot
sta = as.numeric(as.factor(panel_rent$Station))
rgl::plot3d(panel_net$Temperature, sta, panel_net$`Net Return`)
tim = as.numeric(as.factor(str_sub(panel_net$ObsTime, 12, 14))) 
p = plot_ly(panel_net, x = ~Temperature, y = sta, z = ~`Net Return`, color = sta, size = 1) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Temperature'),
                      yaxis = list(title = 'Station'),
                      zaxis = list(title = 'Net Return')))
p

# Stan config
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

# Data
data <- read.csv( "./panel_rent_new.csv" , header=TRUE )
data_2 <- read.csv( "./panel_return_new.csv" , header=TRUE )
data$StationIndex <- as.integer(as.factor(data$Station))
data$HourIndex <- as.integer(as.factor(str_sub(data$ObsTime,12,13)))
data$Return <- data_2$Return; rm( data_2 )
data$NetReturn <- data$Return - data$Rent
head(data)
scaled_data <- data
scaled_data[ , c(4:8,11:12) ] <- sapply( scaled_data[ , c(4:8,11:12) ], scale)
head(scaled_data)
model.data <- list(
  N = NROW( scaled_data ),
  Rent = scaled_data$Rent,
  Return = scaled_data$Return,
  NetReturn = scaled_data$NetReturn,
  T = scaled_data$Temperature,
  P = scaled_data$Precp,
  Min = scaled_data$MinDisMRT,
  Num = scaled_data$NumMRT,
  S = data$StationIndex,
  H = data$HourIndex
)
str( model.data )

# Model 1
model.1 = "
data{
  int<lower=1> N;
  real NetReturn[N];
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
  real sigma;
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
    a_S[S[i]] + a_H[H[i]] + b_TH[H[i]] * T[i] + b_PH[H[i]] * P[i];
  }

  NetReturn ~ normal( mu , sigma );
}
generated quantities{
  vector[2000] log_lik;
  vector[2000] mu;
  for ( i in 1:2000 ) {
    mu[i] = a_S[S[i]] + a_H[H[i]] + b_TH[H[i]] * T[i] + b_PH[H[i]] * P[i];
    log_lik[i] = normal_lpdf( NetReturn[i] | mu[i] , sigma );
  }
}
"
fit.1 <- readRDS(file = "fit_1_ll.rds")
# fit.1 <- stan( model_code=model.1 , data=model.data , 
#                chains=3 , cores=3 , iter=3000 )

# traceplot( fit.1 , pars=c("a_S[1]","a_H[1]","b_TH[1]","b_PH[1]") )
# pairs( fit.1 , pars=c("a_S[1]","a_H[1]","b_TH[1]","b_PH[1]") )
# print( fit.1 , pars=c("a_S[1]","a_H[1]","b_TH[16]","b_PH[16]") , probs=c(.1, .5, .9) )
print(fit.1 , pars=c("b_TH") )
# fit.1@stanmodel@dso <- new("cxxdso")
# saveRDS(fit.1, file = "fit_1_ll.rds")

# Draw posterior
post.1 <- as.data.frame( fit.1 )
unscale_NR <- function(x) x * sd(data$NetReturn) + mean(data$NetReturn)
pred_func <- function(station, hour, tempa, precp){
  post.1[,station] + post.1[,31+hour] + post.1[,31+24+hour] * tempa + post.1[,31+24+24+hour] * precp
}
station <- seq( from=1 , to=31 )
each_hour_prediction <- 0
for ( i in 1:24 ){
  hour <- i
  prediction_by_hour <- 
    sapply(station, pred_func, hour=hour, tempa=0, precp=0) %>%
    as_tibble() %>%
    rename_all(function(x) station) %>%
    mutate(Iter = row_number()) %>%
    gather(Station, NetReturn, -Iter) %>%
    group_by(Station) %>%
    mutate(hpdi_l = quantile(NetReturn, probs = 0.055),
           hpdi_h = quantile(NetReturn, probs = 0.945)) %>%
    mutate(mu = mean(NetReturn)) %>%
    mutate(Hour = hour) %>%
    ungroup() %>%
    mutate(Station = as.numeric(Station)) %>%
    select (-c(Iter,NetReturn))
  prediction_by_hour <- prediction_by_hour[match(unique(prediction_by_hour$Station), prediction_by_hour$Station),]
  prediction_by_hour[ , 2:4 ] <- sapply( prediction_by_hour[ , 2:4 ] , unscale_NR )
  if (!is.data.frame(each_hour_prediction)){
    each_hour_prediction <- prediction_by_hour
  } else {
    each_hour_prediction <- rbind( each_hour_prediction , prediction_by_hour ) 
  }
}
each_hour_prediction$StationName <- unique(data$Station)
# write.csv( each_hour_prediction , file="./each_station_NetReturn_prediction_by_hour.csv", row.names=FALSE)

# Plot status of each station at hour=8, with temperature and precp at mean
plot_data <- each_hour_prediction[ (each_hour_prediction$Hour==8) , ]
p_hr_8 <- ggplot() +
  geom_point(data = plot_data,
             aes(Station, mu), 
             shape = 1, color = 'dodgerblue') +
  scale_x_discrete(limits = plot_data$Station) + 
  geom_segment(data = plot_data,
               aes(x = Station, y = hpdi_l, xend = Station, yend = hpdi_h), alpha = .5) +
  geom_hline(yintercept = 0 , color = "red") +
  ggtitle("Prediction of NetReturn for each station (Hour=8, temperatur & precipitaion at mean)")

# Plot status of each station at hour=10, with temperature and precp at mean
plot_data <- each_hour_prediction[ (each_hour_prediction$Hour==10) , ]
p_hr_10 <- ggplot() +
  geom_point(data = plot_data,
             aes(Station, mu), 
             shape = 1, color = 'dodgerblue') +
  scale_x_discrete(limits = plot_data$Station) + 
  geom_segment(data = plot_data,
               aes(x = Station, y = hpdi_l, xend = Station, yend = hpdi_h), alpha = .5) +
  geom_hline(yintercept = 0 , color = "red") +
  ggtitle("Prediction of NetReturn for each station (Hour=10, temperatur & precipitaion at mean)")

# Plot status of each station at hour=12, with temperature and precp at mean
plot_data <- each_hour_prediction[ (each_hour_prediction$Hour==12) , ]
p_hr_12 <- ggplot() +
  geom_point(data = plot_data,
             aes(Station, mu), 
             shape = 1, color = 'dodgerblue') +
  scale_x_discrete(limits = plot_data$Station) + 
  geom_segment(data = plot_data,
               aes(x = Station, y = hpdi_l, xend = Station, yend = hpdi_h), alpha = .5) +
  geom_hline(yintercept = 0 , color = "red") +
  ggtitle("Prediction of NetReturn for each station (Hour=12, temperatur & precipitaion at mean)")

grid.arrange(p_hr_8, p_hr_10, p_hr_12, nrow = 3)

# Plot status of each station at hour=17, with temperature and precp at mean
plot_data <- each_hour_prediction[ (each_hour_prediction$Hour==17) , ]
p_hr_17 <- ggplot() +
  geom_point(data = plot_data,
             aes(Station, mu), 
             shape = 1, color = 'dodgerblue') +
  scale_x_discrete(limits = plot_data$Station) + 
  geom_segment(data = plot_data,
               aes(x = Station, y = hpdi_l, xend = Station, yend = hpdi_h), alpha = .5) +
  geom_hline(yintercept = 0 , color = "red") +
  ggtitle("Prediction of NetReturn for each station (Hour=17, temperatur & precipitaion at mean)")

# Plot status of each station at hour=19, with temperature and precp at mean
plot_data <- each_hour_prediction[ (each_hour_prediction$Hour==19) , ]
p_hr_19 <- ggplot() +
  geom_point(data = plot_data,
             aes(Station, mu), 
             shape = 1, color = 'dodgerblue') +
  scale_x_discrete(limits = plot_data$Station) + 
  geom_segment(data = plot_data,
               aes(x = Station, y = hpdi_l, xend = Station, yend = hpdi_h), alpha = .5) +
  geom_hline(yintercept = 0 , color = "red") +
  ggtitle("Prediction of NetReturn for each station (Hour=19, temperatur & precipitaion at mean)")

# Plot status of each station at hour=21, with temperature and precp at mean
plot_data <- each_hour_prediction[ (each_hour_prediction$Hour==21) , ]
p_hr_21 <- ggplot() +
  geom_point(data = plot_data,
             aes(Station, mu), 
             shape = 1, color = 'dodgerblue') +
  scale_x_discrete(limits = plot_data$Station) + 
  geom_segment(data = plot_data,
               aes(x = Station, y = hpdi_l, xend = Station, yend = hpdi_h), alpha = .5) +
  geom_hline(yintercept = 0 , color = "red") +
  ggtitle("Prediction of NetReturn for each station (Hour=21, temperatur & precipitaion at mean)")

grid.arrange(p_hr_17, p_hr_19, p_hr_21, nrow = 3)

# Temperature = 2
post.1 <- as.data.frame( fit.1 )
station <- seq( from=1 , to=31 )
each_hour_prediction <- 0
for ( i in 1:24 ){
  hour <- i
  prediction_by_hour <- 
    sapply(station, pred_func, hour=hour, tempa=2, precp=0) %>%
    as_tibble() %>%
    rename_all(function(x) station) %>%
    mutate(Iter = row_number()) %>%
    gather(Station, NetReturn, -Iter) %>%
    group_by(Station) %>%
    mutate(hpdi_l = quantile(NetReturn, probs = 0.055),
           hpdi_h = quantile(NetReturn, probs = 0.945)) %>%
    mutate(mu = mean(NetReturn)) %>%
    mutate(Hour = hour) %>%
    ungroup() %>%
    mutate(Station = as.numeric(Station)) %>%
    select (-c(Iter,NetReturn))
  prediction_by_hour <- prediction_by_hour[match(unique(prediction_by_hour$Station), prediction_by_hour$Station),]
  prediction_by_hour[ , 2:4 ] <- sapply( prediction_by_hour[ , 2:4 ] , unscale_NR )
  if (!is.data.frame(each_hour_prediction)){
    each_hour_prediction <- prediction_by_hour
  } else {
    each_hour_prediction <- rbind( each_hour_prediction , prediction_by_hour ) 
  }
  
}
each_hour_prediction$StationName <- unique(data$Station)

# Plot status of each station at hour=8, with temperature = 2 and precp at mean
plot_data <- each_hour_prediction[ (each_hour_prediction$Hour==8) , ]
p_hr_8_t_2 <- ggplot() +
  geom_point(data = plot_data,
             aes(Station, mu), 
             shape = 1, color = 'dodgerblue') +
  scale_x_discrete(limits = plot_data$Station) + 
  geom_segment(data = plot_data,
               aes(x = Station, y = hpdi_l, xend = Station, yend = hpdi_h), alpha = .5) +
  geom_hline(yintercept = 0 , color = "red") +
  ggtitle("Prediction of NetReturn for each station (Hour=8, temperature=2, precipitaion at mean)")

grid.arrange(p_hr_8, p_hr_8_t_2, nrow = 2)

# Precp = 2
post.1 <- as.data.frame( fit.1 )
station <- seq( from=1 , to=31 )
each_hour_prediction <- 0
for ( i in 1:24 ){
  hour <- i
  prediction_by_hour <- 
    sapply(station, pred_func, hour=hour, tempa=0, precp=2) %>%
    as_tibble() %>%
    rename_all(function(x) station) %>%
    mutate(Iter = row_number()) %>%
    gather(Station, NetReturn, -Iter) %>%
    group_by(Station) %>%
    mutate(hpdi_l = quantile(NetReturn, probs = 0.055),
           hpdi_h = quantile(NetReturn, probs = 0.945)) %>%
    mutate(mu = mean(NetReturn)) %>%
    mutate(Hour = hour) %>%
    ungroup() %>%
    mutate(Station = as.numeric(Station)) %>%
    select (-c(Iter,NetReturn))
  prediction_by_hour <- prediction_by_hour[match(unique(prediction_by_hour$Station), prediction_by_hour$Station),]
  prediction_by_hour[ , 2:4 ] <- sapply( prediction_by_hour[ , 2:4 ] , unscale_NR )
  if (!is.data.frame(each_hour_prediction)){
    each_hour_prediction <- prediction_by_hour
  } else {
    each_hour_prediction <- rbind( each_hour_prediction , prediction_by_hour ) 
  }
  
}
each_hour_prediction$StationName <- unique(data$Station)

# Plot status of each station at hour=8, with temperature at mean and precp = 2
plot_data <- each_hour_prediction[ (each_hour_prediction$Hour==8) , ]
p_hr_8_p_2 <- ggplot() +
  geom_point(data = plot_data,
             aes(Station, mu), 
             shape = 1, color = 'dodgerblue') +
  scale_x_discrete(limits = plot_data$Station) + 
  geom_segment(data = plot_data,
               aes(x = Station, y = hpdi_l, xend = Station, yend = hpdi_h), alpha = .5) +
  geom_hline(yintercept = 0 , color = "red") +
  ggtitle("Prediction of NetReturn for each station (Hour=8, precipitaion=2, temperatur at mean)")

grid.arrange(p_hr_8, p_hr_8_p_2, nrow = 2)

# Temperature from -4 to 4
post.1 <- as.data.frame( fit.1 )
unscale_Temp <- function(x) x * sd(data$Temperature) + mean(data$Temperature)
pred_func <- function(tempa, hour, station, precp){
  post.1[,station] + post.1[,31+hour] + post.1[,31+24+hour] * tempa + post.1[,31+24+24+hour] * precp
}
temp_seq <- seq( from=-4 , to=4 , length.out=30 )
prediction_by_hour <- 
  sapply(temp_seq, pred_func, hour=8, station=1, precp=0) %>%
  as_tibble() %>%
  rename_all(function(x) temp_seq) %>%
  mutate(Iter = row_number()) %>%
  gather(Temperature, NetReturn, -Iter) %>%
  group_by(Temperature) %>%
  mutate(hpdi_l = quantile(NetReturn, probs = 0.055),
         hpdi_h = quantile(NetReturn, probs = 0.945)) %>%
  mutate(mu = mean(NetReturn)) %>%
  ungroup() %>%
  mutate(Temperature = as.numeric(Temperature)) %>%
  select (-c(Iter,NetReturn))
prediction_by_hour <- prediction_by_hour[match(unique(prediction_by_hour$Temperature), prediction_by_hour$Temperature),]
prediction_by_hour[ , 2:4 ] <- sapply( prediction_by_hour[ , 2:4 ] , unscale_NR )
prediction_by_hour[ , 1 ] <- sapply( prediction_by_hour[ , 1 ] , unscale_Temp )
each_hour_prediction <- prediction_by_hour

scaled_data_tp_print <- data[ (data$StationIndex == 1) & (data$HourIndex == 8), ]
plot_data <- each_hour_prediction
p_s8_h1_p <- ggplot() +
  geom_point(data = scaled_data_tp_print,
             aes(Temperature, NetReturn), 
             shape = 1, color = 'dodgerblue') +
  
  geom_ribbon(data = plot_data,
              aes(x = Temperature, ymin = hpdi_l, ymax = hpdi_h), alpha = .5) +
  geom_line(data = plot_data,
            aes(x = Temperature, y = mu)) +
  ggtitle("NetReturn ~ Temparature (Station=1, Hour=8)")
p_s8_h1_p


register_google("AIzaSyD1u3tQHKMT2iPHpYJ_NW6DLw-GN6InCyc")
baseMap <- get_map(location = c(lon = 121.542902,lat = 25.027317), language="en", maptype="roadmap", zoom = 14)
station <- read.csv("stationLocation.csv", header=T)
netReturn <- read.csv("each_station_NetReturn_prediction_by_hour.csv", header=T)

for (i in 1:24) {
  data = subset(netReturn, Hour == i)
  if (i < 10) {
    filename = paste("net_return_0", as.character(i), ".png", sep="")
  } else {
    filename = paste("net_return_", as.character(i), ".png", sep="")
  }
  plot <- ggmap(baseMap)
  data$lon <- station$lon
  data$lat <- station$lat
  data$color <- ifelse(data$mu >= 0, "positive", "negative")
  data$size <- abs(data$mu)
  
  ggmap(baseMap) + 
    geom_point(aes(x = lon, y = lat, color = color, size=size), data = data) + 
    geom_text(aes(x = lon + 0.0002, y = lat + 0.0008, label=id, vjust=0, hjust=0.5), data=station, size=1) + 
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
    ggtitle(paste("Hour", as.character(i)))
  ggsave(filename, width=6, height=6, dpi=600)
}

# Model 2
model.2 = "
  data{
    int<lower=1> N;
    real NetReturn[N];
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
  
  NetReturn ~ normal( mu , sigma );
}
generated quantities{
  vector[2000] log_lik;
  vector[2000] mu;
  for ( i in 1:2000 ) {
    mu[i] = a + b_M * Min[i] + b_N * Num[i] +
      a_H[H[i]] + b_TH[H[i]] * T[i] + b_PH[H[i]] * P[i];
    log_lik[i] = normal_lpdf( NetReturn[i] | mu[i] , sigma );
  }
}
"

fit.2 <- readRDS("fit_2_ll.rds")
# fit.2 <- stan( model_code=model.2 , data=model.data , 
#                chains=3 , cores=3 , iter=3000 )

# traceplot( fit.2 , pars=c("a","b_M","b_N","a_H[1]","b_TH[1]","b_PH[1]") )
# pairs( fit.2 , pars=c("a","b_M","b_N") )
print( fit.2 , pars=c("a","b_M","b_N","b_TH") , probs=c(.1, .5, .9) )
plot( fit.2 , pars=c("a","b_M","b_N") )
# fit.2@stanmodel@dso <- new("cxxdso")
# saveRDS(fit.2, file = "fit_2_ll.rds")

model.3 <- "
data{
  int<lower=1> N;
  real NetReturn[N];
  int S[N];
  int H[N];
}
parameters{
  vector[31] a_S;
  vector[24] a_H;
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
  }
  
  for ( i in 1:N ) {
    mu[i] = a_S[S[i]] + a_H[H[i]];
  }
  
  NetReturn ~ normal( mu , sigma );
}
generated quantities{
  vector[2000] log_lik;
  vector[2000] mu;
  for ( i in 1:2000 ) {
    mu[i] = a_S[S[i]] + a_H[H[i]];
    log_lik[i] = normal_lpdf( NetReturn[i] | mu[i] , sigma );
  }
}
"
fit.3 <- readRDS("fit_3_ll.rds")
# fit.3 <- stan( model_code=model.3 , data=model.data , 
#                chains=3 , cores=3 , iter=3000 )

# traceplot( fit.3 , pars=c("a","b_M","b_N","a_H[1]","b_TH[1]","b_PH[1]") )
# pairs( fit.3 , pars=c("a","b_M","b_N") )
plot( fit.3 , pars=c("a_S") , probs=c(.1, .5, .9) )
# print( fit.3 , pars=c("a_H") )
# fit.3@stanmodel@dso <- new("cxxdso")
# saveRDS(fit.3, file = "fit_3_ll.rds")

# Calculate WAIC and compare models
fit.1 <- readRDS(file = "fit_1_ll.rds")
fit.2 <- readRDS(file = "fit_2_ll.rds")
fit.3 <- readRDS(file = "fit_3_ll.rds")
log_lik_1 <- extract_log_lik(fit.1, merge_chains = FALSE)
waic_1 <- waic(log_lik_1)
log_lik_2 <- extract_log_lik(fit.2, merge_chains = FALSE)
waic_2 <- waic(log_lik_2)
log_lik_3 <- extract_log_lik(fit.3, merge_chains = FALSE)
waic_3 <- waic(log_lik_3)
compare_model <- loo::compare(waic_1, waic_2, waic_3)
compare_model
