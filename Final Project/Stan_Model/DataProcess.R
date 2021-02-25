library(stringr)
library(plyr)
library(rethinking)
library(ggplot2)
library(rgl)
library(plotly)
setwd("D:\\碩士班教材\\碩一下\\多變量分析\\FinalProject")
countD_rent = plyr::count(dat$rent_sta_sarea)
countD_return = plyr::count(dat$return_sta_sarea)
par(mfrow = c(1, 2))
hist(dat$rent_sta_sarea, col = "cyan",breaks = 30, xlab = "District", main = "Frequency of rent")
hist(dat$return_sta_sarea, col = "cyan", xlab = "District", main = "Frequency of return")
dat = read.table("D:\\碩士班教材\\碩一下\\多變量分析\\201601.txt", header = T)
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