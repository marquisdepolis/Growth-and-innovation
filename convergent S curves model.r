time = 2020 #loose proxy for number of cycles
tech = 200 #proxy for number of industries/ technologies/ fields
techinit = 1 #proxy for initial list of technologies to grow
c = 2 #	the logistic curve's maximum value
b1 = -0.005 #logistic growth rate or steepness of the curve
sets = 3 #combination of how many techs is needed to be assessed
rand = runif(tech) #a randomiser to change the S curves for tech
t <- 1:time
x <- matrix (0,time,tech)
pop <- read.csv(file = 'population.csv')
pop <- pop[1:time,]
temp <- matrix (time,time,tech)
effort <- matrix(1, time, tech)
timeline <- rep(0,times = tech)
threshold = (1:tech)^2/tech 
#max threshold the multiples of tech has to achieve, as an S curve
for (j in 1:tech) {
  threshold[j] = (c/((1+(c-1)*(exp(b1*j))))-1)*rand[j]
}

#sum of threshold figures
thresholdsums <- threshold
for (j in 2:tech) {
  thresholdsums[j] = threshold[j]+thresholdsums[j-1]
}

#Set up initial set of technologies, developing from year 0
for (i in 1:time) {
  for (j in 1:tech){
    x[i,j] = (c/((1+(c-1)*(exp(b1*t[i]))))-1)
  }
}

#Set up effort variable - a measure of how difficult tech progress is because how many other techs already exist
for (i in 1:time) {
  for (j in 1:tech){
    effort[i,j] = (c/((1+(c-1)*(exp(b1*j))))+i/time)
#    effort[i,j] = (j/choose(j,sets))
  }
}
product <- matrix(1,time,tech)

#Figure out when a new tech might emerge, and then let it emerge
for (i in 1:time){
  for (j in techinit:tech){
    for (p in 1:j){
      product[i,j] = x[i,p]*product[i,p]
    }
    if (product[i,j] > (effort[i,j]*thresholdsums[j])*rand[j]) {
      temp[i,j] = min(i-1,temp[,j])
      x[i,j] <- (c/(1+(c-1)*(exp(b1*t[i-temp[i,j]])))-1)*(i/time)*(j/tech)+(i/time)
    }
    else{
      x[i,j] = 0
    }
  }
}

#Figure out what year each technology emerged
for (j in 1:tech){
  temp2 <- temp
  temp2 [temp2 == time] <- 0
  timeline[j] <- max(temp2[,j])
}
timegap <- 1:tech
for (j in 2:tech) {
  timegap[j] <- timeline[j] - timeline[j-1]
}

###################################################
#GDP growth
sum <- rowSums(x)
sumgrowth <- sum
for (i in 2:time) {
  sumgrowth[1] <- 1
  sumgrowth[i] <- (sum[i]/sum[i-1])-1
}
plot(sumgrowth)
researcher <- 0.01
GDPpc_start <- 1000 #per capita GDP starting point
GDP <- researcher*pop[,2]*sum*GDPpc_start

#plotting the variables
plot(GDP)
plot(sumgrowth)
plot(timegap)
plot(threshold/max(threshold), type = "b")
lines(x[time,], type = "b")
lines(timeline/time, type = "b")
plot(sum)
plot(thresholdsums)
plot(threshold)
plot(timeline)