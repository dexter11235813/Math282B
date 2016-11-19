setwd("C:/Users/abhijit331/Dropbox/math282B-Winter16")
setwd("/home/abhijit331/Dropbox/math282B-Winter16")
load("04cars.rda")
tmp = dat[,c("All-Wheel_Drive","Rear-Wheel_Drive","Horsepower","Highway_MPG")] # extract selected variables
tmp = tmp[complete.cases(tmp),] # extracts complete cases
tmp = as.data.frame(tmp)
names(tmp) = c("awd","rwd","hp","mpg") # abbreviate names
dim(tmp)
## Part a
pts = seq(0, 600, len=100) # covers the range of hp

fit = lm(mpg ~ poly(hp,2,raw = TRUE), data = tmp)
val = predict(fit,data.frame(hp=pts))
plot(tmp$hp, tmp$mpg, pch = 16)
lines(pts, val, col="red", lwd = 3)



#### Part B 
head(tmp)
tmp$drive = 3
tmp$drive[which(tmp$awd == 1)] = 1
tmp$drive[which(tmp$rwd == 1)] = 2

fit1 = lm(mpg ~ 0+poly(hp,2,raw = T)+as.factor(drive),data = tmp)
val1 = predict(fit1,data.frame(hp = pts,drive = 1))
val2 = predict(fit1,data.frame(hp = pts,drive = 2))
val3= predict(fit1,data.frame(hp = pts, drive = 3))
x = subset(tmp,tmp$drive == 1)
y = subset(tmp,tmp$drive == 2)
z = subset(tmp,tmp$drive == 3)
plot(x$hp, x$mpg, pch = 16,col = "red",xlim =c(0,600),ylim =c(0,100))
par(new = TRUE)
plot(y$hp, y$mpg, pch = 16, col = "blue",xlim =c(0,600),ylim =c(0,100))
par(new = TRUE)
plot(z$hp, z$mpg, pch = 16, col = "green",xlim =c(0,600),ylim = c(0,100))
lines(pts,val1,col = "red",lwd = 3)
lines(pts,val2,col = "blue",lwd = 3)
lines(pts,val3,col = "green",lwd = 3)
legend("topright", legend = c("All-Wheel Drive","Rear-Wheel Drive","Front-Wheel Drive"),col = c("red","blue","green"),lty = 1)


## Part 3


fit2 = lm(mpg ~0+ hp*as.factor(drive)+ hp+ I(hp^2)+as.factor(drive),data = tmp)
val1 = predict(fit2,data.frame(hp = pts,drive = 1))
val2 = predict(fit2,data.frame(hp = pts,drive = 2))
val3= predict(fit2,data.frame(hp = pts, drive = 3))
x = subset(tmp,tmp$drive == 1)
y = subset(tmp,tmp$drive == 2)
z = subset(tmp,tmp$drive == 3)
plot(x$hp, x$mpg, pch = 16,col = "red",xlim =c(0,600),ylim =c(0,100))
par(new = TRUE)
plot(y$hp, y$mpg, pch = 16, col = "blue",xlim =c(0,600),ylim =c(0,100))
par(new = TRUE)
plot(z$hp, z$mpg, pch = 16, col = "green",xlim =c(0,600),ylim = c(0,100))
lines(pts,val1,col = "red",lwd = 3)
lines(pts,val2,col = "blue",lwd = 3)
lines(pts,val3,col = "green",lwd = 3)
legend("topright", legend = c("All-Wheel Drive","Rear-Wheel Drive","Front-Wheel Drive"),col = c("red","blue","green"),lty = 1)

## Part 4

fit3 = lm(mpg ~ 0+ hp + I(hp^2) + hp*as.factor(drive)+ I((hp^2))*as.factor(drive) +as.factor(drive),data = tmp)
val1 = predict(fit3,data.frame(hp = pts,drive = 1))
val2 = predict(fit3,data.frame(hp = pts,drive = 2))
val3= predict(fit3,data.frame(hp = pts, drive = 3))
x = subset(tmp,tmp$drive == 1)
y = subset(tmp,tmp$drive == 2)
z = subset(tmp,tmp$drive == 3)
plot(x$hp, x$mpg, pch = 16,col = "red",xlim =c(0,600),ylim =c(0,100))
par(new = TRUE)
plot(y$hp, y$mpg, pch = 16, col = "blue",xlim =c(0,600),ylim =c(0,100))
par(new = TRUE)
plot(z$hp, z$mpg, pch = 16, col = "green",xlim =c(0,600),ylim = c(0,100))
lines(pts,val1,col = "red",lwd = 3)
lines(pts,val2,col = "blue",lwd = 3)
lines(pts,val3,col = "green",lwd = 3)
legend("topright", legend = c("All-Wheel Drive","Rear-Wheel Drive","Front-Wheel Drive"),col = c("red","blue","green"),lty = 1)







## Q2

ex = read.table("exmpl8.10.txt",header = TRUE)
head(ex)
mod = lm(y ~ as.factor(atemp) * as.factor(gtemp) * as.factor(variety), data = ex)
Anova(mod,type = c("II")) # 0.0001305
Anova(mod,type = c("III")) # 0.4755
anova(mod) # 0.0001305
# The p values for Type 1 and Type II ANOVA test is the same and equal to 0.0001305 . This is beacuse for Type 1 ANOVA, 
# the SS for each factor is the incremental improvement in the error
#SS as each factor is added in the model. Thus, SS(gtemp*variety) is evaluated
# after evaluating SS for (atemp + gtemp+variety + atemp*gtemp+atemp*variety). For Type II ANOVA, we evaluate SS for the smallest 
# model which contains (gtemp*variety), which is the same as (atemp + gtemp+variety + atemp*gtemp+atemp*variety). Therefore, we get the
# same values





#Q3

library(L1pack)
library(MASS)
library(smoothmest)
# for n = 100
B = 200
beta1lsq = c()
beta1lar = c()
x = rnorm(100,mean = 0, sd = 1)
for(i in 1:200)
{
  y = 3*x+rnorm(100,mean = 0, sd = 0.5)
  lsq = lm(y ~ x)
  lar = lad(y ~ x)
  beta1lsq = c(beta1lsq,lsq$coefficients[2])
  beta1lar = c(beta1lar,lar$coefficients[2])
}
boxplot(data.frame(LAR = beta1lar,LSQ = beta1lsq),main = "For normal error n = 100")

# for n = 1000
B = 200
beta1lsq = c()
beta1lar = c()
x = rnorm(1000,mean = 0, sd = 1)
for(i in 1:200)
{
  y = 3*x+rnorm(1000,mean = 0, sd = 0.5)
  lsq = lm(y ~ x)
  lar = lad(y ~ x)
  beta1lsq = c(beta1lsq,lsq$coefficients[2])
  beta1lar = c(beta1lar,lar$coefficients[2])
}
boxplot(data.frame(LAR = beta1lar,LSQ = beta1lsq),main = "For normal error n = 1000")



# For normally distributed error, the tail is thinner compared to laplace distributed error. We can see that the variation in LSQ
# is less than variation of slope in LAR





# Part B
# for n = 100
beta1lsqlap = c()
beta1larlap = c()
for(i in 1:B)
{
  y = 3*x+rdoublex(100, 0,0.5)
  lsq1 = lm( y ~ x)
  lar1 = lad(y ~ x)
  beta1lsqlap = c(beta1lsqlap,lsq1$coefficient[2])
  beta1larlap = c(beta1larlap,lar1$coefficient[2])
}
boxplot(data.frame(LSQ = beta1lsqlap,LAR = beta1larlap), main = "For Laplace errors n=100")

# for n = 1000
beta1lsqlap = c()
beta1larlap = c()
for(i in 1:B)
{
  y = 3*x+rdoublex(1000, 0,0.5)
  lsq1 = lm( y ~ x)
  lar1 = lad(y ~ x)
  beta1lsqlap = c(beta1lsqlap,lsq1$coefficient[2])
  beta1larlap = c(beta1larlap,lar1$coefficient[2])
}
boxplot(data.frame(LSQ = beta1lsqlap,LAR = beta1larlap), main = "For Laplace errors n = 1000")

# Since Laplace distributed error has fatter tails as compared to normal errors, we see that least square model, because it penalizes
# large deviations from the mean , has a higher variation in slope as compared to LAR