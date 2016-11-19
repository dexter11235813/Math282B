n = 1000            #n = 10,100,1000
#Generating a vector x of n random variables with mean = 0 and var = sqrt(2)
x = rnorm(n,mean = 0 , sd = sqrt(2))  
B = 999
var = c()  #vector to store variance 
beta0= c() #vector to store beta0
beta1=c()  #vector to store beta1
#ctr=0
for (i in (1:B))
{
  #Generating y according to instructions
  y = -1 + 3*x + rnorm(n,mean = 0 , sd = 1)  
  #Fitting the linear model with x as the predictor variable and y as the response variable
  lsq = lm(y ~ x)
  #Calculating variance of the model and storing it in variable 'var'
  v = sum(lsq$residuals* lsq$residuals)/(n-2)
  var = c(var,v)
  beta0 = c(beta0, lsq$coefficients[1])
  beta1 = c(beta1, lsq$coefficients[2])
}
##############Q1 A#################################
hist(var,prob = TRUE)
# Obtaining the upper and lower limit of the distribution
lim= par("usr")
x1 = lim[1]
x2 = lim[2]
# Generating a vector s with values b/w x1 and x2
s=seq(x1,x2,0.001)
par(new = TRUE)
plot(dchisq((n-2)*s,n-2),axes = FALSE,xlab = " ",ylab=" ",type = "l")

#The variance estimates is Chi Squared distributed with n-2 degrees of freedom, as expected from theory
####################Part B & C######################

# Finding the variance of beta0 and beta1 and storing the matrix in var 
x = cbind(rep(1,n),x)
trp = solve(t(x) %*% x)

#### B ####

hist(beta0,prob = TRUE)
lim= par("usr")
x3 = lim[1]
x4 = lim[2]
s0=seq(x3,x4,0.001)
par(new= TRUE)
sd0 = sqrt(trp[1,1])
plot(dnorm(s0,-1,sd0),axes = FALSE,xlab =" ",ylab=" ",type = "l")

#The histogram for b0 is normally distributed, as expected from theory.
###### C #####
hist(beta1,prob = TRUE)
lim= par("usr")
x5 = lim[1]
x6 = lim[2]
s1=seq(x5,x6,0.001)
par(new= TRUE)
sd1=sqrt(trp[2,2])
plot(dnorm(s1,3,sd1),axes = FALSE,xlab =" ",ylab=" ",type = "l")

#The histogram for beta1 is normally distributed, as expected. 

##### Optional Problem #######
plot(beta0,beta1,col ="red", main =" Intercept vs Slope")
 # From the scatter plot, we see that the data points are concentrated around the mean of vectors beta0 and beta1. Aslo, as expected 
# in a bivariate normal distribution, we see an approximate ellipse around the data points in the scatterplot.



########################################## Q2 ##############################3



load("04cars.rda") # loads a data frame called "dat"
tmp = dat[,c(13,15)] # extract selected variables
tmp = tmp[complete.cases(tmp),] # extracts complete cases
n=0
ls = c()
# Fitting a linear model on the full dataset 
fitfull = lm(dat$Highway_MPG ~ dat$Horsepower, data = dat) 

for( i in (1:1000))
{
# Sampling 50 random samples from tmp with no replacement
p = tmp[sample(nrow(tmp),50,replace = FALSE),]
names(p) = c("hp","mpg") # abbreviate names

# Fitting model on the sampled data
fit = lm(p$mpg ~ p$hp, data = p)
#storing all the LS slope coefficients in ls
ls = c(ls,fit$coefficients[2]) #for the last part
slope_full=fitfull$coefficients[2]


# If the estimate of slope lies within the 95% confidence interval, 
# the as.numeric fucntion adds 1 to the variable n, initialized at 0
# the final value of n gives us the numebr of times LS slope estimates
# fall within the CI
n = n + as.numeric( (slope_full > confint(fit)[2,1]) && (slope_full < confint(fit)[2,2]))

}
#Proportion of time CI contained the LS slope estimate. Around 85%
print(n/1000)
#Proportion of time CI did not contain the LS estimate. Around 15%
print((1000-n)/1000)

##Part B ##
hist(ls) 
