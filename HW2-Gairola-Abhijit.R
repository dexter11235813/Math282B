B = 200 
#initializing the n vector
n = c(1e2,1e3,1e4,1e5)
#creating a matrix for the final presentation of data. 
tab = matrix(0,nrow = 4,ncol = 3)
tab[1:4,1] = n
for(j  in 1:4)
{
  x = runif(n[j])
  countOLS = 0
  countWLS = 0
for( i in 1:B)
{
#initializing y
y=1+2*x+ rnorm(n[j], mean = 0,sd = x/sqrt(10))
#fitting linear model for OLS estimation
fit = lm(y ~ x)
#fitting linear model for WLS estimation, weight = 1/x^2
fitw = lm(y ~ x,weights = x^(-2))

#calculating the 2.5% and 97.5% CI for OLS and WLS methods
lowlimOLS = confint(fit)[2,1]
uplimOLS = confint(fit)[2,2]
lowlimWLS = confint(fitw)[2,1]
uplimWLS = confint(fitw)[2,2]
#obtaining count of the total number of times the true slope = = 2
# lies within the CI for OLS and WLS methods 
countOLS = countOLS + as.numeric((2 > lowlimOLS)  && (2 < uplimOLS))
countWLS = countWLS + as.numeric((2 > lowlimWLS)  && (2< uplimWLS))
}
tab[j,2] = countOLS
tab[j,3] = countWLS  
}
colnames(tab) = c("n","OLS","WLS")
rownames(tab)=  c(1,2,3,4)
tab

#     n   OLS WLS
# 1 1e+02 184 194
# 2 1e+03 190 191
# 3 1e+04 183 186
# 4 1e+05 188 194

## from the table above, we see that the true mean lies within the CI evaluated by the WLS method more often in 200 iterations
#  than within the CI evaluated by the OLS method. This is expected, since the WLS method gives more accurate estimates
#  when the variation in response variable is not uniform across the independence variable.
######### Q2 ############

p = c(1:30)
n = 1e5
z = as.matrix(seq(0,1,length.out= n))
x = rep(1,n)
ka = c()
for(i in 1:length(p))
{
x = cbind(x,z^(i))
ka = c(ka,kappa(x))
}
plot(log10(ka))

# From the plot, we see that the lof of condition number increases with the increase in the degree of fitted polynomial
# The condition number for p = 1 is 4.51, while the condition number for p = 30 is 9.48e15 , which indicates that the 
# change in response variable will be very large for small changes in the independent variable. For a matrix for condition
# number K , the expected loss of precesion is log10(K) decimal places of accuracy.




######## Q3 #########
load("04cars.version2.rda")
x = dat[,c(8)]
y = dat[,c(9)]
p= 2
confband = function(hp,mpg,p)
{
  # defining range of x
  pts = seq(0,600, len=100)
  #fitting the linear model with a p degree polynomial
  fit3 = lm(mpg ~ poly(hp, p, raw = TRUE),data = dat)
  val = as.data.frame(predict(fit3, data.frame(hp = pts), interval = "confidence"))
  plot(hp,mpg, pch = 16)
  # plotting the condifence intervals
  lines(cbind(pts,val$lwr),col = "blue",lty = "dashed")  
  lines(cbind(pts,val$upr),col = "blue",lty = "dashed")
  #polygon(c(rev(pts),pts),c(rev(val[,3]),val[,2]),col = colors()[c(240)])
  polygon(c(pts,rev(pts)),c(val[,2],rev(val[,3])),col = "blue")
  
  lines(pts, val$fit, col="red", lwd = 3)
  
}

confband(x,y,p)

# from the plots , we can see that the confidence band is close to the fitted line where the data point density is high
# and is more spaced out in regions where the data points are sparse. Also, polynomials with degree > 11 produce a very
# broad confidence band for high values of hp. 