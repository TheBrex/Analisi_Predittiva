### R code from vignette source 'l00.Rnw'

###################################################
### code chunk number 1: l00.Rnw:41-43
###################################################
prostate<-read.table("data/prostate.txt",header=TRUE)
pairs(prostate[,c("lpsa","lcavol","lweight","age")])


###################################################
### code chunk number 2: l00.Rnw:102-105
###################################################
ad<-read.csv(file='http://faculty.marshall.usc.edu/gareth-james/ISL/Advertising.csv')
attach(ad)
plot(TV, sales, cex.lab=2, cex.axis=1.2)


###################################################
### code chunk number 3: l00.Rnw:108-109
###################################################
plot(radio,sales,cex.lab=2,cex.axis=1.2)


###################################################
### code chunk number 4: l00.Rnw:112-113
###################################################
plot(newspaper,sales,cex.lab=2,cex.axis=1.2)


###################################################
### code chunk number 5: l00.Rnw:124-129
###################################################
lm.radio<-lm(sales ~ radio)
lm.tv <- lm(sales ~ TV)
lm.newspaper <-lm(sales ~ newspaper)
plot(TV, sales, cex.lab=2, cex.axis=1.2)
#abline(lm.tv, col = "blue", lty = 1, lwd = 2)


###################################################
### code chunk number 6: l00.Rnw:132-134
###################################################
plot(radio,sales,cex.lab=2,cex.axis=1.2)
#abline(lm.radio, col = "blue", lty = 1, lwd = 2)


###################################################
### code chunk number 7: l00.Rnw:137-139
###################################################
plot(newspaper,sales,cex.lab=2,cex.axis=1.2)
#abline(lm.newspaper, col = "blue", lty = 1, lwd = 2)


###################################################
### code chunk number 8: l00.Rnw:153-158
###################################################
lm.radio<-lm(sales ~ radio)
lm.tv <- lm(sales ~ TV)
lm.newspaper <-lm(sales ~ newspaper)
plot(TV, sales, cex.lab=2, cex.axis=1.2)
abline(lm.tv, col = "blue", lty = 1, lwd = 2)


###################################################
### code chunk number 9: l00.Rnw:161-163
###################################################
plot(radio,sales,cex.lab=2,cex.axis=1.2)
abline(lm.radio, col = "blue", lty = 1, lwd = 2)


###################################################
### code chunk number 10: l00.Rnw:166-168
###################################################
plot(newspaper,sales,cex.lab=2,cex.axis=1.2)
abline(lm.newspaper, col = "blue", lty = 1, lwd = 2)


###################################################
### code chunk number 11: l00.Rnw:239-244
###################################################
income<-read.csv(file='data/Income1.csv')
attach(income)
plot(Education, Income, cex.lab=2, cex.axis=1.2,cex=2,col='red')
fit.lm<-lm(Income~Education)
lines(Education,predict(fit.lm),col="blue",lwd=2)


###################################################
### code chunk number 12: l00.Rnw:251-254
###################################################
plot(Education, Income, cex.lab=2, cex.axis=1.2,cex=2,col='red')
fit.spl<-smooth.spline(Education,Income)
lines(fit.spl, col = "blue",lwd=2)


###################################################
### code chunk number 13: l00.Rnw:277-287
###################################################
set.seed(19)
x<--20:20
x<-c(-20:0,7:20)
n<-length(x)
y<-1+0.4*x+0.05*x^2+rnorm(n,sd=3)
plot(x,y,xlab="X",ylab="Y",cex.lab=1.2,col="red",cex=2)
lm.1<-lm(y~x)
lines(x,fitted(lm.1),col="blue",lwd=3)
title("Underfitting",cex.main=2)



###################################################
### code chunk number 14: l00.Rnw:290-294
###################################################
plot(x,y,xlab="X",ylab="Y",cex.lab=1.2,col="red",cex=2)
lm.2<-lm(y~x+I(x^2))
lines(x,fitted(lm.2),col="blue",lwd=3)
title("Balanced",cex.main=2)


###################################################
### code chunk number 15: l00.Rnw:297-301
###################################################
plot(x,y,xlab="X",ylab="Y",cex.lab=1.2,col="red",cex=2)
lm.10<-lm(y~poly(x,degree = 24))
lines(x,fitted(lm.10),col="blue",lwd=3)
title("Overfitting",cex.main=2)


###################################################
### code chunk number 16: l00.Rnw:324-327
###################################################
plot(x,y,xlab="X",ylab="Y",cex.lab=1.2,col="red",cex=2)
lines(x,fitted(lm.1),col="blue",lwd=3)
title("High Bias & Low Variance",cex.main=2)


###################################################
### code chunk number 17: l00.Rnw:330-333
###################################################
plot(x,y,xlab="X",ylab="Y",cex.lab=1.2,col="red",cex=2)
lines(x,fitted(lm.2),col="blue",lwd=3)
title("Balanced Bias-Variance tradeoff",cex.main=2)


###################################################
### code chunk number 18: l00.Rnw:336-339
###################################################
plot(x,y,xlab="X",ylab="Y",cex.lab=1.2,col="red",cex=2)
lines(x,fitted(lm.10),col="blue",lwd=3)
title("Low Bias & High Variance",cex.main=2)


### R code from vignette source 'l01.Rnw'

###################################################
### code chunk number 1: l01.Rnw:6-7
###################################################
options(continue = "   ", prompt = " ")


### R code from vignette source 'l02.Rnw'

###################################################
### code chunk number 1: l02.Rnw:6-7
###################################################
options(continue = "   ", prompt = " ")


###################################################
### code chunk number 2: l02.Rnw:60-61
###################################################
curve(7+(0.57-x)^2, from=-2,to=2, xlab="m", ylab="MSE(m)",cex.lab=1.1)


###################################################
### code chunk number 3: l02.Rnw:109-125
###################################################
set.seed(19)
n<-400
x<-rnorm(n,4)
b0<-0.1
b1<-0.001
b2<-0.01
sigma<-0.3
f<-function(x)
{
return(b0*x+b1*x^2+b2*x^3)
}
y<-b0*x+b1*x^2+b2*x^3+rnorm(n,sd=sigma)
plot(x,y,col="gray")
curve(f,from=min(x),to=max(x),col=2,add=TRUE)
points(4,f(4),col=2,pch=20,cex=2)
abline(v=4,col=2)


###################################################
### code chunk number 4: l02.Rnw:153-164
###################################################
	mse <- function(b0,b1,E.Y.sq=10,E.Y=2,Cov.XY=-1,E.X=-0.5,Var.X=3) {
	E.Y.sq-2*b0*E.Y-2*b1*Cov.XY-2*b1*E.X*E.Y+b0^2+2*b0*b1*E.X + Var.X*b1^2 + (E.X*b1)^2
	}
	curve(mse(b0=-1,b1=x),from=-1,to=1,lty="solid",ylim=c(0,25),xlab=expression(b[1]),
	ylab=expression(MSE(b[0],b[1])),cex.lab=1.1)
	curve(mse(b0=0,b1=x),add=TRUE,lty="dashed")
	curve(mse(b0=1,b1=x),add=TRUE,lty="dotted")
	l<-expression(b[0]==-1,b[0]==0,b[0]==1)
# #legend("topleft",legend=c(expression(x[2],expression(x[2])),lty=c("solid","dashed","dotted"))
	legend("topleft",legend=l,lty=c("solid","dashed","dotted"))
	


###################################################
### code chunk number 5: l02.Rnw:291-297
###################################################

plot(x,y,col="gray",cex.lab=1.2)
curve(f,from=min(x),to=max(x),col=2,add=TRUE,lwd=2)
fit.lm<-lm(y~x)	
lines(x,predict(fit.lm),col=4,lwd=2)
legend("topleft",legend=c("regression function","optimal linear predictor"),col=c(2,4),lwd=2,cex=1.2)


### R code from vignette source 'l03.Rnw'

###################################################
### code chunk number 1: l03.Rnw:6-7
###################################################
options(continue = "   ", prompt = " ")


###################################################
### code chunk number 2: l03.Rnw:96-113
###################################################
set.seed(20)
n<-30
rho<-0.4
# library(mvtnorm)
# y<-rmvnorm(n = n,mean = c(1,2),sigma = matrix(c(1,rho,rho,1),nrow = 2))
y <- MASS::mvrnorm(n = n, mu = c(1,2), Sigma =  matrix(c(1,rho,rho,1),nrow = 2))
plot(y,xlab="x",ylab="y",col="gray",pch=20,cex=2, 
     xlim = c(floor(min(y[,1])),ceiling(max(y[,1]))), 
     ylim = c(floor(min(y[,2])),ceiling(max(y[,2]))))
mx<-mean(y[,1])
my<-mean(y[,2])
abline(v=mx)
abline(h=my)
text(mx+1,my+2,labels = expression((x[i]-bar(x))(y[i]-bar(y)) >0))
text(mx+1,my-1,labels = expression((x[i]-bar(x))(y[i]-bar(y)) <0))
text(mx-1,my+2,labels = expression((x[i]-bar(x))(y[i]-bar(y)) <0))
text(mx-1,my-1,labels = expression((x[i]-bar(x))(y[i]-bar(y)) >0))


###################################################
### code chunk number 3: l03.Rnw:122-129
###################################################
ad<-read.csv(file='http://faculty.marshall.usc.edu/gareth-james/ISL/Advertising.csv')
attach(ad)
plot(TV, sales, cex.lab=2, cex.axis=1.2,pch=20,col="grey",cex=2)
cr<-round(cor(TV,sales),2)
title(main=substitute(r[xy]==a,list(a=cr)),cex.main=2)
abline(v=mean(TV))
abline(h=mean(sales))


###################################################
### code chunk number 4: l03.Rnw:132-137
###################################################
plot(newspaper,sales,cex.lab=2,cex.axis=1.2,pch=20,col="grey",cex=2)
cr<-round(cor(newspaper,sales),2)
title(main=substitute(r[xy]==a,list(a=cr)),cex.main=2)
abline(v=mean(newspaper))
abline(h=mean(sales))


###################################################
### code chunk number 5: l03.Rnw:163-174
###################################################
library(ggplot2)
library(dslabs)
ds_theme_set()
n <- 250
cors <- c(-0.9,-0.5,0,0.5,0.9,0.99)
sim_data <- lapply(cors,function(r) MASS::mvrnorm(n,c(0,0), matrix(c(1,r,r,1),2,2)))
sim_data <- do.call(what = rbind, args = sim_data)
sim_data <- cbind( rep(cors, each=n), sim_data)
colnames(sim_data) <- c("r","x","y")
ggplot(data = as.data.frame(sim_data),  aes(x,y)) +
  facet_wrap(~r) + geom_point() +geom_vline(xintercept = 0,lty=2) + geom_hline(yintercept = 0,lty=2) 


###################################################
### code chunk number 6: l03.Rnw:187-197
###################################################
library(tidyr)#; library(magrittr)
anscombe$row <- seq(1,nrow(anscombe))
anscombe %>% 
gather(name, value, -row) %>% 
separate(name, c("axis", "group"), sep=1) %>%
spread(axis, value) -> anscombe 
ggplot(anscombe, aes(x,y)) +
facet_wrap(~group)  +
geom_smooth(method="lm", fill=NA, fullrange=TRUE, color="blue") +
geom_point(bg="orange",color="red",cex=3,pch=21)


###################################################
### code chunk number 7: l03.Rnw:206-209
###################################################
library(dplyr)
# library(broom)
ds_theme_set()


###################################################
### code chunk number 8: l03.Rnw:216-227
###################################################
the_title <- paste("Correlation =", 
round(with(divorce_margarine, 
cor(margarine_consumption_per_capita, divorce_rate_maine)),2))
data(divorce_margarine)
divorce_margarine %>% 
ggplot(aes(margarine_consumption_per_capita, divorce_rate_maine)) + 
geom_point(cex=3) + 
geom_smooth(method = "lm") + 
ggtitle(the_title) +
xlab("Margarine Consumption per Capita (lbs)") + 
ylab("Divorce rate in Maine (per 1000)")


###################################################
### code chunk number 9: l03.Rnw:275-286
###################################################
N <- 100
Sigma <- matrix(c(1,0.75,0.75, 1), 2, 2)*1.5
means <- list(c(11,3), c(9,5), c(7,7), c(5,9), c(3,11))
dat <- lapply(means, function(mu) 
MASS::mvrnorm(N, mu, Sigma))
a<-Reduce(rbind, dat)
colnames(a)<-c("X","Y")
dat <- tibble::as.tibble(a) %>% mutate(Z = as.character(rep(seq_along(means), each = N)))
names(dat) <- c("X", "Y", "Z")
dat %>% ggplot(aes(X,Y)) + geom_point(alpha = .5) +
ggtitle(paste("correlation = ", round(cor(dat$X, dat$Y), 2)))


###################################################
### code chunk number 10: l03.Rnw:294-302
###################################################
a<-Reduce(rbind, means)
colnames(a)<-c("x","y")
means <- tibble::as.tibble(a) %>% setNames(c("x","y")) %>%mutate(z = as.character(seq_along(means)))
corrs <- dat %>% group_by(Z) %>% summarize(cor = cor(X,Y)) %>% .$cor 
dat %>% ggplot(aes(X, Y, color = Z)) + 
geom_point(show.legend = FALSE, alpha = 0.5) +
ggtitle(paste("correlations =",  paste(signif(corrs,2), collapse=" "))) +
annotate("text", x = means$x, y = means$y, label = paste("Z=", means$z), cex = 5)


### R code from vignette source 'l04.Rnw'

###################################################
### code chunk number 1: l04.Rnw:4-5
###################################################
options(continue = "   ", prompt = " ")


###################################################
### code chunk number 2: l04.Rnw:185-186
###################################################
head(cars)


###################################################
### code chunk number 3: l04.Rnw:189-190 (eval = FALSE)
###################################################
## ?cars


###################################################
### code chunk number 4: l04.Rnw:195-200
###################################################
plot(dist ~ speed, data = cars, 
xlab = "Speed (in Miles Per Hour)", 
ylab = "Stopping Distance (in Feet)",
main = "Stopping Distance vs Speed",
pch  = 20, cex  = 2,col  = "grey50")


###################################################
### code chunk number 5: l04.Rnw:204-206
###################################################
x <- cars$speed
y <- cars$dist


###################################################
### code chunk number 6: l04.Rnw:209-212
###################################################
my<-mean(y);   mx<-mean(x)
Cxy <- mean((x - mx) * (y - my))
Sxx <- mean((x - mx) ^ 2)


###################################################
### code chunk number 7: l04.Rnw:215-218
###################################################
beta_1_hat <- Cxy / Sxx
beta_0_hat <- my - beta_1_hat * mx
c(beta_0_hat, beta_1_hat) ## vector of estimated betas 


###################################################
### code chunk number 8: l04.Rnw:227-233
###################################################
plot(dist ~ speed, data = cars, 
xlab = "Speed (in Miles Per Hour)", 
ylab = "Stopping Distance (in Feet)",
main = "Stopping Distance vs Speed",
pch  = 20, cex  = 2,col  = "grey50")
abline(beta_0_hat,beta_1_hat, col = 2, lwd = 2)


###################################################
### code chunk number 9: l04.Rnw:241-247
###################################################
sum((y - (beta_0_hat+beta_1_hat*x))^2)
## take some othe r values of beta_0 and beta_1
sum((y - (17.6+4*x))^2)
sum((y - (17.5+3.8*x))^2)
### try any other value 
# the estimates minimize the sum of squares


###################################################
### code chunk number 10: l04.Rnw:261-262
###################################################
cars[cars$speed == 8,]


###################################################
### code chunk number 11: l04.Rnw:269-270
###################################################
beta_0_hat + beta_1_hat * 8


###################################################
### code chunk number 12: l04.Rnw:276-286
###################################################
plot(dist ~ speed, data = cars, 
xlab = "Speed (in Miles Per Hour)", 
ylab = "Stopping Distance (in Feet)",
main = "Stopping Distance vs Speed",
pch  = 20, cex  = 2,col  = "grey50")
abline(beta_0_hat,beta_1_hat, col = 2, lwd = 2)
segments(x0 = 8, y0=0, y1=beta_0_hat+beta_1_hat*8, 
         lty = 2, lwd = 2, col = 4)
segments(x0 = 0, x1 = 8, y0=beta_0_hat+beta_1_hat*8, 
         lty = 2, lwd = 2, col = 4)


###################################################
### code chunk number 13: l04.Rnw:293-295
###################################################
cars[cars$speed == 21,]
## no observations


###################################################
### code chunk number 14: l04.Rnw:299-300
###################################################
beta_0_hat + beta_1_hat * 21


###################################################
### code chunk number 15: l04.Rnw:304-314
###################################################
plot(dist ~ speed, data = cars, 
xlab = "Speed (in Miles Per Hour)", 
ylab = "Stopping Distance (in Feet)",
main = "Stopping Distance vs Speed",
pch  = 20, cex  = 2,col  = "grey50")
abline(beta_0_hat,beta_1_hat, col = 2, lwd = 2)
segments(x0 = 21, y0=0, y1=beta_0_hat+beta_1_hat*21, 
         lty = 2, lwd = 2, col = 4)
segments(x0 = 0, x1 = 21, y0=beta_0_hat+beta_1_hat*21, 
         lty = 2, lwd = 2, col = 4)


###################################################
### code chunk number 16: l04.Rnw:320-321
###################################################
beta_0_hat + beta_1_hat * 50


###################################################
### code chunk number 17: l04.Rnw:326-337
###################################################
plot(dist ~ speed, data = cars, 
xlab = "Speed (in Miles Per Hour)", 
ylab = "Stopping Distance (in Feet)",
main = "Stopping Distance vs Speed",
pch  = 20, cex  = 2,col  = "grey50",
xlim = c(min(x),55), ylim = c(0,beta_0_hat+beta_1_hat*55))
abline(beta_0_hat,beta_1_hat, col = 2, lwd = 2)
segments(x0 = 50, y0=0, y1=beta_0_hat+beta_1_hat*50, 
         lty = 2, lwd = 2, col = 4)
segments(x0 = 0, x1 = 50, y0=beta_0_hat+beta_1_hat*50, 
         lty = 2, lwd = 2, col = 4)


###################################################
### code chunk number 18: l04.Rnw:347-348 (eval = FALSE)
###################################################
## lm(formula = response ~ predictor, data = data)


###################################################
### code chunk number 19: l04.Rnw:351-352
###################################################
lm(dist~speed,data=cars)


###################################################
### code chunk number 20: l04.Rnw:356-357
###################################################
fit<-lm(dist~speed,data=cars);  fit


###################################################
### code chunk number 21: l04.Rnw:360-361
###################################################
options(width=60)


###################################################
### code chunk number 22: l04.Rnw:363-364
###################################################
typeof(fit); names(fit)


###################################################
### code chunk number 23: l04.Rnw:367-369
###################################################
fit$coefficients
fit$call


###################################################
### code chunk number 24: l04.Rnw:374-377
###################################################
plot(dist~speed,data=cars,pch  = 20,cex  = 2,
     col  = "grey")
abline(coef = fit$coefficients, col = 2)


###################################################
### code chunk number 25: l04.Rnw:541-571
###################################################
set.seed(1)
m<-1000 ### number of replicates of the "experiemnt" 
n<-100 ## sample size 
beta0<-1 ## beta0 true value
beta1<-2 ## beta1 true value
# use the replicate function - see ?replicate
## the experiment is 
# creating the Xs under two different Uniforms 
# creating the Ys as Y = beta0+beta1*X+noise
# estimating betas for the two different set of Xs
# make a function that does that 
testUnconditionalVars <- function(n,pars){
x1<-runif(n,-1,1); x2<-runif(n,-3,3)
e<-rexp(n)-1 ## notice same error for both generated Ys
y1<-pars[1]+pars[2]*x1+e
y2<-pars[1]+pars[2]*x2+e
out <- c(lm(y1~x1)$coef,lm(y2~x2)$coef)
as.vector(out)  
}
experiment <- replicate(m, testUnconditionalVars(n = n, pars = c(beta0,beta1)))
# m cols, 4 rows 
beta0.hat.1 <- experiment[1,]
beta0.hat.2 <- experiment[3,]
beta1.hat.1 <- experiment[2,]
beta1.hat.2 <- experiment[4,]
hist(beta1.hat.1,col=rgb(1,0,0,1/4),
freq=FALSE,xlab=expression(hat(beta)[1]),
cex.lab=1.5,xlim=c(1.3,2.7),ylim=c(0,7),main="")
hist(beta1.hat.2,col=rgb(0,0,1,1/4),add=TRUE,freq=FALSE)
legend("topright",legend=c("U(-1,1)","U(-3,3)"),fill = c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)))


###################################################
### code chunk number 26: scatter-of-regression-lines
###################################################
# Simulate from a linear model with uniform X and t-distributed noise
# Inputs: number of points; intercept; slope; width of uniform X distribution
# (symmetric around 0); degrees of freedom for t
# Output: data frame with columns for X and Y
sim.linmod <- function(n, beta.0, beta.1, width, df) {
# draw n points from a uniform distribution centered on 0
x <- runif(n, min=-width/2, max=width/2)
# draw n points from a t distribution with the given number of degrees
# of freedom
epsilon <- rt(n, df=df)
# make y from a linear model
y <- beta.0 + beta.1*x + epsilon
# return the data frame
return(data.frame(x=x, y=y))
}

# Create an empty plot (type="n" for "do Nothing")
plot(0,type="n",xlim=c(-10,10),ylim=c(-10,10),xlab="x",ylab="y")
# Repeat 10 times: do a simulation, fit a line to the sim., add the fitted
# line to the plot
invisible(replicate(20, abline(lm(y ~ x, data=sim.linmod(n=10,beta.0=5,
beta.1=-2,width=4,df=3)),col="grey")))
# Add the true regression line; exaggerate width so it stands out
abline(a=5, b=-2, lwd=5)
#abline(v=0,col="red",lwd=2)


###################################################
### code chunk number 27: l04.Rnw:776-783
###################################################
# ad<-read.csv(file='data/Advertising.csv')
ad<-read.csv(file='http://faculty.marshall.usc.edu/gareth-james/ISL/Advertising.csv')
attach(ad)
plot(radio, sales, cex.lab=2, cex.axis=1.2,pch=20,col="grey",cex=2)
fit<-lm(sales~radio)
abline(coef = fit$coefficients, col = 2)
#segments(TV,(fit$coefficients[1]+fit$coefficients[2]*TV),TV,sales)


###################################################
### code chunk number 28: l04.Rnw:836-843
###################################################
ad<-read.csv(file='http://faculty.marshall.usc.edu/gareth-james/ISL/Advertising.csv')
attach(ad)
fit<-lm(sales~radio) 
### sales is y. TV is x
e <- resid(fit) # The easiest way to get the residuals
yhat <- predict(fit) ## The easiest way to get the fitted values 
## The residuals are y - yhat


###################################################
### code chunk number 29: l04.Rnw:856-867
###################################################
x <-  ad$radio; y <- ad$sales
plot(x,y,pch=20,xlab="Radio",ylab="Sales",cex.lab=1.25,col="grey80")
fit0<-lm(y~1)
xp<-56
points(x[xp],y[xp], pch = 20)
abline(fit0,col=1,lwd=2)
small<-0.6
arrows(x[xp]+small,fit0$fit[xp],x[xp]+small,y[xp],code=3,length=0.07,lwd=1.5)
abline(fit,lwd=2,col="purple3")
arrows(x[xp],fit$fit[xp],x[xp],y[xp],col="red",code=3,length=0.07,lwd=1.5)
arrows(x[xp],fit0$fit[xp],x[xp],fit$fit[xp],col="blue",code=3,length=0.07,lwd=1.5)


###################################################
### code chunk number 30: l04.Rnw:930-935
###################################################
data(anscombe)
fit1<-lm(y1~x1,data=anscombe) 
fit2<-lm(y2~x2,data=anscombe)
fit3<-lm(y3~x3,data=anscombe)
fit4<-lm(y4~x4,data=anscombe) 


###################################################
### code chunk number 31: l04.Rnw:940-942
###################################################
plot(y1~x1,cex.lab=2,pch=20,cex=2,xlab="x",ylab="y",cex.lab=1.5,data=anscombe)
abline(fit1)


###################################################
### code chunk number 32: l04.Rnw:945-947
###################################################
plot(y2~x2,cex.lab=2,pch=20,cex=2,xlab="x",ylab="y",cex.lab=1.5,data=anscombe)
abline(fit2)


###################################################
### code chunk number 33: l04.Rnw:952-954
###################################################
plot(y3~x3,cex.lab=2,pch=20,cex=2,xlab="x",ylab="y",cex.lab=1.5,data=anscombe)
abline(fit3)


###################################################
### code chunk number 34: l04.Rnw:957-959
###################################################
plot(y4~x4,cex.lab=2,pch=20,cex=2,xlab="x",ylab="y",cex.lab=1.5,data=anscombe)
abline(fit4)


###################################################
### code chunk number 35: l04.Rnw:968-969
###################################################
plot(residuals(fit1)~x1,cex.lab=2,pch=20,cex=2,xlab="x",ylab="y",data=anscombe)


###################################################
### code chunk number 36: l04.Rnw:972-973
###################################################
plot(residuals(fit2)~x2,cex.lab=2,pch=20,cex=2,xlab="x",ylab="y",data=anscombe)


###################################################
### code chunk number 37: l04.Rnw:976-977
###################################################
plot(residuals(fit3)~x3,cex.lab=2,pch=20,cex=2,xlab="x",ylab="y",data=anscombe)


###################################################
### code chunk number 38: l04.Rnw:980-981
###################################################
plot(residuals(fit4)~x4,cex.lab=2,pch=20,cex=2,xlab="x",ylab="y",data=anscombe)


###################################################
### code chunk number 39: l04.Rnw:1013-1014
###################################################
plot(e~x,xlab="Mass (carats)")


###################################################
### code chunk number 40: l04.Rnw:1053-1056
###################################################
artic<-read.table("data/N_09_area_v2_data.txt",header = TRUE)
attach(artic)
  plot(year,extent,ylab="1,000,000 km",main="Evolution of sea ice extent",pch=20,cex.lab=1.5)


###################################################
### code chunk number 41: l04.Rnw:1078-1082
###################################################
plot(year,extent,ylab="1,000,000 km",
main="Evolution of sea ice extent",pch=20,cex.lab=1.5)
fit<-lm(extent~year)
abline(fit,lwd=2,col="red")


###################################################
### code chunk number 42: l04.Rnw:1093-1107
###################################################
betahat<-NULL
period<-2001:2016
y<-extent
x<-year
for (i in 2001:2016) {
sel<- x <= i
fit.tmp<-lm(y~x,subset = sel)
betahat<-c(betahat,coefficients(fit.tmp)[2])
}
plot(period,betahat,ylab = expression(hat(beta)),xlab="year",cex.lab=1.5)
a<-data.frame(starting.year= 1979,final.year=period, slope=betahat)
xa<-xtable::xtable(a)
xtable::digits(xa)<-c(0,0,0,3)
print(xa,include.rownames = FALSE)


###################################################
### code chunk number 43: l04.Rnw:1114-1118
###################################################
res<-resid(fit)
plot(x,res,ylab="Residuals",
pch=20,cex.lab=1.2,xlab="Year",cex.lab=1.5)
lines(x,predict(lm(res~x+I(x^2))),lty=2,lwd=2)


###################################################
### code chunk number 44: l04.Rnw:1160-1165
###################################################
ad<-read.csv(file='data/Advertising.csv')
attach(ad)
plot(TV, sales, cex.lab=2, cex.axis=1.2,pch=20,col="grey",cex=2)
fit<-lm(sales~TV)
abline(fit,col="red")


###################################################
### code chunk number 45: l04.Rnw:1168-1169
###################################################
plot(radio,resid(fit),cex.lab=2,cex.axis=1.2,pch=20,col="grey",cex=2, ylab="residuals")


###################################################
### code chunk number 46: l04.Rnw:1172-1173
###################################################
plot(newspaper,resid(fit),cex.lab=2,cex.axis=1.2,pch=20,col="grey",cex=2, ylab="residuals")


###################################################
### code chunk number 47: l04.Rnw:1188-1191
###################################################
fit.cars<-lm(dist~speed,data=cars)
plot(dist~speed,data=cars)
abline(fit.cars)


###################################################
### code chunk number 48: l04.Rnw:1194-1195
###################################################
plot(resid(fit.cars)~speed,data=cars,ylab="residuals")


###################################################
### code chunk number 49: l04.Rnw:1204-1209
###################################################
ds<-cars$dist/cars$speed
invspeed<-1/cars$speed
fit.cars2<-lm(ds~invspeed)
plot(ds~invspeed,xlab="1/speed",ylab="dist/speed")
abline(fit.cars2)


###################################################
### code chunk number 50: l04.Rnw:1212-1213
###################################################
plot(resid(fit.cars2)~invspeed,xlab="1/speed",ylab="residuals")


### R code from vignette source 'l05.Rnw'

###################################################
### code chunk number 1: l05.Rnw:6-7
###################################################
options(continue = "   ", prompt = " ")


###################################################
### code chunk number 2: l05.Rnw:30-31
###################################################
curve(dnorm(x), from=-3, to=3, xlab=expression(epsilon), ylab="", ylim=c(0,1))


###################################################
### code chunk number 3: l05.Rnw:34-36
###################################################
curve(exp(-abs(x))/2, from=-3, to=3, xlab=expression(epsilon), ylab="",
ylim=c(0,1))


###################################################
### code chunk number 4: l05.Rnw:39-41
###################################################
curve(sqrt(pmax(0,1-x^2))/(pi/2), from=-3, to=3, xlab=expression(epsilon),
ylab="", ylim=c(0,1))


###################################################
### code chunk number 5: l05.Rnw:44-45
###################################################
curve(dt(x,3), from=-3, to=3, xlab=expression(epsilon), ylab="", ylim=c(0,1))


###################################################
### code chunk number 6: l05.Rnw:48-50
###################################################
curve(dgamma(x+1.5, shape=1.5, scale=1), from=-3, to=3,
xlab=expression(epsilon), ylab="", ylim=c(0,1))


###################################################
### code chunk number 7: l05.Rnw:53-56
###################################################
curve(0.5*dgamma(x+1.5, shape=1.5, scale=1) +
0.5*dgamma(0.5-x, shape=0.5, scale=1), from=-3,
to=3, xlab=expression(epsilon), ylab="", ylim=c(0,1))


###################################################
### code chunk number 8: l05.Rnw:219-233
###################################################
# define grid of x values
x = seq(-5, 5, length = 100)

# plot curve for standard Gaussian
plot(x, dnorm(x), type = "l", lty = 1, lwd = 2,
xlab = "x", ylab = "Density", main = "Gaussian vs t distribution",xlim=c(-5,5))
# add curves for t distributions
lines(x, dt(x, df = 1), lty = 1, lwd = 2, col = "darkorange")
lines(x, dt(x, df = 10), lty = 1, lwd = 2, col = "dodgerblue")

# add legend
legend("topright", title = "Distributions",
legend = c("t, df = 1", "t, df = 10", "N(0,1)"), 
lwd = 2, lty = c(1, 1, 1), col = c("darkorange", "dodgerblue", "black"))


### R code from vignette source 'l06.Rnw'

###################################################
### code chunk number 1: l06.Rnw:5-6
###################################################
options(continue = "   ", prompt = " ")


###################################################
### code chunk number 2: l06.Rnw:49-51
###################################################
fit<- lm(dist ~ speed, data = cars)
coef(fit)


###################################################
### code chunk number 3: l06.Rnw:54-55
###################################################
confint(fit, level = 0.99)


###################################################
### code chunk number 4: l06.Rnw:97-100
###################################################
new_speeds <- data.frame(speed = c(5, 21))
predict(fit, newdata = new_speeds, 
interval = "confidence", level = 0.99)


###################################################
### code chunk number 5: l06.Rnw:150-152
###################################################
predict(fit, newdata = new_speeds, 
interval = "prediction", level = 0.99)


###################################################
### code chunk number 6: l06.Rnw:160-174
###################################################
new_speeds <-data.frame(speed = seq(min(cars$speed),
                        max(cars$speed),l=50))
conf<-predict(fit,newdata = new_speeds,
                 interval = "confidence", level = 0.99)
pred<-predict(fit,newdata = new_speeds,
                 interval = "prediction", level = 0.99)
ylim<-range(range(pred),range(cars$dist))
plot(dist~speed,data=cars,ylim=ylim,
     col="gray",pch=20,cex=2,cex.lab=1.3)
lines(new_speeds$speed,conf[,1],lwd=2)
lines(new_speeds$speed,conf[,2],lwd=2,lty=2)
lines(new_speeds$speed,conf[,3],lwd=2,lty=2)
lines(new_speeds$speed,pred[,2],lwd=2,lty=2,col=2)
lines(new_speeds$speed,pred[,3],lwd=2,lty=2,col=2)


###################################################
### code chunk number 7: l06.Rnw:220-221
###################################################
summary(fit)$coefficients


###################################################
### code chunk number 8: l06.Rnw:225-226
###################################################
summary(fit)$coefficients[2,]


###################################################
### code chunk number 9: l06.Rnw:280-285
###################################################
set.seed(42)
x = seq(-1, 1, 0.01)
y = 5 + 4 * x ^ 2 + rnorm(length(x), 0, 0.5)
plot(x, y, pch  = 20, cex  = 2, col  = "grey")
abline(lm(y ~ x), lwd = 3, col = "darkorange")


###################################################
### code chunk number 10: l06.Rnw:300-305
###################################################
set.seed(42)
x = seq(-1, 1, 0.01)
y = 5 + (2 *  x + 1.3)^ 2 + rnorm(length(x), 0, 0.75)
plot(x, y, pch  = 20, cex  = 2, col  = "grey")
abline(lm(y ~ x), lwd = 3, col = "darkorange")


### R code from vignette source 'l07.Rnw'

###################################################
### code chunk number 1: l07.Rnw:6-7
###################################################
options(continue = "   ", prompt = " ")


###################################################
### code chunk number 2: l07.Rnw:24-42
###################################################
# read the data 
autompg = read.table(
    "http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",
    quote = "\"",
    comment.char = "",
    stringsAsFactors = FALSE)
# give the dataframe headers
colnames(autompg) <- c("mpg", "cyl", "disp", "hp", "wt", "acc", "year", "origin", "name")
# remove missing data, which is stored as "?"
autompg <-subset(autompg, autompg$hp != "?")
# remove the plymouth reliant, as it causes some issues
autompg <- subset(autompg, autompg$name != "plymouth reliant")
# give the dataset row names, based on the engine, year and name
rownames(autompg) <- paste(autompg$cyl, "cylinder", autompg$year, autompg$name)
# remove the variable for name, as well as origin
autompg <- subset(autompg, select = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year"))
# change horsepower from character to numeric
autompg$hp <- as.numeric(autompg$hp)


###################################################
### code chunk number 3: l07.Rnw:50-52 (eval = FALSE)
###################################################
## autompg <- read.table("autompg.txt",header=TRUE)
## names(autompg)


###################################################
### code chunk number 4: l07.Rnw:54-55
###################################################
names(autompg)


###################################################
### code chunk number 5: l07.Rnw:76-99
###################################################
library("plot3D")

x = autompg$wt
y = autompg$year
z = autompg$mpg

fit <- lm(z ~ x + y)

grid.lines = 25
x.pred     = seq(min(x), max(x), length.out = grid.lines)
y.pred     = seq(min(y), max(y), length.out = grid.lines)
xy         = expand.grid(x = x.pred, y = y.pred)

z.pred = matrix(predict(fit, newdata = xy), 
nrow = grid.lines, ncol = grid.lines)

fitpoints = predict(fit)

scatter3D(x, y, z, pch = 19, cex = 2, col = gg.col(1000), lighting = TRUE,
theta = 25, phi = 45, ticktype = "detailed",
xlab = "wt", ylab = "year", zlab = "mpg", zlim = c(0, 40), clim = c(0, 40),
surf = list(x = x.pred, y = y.pred, z = z.pred,  
facets = NA, fit = fitpoints), main = "")


###################################################
### code chunk number 6: l07.Rnw:137-139
###################################################
mpg.fit <- lm(mpg ~ wt + year, data = autompg)
coef(mpg.fit)


###################################################
### code chunk number 7: l07.Rnw:272-279
###################################################
n <- nrow(autompg)
p <- length(coef(mpg.fit))
X <- cbind(rep(1, n), autompg$wt, autompg$year)
y <- autompg$mpg
beta.hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta.hat
coef(mpg.fit)


###################################################
### code chunk number 8: l07.Rnw:287-288
###################################################
head(model.matrix(fit))


###################################################
### code chunk number 9: l07.Rnw:292-293
###################################################
head(X)


###################################################
### code chunk number 10: l07.Rnw:343-344
###################################################
summary(mpg.fit)$sigma


###################################################
### code chunk number 11: l07.Rnw:349-353
###################################################
y_hat = X %*% solve(t(X) %*% X) %*% t(X) %*% y
e     = y - y_hat
sqrt(t(e) %*% e / (n - p))
sqrt(sum((y - y_hat) ^ 2) / (n - p))


###################################################
### code chunk number 12: l07.Rnw:493-499
###################################################
C <- solve(t(X) %*% X) 
s<-sqrt(sum((y - y_hat) ^ 2) / (n - p))
std.err<-s*sqrt(diag(C))
std.err
t(summary(mpg.fit)$coef[,2])
sqrt(diag(vcov(mpg.fit)))


###################################################
### code chunk number 13: l07.Rnw:554-555
###################################################
summary(mpg.fit)$coef


###################################################
### code chunk number 14: l07.Rnw:593-594
###################################################
confint(mpg.fit, level = 0.99)


###################################################
### code chunk number 15: l07.Rnw:650-652
###################################################
new.cars <- data.frame(wt = c(3500, 5000),
 year = c(76, 81)); new.cars


###################################################
### code chunk number 16: l07.Rnw:657-659
###################################################
(cint <- predict(mpg.fit, newdata = new.cars, 
interval = "confidence", level = 0.99))


###################################################
### code chunk number 17: l07.Rnw:665-666
###################################################
cint[,3] - cint[,2]


###################################################
### code chunk number 18: l07.Rnw:670-671
###################################################
new.cars$wt; range(autompg$wt)


###################################################
### code chunk number 19: l07.Rnw:674-675
###################################################
new.cars$year; range(autompg$year)


###################################################
### code chunk number 20: l07.Rnw:681-683
###################################################
plot(year ~ wt, data = autompg, pch = 20, col = "dodgerblue", cex = 1.5)
points(new.cars, col = "darkorange", cex = 3, pch = "X")


###################################################
### code chunk number 21: l07.Rnw:690-692
###################################################
x0 <- c(1, 3500, 76)
x0 %*% beta.hat


###################################################
### code chunk number 22: l07.Rnw:730-732
###################################################
x0 <- c(0, 0, 1)
x0 %*% beta.hat


###################################################
### code chunk number 23: l07.Rnw:777-779
###################################################
predict(mpg.fit, newdata = new.cars, 
     interval = "prediction", level = 0.99)


### R code from vignette source 'l08.Rnw'

###################################################
### code chunk number 1: l08.Rnw:6-7
###################################################
options(continue = "   ", prompt = " ",scipen = 10)


###################################################
### code chunk number 2: l08.Rnw:21-40
###################################################
# read the data 
autompg = read.table(
    "http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",
    quote = "\"",
    comment.char = "",
    stringsAsFactors = FALSE)
# give the dataframe headers
colnames(autompg) <- c("mpg", "cyl", "disp", "hp", "wt", "acc", "year", "origin", "name")
# remove missing data, which is stored as "?"
autompg <-subset(autompg, autompg$hp != "?")
# remove the plymouth reliant, as it causes some issues
autompg <- subset(autompg, autompg$name != "plymouth reliant")
# give the dataset row names, based on the engine, year and name
rownames(autompg) <- paste(autompg$cyl, "cylinder", autompg$year, autompg$name)
# remove the variable for name, as well as origin
autompg <- subset(autompg, select = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year"))
# change horsepower from character to numeric
autompg$hp <- as.numeric(autompg$hp)
mpg.fit <- lm(mpg ~ wt + year, data = autompg)


###################################################
### code chunk number 3: l08.Rnw:75-76
###################################################
summary(mpg.fit)$r.squared


###################################################
### code chunk number 4: l08.Rnw:136-148
###################################################
lwd<-3 ; from<-0; to<-5
df1<-1; df2<-1
curve(df(x,df1=df1,df2=df2),from=from,to=to,lwd=lwd,ylab="density",xlab="t",ylim=c(0,2))
df1<-2; df2<-1
curve(df(x,df1=df1,df2=df2),from=from,to=to,lwd=lwd,ylab="density",xlab="t",add=TRUE,col=2)
df1<-5; df2<-2
curve(df(x,df1=df1,df2=df2),from=from,to=to,lwd=lwd,ylab="density",xlab="t",add=TRUE,col=3)
df1<-10; df2<-1
curve(df(x,df1=df1,df2=df2),from=from,to=to,lwd=lwd,ylab="density",xlab="t",add=TRUE,col=4)
df1<-100; df2<-100
curve(df(x,df1=df1,df2=df2),from=from,to=to,lwd=lwd,ylab="density",xlab="t",add=TRUE,col=5)
legend("topright",legend=c("a=1 b=1","a=2 b=1","a=5 b=2","a=10 b=1","a=100 b=100"),col=1:5,lty=1,lwd=lwd)


###################################################
### code chunk number 5: l08.Rnw:196-199
###################################################
null_mpg.fit = lm(mpg ~ 1, data = autompg)
full_mpg.fit = lm(mpg ~ wt + year, data = autompg)
anova(null_mpg.fit, full_mpg.fit)


###################################################
### code chunk number 6: l08.Rnw:207-209
###################################################
mpg.fit <- lm(mpg ~ wt + year, data = autompg)
summary(mpg.fit)


###################################################
### code chunk number 7: l08.Rnw:216-226
###################################################
sum((fitted(full_mpg.fit) - fitted(null_mpg.fit)) ^ 2) # SSreg
sum(resid(full_mpg.fit) ^ 2)# SSres
sum(resid(null_mpg.fit) ^ 2)# SStot
# 
# Degrees of Freedom: Regression
length(coef(full_mpg.fit)) - length(coef(null_mpg.fit))
# Degrees of Freedom: Error
length(resid(full_mpg.fit)) - length(coef(full_mpg.fit))
# Degrees of Freedom: Total
length(resid(null_mpg.fit)) - length(coef(null_mpg.fit))


###################################################
### code chunk number 8: l08.Rnw:318-319
###################################################
names(autompg)


###################################################
### code chunk number 9: l08.Rnw:335-339
###################################################
null_mpg.fit <- lm(mpg ~ wt + year, data = autompg)
full_mpg.fit <- lm(mpg ~ wt + year + cyl 
                 + disp + hp + acc, data = autompg)
anova(null_mpg.fit, full_mpg.fit)


###################################################
### code chunk number 10: l08.Rnw:347-358
###################################################
(ssdiff <- sum((fitted(full_mpg.fit) - fitted(null_mpg.fit)) ^ 2)) # SSdiff
sum(resid(full_mpg.fit) ^ 2) # SSR (For Full)
sum(resid(null_mpg.fit) ^ 2) # SSR (For Null)
# Degrees of Freedom: Diff
(dfdiff <- length(coef(full_mpg.fit)) - length(coef(null_mpg.fit)))
# Degrees of Freedom: Full
length(resid(full_mpg.fit)) - length(coef(full_mpg.fit))
# Degrees of Freedom: Null
length(resid(null_mpg.fit)) - length(coef(null_mpg.fit))
# F value
(ssdiff/dfdiff) / (sum(resid(full_mpg.fit) ^ 2)/full_mpg.fit$df.resid)


###################################################
### code chunk number 11: l08.Rnw:371-373
###################################################
summary(full_mpg.fit)$ r.squared
summary(null_mpg.fit)$ r.squared


###################################################
### code chunk number 12: l08.Rnw:378-380
###################################################
useless_covariates <- matrix(rnorm(390*389), ncol = 389)
summary(lm(autompg$mpg ~ useless_covariates))$ r.squared


###################################################
### code chunk number 13: l08.Rnw:386-387
###################################################
summary(lm(autompg$mpg ~ useless_covariates))$coef[1:3,]


###################################################
### code chunk number 14: l08.Rnw:452-462
###################################################
# Two models with different predictors
mod1 <- lm(mpg ~ wt + year, data = autompg) 
mod2 <- lm(mpg ~ wt + year+cyl, data = autompg) 
# AICs
c(AIC(mod1) , AIC(mod2) )
# BICs 
c(BIC(mod1) , BIC(mod2) )
## adding cyl doesn't pay off
# BIC can also be derived as - see ?AIC
AIC(mod1, k=log(nrow(autompg)))


###################################################
### code chunk number 15: l08.Rnw:465-466
###################################################
summary(mod2)


###################################################
### code chunk number 16: l08.Rnw:485-487
###################################################
summary(mod1)$adj.r.squared
summary(mod2)$adj.r.squared


###################################################
### code chunk number 17: l08.Rnw:540-544
###################################################
null<-lm(mpg ~ 1,data = autompg) 
full<-lm(mpg ~ .,data = autompg) 
step(null, scope=list(lower=null, upper=full), direction="forward",
 k=2, trace=1)


###################################################
### code chunk number 18: l08.Rnw:550-552
###################################################
step(full, scope=list(lower=null, upper=full),
   direction="backward", k=2, trace=1)


###################################################
### code chunk number 19: l08.Rnw:564-566
###################################################
step(null, scope = list(upper=full), 
    direction="both",trace=1,k=2)


###################################################
### code chunk number 20: l08.Rnw:590-591 (eval = FALSE)
###################################################
## install.packages("leaps")


###################################################
### code chunk number 21: l08.Rnw:594-596
###################################################
library(leaps)
all.mod <-summary(regsubsets(mpg ~ ., data = autompg))


###################################################
### code chunk number 22: l08.Rnw:599-600
###################################################
all.mod$which


###################################################
### code chunk number 23: l08.Rnw:607-610
###################################################
all.mod$rss
all.mod$bic
all.mod$adjr2


###################################################
### code chunk number 24: l08.Rnw:615-617
###################################################
best.BIC<- which.max(all.mod$bic) 
all.mod$which[best.BIC, ]


###################################################
### code chunk number 25: l08.Rnw:631-638
###################################################
make_poly_data <- function(sample_size = 11) {
x <- seq(0, 10)
y <- 3 + x + 4 * x ^ 2 + rnorm(n = sample_size, mean = 0, sd = 20)
data.frame(x, y)
}
set.seed(1234)
poly_data <- make_poly_data()


###################################################
### code chunk number 26: l08.Rnw:654-662
###################################################
fit_quad <- lm(y ~ poly(x, degree = 2), data = poly_data)
fit_big  <- lm(y ~ poly(x, degree = 8), data = poly_data)
plot(y ~ x, data = poly_data, ylim = c(-100, 400), cex = 2, pch = 20)
xplot = seq(0, 10, by = 0.1)
lines(xplot, predict(fit_quad, newdata = data.frame(x = xplot)),
col = "dodgerblue", lwd = 2, lty = 1)
lines(xplot, predict(fit_big, newdata = data.frame(x = xplot)),
col = "darkorange", lwd = 2, lty = 2)


###################################################
### code chunk number 27: l08.Rnw:669-671 (eval = FALSE)
###################################################
## sqrt(mean(resid(fit_quad) ^ 2))
## sqrt(mean(resid(fit_big) ^ 2))


###################################################
### code chunk number 28: l08.Rnw:693-701
###################################################
fit_quad_removed = lm(y ~ poly(x, degree = 2), data = poly_data[-3, ])
fit_big_removed  = lm(y ~ poly(x, degree = 8), data = poly_data[-3, ])
plot(y ~ x, data = poly_data, ylim = c(-100, 400), cex = 2, pch = 20)
xplot = seq(0, 10, by = 0.1)
lines(xplot, predict(fit_quad_removed, newdata = data.frame(x = xplot)),
col = "dodgerblue", lwd = 2, lty = 1)
lines(xplot, predict(fit_big_removed, newdata = data.frame(x = xplot)),
col = "darkorange", lwd = 2, lty = 2)


###################################################
### code chunk number 29: l08.Rnw:728-731
###################################################
calc_loocv_rmse = function(model) {
sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}


###################################################
### code chunk number 30: l08.Rnw:736-738
###################################################
calc_loocv_rmse(fit_quad)
calc_loocv_rmse(fit_big)


### R code from vignette source 'l09.Rnw'

###################################################
### code chunk number 1: l09.Rnw:7-8
###################################################
options(continue = "   ", prompt = " ",scipen = 10)


###################################################
### code chunk number 2: l09.Rnw:28-29
###################################################
head(mtcars)


###################################################
### code chunk number 3: l09.Rnw:39-44
###################################################
plot(mpg ~ hp, data = mtcars, col = am + 1, 
     pch = am + 18, cex = 2,bty="l")
legend("topright", 
       c("Automatic", "Manual"), 
       col = c(1, 2), pch = c(18, 19))


###################################################
### code chunk number 4: l09.Rnw:51-52
###################################################
mpg_hp_slr <-lm(mpg ~ hp, data = mtcars)


###################################################
### code chunk number 5: l09.Rnw:56-62
###################################################
plot(mpg ~ hp, data = mtcars, col = am + 1, 
     pch = am + 18, cex = 2,bty="l")
legend("topright", 
       c("Automatic", "Manual"), 
       col = c(1, 2), pch = c(18, 19))
abline(mpg_hp_slr, lwd = 3, col = "grey")


###################################################
### code chunk number 6: l09.Rnw:111-112
###################################################
mpg_hp_add <- lm(mpg ~ hp + am, data = mtcars)


###################################################
### code chunk number 7: l09.Rnw:116-126
###################################################
plot(mpg ~ hp, data = mtcars, col = am + 1, 
     pch = am + 18, cex = 2,bty="l")
legend("topright", c("Automatic", "Manual"), 
       col = c(1, 2), pch = c(18, 19),bty="n")
int_auto <- coef(mpg_hp_add)[1]
int_manu <- coef(mpg_hp_add)[1] + coef(mpg_hp_add)[3]
slope_auto <- coef(mpg_hp_add)[2]
slope_manu <- coef(mpg_hp_add)[2]
abline(int_auto, slope_auto, col = 1, lty = 1, lwd = 2) 
abline(int_manu, slope_manu, col = 2, lty = 2, lwd = 2) 


###################################################
### code chunk number 8: l09.Rnw:137-138
###################################################
summary(mpg_hp_add)$coefficients["am",]


###################################################
### code chunk number 9: l09.Rnw:143-144
###################################################
anova(mpg_hp_slr, mpg_hp_add)


###################################################
### code chunk number 10: l09.Rnw:157-173
###################################################
dl <-"http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data" 
autompg = read.table(dl, quote = "\"", comment.char = "",
    stringsAsFactors = FALSE)
colnames(autompg) <- c("mpg", "cyl", "disp", 
		"hp", "wt", "acc", "year", "origin", "name")
autompg <- subset(autompg, autompg$hp != "?")
autompg <- subset(autompg, autompg$name != "plymouth reliant")
rownames(autompg) <- paste(autompg$cyl, "cylinder", 
			autompg$year, autompg$name)
autompg <- subset(autompg, 
  select = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year", "origin"))
autompg$domestic <- as.numeric(autompg$origin == 1)
autompg$hp <- as.numeric(autompg$hp)
autompg <- autompg[autompg$cyl != 5,]
autompg <- autompg[autompg$cyl != 3,]
autompg$cyl <- as.factor(autompg$cyl)


###################################################
### code chunk number 11: l09.Rnw:197-200
###################################################
mpg_disp_add <- lm(mpg ~ disp
             + domestic , data = autompg) 
summary(mpg_disp_add)             


###################################################
### code chunk number 12: l09.Rnw:226-229 (eval = FALSE)
###################################################
## autompg$x3 <- autompg$disp * autompg$domestic
## do_not_do_this <- lm(mpg ~ disp + domestic + x3, 
##                  data = autompg) 


###################################################
### code chunk number 13: l09.Rnw:233-235
###################################################
mpg_disp_int <- lm(mpg ~ disp + domestic + disp:domestic, 
                data = autompg)


###################################################
### code chunk number 14: l09.Rnw:240-241
###################################################
mpg_disp_int2 <-lm(mpg ~ disp * domestic, data = autompg)


###################################################
### code chunk number 15: l09.Rnw:247-248
###################################################
summary(mpg_disp_int)


###################################################
### code chunk number 16: l09.Rnw:258-259
###################################################
summary(mpg_disp_int)$coefficients["disp:domestic",]


###################################################
### code chunk number 17: l09.Rnw:266-267
###################################################
anova(mpg_disp_add, mpg_disp_int)


###################################################
### code chunk number 18: l09.Rnw:283-284
###################################################
is.factor(autompg$domestic)


###################################################
### code chunk number 19: l09.Rnw:288-292
###################################################
autompg$origin[autompg$domestic == 1] = "domestic"
autompg$origin[autompg$domestic == 0] = "foreign"
autompg$origin[1:4]
is.factor(autompg$origin)


###################################################
### code chunk number 20: l09.Rnw:298-299
###################################################
autompg$origin <- as.factor(autompg$origin)


###################################################
### code chunk number 21: l09.Rnw:304-306
###################################################
autompg$origin[1:4]
levels(autompg$origin)


###################################################
### code chunk number 22: l09.Rnw:311-313
###################################################
old<-options()$width
options(width=40)


###################################################
### code chunk number 23: l09.Rnw:316-317
###################################################
mod1<-lm(mpg ~ disp * domestic, data = autompg)


###################################################
### code chunk number 24: l09.Rnw:322-323
###################################################
mod2<-lm(mpg ~ disp * origin, data = autompg)


###################################################
### code chunk number 25: l09.Rnw:326-328
###################################################
summary(mod1)$r.squared
summary(mod2)$r.squared


###################################################
### code chunk number 26: l09.Rnw:333-335
###################################################
mod1
mod2


###################################################
### code chunk number 27: l09.Rnw:338-339
###################################################
options(width=old)


###################################################
### code chunk number 28: l09.Rnw:370-372
###################################################
is.factor(autompg$cyl)
levels(autompg$cyl)


###################################################
### code chunk number 29: l09.Rnw:425-428
###################################################
mpg_disp_add_cyl <- lm(mpg ~ disp + cyl, 
                    data = autompg)
mpg_disp_add_cyl


###################################################
### code chunk number 30: l09.Rnw:445-458
###################################################
	int_4cyl = coef(mpg_disp_add_cyl)[1]
	int_6cyl = coef(mpg_disp_add_cyl)[1] + coef(mpg_disp_add_cyl)[3]
	int_8cyl = coef(mpg_disp_add_cyl)[1] + coef(mpg_disp_add_cyl)[4]
	
	slope_all_cyl = coef(mpg_disp_add_cyl)[2]
	
	plot_colors = c("Darkorange", "Darkgrey", "Dodgerblue")
	plot(mpg ~ disp, data = autompg, col = plot_colors[cyl], pch = as.numeric(cyl))
	abline(int_4cyl, slope_all_cyl, col = plot_colors[1], lty = 1, lwd = 2)
	abline(int_6cyl, slope_all_cyl, col = plot_colors[2], lty = 2, lwd = 2)
	abline(int_8cyl, slope_all_cyl, col = plot_colors[3], lty = 3, lwd = 2)
	legend("topright", c("4 Cylinder", "6 Cylinder", "8 Cylinder"),
	col = plot_colors, lty = c(1, 2, 3), pch = c(1, 2, 3))


###################################################
### code chunk number 31: l09.Rnw:466-468
###################################################
mpg_disp_int_cyl <- lm(mpg ~ disp * cyl, data = autompg)
mpg_disp_int_cyl


###################################################
### code chunk number 32: l09.Rnw:497-512
###################################################
int_4cyl = coef(mpg_disp_int_cyl)[1]
int_6cyl = coef(mpg_disp_int_cyl)[1] + coef(mpg_disp_int_cyl)[3]
int_8cyl = coef(mpg_disp_int_cyl)[1] + coef(mpg_disp_int_cyl)[4]

slope_4cyl = coef(mpg_disp_int_cyl)[2]
slope_6cyl = coef(mpg_disp_int_cyl)[2] + coef(mpg_disp_int_cyl)[5]
slope_8cyl = coef(mpg_disp_int_cyl)[2] + coef(mpg_disp_int_cyl)[6]

plot_colors = c("Darkorange", "Darkgrey", "Dodgerblue")
plot(mpg ~ disp, data = autompg, col = plot_colors[cyl], pch = as.numeric(cyl))
abline(int_4cyl, slope_4cyl, col = plot_colors[1], lty = 1, lwd = 2)
abline(int_6cyl, slope_6cyl, col = plot_colors[2], lty = 2, lwd = 2)
abline(int_8cyl, slope_8cyl, col = plot_colors[3], lty = 3, lwd = 2)
legend("topright", c("4 Cylinder", "6 Cylinder", "8 Cylinder"),
col = plot_colors, lty = c(1, 2, 3), pch = c(1, 2, 3))


###################################################
### code chunk number 33: l09.Rnw:529-530
###################################################
anova(mpg_disp_add_cyl, mpg_disp_int_cyl)


### R code from vignette source 'l10.Rnw'

###################################################
### code chunk number 1: l10.Rnw:7-8
###################################################
options(continue = "   ", prompt = " ")


###################################################
### code chunk number 2: l10.Rnw:27-36
###################################################
sim_initech = function(sample_size = 50) {
x <- round(seq(1, 25, length.out = sample_size))
log_y = 10.5 + 0.08 * x + rnorm(n = sample_size, sd = 0.20)
y = round(exp(log_y))
data.frame(years = x, salary = y)
}
set.seed(420)
initech <- sim_initech(sample_size = 100)
write.csv(initech, "data/initech.csv", row.names = FALSE)


###################################################
### code chunk number 3: l10.Rnw:39-40
###################################################
initech <- read.csv("data/initech.csv")


###################################################
### code chunk number 4: l10.Rnw:43-44 (eval = FALSE)
###################################################
## initech <- read.csv("initech.csv")


###################################################
### code chunk number 5: l10.Rnw:47-50
###################################################
plot(salary ~ years, data = initech, col = "grey", 
     pch = 20, cex = 1.5,
     main = "Salaries at Initech by Seniority")


###################################################
### code chunk number 6: l10.Rnw:56-58
###################################################
initech_fit <- lm(salary ~ years, data = initech)
summary(initech_fit)


###################################################
### code chunk number 7: l10.Rnw:64-67
###################################################
plot(salary ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,
main = "Salaries at Initech by Seniority")
abline(initech_fit, col = "darkorange", lwd = 2)


###################################################
### code chunk number 8: l10.Rnw:74-80
###################################################
#par(mfrow = c(1, 2))
plot(fitted(initech_fit), resid(initech_fit), col = "grey", pch = 20,
xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
#qqnorm(resid(initech_fit), main = "Normal Q-Q Plot", col = "darkgrey")
#qqline(resid(initech_fit), col = "dodgerblue", lwd = 2)


###################################################
### code chunk number 9: l10.Rnw:134-135
###################################################
initech_fit_log <- lm(log(salary) ~ years, data = initech)


###################################################
### code chunk number 10: l10.Rnw:139-142
###################################################
plot(log(salary) ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,
main = "Salaries at Initech, By Seniority")
abline(initech_fit_log, col = "darkorange", lwd = 2)


###################################################
### code chunk number 11: l10.Rnw:149-153
###################################################
plot(salary ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,
main = "Salaries at Initech, By Seniority")
curve(exp(initech_fit_log$coef[1] + initech_fit_log$coef[2] * x),
from = 0, to = 30, add = TRUE, col = "darkorange", lwd = 2)


###################################################
### code chunk number 12: l10.Rnw:162-165
###################################################
plot(fitted(initech_fit_log), resid(initech_fit_log), col = "grey", pch = 20,
xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)


###################################################
### code chunk number 13: l10.Rnw:173-175
###################################################
sqrt(mean(resid(initech_fit) ^ 2))
sqrt(mean(resid(initech_fit_log) ^ 2))


###################################################
### code chunk number 14: l10.Rnw:180-182
###################################################
sqrt(mean((initech$salary - fitted(initech_fit)) ^ 2))
sqrt(mean((initech$salary - exp(fitted(initech_fit_log))) ^ 2))


###################################################
### code chunk number 15: l10.Rnw:229-230
###################################################
library(MASS)


###################################################
### code chunk number 16: l10.Rnw:241-242 (eval = FALSE)
###################################################
## boxcox(initech_fit, plotit = TRUE)


###################################################
### code chunk number 17: l10.Rnw:246-249
###################################################
a<-boxcox(initech_fit, plotit = TRUE)
abline(v=0,col="blue",lwd=2)
lambda.hat<-a$x[which.max(a$y)]


###################################################
### code chunk number 18: l10.Rnw:283-309
###################################################
# read data frame from the web
autompg = read.table("data/auto-mpg.data",
quote = "\"",
comment.char = "",
stringsAsFactors = FALSE)
# give the dataframe headers
colnames(autompg) = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year", "origin", "name")
# remove missing data, which is stored as "?"
autompg = subset(autompg, autompg$hp != "?")
# remove the plymouth reliant, as it causes some issues
autompg = subset(autompg, autompg$name != "plymouth reliant")
# give the dataset row names, based on the engine, year and name
rownames(autompg) = paste(autompg$cyl, "cylinder", autompg$year, autompg$name)
# remove the variable for name
autompg = subset(autompg, select = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year", "origin"))
# change horsepower from character to numeric
autompg$hp = as.numeric(autompg$hp)
# create a dummary variable for foreign vs domestic cars. domestic = 1.
autompg$domestic = as.numeric(autompg$origin == 1)
# remove 3 and 5 cylinder cars (which are very rare.)
autompg = autompg[autompg$cyl != 5,]
autompg = autompg[autompg$cyl != 3,]
# the following line would verify the remaining cylinder possibilities are 4, 6, 8
#unique(autompg$cyl)
# change cyl to a factor variable
autompg$cyl = as.factor(autompg$cyl)


###################################################
### code chunk number 19: l10.Rnw:319-323
###################################################

plot(mpg ~ hp, data = autompg, col = "dodgerblue", pch = 20, cex = 1.5)
mpg.fit <- lm(mpg ~ hp, data = autompg)
abline(mpg.fit, col = "darkorange", lwd = 2)


###################################################
### code chunk number 20: l10.Rnw:326-329
###################################################
plot(fitted(mpg.fit), resid(mpg.fit), col = "dodgerblue",
pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)


###################################################
### code chunk number 21: l10.Rnw:335-336
###################################################
boxcox(mpg.fit,plotit=TRUE)


###################################################
### code chunk number 22: l10.Rnw:343-346
###################################################
plot(log(mpg) ~ hp, data = autompg, col = "dodgerblue", pch = 20, cex = 1.5)
log.mpg.fit <- lm(log(mpg) ~ hp, data = autompg)
abline(log.mpg.fit, col = "darkorange", lwd = 2)


###################################################
### code chunk number 23: l10.Rnw:349-352
###################################################
plot(fitted(log.mpg.fit), resid(log.mpg.fit), col = "dodgerblue",
pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)


###################################################
### code chunk number 24: l10.Rnw:364-367
###################################################
plot(log(mpg) ~ log(hp), data = autompg, col = "dodgerblue", pch = 20, cex = 1.5)
log.mpg.fit <- lm(log(mpg) ~ log(hp), data = autompg)
abline(log.mpg.fit, col = "darkorange", lwd = 2)


###################################################
### code chunk number 25: l10.Rnw:370-373
###################################################
plot(fitted(log.mpg.fit), resid(log.mpg.fit), col = "dodgerblue",
pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)


###################################################
### code chunk number 26: l10.Rnw:398-409
###################################################
	x <- seq(-2, 5, l = 200)
	plot(x, x, xlab = "x", ylab = "y", type = "l", col = 1, lwd = 2,cex.lab=1.5)
	lines(x, x^2, col = 2, lwd = 2)
	lines(x, x^3, col = 3, lwd = 2)
	lines(x, sqrt(x), col = 4, lwd = 2)
	lines(x, exp(x), col = 5, lwd = 2)
	lines(x, exp(-x), col = 6, lwd = 2)
	lines(x, log(x), col = 7, lwd = 2)
	legend("bottomright", legend = expression(y == x, y == x^2, y == x^3, y == sqrt(x),
	y == exp(x), y == exp(-x), y == log(x)),
	lwd = 2, col = 1:7)


###################################################
### code chunk number 27: l10.Rnw:416-426
###################################################
	plot(x, -x, xlab = "x", ylab = "y", type = "l", col = 1, lwd = 2,cex.lab=1.5)
	lines(x, -x^2, col = 2, lwd = 2)
	lines(x, -x^3, col = 3, lwd = 2)
	lines(x, -sqrt(x), col = 4, lwd = 2)
	lines(x, -exp(x), col = 5, lwd = 2)
	lines(x, -exp(-x), col = 6, lwd = 2)
	lines(x, -log(x), col = 7, lwd = 2)
	legend("topright", legend = expression(y == -x, y == -x^2, y == -x^3, y == -sqrt(x),
	y == -exp(-x), y == -exp(x), y == -log(x)),
	lwd = 2, col = 1:7)


###################################################
### code chunk number 28: l10.Rnw:488-492
###################################################
econ <- read.csv("data/fuel-econ.csv")
plot(mpg ~ mph, data = econ, xlab = "Speed (Miles per Hour)", 
ylab = "Fuel Efficiency (Miles per Gallon)", col = "dodgerblue", 
pch = 20, cex =2)


###################################################
### code chunk number 29: l10.Rnw:496-504
###################################################
plot_econ_curve = function(model){
	plot(mpg ~ mph, data = econ, xlab = "Speed (Miles per Hour)", 
	ylab = "Fuel Efficiency (Miles per Gallon)", col = "dodgerblue", 
	pch = 20, cex =2)
	xplot = seq(10, 75, by = 0.1)
	lines(xplot, predict(model, newdata = data.frame(mph = xplot)),
	col = "darkorange", lwd = 2, lty = 1)
}


###################################################
### code chunk number 30: l10.Rnw:510-513
###################################################
econ <- read.csv("data/fuel-econ.csv")
fit1 <- lm(mpg ~ mph, data = econ)
summary(fit1)


###################################################
### code chunk number 31: l10.Rnw:519-520
###################################################
plot_econ_curve(fit1)


###################################################
### code chunk number 32: l10.Rnw:523-526
###################################################
plot(fitted(fit1), resid(fit1), xlab = "Fitted", ylab = "Residuals", 
col = "dodgerblue", pch = 20, cex =2)
abline(h = 0, col = "darkorange", lwd = 2)


###################################################
### code chunk number 33: l10.Rnw:539-541
###################################################
fit2 <- lm(mpg ~ mph + I(mph ^ 2), data = econ)
summary(fit2)


###################################################
### code chunk number 34: l10.Rnw:546-547
###################################################
plot_econ_curve(fit2)


###################################################
### code chunk number 35: l10.Rnw:550-553
###################################################
plot(fitted(fit2), resid(fit2), xlab = "Fitted", ylab = "Residuals", 
col = "dodgerblue", pch = 20, cex =2)
abline(h = 0, col = "darkorange", lwd = 2)


###################################################
### code chunk number 36: l10.Rnw:560-562
###################################################
fit3 <- lm(mpg ~ mph + I(mph ^ 2) + I(mph ^ 3), data = econ)
summary(fit3)


###################################################
### code chunk number 37: l10.Rnw:567-568
###################################################
plot_econ_curve(fit3)


###################################################
### code chunk number 38: l10.Rnw:571-574
###################################################
plot(fitted(fit3), resid(fit3), xlab = "Fitted", ylab = "Residuals", 
col = "dodgerblue", pch = 20, cex =2)
abline(h = 0, col = "darkorange", lwd = 2)


###################################################
### code chunk number 39: l10.Rnw:583-585
###################################################
fit4 <-lm(mpg ~ mph + I(mph ^ 2) + I(mph ^ 3) + I(mph ^ 4), data = econ)
summary(fit4)


###################################################
### code chunk number 40: l10.Rnw:590-591
###################################################
plot_econ_curve(fit4)


###################################################
### code chunk number 41: l10.Rnw:594-597
###################################################
plot(fitted(fit4), resid(fit4), xlab = "Fitted", ylab = "Residuals", 
col = "dodgerblue", pch = 20, cex =2)
abline(h = 0, col = "darkorange", lwd = 2)


###################################################
### code chunk number 42: l10.Rnw:606-608
###################################################
fit6 <- lm(mpg ~ mph + I(mph ^ 2) + I(mph ^ 3) + I(mph ^ 4) + I(mph ^ 5) + I(mph^6), data = econ)
summary(fit6)


###################################################
### code chunk number 43: l10.Rnw:613-614
###################################################
plot_econ_curve(fit6)


###################################################
### code chunk number 44: l10.Rnw:617-620
###################################################
plot(fitted(fit6), resid(fit6), xlab = "Fitted", ylab = "Residuals", 
col = "dodgerblue", pch = 20, cex =2)
abline(h = 0, col = "darkorange", lwd = 2)


###################################################
### code chunk number 45: l10.Rnw:633-634
###################################################
anova(fit4, fit6)


###################################################
### code chunk number 46: l10.Rnw:641-644
###################################################
fit8 <- lm(mpg ~ mph + I(mph ^ 2) + I(mph ^ 3) + I(mph ^ 4) + I(mph ^ 5)
+ I(mph ^ 6) + I(mph ^ 7) + I(mph ^ 8), data = econ)
anova(fit6, fit8)


###################################################
### code chunk number 47: l10.Rnw:651-653
###################################################
fit6_alt <- lm(mpg ~ poly(mph, 6), data = econ)
all.equal(fitted(fit6), fitted(fit6_alt))


###################################################
### code chunk number 48: l10.Rnw:659-661
###################################################
coef(fit6)
coef(fit6_alt)


###################################################
### code chunk number 49: l10.Rnw:670-672
###################################################
fit6_alt2 <- lm(mpg ~ poly(mph, 6, raw = TRUE), data = econ)
coef(fit6_alt2)


### R code from vignette source 'l11.Rnw'

###################################################
### code chunk number 1: l11.Rnw:6-7
###################################################
options(continue = "   ", prompt = " ")


###################################################
### code chunk number 2: l11.Rnw:22-25
###################################################
library(knitr)
opts_chunk$set(size='small', background='white', cache=TRUE, autodep=TRUE,
options(show.signif.stars=FALSE))


###################################################
### code chunk number 3: l11.Rnw:128-137
###################################################
# Simulation: two independent Gaussians
set.seed(1)
sigma<-15
n<-100
x1 <- rnorm(n, mean=70, sd=sigma)
x2 <- rnorm(n, mean=70, sd=sigma)
# Add in a linear combination of X1 and X2
x3 <- (x1+x2)/2
pairs(cbind(x1,x2,x3))


###################################################
### code chunk number 4: l11.Rnw:142-143
###################################################
round(cor(cbind(x1,x2,x3)),3)


###################################################
### code chunk number 5: l11.Rnw:216-217
###################################################
data("Boston", package = "MASS")


###################################################
### code chunk number 6: l11.Rnw:221-222
###################################################
model1 <- lm(medv~., data = Boston)


###################################################
### code chunk number 7: l11.Rnw:225-228 (eval = FALSE)
###################################################
## install.packages("car")
## library(car)
## vif(model1)


###################################################
### code chunk number 8: l11.Rnw:230-232
###################################################
old<-options()$width
options(width=50)


###################################################
### code chunk number 9: l11.Rnw:235-237
###################################################
library(car)
vif(model1)


###################################################
### code chunk number 10: l11.Rnw:241-242
###################################################
options(width=old)


###################################################
### code chunk number 11: l11.Rnw:484-486
###################################################
A<-matrix(c(1,0.5,0.5,1),2,2)
eigen(A)


###################################################
### code chunk number 12: l11.Rnw:516-526
###################################################
set.seed(1)
# Simulation: two independent Gaussians
n<-100
x1 <- rnorm(n, mean=70, sd=15)
x2 <- rnorm(n, mean=70, sd=15)
# Add in a linear combination of X1 and X2
x3 <- (x1+x2)/2
# Y is a linear combination of the X's plus noise
y <- 0.7*x1 + 0.3*x2 + rnorm(n, mean=0, sd=sqrt(15))
df <- data.frame(x1=x1, x2=x2, x3=x3,  y=y)


###################################################
### code chunk number 13: l11.Rnw:530-532
###################################################
pairs(df)
cor(df)


###################################################
### code chunk number 14: l11.Rnw:539-540
###################################################
XX <- var(df[,c("x1","x2","x3")])*(n-1)


###################################################
### code chunk number 15: l11.Rnw:545-546
###################################################
XX.eigen <- eigen(XX)


###################################################
### code chunk number 16: l11.Rnw:551-552
###################################################
(zero.eigenvals <- which(XX.eigen$values < 1e-10))


###################################################
### code chunk number 17: l11.Rnw:557-558
###################################################
(zero.eigenvectors <- XX.eigen$vectors[,zero.eigenvals])


###################################################
### code chunk number 18: l11.Rnw:563-565
###################################################
zero.eigenvectors
zero.eigenvectors/abs(zero.eigenvectors[1])


### R code from vignette source 'l12.Rnw'

###################################################
### code chunk number 1: l12.Rnw:7-8
###################################################
options(continue = "   ", prompt = " ",scipen = 10)


###################################################
### code chunk number 2: l12.Rnw:22-25
###################################################
library(knitr)
opts_chunk$set(size='small', background='white', cache=TRUE, autodep=TRUE,
options(show.signif.stars=FALSE))


###################################################
### code chunk number 3: l12.Rnw:116-136
###################################################
# Add a circle to an existing plot
# R, bizarrely, does not have any built-in function for this
# Inputs: x coordinate of center; y coordinate of center; radius;
# number of equiangular steps; additional graphical parameters
# Outputs: none
# Side-effects: a circle is added to the existing plot
circle <- function(x0, y0, r, n=1000, ...) {
theta <- seq(from=0, to=2*pi, length.out=n)  # Angles
x <- x0 + r*cos(theta)                       # x coordinates
y <- y0 + r*sin(theta)                       # y coordinates
lines(x,y,...) # Draw the lines connecting all the points, in order
}
plot(0,type="n",xlab=expression(beta[1]),ylab=expression(beta[2]),
xlim=c(-10,10), ylim=c(-10,10))
abline(a=10,b=-2,lwd=2)
points(0,0)
circle(0,0,sqrt(20),col="grey",lwd=2)
points(4,2,col="black",pch=19)
circle(0,0,5,col="grey",lty="dashed",lwd=2)
circle(0,0,6,col="grey",lty="dashed",lwd=2)


###################################################
### code chunk number 4: l12.Rnw:391-403
###################################################
set.seed(1)
# Simulation: two independent Gaussians
n<-100
x1 <- rnorm(n, mean=70, sd=15)
x2 <- rnorm(n, mean=70, sd=15)
# Add in a linear combination of X1 and X2
x3 <- (x1+x2)/2
# X4 is somewhat correlated with X1 but not relevant to Y
x4 <- x1+runif(100,min=-100,max=100)
# Y is a linear combination of the X's plus noise
y <- 0.7*x1 + 0.3*x2 + rnorm(n, mean=0, sd=sqrt(15))
df <- data.frame(x1=x1, x2=x2, x3=x3, x4, y=y)


###################################################
### code chunk number 5: l12.Rnw:408-409 (eval = FALSE)
###################################################
## install.package("ridge")


###################################################
### code chunk number 6: l12.Rnw:414-420
###################################################
library(ridge)
# Fit a ridge regression
# lambda="automatic" is actually the default setting
demo.ridge <- linearRidge(y ~ x1 + x2 + x3 + x4, data=df, lambda="automatic")
coefficients(demo.ridge)
demo.ridge$lambda


###################################################
### code chunk number 7: l12.Rnw:431-436
###################################################
plot(predict(demo.ridge), fitted(lm(y ~ x1 + x3 + x4, data=df)),
#plot(predict(demo.ridge), fitted(lm(y ~ x1 + x2, data=df)),
xlab="Predictions from ridge regression",
ylab="Predictions from least squares")
abline(0,1)


###################################################
### code chunk number 8: l12.Rnw:446-458
###################################################
# Load data - baseball players statistics 
library(ISLR) 
data(Hitters) 
# Discard NA's 
Hitters <- na.omit(Hitters) 
# The glmnet function works with the design matrix of predictors (without 
# the ones). This can be obtained easily through model.matrix() 
x <- model.matrix(Salary ~ ., data = Hitters)[, -1] 
# Interestingly, note that in Hitters there are two-level factors and these 
# are automatically transformed into dummy variables in x
# We also need the vector of responses 
y <- Hitters$Salary


###################################################
### code chunk number 9: l12.Rnw:464-471
###################################################
# Call to the main function - use alpha = 0 for ridge regression 
library(glmnet)  
ridgeMod <- glmnet(x=x, y = y, alpha = 0) 
# By default, it computes the ridge solution over a set of lambdas 
# automatically chosen. It also standardizes the variables by default to make 
# the model fitting since the penalization is scale-sensitive. Importantly, 
# the coefficients are returned on the original scale of the predictors


###################################################
### code chunk number 10: l12.Rnw:476-479
###################################################
# Plot of the solution path - gives the value of the coefficients 
# for different  measures of lambda
plot(ridgeMod, label = TRUE, xvar = "lambda")


###################################################
### code chunk number 11: l12.Rnw:486-488
###################################################
# Versus the percentage of deviance explained, the same as the R^2 
plot(ridgeMod, label = TRUE, xvar = "dev")


###################################################
### code chunk number 12: l12.Rnw:493-496
###################################################
# The maximum R^2 is slightly above 0.5 
# Indeed, we can see that R^2 = 0.5461 
summary(lm(Salary ~., data = Hitters))$r.squared


###################################################
### code chunk number 13: l12.Rnw:500-502
###################################################
# Some persistently important predictors are 15, 14, and 19 
colnames(x)[c(15, 14, 19)]


###################################################
### code chunk number 14: l12.Rnw:507-511
###################################################
# lambda versus R^2 - fitness decreases when sparsity is introduced, 
# in exchange of better variable interpretation and avoidance of overfitting
plot(log(ridgeMod$lambda), ridgeMod$dev.ratio, type = "l",
    xlab = "log(lambda)", ylab = "R2")	


###################################################
### code chunk number 15: l12.Rnw:516-524
###################################################
ridgeMod$dev.ratio[length(ridgeMod$dev.ratio)] 
# Slightly different to lm's because of compromises in accuracy for speed 
# The coefficients for different values of lambda are given 
# in $a0 (intercepts) 
# and $beta (slopes) or, alternatively, both in coef(ridgeMod) 
length(ridgeMod$a0)  
dim(ridgeMod$beta) 
length(ridgeMod$lambda) # 100 lambda's were automatically chosen 


###################################################
### code chunk number 16: l12.Rnw:529-531
###################################################
# Inspecting the coefficients associated to the 50th lambda
coef(ridgeMod)[, 50]


###################################################
### code chunk number 17: l12.Rnw:537-540
###################################################
# The squared l2-norm of the coefficients decreases as lambda increases
plot(log(ridgeMod$lambda), sqrt(colSums(ridgeMod$beta^2)), type = "l",
 xlab = "log(lambda)", ylab = "l2 norm")


###################################################
### code chunk number 18: l12.Rnw:546-558
###################################################
# Lambda is a tuning parameter that can be chosen by cross-validation, using as # error the MSE (other possible error can be considered for generalized models # using the argument type.measure) 
# 10-fold cross-validation. Change the seed for a different result. set.seed(12345)
kcvRidge <- cv.glmnet(x = x, y = y, alpha = 0, nfolds = 10) 
# The lambda grid in which CV is done is automatically selected 
# The lambda that minimises the CV error is 
kcvRidge$lambda.min 
# Equivalent to 
indMin <- which.min(kcvRidge$cvm) 
kcvRidge$lambda[indMin] 
# The minimum CV error 
kcvRidge$cvm[indMin]  
min(kcvRidge$cvm)  


###################################################
### code chunk number 19: l12.Rnw:562-578
###################################################
# Potential problem! Minimum occurs at one extreme of the lambda grid 
# in which CV is done. This was automatically selected, 
# but can be manually inputted
range(kcvRidge$lambda) 
lambdaGrid <- 10^seq(log10(kcvRidge$lambda[1]), log10(0.1), length.out = 150)
kcvRidge2 <- cv.glmnet(x = x, y = y, nfolds = 10, alpha = 0, 
    lambda = lambdaGrid) 
# Much better plot(kcvRidge2) 
kcvRidge2$lambda.min  
# But the CV curve is random, since it depends on the sample. Its variability 
# can be estimated by considering the CV curves of each fold. An alternative 
# approach to select lambda is to choose the largest within one standard 
# deviation of the minimum error, in order to favour simplicity of the model 
# around the optimal lambda value. 
# This is know as the "one standard error rule" 
kcvRidge2$lambda.1se 


###################################################
### code chunk number 20: l12.Rnw:583-588
###################################################
# Location of both optimal lambdas in the CV loss function in dashed 
# vertical lines, and lowest CV error 
# and lowest CV error + one standard error
plot(kcvRidge2) 
abline(h = kcvRidge2$cvm[indMin] + c(0, kcvRidge2$cvsd[indMin]))


###################################################
### code chunk number 21: l12.Rnw:594-603
###################################################
# The consideration of the one standard error rule for selecting lambda
# makes  special sense when the CV function is quite flat around 
# the minimum (hence an  overpenalization that gives more sparsity 
# does not affect so much the CV loss) 

# Leave-one-out cross-validation. More computationally intense
# but completely  objective in the choice of the fold-assignment 
ncvRidge <- cv.glmnet(x = x, y = y, alpha = 0, nfolds = nrow(Hitters), 
     lambda = lambdaGrid)     


###################################################
### code chunk number 22: l12.Rnw:609-611
###################################################
# Location of both optimal lambdas in the CV loss function 
plot(ncvRidge)


###################################################
### code chunk number 23: l12.Rnw:617-619
###################################################
# The glmnet fit is inside the output of cv.glmnet 
modRidgeCV <- kcvRidge2$glmnet.fit


###################################################
### code chunk number 24: l12.Rnw:625-628
###################################################
# Inspect the best models 
plot(modRidgeCV, label = TRUE, xvar = "lambda") 
abline(v = log(c(kcvRidge2$lambda.min, kcvRidge2$lambda.1se)))


###################################################
### code chunk number 25: l12.Rnw:633-637 (eval = FALSE)
###################################################
## # The model associated to lambda.1se (or any other lambda not included 
## # in the # original path solution - obtained by an interpolation) 
## # can be retrieved with
## predict(modRidgeCV, type = "coefficients", s = kcvRidge2$lambda.1se)


###################################################
### code chunk number 26: l12.Rnw:643-652
###################################################
# Predictions for the first two observations 
predict(modRidgeCV, type = "response", 
   s = kcvRidge2$lambda.1se, newx = x[1:2, ]) 
# Predictions for the first observation, for all the lambdas. We can see how 
# the prediction for one observation changes according to lambda
plot(log(modRidgeCV$lambda), predict(modRidgeCV, type = "response", 
    newx = x[1, , drop = FALSE]), type = "l", xlab = "log(lambda)", 
    ylab = " Prediction")
abline(h=y[1],col="red")


### R code from vignette source 'l13.Rnw'

###################################################
### code chunk number 1: l13.Rnw:5-6
###################################################
options(continue = "   ", prompt = " ")#,scipen = 10)


###################################################
### code chunk number 2: l13.Rnw:91-92
###################################################
curve(exp(x)/(1+exp(x)),from=-10,to=10,ylab=expression(logit(xi)),cex.lab=1.2,xlab=expression(xi))


###################################################
### code chunk number 3: l13.Rnw:330-331 (eval = FALSE)
###################################################
## glm(y~x, family = poisson)


###################################################
### code chunk number 4: l13.Rnw:336-337 (eval = FALSE)
###################################################
## glm(y~x, family = binomial)


###################################################
### code chunk number 5: l13.Rnw:344-345
###################################################
data("Pima.te", package = "MASS")


###################################################
### code chunk number 6: l13.Rnw:348-350
###################################################
logitout <- glm(type ~ . , data = Pima.te, 
 family = binomial)


###################################################
### code chunk number 7: l13.Rnw:353-354 (eval = FALSE)
###################################################
## summary(logitout)


###################################################
### code chunk number 8: l13.Rnw:358-359
###################################################
summary(logitout)


###################################################
### code chunk number 9: l13.Rnw:365-366
###################################################
binomial()$link; binomial()$variance


###################################################
### code chunk number 10: l13.Rnw:386-389
###################################################
# install.packages("GLMsData")
data(hcrabs, package = "GLMsData")
### here do some exploration of the data 


###################################################
### code chunk number 11: l13.Rnw:393-395 (eval = FALSE)
###################################################
## model<-glm(Sat~1, family=poisson(link=log),data=hcrabs)
## summary(model)


###################################################
### code chunk number 12: l13.Rnw:399-401
###################################################
model<-glm(Sat~1, family=poisson(link=log),data=hcrabs)
summary(model)


###################################################
### code chunk number 13: l13.Rnw:406-408 (eval = FALSE)
###################################################
## model<-glm(Sat~1+Width,family=poisson(link=log),data=hcrabs)
## summary(model)


###################################################
### code chunk number 14: l13.Rnw:411-413
###################################################
model<-glm(Sat~1+Width,family=poisson(link=log),data=hcrabs)
summary(model)


###################################################
### code chunk number 15: l13.Rnw:437-451
###################################################
set.seed(12)
n1<-30
n2<-15
x1<-rnorm(n1,9,3)
y1<-rnorm(n1,9,3)
x2<-rnorm(n2,-1,3)
y2<-rnorm(n2,-1,3)
X<-cbind(c(x1,x2),c(y1,y2))
plot(X,type = "n",xlab="x1",ylab="x2")
points(x1,y1,col="red",pch='+',cex=1.4)
points(x2,y2,col="blue",pch=20,cex=1.4)
abline(a=9,b=-1,col="green",lwd=2)
abline(a=10,b=-2,col="black",lwd=2)
z<-c(rep(1,n1),rep(0,n2))


###################################################
### code chunk number 16: l13.Rnw:629-632
###################################################
fl <- "http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data"
SAheart <- read.table(fl,
	sep=",",head=T,row.names=1)


###################################################
### code chunk number 17: l13.Rnw:636-637
###################################################
head(SAheart)


###################################################
### code chunk number 18: l13.Rnw:656-662
###################################################
chd_mod_ldl <- glm(chd ~ ldl, data = SAheart, family = binomial)
plot(jitter(chd, factor = 0.1) ~ ldl, data = SAheart, pch = 20, 
ylab = "Probability of CHD", xlab = "Low Density Lipoprotein Cholesterol")
grid()
curve(predict(chd_mod_ldl, data.frame(ldl = x), type = "response"), 
add = TRUE, col = "dodgerblue", lty = 2)


###################################################
### code chunk number 19: l13.Rnw:677-678
###################################################
coef(summary(chd_mod_ldl))


###################################################
### code chunk number 20: l13.Rnw:683-684
###################################################
confint.default(chd_mod_ldl,level=0.95)


###################################################
### code chunk number 21: l13.Rnw:689-691
###################################################
chd_mod_additive <- glm(chd ~ ., data = SAheart, 
                    family = binomial)


###################################################
### code chunk number 22: l13.Rnw:702-703
###################################################
-2 * as.numeric(logLik(chd_mod_ldl) - logLik(chd_mod_additive))


###################################################
### code chunk number 23: l13.Rnw:709-710
###################################################
anova(chd_mod_ldl, chd_mod_additive, test = "LRT")


###################################################
### code chunk number 24: l13.Rnw:719-721
###################################################
chd_mod_selected <- step(chd_mod_additive, trace = 1, k = 2)
coef(chd_mod_selected)


###################################################
### code chunk number 25: l13.Rnw:732-733
###################################################
anova(chd_mod_selected, chd_mod_additive, test = "LRT")


###################################################
### code chunk number 26: l13.Rnw:857-858 (eval = FALSE)
###################################################
## residuals(model)


###################################################
### code chunk number 27: l13.Rnw:860-861
###################################################
residuals(model)[1:5]


###################################################
### code chunk number 28: l13.Rnw:865-866 (eval = FALSE)
###################################################
## residuals(model,"pearson")


###################################################
### code chunk number 29: l13.Rnw:868-869
###################################################
residuals(model,"pearson")[1:5]


###################################################
### code chunk number 30: l13.Rnw:883-885
###################################################
data(gala, package="faraway") 
gala <- gala[,-2]


###################################################
### code chunk number 31: l13.Rnw:891-893
###################################################
modl <- lm(Species ~ . , gala) 
plot(modl, 1)


###################################################
### code chunk number 32: l13.Rnw:901-903
###################################################
library(MASS)
boxcox(modl, plotit = TRUE)


###################################################
### code chunk number 33: l13.Rnw:909-912
###################################################
modt <- lm(sqrt(Species) ~ . , gala) 
summary(modt)
plot(modt, 1)


###################################################
### code chunk number 34: l13.Rnw:925-929
###################################################
modp <- glm(Species ~ .,family=poisson,gala) 
plot(residuals(modp) ~ predict(modp,type="response"), 
xlab=expression(hat(mu)),
ylab="Deviance residuals") 


###################################################
### code chunk number 35: l13.Rnw:936-938
###################################################
plot(residuals(modp) ~ predict(modp,type="link"), 
xlab=expression(hat(eta)),ylab="Deviance residuals")


###################################################
### code chunk number 36: l13.Rnw:944-946
###################################################
plot(residuals(modp,type="response") ~ predict(modp,type="link"),
xlab=expression(hat(eta)),ylab="Response residuals")


###################################################
### code chunk number 37: l13.Rnw:954-956
###################################################
plot(residuals(modp,type="response") ~ predict(modp,type="link"), 
    xlab=expression(hat(eta)),ylab="Response residuals")


