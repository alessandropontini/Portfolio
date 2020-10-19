library(Hmisc)
library(pander)
library(car)
library(olsrr)
library(systemfit)
library(het.test)
library(lmtest)

panderOptions('knitr.auto.asis', FALSE)

#-- White test function
white.test <- function(lmod,data=d){
  u2 <- lmod$residuals^2
  y <- fitted(lmod)
  Ru2 <- summary(lm(u2 ~ y + I(y^2)))$r.squared
  LM <- nrow(data)*Ru2
  p.value <- 1-pchisq(LM, 2)
  data.frame("Test statistic"=LM,"P value"=p.value)
}

#-- funzione per ottenere osservazioni outlier univariate
FIND_EXTREME_OBSERVARION <- function(x,sd_factor=2){
  which(x>mean(x)+sd_factor*sd(x) | x<mean(x)-sd_factor*sd(x))
}

path_data <- "/Volumes/HDD_Ale/Statistical Modelling/Esercitazioni/2a_Esercitazione - Modelli Generalizzati/R 01 - Modelli Generalizzati/Esercizio 6/QUAKES.TXT"

d <- read.table(path_data, header=TRUE, sep = " ")

head(d)
str(d)

var_numeric <- c("lat", "long", "depth", "mag")

pander(summary(d[var_numeric]))

cor(d[var_numeric])

plot(d[, var_numeric], pch=17, cex=0.5)

par(mfrow=c(2,2))
for(i in var_numeric){
	boxplot(d[, i], col="lightgreen", main=i)
}

# REGRESSIONE

mod1 <- lm(mag ~ stations + depth + long + lat, d)

summary(mod1)

anova(mod1)

white.test(mod1)

dwtest(mod1)
# collinearità
ols_eigen_cindex(mod1)
ols_vif_tol(mod1)

# disegniamo grafici dei residui

plot(mod1, which=1, pch=17)
plot(mod1, which=2, pch=17)
plot(mod1, which=3, pch=17)
plot(mod1, which=4, pch=17)
abline(h=2*4/nrow(d),col=2,lwd=3,lty=2)
plot(mod1,which=5,pch=19)


#-- R CODE
# COVRATIO- measures the change in the determinant of the covariance matrix of the estimates by deleting the ith observation:

plot(covratio(mod1),pch=19,ylab="Covratio")
abline(h=1-3*4/nrow(d),lwd=3,col=2,lty=2)
abline(h=1+3*4/nrow(d),lwd=3,col=2,lty=2)

hist(resid(mod1),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1)),col=2,lwd=2)
shapiro.test(resid(mod1))

ks.test(resid(mod1),"pnorm") ks.test(resid(mod1),"pnorm")




