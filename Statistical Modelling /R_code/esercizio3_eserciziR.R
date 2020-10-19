library(olsrr)
library(pander)
library(lmtest)
library(het.test)
library(car)
library(systemfit)

panderOptions('knitr.auto.asis', FALSE)
  
#-- White test function
  white.test <- function(lmod,data){
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

path_data <- "/Volumes/HDD_Ale/Statistical Modelling/Esercitazioni/1a_Esercitazione - Modello Lineare/R/Esercizio 3/Prestige.txt"  
df <- read.table(path_data, header=TRUE, sep = " ")

head(df)
str(df)

var_numeric <- c("education", "income", "women", "prestige")

pander(summary(df[var_numeric]))

pander(cor(df[var_numeric]))

plot(df[var_numeric], pch=17, cex=0.5)

par(mfrow=c(2,2))
for(i in var_numeric){
	boxplot(df[i], main=i, col="lightgreen")
}

par(mfrow=c(2,2))
for(i in var_numeric){
	hist(df[,i], main=i, col="lightgreen", xlab=i)
}

# primo modello lineare

mod1 <- lm(prestige ~ income, df)

summary(mod1)
anova(mod1)
white.test(mod1, df)
dwtest(mod1)

plot(df$income, df$prestige, xlab="Income", ylab="Pineo-Porter Points", pch=17)
abline(mod1,col=2,lwd=3) #-- abline del modello lineare

# controlliamo se errori sono sferici
plot(fitted(mod1),resid(mod1),pch=19,xlab="Predicted",ylab="Residual")

plot(fitted(mod1),rstudent(mod1),pch=19,xlab="Predicted",ylab="Student - Residual")
abline(h=-2,col=2,lty=2,lwd=2)
abline(h=2,col=2,lty=2,lwd=2)

plot(hatvalues(mod1),rstudent(mod1),pch=19,xlab="Leverage",ylab="Student - Residual")
abline(v=0.04,col=2,lty=2,lwd=2)

plot(cooks.distance(mod1),pch=19,xlab="Observation Index",ylab="Cook DIstance",type="h")
points(cooks.distance(mod1),pch=19)
abline(h=4/nrow(df),col=2,lty=2,lwd=2)

# modello quadratico
mod2 <- lm(prestige~income+I(income^2),df)
summary(mod2)

anova(mod2)

white.test(mod2, df) #-- White test (per dettagli ?bptest)
dwtest(mod2) #-- Durbin-Whatson test

# modello cubico
mod3 <- lm(prestige~income+I(income^2)+I(income^3),df)
summary(mod3)

anova(mod3)

white.test(mod3, df) #-- White test (per dettagli ?bptest)
dwtest(mod3)

plot(df$income,rstudent(mod2),pch=19,xlab="Income",ylab="Student - Residual")
abline(h=-2,col=2,lty=2,lwd=2)
abline(h=2,col=2,lty=2,lwd=2)

plot(df$income^2,rstudent(mod2),pch=19,xlab="Income^2",ylab="Student - Residual")
abline(h=-2,col=2,lty=2,lwd=2)
abline(h=2,col=2,lty=2,lwd=2)

# modello alla 4
mod4 <- lm(prestige~income+I(income^2)+I(income^3)+I(income^4),df)
summary(mod4)

anova(mod4)

white.test(mod4, df) #-- White test (per dettagli ?bptest)

dwtest(mod4)

# log-lin

mod5 <- lm(I(log(prestige))~income,df)
summary(mod5)


anova(mod5)

white.test(mod5, df) #-- White test (per dettagli ?bptest)
dwtest(mod5) #-- Durbin-Whatson test

# lin-log
mod6 <- lm(prestige~I(log(income)),df)
summary(mod6)

anova(mod6)

white.test(mod6, df) #-- White test (per dettagli ?bptest)

dwtest(mod6) #-- Durbin-Whatson test

# log-log
mod7 <- lm(I(log(prestige))~I(log(income)),df)
summary(mod7)

anova(mod7)

white.test(mod7, df) #-- White test (per dettagli ?bptest)

dwtest(mod7)

plot(df$income,df$prestige,pch=19,xlab="Income",ylab="Prestige")
lines(seq(0,25000,1),predict(mod1,data.frame(income=seq(0,25000,1))),col="green",lwd=2)
lines(seq(0,25000,1),predict(mod2,data.frame(income=seq(0,25000,1))),col="red",lwd=2)
lines(seq(0,25000,1),predict(mod3,data.frame(income=seq(0,25000,1))),col=4,lwd=2)
lines(seq(0,25000,1),predict(mod4,data.frame(income=seq(0,25000,1))),col=5,lwd=2)
lines(seq(0,25000,1),exp(predict(mod5,data.frame(income=seq(0,25000,1)))),col=6,lwd=2)
lines(seq(0,25000,1),predict(mod6,data.frame(income=seq(0,25000,1))),col=7,lwd=2)
lines(seq(0,25000,1),exp(predict(mod7,data.frame(income=seq(0,25000,1)))),col=8,lwd=2)