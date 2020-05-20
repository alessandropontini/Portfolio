library(pander)
library(het.test)
library(olsrr)
library(car)
library(lmtest)

path_data <- "/Volumes/HDD_Ale/Statistical Modelling/Esercitazioni/1a_Esercitazione - Modello Lineare/R/Esercizio 2/airquality.txt"

df <- read.table(path_data, header=TRUE, sep= " ")

head(df)
str(df)

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
  
var_numeric <- c("Ozone", "Solar.R", "Wind", "Temp")

pander(summary(df[var_numeric]))

pander(cor(df[var_numeric]))

plot(df[var_numeric], pch=17, cex=0.5)

par(mfrow=c(2,2))
for(i in var_numeric){
	boxplot(df[i], main=i, col="lightblue", xlab=i, freq=F)
}


par(mfrow=c(2,2))
for(i in var_numeric){
  hist(df[,i],main=i,col="lightblue",xlab=i,freq=F)
}

mod1 <- lm(Temp ~ Ozone, df)
pander(summary(mod1))

pander(anova(mod1))

white.test(mod1, df)

dwtest(mod1)

# Provo con quadratico

mod2 <- lm(Temp ~ Ozone + I(Ozone^2), df)

summary(mod2)

anova(mod2)

white.test(mod2, df)

dwtest(mod2)

# cubico
mod3 <- lm(Temp~Ozone+I(Ozone^2)+I(Ozone^3),df)
summary(mod3)

anova(mod3)

white.test(mod3, df) #-- White test (per dettagli ?bptest)

dwtest(mod3) #-- Durbin-Whatson test

# modello quadratico
mod4 <- lm(Temp~Ozone+I(Ozone^2)+I(Ozone^3)+I(Ozone^4),df)
summary(mod4)

anova(mod4)

white.test(mod4, df) #-- White test (per dettagli ?bptest)

dwtest(mod4) #-- Durbin-Whatson test

plot(df$Ozone,df$Temp,pch=17,xlab="Ozone",ylab="Temp", col="black")
lines(seq(0,150,0.1),predict(mod1,data.frame(Ozone=seq(0,150,0.1))),col="green",lwd=2)
#abline(mod1,col=2,lwd=3) #-- abline del modello lineare; graficamente � la stessa #cosa della riga sopra

lines(seq(0,150,0.1),predict(mod2,data.frame(Ozone=seq(0,150,0.1))),col="yellow",lwd=2)
lines(seq(0,150,0.1),predict(mod3,data.frame(Ozone=seq(0,150,0.1))),col=3,lwd=2)
lines(seq(0,150,0.1),predict(mod4,data.frame(Ozone=seq(0,150,0.1))),col=4,lwd=2)

# proviamo valori lin - log

mod5 <- lm(Temp ~ I(log(Ozone)), df)

summary(mod5)

anova(mod5)

white.test(mod5, df)

dwtest(mod5)

plot(df$Ozone,df$Temp,pch=19,xlab="Ozone",ylab="Temp",main="")
lines(seq(0,150,0.1),predict(mod5,data.frame(Ozone=seq(0,150,0.1))),col="blue",lwd=3)

# proviamo il mod log lin

mod6 <- lm(I(log(Temp)) ~ Ozone, df)

summary(mod6)
anova(mod6)
white.test(mod6, df)
dwtest(mod6)

plot(df$Ozone,df$Temp,pch=19,xlab="Ozone",ylab="Temp",main="")
lines(seq(0,150,0.1),predict(mod5,data.frame(Ozone=seq(0,150,0.1))),col="blue",lwd=3)
lines(seq(0,150,0.1),exp(predict(mod6,data.frame(Ozone=seq(0,150,0.1)))),col="green",lwd=3)
# proviamo log-log

mod7 <- lm(I(log(Temp)) ~ I(log(Ozone)), df)

summary(mod7)
anova(mod7)
white.test(mod7, df)
dwtest(mod7)

plot(df$Ozone,df$Temp,pch=19,xlab="Ozone",ylab="Temp",main="")
lines(seq(0,150,0.1),predict(mod5,data.frame(Ozone=seq(0,150,0.1))),col="blue",lwd=3)
lines(seq(0,150,0.1),exp(predict(mod6,data.frame(Ozone=seq(0,150,0.1)))),col="green",lwd=3)
lines(seq(0,150,0.1),exp(predict(mod7,data.frame(Ozone=seq(0,150,0.1)))),col="purple",lwd=3)
