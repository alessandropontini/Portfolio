library()
library(pander)
library(car)
library(olsrr)
library(systemfit)
library(het.test)
panderOptions('knitr.auto.asis', FALSE)

white.test <- function(lmod,data){
  u2 <- lmod$residuals^2
  y <- fitted(lmod)
  Ru2 <- summary(lm(u2 ~ y + I(y^2)))$r.squared
  LM <- nrow(data)*Ru2
  p.value <- 1-pchisq(LM, 2)
  data.frame("Test statistic"=LM,"P value"=p.value) }
#-- funzione per ottenere osservazioni outlier univariate
FIND_EXTREME_OBSERVARION <- function(x,sd_factor=2){ 
  which(x>mean(x)+sd_factor*sd(x) | x<mean(x)-sd_factor*sd(x))
}
path_data = "/Volumes/HDD_Ale/Statistical Modelling/Esercitazioni/1a_Esercitazione - Modello Lineare/R/Esercizio 1/car.test.txt"

df <- read.table(path_data, sep=" ", header = TRUE)
head(df)

str(df)

var_numeric <- c("Price", "Mileage", "Weight","Disp.", "HP")

summary((df[,var_numeric]))

cor(df[,var_numeric])

plot(df[,var_numeric], pch=17)

par(mfrow=c(2,3))
for(i in var_numeric){
  boxplot(df[,i], col="lightgreen", main=i)
}

par(mfrow=c(2,3))
for(i in var_numeric){
  hist(df[,i], col="lightgreen", main=i)
}
# STATISTICHE DESCRITTIVE FINITE

# REGRESSIONE LINEARE 

mod1 <- lm(Price ~ Disp., df)

summary(mod1)
anova(mod1)

white.test(mod1, df)
dwtest(mod1)

plot(df$Disp.,df$Price,pch=17,col=1,xlab="Disp",ylab="Price")
abline(mod1,col=2,lwd=3)

# MODELLO QUADRATICO

mod2 <- lm(Price ~ Disp. + I(Disp.^2), df)

summary(mod2)

anova(mod2)

white.test(mod2, df)
dwtest(mod2)

f_mod2 <- function(x) coefficients(mod2)[1]+coefficients(mod2)[2]*x+coefficients(mod2)[3]*x^2
plot(df$Disp.,df$Price,pch=19,xlab="Disp",ylab="Price")
abline(mod1,col=2,lwd=3) #-- abline del modello lineare
curve(f_mod2,add=T,col="blue",lwd=3,lty=2) #-- abline del modello quadratico

# PER TOGLIERE OUTLIER
df$ESTREME <- 1 #-- inserisco una nuova colonna del dataset con obs. estreme
#-- ora applico la funzione FIND_EXTREME_OBSERVARION(variabile di interesse,fattore).
#-- si include anche l'osservazione 23 come outlier.
df$ESTREME[c(FIND_EXTREME_OBSERVARION(df$Price,2),FIND_EXTREME_OBSERVARION(df$Disp.,2))] <- 2
plot(df$Disp.,df$Price,pch=19,col=df$ESTREME,xlab="Disp",ylab="Price")

d_noout <- df[-c(FIND_EXTREME_OBSERVARION(df$Price,2),FIND_EXTREME_OBSERVARION(df$Disp.,2)),]

# REGRESSIONE SEMPLICE SENZA OUTLIER 

mod3 <- lm(Price ~ Disp., d_noout)
summary(mod3)
anova(mod3)
white.test(mod3, d_noout)
dwtest(mod3)

plot(d_noout$Disp.,d_noout$Price,pch=17,col=1,xlab="Disp",ylab="Price")
abline(mod3,col=2,lwd=3)

# REGRESSIONE QUADRATICA SENZA OUTLIER

mod4 <- lm(Price ~ Disp. + I(Disp.^2), d_noout)

summary(mod4)

anova(mod4)

white.test(mod4, d_noout)
dwtest(mod4)

f_mod4 <- function(x) coefficients(mod4)[1]+coefficients(mod4)[2]*x+coefficients(mod4)[3]*x^2
plot(df$Disp.,df$Price,pch=19,xlab="Disp",ylab="Price")
abline(mod3,col=2,lwd=3) #-- abline del modello lineare
curve(f_mod4,add=T,col="blue",lwd=3,lty=2)

# LOG - LIN
mod5 <- lm(I(log(Price))~Disp.,d_noout)
summary(mod5)

anova(mod5)

white.test(mod5, d_noout)
dwtest(mod5)

# LIN - LOG
mod6 <- lm(Price~I(log(Disp.)),d_noout)
summary(mod6)

anova(mod6)

white.test(mod6, d_noout)
dwtest(mod6)

# LOG-LOG
mod7 <- lm(I(log(Price))~I(log(Disp.)),d_noout)
summary(mod7)
anova(mod7)
white.test(mod7, d_noout)
dwtest(mod7)