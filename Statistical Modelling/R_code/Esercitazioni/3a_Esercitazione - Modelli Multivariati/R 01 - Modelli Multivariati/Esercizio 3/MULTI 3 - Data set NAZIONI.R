# MULTI 3 - Data set: NAZIONI
# Nel dataset sono riportati i risultati di un'indagine effettuata nel 1995 su 66 nazioni e riguardanti alcuni fra
# gli aspetti socio-demografici prevalenti. Le variabili presenti nel data set sono le seguenti:
# 1. DENSITA': densità di popolazione (abitanti per Kmq)
# 2. URBANA: percentuale di popolazione residente nelle città
# 3. VITAFEM: speranza di vita alla nascita delle donne
# 4. VITAMAS: speranza di vita alla nascita dei maschi
# 5. ALFABET: percentuale di alfabetizzati sul totale della popolazione
# 6. PIL: prodotto interno lordo pro-capite
# 7. RELIG: religione prevalente nella nazione: 1 = Cattolica; 2 = Ortodossa; 3 = Protestante
# Analisi proposte:
# 1. Statistiche descrittive
# 2. Regressione Multivariata


install.packages("car")
install.packages("sjstats")
install.packages("plotrix")
install.packages("sjPlot")
install.packages("sjmisc")
install.packages("lme4")
install.packages("pander")
install.packages("car")
install.packages("olsrr")
install.packages("systemfit")
install.packages("het.test")
install.packages("ppcor")


#-- R CODE
library(car)
library(sjstats)
library(plotrix)
library(sjPlot)
library(sjmisc)
library(lme4)
library(pander)
library(car)
library(olsrr)
library(systemfit)
library(het.test)
library(ppcor)

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




#-- import dei dati
ABSOLUTE_PATH <- "C:\\Users\\daniele.riggi\\Desktop\\Esame Vitta - Statistical Modeling\\esercitazioni\\3a_Esercitazione\\R 01 - Modelli Multivariati\\Esercizio 3"

d <- read.csv(paste0(ABSOLUTE_PATH,"\\nazioni.csv"),sep=";")

str(d)
#d$relig <- factor(d$relig,1:3,c("catt","ortod","prot"))
d$dummy_cat <- ifelse(d$relig==1,1,0)
d$dummy_ort <- ifelse(d$relig==2,1,0)
d$dummy_prot <- ifelse(d$relig==3,1,0)



#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("densita","urbana","alfabet","pil")


#-- print delle prime 6 righe del dataset
head(d)


  

# STATISTICHE DESCRITTIVE

# Come variabili dipendenti si usa "bmr" e "Ebmr"; come variabili esplicative si usa "wt", "ht", "Cvit."


summary(d[,VAR_NUMERIC]) #-- statistiche descrittive
cor(d[,VAR_NUMERIC]) #-- matrice di correlazione

plot(d[,VAR_NUMERIC],pch=19,cex=.5) #-- scatter plot multivariato

par(mfrow=c(2,2))
for(i in VAR_NUMERIC){
  boxplot(d[,i],main=i,col="lightblue",ylab=i)
}

par(mfrow=c(2,2))
for(i in VAR_NUMERIC){
  hist(d[,i],main=i,col="lightblue",xlab=i,freq=F)
}




# REGRESSIONE 1

# Si propongano ora le regressioni multiple con "vitamas" e "vitafem" variabili dipendenti

#-- R CODE
mod1 <- lm(vitamas ~ densita + urbana + alfabet + pil + dummy_ort + dummy_prot, d)

summary(mod1)

anova(mod1)


#-- R CODE
white.test(mod1)
dwtest(mod1) #-- Durbin-Whatson test
# 
# 
par(mfrow=c(1,1))
# #-- R CODE
# 
plot(mod1,which=2,pch=19)

hist(resid(mod1),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1)),col=2,lwd=2)





#-- R CODE
plot(hatvalues(mod1),rstudent(mod1),pch=19,xlab="Leverage",ylab="Student - Residual")
abline(h=2,col=2,lty=2,lwd=2)
abline(h=-2,col=2,lty=2,lwd=2)

#-- R CODE
plot(cooks.distance(mod1),pch=19,xlab="Observation Index",ylab="Cook DIstance",type="h")
points(cooks.distance(mod1),pch=19)
abline(h=4/nrow(d),col=2,lty=2,lwd=2)




# REGRESSIONE 2

#-- R CODE
mod2 <- lm(vitafem ~ densita + urbana + alfabet + pil + dummy_ort + dummy_prot, d)

summary(mod2)


anova(mod2)

#-- R CODE
white.test(mod2)

dwtest(mod2)

# In entrambe le regressioni il fitting è molto elevato. Si passi ora al modello multivariato e all'analisi dei test multivariati.


plot(mod2,which=2,pch=19)

hist(resid(mod2),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod2)),col=2,lwd=2)




#-- R CODE
mod3 <- lm(cbind(vitamas,vitafem) ~ densita + urbana + alfabet + pil + dummy_ort + dummy_prot, d)


cor(d$vitamas,d$vitafem)
pcor.test(d$vitamas,d$vitafem,d[,c("densita","urbana","alfabet","pil","dummy_ort","dummy_prot")])

summary(mod3)

summary(manova(mod3))

Anova(mod3, type="III")


#-- R CODE
summary(manova(cbind(vitamas,vitafem) ~ densita, data = d))
summary(manova(cbind(vitamas,vitafem) ~ urbana, data = d))
summary(manova(cbind(vitamas,vitafem) ~ alfabet, data = d))
summary(manova(cbind(vitamas,vitafem) ~ pil, data = d))
summary(manova(cbind(vitamas,vitafem) ~ dummy_ort, data = d))
summary(manova(cbind(vitamas,vitafem) ~ dummy_prot, data = d))

