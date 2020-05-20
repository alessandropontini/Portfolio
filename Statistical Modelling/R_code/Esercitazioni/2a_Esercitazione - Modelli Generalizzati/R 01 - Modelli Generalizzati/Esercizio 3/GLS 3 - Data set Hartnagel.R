# # GLS 3 - Data set: HARTNAGEL
# 
# Il dataset contiene 38 osservazioni e 7 variabili, con dati raccolti dal 1931 al 1968. Le variabili sono le seguenti:
# 1. YEAR: 1931-1968
# 2. TFR: tasso di fertilit? totale per 1000 donne
# 3. PARTIC: forza lavoro femminile per 1000
# 4. DEGREES: grado di studio di scuola secondaria per 10.000
# 5. FCONVICT: tasso (femminile) di offese subite per 100.000
# 6. FTHEFT: tasso (femminile) di furti subiti per 100.000
# 7. MCONVICT: tasso (maschile) di offese subite per 100.000
# 8. MTHEFT: tasso (maschile) di furti subiti per 100.000

# Analisi proposte:
# 1. Statistiche descrittive
# 2. Regressione
# 3. Gestione dell'autocorrelazione

# install.packages("Hmisc")
# install.packages("pander")
# install.packages("car")
# install.packages("olsrr")
# install.packages("systemfit")
# install.packages("het.test")
# install.packages("lmtest")



#-- R CODE
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

#-- import dei dati
ABSOLUTE_PATH <- "/Volumes/HDD_Ale/Statistical Modelling/Esercitazioni/2a_Esercitazione - Modelli Generalizzati/R 01 - Modelli Generalizzati/Esercizio 3/Hartnagel.csv"

d <- read.table(ABSOLUTE_PATH,header=TRUE,sep=";")


#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("year","tfr","partic","degrees","fconvict","ftheft","mconvict","mtheft")

#-- print delle prime 6 righe del dataset
head(d)
str(d)

# STATISTICHE DESCRITTIVE

summary(d[,VAR_NUMERIC]) #-- statistiche descrittive
cor(d[,VAR_NUMERIC]) #-- matrice di correlazione

plot(d[,VAR_NUMERIC],pch=19,cex=.5) #-- scatter plot multivariato

par(mfrow=c(3,3))
for(i in VAR_NUMERIC){
  boxplot(d[,i],main=i,col="lightblue",ylab=i)
}

# Non esistono correlazioni elevate tra le variabili.


# REGRESSIONE
# Si regredisce la variabile "ftheft" su "partic", "degrees", "mtheft"


#-- R CODE
mod1 <- lm(ftheft ~ partic + degrees + mtheft, d) #-- stima modello lineare semplice
summary(mod1)

anova(mod1)
par(mfrow=c(1,1))

# Il modello interpreta bene la variabile dipendente e il fitting ? molto elevato. I parametri significativi sono
# quelli relativi a "partic" - forza lavoro femminile e "degrees" - grado di istruzione. Gli errori sono normali come si evince dalla distribuzione dei residui.

#-- R CODE
white.test(mod1)

dwtest(mod1) #-- Durbin-Whatson test

#-- R CODE
plot(mod1,which=1,pch=19)

plot(mod1,which=2,pch=19)

plot(mod1,which=3,pch=19)

plot(mod1,which=4,pch=19)
abline(h=2*3/nrow(d),col=2,lwd=3,lty=2)

plot(mod1,which=5,pch=19)

 
# Il grafico Q-Q plot mostra la presenza di outlier ai valori estremi della distribuzione; i grafici residui
# studentizzati, leverage, Distanza di Cook confermano tale ipotesi. Tuttavia non operiamo alcuna correzione
# per quel che concerne gli outlier. Il test di White non respinge l'ipotesi di omoschedasticit? dei residui.


#-- R CODE
index <- as.numeric(row.names(mod1$model))
plot(d$year[index],resid(mod1),pch=19,xlab="Year",ylab="Residuo",type="l",col=1,lwd=2)
text(d$year[index],resid(mod1),d$year[index],pos=1,cex=.6)


# Il test respinge senza alcun dubbio l'ipotesi nulla di non correlazione dei residui. Ora si regrediscono i residui
# con i residui ritardati per ottenere il coefficiente di autocorrelazione seriale di primo grado. Si parte dalle
# statistiche descrittive

#-- R CODE
autocorr <- acf(resid(mod1),main="Autocorrelazion",lwd=2)


data.frame(LAG=autocorr$lag,VALUE=autocorr$acf)[1:5,]


#-- metodo alternativo per ottenere il corff. di autocorrelazione
cor(resid(mod1),c(NA,resid(mod1)[1:(length(resid(mod1))-1)]),use="pairwise.complete.obs")


# Ora si regrediscono OLS "res" su OLS "res_1":
  
  
d1 <- data.frame(
  mod1$model,
  resid=resid(mod1),
  resid_l1=c(NA,resid(mod1)[1:(length(resid(mod1))-1)]) #-- residui ritardati
)



mod2 <- lm(resid ~ resid_l1,d1)

#-- R CODE

summary(mod2)

anova(mod2)

#-- R CODE
white.test(mod2)

dwtest(mod2)


# Si propone il modello corretto per la correlazione seriale sostituendo manualmente ad ogni valore Yt il valore
# ^ Yt = Yt - 0.542Yt-1 in modo tale che i nuovi residui siano tra loro incorrelati.


#-- R CODE
d1 <- data.frame(
  mod1$model,
  resid=resid(mod1)
)

d1$ftheft_l1 <- Lag(d1$ftheft,1)
d1$partic_l1 <- Lag(d1$partic,1)
d1$degrees_l1 <- Lag(d1$degrees,1)
d1$mtheft_l1 <- Lag(d1$mtheft,1)
d1$resid_l1 <- Lag(d1$resid,1)

d1$int_tild <- 1-0.542
d1$ftheft_t <- d1$ftheft-0.542*d1$ftheft_l1
d1$partic_t <- d1$partic-0.542*d1$partic_l1
d1$degrees_t <- d1$degrees-0.542*d1$degrees_l1
d1$mtheft_t <- d1$mtheft-0.542*d1$mtheft_l1
d1$resid_t <- d1$resid-0.542*d1$resid_l1


mod3 <- lm(ftheft_t ~ 0 + int_tild + partic_t + degrees_t + mtheft_t,d1)

summary(mod3)
anova(mod3)

#-- R CODE
white.test(mod3)

dwtest(mod3)


# Il modello interpreta ancora meglio i dati. Ancora "partic" e "degrees" risultano significative a significare che
# il numero di condanne delle donne per furto aumenta all'aumentare della quota di laureate sulla popolazione e
# partecipazione alla forza lavoro quasi a significare che un loro aumento di partecipazione alla vita in positivo
# significa anche un aumento della loro criminalit?.
# La non correlazione fra gli errori confermata dal test di Durbin-Watson.
# Si conferma l'omoschedasticit? dei residui e diminuiscono nettamente gli outlier.

#-- R CODE

plot(mod3,which=1,pch=19)
plot(mod3,which=2,pch=19)
plot(mod3,which=3,pch=19)
plot(mod3,which=4,pch=19)
abline(h=2*3/nrow(d),col=2,lwd=3,lty=2)

plot(mod3,which=5,pch=19)


# Proviamo ora ad utilizzare funzioni che permettono di considerare automaticamente le correlazioni del residui.

#-- R CODE
mod4 <- arima(d1$ftheft, order=c(1,0,0), xreg = d1[,c("partic","degrees","mtheft")],method="ML")
mod4

coeftest(mod4)


durbinWatsonTest(as.numeric(mod4$residuals))


#-- R CODE
mod5 <- arima(d1$ftheft, order=c(2,0,0), xreg = d1[,c("partic","degrees","mtheft")],method="ML")

mod5


coeftest(mod5)

durbinWatsonTest(as.numeric(mod5$residuals), max.lag=2)

#-- R CODE
mod6 <- arima(d1$ftheft, order=c(3,0,0), xreg = d1[,c("partic","degrees","mtheft")],method="ML")
mod6


coeftest(mod6)

durbinWatsonTest(as.numeric(mod5$residuals), max.lag=3)



