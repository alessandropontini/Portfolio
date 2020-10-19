# LINEAR 4 - Data set: ROAD

# INTRODUZIONE
# 
# Il data set contiene contiene 26 osservazioni e le seguenti 7 variabili.
#   1. STATE: nome dello stato
#   2. DEATHS: numero di morti per incidenti stradali
#   3. DRIVERS: numero di automobilisti (in 10000s)
#   4. POPDEN: densità di popolazione per miglio gradrato
#   5. RURAL: lunghezza delle strade di tipo rurali
#   6. FUEL: consumo di carburante (in 10 000 000 galloni americani per anno)
# Variabile dipendente: DEATHS
# Analisi proposte:
#   1. Statistiche descrittive
#   2. Regressione lineare e polinomiale

  
installed.packages("pander")
install.packages("car")
install.packages("olsrr")
install.packages("systemfit")
install.packages("het.test")
install.packages("lmtest")
  
  
  #-- R CODE
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


#-- import dei dati
ABSOLUTE_PATH <- "C:\\Users\\daniele.riggi\\Desktop\\Corso Bicocca Vittadini\\07_Esercitazioni Vitta\\1a_Esercitazione - Modello Lineare\\R\\Esercizio 4"
              
d <- read.csv(paste0(ABSOLUTE_PATH,"\\ROAD.txt"),sep=" ")

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("deaths","drivers","popden","rural","temp","fuel")
#-- print delle prime 6 righe del dataset
head(d)



#################################
#   STATISTICHE DESCRITTIVE
#################################

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

par(mfrow=c(1,1))

#################################
#   REGRESSIONE
#################################
# Si propone una regressione di "deaths" su "rural".

#-- R CODE
mod1 <- lm(deaths~rural,d)
summary(mod1)

summary(mod1)
anova(mod1)
white.test(mod1) #-- White test (per dettagli ?bptest)
dwtest(mod1) #-- Durbin-Whatson test

#-- R CODE
plot(d$rural,d$deaths,pch=19,xlab="Rural",ylab="Deaths")
text(d$rural,d$deaths,d$country,pos=1)
abline(mod1,col=2,lwd=3) #-- abline del modello lineare

#-- R CODE
plot(fitted(mod1),resid(mod1),pch=19,xlab="Predicted",ylab="Residual")
plot(fitted(mod1),d$deaths,pch=19,xlab="Predicted",ylab="Deaths")



# Il fitting è modesto (R2 = 0.3168) e il parametro associato alla variabile "rural" è significativo. Le rappresentazioni
# grafiche mostrano che i residui sono omoschedastici ma esiste un outlier che andrebbe eliminato la
# California.
# La presenza di tale outlier è confermata da tutti i grafici.

#-- R CODE
plot(fitted(mod1),rstudent(mod1),pch=19,xlab="Predicted",ylab="Student - Residual")
abline(h=-2,col=2,lty=2,lwd=2)
abline(h=2,col=2,lty=2,lwd=2)

#-- R CODE
plot(hatvalues(mod1),rstudent(mod1),pch=19,xlab="Leverage",ylab="Student - Residual")
abline(v=0.04,col=2,lty=2,lwd=2)


plot(cooks.distance(mod1),pch=19,xlab="Observation Index",ylab="Cook DIstance",type="h")
points(cooks.distance(mod1),pch=19)
abline(h=4/nrow(d),col=2,lty=2,lwd=2)

# La distribuzione dei residui sembra normale eccetto per che una leggera asimmetria negativa ed emerge la
# presenza di outlier sulle code del Q-Q plot.


#-- R CODE
plot(mod1,which=2,pch=19)

hist(resid(mod1),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1)),col=2,lwd=2)


# Invece di eliminare l'osservazione California si prova a vedere se una funzione non lineare può interpretarla
# come può intepretare meglio la varaibile "deaths". Si provi innanzitutto con il modello quadratico.


#-- R CODE

#-- R CODE
mod2 <- lm(deaths~rural+I(rural^2),d)
summary(mod2)


anova(mod2)

white.test(mod2) #-- White test (per dettagli ?bptest)
dwtest(mod2) #-- Durbin-Whatson test

# Il fitting migliora leggermente ma "rural" e rural2 non risultano significativi quindi non è adeguato il modello
# quadratico come si vede anche dalla rappresentazione grafica seguente:

#-- R CODE
plot(d$rural,d$deaths,pch=19,xlab="Rural",ylab="Deaths")
lines(seq(0,25000,1),predict(mod1,data.frame(rural=seq(0,25000,1))),col=2,lwd=2)
lines(seq(0,25000,1),predict(mod2,data.frame(rural=seq(0,25000,1))),col=3,lwd=2)

# Si prova ora con il modello lin-log in cui la variabile esplicativa è log(Rural). Si deve per forza eliminare
# l'osservazione distretto di Washington perché la lunghezza delle strade rurali è zero e quindi il logaritmo di
# zero sarebbe infinito che non ha senso.



# I modelli di grado 3 e 4 non sono adeguati perché i parametri non sono significativi.

#-- R CODE

#-- R CODE
d_log <- d[!is.infinite(log(d$rural)),] #-- elimino le osservazioni che hanno log(rural)=Infinito
mod3 <- lm(deaths~I(log(rural)),d_log)
summary(mod3)

anova(mod3)

white.test(mod3) #-- White test (per dettagli ?bptest)
dwtest(mod3) #-- Durbin-Whatson test

# Ora log(Rural) è significativo, ma il fitting peggiora e quindi il modello non va bene. Lo si ve anche dalla
# rappresentazione grafica seguente:

#-- R CODE
plot(d$rural,d$deaths,pch=19,xlab="Rural",ylab="Deaths")
lines(seq(0,25000,1),predict(mod3,data.frame(rural=seq(0,25000,1))),col=2,lwd=2)


# Si propone ora il modello log lineare ove la variabile log(Death) dipende da "rural".



#-- R CODE
mod4 <- lm(I(log(deaths))~rural,d)
summary(mod4)


anova(mod4)

white.test(mod4) #-- White test (per dettagli ?bptest)

dwtest(mod4) #-- Durbin-Whatson test


# La rappresentazione grafica conferma il buon fitting del modello eccetto che per California.
# Gli errori sono chiaramente omoschedatici.

#-- R CODE
plot(d$rural,d$deaths,pch=19,xlab="Rural",ylab="Deaths")
lines(seq(0,25000,1),exp(predict(mod4,data.frame(rural=seq(0,25000,1)))),col=2,lwd=2)

#-- R CODE
hist(resid(mod4),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod4)),col=2,lwd=2)



# Si propone ora il modello log-log in cui la variabile log(Deaths) dipende da log(Rural). Migliora il fitting
# ed è il massimo tra i modelli proposti. Log(Rural) è significativo. Le rappresentazioni grafiche confermano
# che il modello è il più adeguato tra quelli proposti e i residui sono omoschedastici. Le statistiche inerenti gli
# outlier mostrano che solo California ha valori fuori norma. Il Q-Q plot e la distribuzione dei residui mostrano
# con chiarezza che ora tali residui sono chiaramente normali.

#-- R CODE
mod5 <- lm(I(log(deaths))~I(log(rural)),d_log)
summary(mod5)

anova(mod5)

white.test(mod5) #-- White test (per dettagli ?bptest)
dwtest(mod5) #-- Durbin-Whatson test


#-- R CODE
plot(d$rural,d$deaths,pch=19,xlab="Rural",ylab="Deaths")
lines(seq(0,25000,1),exp(predict(mod5,data.frame(rural=seq(0,25000,1)))),col=2,lwd=2)

#-- R CODE
#plot(mod5,which=2,pch=19)
hist(resid(mod5),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod5)),col=2,lwd=2)


#-- R CODE
plot(hatvalues(mod5),rstudent(mod5),pch=19,xlab="Leverage",ylab="Student - Residual")
abline(h=2,col=2,lty=2,lwd=2)
abline(h=-2,col=2,lty=2,lwd=2)


plot(cooks.distance(mod5),pch=19,xlab="Observation Index",ylab="Cook DIstance",type="h")
points(cooks.distance(mod5),pch=19)
abline(h=4/nrow(d),col=2,lty=2,lwd=2)

# In definitiva il modello migliore è quello log-log. Per migliorare i risultati occorre ora eliminare l'osservazione California.

