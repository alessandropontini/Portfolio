# LINEAR 2 - Data set: AIRQUALITY

# INTRODUZIONE
# Il data set contiene contiene 154 osservazioni con 6 variabili.
#   1. OZONO: concentrazioni di Ozono (parti per milione misurata a Roosevelt Island)
#   2. SOLAR.R: radiazione solare (misurata al Central Park)
#   3. WIND: velocità media del vento (misurata all'aeroporto LaGuardia)
#   4. TEMP: temperatura in F (misurata all'aeroporto LaGuardia)
#   5. MONTH: mese
#   6. DAY: giorno del mese
# Variabile dipendente: TEMP.
# 
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
ABSOLUTE_PATH <- "C:\\Users\\daniele.riggi\\Desktop\\Corso Bicocca Vittadini\\07_Esercitazioni Vitta\\1a_Esercitazione - Modello Lineare\\R\\Esercizio 2"
              
d <- read.csv(paste0(ABSOLUTE_PATH,"\\airquality.txt"),sep=" ")

VAR_NUMERIC <- c("Ozone","Solar.R","Wind","Temp")
#-- print delle prime 6 righe del dataset
head(d)



#################################
#   STATISTICHE DESCRITTIVE
#################################


#-- R CODE
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

#################################
#   REGRESSIONE
#################################


par(mfrow=c(1,1))
mod1 <- lm(Temp~Ozone,d)
summary(mod1)


anova(mod1)

white.test(mod1) #-- White test (per dettagli ?bptest)

dwtest(mod1) #-- Durbin-Whatson test


# Il modello ha un fitting buono ma non elevatissimo (R2 = 0.47) e "Ozono" è significativo. Tuttavia l'ipotesi di
# incorrelazione è respinta così come l'omoschedasticità. Si prova ora con polinomi di grado superiore (2, 3, 4).



#-- R CODE
plot(d$Ozone,d$Temp,pch=19,xlab="Ozone",ylab="Temp")
abline(mod1,col=2,lwd=3) #-- abline del modello lineare

#-- R CODE
mod2 <- lm(Temp~Ozone+I(Ozone^2),d)
summary(mod2)


anova(mod2)

white.test(mod2) #-- White test (per dettagli ?bptest)

dwtest(mod2) #-- Durbin-Whatson test

# Il modello polinomiale di ordine 2 ha un fitting migliore (R2 = 0.5849) e i parametri relativi a "Ozono" e
# anche a Ozono2 sono significativi. Il valore negativo del parametro segnala che la concavità è verso il basso.
# Si prova a verificare ora se sia opportuno utilizzare il modello polinomiale di ordine 3 e 4.



#-- R CODE
mod3 <- lm(Temp~Ozone+I(Ozone^2)+I(Ozone^3),d)
summary(mod3)

anova(mod3)

white.test(mod3) #-- White test (per dettagli ?bptest)

dwtest(mod3) #-- Durbin-Whatson test



#-- R CODE
mod4 <- lm(Temp~Ozone+I(Ozone^2)+I(Ozone^3)+I(Ozone^4),d)
summary(mod4)

anova(mod4)

white.test(mod4) #-- White test (per dettagli ?bptest)

dwtest(mod4) #-- Durbin-Whatson test


#-- R CODE
plot(d$Ozone,d$Temp,pch=19,xlab="Ozone",ylab="Temp")
lines(seq(0,150,0.1),predict(mod1,data.frame(Ozone=seq(0,150,0.1))),col=2,lwd=2)
#abline(mod1,col=2,lwd=3) #-- abline del modello lineare; graficamente è la stessa cosa della riga sopra

lines(seq(0,150,0.1),predict(mod2,data.frame(Ozone=seq(0,150,0.1))),col=2,lwd=2)
lines(seq(0,150,0.1),predict(mod3,data.frame(Ozone=seq(0,150,0.1))),col=3,lwd=2)
lines(seq(0,150,0.1),predict(mod4,data.frame(Ozone=seq(0,150,0.1))),col=4,lwd=2)

# Il fitting del modelli polinomiale di ordine 3 migliora leggermente, ma solo il parametro relativo a Ozono
# risulta significativo; il modello polinomiale di ordine 4 migliora ancora un po' il fitting ma nessun parametro
# è significativo.





# Si prova ora a verificare l'opportunità di usare un modello lin-log che utilizza il logaritmo dell'ozono come
# variabile esplicativa.

#-- R CODE
mod5 <- lm(Temp~I(log(Ozone)),d)
summary(mod5)

anova(mod5)
white.test(mod5) #-- White test (per dettagli ?bptest)

dwtest(mod5) #-- Durbin-Whatson test

#-- R CODE
plot(d$Ozone,d$Temp,pch=19,xlab="Ozone",ylab="Temp",main="")
lines(seq(0,150,0.1),predict(mod5,data.frame(Ozone=seq(0,150,0.1))),col="blue",lwd=3)



# Il fitting è peggiore (R2 = 0.51), log(Ozono) è significativo, ma è respinta l'ipotesi di non correlazione fra
# gli errori e anche a riguardo della omoschedaticità. Si propone quindi il modello log-lin in cui la variabile
# dipendente è log(Temp):



#-- R CODE
mod6 <- lm(I(log(Temp))~Ozone,d)
summary(mod6)

anova(mod6)


white.test(mod6) #-- White test (per dettagli ?bptest)

dwtest(mod6) #-- Durbin-Whatson test

#-- R CODE
plot(d$Ozone,d$Temp,pch=19,xlab="Ozone",ylab="Temp",main="")
lines(seq(0,150,0.1),exp(predict(mod6,data.frame(Ozone=seq(0,150,0.1)))),col="blue",lwd=3) #-- notare exp( 0 50


# Il parametro relativo a ozono è significativo ma il fitting peggiora ancora e vale quanto detto per il modello
# lin log per ciò che concerne la sfericità degli errori. Si propone ora il modello log-log che studia la dipendenza
# di log(Temp) da log(Ozono).



#-- R CODE
mod7 <- lm(I(log(Temp))~I(log(Ozone)),d)
summary(mod7)


anova(mod7)

white.test(mod7) #-- White test (per dettagli ?bptest)

dwtest(mod7) #-- Durbin-Whatson test

#-- R CODE
plot(d$Ozone,d$Temp,pch=19,xlab="Ozone",ylab="Temp",main="")
lines(seq(0,150,0.1),exp(predict(mod7,data.frame(Ozone=seq(0,150,0.1)))),col="blue",lwd=3)

# Log(Ozono) è significativo ma il fitting peggiora ancora e inoltre viene respinta sia l'ipotesi di omoschedasticità
# che quella di non correlazione fra i residui. In definitiva il modello prescelto è il modello quadratico che però
# necessiterebbe di verifica della sfericità degli errori.

