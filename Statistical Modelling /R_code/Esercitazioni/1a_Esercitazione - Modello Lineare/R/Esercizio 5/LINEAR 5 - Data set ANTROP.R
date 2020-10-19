# LINEAR 5 - Data set: ANTROP

# # INTRODUZIONE
# Il dataset è costituito da alcune misure antropometriche rilevate su 248 uomini.
#   1. ETA': età in anni compiuti
#   2. PESO: peso rilevato in libbre
#   3. ALTEZ: altezza (cm)
#   4. COLLO: circonferenza del collo (cm)
#   5. TORACE: circonferenza toracica (cm)
#   6. ADDOM: circonferenza addominale (cm)
#   7. ANCA: circonferenza dell'anca (cm)
#   8. COSCIA: circonferenza della coscia (cm)
#   9. GINOCCH: circonferenza del ginocchio (cm)
#   10. CAVIGLIA: circonferenza della caviglia (cm)
#   11. BICIPITE: circonferenza del bicipite in estensione (cm)
#   12. AVANBR: circonferenza dell'avambraccio (cm)
#   13. POLSO: circonferenza del polso (cm)
# 
# Variabile dipendente: PESO
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
ABSOLUTE_PATH <- "C:\\Users\\daniele.riggi\\Desktop\\Corso Bicocca Vittadini\\07_Esercitazioni Vitta\\1a_Esercitazione - Modello Lineare\\R\\Esercizio 5"
              
d <- read.csv(paste0(ABSOLUTE_PATH,"\\ANTROP.txt"),sep="\t")

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- names(d)[-1]
#-- print delle prime 6 righe del dataset
head(d)



#################################
#   STATISTICHE DESCRITTIVE
#################################

summary(d[,VAR_NUMERIC]) #-- statistiche descrittive

cor(d[,VAR_NUMERIC]) #-- matrice di correlazione



# Si decide di studiare il nesso lineare tra peso e circonferenza del bicipite. Si propongono quindi innanzitutto
# il grafico a dispersione inerente le due variabili, i box plot, i quantili e le osservazioni estreme.

#-- R CODE
d$EXTREME <- 1
d$EXTREME[c(FIND_EXTREME_OBSERVARION(d$bicipite),FIND_EXTREME_OBSERVARION(d$peso))] <- 2

#-- Evidenzio in rosso le osservazioni estreme (superiori ed inferiori)

plot(d$bicipite,d$peso,pch=19,xlab="Bicipite",ylab="Peso",col=d$EXTREME)

par(mfrow=c(1,2))
boxplot(d[,"bicipite"],main="Bicipite",col="lightblue",ylab="Bicipite",freq=F)
boxplot(d[,"peso"],main="Peso",col="lightblue",ylab="Peso",freq=F)

par(mfrow=c(1,2))
hist(d[,"bicipite"],main="Bicipite",col="lightblue",xlab="Bicipite",freq=F)
hist(d[,"peso"],main="Peso",col="lightblue",xlab="Peso",freq=F)

#################################
#   REGRESSIONE
#################################
# A questa prima vista non si vedono particolari aspetti anomali delle due distribuzioni. Si propone prima il
# legame lineare tra le due variabili.


#-- R CODE
mod1 <- lm(peso~bicipite,d)
summary(mod1)

summary(mod1)
anova(mod1)
white.test(mod1) #-- White test (per dettagli ?bptest)
dwtest(mod1) #-- Durbin-Whatson test

#-- R CODE
plot(d$bicipite,d$peso,pch=19,xlab="Bicipite",ylab="Peso")
abline(mod1,col=2,lwd=3) #-- abline del modello lineare


# La variabile esplicativa bicipite è significativa e spiega in modo notevole peso (osservare il valore dell'R2).
# Inoltre gli errori sono omoschedastici come si vede dal test di White. Si verifica ora se un modello linear-log
# sia preferibile al modello lineare.

#-- R CODE
mod2 <- lm(peso~I(log(bicipite)),d)
summary(mod2)


anova(mod2)

white.test(mod2) #-- White test (per dettagli ?bptest)
dwtest(mod2) #-- Durbin-Whatson test

# Si utilizza per il confronto l'R2 e si vede che la differenza è minima a favore del modello lineare. In ogni caso
# anche il modello linear-log ha errori omoschedastici. A questo punto si propone un modello log-lineare


mod3 <- lm(I(log(peso))~bicipite,d)
summary(mod3)

anova(mod3)

white.test(mod3) #-- White test (per dettagli ?bptest)
dwtest(mod3) #-- Durbin-Whatson test


# Anche in questo caso confrontando gli R2 il modello lineare è preferibile leggermente al modello log-lineare a
# sua volta leggermente migliore del modello linear-log. Il modello log-lineare ha anche esso errori omoschedastici.
# Si propone a questo punto il modello log-log:



#-- R CODE
mod4 <- lm(I(log(peso))~I(log(bicipite)),d)
summary(mod4)

anova(mod4)

white.test(mod4) #-- White test (per dettagli ?bptest)

dwtest(mod4) #-- Durbin-Whatson test


# Ancora una volta il modello lineare è migliore del modello log-log che ha ancora errori omoschedastici. Si
# sceglie quindi in definitiva il modello lineare.