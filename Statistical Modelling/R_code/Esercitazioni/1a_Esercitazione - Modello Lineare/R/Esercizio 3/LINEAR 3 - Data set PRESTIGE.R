# LINEAR 3 - Data set: PRESTIGE

# INTRODUZIONE
# Il data set contiene contiene 102 osservazioni e le seguenti 6 variabili.
#   1. EDUCATION: istruzione media (in anni) dei lavoratori nel 1971
#   2. INCOME: reddito medio (in dollari) dei lavoratori nel 1971
#   3. WOMEN: percentuale di lavoratori donne nel 1971
#   4. PRESTIGE: punteggio di Pineo-Porter relativo al prestigio delle occupazioni, ottenuto tramite sondaggio sociale condotto a metà del 1960.
#   5. CENSUS: codice dell'occupazione nel censimento canadese
#   6. TYPE: tipologia di occupazione (variabile categoriale).
# Variabile dipendente: PRESTIGE.
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
ABSOLUTE_PATH <- "C:\\Users\\daniele.riggi\\Desktop\\Corso Bicocca Vittadini\\07_Esercitazioni Vitta\\1a_Esercitazione - Modello Lineare\\R\\Esercizio 3"
              
d <- read.csv(paste0(ABSOLUTE_PATH,"\\Prestige.txt"),sep=" ")

VAR_NUMERIC <- c("education","income","women","prestige")

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
# Si analizza la dipendenza di "Prestige" da "Income" innanzitutto con una regressione lineare.

#-- R CODE
mod1 <- lm(prestige~income,d)
summary(mod1)
anova(mod1)
white.test(mod1) #-- White test (per dettagli ?bptest)
dwtest(mod1) #-- Durbin-Whatson test

#-- R CODE
plot(d$income,d$prestige,pch=19,xlab="Income",ylab="Prestige")
abline(mod1,col=2,lwd=3) #-- abline del modello lineare

#-- R CODE
plot(fitted(mod1),resid(mod1),pch=19,xlab="Predicted",ylab="Residual")

# Il modello ha un discreto fitting (R2 = 0.5111), "income" è significativa e gli errori sono sferici. Si nota piuttosto
# la presenza di outlier confermata dai grafici seguenti. Inoltre dai grafici prestige-income e residui-income
# traspare un legame non lineare non interpretato dal modello lineare semplice.

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


#Pur dovendo eliminare gli outlier per avere risultati migliori ci si concentra sulla scelta di migliori interpolanti.
# Si verifica dapprima se e quali interpolanti di grado superiore al primo siano opportuni.

#-- R CODE
mod2 <- lm(prestige~income+I(income^2),d)
summary(mod2)

anova(mod2)

white.test(mod2) #-- White test (per dettagli ?bptest)
dwtest(mod2) #-- Durbin-Whatson test

# Il fitting migliora nettamente e risultano significativi sia il temine "income" lineare che quadratico. Lo si
# vede anche dai grafici residui-income residui - income^2 ove i residui sono compresi in intervalli di valori più
# contenuti.

#-- R CODE
plot(d$income,rstudent(mod2),pch=19,xlab="Income",ylab="Student - Residual")
abline(h=-2,col=2,lty=2,lwd=2)
abline(h=2,col=2,lty=2,lwd=2)


plot(d$income^2,rstudent(mod2),pch=19,xlab="Income^2",ylab="Student - Residual")
abline(h=-2,col=2,lty=2,lwd=2)
abline(h=2,col=2,lty=2,lwd=2)


# I modelli di grado 3 e 4 non sono adeguati perché i parametri non sono significativi.

#-- R CODE
mod3 <- lm(prestige~income+I(income^2)+I(income^3),d)
summary(mod3)

anova(mod3)

white.test(mod3) #-- White test (per dettagli ?bptest)
dwtest(mod3) #-- Durbin-Whatson test

#-- R CODE
mod4 <- lm(prestige~income+I(income^2)+I(income^3)+I(income^4),d)
summary(mod4)

anova(mod4)

white.test(mod4) #-- White test (per dettagli ?bptest)

dwtest(mod4) #-- Durbin-Whatson test


# Si propone ora un modello log-lin in cui la variabile log(Prestige) viene regredita su "Income".

#-- R CODE
mod5 <- lm(I(log(prestige))~income,d)
summary(mod5)


anova(mod5)

white.test(mod5) #-- White test (per dettagli ?bptest)
dwtest(mod5) #-- Durbin-Whatson test


# Il parametro associato alla variabile "income" è significativo ma il fitting è peggiore e gli errori sono non
# correlati ma viene respinta l'ipotesi di omoschedasticità. Se si analizza quindi il modello lin-log in cui la
# variabile prestige è regredita rispetto a log(Income); i parametri sono significativi ma il fitting è leggermente
# peggiore rispetto al caso quadratico e gli errori omoschedastici ma viene respinta l'ipotesi di loro non
# correlazione.

#-- R CODE
mod6 <- lm(prestige~I(log(income)),d)
summary(mod6)

anova(mod6)

white.test(mod6) #-- White test (per dettagli ?bptest)

dwtest(mod6) #-- Durbin-Whatson test

# il modello log-log in cui la variabile log(Prestige) viene regredita su log(Income) ha un fitting solo leggermente
# peggiore che il modello quadratico, i parametri sono significativi ma viene respinta sia l'ipotesi di
# omoschedasticità che di non correlazione dei residui.


#-- R CODE
mod7 <- lm(prestige~I(log(income)),d)
summary(mod7)

anova(mod7)

white.test(mod7) #-- White test (per dettagli ?bptest)

dwtest(mod7) #-- Durbin-Whatson test


#-- R CODE
plot(d$income,d$prestige,pch=19,xlab="Income",ylab="Prestige")
lines(seq(0,25000,0.1),predict(mod7,data.frame(income=seq(0,25000,0.1))),col=2,lwd=2)


# Il modello prescelto è quindi quello quadratico.
# Si rappresentano congiuntamente i diversi modelli:


#-- R CODE
plot(d$income,d$prestige,pch=19,xlab="Income",ylab="Prestige")
lines(seq(0,25000,1),predict(mod1,data.frame(income=seq(0,25000,1))),col=2,lwd=2)
lines(seq(0,25000,1),predict(mod2,data.frame(income=seq(0,25000,1))),col=3,lwd=2)
lines(seq(0,25000,1),predict(mod3,data.frame(income=seq(0,25000,1))),col=4,lwd=2)
lines(seq(0,25000,1),predict(mod4,data.frame(income=seq(0,25000,1))),col=5,lwd=2)
lines(seq(0,25000,1),exp(predict(mod5,data.frame(income=seq(0,25000,1)))),col=6,lwd=2)
lines(seq(0,25000,1),predict(mod6,data.frame(income=seq(0,25000,1))),col=7,lwd=2)
lines(seq(0,25000,1),predict(mod7,data.frame(income=seq(0,25000,1))),col=8,lwd=2)












