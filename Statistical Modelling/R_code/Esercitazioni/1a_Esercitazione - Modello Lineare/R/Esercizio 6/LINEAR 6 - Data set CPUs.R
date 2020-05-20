# LINEAR 5 - Data set: CPUs

# # INTRODUZIONE
#Il data set contiene performance di misura e caratteristiche di 209 CPUs. Le variabili sono le seguenti:
# 1. NAME: produttore del modello
# 2. SYCT: cycle time in nanosecondi
# 3. MMIN: minimim main memory in KB
# 4. MMAX: maximum main memory in KB
# 5. CACH: cache size in KB
# 6. CHMIN: minimum number of channels
# 7. CHMAX: maximum number of channels
# 8. PERF: performance della CPU comparata con il modello IBM 370/158-3
# 9. ESTPERF: stima della performance

#VAriabile target =perf
# Analisi proposte:
#   1. Statistiche descrittive
#   2. Regressione lineare

  
installed.packages("pander")
install.packages("car")
install.packages("olsrr")
install.packages("systemfit")
install.packages("het.test")
install.packages("lmtest")
  
install.packages("olsrr")

  
  #-- R CODE
library(pander)
library(car)
library(olsrr)
library(systemfit)
library(het.test)
library(lmtest)
library(olsrr)
  

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
ABSOLUTE_PATH <- "/Volumes/HDD_Ale/Statistical Modelling/Esercitazioni/1a_Esercitazione - Modello Lineare/R/Esercizio 6/cpus.txt"
              
d <- read.table(ABSOLUTE_PATH, header=TRUE, sep=" ")

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- names(d)[3:ncol(d)]
#-- print delle prime 6 righe del dataset
head(d)



#################################
#   STATISTICHE DESCRITTIVE
#################################

summary(d[,VAR_NUMERIC]) #-- statistiche descrittive

cor(d[,VAR_NUMERIC]) #-- matrice di correlazione


plot(d[,VAR_NUMERIC],pch=19,cex=.5) #-- scatter plot multivariato

par(mfrow=c(2,4))
for(i in VAR_NUMERIC){
  boxplot(d[,i],main=i,col="lightblue",ylab=i)
}

par(mfrow=c(2,4))
for(i in VAR_NUMERIC){
  hist(d[,i],main=i,col="lightblue",xlab=i,freq=F)
}


# Si costruisce quindi un modello lineare in cui la variabile dipendente "perf" viene regredita rispetto alle
# variabili esplicative.



#################################
#   REGRESSIONE
#################################
# A questa prima vista non si vedono particolari aspetti anomali delle due distribuzioni. Si propone prima il
# legame lineare tra le due variabili.


#-- R CODE
mod1 <- lm(perf ~ syct + mmin + mmax + cach + chmin + chmax + estperf, d)
summary(mod1)

summary(mod1)
anova(mod1)
white.test(mod1) #-- White test (per dettagli ?bptest)
dwtest(mod1) #-- Durbin-Whatson test

ols_vif_tol(mod1)

ols_eigen_cindex(mod1)

# I modello risulta significativo ma solo la variabile "estperf" ha associato un parametro che cade nella regione
# di rifiuto per cui ? respinta l'ipotesi nulla di non significativit?.
# Si esamina quindi la collinearit?; come si pu? notare l'indice di tolleranza ? molto piccolo e l'inflation indice ?
# molto grande proprio per "estperf" l'unica variabile significativa, per cui la quota di varianza risulta altres?
# molto elevata per l'8^ autovalore. "estperf" risulta quindi multicollineare con le altre variabili e viene quindi
# eliminata. Si effettua una nuova regressione escludendo "estperf".
# Si vede come come cambia radicalmente la situazione inerente la significativit? delle variabili: "mmin",
# "mmax", "cach", "chmax" risultano significative. Inoltre nessuna delle variabili ? ora collineare.


mod1 <- lm(perf ~ syct + mmin + mmax + cach + chmin + chmax, d)
summary(mod1)

summary(mod1)
anova(mod1)
white.test(mod1) #-- White test (per dettagli ?bptest)
dwtest(mod1) #-- Durbin-Whatson test

ols_vif_tol(mod1)
ols_eigen_cindex(mod1)




# Si analizza ora la normalit? dei residui considerando il modello con le sole variabili significative "mmin",
# "mmax", "cach", "chmax". Si inizia studiando il valore degli indici di asimmetria e curtosi, la distribuzione
# dei residui e il box plot.

#-- R CODE
par(mfrow=c(1,1))
plot(mod1,which=2,pch=19)


hist(resid(mod1),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1)),col=2,lwd=2)


shapiro.test(resid(mod1))


ks.test(resid(mod1),"pnorm")

# La distribuzione dei residui sembra respingere l'ipotesi di normalit? e tutti i test respingono l'ipotesi nulla di
# normalit?. Sia dal grafico del Q-Q plot che dal confronto dei quantili della distribuzione normale teorica e
# osservata si vede la forte discrepanza tra tali distribuzioni. E' una ulteriore prova della non normalit? dei
# residui.

             

