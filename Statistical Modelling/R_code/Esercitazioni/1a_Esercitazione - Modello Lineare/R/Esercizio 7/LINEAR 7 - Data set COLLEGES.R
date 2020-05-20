# LINEAR 7 - Data set: COLLEGES

# # INTRODUZIONE
# Il data set contiene informazioni riguardanti 521 università americane alla fine dell'anno accademico 1993/1994.
# Le variabili contenute sono:
# 1. AVE_MAT: indicatore qualitativo della preparazione nelle discipline matematiche
# 2. APPL_RIC: numero di domande di iscrizione ricevute all'inizio dell'anno
# 3. APPL_ACC: numero di domande di iscrizione accettate all'inizio dell'anno
# 4. P_STUD10: percentuale di studenti procenienti dalle prime 10 scuole superiori americane
# 5. COSTI_V: costi medi pro-capite per vitto, alloggio sostenuti nell'anno (dollari)
# 6. COSTI_B: costi medi pro-capite per l'acquisto di libri di testo sostenuti nell'anno (dollari)
# 7. TASSE: tasse universitarie medie pro-capite versate durante l'anno
# 8. STUD_DOC: numero di studenti per docente
# 9. P_LAUR: percentuale di laureati alla fine dell'anno sul totale degli iscritti al primo anno

#VAriabile target =APPL_ACC
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
ABSOLUTE_PATH <- "C:\\Users\\daniele.riggi\\Desktop\\Corso Bicocca Vittadini\\07_Esercitazioni Vitta\\1a_Esercitazione - Modello Lineare\\R\\Esercizio 7"
              
d <- read.csv(paste0(ABSOLUTE_PATH,"\\COLLEGES.CSV"),sep=";")

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- names(d)[2:ncol(d)]
#-- print delle prime 6 righe del dataset
head(d)



#################################
#   STATISTICHE DESCRITTIVE
#################################

summary(d[,VAR_NUMERIC]) #-- statistiche descrittive

cor(d[,VAR_NUMERIC]) #-- matrice di correlazione


plot(d[,VAR_NUMERIC],pch=19,cex=.5) #-- scatter plot multivariato

par(mfrow=c(3,3))
for(i in VAR_NUMERIC){
  boxplot(d[,i],main=i,col="lightblue",ylab=i)
}

par(mfrow=c(3,3))
for(i in VAR_NUMERIC){
  hist(d[,i],main=i,col="lightblue",xlab=i,freq=F)
}



#################################
#   REGRESSIONE
#################################
# A questa prima vista non si vedono particolari aspetti anomali delle due distribuzioni. Si propone prima il
# legame lineare tra le due variabili.


#-- R CODE
mod1 <- lm(appl_acc ~ ave_MAT + appl_ric + p_stud10, d)
summary(mod1)

summary(mod1)
anova(mod1)
white.test(mod1) #-- White test (per dettagli ?bptest)
dwtest(mod1) #-- Durbin-Whatson test

ols_vif_tol(mod1)

ols_eigen_cindex(mod1)

# Si verifica ora l'omoschedasticità e incorrelazione degli errori cominciando con le rappresentazioni grafiche. Sia
# nel grafico dei valori osservati-previsti della variabile dipendente che in quello dei valori residui-previsti si nota
# una configurazione non omogenea della nuvola di punti a segnalare la probabile presenza di eteroschedasticità
# dei residui.
# Tale eteroschedasticità sembra confermata dai grafici dei residui-valori osservati delle regressioni semplici con
# una sola variabile esplicativa per volta in cui esistono molti punti che si discostano dalla nuvola di punti. Si
# passa ora a esaminare i test sulla sfericità dei residui.
# 
# Il test di White porta a rigettare l'ipotesi di omoschedasticità. Si può quindi concludere che gli errori sono
# eteroschedastici e correlati.


#-- R CODE
par(mfrow=c(2,2))
plot(d$ave_MAT,resid(mod1),pch=19,xlab="ave_MAT",ylab="Residual")
abline(h=0,lwd=3,lty=2,col=2)
plot(d$appl_ric,resid(mod1),pch=19,xlab="appl_ric",ylab="Residual")
abline(h=0,lwd=3,lty=2,col=2)
plot(d$p_stud10,resid(mod1),pch=19,xlab="p_stud10",ylab="Residual")
abline(h=0,lwd=3,lty=2,col=2)
plot(1:nrow(d),rstudent(mod1),pch=19,xlab="Observation Index",ylab="Residual Studentized",type="h")
abline(h=2,lwd=3,lty=2,col=2)
abline(h=-2,lwd=3,lty=2,col=2)

# Si esamina ora la normalità dei residui cominciando con le rappresentazioni grafiche


#-- R CODE
par(mfrow=c(1,1))
plot(mod1,which=2,pch=19)


hist(resid(mod1),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1)),col=2,lwd=2)


shapiro.test(resid(mod1))


ks.test(resid(mod1),"pnorm")

# La distribuzione dei residui e il Q-Q plot mostrano chiaramente una situazione di non normalità confermata
# dal confronto tra quantili delle distribuzione empirica e teorica normale.
# Tal non normalità è confermata dal grafico in cui si confrontano valori residui-predetti. Si nota come la nuvola
# di punti differisce molto dalla configurazione sferica o elittica tipica di una distribuzione normale degli errori.
# Nel complesso quindi si hanno errori eteroschedastici, non normalità dei residui, presenza di outlier: si
# conclude che non è opportuno usare il modello lineare classico basato su OLS.
             

