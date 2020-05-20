# # GLS 6 - Data set: QUAKES
# 
# INTRODUZIONE
# Il dataset riguarda i terremoti rilevati vicino a Fiji. Le osservazioni rappresentano i movimenti sismici rilevati
# nel 1964 con magnitudo maggiore di 4. Le variabili sono:
# 1. LAT: latitudine
# 2. LONG: longitudine
# 3. DEPTH: profondità
# 4. MAG: magnitudo
# 5. STATIONS: stazione
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
# 


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
ABSOLUTE_PATH <- "C:\\Users\\daniele.riggi\\Desktop\\Esame Vitta - Statistical Modeling\\esercitazioni\\2a_Esercitazione\\R 01 - Modelli Generalizzati\\Esercizio 6"

d <- read.csv(paste0(ABSOLUTE_PATH,"\\QUAKES.txt"),sep=" ")



#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("lat","long","depth","mag")

#-- print delle prime 6 righe del dataset
head(d)
str(d)

# STATISTICHE DESCRITTIVE

summary(d[,VAR_NUMERIC]) #-- statistiche descrittive
cor(d[,VAR_NUMERIC]) #-- matrice di correlazione

plot(d[,VAR_NUMERIC],pch=19,cex=.5) #-- scatter plot multivariato

par(mfrow=c(2,2))
for(i in VAR_NUMERIC){
  boxplot(d[,i],main=i,col="lightblue",ylab=i)
}




# REGRESSIONE

#-- R CODE
mod1 <- lm(mag ~ stations + depth + long + lat , d) #-- stima modello lineare semplice

summary(mod1)

anova(mod1)
par(mfrow=c(1,1))


#-- R CODE
white.test(mod1)

dwtest(mod1) #-- Durbin-Whatson test

# Le 4 variabili esplicative "stations", "depth", "long", "lat" risultano tutte significative. Il valore dell'R2 è
# molto buono e il modello interpreta bene la variabile dipendente.
# Si verifica ora la multicollinearità delle variabili esplicative.
# Per tutti e 4 i valori l'indice di tolleranza è quasi prossimo a uno e quindi mostra che non esiste collinearità.
# Il condition index perfeziona tale conclusione perché se risulta debolmente dipendente per il quarto auto valore
# mentre il quinto assume valore molto elevato andando a spiegare quota di varianza elevata per l'intercetta e
# la variabile "long".

ols_eigen_cindex(mod1)

ols_vif_tol(mod1)

# Si verifica ora la normalità. Si analizza innanzitutto la distribuzione dei residui e il box plot. L'istogramma si
# sovrappone bene alla curva normale teorica.
# Per ciò che concerne il box plot dei residui si verifica che c'è simmetria intorno alla media. Anche la
# distribuzione cumulata dei residui empirici si sovrappone a quella dei residui della distribuzione teorica
# normale
# 


plot(mod1,which=1,pch=19)
plot(mod1,which=2,pch=19)
plot(mod1,which=3,pch=19)
plot(mod1,which=4,pch=19)
abline(h=2*4/nrow(d),col=2,lwd=3,lty=2)
plot(mod1,which=5,pch=19)


#-- R CODE
# COVRATIO- measures the change in the determinant of the covariance matrix of the estimates by deleting the ith observation:

plot(covratio(mod1),pch=19,ylab="Covratio")
abline(h=1-3*4/nrow(d),lwd=3,col=2,lty=2)
abline(h=1+3*4/nrow(d),lwd=3,col=2,lty=2)

hist(resid(mod1),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1)),col=2,lwd=2)


shapiro.test(resid(mod1))
ks.test(resid(mod1),"pnorm")


# I test, in particolare Shapiro-Wilk (vicino a 1 come valore) e Kolmogorov-Smirnov, cadono tutti nella regione
# di accettazione: non respingo l'ipotesi nulla di normalità dei residui.



#-- R CODE
plot(fitted(mod1),resid(mod1),pch=19,xlab="Predicted",ylab="Residuo",type="p",col=1,lwd=2)
text(fitted(mod1),resid(mod1),d$nazione,pos=1,cex=.6)
abline(h=0,lwd=2,lty=2,col=2)


# La rappresentazione grafica dei residui ben lontana da una forma rettangolare e il test di White mostrano con
# chiarezza che l'ipotesi di omoschedasticità degli errori va respinta. Inoltre si vede la presenza di outlier (Stati
# Uniti e Svizzera). Per ciò che concerne la non correlazione degli errori si vede dal test di Durbin-Watson che
# è respinta l'ipotesi di non correlazione dei residui. Per risolvere il problema si propone un metodo di stima FGLS.



                                                                                              
                                                                                              
   