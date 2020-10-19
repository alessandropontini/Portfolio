# # GLS 4 - Data set: NAZIONI
# 
# Nel dataset in oggetto sono riportati i risultati di un'indagine effettuata nel 1995 su 66 nazioni riguardanti
# alcuni fra gli aspetti socio-demografici prevalenti. Le variabili presenti nel dataset sono le seguenti:
# 1.	DENSITA': densità di popolazione
# 2.	URBANA: percentuale di popolazione residente nelle città
# 3.	VITAFEM: speranza di vita alla nascita delle donne
# 4.	VITAMAS: speranza di vita alla anascita dei maschi
# 5.	ALFABET: percentuale di alfabetizzati sul totale della popolazione
# 6.	PIL: prodotto interno lordo pro-capite
# 7.	RELIG: religione prevalente nella nazione (1=cattolica, 2=ortodossa, 3=protestante)

# Analisi proposte:
# 1.	Statistiche descrittive
# 2.	Regressione
# 3.	Gestione dell'autocorrelazione


install.packages("Hmisc")
install.packages("pander")
install.packages("car")
install.packages("olsrr")
install.packages("systemfit")
install.packages("het.test")
install.packages("lmtest")



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
ABSOLUTE_PATH <- "C:\\Users\\daniele.riggi\\Desktop\\Corso Bicocca Vittadini\\07_Esercitazioni Vitta\\2a_Esercitazione\\R - Modelli Generalizzati\\Esercizio 5"

d <- read.csv(paste0(ABSOLUTE_PATH,"\\nazioni.csv"),sep=";")


#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("GNP.deflator","GNP","Unemployed","Armed.Forces","Population","Employed")

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

# Esiste una fortissima correlazione fra "vitafem" e "vitamas" che fa presagire una collinearità fra le due
# variabili. Effettuiamo ora una regressione di "pil" su "urbana", "vitamas", "vitafem", "alfabet".



# REGRESSIONE
# Data questa situazione si utilizzano come variabili esplicative rispetto a "GNP" solo "Unemployed", "Armed
# Forces", "Population", "Employed". Gli errori sono omoschedastici secondo il test di White. Il fitting è
# altissimo ma le uniche variabili veramente significative sono "Population" e "Employed".


#-- R CODE
mod1 <- lm(pil ~ urbana + vitamas + vitafem + alfabet, d) #-- stima modello lineare semplice

summary(mod1)

anova(mod1)
par(mfrow=c(1,1))


#-- R CODE
white.test(mod1)

dwtest(mod1) #-- Durbin-Whatson test


# Il modello è significativo ma non interpreta molto bene la variabile dipendente e infatti solo l'intercetta risulta
# significativa. Si sarebbe portati a cambiar modello, ma prima occorre verificare la diagnostica. Tralasciando la verifica
# di multicollinearità che pur sarebbe molto opportuna consideriamo la sfericità degli errori. Analizziamo
# dapprima l'eteroschedasticità degli errori.


#-- R CODE
plot(fitted(mod1),resid(mod1),pch=19,xlab="Predicted",ylab="Residuo",type="p",col=1,lwd=2)
text(fitted(mod1),resid(mod1),d$nazione,pos=1,cex=.6)
abline(h=0,lwd=2,lty=2,col=2)


# La rappresentazione grafica dei residui ben lontana da una forma rettangolare e il test di White mostrano con
# chiarezza che l'ipotesi di omoschedasticità degli errori va respinta. Inoltre si vede la presenza di outlier (Stati
# Uniti e Svizzera). Per ciò che concerne la non correlazione degli errori si vede dal test di Durbin-Watson che
# è respinta l'ipotesi di non correlazione dei residui. Per risolvere il problema si propone un metodo di stima FGLS.


#-- R CODE
mod2 <- lm(resid(mod1)^2 ~ urbana + vitamas + vitafem + alfabet, d)
weight <- 1/fitted(mod2)
mod3 <- lm(pil ~ urbana + vitamas + vitafem + alfabet, d,weights=weight)
summary(mod3)
anova(mod3)


#-- R CODE
plot(fitted(mod3),resid(mod3),pch=19,xlab="Predicted",ylab="Residuo",type="p",col=1,lwd=2)
text(fitted(mod3),resid(mod3),d$nazione,pos=1,cex=.6)
abline(h=0,lwd=2,lty=2,col=2)

# Il modello ora fitta in modo rilevante i dati e "vitamas" è significativa.

# Si propone ora un modello basato su stime FGLS con errori espressi in forma esponenziale


#-- R CODE
mod5 <- lm(log(resid(mod1)^2) ~ urbana + vitamas + vitafem + alfabet, d)
sd_error <- sqrt(exp(fitted(mod5)))
mod6 <- lm(I(pil/sd_error) ~ 0 + I(1/sd_error) + I(urbana/sd_error) + I(vitamas/sd_error) + I(vitafem/sd_error) + I(alfabet/sd_error) , d)
                                                                                              
                                                                                              
summary(mod6)

anova(mod6)  

#-- R CODE
white.test(mod6)

dwtest(mod6) #-- Durbin-Whatson test

# Il modello ora ha un fitting ancora più elevato che il precedente modello e "vitamas" rimane l'unica variabile
# con parametri significativi a mostrare che il "Pil" è influenzato solo dalla speranza di vita maschile tra le
# variabili esplicative prescelte. I test di White e Durbin Watson mostrano che i residui sono omoschedastici e
# incorrelati.


                                                                                              
                                                                                              
   