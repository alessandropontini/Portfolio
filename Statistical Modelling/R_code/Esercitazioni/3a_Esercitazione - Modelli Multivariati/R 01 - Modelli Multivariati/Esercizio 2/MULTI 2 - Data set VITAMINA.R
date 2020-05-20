# MULTI 2 - Data set: VITAMINA
# Per 2224 individui è stata studiata la quanità di energia consumata. Le variabili a disposizione sono le
# seguenti:
# 1. PERSON: identificativo dell'individuo
# 2. WT: peso in Kg
# 3. HT: altezza in cm
# 4. SEX: 1 maschio, 2 femmina
# 5. AGE: età
# 6. BMR: basal metabolic rate
# 7. E_BMR: energy per BMR
# 8. ENERGI: energy content (kj)
# 9. AVIT: vitamina A (RE)
# 10. RETINOL: retinol (mg)
# 11. BETACAR: beta carotene (mg)
# 12. DVIT: vitamina D (mg)
# 13. EVIT: vitamina E (alphaTE)
# 14. B1VIT: vitamina BA (mg)
# 15. B2VIT: vitamina B2 (mg)
# 16. B6VIT: vitamina B6 (mg)
# 17. FOLACIN: folacin (mg)
# 18. B12VUIR: vitamina B12 (mg)
# 19. CVIT: vitamina C (mg)
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
ABSOLUTE_PATH <- "C:\\Users\\daniele.riggi\\Desktop\\Esame Vitta - Statistical Modeling\\esercitazioni\\3a_Esercitazione\\R 01 - Modelli Multivariati\\Esercizio 2"

d <- read.csv(paste0(ABSOLUTE_PATH,"\\vitamina.csv"),sep=";")

str(d)
#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("bmr","E_bmr","wt","ht","Cvit")

# BMR: basal metabolic rate
# E_BMR: energy per BMR
# CVIT: vitamina C (mg)
# WT: peso in Kg
# HT: altezza in cm


#-- print delle prime 6 righe del dataset
head(d)

# STATISTICHE DESCRITTIVE

# Come variabili dipendenti si usa "bmr" e "Ebmr"; come variabili esplicative si usa "wt", "ht", "Cvit."


summary(d[,VAR_NUMERIC]) #-- statistiche descrittive
cor(d[,VAR_NUMERIC]) #-- matrice di correlazione

plot(d[,VAR_NUMERIC],pch=19,cex=.5) #-- scatter plot multivariato

# Il metabolismo basale o BMR è il dispendio energetico di un organismo vivente a riposo, comprendente dunque 
# l'energia necessaria per le funzioni metaboliche vitali (respirazione, circolazione sanguigna, digestione, 
# attività del sistema nervoso, ecc.). Rappresenta circa il 45-75% del dispendio energetico totale nella giornata (wikipedia).

par(mfrow=c(2,3))
for(i in VAR_NUMERIC){
  boxplot(d[,i],main=i,col="lightblue",ylab=i)
}

par(mfrow=c(2,3))
for(i in VAR_NUMERIC){
  hist(d[,i],main=i,col="lightblue",xlab=i,freq=F)
}


# Non esistono correlazioni particolarmente forti che facciano pensare a collinearità o
# legami di dipendenza lineare perfetta.



# ESERCIZIO 1
# Non appaiono correlazioni di particolare valore. Si propone innanzitutto la regressione multipla di "bmr" sulle 3 variabili esplicative.

#-- R CODE
mod1 <- lm(bmr ~ wt + ht + Cvit, d)

summary(mod1)

anova(mod1)


# Il fitting è molto elevato e "wt" e "ht" sono significative. Tuttavia gli errori sono eteroschedatici come si vede
# dal grafici residui-predetti e residui-variabile esplicativa ht e dal test di White.

#-- R CODE
white.test(mod1)
dwtest(mod1) #-- Durbin-Whatson test


par(mfrow=c(1,1))
#-- R CODE

plot(mod1,which=2,pch=19)

hist(resid(mod1),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1)),col=2,lwd=2)


# Esistono anche diversi outlier come si vede dai grafici inerenti le misure che analizzano tali outlier. La
# distribuzione dei residui appare comunque normale.


plot(fitted(mod1),d$bmr,pch=19,xlab="Fitted",ylab="BMR")

#-- R CODE
plot(hatvalues(mod1),rstudent(mod1),pch=19,xlab="Leverage",ylab="Student - Residual")
abline(h=2,col=2,lty=2,lwd=2)
abline(h=-2,col=2,lty=2,lwd=2)

#-- R CODE
plot(cooks.distance(mod1),pch=19,xlab="Observation Index",ylab="Cook DIstance",type="h")
points(cooks.distance(mod1),pch=19)
abline(h=4/nrow(d),col=2,lty=2,lwd=2)



# Si consideri allora la seconda regressione multipla di "E_bmr" su "wt", "ht", "Cwit."


#-- R CODE
mod2 <- lm(E_bmr ~ wt + ht + Cvit, d)
summary(mod2)


anova(mod2)

#-- R CODE
white.test(mod2)

dwtest(mod2)



# Nonostante le 3 variabili esplicative siano significative il fitting è molto scadente. Inoltre gli errori sono
# eteroschedastici ed esistono anche diversi outlier che risultano essere tuttavia incorrelati.

plot(mod2,which=2,pch=19)

hist(resid(mod2),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod2)),col=2,lwd=2)



# Prima di proseguire si dovrebbe a questo punto eliminare l'eteroschedasticità degli errori dividendo variabile
# dipendente, variabili esplicative, errori medesimi per lo scarto quadratico degli errori stessi. Inoltre si
# dovrebbero eliminare gli outlier individuati nelle due regressioni multiple. Si prosegue invece senza operare
# queste trasformazioni per potere mostrare, a puri scopi didattici, il nesso tra regressione multipla OLS e
# regressione multivariata OLS.
# Si propone quindi la regressione multivariata delle due variabili dipendenti sulle tre variabili esplicative.
# Il modello multivariato con le stesse variabili esplicative sotto il profilo descrittivo è accostamento di due
# regressioni multiple che vengono risolte l'una indipendentemente dall'altra perciò gli R2 e le stime dei
# parametri usando il test sono identici. Il test F conferma i risultati perché la f non è altro che il quadrato della t.




#-- R CODE
mod3 <- lm(cbind(E_bmr,bmr) ~ wt + ht + Cvit, d)

cor(d$E_bmr,d$bmr) #-- matrice di correlazione
pcor.test(d$E_bmr,d$bmr,d[,c("wt","ht","Cvit")])
# A partial correlation coefficient describes the strength of a linear relationship between two variables, holding constant a number of other variables.

summary(mod3)

summary(manova(mod3))

anova(mod3)

Anova(mod3, type="III")

# Type 1 Anova  - Type I sum of squares are "sequential."  In essence the factors are tested in the order they are listed in the model.  

# Estimates using type I SS tell you how much of the variability in Y can be explained by A, how much of the residual variability can be explained by B, 
# how much of the remaining residual variability can be explained by the interaction, and so on, in order.

# Type III are "partial."  In essence, every term in the model is tested in light of every other term in the model.  
# That means that main effects are tested in light of interaction terms as well as in light of other main effects.  
# Estimates based on type III SS tell you how much of the residual variability in Y can be accounted for by A after having accounted for everything else, 
# and how much of the residual variability in Y can be accounted for by B after having accounted for everything else as well, and so on.
# (Note that both go both first and last simultaneously; 


# Si costruiscono quindi i test multivariati inerenti le tre variabile esplicative. Poiché l'ipotesi testata è che
# i parametri relativi alle 3 variabili risultino nulle per entrambe le equazioni, tutti i parametri risultano
# significativi per tutti i test, come era prevedibile dati i risultati delle regressioni multiple.

#-- R CODE
summary(manova(cbind(E_bmr,bmr) ~ wt, data = d))

summary(manova(cbind(E_bmr,bmr) ~ ht, data = d))

summary(manova(cbind(E_bmr,bmr) ~ Cvit, data = d))

# Infine effettuiamo un test congiunto multivariato per tutte le variabile esplicative congiuntamente. Come
# deve essere per quanto visto viene respinta l'ipotesi nulla di non significatività di almeno una delle variabili

#-- R CODE
summary(manova(cbind(E_bmr,bmr) ~ wt + ht + Cvit, data = d))


