# # GLS 2 - Data set: COMPANIES
# 
# I dati contengono alcune informazioni riguardanti 64 compagnie. Le variabili presenti nel dataset sono:
# 1. ASSETS: attivo in bilancio (milioni di dollari)
# 2. SALES: fatturato relativo alle vendite
# 3. MARK_VAL: valore di mercato della compagnia (milioni di dollari)
# 4. PROFITS: profitto (milioni di dollari)
# 5. CASH: flusso di cassa
# 6. EMPLOY: numero complessivo di dipendenti
# 7. SECTOR: settore di mercato in cui opera la compagnia (comunicazioni, energia, finanza, hitech,
#                                                          manufatturiero, medico, retail, trasporti, altro)
# Analisi proposte:
# 1. Statistiche descrittive
# 2. Regressione
# 3. Studio dell'autocorrelazione


# installed.packages("pander")
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

#panderOptions('knitr.auto.asis', FALSE)

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
ABSOLUTE_PATH <- "/Volumes/HDD_Ale/Statistical Modelling/Esercitazioni/2a_Esercitazione - Modelli Generalizzati/R 01 - Modelli Generalizzati/Esercizio 2/companies.csv"

d <- read.csv(ABSOLUTE_PATH,sep=";")

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("assets","sales","mark_val","profits","cash","employ")

#-- print delle prime 6 righe del dataset
head(d)

# STATISTICHE DESCRITTIVE

summary(d[,VAR_NUMERIC]) #-- statistiche descrittive
cor(d[,VAR_NUMERIC]) #-- matrice di correlazione

plot(d[,VAR_NUMERIC],pch=19,cex=.5) #-- scatter plot multivariato

par(mfrow=c(2,3))
for(i in VAR_NUMERIC){
  boxplot(d[,i],main=i,col="lightblue",ylab=i)
}

# Non esistono correlazioni elevate tra le variabili.


# REGRESSIONE
# Si effettua la regressione con variabile dipendente con "mark_val" e variabile esplicativa "assets", "sales",
# "profits", "cash", "employ".


#-- R CODE
mod1 <- lm(mark_val ~ assets + sales + profits + cash + employ, d) #-- stima modello lineare semplice
summary(mod1)

anova(mod1)

# Il modello interpreta bene la variabile dipendente (R2 = 0.7394) ma solo "cash" e in parte "employ" hanno
# associato parametri significativi. Si verifica ora omoschedasticit? e incorrelazione.

#-- R CODE
white.test(mod1)

dwtest(mod1) #-- Durbin-Whatson test
par(mfrow=c(1,1))
#-- R CODE
plot(mod1,which=1,pch=19)

plot(mod1,which=2,pch=19)

plot(mod1,which=3,pch=19)

plot(mod1,which=4,pch=19)
abline(h=2*4/nrow(d),col=2,lwd=3,lty=2)

plot(mod1,which=5,pch=19)

# 
# I grafici dei residui hanno una configurazione non regolare che suggerisce eteroschedasticit?. Proviamo ora a
# regredire i residui al quadrato del modello sviluppato precedentemente sui regressori. Si nota che il modello
# risulta significativo, cosa che non accadrebbe se i residui stessi fossero omoschedastici.


#-- R CODE
mod2 <- lm(resid(mod1)^2 ~ assets + sales + profits + cash + employ, d) #-- stima modello lineare semplice

summary(mod2)

anova(mod2)


# Da tale procedimento si calcola il valore della varianza dei residui che si ottiene dai valori previsti della
# regressione dei residui al quadrato rispetto ai regressori. La varianza dei residui sar? la varianza di tali valori previsti

#-- R CODE
var(fitted(mod2))

sd(fitted(mod2))

# Il test Durbin Watson non respinge l'ipotesi di non correlazione fra gli errori.

#-- R CODE
white.test(mod2)

dwtest(mod2)


# Per eliminare l'eteroschedasticit? si propone un modello lineare basato su FGLS-WLS avendo costruito errori
# omoschedastici in cui tutte le variabili e gli errori sono divisi per il reciproco dello scarto quadratico medio
# della varianza dei valori previsti.

# Approfondimento: implementazione del metodo FGLS - Feasible Generalized Least Squares
# Si tratta di considerare l'eteroschedasticita' nel processo di stima; i passi principali sono i seguenti:

#   1. Si stima il modello di regressione utilizzando il metodo OLS (Ordinary Least Squares). In questo modo
#   si ottengono i residui OLS
# 
#   2. Si assume un modello che descriva la varianza degli errori in funzione dei regressori (ad esempio, una
#      forma lineare, quadratica, logaritmica, ecc.). Da un punto di vista operativo, cio' equivale ad assumere
#      una relazione fra i residui al quadrato e i regressori. Quindi, si stimano i parametri del modello cosi' specificato
# 
#   3. Si utilizza il modello stimato al punto 2 per ottenere i valori previsti dei residui al quadrato, che
#     rappresentano i valori previsti della varianza.
# 
#   4. In luogo delle variabili di origine Y,X1,X2, ...,Xk si considerano le nuove variabili ^ Y , ^X1, ^X2, ..., ^Xk,
#      ottenute rapportando le variabili di origine ai valori previsti della deviazione standard (ossia, la radice quadrata della varianza)
# 
#   5. Infine, da un punto di vista operativo si hanno due possibilit?. (1) Con il metodo OLS si stima il
#      modello di regressione (senza intercetta) costruito sulle variabili che derivano dal punto 4. (2) Si applica
#      il metodo dei minimi quadrati pesati (Weighted Least Squares) al modello di origine usando come peso
#      il reciproco dei valori stimati della varianza.
#
# Proviamo con la procedura con metodo OLS calcolando quindi le nuove variabili:


#-- R CODE
sd_error <- sqrt(fitted(mod2))

mod3 <- lm( I(mark_val/sd_error) ~ 0 + I(1/sd_error) + I(assets/sd_error) + I(sales/sd_error) + I(profits/sd_error) + I(cash/sd_error) + I(employ/sd_error), d)

summary(mod3)
anova(mod3)

# Proviamo con la procedura con metodo WLS mantenendo quindi le varibili di origine e applicando un vettore
# di pesi. Si noti che i pesi negativi non hanno senso, pertanto ? necessario eliminare le relative osservazioni


#-- R CODE
weight <- 1/fitted(mod2)
mod4 <- lm(mark_val ~ assets + sales + profits + cash + employ, d[-which(weight<0),],weights = weight[-which(weight<0)])
           
                                                                                                                                                                                                 
summary(mod4)

anova(mod4)


# Commentiamo i risultati per modello 3 (mod3). Il modello risulta ancora significativo, anzi il fitting migliora
# (R2=0.8727) e solo "cash" risulta significativa. Inoltre si pu? verificare dal grafico residui-predetti e dai test di White e 
# Durbin Watson che gli errori sono  ora omoschedastici e incorrelati.

#-- R CODE
white.test(mod3)

dwtest(mod3)

# 
# Tuttavia si vede che il modello non usa tutte le osservazioni perch? alcune delle stime delle suddette osservazioni
# hanno varianze negative per limiti computazionali del programma FGLS. Si propone quindi una nuova stima
# del modello basata su esponenziale FGLS che per propriet? della funzione esponenziale non pu? avere stime
# con varianze negative.

mod2 <- lm(log(resid(mod1)^2) ~ assets + sales + profits + cash + employ, d)
sd_error <- sqrt(exp(fitted(mod2)))
mod5 <- lm(I(mark_val/sd_error) ~ 0 + I(1/sd_error) + I(assets/sd_error) + I(sales/sd_error) + I(profits/sd_error) + I(cash/sd_error) + I(employ/sd_error), d)

summary(mod5)

anova(mod5)

#-- R CODE
white.test(mod5)

dwtest(mod5)

# ll modello ora usa tutte le osservazioni, migliora il fitting e si conferma come unica variabile con parametro significativo "cash".
