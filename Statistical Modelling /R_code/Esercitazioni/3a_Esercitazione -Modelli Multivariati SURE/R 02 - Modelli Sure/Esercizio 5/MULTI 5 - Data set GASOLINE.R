# MULTI 4 - Data set: CIGARETTE
# In questo dataset sono contenute 48 osservazioni e le seguenti variabili:
# 1. STATE: stato
# 2. YEAR: anno
# 3. CPI: consumer price index
# 4. POP: popolazione
# 5. PACKPC: numero di pacchetti consumati pro-capite
# 6. INCOME: state personal income
# 7. TAX: tassazione
# 8. AVGPRS: prezzo medio incluse le tasse
# 9. TAXS: tassazione per esercizio
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
ABSOLUTE_PATH <- "C:\\Users\\daniele.riggi\\Desktop\\Esame Vitta - Statistical Modeling\\esercitazioni\\3a_Esercitazione\\R 02 - Modelli Sure\\Esercizio 5"

d <- read.csv(paste0(ABSOLUTE_PATH,"\\Gasoline.csv"),sep=";")

d_au <- d[d$COUNTRY=="AUSTRIA",]
names(d_au) <- paste0(names(d_au),"_AU")

d_be <- d[d$COUNTRY=="BELGIUM",]
names(d_be) <- paste0(names(d_be),"_BE")

d1 <- cbind(d_au,d_be)

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("LGASPCAR_AU","LINCOMEP_AU","LRPMG_AU","LCARPCAP_AU","LGASPCAR_BE","LINCOMEP_BE","LRPMG_BE","LCARPCAP_BE")

#-- print delle prime 6 righe del dataset
head(d1)



# STATISTICHE DESCRITTIVE

# Si propongono la matrice di correlazione tra le variabili e alcune descrittive di base


summary(d1[,VAR_NUMERIC]) #-- statistiche descrittive
cor(d1[,VAR_NUMERIC]) #-- matrice di correlazione

plot(d1[,VAR_NUMERIC],pch=19,cex=.5) #-- scatter plot multivariato




par(mfrow=c(3,3))
for(i in VAR_NUMERIC){
  boxplot(d1[,i],main=i,col="lightblue",ylab=i)
}

par(mfrow=c(3,3))
for(i in VAR_NUMERIC){
  hist(d1[,i],main=i,col="lightblue",xlab=i,freq=F)
}




# REGRESSIONE 1

# L'obiettivo dell'analisi sarà quello di spiegare la variabile "LGASPCAR" tramite i regressori "LCARPCAP", "LINCOMEP", "LRPMG"
#  L'analisi in questione si svolgerà su base regionale considerando due stati: AUSTRIA e BELGIO.


#-- R CODE
mod1_AU <- lm(LGASPCAR_AU ~ LCARPCAP_AU + LINCOMEP_AU + LRPMG_AU, d1)

summary(mod1_AU)

anova(mod1_AU)


# REGRESSIONE 2

#-- R CODE

mod1_BE <- lm(LGASPCAR_BE ~ LCARPCAP_BE + LINCOMEP_BE + LRPMG_BE, d1)

summary(mod1_BE)

anova(mod1_BE)
 

# Entrambi i modelli spiegano bene la variabile dipendente soprattutto nel caso del Belgio. Risultano fortemente
# significativi i parametri relativi a "Lcarcap" in entrambi i casi e "lincompep" più per l'Austria che per il Belgio.
# Simile anche il valore dei loro coefficienti di regressione. Il risultato si interpreta nel senso che "Lcarcap"
# logaritmo numero macchine procapite e "lincompep" logaritmo reddito procapite hanno un legame simile con
# "Lgaspcar" logaritmo consumi per macchina a significare comportamenti simili nell'uso delle automobili nei
# due paesi, sia rispetto all'uso delle macchine in dotazione sia rispetto al reddito.
# Completamente diversi invece i coefficienti "Lrpmq" logaritmo prezzo della benzina logartimo consumo per
# macchima: il segno è sempre negativo ma il valore molto più forte per Austria dove il parametro è fortemente
# significativo rispetto al Belgio dove il parametro non è significativo. Solo in Austria il prezzo della benzina è
# determinante nel livello dei consumi.

# Si propone innanzitutto la matrice di covarianza e correlazione tra valori predetti considerati congiuntamente.

cor(data.frame(resid(mod1_BE),resid(mod1_AU)))

var(data.frame(resid(mod1_BE),resid(mod1_AU)))


# Si considerino ora le stesse regressioni stimate con errori correlati per individui posizionati nella stessa posizione.



#-- R CODE
e1 <- LGASPCAR_AU ~ LCARPCAP_AU + LINCOMEP_AU + LRPMG_AU
e2 <- LGASPCAR_BE ~ LCARPCAP_BE + LINCOMEP_BE + LRPMG_BE
sistema <- list(e1=e1,e2=e2)
mod1 <- systemfit(sistema,"SUR",data=d1)
summary(mod1)


# I modelli interpretano bene le variabili dipendenti, meglio quello relativo al Belgio. Per quanto riguarda
# i singoli parametri i valori sono molto simili a quelli ottenuti con la stima OLS e i parametri significativi
# sono i medesimi. Ciò significa che la correlazione tra individui nella stessa posizione non è elevata come era
# prevedibile visto che gli individui nella stessa posizione non sono gli stessi.


#-- R CODE
par(mfrow=c(1,1))
plot(fitted(mod1)[,1],resid(mod1)[,1],pch=19,xlab="Predicted",ylab="Residual",main="Austria")

hist(resid(mod1)[,1],col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1)[,1]),col=2,lwd=2)


white.test(mod1[[1]][[1]])

dwtest(mod1[[1]][[1]])


#-- R CODE
plot(fitted(mod1)[,2],resid(mod1)[,2],pch=19,xlab="Predicted",ylab="Residual",main="Belgio")
hist(resid(mod1)[,2],col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1)[,2]),col=2,lwd=2)



white.test(mod1[[1]][[2]])

dwtest(mod1[[1]][[2]])

# Si evidenzia l'assenza di correlazione tra gli errori in entrambi i casi.
# Si testano ora le ipotesi che rispettivamente i coefficienti A2 e B2, A3 e B3 e (A2 e B2, A3 e B3) siano uguali
# nelle due equazioni


linearHypothesis(mod1,"e1_LINCOMEP_AU = e2_LINCOMEP_BE",test="FT")

linearHypothesis(mod1,"e1_LCARPCAP_AU = e2_LCARPCAP_BE",test="FT")

linearHypothesis(mod1,"e1_LRPMG_AU = e2_LRPMG_BE",test="FT")


# Si passa a equazioni con regressori differenti, stimati con metodo OLS. Nella equazione 1 appaiono gli stessi
# regressori, nella 2 invece B2 e B4.


#-- R CODE
mod1_BE <- lm(LGASPCAR_BE ~ LCARPCAP_BE + LINCOMEP_BE, d1)
summary(mod1_BE)

anova(mod1_BE)

#-- R CODE
mod1_AU <- lm(LGASPCAR_AU ~ LCARPCAP_AU + LINCOMEP_AU + LRPMG_AU, d1)
summary(mod1_AU)

anova(mod1_AU)

# La correlazione tra valori predetti delle variabile dipendente diminuisce leggermente ma il fitting complessivo
# rimane elevatissimo.


cor(data.frame(resid(mod1_BE),resid(mod1_AU)))
var(data.frame(resid(mod1_BE),resid(mod1_AU)))
  
# Effettuiamo ora una stima Sure sulle due equazioni con differenti regressori.


#-- R CODE
e1 <- LGASPCAR_AU ~ LCARPCAP_AU + LINCOMEP_AU + LRPMG_AU
e2 <- LGASPCAR_BE ~ LCARPCAP_BE + LINCOMEP_BE
sistema <- list(e1=e1,e2=e2)
mod1 <- systemfit(sistema,"SUR",data=d1)
summary(mod1)
  
# 
# Anche in questo caso i cambiamenti sono minimi rispetto al caso OLS.
# Per quanto riguarda i singoli parametri i valori sono molto simili a quelli ottenuti con la stima OLS e i
# parametri significativi sono i medesimi. Ciò significa che la correlazione tra individui nella stessa posizione
# non è elevata come era prevedibile visto che gli individui nella stessa posizione non sono gli stessi.
# Si considerano infine tests sull'uguaglianza dei parametri in differenti equazioni.

linearHypothesis(mod1,"e1_LCARPCAP_AU = -0.5199",test="FT")

linearHypothesis(mod1,"e2_LCARPCAP_BE = -0.6687",test="FT")








