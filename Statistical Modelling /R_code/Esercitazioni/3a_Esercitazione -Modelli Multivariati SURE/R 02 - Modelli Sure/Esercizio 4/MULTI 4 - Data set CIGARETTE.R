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



# install.packages("car")
# install.packages("sjstats")
# install.packages("plotrix")
# install.packages("sjPlot")
# install.packages("sjmisc")
# install.packages("lme4")
# install.packages("pander")
# install.packages("car")
# install.packages("olsrr")
# install.packages("systemfit")
# install.packages("het.test")
# install.packages("ppcor")


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
ABSOLUTE_PATH <- "C:\\Users\\daniele.riggi\\Desktop\\Esame Vitta - Statistical Modeling\\esercitazioni\\3a_Esercitazione\\R 02 - Modelli Sure\\Esercizio 4"

d <- read.csv(paste0(ABSOLUTE_PATH,"\\cigarette.csv"),sep=";")
str(d)

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("cpi","pop","packpc","income","tax")


#-- print delle prime 6 righe del dataset
head(d)


d_ca <- d[d$state=="CA",]
names(d_ca) <- paste0(names(d_ca),"_CA")

str(d_ca)

d_ar <- d[d$state=="AR",]
names(d_ar) <- paste0(names(d_ar),"_AR")

d1 <- cbind(d_ar,d_ca)

d_tx <- d[d$state=="TX",]
names(d_tx) <- paste0(names(d_tx),"_TX")
d2 <- cbind(d_tx,d_ca)

  

# STATISTICHE DESCRITTIVE

# Si propongono la matrice di correlazione tra le variabili e alcune descrittive di base.

summary(d[,VAR_NUMERIC]) #-- statistiche descrittive
cor(d[,VAR_NUMERIC]) #-- matrice di correlazione

plot(d[,VAR_NUMERIC],pch=19,cex=.5) #-- scatter plot multivariato

par(mfrow=c(2,3))
for(i in VAR_NUMERIC){
  boxplot(d[,i],main=i,col="lightblue",ylab=i)
}

par(mfrow=c(2,3))
for(i in VAR_NUMERIC){
  hist(d[,i],main=i,col="lightblue",xlab=i,freq=F)
}




# REGRESSIONE 1

# L'obiettivo dell'analisi sar? quello di spiegare la variabile "packpc" tramite i regressori "cpi", "pop", "income"
# e "tax". L'analisi in questione si svolger? su base regionale considerando due stati: Arkansas e California.

#-- R CODE

mod1_AR <- lm(packpc_AR ~ cpi_AR + pop_AR + income_AR + tax_AR, d1)

summary(mod1_AR)

anova(mod1_AR)

# Il modello spiega molto bene la variabile dipendente packpc (R2 = 0.95), ma le uniche variabili significative
# sono "cpi" ad un livello alpha = 0.01, "income" (alpha = 0.005) e "tax" alpha= 0.01. Vediamo ora cosa accade in California.



# REGRESSIONE 2

#-- R CODE

mod1_CA <- lm(packpc_CA ~ cpi_CA + pop_CA + income_CA + tax_CA, d1)

summary(mod1_CA)


anova(mod1_CA)

 
# Il modello interpreta quasi perfettamente la variabile dipendente e in questo caso le variabili significative
# sono "cpi", "pop".
# Si ricorda che se si sceglie il metodo OLS la regressione multivariata pu? essere costruita per ci? che riguarda
# l'ottenimento del fitting e dei parametri sulla base delle regressioni multiple costruite separatamente.



cor(data.frame(resid(mod1_CA),resid(mod1_AR)))

# La correlazione tra i residui delle variabili dipendenti nelle due equazioni che risulta essere 0.5662. Si passa ora
# a un modello in cui le variabili esplicative rimangano identiche in entrambe le regressioni e vi ? correlazione
# fra gli stessi individui nelle diverse equazioni


e1 <- packpc_AR ~ cpi_AR + pop_AR + income_AR + tax_AR
e2 <- packpc_CA ~ cpi_CA + pop_CA + income_CA + tax_CA
sistema <- list(e1=e1,e2=e2)
mod1 <- systemfit(sistema,"SUR",data=d1)
summary(mod1)


# Si vede la differenza dei risultati rispetto al caso OLS senza correlazione: "cpl_AR" e "income_AR" sono
# chiaramente significative molto pi? di quanto avveniva in precedenza (per alpha = 0.01). Ci? ? dovuto al fatto
# che mentre nel caso precedente esisteva una completa incorrelazione sia fra gli errori appartenenti alla stessa
# equazione che con gli errori dell' altra equazione in questo caso esiste correlazione fra i medesimi individui
# considerati nelle due diverse equazioni. Si verifica ora se il parametro relativo a "cpi_AR" si pu? ritenere in
# questa equazione uguale a quello relativo a "tax_CA".



R1 <- matrix(0,nrow=1,ncol=12)
R1[ 1, 2 ] <- 1
R1[ 1, 10 ] <- -1

linearHypothesis(mod1,R1,test="FT")


# Si verifica ora se i parametri relativi alle medesime variabili "cpi_AR" e "cpi_CA" e "tax_AR" e "tax_CA"
# hanno la stessa influenza nelle due equazioni.


#-- TEST: clp_AR=cpl_CA
R1 <- matrix(0,nrow=1,ncol=10)
R1[ 1, 2 ] <- 1
R1[ 1, 7 ] <- -1

linearHypothesis(mod1,R1,test="FT") 

#-- TEST: tax_AR=tax_CA

R2 <- matrix(0,nrow=1,ncol=10)
R2[ 1, 5 ] <- 1
R2[ 1, 10 ] <- -1
linearHypothesis(mod1,R2,test="FT")


R2
# La prima ? respinta per alpha= 0.01, la seconda solo per alpha = 0.05.

# Passiamo ora a modello SURE con variabili esplicative differenti. In un primo caso le variabili esplicative
# per la prima equazione relativa all'Arkansas sono "cpi", "income", "tax" mentre per la seconda relativa alla
# California "cpi" e "pop" Si propone dapprima la stima OLS.


#-- R CODE
mod1_AR <- lm(packpc_AR ~ cpi_AR + income_AR + tax_AR, d1)
summary(mod1_AR)

anova(mod1_AR)


#-- R CODE
mod1_CA <- lm(packpc_CA ~ cpi_CA + pop_CA, d1)
summary(mod1_CA)

anova(mod1_CA)

# I risultati cambiano ancora. In entrambe le equazioni il fitting rimane molto alto ma nella prima tutte e 3 le
# variabili esplicative "cpi", "income", "tax" risultano significative per un p-value inferiore che in precedenza
# = 0.005 mentre nella seconda entrambe le variabili esplicative "cpl" e "pop" risultano significative per
# p-value rispettivamente alpha = 0.0005 e alpha = 0.01.

#-- R CODE
cor(data.frame(resid(mod1_CA),resid(mod1_AR)))

# Si nota che la correlazione tra i valori stimati delle variabili dipendenti stimate diminuisce sensibilmente
# rispetto al primo caso OLS.


# Si propone il modello con variabili esplicative diverse stimate con il metodo SURE


#-- R CODE
e1 <- packpc_AR ~ cpi_AR + income_AR + tax_AR
e2 <- packpc_CA ~ cpi_CA + pop_CA
sistema <- list(e1=e1,e2=e2)
mod1 <- systemfit(sistema,"SUR",data=d1)
summary(mod1)

# La variabili esplicative rimangono tutte significative ma con parametri e significativit? diversa: alpha = 0.005 per
# cpi_AR e tax_AR, alpha = 0.01 per income_AR e pop_CA e alpha= 0.0005 per cpl_CA. Si testano ora le ipotesi
# che non siano mutati i valori dei parametri per cpl_AR e tax_AR nella prima equazione SURE cambiando le variabili esplicative.



#-- R CODE
linearHypothesis(mod1,"e1_cpi_AR = -249.217",test="FT")

linearHypothesis(mod1,"e1_tax_AR = -1.43426",test="FT")

# Sono respinte le ipotesi che i parametri siano cambiati. Si testa quindi l'ipotesi che non siano mutati i valori
# dei parametri per "cpl_CA" nella seconda equazione SURE.

linearHypothesis(mod1,"e2_cpi_CA = -67.7451",test="FT")

# Anche in questo caso l'ipotesi ? respinta


# ESERCIZIO 2
# Si analizza ora per Texas e California la dipendenza lineare della variabile "packpc" da "income" e "avgprs".

#-- R CODE 
mod1_CA <- lm(packpc_CA ~ income_CA + avgprs_CA, d2)
summary(mod1_CA)

anova(mod1_CA)

#-- R CODE 

mod1_TX <- lm(packpc_TX ~ income_TX + avgprs_TX, d2)
summary(mod1_TX)
anova(mod1_TX)


# In entrambe le equazioni il fitting ? altissimo ma mentre per la California l'unica variabile con parametro
# significativo ? "income" nel Texas ? "avgprs". Si verifica ora omoschedasticit? e incorrelazione dei residui.

#-- R CODE
par(mfrow=c(1,1))
plot(fitted(mod1_CA),resid(mod1_CA),pch=19,xlab="Predicted",ylab="Residual")
text(fitted(mod1_CA),resid(mod1_CA),d2$year_TX,pos=1,cex=0.7)
plot((d2$year_CA),d2$packpc_CA,pch=19,xlab="Observation Index",ylab="packpc")



white.test(mod1_CA)

dwtest(mod1_CA)

#-- R CODE
plot(mod1_CA,which=2,pch=19)
hist(resid(mod1_CA),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1_CA)),col=2,lwd=2)

# WHITE TEST
# H_0 : HOMOSCHEDASTICITY (Low p-Value reject H_0)
# H_1 : HETEROSCHEDASTICITY

# DURBIN WATSON TEST
# H_0 : NO AUTOCORRELATION  (Low p-Value reject H_0)
# H_1 : YES AUTOCORRELATION


# I grafici indicano che i residui sono omoschedastici mentre il valore del test di Durbin-Watson mostra che
# l'ipotesi di non correlazione ? da accettare.
# Sulla normalit? si osservano problemi: c'? uno scostamento abbastanza netto della distribuzione empirica da
# quella teorica. Anche in questo caso necessiterebbe una opportuna correzione. Si consideri ora la seconda
# equazione.

#-- R CODE
plot(fitted(mod1_TX),resid(mod1_TX),pch=19,xlab="Predicted",ylab="Residual")
text(fitted(mod1_TX),resid(mod1_TX),d2$year_TX,pos=1,cex=0.7)

plot(d2$year_TX,d2$packpc_TX,pch=19,xlab="Observation Index",ylab="packpc")

white.test(mod1_TX)
  
dwtest(mod1_TX)

#-- R CODE
plot(mod1_TX,which=2,pch=19)

hist(resid(mod1_TX),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1_TX)),col=2,lwd=2)


# I tre grafici (valori residui- predetti; residui-osservati, predetti-osservati) indicano che i residui sono omoschedastici
# mentre il valore del test di Durbin-Watson mostra che l'ipotesi di non correlazione ? da accettare. Sulla
# normalit? si osservano problemi: c'? uno scostamento abbastanza netto della distribuzione empirica da quella
# teorica. Anche in questo caso necessiterebbe una opportuna correzione.
# Si propone ora il modello multivariato con le stesse variabili esplicative e errori correlati per medesimo
# individuo.


#-- R CODE
e1 <- packpc_CA ~ income_CA + avgprs_CA
e2 <- packpc_TX ~ income_TX + avgprs_TX
sistema <- list(e1=e1,e2=e2)
mod3 <- systemfit(sistema,"SUR",data=d2)
summary(mod3)


# In questo caso vi sono piccoli cambiamenti nei valori dei parametri che non modificano la loro significativit? n?
# il livello del p-value. Si verifica ora se gli errori sono omoschedastici e incorrelati all'interno di ogni equazione.


#-- R CODE
plot(fitted(mod3)[,1],resid(mod3)[,1],pch=19,xlab="Predicted",ylab="Residual",main="CA")
text(fitted(mod3)[,1],resid(mod3)[,1],d2$year_TX,pos=1,cex=0.7)

#-- R CODE
white.test(mod3[[1]][[1]])
dwtest(mod3[[1]][[1]])

#-- R CODE
plot(fitted(mod3)[,2],resid(mod3)[,2],pch=19,xlab="Predicted",ylab="Residual",main="TX")
text(fitted(mod3)[,2],resid(mod3)[,2],d2$year_TX,pos=1,cex=0.7)

white.test(mod3[[1]][[2]])
dwtest(mod3[[1]][[2]])

# I grafici indicano che i residui sono omoschedastici all'interno di ogni equazione mentre il valore del test di
# Durbin-Watson mostra che l'ipotesi di non correlazione ? da accettare.


# ESERCIZIO 3
# Si consideri ora il modello con regressori diversi: "income" per California e "avgprs" per Texas. Iniziamo con
# le stime OLS

#-- R CODE

mod1_CA <- lm(packpc_CA ~ income_CA, d2)
summary(mod1_CA)

anova(mod1_CA)

mod1_TX <- lm(packpc_TX ~ avgprs_TX, d2)
summary(mod1_TX)

anova(mod1_TX)

# I modelli offrono entrambi un fitting molto elevato e le variabili esplicative hanno entrambe per la prima volta
# una significativit? legata a un p-value pari a inferiore a 0.0001 e hanno un legame negativo e pi? rilevante che
# nel modello precedente sulle rispettiva variabili dipendenti. Verifichiamo ora omoschedasticit? e incorrelazione
# degli errori.


#-- R CODE
plot(fitted(mod1_CA),resid(mod1_CA),pch=19,xlab="Predicted",ylab="Residual")
text(fitted(mod1_CA),resid(mod1_CA),d2$year_TX,pos=1,cex=0.7)


#-- R CODE
white.test(mod1_CA)

dwtest(mod1_CA)


#-- R CODE
plot(mod1_CA,which=2,pch=19)

hist(resid(mod1_CA),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1_CA)),col=2,lwd=2)




#-- R CODE
white.test(mod1_TX)

dwtest(mod1_TX)

#-- R CODE
plot(mod1_TX,which=2,pch=19)

hist(resid(mod1_TX),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1_TX)),col=2,lwd=2)


# Anche in questo caso si pu? concludere in linea di massima che esiste eteroschedasticit? anche se il grafico
# valori predetti-osservati si mostra nella parte finale molto distante da una situazione di ottimalit? in cui
# esista una chiara relazione lineare tra detti valori. Si conferma inoltre la non correlazione tra gli errori.
# Passiamo ora alla stima Sure.


#-- R CODE
e1 <- packpc_CA ~ income_CA
e2 <- packpc_TX ~ avgprs_TX
sistema <- list(e1=e1,e2=e2)
mod4 <- systemfit(sistema,"SUR",data=d2)
summary(mod4)



# I valori dei parametri non cambiano quasi per niente. Viene respinta invece l'ipotesi che i parametri relativi a
# "income" e "avgprs" rispettivamente della prima e seconda equazione siano identici.







