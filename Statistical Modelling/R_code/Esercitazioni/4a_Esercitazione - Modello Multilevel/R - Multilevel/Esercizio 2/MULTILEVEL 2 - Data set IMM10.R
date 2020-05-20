# MULTILEVEL 2 - Data set: IMM10
# In questo data set sono contenuti i dati riferiti a 260 studenti dipartiti in 10 scuole con le seguenti variabili:
# 1. HOMEWORK: numero di ore settimanali implegate per svolgere i compiti di matematica
# 2. MATH: punteggio conseguito nel test di matematica
# 3. MEANSES: media dello stato socio-economico degli studenti nelle singole scuole
# 4. PARENTED: grado di educazione dei genitori
# 5. PERCMIN: presenza di minoranze (in percentuale) nelle singole scuole
# 6. PUBLIC: scuola pubblica (1) o privata (0)
# 7. RACE: razza dello studente. 1=asiatico, 2=ispanico, 3=di colore, 4=bianco, 5=nativo americano
# 8. RATIO: rapporto tra numero di alunni e numero di insegnanti all'interno delle singole scuole
# 9. REGION: codice identificativo della regione in cui è situata la scuola
# 10. SCHID: codice identificativo della scuola
# 11. SCHNUM: scuola frequentata dallo studente
# 12. SCSIZE: dimensioni della scuola (da 1 a 6)
# 13. SCTYPE: tipologia della scuola. 1=pubblica, 2=cattolica, 3=privata con altra religione, 4=privata non
# religiosa
# 14. SES: status socio-economico dello studente
# 15. SEX: genere dello studente. 1=maschio, 2=femmina
# 16. STUID: codice identificativo dello studente
# 17. URBAN: codice identificativo dell'area in cui è sita la scuola
# 18. WHITE: lo studente è di razza bianca (1) oppure appartiene ad un altra razza (0)
# Variabile dipendente: MATH
# Analisi proposte:
# 1. Statistiche descrittive
# 2. Analisi multilevel
  


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
install.packages('TMB', type = 'source')
install.packages("performance")

install.packages('snakecase', type = 'source')

install.packages("glmmTMB")


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
library(snakecase)

library(performance)
library(glmmTMB)

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
ABSOLUTE_PATH <- "C:\\Users\\daniele.riggi\\Desktop\\Esame Vitta - Statistical Modeling\\esercitazioni\\4a_Esercitazione - Modello Multilevel\\R - Multilevel\\Esercizio 2"

d <- read.csv(paste0(ABSOLUTE_PATH,"\\imm10.csv"),sep=";")


#-- Fisso la decima scuola come riferimento
table(d$schnum)

d$schnum <- factor(d$schnum)
contrasts(d$schnum) <- contr.treatment(levels(d$schnum),base=which(levels(d$schnum) == '10'))

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("homework","percmin","math","meanses")

# HOMEWORK: numero di ore settimanali implegate per svolgere i compiti di matematica
# PERCMIN: presenza di minoranze (in percentuale) nelle singole scuole
# MATH: punteggio conseguito nel test di matematica
# MEANSES: media dello stato socio-economico degli studenti nelle singole scuole

#-- print delle prime 6 righe del dataset
head(d)



# STATISTICHE DESCRITTIVE

# Si propongono la matrice di correlazione tra le variabili e alcune descrittive di base.

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


# Proponiamo ora il box plot per la variabile "math" che sarà la variabile dipendente nel modello Multilevel

#-- R CODE
par(mfrow=c(1,1))
boxplot(d$math~d$schnum,main="Math by school",col="lightblue",ylab="math")



# ANALISI DELLA VARIANZA (EFFETTI FISSI) 
# Si consideri ora innanzitutto una analisi della varianza a effetti fissi. schnum - scuola frequentata dallo studente 

#-- R CODE

aggregate(d$math, list(d$schnum), mean)

mod1 <- lm(math ~ schnum,d)
summary(mod1)

anova(mod1)


# Viene respinta l'ipotesi che le medie dei gruppi siano tutte uguali. Analizzandole e avendo come riferimento
# la media del gruppo 10 si osserva ad esempio che la scuola 7 ha un punteggio in math di 14.97 punti più
# elevato rispetto alla scuola 10 stessa, mentre la scuola 2 avrà un punteggio inferiore in media di 5.7. Si passa
# ora al modello empty.



# REGRESSIONE MULTILEVEL: Empty Model

# Modello ANOVA ad effetti casuali (empty model)
  

#-- R CODE
mod2 <- lmer(math ~ (1| schnum),d,REML=T)
summary(mod2)

Anova(mod2, type="III")

mod1_null <- lm(math ~ 1,d)
anova(mod2,mod1_null)

performance::icc(mod2)

res <- plot_model(mod2, type = "re", grid = FALSE, show.values=T,title="T",prnt.plot=F, sort.est ="sort.all" )
data_1 =res$data[order(res$data$estimate),]
plotCI(1:nrow(data_1),data_1$estimate,ui=data_1$conf.high, li=data_1$conf.low ,pch=19,scol="blue",xlab="School",ylab="Estimate")
abline(h=mean(data_1$estimate),col=2,lwd=3,lty=2)



# E' respinta l'ipotesi che il modello non interpreti i dati e dal rapporto tra varianza spiegata e totale si ricava
# un coefficiente intraclasse pari a 0.32 che è elevato, e segnala una buona variabilità fra le scuole nei punteggi
# di matematica. Si propongono poi i valori attesi e gli intervalli di confidenza dei parametri casuali inerenti le
# singole scuole. Per i parametri casuali il modello postula graduatorie basate su valori attesi e intervalli di
# confidenza.
# Come è noto una scuola A può ritenersi superiore a una scuola B in termini di efficacia solo se l'estremo
# inferiore dell'intervallo di confidenza di A sia superiore all'estremo superiore dell'intervallo di confidenza di B.
# Si può notare che 6 scuole su 10 hanno un andamento peggiore rispetto a quello della media generale. Si può
# verificare che solo l'effetto casuale del gruppo 10 è significativo. Si passa poi a proporre il modello random
# intercept introducendo prima la variabile esplicativa "homework".




# REGRESSIONE MULTILEVEL: Random Intercept
# MOdello empty al quale si inserisce una variabile esplicativa  (random  intercept model -  mixed model) 
# Multilevel perchè tiene conto sia della parte di regressione che di analisi della varianza ed è la sintesi dei modelli empty e OLS

#-- R CODE
mod3 <- lmer(math ~ homework + (1| schnum),d,REML=T)
summary(mod3)

Anova(mod3, type="III")

performance::icc(mod3)


res <- plot_model(mod3, type = "re", grid = FALSE, show.values=T,title="T",prnt.plot=F, sort.est ="sort.all" )
data_1 =res$data[order(res$data$estimate),]
plotCI(1:nrow(data_1),data_1$estimate,ui=data_1$conf.high, li=data_1$conf.low ,pch=19,scol="blue",xlab="School",ylab="Estimate")
abline(h=mean(data_1$estimate),col=2,lwd=3,lty=2)

# Il coefficiente di correlazione intraclasse si abbassa di pochissimo (0.28) in quanto si abbassano in uguale
# proporzione varianza spiegata e residua. Il modello interpreta bene i dati e la variabile "homework" risulta
# altresi significativa. Anche il test di 3° tipo degli effetti fissi conferma questa significatività.
# Si propongono i valori attesi e gli intervalli di confidenza dei parametri casuali inerenti i gruppi. Si vede come
# il ranking muti in modo rilevante al caso empty. Ciò mostra che la diversa distribuzione fra le scuole della
# variabile "homework" è all'origine di parte della variabilità di "math" attribuito in prima istanza nel modello
# empty alla efficacia delle scuole.
# Tenere conto di questo non solo modifica l'efficacia complessiva delle scuole ma anche l'efficacia relativa di
# ogni scuola rispetto ad altre. Si può verificare che solo l'effetto casuale del gruppo 7 è significativo e anche in
# questo caso quindi la situazione cambia radicalmente rispetto al modello empty.
# Si aggiunge ora nel modello mixed anche la variabile esplicativa "ses".

#-- R CODE
mod4 <- lmer(math ~ homework + ses + (1| schnum),d,REML=T)
summary(mod4)


Anova(mod4, type="III")

performance::icc(mod4)


res <- plot_model(mod4, type = "re", grid = FALSE, show.values=T,title="T",prnt.plot=F, sort.est ="sort.all" )
data_1 =res$data[order(res$data$estimate),]
plotCI(1:nrow(data_1),data_1$estimate,ui=data_1$conf.high, li=data_1$conf.low ,pch=19,scol="blue",xlab="School",ylab="Estimate")
abline(h=mean(data_1$estimate),col=2,lwd=3,lty=2)


# Il coefficiente di correlazione intraclasse si dimezza (0.136) perché diminuisce la varianza spiegata molto più
# che la varianza complessiva a segnalare che la variabile "ses", molto più che "homework" cattura la variabilità
# di "math". In altre parole molta della variabilità che sembrava doversi attribuire alle scuole è invece dovuta
# alla diversa distribuzione fra le scuole della variabile "ses".
# La diversa composizione socio-economica delle scuole cattura quindi una parte della variabilità della variabile
# dipendente e non può essere quindi attribuita alla diversa efficacia delle scuole. Per il resto anche questo
# modello è significativo come anche le variabili esplicative. Il test di 3° tipo degli effetti fissi conferma la
# significatività di queste variabili.
# Si osserva come sia i valori attesi che gli intervalli di confidenza dei gruppi mutano per la diversa influenza
# dello stato-socioeconomico nelle diverse scuole. Tenere conto di questo non solo modifica l'efficacia complessiva
# delle scuole ma anche l'efficacia relativa di ogni scuola rispetto ad altre. Si può verificare che solo l'effetto
# casuale del gruppo 7 rimane significativo anche se in misura minore che nel caso con variabile esplicativa solo
# "homework" e in parte diviene significativo l'effetto 2 e 4. Si passa ora a un modello random effect in cui
# anche il parametro relativo a "homework" è di tipo casuale.


# REGRESSIONE MULTILEVEL: Random Slope
# Questo modello prvede che i   coefficienti della regressione varino da gruppo a gruppo 

#-- R CODE
mod5 <- lmer(math ~ homework + ses + (homework| schnum),d,REML=T)

summary(mod5)

Anova(mod5, type="III")

install.packages("afex")
library(afex) 

#-- R CODE


res <- plot_model(mod5, type = "re", grid = FALSE, show.values=T,title="T",prnt.plot=F, sort.est ="sort.all" ,value.offset = .3)


data_int  = res[[2]]$data[order(res[[2]]$data$estimate),]

plotCI(1:nrow(data_int),data_int$estimate,ui=data_int$conf.high, li=data_int$conf.low ,pch=19,scol="red",xlab="School",ylab="Estimate")
abline(h=mean(data_int$estimate),col=2,lwd=3,lty=2)


data_coef = res[[1]]$data[order(res[[1]]$data$estimate),]

plotCI(1:nrow(data_coef),data_coef$estimate,ui=data_coef$conf.high, li=data_coef$conf.low ,pch=19,scol="blue",xlab="School",ylab="Estimate")
abline(h=mean(data_coef$estimate),col=2,lwd=3,lty=2)


# Il coefficiente intraclasse non può più essere calcolato nel modo semplice precedente perché si deve tener
# conto della correlazione tra effetti casuali relativi a "homework" e alle scuole nel loro complesso. Gli effetti
# casuali complessivi relativi all'efficacia delle scuole nel loro complesso e "homework" sono significativi anche
# se con un p-value non molto basso. La correlazione tra effetti relativi alle scuole e a "homework" è negativa.
# Inoltre il modello interpreta bene i dati ma la parte fissa della variabile "homework" non è più significativa.
# Questo risultato è confermato anche dal test 3 degli effetti fissi. 
# Per ciò che concerne gli effetti casuali di "homework" diversi tra scuola e scuola non risulta significativo 
# per nessun p-value eccetto quello relativo alla scuola 7; 

# Per le intercette inerenti l'efficacia relativa di ogni scuola solo quello relativo alla scuola 3.

# Si aggiunge ora la variabile SES di primo livello e ratio di secondo.

summary(d$ratio)
#-- R CODE

par(mfrow=c(1,1))
boxplot(d$ratio~d$schnum,main="Ratio by school",col="lightblue",ylab="math")



#-- R CODE
mod6 <- lmer(math ~ homework + ses + ratio + (homework| schnum),d,REML=T)
summary(mod6)

Anova(mod6, type="III")


res <- plot_model(mod6, type = "re", grid = FALSE, show.values=T,title="T",prnt.plot=F, sort.est ="sort.all" )



data_int  = res[[2]]$data[order(res[[2]]$data$estimate),]

plotCI(1:nrow(data_int),data_int$estimate,ui=data_int$conf.high, li=data_int$conf.low ,pch=19,scol="red",xlab="School",ylab="Estimate")
abline(h=mean(data_int$estimate),col=2,lwd=3,lty=2)


data_coef = res[[1]]$data[order(res[[1]]$data$estimate),]

plotCI(1:nrow(data_coef),data_coef$estimate,ui=data_coef$conf.high, li=data_coef$conf.low ,pch=19,scol="blue",xlab="School",ylab="Estimate")
abline(h=mean(data_coef$estimate),col=2,lwd=3,lty=2)



# Il modello interpreta bene i dati e tutti i parametri casuali sia quello relativo alle scuole che a "homework" è
# significativo. La correlazione tra il parametro relativo alle scuole e "homework" rimane negativo. "ses" come
# parametro fisso è significativo come "ratio" per un livello alpha di 0.05, mentre la parte fissa di "homework"
# non è significativa. 
# Tra i parametri casuali inerenti le scuole il 6 e il 9 non sono significativi
# mentre per ciò che concerne homework non lo è il 7. 
# Per entrambi i tipi di parametri casuali il modello postula graduatorie
# basati su valori attesi e intervalli di confidenza. Si aggiunge infine un modello random che ha fra le variabili
# esplicative anche l'interazione fra home e ratio.


#-- R CODE
mod7 <- lmer(math ~ homework + ses + ratio + homework*ratio + (homework| schnum),d,REML=T)

summary(mod7)

Anova(mod7, type="III")

res <- plot_model(mod7, type = "re", grid = FALSE, show.values=T,title="T",prnt.plot=F, sort.est ="sort.all" )



data_int  = res[[2]]$data[order(res[[2]]$data$estimate),]
plotCI(1:nrow(data_int),data_int$estimate,ui=data_int$conf.high, li=data_int$conf.low ,pch=19,scol="red",xlab="School",ylab="Estimate")
abline(h=mean(data_int$estimate),col=2,lwd=3,lty=2)

data_coef = res[[1]]$data[order(res[[1]]$data$estimate),]
plotCI(1:nrow(data_coef),data_coef$estimate,ui=data_coef$conf.high, li=data_coef$conf.low ,pch=19,scol="blue",xlab="School",ylab="Estimate")
abline(h=mean(data_coef$estimate),col=2,lwd=3,lty=2)




# Anche questo modello risulta significativo come tutti i parametri casuali inerenti scuole e "homework" e la
# loro correlazione che rimane negativa. In questo caso tra i parametri fissi però oltre a "homework" risultano
# non significativi anche ratio e l'interazione ratio-homework. 
# Tra i parametri casuali inerenti le scuole il 6,9  non sono significativi 
# Mentre per ciò che concerne homework non lo è il 6 9. 
# Come si vede c'è un altro rilevante cambiamento dei ranking rispetto al modello precedente.









