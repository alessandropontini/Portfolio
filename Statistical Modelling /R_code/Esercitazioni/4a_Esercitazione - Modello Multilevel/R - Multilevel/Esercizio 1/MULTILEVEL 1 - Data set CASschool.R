# MULTILEVEL 1 - Data set: CAS sCHOOL
# Il data set contiene informazioni sulle performance dei test, sulle caratteristiche delle scuole e sulla situazione
# demografica di 420 studenti nei diversi distretti scolastici della California. Ci sono 14 variabili:
# 1. DISTRICT: codice del distretto
# 2. SCHOOL: nome della scuola
# 3. COUNTRY: nome della contea
# 4. GRADES: metodo di voto utilizzato nella contea
# 5. STUDENTS: totale degli studenti nella scuola
# 6. TEACHERS: totale degli insegnanti a tempo pieno
# 7. CALWORKS: percentuale di studenti che rientrano nel programma pubblico assistenziale CalWorks
# 8. LUNCH: percentuale di studenti che hanno diritto ad una riduzione sul prezzo del pranzo
# 9. COMPUTERS: numero di computer per classe
# 10. EXPENDITURE: spesa per studente
# 11. INCOME: reddito medio del distretto (migliaia di USD)
# 12. ENGLISH: percentuale di studenti per cui l'inglese è la seconda lingua
# 13. READ: punteggio megio nel test di lettura
# 14. MATH: punteggio megio nel test di matematica
# Variabile dipendente: MATH
# Analisi proposte:
#   1. Statistiche descrittive
#   2. Analisi multilevel

  


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

install.packages('snakecase', type = 'source')


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
ABSOLUTE_PATH <- "C:\\Users\\daniele.riggi\\Desktop\\Corso Bicocca Vittadini\\07_Esercitazioni Vitta\\4a_Esercitazione - Modello Multilevel\\R - Multilevel\\Esercizio 1"

d <- read.csv(paste0(ABSOLUTE_PATH,"\\CASchools.txt"),sep=" ")


#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- names(d)[6:ncol(d)]



#-- print delle prime 6 righe del dataset
head(d)

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


# REGRESSIONE MULTILEVEL: Empty Model
# Il primo modello proposto è l'empty model.


#-- R CODE
#-- R CODE
mod1 <- lmer(math ~ 1 + (1 | county),d,REML=T) #-- empty model
summary(mod1)


Anova(mod1, type="III")


mod1_null <- lm(math ~ 1,d) #-- modello nullo
anova(mod1,mod1_null) #-- test del rapporto di verosimiglianza

data.frame("ICC"=icc(mod1))



# In questo caso come è noto non esistono variabili esplicative e si vede che il coefficiente interclasse è rilevante
# e pari a 0.273. Il test di verosimiglianza respinge l'ipotesi che il modello non interpreti la variabile dipendente.
# Si propongono poi gli intervalli di confidenza dei parametri casuali inerenti i distretti e quindi la relativa
# rappresentazione grafica.

# 
# The estimated between variance,  ??2 corresponds to the term INTERCEPT in the output of Covariance Parameter Estimates 
# ??2  = 95.96
# and the estimated within variance, ??2, corresponds to the term RESIDUAL in the same output section.
# ??2 = 255.95 
# INTRACLASS CORRELATION
# Tells us the portion of the total variance that occurs between class.
# 95.96 / (95.96  + 255.95 ) = 0.2726834
# To measure the magnitude of the variation among schools in their language test score , 
# we can calculate the plausible values range for these means, based on the between variance we obtained from the model: 
# 653.696 ± 1.96*(95.96)^1/2 = (559.6552, 747.7368).
#



#-- R CODE

par(mfrow=c(1,1))
res <- plot_model(mod1, type = "re", grid = FALSE, show.values=T,title="T",prnt.plot=F, sort.est ="sort.all" )


data_1 =res$data[order(res$data$estimate),]

plotCI(1:nrow(data_1),data_1$estimate,ui=data_1$conf.high, li=data_1$conf.low ,pch=19,scol="blue",xlab="Country",ylab="Estimate")

abline(h=mean(data_1$estimate),col=2,lwd=3,lty=2)


# Si osserva che per pochi distretti si può affermare una chiara superiorità in termini di efficacia rispetto ad
# altri perché l'estremo inferiore di molti si interseca con l'estremo superiore di altri.



# REGRESSIONE MULTILEVEL: Random Intercept
# Si propone ora un random intercept model con variabili di primo livello "calworks"" e "read" e la loro interazione.
# Si è visto che il coefficiente intraclasse si dimezza perché una buona parte della varianza complessiva viene
# spiegata dalla variabili esplicative di primo livello. Tutte le variabili risultano essere significative.

#-- R CODE
mod1 <- lmer(math ~ calworks + read + calworks*read + (1| county),d,REML=F) #-- empty model
summary(mod1)
Anova(mod1, type="III")


data.frame("ICC"=icc(mod1)) #-- ICC



# REGRESSIONE MULTILEVEL: Random Slope


mod1 <- lmer(math ~ calworks + read + calworks*read + (calworks| county),d,REML=T) #-- empty model

summary(mod1)

Anova(mod1, type="III")

data.frame("ICC"=icc(mod1)) #-- ICC

# Il coefficiente di correlazione dovrebbe essere calcolato in modo diverso tenendo conto della correlazione tra i
# coefficienti casuali di 1 e 2 livello che risulta negativa. Il coefficiente intraclasse calcolato in modo da tenere
# conto della correlazione fra coefficienti casuali di 1° e 2° livello vale 0.207. Il modello rimane significativo
# come ogni variabile.  Si presentano ora i grafici per l'intercetta e il parametro di regressione casuale inerente "calworks".




#-- R CODE

res <- plot_model(mod1, type = "re", grid = FALSE, show.values=T,title="T",prnt.plot=F, sort.est ="sort.all" )


data_coef = res[[1]]$data[order(res[[1]]$data$estimate),]
data_int  = res[[2]]$data[order(res[[2]]$data$estimate),]


plotCI(1:nrow(data_coef),data_coef$estimate,ui=data_coef$conf.high, li=data_coef$conf.low ,pch=19,scol="blue",xlab="Country",ylab="Estimate")
abline(h=mean(data_coef$estimate),col=2,lwd=3,lty=2)


plotCI(1:nrow(data_int),data_int$estimate,ui=data_int$conf.high, li=data_int$conf.low ,pch=19,scol="red",xlab="Country",ylab="Estimate")
abline(h=mean(data_int$estimate),col=2,lwd=3,lty=2)

# Si vede come gli intervalli di confidenza si intersecano in gran parte in entrambi i casi e rendono difficile la
# costruzione di una graduatoria





