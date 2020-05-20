# MULTILEVEL 3 - Data set: exam
# In questo dataset sono contenute 4059 osservazioni e le seguenti 9 variabili:
# 1. SCHOOL: id della scuola
# 2. NORMEXAM: score ottenuto all'esame normalizzato
# 3. SCHGEND: genere della scuola (mixed, boys, girls)
# 4. SCHAVG: intake score a livello di scuola
# 5. VR: verbal reasoning score a livello di studente
# 6. INTAKE: intake score a livello di studente
# 7. STANDLRT: LR test score
# 8. SEX: genere (M, F)
# 9. TYPE: tipologia di scuola (MXD, SNGL)
# 10. STUDENT: id dello studente
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
install.packages("mvtnorm")
install.packages('emmeans')
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
ABSOLUTE_PATH <- "C:\\Users\\daniele.riggi\\Desktop\\Corso Bicocca Vittadini\\07_Esercitazioni Vitta\\4a_Esercitazione - Modello Multilevel\\R - Multilevel\\Esercizio 3"

d <- read.csv(paste0(ABSOLUTE_PATH,"\\Exam.txt"),sep=" ")

#-- Fisso la decima scuola come riferimento
d$school <- factor(d$school)
contrasts(d$school) <- contr.treatment(levels(d$school),base=which(levels(d$school) == '65'))

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("normexam","schavg","standLRT")

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


# Si propongono poi i box-plot per la variabile dipendente "normexam" per scuola:

#-- R CODE
par(mfrow=c(1,1))
boxplot(d$normexam~d$school,main="Normexam by school",col="lightblue",ylab="Normexam")



# ANALISI DELLA VARIANZA (EFFETTI FISSI)
# Si consideri ora innanzitutto una analisi della varianza a effetti fissi.

#-- R CODE

mod1 <- lm(normexam ~ school,d)

summary(mod1)

anova(mod1)


# Il test F ci mostra che esiste una struttura gerarchica dei dati in quanto ? respinta l'ipotesi nulla che il
# modello non interpreti i dati e che le scuole non siano significative nello spiegare i risultati scolastici. Si
# possono quindi presentare i valori delle intercette relative alle scuole che sono calcolate come differenza dai
# valori attesi generali per il modello e quindi possono essere positive per le scuole pi? efficaci che la media
# delle scuole e negativi per quelle meno efficaci. Si pu? inoltre costruire una graduatoria dell'efficacia delle
# singole scuole. A questo punto si propone l'empty model.


# REGRESSIONE MULTILEVEL: Empty Model

#-- R CODE

mod1 <- lmer(normexam ~ (1| school),d,REML=T)
summary(mod1)

Anova(mod1, type="III")

mod1_null <- lm(math ~ 1,d)
anova(mod1,mod1_null)

data.frame("ICC"=icc(mod1))


res <- plot_model(mod1, type = "re", grid = FALSE, show.values=T,title="T",prnt.plot=F, sort.est ="sort.all" )
data_1 =res$data[order(res$data$estimate),]
plotCI(1:nrow(data_1),data_1$estimate,ui=data_1$conf.high, li=data_1$conf.low ,pch=19,scol="blue",xlab="School",ylab="Estimate")
abline(h=mean(data_1$estimate),col=2,lwd=3,lty=2)


dago
# Il modello interpreta bene i dati ma l'intercetta, unico effetto fisso non ? significativo. Il coefficiente di
# correlazione intraclasse non ? insignifiante bench? non particolaremnte elevato trattandosi di un modello
# empty.
# Si propongono quindi gli effetti casuali relativi ad ogni scuola che, come ? si ? detto sono espressi in termini di
# differenza dal valore atteso generale. Alcuni sono positivi altri negativi. Tra essi alcuni sono significativi sia
# pure per diversi livelli di significativit?, vale come ? noto il confronto tra diverse scuole in termini di efficacia
# non ? svolto sulla base dei valori attesi ma in termini di intervalli di confidenza che appaiono nelle ultime due
# colonne: una scuola A ? pi? efficace di un'altra B se l'estremo inferiore dell' intervallo di confidenza di A ?
# superiore all'estremo superiore dell'intervallo di confidenza di B.
# Si propone ora un mixed model con variabili esplicative "sex", "intake" e "standLRT".



# REGRESSIONE MULTILEVEL: Random Intercept

#-- R CODE
mod1 <- lmer(normexam ~ sex + intake + standLRT + (1| school),d,REML=T)
summary(mod1)

Anova(mod1, type="III")

data.frame("ICC"=icc(mod1))


res <- plot_model(mod1, type = "re", grid = FALSE, show.values=T,title="T",prnt.plot=F, sort.est ="sort.all" )
data_1 =res$data[order(res$data$estimate),]
plotCI(1:nrow(data_1),data_1$estimate,ui=data_1$conf.high, li=data_1$conf.low ,pch=19,scol="blue",xlab="School",ylab="Estimate")
abline(h=mean(data_1$estimate),col=2,lwd=3,lty=2)


# Tutte i variabili esplicative sono risultano significativi. Si passa ora al mixed model con tutte le variabili
# esplicative.

#-- R CODE
mod1 <- lmer(normexam ~ vr + intake + sex + type + schgend + (1| school),d,REML=T)
summary(mod1)

Anova(mod1, type="III")

data.frame("ICC"=icc(mod1))


res <- plot_model(mod1, type = "re", grid = FALSE, show.values=T,title="T",prnt.plot=F, sort.est ="sort.all" )
data_1 =res$data[order(res$data$estimate),]
plotCI(1:nrow(data_1),data_1$estimate,ui=data_1$conf.high, li=data_1$conf.low ,pch=19,scol="blue",xlab="School",ylab="Estimate")
abline(h=mean(data_1$estimate),col=2,lwd=3,lty=2)

# Il modello interpreta bene i dati e il coefficiente intraclasse diminuisce leggermente rispetto al precedente
# modello. Si passa ora la modello total effects che contiene due variabili esplicative, una con parametro casuale
# "standLRT" e l'altra con effetto fisso "schgend".



# REGRESSIONE MULTILEVEL: Random Slope

#-- R CODE
mod1 <- lmer(normexam ~ standLRT + schavg + (standLRT| school),d,REML=T)

summary(mod1)

Anova(mod1, type="III")


#-- R CODE

res <- plot_model(mod1, type = "re", grid = FALSE, show.values=T,title="T",prnt.plot=F, sort.est ="sort.all" )


data_coef = res[[1]]$data[order(res[[1]]$data$estimate),]
data_int  = res[[2]]$data[order(res[[2]]$data$estimate),]


plotCI(1:nrow(data_coef),data_coef$estimate,ui=data_coef$conf.high, li=data_coef$conf.low ,pch=19,scol="blue",xlab="School",ylab="Estimate")
abline(h=mean(data_coef$estimate),col=2,lwd=3,lty=2)


plotCI(1:nrow(data_int),data_int$estimate,ui=data_int$conf.high, li=data_int$conf.low ,pch=19,scol="red",xlab="School",ylab="Estimate")
abline(h=mean(data_int$estimate),col=2,lwd=3,lty=2)


# Il modello interpreta bene i dati e sia i parametri casuali relativi a intercetta che la variabile esplicativa
# risultano significativi come anche il coefficiente di correlazione di valore positivo. I parametri fissi (la parte
# fissa del parametro casuale relativo a "standLRT" scomponibile in una parte propriamente casuale e una
# fissa e il parametro relativo a "schavg") sono entrambi significativi. Il test di tipo 3 sugli effetti fissi viene
# effettuato con la variabile casuale F invece che con la t ma d? risultati identici perch? i valori di F non sono
# altro che i quadrati dei valori di t. Si propone ora un altro random model con "intake" come variabile esplicativa con parametro fisso e "standRLT"
# con parametro casuale.


#-- R CODE
mod1 <- lmer(normexam ~ intake + (standLRT| school),d,REML=T)
summary(mod1)

Anova(mod1, type="III")


res <- plot_model(mod1, type = "re", grid = FALSE, show.values=T,title="T",prnt.plot=F, sort.est ="sort.all" )


data_coef = res[[1]]$data[order(res[[1]]$data$estimate),]
data_int  = res[[2]]$data[order(res[[2]]$data$estimate),]


plotCI(1:nrow(data_coef),data_coef$estimate,ui=data_coef$conf.high, li=data_coef$conf.low ,pch=19,scol="blue",xlab="School",ylab="Estimate")
abline(h=mean(data_coef$estimate),col=2,lwd=3,lty=2)


plotCI(1:nrow(data_int),data_int$estimate,ui=data_int$conf.high, li=data_int$conf.low ,pch=19,scol="red",xlab="School",ylab="Estimate")
abline(h=mean(data_int$estimate),col=2,lwd=3,lty=2)



# Si propongono ora un ultimo modello:
  
#-- R CODE
mod1 <- lmer(normexam ~ standLRT + (intake| school),d,REML=T)
summary(mod1)



Anova(mod1, type="III")

data.frame("ICC"=icc(mod1))#-- ICC

res <- plot_model(mod1, type = "re", grid = FALSE, show.values=T,title="T",prnt.plot=F, sort.est ="sort.all" )


data_coef = res[[1]]$data[order(res[[1]]$data$estimate),]
data_int  = res[[2]]$data[order(res[[2]]$data$estimate),]


plotCI(1:nrow(data_coef),data_coef$estimate,ui=data_coef$conf.high, li=data_coef$conf.low ,pch=19,scol="blue",xlab="School",ylab="Estimate")
abline(h=mean(data_coef$estimate),col=2,lwd=3,lty=2)


plotCI(1:nrow(data_int),data_int$estimate,ui=data_int$conf.high, li=data_int$conf.low ,pch=19,scol="red",xlab="School",ylab="Estimate")
abline(h=mean(data_int$estimate),col=2,lwd=3,lty=2)


?lmer
