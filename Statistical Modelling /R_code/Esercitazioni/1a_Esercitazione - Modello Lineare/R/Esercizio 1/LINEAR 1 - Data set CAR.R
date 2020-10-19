# LINEAR 1 - Data set: CAR

Il dataset 'CAR' è composto da 60 unità statistiche per le quali è riportato il valore di 8 variabili. In
particolare, viene considerato un insieme di 60 automobili per ogniuna delle quali viene misurato il valore
delle seguenti variabili:
1. PRICE: prezzo di listino dell'autovettura (in particolare di un modello standard), espresso in dollari
2. COUNTRY: paese d'origine
3. RELIABILITY: grado di affidabilità (fattore codificato in livelli da 1 a 5)
4. MILIAGE: (consumo di carburante espresso in miglia / dollaro)
5. TYPE: tipologia di autovettura
6. WEIGHT: peso a vuoto misurato in libbre
7. DISP: capacità del motore (cilindrata), in litri
8. HP: potenza del veicolo

Variabile dipendente: PRICE. Le caratteristiche del veicolo sono variabili esplicative (o covariate).

Analisi proposte:
1. Statistiche descrittive
2. Regressione lineare semplice
3. Test di correlazione dei residui
4. Modello quadratico (con e senza outlier)
5. Modelli log lineari




installed.packages("pander")
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
ABSOLUTE_PATH <- "C:\\Users\\daniele.riggi\\Desktop\\Corso Bicocca Vittadini\\07_Esercitazioni Vitta\\1a_Esercitazione - Modello Lineare\\R"

d <- read.csv(paste0(ABSOLUTE_PATH,"\\car.test.txt"),sep=" ")

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("Price","Mileage","Weight","Disp.","HP")

#-- print delle prime 6 righe del dataset
head(d)


summary(d[,VAR_NUMERIC]) #-- statistiche descrittive
cor(d[,VAR_NUMERIC]) #-- matrice di correlazione

plot(d[,VAR_NUMERIC],pch=19,cex=.5) #-- scatter plot multivariato


#-- R CODE
mod1 <- lm(Price~Disp.,d) #-- stima modello lineare semplice
summary(mod1)

anova(mod1)


#-- R CODE
white.test(mod1) #-- white test

dwtest(mod1) #-- Durbin-Whatson test

#-- R CODE
plot(d$Disp.,d$Price,pch=19,col=1,xlab="Disp",ylab="Price") #-- scatter plot

#-- R CODE
mod2 <- lm(Price~Disp.+I(Disp.^2),d) #-- stima del modello quadratico
summary(mod2)

anova(mod2)

#-- R CODE
f_mod2 <- function(x) coefficients(mod2)[1]+coefficients(mod2)[2]*x+coefficients(mod2)[3]*x^2
plot(d$Disp.,d$Price,pch=19,xlab="Disp",ylab="Price")
abline(mod1,col=2,lwd=3) #-- abline del modello lineare
curve(f_mod2,add=T,col="blue",lwd=3,lty=2) #-- abline del modello quadratico

#-- R CODE
white.test(mod2) #-- white test

dwtest(mod2) #-- Durbin-Whatson test

#-- R CODE
d$ESTREME <- 1 #-- inserisco una nuova colonna del dataset con obs. estreme
#-- ora applico la funzione FIND_EXTREME_OBSERVARION(variabile di interesse,fattore).
#-- si include anche l'osservazione 23 come outlier.
d$ESTREME[c(FIND_EXTREME_OBSERVARION(d$Price,2),FIND_EXTREME_OBSERVARION(d$Disp.,2),23)] <- 2
plot(d$Disp.,d$Price,pch=19,col=d$ESTREME,xlab="Disp",ylab="Price")

#-- d_noout è un nuovo data frame senza le osservazioni outlier
d_noout <- d[-c(FIND_EXTREME_OBSERVARION(d$Price,2),FIND_EXTREME_OBSERVARION(d$Disp.,2),23),]

#-- R CODE
mod_noout <- lm(Price~Disp.,d_noout) #-- modello senza outlier
summary(mod_noout)

anova(mod_noout)
white.test(mod_noout) #-- white test


dwtest(mod_noout) #-- Durbin-Whatson test
#-- R CODE
plot(d_noout$Disp.,resid(mod_noout),xlab="Disp",ylab="Residui",pch=19)

plot(d_noout$Disp.,d_noout$Price,xlab="Disp",ylab="Price",pch=19)
abline(mod_noout,col=2,lwd=3)


#-- R CODE
mod2_noout <- lm(Price~Disp.+I(Disp.^2),d_noout)
summary(mod2_noout)

anova(mod2_noout)

#-- R CODE

#-- R CODE
  
f_mod2_noout <- function(x) coefficients(mod2_noout)[1]+coefficients(mod2_noout)[2]*x+coefficients(mod2_noout)[3]*x^2
  
plot(d_noout$Disp.,d_noout$Price,pch=19,xlab="Disp",ylab="Price")
abline(mod_noout,col=2,lwd=3) #-- abline del modello lineare
curve(f_mod2_noout,add=T,col="blue",lwd=3,lty=2) #-- abline del modello quadratico


 #-- R CODE
 white.test(mod2_noout) #-- white test

 dwtest(mod2_noout) #-- Durbin-Whatson test

 #-- R CODE
 mod3_noout <- lm(Price~Disp.+I(Disp.^2)+I(Disp.^3),d_noout) #-- modello cubico
 summary(mod3_noout)


 anova(mod3_noout)

 #-- R CODE
 mod4_noout <- lm(Price~log(Disp.),d_noout) #-- stima modello log lineare
 summary(mod4_noout)
 
 
 anova(mod4_noout)
 
 #-- R CODE
 white.test(mod4_noout)
 
 dwtest(mod4_noout) #-- Durbin-Whatson test
 
 #-- R CODE
 mod5_noout <- lm(I(log(Price))~Disp.,d_noout) #-- stima modello log lineare
 summary(mod5_noout)
 
 anova(mod5_noout)
 
 white.test(mod5_noout) #-- white test
 
 dwtest(mod5_noout) #-- Durbin-Whatson test
 
 #-- R CODE
 mod6_noout <- lm(I(log(Price))~I(log(Disp.)),d_noout) #-- stima modello log lineare
 summary(mod6_noout)
 
 
 anova(mod6_noout)
 white.test(mod6_noout) #-- white test
 dwtest(mod6_noout) #-- Durbin-Whatson test
 
 







