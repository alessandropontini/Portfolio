# # INTRODUZIONE
# Il data set contiene informazioni riguardanti 521 università americane alla fine dell'anno accademico 1993/1994.
# Le variabili contenute sono:
# 1. AVE_MAT: indicatore qualitativo della preparazione nelle discipline matematiche
# 2. APPL_RIC: numero di domande di iscrizione ricevute all'inizio dell'anno
# 3. APPL_ACC: numero di domande di iscrizione accettate all'inizio dell'anno
# 4. P_STUD10: percentuale di studenti procenienti dalle prime 10 scuole superiori americane
# 5. COSTI_V: costi medi pro-capite per vitto, alloggio sostenuti nell'anno (dollari)
# 6. COSTI_B: costi medi pro-capite per l'acquisto di libri di testo sostenuti nell'anno (dollari)
# 7. TASSE: tasse universitarie medie pro-capite versate durante l'anno
# 8. STUD_DOC: numero di studenti per docente
# 9. P_LAUR: percentuale di laureati alla fine dell'anno sul totale degli iscritti al primo anno

#VAriabile target =APPL_ACC
# Analisi proposte:
#   1. Statistiche descrittive
#   2. Regressione lineare
data_path <- "/Volumes/HDD_Ale/Statistical Modelling/Esercitazioni/1a_Esercitazione - Modello Lineare/R/Esercizio 7/colleges.csv"

install.packages("pander")
install.packages("car")
install.packages("olsrr")
install.packages("systemfit")
install.packages("het.test")
install.packages("lmtest")
install.packages("olsrr")

  
  #-- R CODE
library(pander)
library(car)
library(olsrr)
library(systemfit)
library(het.test)
library(lmtest)
library(olsrr)

panderOptions('knitr.auto.asis', FALSE)
  
#-- White test function
  white.test <- function(lmod,data){
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
  
df <- read.csv(data_path, sep=";")

str(df)
# statistiche descrittive
var_numeric <- names(df)[2:ncol(df)]

summary(df[,var_numeric])

cor(df[, var_numeric])

plot(df[, var_numeric], pch=17, cex=.5)

par(mfrow=c(3,3))
for(i in var_numeric){
	boxplot(df[,i], main=i, col="lightgreen")
}

par(mfrow=c(3,3))
for(i in var_numeric){
	hist(df[,i], main=i, col="lightgreen", freq=F)
}

# Regressione

mod1 <- lm(appl_acc ~ ave_MAT + appl_ric + p_stud10, df)

summary(mod1)

anova(mod1)

white.test(mod1, df)

dwtest(mod1)
# multicollinearità
ols_vif_tol(mod1)

ols_eigen_cindex(mod1)

# disegni residui
par(mfrow=c(2,2))
plot(df$ave_MAT,resid(mod1),pch=19,xlab="ave_MAT",ylab="Residual")
abline(h=0,lwd=3,lty=2,col=2)
plot(df$appl_ric,resid(mod1),pch=19,xlab="appl_ric",ylab="Residual")
abline(h=0,lwd=3,lty=2,col=2)
plot(df$p_stud10,resid(mod1),pch=19,xlab="p_stud10",ylab="Residual")
abline(h=0,lwd=3,lty=2,col=2)
plot(1:nrow(df),rstudent(mod1),pch=19,xlab="Observation Index",ylab="Residual Studentized",type="h")
abline(h=2,lwd=3,lty=2,col=2)
abline(h=-2,lwd=3,lty=2,col=2)

# normalità
par(mfrow=c(1,1))
plot(mod1,which=2,pch=19)


hist(resid(mod1),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1)),col=2,lwd=2)


shapiro.test(resid(mod1))


ks.test(resid(mod1),"pnorm")