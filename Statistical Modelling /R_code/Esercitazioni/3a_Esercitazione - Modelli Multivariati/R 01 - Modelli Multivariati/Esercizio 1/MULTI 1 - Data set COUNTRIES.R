# MULTI 1 - Data set: COUNTRIES
# In questo dataset sono 12 variabili su 38 osservazioni:
# 1. REGION: regione della country
# 2. AREA: area della country (Km2)
# 3. IRRIGATED: area di campi irrigati (Km2)
# 4. POPULATION: popolazione in milioni di persone
# 5. UNDER.14: % di popolazione con meno di 14 anni
# 6. LIFE.EXPECTANCY: speranza di vita alla nascita in anni
# 7. LITERACY.RATE: tasso di alfabetismo
# 8. UNEMPLOYMENT: tasso di disoccupazione
# 9. ISPS/MILLION: numero di ISPs per milione di persone
# 10. TVs/PERSON: numero di televisioni per persona
# 11. RAILWAYS: lunghezza in km della rete ferroviaria
# 12. AIRPORTS: numero di aeroporti
# Analisi proposte:
# 1. Statistiche descrittive
# 2. Regressione Multivariata

  


install.packages("car")
install.packages("sjstats")
install.packages("plotrix")ß
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




#-- import dei dati
ABSOLUTE_PATH <- "/Volumes/HDD_Ale/Statistical Modelling/Esercitazioni/3a_Esercitazione - Modelli Multivariati/R 01 - Modelli Multivariati/Esercizio 1"

d <- read.csv(paste0(ABSOLUTE_PATH,"/countries.csv"),sep=";")


#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("Life.expectancy","Unemployment","Literacy.Rate","ISPs.million","Irrigated","Under.14")




#-- print delle prime 6 righe del dataset
head(d)

# STATISTICHE DESCRITTIVE

# Si vuole studiare la dipendenza delle variabili "life_expectancy" e "Unemployment" da "ISPs_million",
# "irrigated", "Under_14", "Literacy_Rate". Si propongono dapprima le statistiche descrittive, a seguire le
# matrici di correlazione tra variabili dipendenti, tra variabili esplicative e tra variabili dipendenti.

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


# Non esistono correlazioni particolarmente forti che facciano pensare a collinearit? o
# legami di dipendenza lineare perfetta. Si propongano ora le regressioni uni variate cominciando ora la variabile dipendente
# "life_expentancy".


# REGRESSIONE


#-- R CODE
mod1 <- lm(Life.expectancy ~ ISPs.million + Irrigated + Under.14 + Literacy.Rate, d)

summary(mod1)

anova(mod1)

#-- R CODE
white.test(mod1)
dwtest(mod1) #-- Durbin-Whatson test


par(mfrow=c(1,1))
#-- R CODE

plot(mod1,which=2,pch=19)

hist(resid(mod1),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1)),col=2,lwd=2)


# Si osserva qualche outlier che andrebbe eliminato

#-- R CODE
plot(hatvalues(mod1),rstudent(mod1),pch=19,xlab="Leverage",ylab="Student - Residual")
abline(h=2,col=2,lty=2,lwd=2)
abline(h=-2,col=2,lty=2,lwd=2)
plot(fitted(mod1),resid(mod1),pch=19,xlab="Predicted",ylab="Residuo",type="p",col=1,lwd=2)
abline(h=0,lwd=2,lty=2,col=2)
plot(mod1,which=1,pch=19)
plot(mod1,which=2,pch=19)
plot(mod1,which=3,pch=19)
plot(mod1,which=4,pch=19)
abline(h=2*4/nrow(d_noaut),col=2,lwd=3,lty=2)

plot(mod1,which=5,pch=19)

#-- R CODE
plot(cooks.distance(mod1),pch=19,xlab="Observation Index",ylab="Cook DIstance",type="h")
points(cooks.distance(mod1),pch=19)
abline(h=4/nrow(d_noaut),col=2,lty=2,lwd=2)



# Si passa ora alla regressione multipla dove la variabile dipendente ? "unemployment." Anche in questo caso
# l'unica variabile significativa rimane "under 14" con un discreto fitting. Gli errori sono anche in questo caso
# omoschedastici e gli errori sono anche non correlati con distribuzione normale.

#-- R CODE
mod2 <- lm(Unemployment ~ ISPs.million + Irrigated + Under.14 + Literacy.Rate, d_noaut)
summary(mod2)

anova(mod2)

#-- R CODE
white.test(mod2, d_noaut)

dwtest(mod2)


plot(mod2,which=2,pch=19)

hist(resid(mod2),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod2)),col=2,lwd=2)

shapiro.test(resid(mod2))


ks.test(resid(mod2),"pnorm")
# Si osserva anceh in questo caso che qualche outlier che andrebbe eliminato:
plot(covratio(mod2), pch = 19, ylab = "Covratio")
abline(h = 1-3*7/nrow(d_noaut), lwd = 3, col = 2, lty = 2)
abline(h = 1+3*7/nrow(d_noaut), lwd = 3, col = 2, lty = 2)
d_noaut[33,]
summary(d_noaut[VAR_NUMERIC])
#-- R CODE
plot(hatvalues(mod2),rstudent(mod2),pch=19,xlab="Leverage",ylab="Student - Residual")
abline(h=2,col=2,lty=2,lwd=2)
abline(h=-2,col=2,lty=2,lwd=2)

plot(cooks.distance(mod2),pch=19,xlab="Observation Index",ylab="Cook DIstance",type="h")
points(cooks.distance(mod2),pch=19)
abline(h=4/nrow(d_noaut),col=2,lty=2,lwd=2)
d_noaut[34,]
plot(mod2,which=1,pch=19)
plot(mod2,which=2,pch=19)
plot(mod2,which=3,pch=19)
plot(mod2,which=4,pch=19)
abline(h=2*4/nrow(d_noaut),col=2,lwd=3,lty=2)

plot(mod2,which=5,pch=19)
# Rinunciando a eliminare gli outlier (provare per esercizio) come sarebbe comunque opportuno si passa ora
# alla regressione multivariata.

d_noaut <- d[-c(FIND_EXTREME_OBSERVARION(d$Irrigated,2)),]
#-- R CODE
mod3 <- lm(cbind(Unemployment,Life.expectancy) ~ ISPs.million + Irrigated + Under.14 + Literacy.Rate, d)

#-- calcolo correlazione parziale tra "Life.expectancy" e "Unemployment"
#-- al netto delle altre variabili



pcor.test(d$Life.expectancy,d$Unemployment,d[,c("ISPs.million","Irrigated","Under.14","Literacy.Rate")])


# Il modello multivariato con le stesse variabili esplicative sotto il profilo descrittivo ? l'accostamento di due
# regressioni multiple che vengono risolte l'una indipendentemente dall' altra perci? gli R2 e le stime dei
# parametri usando il test sono identici.
# La variabile "under 14", come previsto risulta significativa. "Literacy" non risulta significativa

# Si passa ora a verificare ipotesi multiple mediante il test Manova cominciando con le 2 variabili Isps e literacy
# che risultano congiuntamente non significative:


summary(mod3)
summary(mod1)


summary(manova(mod3))

anova(mod3)

#

Anova(mod3, type="III")

mod4 <- update(mod3, . ~ . - ISPs.million - Irrigated - Under.14 - Literacy.Rate )
anova(mod3, mod4)

lh.out <- linearHypothesis(mod3, hypothesis.matrix = c("ISPs.million = 0", "Irrigated = 0", "Under.14 = 0","Literacy.Rate=0"))
lh.out

plot(dffits(mod1),pch=19,ylab="DFFITS",type="h")
abline(h=2*sqrt(4/nrow(d)),lwd=3,col=2,lty=2)
dfbetaPlots(mod1,pch=19,main="DFBETA")
summary(manova(cbind(Life.expectancy, Unemployment) ~ ISPs.million, data = d))
Anova(lm(cbind(Life.expectancy, Unemployment) ~ ISPs.million, data = d),type="III")

summary(manova(cbind(Life.expectancy, Unemployment) ~ Under.14, data = d))
summary(manova(cbind(Life.expectancy, Unemployment) ~ Irrigated, data = d))
summary(manova(cbind(Life.expectancy, Unemployment) ~ Literacy.Rate, data = d))

#-- R CODE
summary(manova(cbind(Life.expectancy, Unemployment) ~ Literacy.Rate + ISPs.million, data = d))

Anova(lm(cbind(Life.expectancy, Unemployment) ~ Literacy.Rate + ISPs.million, data = d),type="III")

summary(manova(cbind(Life.expectancy, Unemployment) ~ Irrigated + Under.14, data = d))

Anova(lm(cbind(Life.expectancy, Unemployment) ~ Irrigated + Under.14, data = d),type="III")

summary(manova(cbind(Life.expectancy, Unemployment) ~ Literacy.Rate + Under.14, data = d))
summary(manova(cbind(Life.expectancy, Unemployment) ~ Under.14 + Literacy.Rate, data = d))
Anova(lm(cbind(Life.expectancy, Unemployment) ~ Literacy.Rate + Under.14, data = d),type="III")
summary(mod3)

head(resid(mod3))
head(fitted(mod3))
coef(mod3)

sigma(mod3)


# https://data.library.virginia.edu/getting-started-with-multivariate-multiple-regression/

# 
# The main takeaway is that the coefficients from both models covary. That covariance needs to be taken into account when determining 
# if a predictor is jointly contributing to both models. 
# For example, the effects of some variables  seem borderline (Irrigated). They appear significant less significant fot Life.expectancy than for Unemployment
# But it's not enough to eyeball the results from the two separate regressions! We need to formally test for their inclusion. 
# And that test involves the covariances between the coefficients in both models.
# Determining whether or not to include predictors in a multivariate multiple regression requires the use of multivariate test statistics. 
# These are often taught in the context of MANOVA, or multivariate analysis of variance. Again the term "multivariate" here refers to 
# multiple responses or dependent variables. This means we use modified hypothesis tests to determine whether a predictor contributes to a model.
# The easiest way to do this is to use the Anova() or Manova() functions in the car package
    
  
# The results are titled "Type III MANOVA Tests". The Anova() function automatically detects that mlm1 is a multivariate multiple regression object. 
# "Type III" refers to the type of sum-of-squares. This basically says that predictors are tested assuming all other predictors are already in the model. 
#   This is usually what we want. Notice that Irrigated, ISPs.million and Literacy.rate appear to be jointly insignificant for the two models despite what we were led to believe 
#   by examining each model separately.
  
#   Based on these results we may want to see if a model with just Life.expectancy and Unemployment fits as well as a model with all 4 predictors. 
#   One way we can do this is to fit a smaller model and then compare the smaller model to the larger model using the anova() function, 
#   (notice the little "a"; this is different from the Anova() function in the car package). For example, below we create a new model using 
#   the update() function that only includes Life.expectancy and Unemployment 
# 
# The large p-value provides good evidence that the model with two predictors fits as well as the model with 4 predictors. 
#   Notice the test statistic is "Pillai", which is one of the four common multivariate test statistics.
#   The car package provides another way to conduct the same test using the linearHypothesis() function. The beauty of this function is that 
#   it allows us to run the test without fitting a separate model. It also returns all four multivariate test statistics. The first argument to the
#   function is our model. The second argument is our null hypothesis. The linearHypothesis() function conveniently allows us to enter this hypothesis 
#   as character phrases. The null entered below is that the coefficients for the 4 predictors are all 0.

# The Pillai result is the same as we got using the anova() function above. The Wilks, Hotelling-Lawley, and Roy results are different versions 
# of the same test. The consensus is that the coefficients for the 4 predictors do not seem to be statistically different from 0. There is 
# some discrepancy in the test results. The Roy test in particular is significant, but this is likely due to the small sample size (n = 17).
  
  # Also included in the output are two sum of squares and products matrices, one for the hypothesis and the other for the error. These matrices 
  # are used to calculate the four test statistics. These matrices are stored in the lh.out object as SSPH (hypothesis) and SSPE (error). We can use 
  # these to manually calculate the test statistics. For example, let SSPH = H and SSPE = E. The formula for the Wilks test statistic is
  
  # wilk |E||E+H|
      E <- lh.out$SSPE
      H <- lh.out$SSPH
      det(E)/det(E + H)

  # Pillai is tr[H(H+E)???1]
    sum(diag(H %*% solve(E + H)))
  
  # Hotelling-Lawley is   tr[HE???1]
  sum(diag(H %*% solve(E)))
  
  # Roy statistics is the largest eigenvalue of  HE???1
  e.out <- eigen(H %*% solve(E))



