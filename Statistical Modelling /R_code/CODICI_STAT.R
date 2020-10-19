#################################
#   LIBRERIE UTILIZZATE
#################################
library(pander)
library(car)
library(olsrr)
library(systemfit)
library(het.test)
library(lmtest)
library(Hmisc)
library(sjstats)
library(plotrix)
library(sjPlot)
library(sjmisc)
library(lme4)
library(ppcor)
library(snakecase)
library(performance)
library(glmmTMB)
library(afex) 
library(moments)
library(ggplot2)
library(ggcorrplot)
library(lindia)
library(survival)
#################################
#   CODICI WHITE TEST E OUTLIER
#################################
white.test <- function(lmod,data){
  u2 <- lmod$residuals^2
  y <- fitted(lmod)
  Ru2 <- summary(lm(u2 ~ y + I(y^2)))$r.squared
  LM <- nrow(data)*Ru2
  p.value <- 1-pchisq(LM, 2)
  data.frame("Test statistic"=LM,"P value"=p.value)
}

FIND_EXTREME_OBSERVARION <- function(x,sd_factor=2){
  which(x>mean(x)+sd_factor*sd(x) | x<mean(x)-sd_factor*sd(x))
}

#################################
#   STATISTICHE DESCRITTIVE
#################################
VAR_NUMERIC <- c('X1', 'X2', ETC....)

# MEDIA, MEDIANA, QUANTILI 
summary(df[,VAR_NUMERIC])

# CORRELOGRAMMA TRA VARIABILI
cor(df[,VAR_NUMERIC])
# ggplotcorr
corr <- round(cor(df[,VAR_NUMERIC]), 2)
p.mat <- cor_pmat(df[,VAR_NUMERIC])
ggcorrplot(corr, hc.order = F, lab = T, 
           p.mat = p.mat,
           outline.color = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))
# DISEGNO DEL CORRELOGRAMMA
plot(df[,VAR_NUMERIC], pch=17)
?aes
# BISOGNA INDICARE QUANTI DISEGNI DA METTERE IN UN FOGLIO SINGOLO + BOXPLOT
par(mfrow=c(RIGA,COLONNA))
for(i in VAR_NUMERIC){
  boxplot(df[,i], main = i, col="lightgreen")
}

# ISTOGRAMMA DELLA DISTRIBUZIONE
par(mfrow=c(RIGA,COLONNA))
for(i in VAR_NUMERIC){
  hist(df[,i], main = i, col="lightgreen", xlab=i, freq=F)
}
#################################
#   TIPI REGRESSIONE SEMPLICE
#################################

# LOG/LOG
LOGLOG <- lm(I(log(Y))~I(log(X)),df) 

# LOG/LIN
LOGLIN <- lm(I(log(Y))~X,df)

# LIN/LOG
LINLOG <- lm(Y~log(X),df)

# POLINOMIALE
LINEARE <- lm(Y~X+,df)
QUADRATICO <- lm(Y~X+I(X^2),df)
CUBICO <- lm(Y~X+I(X^2)+I(X^3),df)

#################################
#      MODELLI LINEARI
#################################

# ELIMINARE OUTLIER CON 23 SINGOLA OSSERVAZIONE E POI USARE NUOVO DATASET
DATASENZAOUT <- df[-c(FIND_EXTREME_OBSERVARION(d$Y,2),FIND_EXTREME_OBSERVARION(d$X,2),23),]

# DESCRIVERE MODELLO DI REGRESSIONE APPENA FATTO

# CONTROLLO SIGNIFICATIVITA'VARIABILI E MODELLO
summary(MODELLODACAMBIARE)

# ANALISI DELLE VARIANZE MODELLO
anova(MODELLODACAMBIARE)

# ETEROSCHEDASTICITA' DA CONTROLLARE PVALUE MAGGIORE 0.1
white.test(MODELLODACAMBIARE, df)


# CONTROLLARE SE ERRORI SONO INCORRELATI VALORE TEST
dwtest(MODELLODACAMBIARE)

#  CONTROLLARE EVENTUALE MULTICOLLINEARITA' TOL VICINO A 1 E VIF MINORE DI 10
# VIF + TOL 
ols_vif_tol(MODELLODACAMBIARE)

# CINDEX SE VALORE MAGGIORE DI 30 GUARDO I VALORE
ols_eigen_cindex(MODELLODACAMBIARE)

# ENTRAMBI CONTEMPORANEAMENTE
ols_coll_diag(MODELLODACAMBIARE)

# SE VOGLIO FARE SBORONE
# QUESTO PIU' COMPLESSO
VARNUMERIC_x <- c("X1", "X2", "X3", "X4")
X <- df[VARNUMERIC_x]
VARNUMERIC_y <- c("Price")
Y <- df[VARNUMERIC_y]
imcdiag(x = X, y = Y)

# NORMALITA' DEI RESIDUI PVALUE MAGGIORE VALORE SOGLIA TIPO 0.05
shapiro.test(resid(MODELLODACAMBIARE))
ks.test(resid(MODELLODACAMBIARE),"pnorm",exact = TRUE, sd=sd(resid(MODELLODACAMBIARE)), mean=mean(resid(MODELLODACAMBIARE)))
# SKEWNESS
agostino.test(MODELLODACAMBIARE)
skewness(MODELLODACAMBIARE)

# KURTOSIS
anscombe.test(MODELLODACAMBIARE)
kurtosis(MODELLODACAMBIARE)
################################################################
#   SISTEMO ETEROSCHEDASTICITA' CON INCORRELAZIONE
################################################################

#############################################
#      MODELLI WLS SE O NA VADO A FGLSEXP
#############################################
MODELLORESIDUI <- lm(resid(MODELLODACAMBIARE)^2 ~ X1 + X2 + X3, df)
weight <- 1/fitted(MODELLORESIDUI )
MODELLOWLS <- lm(Y ~ X1 + X2 + X3, df[-which(weight<0),],weights = weight[-which(weight<0)])

################################################################
#   SISTEMO ETEROSCHEDASTICITA' CON CORRELAZIONE
################################################################

################################################
#      MODELLI FGLS SE O NA VADO A FGLSEXP
################################################
MODELLORESIDUI <- lm(resid(MODELLODACAMBIARE)^2 ~ X1 + X2 + X3, df)
sd_error <- sqrt(fitted(MODELLORESIDUI))
MODELLOFGLS <- lm( I(Y/sd_error) ~ 0 + I(1/sd_error) + I(X1/sd_error) + I(X2/sd_error) + I(X3/sd_error), df)

#############################
#      MODELLI FGLSEXP
#############################
MODELLORESIDUI <- lm(log(resid(MODELLODACAMBIARE)^2) ~ X1 + X2 + X3, df)
sd_error <- sqrt(exp(fitted(MODELLODACAMBIARE)))
MODELLOFGLSEXP <- lm( I(Y/sd_error) ~ 0 + I(1/sd_error) + I(X1/sd_error) + I(X2/sd_error) + I(X3/sd_error), df)

################################################################
#   SISTEMO CORRELAZIONE CON OMOSCHEDASTICITA'
################################################################

#############################
#      MODELLI GLS 
#############################

# SI OTTENGONO AUTOVALORI DEI TEMPI 
# METODO 1
autocorr <- acf(resid(MODELLODACAMBIARE),main="Autocorrelazion",lwd=2)
data.frame(LAG=autocorr$lag,VALUE=autocorr$acf)[1:5,]

# METODO 2
cor(resid(MODELLODACAMBIARE),c(NA,resid(MODELLODACAMBIARE)[1:(length(resid(MODELLODACAMBIARE))-1)]),use="pairwise.complete.obs")

# SUCCESSIVAMENTE SU REGREDISCONO I RISULTATI OLS "res" su OLS "res_1":
# METODO 1
d1 <- data.frame(
  MODELLODACAMBIARE$model,
  resid=resid(MODELLODACAMBIARE),
  resid_l1=c(NA,resid(MODELLODACAMBIARE)[1:(length(resid(MODELLODACAMBIARE))-1)]) #-- residui ritardati
)

MODELLORITARDATO1TEMPO <- lm(resid ~ resid_l1,d1)

# METODO 2 MANUALE
d1 <- data.frame(
  MODELLODACAMBIARE$model,
  resid=resid(MODELLODACAMBIARE)
)

# SI PRENDE RITARDO INTERESSATO CALCOLATO PRECEDENTEMENTE 
d1$Y_l1 <- Lag(d1$Y,1)
d1$X1_l1 <- Lag(d1$X1,1)
d1$X2_l1 <- Lag(d1$X2,1)
d1$resid_l1 <- Lag(d1$resid,1)

d1$int_tild <- 1-RITARDO
d1$Y_t <- d1$Y-RITARDO*d1$Y_l1
d1$X1_t <- d1$X1-RITARDO*d1$X1_l1
d1$X2_t <- d1$X2-RITARDO*d1$X2_l1
d1$resid_t <- d1$resid-RITARDO*d1$resid_l1


MODELLORITARDATO1TEMPO <- lm(Y_t ~ 0 + int_tild + X1_t + X2_t ,d1)

# METODO 3 SEMPRE MANUALE E SEMPRE 1

d1 <- df
d1$resid <- resid(MODELLODACAMBIAR)
d1$resid_l1 <- Lag(d1$resid,1)
d1$Y_t <- d1$Y-RITARDO*Lag(d1$Y,1)
d1$X1_t <- d1$X1-RITARDO*Lag(d1$X1,1)
d1$X2_t <- d1$X2-RITARDO*Lag(d1$X2,1)
d1$int_tild <- 1-RITARDO

MODELLORITARDATO1TEMPO <- lm(Y_t ~ 0 + int_tild + X1_t + X2_t ,d1)

# MODELLI AUTOREGRESSIVI PER CUI DECIDI CHE TEMPO USARE DI RITARDO

# 1 TEMPO CONTROLLO POI I COEFFICIENTI E IL DW TEST PER VEDERE SE HA CORRETTO
MODELLORITARDATO1TEMPO <- arima(df$Y, order=c(1,0,0), xreg = df[,c("X1","X2")],method="ML")

# ANALISI POST
MODELLORITARDATO1TEMPO
summary(MODELLORITARDATO1TEMPO)
coeftest(MODELLORITARDATO1TEMPO)
durbinWatsonTest(as.numeric(MODELLORITARDATO1TEMPO$residuals))


# 2 TEMPO CONTROLLO POI I COEFFICIENTI E IL DW TEST PER VEDERE SE HA CORRETTO
MODELLORITARDATO2TEMPO <- arima(df$Y, order=c(2,0,0), xreg = df[,c("X1","X2")],method="ML")

# ANALISI POST
MODELLORITARDATO2TEMPO
summary(MODELLORITARDATO2TEMPO)
coeftest(MODELLORITARDATO2TEMPO)
durbinWatsonTest(as.numeric(MODELLORITARDATO2TEMPO$residuals), max.lag = 2)

# 3 TEMPO CONTROLLO POI I COEFFICIENTI E IL DW TEST PER VEDERE SE HA CORRETTO
MODELLORITARDATO3TEMPO <- arima(df$Y, order=c(3,0,0), xreg = df[,c("X1","X2")],method="ML")

# ANALISI POST
MODELLORITARDATO3TEMPO
summary(MODELLORITARDATO3TEMPO)
coeftest(MODELLORITARDATO3TEMPO)
durbinWatsonTest(as.numeric(MODELLORITARDATO3TEMPO$residuals), max.lag = 2)

####################################
#   SISTEMO NORMALITA' MODELLO 
####################################

# PER SISTEMARLA DEVO PROCEDERE A DIVERSE TRASFORMAZIONI PRECEDENTI SIA PER LA VARIABILE TARGET
# SIA NEL CASO PER I REGRESSORI

# ESEMPIO TRASFORMAZIONE DOPO UN FGLS

# 1 TRASFORMAZIONE LOGY Y DEVE ESSERE MAGGIORE DI 0
df$Y_log_val_sd <- log(d$Y/sd_error)

# CONTROLLO SE TRASFORMAZIONE HA SENSO PER LA VARIABILE
hist(df$Y_log_val_sd,pch=17,cex=.5,col='lightgreen', freq = F)
lines(df$Y_log_val_sd, col=2, lwd=2)

# ANALITICO
shapiro.test(df$Y_log_val_sd)

# 2 TRASFORMAZIONE Yˆ2
d$Y_quadr_val_sd <- (df$Y/sd_error)^2

# CONTROLLO SE TRASFORMAZIONE HA SENSO PER LA VARIABILE
hist(df$Y_quadr_val_sd,pch=17,cex=.5,col='lightgreen', freq = F)
lines(df$Y_quadr_val_sd, col=2, lwd=2)

# ANALITICO
shapiro.test(df$Y_quadr_val_sd)

# 3 TRASFORMAZIONE RADICE DI Y
d$Y_sqrt_val_sd <- sqrt((df$Y/sd_error))

# CONTROLLO SE TRASFORMAZIONE HA SENSO PER LA VARIABILE
hist(df$Y_sqrt_val_sd,pch=17,cex=.5,col='lightgreen', freq = F)
lines(df$Y_sqrt_val_sd, col=2, lwd=2)

# ANALITICO
shapiro.test(df$Y_sqrt_val_sd)

# 4 TRASFORMAZIONE RECIPROCO DI Y
d$Y_rec_val_sd <-(df$Y/sd_error)^-1

# CONTROLLO SE TRASFORMAZIONE HA SENSO PER LA VARIABILE
hist(df$Y_rec_val_sd,pch=17,cex=.5,col='lightgreen', freq = F)
lines(df$Y_rec_val_sd, col=2, lwd=2)

# ANALITICO
shapiro.test(df$Y_rec_val_sd)

###############################################################################################
#   NOTA BENE SE NON FUNZIONA CON LA VARIABILE TARGET BISOGNA FARLO ANCHE PER I REGRESSORI
###############################################################################################

#############################
#   MODELLI MULTIVARIATI
#############################

####################################################################################################################
#   NOTA BENE BISOGNA SISTEMARE LE SINGOLE EQUAZIONI E SUCCESSIVAMENTE FARE ANALISI SU ENTRAMBE LE REGRESSIONI
####################################################################################################################
d$dummy_cat <- ifelse(d$relig==1,1,0)
d$dummy_ort <- ifelse(d$relig==2,1,0)
d$dummy_prot <- ifelse(d$relig==3,1,0)
# CALCOLARE CORRELAZIONE DELLE VARIABILI TARGET AL NETTO DEGLI STESSI REGRESSORI 
pcor.test(df$Y1,df$Y2,d[,c("X1","X2","X3")])

# CALCOLARE CORRELAZIONE DELLE VARIABILI TARGET SENZA REGRESSORI 
cor(df$Y1,df$Y2)

# CALCOLO REGRESSIONE MULTIPLA
MODELLOMULTIVARIATO <- lm(cbind(Y1,Y2) ~ X1 + X2 + X3, df)

# VEDO COME SI COMPORTA IL MODELLO E VEDO LA VARIABILITA' SPIEGATA SEMBRANO IDENTICI CONTROLLARE
summary(manova(MODELLOMULTIVARIATO))
anova(MODELLOMULTIVARIATO)

# VEDO LA VARIABILITA' CONSIDERANDO GLI EFFETTI MOBILI E LE INTERAZIONI DEI REGRESSORI 
Anova(MODELLOMULTIVARIATO, type="III")

# CONTROLLO SE MODELLO CREATO HA SIGNIFICATIVITA' CREANDO MODELLO NULLO E GUARDANDO SIGNIFICATIVITA
MODELLOMULTIVUOTO <- update(MODELLOMULTIVARIATO, . ~ . - X1 + X2 + X3)
anova(MODELLOMULTIVARIATO,MODELLOMULTIVUOTO)

TESINULLA <- linearHypothesis(MODELLOMULTIVARIATO, hypothesis.matrix = c("X1 = 0", "X2 = 0", "X3 = 0"))
TESINULLA

# SUCCESSIVAMENTE DEVO VEDERE COME I SINGOLI REGRESSORI O COPPIE O TRIS ETC.. SI COMPORTANO CON LE VARIABILI TARGET 
summary(manova(cbind(Y1,Y2) ~ X1, data = df))
Anova(lm(cbind(Y1,Y2) ~X1, data = df),type="III")

# OPPURE
summary(manova(cbind(Y1,Y2) ~ X1 + X2, data = df))
Anova(lm(cbind(Y1,Y2) ~X1 + X2, data = df),type="III")

#############################
#      MODELLI SURE
#############################

####################################################################################################################
#   NOTA BENE BISOGNA SISTEMARE LE SINGOLE EQUAZIONI E SUCCESSIVAMENTE FARE ANALISI SU ENTRAMBE LE REGRESSIONI
####################################################################################################################

#######################################################################################################################
#   NOTA BENE 2 BISOGNA CERCARE ALL'INTERNO DEI DAI UNA STRUTTURA AD ESEMPIO NAZIONI, STATI, AZIENDE ED AGGIUNGERCI 
#   LE DESINENZE FINALI. COSI POI POSSIAMO USARE QUELLE PER LE NOSTRE ANALISI SURE
#######################################################################################################################
d_ca <- df[d$state=="CA",]
names(d_ca) <- paste0(names(d_ca),"_CA")

str(d_ca)

d_ar <- df[d$state=="AR",]
names(d_ar) <- paste0(names(d_ar),"_AR")

d1 <- cbind(d_ar,d_ca)

# CONTROLLO CORRELAZIONE TRA I MODELLI SISTEMATI PER CA E AR
cor(data.frame(resid(MODELLO_CA),resid(MODELLO_AR)))

# FACCIO MODELLO SURE
e1 <- Y_AR ~ X1_AR + X2_AR + X3_AR
e2 <- Y_CA ~ X1_CA + X2_CA + X3_CA
sistema <- list(e1=e1,e2=e2)
MODELLOSURE <- systemfit(sistema,"SUR",data=d1)
summary(MODELLOSURE)

######################################################
#   DEVO CONTROLLARE LE MATRICI POI SIGNIFICATIVITA 
######################################################

# FACCIO TEST DI IPOTESI FRA DUE VARIABILI DIVERSE O UGUALI PER SOSTANZA
# DELLE DUE DIVERSE EQUAZIONI PER VEDERE SE SONO UGUALI
R2 <- matrix(0,nrow=1,ncol=REGRESSORI+INTERCETTA)
R2[ 1, 5 ] <- 1
R2[ 1, 10 ] <- -1
linearHypothesis(MODELLOSURE,R2,test="FT")

# POSSO FARLO TRA VALORI DIVERSI PER MODELLI DIVERSI
linearHypothesis(MODELLOSURE2,"VALOREMODELLOSURE = -1.43426",test="FT")

############################################################################################
#   INIZIO A ITERARE NEL TOGLIERE E METTERE VARIABILI FINCHE MODELLO NON MI SODDISFA
#   NOTA BENE POSSONO ESSERE VARIABILI DIVERSE PER LE DUE EQUAZIONI
############################################################################################
e1 <- Y_AR ~ X2_AR + X3_AR
e2 <- Y_CA ~ X1_CA + X2_CA 
sistema <- list(e1=e1,e2=e2)
MODELLOFINALESURE <- systemfit(sistema,"SUR",data=d2)
summary(mod3)

# PLOT PER MULTI SURE EQUAZIONE E1
plot(fitted(mod3)[,1],resid(mod3)[,1],pch=19,xlab="Predicted",ylab="Residual",main="CA")
text(fitted(mod3)[,1],resid(mod3)[,1],d2$year_TX,pos=1,cex=0.7)

# TEST PER EQUAZIONE E1
white.test(mod3[[1]][[1]], df)
dwtest(mod3[[1]][[1]])

# PLOT PER MULTI SURE EQUAZIONE E2
plot(fitted(mod3)[,2],resid(mod3)[,2],pch=19,xlab="Predicted",ylab="Residual",main="TX")
text(fitted(mod3)[,2],resid(mod3)[,2],d2$year_TX,pos=1,cex=0.7)

# TEST PER EQUAZIONE E2
white.test(mod3[[1]][[2]], df)
dwtest(mod3[[1]][[2]])

######################################
#      MODELLI MULTILEVEL
######################################

###########################################################################################################
#   NOTA BENE DOBBIAMO CONTROLLARE LA STRUTTURA DEI DATI E DECIDERE SU CHE COSA VOGLIAMO FARE RANKING
#   METTIAMO DI AVERE DELLE SCUOLE BISOGNA INANZITUTTO PRENDERNE UNA COME RIFERIMENTO 
###########################################################################################################
table(df$SCUOLE)
d$SCUOLE <- factor(df$SCUOLE)
contrasts(df$SCUOLE) <- contr.treatment(levels(df$SCUOLE),base=which(levels(df$SCUOLE) == 'SCUOLADDIRIFERIMENTO'))

par(mfrow=c(1,1))
boxplot(df$Y~d$SCUOLE,main="Y PER SCUOLE",col="lightblue",ylab="Y", xlab = 'SCUOLE')

ggplot(df, aes(x=as.factor(county),
               y=math,
               color=county,
               fill=county)) + 
  theme_minimal()+
  geom_boxplot(col="black", alpha=0.2)+ 
  coord_flip()+
  labs(x="Math", y="County", title="Contee rispetto alle Scuole")
##########################################################
#     VARIANZA (EFFETTI FISSI) CIOE' OLS CON MEDIA
##########################################################
aggregate(df$Y, list(d$SCUOLE), mean)

MODELLOFISSO <- lm(Y ~ SCUOLE,df)
summary(MODELLOFISSO)
anova(MODELLOFISSO)

################################################################################################
#      SI PUO' ANALIZZARE COME SEMPLICISSIMA OLS PERCIO' CON TUTTI I PROBLEMI A RIGUARDO
################################################################################################

#######################
#     EMPTY MODEL
#######################
EMPTYMODEL <- lmer(Y ~ (1| SCUOLA),df,REML=T)
summary(EMPTYMODEL)

Anova(EMPTYMODEL, type="III")

# SERVE PER CONFRONTARE DEVIANCE
MODELLONULLO <- lm(Y ~ 1,df)
anova(EMPTYMODEL,MODELLONULLO)

EMPTYMODEL <- lmer(math ~(1| county),df,REML=F) 

df$EMPTYMODELRANDOM <- predict(EMPTYMODEL)

ggplot(data=df, aes(x=math, y=EMPTYMODELRANDOM, group = county, colour = county)) +
  geom_line() + 
  scale_colour_discrete('County') 

# ICC DEVE ESSERE SUPERIORE A 0.1 SENNO TORNO A OLS
performance::icc(EMPTYMODEL)

#############################
#     RANDOM INTERCEPT
#############################

#Random intercepts only
# (1 | Group)
RANDOMINT <- lmer(Y ~ X + (1| SCUOLA),df,REML=T)
summary(RANDOMINT)

Anova(RANDOMINT, type="III")

performance::icc(RANDOMINT)
anova(RANDOMINT,MODELLONULLO)
anova(RANDOMINT,EMPTYMODEL)

#################################################################
#     #Random slopes 
#################################################################
#Random slopes only
# (0 + Variable | Group)

RANDOMSLOPEINT <- lmer(Y ~ X1 + X2 (0 + X3| SCUOLA),df,REML=T)
summary(RANDOMSLOPEINT)

Anova(RANDOMSLOPEINT, type="III")

performance::icc(RANDOMSLOPEINT)
anova(RANDOMSLOPEINT,MODELLONULLO)
anova(RANDOMSLOPEINT,EMPTYMODEL)
anova(RANDOMSLOPEINT,RANDOMINT)

#################################################################
#     #Random intercepts and slopes (and their correlation)
#################################################################
#Random intercepts and slopes (and their correlation)
# (Variable1 | Group) + (Variable2 | Group) 
RANDOMSLOPEINT <- lmer(Y ~ X1 + X2 (X3| SCUOLA),df,REML=T)
summary(RANDOMSLOPEINT)

Anova(RANDOMSLOPEINT, type="III")

performance::icc(RANDOMSLOPEINT)
anova(RANDOMSLOPEINT,MODELLONULLO)
anova(RANDOMSLOPEINT,EMPTYMODEL)
anova(RANDOMSLOPEINT,RANDOMINT)


#Intercetta solo per fattore casuale: (1 | random.factor)
#Pendenze solo per fattore casuale: (0 + fixed.factor | random.factor)
#Intercettazioni e pendenze per fattore casuale: (1 + fixed.factor | random.factor)
######################################
#     #PER FARE GRAFICI FIGHI
######################################
mod2 <- lmer(math ~ calworks + (1| county),df,REML=F) 

df$random.intercpet.preds <- predict(mod2)

ggplot(data=df, aes(x=calworks, y=random.intercpet.preds, group = county, colour = county)) +
  geom_line() + 
  labs(x="Calworks", y="Matematica") +
  ggtitle("Varying Intercept Math Prediction") + 
  scale_colour_discrete('County') +
  geom_point(aes(x=calworks, y=math, group = county, colour = county)) 

mod3 <- lmer(math ~ calworks + (0 + calworks| county),df,REML=F) 
df$random.slope.preds <- predict(mod3)
ggplot(data=df, aes(x=calworks, y=random.slope.preds, group = county, colour = county)) +
  geom_line() + 
  labs(x="Calworks", y="Predicted Math") +
  ggtitle("Varying Slope Math Prediction") + 
  scale_colour_discrete('County') +
  geom_point(aes(x=calworks, y=math, group = county, colour = county)) 


mod4 <- lmer(math ~ calworks + (1 + calworks| county),df,REML=F) 
df$random.slope.int.preds <- predict(mod4)

ggplot(data=df, aes(x=calworks, y=random.slope.int.preds, group = county, colour = county)) +
  geom_line() + 
  geom_point(aes(x=calworks, y=math, group = county, colour = county)) +
  labs(x="Calworks", y="Predicted Math") +
  ggtitle("Varying Slope and Intercept Math Prediction") + 
  scale_colour_discrete('County')
summary(mod2)
Anova(mod2, type="III")

model<- lmer(variable ~ target_effect*condition+(1+target_effect|random_effect),data=d_test, REML=F)
#Random intercepts only
(1 | Group)

#Random slopes only
(0 + Variable | Group)

#Random intercepts and slopes (and their correlation)
(Variable | Group)

#Random intercepts and slopes (without their correlation)
(1 | Group) + (0 + Variable | Group)

#Same as above only if Variable is *continuous* and not a factor
(Variable || Group)

#Random intercept and slope for more than one variable (and their correlations)
(Variable_1 + Variable_2 | Group)