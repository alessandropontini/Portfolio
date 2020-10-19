/********************************************/
/******************** GLS *******************/
/********************************************/

/***********  SAS 9.4 **********/

/*
++ Testo di riferimento per il software SAS: Manuale SAS/STAT:
http://support.sas.com/documentation/onlinedoc/stat/index.html#stat121
http://support.sas.com/documentation/cdl/en/statug/66103/HTML/default/viewer.htm#titlepage.htm
http://support.sas.com/documentation/cdl/en/statug/66103/PDF/default/statug.pdf

++ Altri manuali sono disponibili alla pagina:
http://support.sas.com/documentation/94/index.html
*/


/* DIRECTORY DI LAVORO: */
/*Modificare inserendo il percorso opportuno:*/

LIBNAME User 'C:\Users\daniele.riggi\Desktop\Corso Bicocca Vittadini\07_Esercitazioni Vitta\2a_Esercitazione\SAS - Modelli Generalizzati\02_Dataset'; run;


/*APRIRE LA VISUALIZZAZIONE DELL'OUTPUT IN HTML*/


 /*Matrice di dati 'companies'*/ 

/*------------------------------------ COMPANIES ---------------------------------------* 
|Matrice di dati che contiene alcune informazioni riguardanti 64 compagnie 		        |
|che compaiono nella top 500 redatta da Forbes per l’anno 1986.				            |
|Le variabili presenti nel dataset sono le seguenti:					                |
|assets  = attivo in bilancio (milioni di dollari);					                    |
|sales = fatturato relativo alle vendite (milioni di dollari);			                |
|mark_val = valore di mercato della compagnia (milioni di dollari);			            |
|profits = profitto (milioni di dollari);						                        |
|cash = flusso di cassa (milioni di dollari);						                    |
|employ = numero complessivo di dipendenti (migliaia);					                |  
|sector = settore di mercato in cui opera la compagnia: Communication; Energy; Finance; |
|		 HiTech; Manufacturing; Medical; Retail; Transportation; Other.		            |
*---------------------------------------------------------------------------------------*/ 


/*Alla ripresa della sessione di lavoro e' necessario assegnare di nuovo 
  le etichette alle modalita' delle variabili, in questo caso macrosector*/

/*1. creazione della corrispondenza 'codice numerico - etichetta'*/
proc format;  
      value macroname 
         1='sett. terziario' 
         2='sett. servizi' 
         3='sett. commercio' 
         4='sett. finanza'
         5='altri settori'; 
run;

/*2. attribuzione della corrispondenza 'codice numerico - etichetta' alla variabile macrosector*/
data companies;
set companies;
format macrosector macroname.;
run;

title 'Dataset: companies';
proc print data=companies(obs=5) label;
id company;
run;


*----------------------------------------------------------------*
/******************* MODELLO LINEARE CLASSICO ********************/
*----------------------------------------------------------------*;

/****************************************/
/*COSTRUZIONE DEL MODELLO DI REGRESSIONE*/
/* Variabile dipendente: mark_val*/

/* Sintassi essenziale di PROC REG:

PROC REG < options > ; 
< label: > MODEL dependents=<regressors> < / options > ; 
WEIGHT variable ; 
PLOT <yvariable*xvariable> <=symbol> 
      < ...yvariable*xvariable> <=symbol> < / options > ; 
*/

title 'Modello di regressione lineare multipla: modello completo';
ods graphics on;
   proc reg data=companies simple corr;
      model  mark_val=assets sales profits cash employ / stb clb corrb spec dwprob;
	  /*Ulteriori opzioni di 'model': 
        'COVB' e 'CORRB' = per visualizzare in output le stime delle matrici, rispettivamente,
    					   di varianze e covarianze e di correlazione per lo stimatore dei
                           minimi quadrati	  
	  */
/*STATEMENT 'OUTPUT': per salvare in un file esterno la matrice di dati di origine
	  ('out='), i valori previsti ('predicted='), i residui ('residual=')*/
	  output out=companies
	     predicted=yhat_compl 
         residual=yresid_compl;
	run;
	quit;
ods graphics off;


title2 "Analisi di normalità dei residui";
proc univariate data=companies normaltest;
var yresid_compl;
histogram  / cfill=lightblue normal(color=red w=3) kernel(color=green w=3);
inset mean std="Std Dev" median min max kurtosis skewness NORMALTEST="Shapiro-Wilk test" PNORMAL="p-value" / cfill=lightgray;
qqplot / normal(mu=est sigma=est color=red w=2);
id company;
run;


/*Per avere anche i test di curtosi e di skewness si deve impiegare la PROC MODEL
  del prodotto di SAS/ETS - Econometric and Time Series*/

title2 'test di asimmetria e curtosi';
proc model data=companies;
   mark_val=beta0 + beta1*assets + beta2*sales + beta3*profits + beta4*cash + beta5*employ;
   fit mark_val / normal;
run;
quit;
title2;


/*http://support.sas.com/documentation/cdl/en/etsug/63939/HTML/default/viewer.htm#etsug_model_sect043.htm*/;

*---------------------------------------------------------------------------*
/******************* ETEROSCHEDASTICITA' - I PARTE *************************/
*---------------------------------------------------------------------------*;

/*ESEMPIO 1 - GLS: Errori eteroschedastici*/

/*********************/
/*METODO DI STIMA GLS*/
/*********************/

/*Implementazione del metodo FGLS - Feasible Generalized Least Squares -
Si tratta di considerare l'eteroschedasticita' nel processo di stima.
Passi principali:
1. si stima il modello di regressione utilizzando il metodo OLS (Ordinary Least
   Squares). In questo modo si ottengono i residui OLS;
2. si assume un modello che descriva la varianza degli errori in funzione dei regressori
   (ad esempio, una forma lineare, quadratica, logaritmica, ecc.). Da un punto di vista 
   operativo, cio' equivale ad assumere una relazione fra i residui al quadrato e i 
   regressori. Quindi, si stimano i parametri del modello cosi' specificato;
3. si utilizza il modello stimato al punto 2. per ottenere i valori previsti dei residui al
   quadrato, che rappresentano i valori previsti della varianza;
4. in luogo delle variabili di origine Y, X1, X2, ..., Xk, si considerano le nuove
   variabili: Y~, X1~, X2~, ..., Xk~, ottenute rapportando le variabili di origine
   ai valori previsti della deviazione standard (ossia, la radice quadrata della varianza);
5. infine, da un punto di vista operativo si hanno due possibilita':
   5a. con il metodo OLS si stima il modello di regressione (senza intercetta)
       costruito sulle variabili che derivano dal punto 4;
   5b. si applica il metodo dei minimi quadrati pesati (Weighted Least Squares) al modello
       di origine usando come peso il reciproco dei valori stimati della varianza.
*/

/*INDIVIDUAZIONE DELLA FORMA FUNZIONALE CHE LEGA LA VARIANZA AI REGRESSORI*/
/*VARIE POSSIBILITA':*/

/*1. FORMA LINEARE*/

/*Nel caso in esame, possiamo assumere la seguente forma parametrica generale:
  Var(Epsilon_i) = gamma_0 + gamma_1*X1 + .... + gamma_k*Xk = h(x1,..., xk) 
  Creiamo prima la colonna che contiene i residui OLS al quadrato. */

title 'Errori eteroschedastici';
title2 'costruzione delle stime FGLS: forma lineare';
data companies;
  set companies;
  res_2 = yresid_compl**2;
run;

/*Grafico dei residui al quadrato e dei valori previsti di Y*/
symbol v=dot c=red POINTLABEL=("#company");
proc gplot data=companies;
  plot res_2*yhat_compl;
  title3 'OLS squared residuals against y-predicted values';
run;
quit;


/*Costruzione del modello lineare secondo il precedente punto 2.
e salvataggio dei valori previsti per la varianza (punto 3.) :*/
proc reg data=companies;
  mod_lin_res: model res_2 = assets sales profits cash employ;
  title3 'Previsione dei valori della varianza';
  output out=res_var predicted=var_hat residual = var_res;
run;
quit;


/*Costruzione delle nuove variabili (punto 4.)*/
data res_var;
  set res_var;
  /*Si usano ora i valori previsti dei residui al quadrato:*/
  h_fun = var_hat;
  /*Costruzione della variabile peso*/
  /*Se si lavorera' inserendo nel processo di stima una variabile 'peso', 
  	tale peso sara' dato dal reciproco della varianza. Pertanto,
    la trasformazione e' del tipo:*/
  weight_h = 1/h_fun;
  /*Viceversa, se si lavorera' sul modello con variabili trasformate,
   tale trasformazione sara' data dal reciproco della deviazione standard
   dei valori previsti dei residui al quadrato (ossia, dei valori previsti
   della varianza)*/
  int_tild = 1/sqrt(h_fun);
  mark_val_tild = mark_val / sqrt(h_fun);
  assets_tild = assets / sqrt(h_fun);
  sales_tild = sales / sqrt(h_fun);
  profits_tild = profits / sqrt(h_fun);
  cash_tild = cash / sqrt(h_fun);
  employ_tild = employ /sqrt(h_fun);
run;

/*MODELLO STIMATO CON I FGLS*/
/*1a procedura: Stima del modello di regressione impiegando le nuove variabili (punto 5a.) */
proc reg data=res_var;
  mod1_fgls: model mark_val_tild = int_tild assets_tild sales_tild profits_tild cash_tild employ_tild /noint stb clb corrb spec dwprob;
  /*opzione 'noint': specifica che nel modello l'intercetta non e' presente*/
  title3 'FGLS ottenuti previa trasformazione delle variabili' ;
  output out=fgls_lin_resid predicted=yhat_lin residual=yresid_lin;
run;
quit;

proc gplot data=fgls_lin_resid;
  plot yresid_lin*yhat_lin /vref=0;
  title3 'FGLS Residuals against Fitted values';
run;

/*2a procedura: Utilizzo della variabile peso: 'weight_h', data dal reciproco
  dei valori previsti della varianza (punto 5b.)*/
proc reg data=res_var;
  mod2_fgls: model mark_val=assets sales profits cash employ / stb clb corrb spec dwprob;
  weight weight_h;
  title3 'FGLS ottenuti con la variabile peso';
run;
quit;
title2;
title3;

/*E' immediato notare che le due procedure per la stima GLS vengono a coincidere */
/*N.B.: Nella procedura di stima FGLS di cui sopra 5 unita' sono ora 'missing'.
  Questo perche' tali unita' hanno associata una stima negativa della varianza.
  E' necessario allora ricorrere ad un'altra forma funzionale.
  */

/* FILE LOG:
NOTE: 5 observations have invalid weights or frequencies.
*/

/*-------------- ALTRA FORMA FUNZIONALE ---------------*/
/*2. FORMA ESPONENZIALE*/

/*Nel caso in esame, possiamo assumere la seguente forma parametrica generale:
  h(x1, ..., xk) = exp(gamma_0 + gamma_1*x1 + .... + gamma_k*xk). 
  E' piu' semplice pero' ricorrere al modello linearizzato.
  Creiamo prima la colonna che contiene i logaritmi (naturali) dei residui OLS al quadrato. */

title2 'Costruzione delle stime FGLS: forma esponenziale';
data companies;
  set companies;
  log_res_2 = log(yresid_compl**2);
run;


/*Grafico del logaritmo dei residui al quadrato e dei valori previsti di Y*/
proc gplot data=companies;
  plot log_res_2*yhat_compl;
  title3 'OLS log-squared residuals against y-predicted values';
run;


/*Costruzione del modello lineare secondo il precedente punto 2.
e salvataggio dei valori previsti per la varianza (punto 3.) :*/

proc reg data=companies;
  mod_log_res: model log_res_2 = assets sales profits cash employ;
  title3 'Previsione dei logaritmi dei valori della varianza';
  output out=res_var2 predicted=log_var_hat residual = log_var_res;
run;
quit;


/*Costruzione delle nuove variabili (punto 4.)*/
data res_var2;
  set res_var2;
  h_fun = exp(log_var_hat);
  /*Costruzione della variabile peso*/
  weight_h = 1/h_fun;
  /*Trasformazione delle variabili di origine*/
  mark_val_tild = mark_val / sqrt(h_fun);
  int_tild = 1/sqrt(h_fun);
  assets_tild = assets / sqrt(h_fun);	
  sales_tild = sales / sqrt(h_fun);
  profits_tild = profits / sqrt(h_fun);
  cash_tild = cash / sqrt(h_fun);
  employ_tild = employ /sqrt(h_fun);
run;


/*1a procedura: Stima del modello di regressione impiegando le nuove variabili (punto 5a.) */
proc reg data=res_var2;
  mod1_fgls: model mark_val_tild = int_tild assets_tild sales_tild profits_tild cash_tild employ_tild /noint stb clb corrb spec dwprob;
  /*opzione 'noint': specifica che nel modello l'intercetta e' nulla*/
  title3  'FGLS ottenuti previa trasformazione delle variabili' ;
  output out=fgls_exp_resid predicted=yhat_exp residual=yresid_exp;
run;
quit;

proc gplot data=fgls_exp_resid;
  plot yresid_exp*yhat_exp /vref=0;
  title3 'FGLS Residuals against Fitted values';
run;
quit;

/*2a procedura: Utilizzo della variabile peso: 'weight_h', data dal reciproco
  dei valori previsti della varianza (punto 5b.)*/

proc reg data=res_var2;
  mod2_fgls: model  mark_val=assets sales profits cash employ / stb clb corrb spec dwprob;
  weight weight_h;
  title3 'FGLS ottenuti con la variabile peso';
run;
quit;


*---------------------------------------------------------------------------*
/******************* ETEROSCHEDASTICITA' - II PARTE ************************/
*---------------------------------------------------------------------------*;

********************************************************************************
 /*In alternativa alla costruzione FGLS si puo' impiegare direttamente la procedura di SAS: PROC AUTOREG,     
   che permette di specificare le variabili esplicative collegate all'eteroschedasticita' degli errori
   e la forma funzionale della loro relazione. 
 */

/* MANUALE SAS/ETS: (ETS - Econometrics and Time Series analysis),
The AUTOREG procedure estimates and forecasts linear regression models 
for time series data when the errors are autocorrelated or heteroscedastic.
*/

********************************************************************************;

/* SINTASSI ESSENZIALE
PROC AUTOREG options ; 
MODEL dependent = regressors / options ; 
HETERO variables / options ; 
	(The HETERO statement specifies variables that are related 
	to the heteroscedasticity of the residuals and the way these 
	variables are used to model the error variance of the regression.)
OUTPUT OUT = SAS data set options ; 
*/

/*Se non si specificano altre opzioni, PROC AUTOREG fornisce le tradizionali stime OLS*/

title2 'OLS Regression';
proc autoreg data=companies;
  model mark_val=assets sales profits cash employ;
  output out=OLSresid_comp RESIDUAL=OLSresid_markval;
run;

/*Trasformazioni sui residui OLS: residui al quadrato, logaritmo dei residui al quadrato, residui in valore assoluto */
data OLSresid_comp;
set OLSresid_comp;
sqOLSres_markval = OLSresid_markval**2;  /*forma funzionale lineare*/
LNsqOLSres_markval = log(OLSresid_markval**2); /*forma funzionale esponenziale*/
absOLSres_markval = abs(OLSresid_markval);    /*forma funzionale quadratica*/
run;

proc print data=OLSresid_comp(obs=5);
var OLSresid_markval sqOLSres_markval LNsqOLSres_markval absOLSres_markval;
id company;
run;


/*Matrice di correlazione fra i residui OLS trasformati e le variabili esplicative*/
ods graphics on;
proc corr data=OLSresid_comp plots=matrix(NVAR=ALL);
var assets sales profits cash employ;
with absOLSres_markval sqOLSres_markval LNsqOLSres_markval;
run;
ods graphics off;


/*In assenza di un'ipotesi di partenza riguardante la forma funzionale che regola l'eteroschedasticita',
  si puo' procedere in termini euristici. In questo contesto si costruiscono le possibili forme funzionali
  h(x_1,..., x_k) implementate nella PROC AUTOREG, ossia lineare, esponenziale, quadratica. 
  Quindi sulla base dei risultati ottenuti si sceglie la forma funzionale che meglio interpreta l'eteroschedasticita'. */

title2 'ML estimates with linear heteroscedasticity model';
ods graphics on;
proc autoreg data=companies plots(only)=(FITPLOT QQ RESIDHISTOGRAM);
 /* Opzioni di 'plots':
    FITPLOT: plots the predicted and actual values. 
    QQ: Q-Q plot of residuals. 
    RESIDUALHISTOGRAM | RESIDHISTOGRAM: plots the histogram of residuals. 
  */
  model mark_val=assets sales profits cash employ / method=ml normal maxiter=1000;
  /* Opzioni di 'model':
     METHOD=ML: specifies maximum likelihood estimates
     NORMAL: specifies the Jarque-Bera’s normality test statistic for regression residuals.
     MAXITER=number: sets the maximum number of iterations allowed. The default is MAXITER=50. 
  */
  hetero assets sales profits cash employ / link=linear test=LM;
run;

/* Opzioni di 'hetero':
LINK=value 
specifies the functional form of the heteroscedasticity model. By default, LINK=EXP. 
Values of the LINK= option are as follows: 
- EXP: specifies the exponential link function
- SQUARE: specifies the square link function 
- LINEAR: specifies the linear function; that is, the HETERO statement variables predict the error variance linearly. 

TEST=LM 
produces a Lagrange multiplier test for heteroscedasticity 
(Engle's test -  The test statistic is asymptotically equivalent to the test used by Breusch and Pagan). 
The null hypothesis is homoscedasticity; the alternative hypothesis is heteroscedasticity of the form specified 
by the HETERO statement. The power of the test depends on the variables specified in the HETERO statement. 
The test may give different results depending on the functional form specified by the LINK= option. 
However, in many cases the test does not depend on the LINK= option.
*/

title2 'ML estimates with exponential heteroscedasticity model';
proc autoreg data=companies plots(only)=(FITPLOT QQ RESIDHISTOGRAM);
  model mark_val=assets sales profits cash employ / method=ml normal maxiter=1000;
  hetero assets sales profits cash employ / link=exp test=LM;
 run;


 title2 'ML estimates with square heteroscedasticity model';
proc autoreg data=companies plots(only)=(FITPLOT QQ RESIDHISTOGRAM);
  model mark_val=assets sales profits cash employ / method=ml normal maxiter=1000;
  hetero assets sales profits cash employ / link=square test=LM;
 run;
title2;
ods graphics off;


 /*Sulla base dei risultati ottenuti dalle tre forme funzionali si decide di mantenere la forma
   funzionale esponenziale. I risultati ottenuti sono simili alla forma quadratica, tuttavia
   dal test di normalita' risulta che nel caso della forma esponenziale i residui hanno distribuzione
   che sembra approssimarsi meglio alla distribuzione normale.
   Inoltre, come si nota nel caso della forma lineare (la stima dei parameteri HET2-HET5 non viene calcolata), 
   anche nel caso della forma quadratica si rilevano alcuni problemi numerici, poiche'
   non viene fornita la stima associata al parametro HET4 (che corrisponde al parametro gamma_4 
   associato a cash). 
  */


