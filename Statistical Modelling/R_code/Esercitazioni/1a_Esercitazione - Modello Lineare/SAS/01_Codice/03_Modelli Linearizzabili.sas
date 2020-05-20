/************************************************************************/
/************************** MODELLI LINEARIZZABILI **********************/
/************************************************************************/

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

LIBNAME User 'C:\Users\daniele.riggi\Desktop\Corso Bicocca Vittadini\07_Esercitazioni Vitta\2a_Esercitazione - Modelli Generalizzati\SAS\02_Dataset'; run;


/*APRIRE LA VISUALIZZAZIONE DELL'OUTPUT IN HTML*/


*--------------------------------------------------------*
/******************* MODELLI LINEARIZZABILI *************/
*--------------------------------------------------------*;

  /* Matrice di dati 'companies'*/ 

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


/*Riassegnazione di etichette alle modalita' di macrosector:*/
proc format;  
      value macroname 
         1='sett. terziario' 
         2='sett. servizi' 
         3='sett. commercio' 
         4='sett. finanziario'
         5='altri settori'; 
run;

data companies;
set companies;
format macrosector macroname.;
run;


title 'Companies: Matrice dei grafici a dispersione';
ods graphics on;
proc corr data=companies plots=matrix(histogram nvar=all);
var mark_val assets sales profits cash employ;
run;
ods graphics off;

/*Diagnostiche su alcuni modelli lineari che descrivono "mark_val" in funzione
  di alcune variabili esplicative quantitative*/

title 'Companies: quattro modelli lineari semplici di interesse';
ods graphics on;
   proc reg data=companies simple corr plots(only label)=(diagnostics FitPlot(NOCLI));
   mod1_profits:  model  mark_val=profits / clb spec dwprob;
   mod2_sales:    model  mark_val=sales   / clb spec dwprob;
   mod3_employ:   model  mark_val=employ  / clb spec dwprob;
   mod4_assets:   model  mark_val=assets  / clb spec dwprob;
  run;
quit;
ods graphics off;

/*************************************************/
/* MODELLO 1: REGRESSIONE DI MARK_VAL SU PROFITS */
/*************************************************/

title 'Modello mark_val con profits';
proc sgplot data=companies noautolegend;
  reg x=profits y=mark_val / MARKERATTRS=(symbol=CircleFilled color=red) LINEATTRS=(THICKNESS=.5mm color=black);
  inset "Retta di regressione OLS:" "mark_val=543.58459 + 5.73352 profits" "R(*ESC*){sup '2'} = 0.4881"/ border;
run;


/*ALTRI MODELLI POSSIBILI PER INTERPRETARE MARK_VAL IN FUNZIONE DI PROFITS: MODELLI DI REGRESSIONE POLINOMIALI*/

/*Costruzione delle variabili profits^2, profits^3, profits^4, profits^5*/
data companies;
set companies;
profits2 = profits**2;
profits3 = profits**3;
profits4 = profits**4;
profits5 = profits**5;
run;

title 'mark_val vs. profits: Polinomi di regressione fino al 5° grado';
proc reg data=companies;
    Linear:     model mark_val=profits;  
    Quadratic:  model mark_val=profits profits2;  
	Cubic:      model mark_val=profits profits2 profits3;  
    Quartic:    model mark_val=profits profits2 profits3 profits4;  
    Quintic:    model mark_val=profits profits2 profits3 profits4 profits5;  
	run;
quit;

/*Dall'analisi dei test t di Student si preferisce il modello quadratico*/

/*Adattamento dei polinomi di regressione fino al 4° grado*/
title 'mark_val vs. profits: Polinomi di regressione fino al 4° grado';
proc sgplot data=companies;
  reg x=profits y=mark_val / LINEATTRS=(THICKNESS=.75mm color=black) name="lin" legendlabel="Linear Regression";
  reg x=profits y=mark_val / DEGREE=2 LINEATTRS=(THICKNESS=.75mm color=red)
        name="quadr" legendlabel="Quadratic Regression";
  reg x=profits y=mark_val / DEGREE=3 LINEATTRS=(THICKNESS=.75mm color=darkgreen)
        name="cub" legendlabel="Cubic Regression";
  reg x=profits y=mark_val / DEGREE=4 LINEATTRS=(THICKNESS=.75mm color=blue)
        name="quart" legendlabel="Quartic Regression";
  keylegend "lin" "quadr" "cub" "quart";
run;
title;


title 'mark_val vs. profits: Polinomio di 2° grado';
proc sgplot data=companies;
  reg x=profits y=mark_val / DEGREE=2 LINEATTRS=(THICKNESS=.75mm color=red)
        name="quadr" legendlabel="Quadratic Regression";
  inset "Parabola di regressione OLS:" "mark_val=537.04794 + 2.45125 profits + 0.01640 profits(*ESC*){sup '2'}" 
        "R(*ESC*){sup '2'} = 0.6224"/ border;
  keylegend "quadr";
run;
title;



/********** MODELLI LINEARIZZABILI ***************/
/*Consideriamo tre diversi modelli linearizzabili che coinvolgono la trasformazione del logaritmo naturale
  o sulla variabile indipendente, o sulla variabile dipendente, o su entrambe le variabili:
  1. Modello lineare-log: Y = beta0 + beta1*ln(X) + epsilon
  2. Modello log-lineare: ln(Y) = beta0 + beta1*X + epsilon
     In questo caso, una volta stimati i parametri del modello lineare sara' necessario considerare
     la trasformazione inversa della variabile Y da cui si ottiene il modello moltiplicativo: 

     Y = exp(beta0 + beta1*X + epsilon) = exp(beta0)* exp(beta1*X)* exp(epsilon)
  
  3. Modello log-log-lineare: ln(Y) = beta0 + beta1*ln(X) + epsilon,
     che ha quale modello moltiplicativo:

     Y = exp(beta0 + beta1*ln(X) + epsilon) = exp(beta0)* X^beta1 * exp(epsilon)
*/

/*Trasformazione del logaritmo naturale delle variabili mark_val e profits*/
data companies;
set companies;
lnprof = log(profits);
lnmark = log(mark_val);
label lnprof='ln(profitto)'
      lnmark='ln(valore di mercato)';
run;

/*Funzioni matematiche principali: ABS (valore assoluto), EXP (esponenziale), LOG (logaritmo naturale)
  LOG10 (logaritmo in base 10), SQRT (radice quadrata) */

/* NOTA: Nel file log compaiono i seguenti messaggi:
NOTE: Invalid argument to function LOG(-93.8) at line 302 column 10.
...
NOTE: Invalid argument to function LOG(-3.1) at line 302 column 10.

NOTE: Mathematical operations could not be performed at the following places. 
      The results of the operations have been set to missing values.

Il problema e' che 5 compagnie hanno profitti negativi. Quindi per queste la trasformazione logaritmica
non e' definita. SAS automaticamente considera i valori di queste unita' come 'missing values'.
*/

/*---/*---/*---/*---/*---/*---*/
/*--- MODELLO LINEARE-LOG ---*/
/*---/*---/*---/*---/*---/*---*/

title 'mark_val vs. profits: Modello lineare-log';
ods graphics on;
proc reg data=companies plots(only label)=(diagnostics FitPlot(NOCLI));
    Linear:    model mark_val=profits / clb spec dwprob;  
    LinearLog: model mark_val=lnprof   / clb spec dwprob;  
	/*The OUTPUT statement refers to the most recent MODEL statement. */
output out=companies predicted=yhat_linlog1;
run;
quit;
ods graphics off;

proc print data=companies(obs=5);
var profits lnprof mark_val lnmark yhat_linlog1;
id company;
run;


/*Osservazione1: nella prima tabella dell'output e' riportato:
  Number of Observations with Missing Values 5,
  ossia come gia' notato si sono generati 5 missing values a causa della 
  trasformazione logaritmica applicata alla variabile profits. 
*/

/*Osservazione2: poiche' ora mancano cinque compagnie, l'adattamento del modello lineare
  porta ovviamente a risultati diversi. In particolare, avendo tolto le compagnie
  dal profitto negativo, si osserva che l'adattemento del modello ai dati e' visibilmente
  migliorato. 
*/

title2 'Compagnie con profitti negativi';
  proc print data=companies(where=(profits <= 0)) label;
   var profits lnprof mark_val lnmark;
   id company;
   run;
title2;

/*Poiche' il dataset e' cambiato conviene costruire di nuovo il modello di regressione quadratico*/
title 'mark_val vs. profits: Modello lineare-log';
proc reg data=companies(where=(profits>0));
    Quadratic: model mark_val=profits profits2 / clb;  
run;
quit;


/*RAPPRESENTAZIONE GRAFICA CON PROC GPLOT*/
title 'mark_val vs. profits: Modelli lineare, quadratico, lineare-log';
title2 'Asse delle ascisse su scala di origine';
symbol1 v=circle c=black I=none;
symbol2 v=none c=black I=RL WIDTH=2; /*linear regression*/
symbol3 v=none c=red I=RQ WIDTH=2; /*quadratic regression*/
symbol4 v=none c=cyan I=splines WIDTH=2; /*linear-log model*/

legend1 label=none value=("companies" "Linear Regr." "Quadratic Regr." "Linear-log Regr.")
        position=(top left inside) frame across=2 offset=(,-1.5)
        mode=share;

proc gplot data=companies(where=(profits>0));
plot mark_val*profits=1 mark_val*profits=2 mark_val*profits=3 yhat_linlog1*profits=4 / 
                                 overlay legend=legend1;
run;
quit;
title2;


/*Grafico con l'asse delle ascisse su scala logaritmica*/
axis1 logbase=e
      logstyle=expand
      label=("ln(Profits)" );

title2 'Asse delle ascisse su scala logaritmica';
proc gplot data=companies(where=(profits>0));
plot mark_val*profits=1 mark_val*profits=2 mark_val*profits=3 yhat_linlog1*profits=4 / 
                                 overlay legend=legend1 haxis=axis1;
run;
quit;

goptions reset=(axis symbol legend);


/*---/*---/*---/*---/*---/*---*/
/*--- MODELLO LOG-LINEARE ---*/
/*---/*---/*---/*---/*---/*---*/

title 'mark_val vs. profits: Modello log-lineare';
ods graphics on;
proc reg data=companies plots(only label)=(diagnostics FitPlot(NOCLI));
    LogLinear: model lnmark=profits   / clb spec dwprob;  
output out=companies predicted=lnyhat_loglin1;
run;
quit;
ods graphics off;


/*Determinazione della trasformazione inversa:

ln(y) = beta0 + beta1*x => y = exp(beta0)*exp(x*beta1)
*/

data companies;
set companies;
yhat_loglin1 = exp(lnyhat_loglin1);
run;


proc print data=companies(obs=5);
var mark_val lnmark yhat_linlog1 yhat_loglin1;
id company;
run;


/*RAPPRESENTAZIONE GRAFICA CON PROC GPLOT*/
title 'mark_val vs. profits: Modelli lineare, quadratico, lineare-log, log-lineare';
symbol1 v=circle c=black I=none;
symbol2 v=none c=black I=RL WIDTH=2; /*linear regression*/
symbol3 v=none c=red I=RQ WIDTH=2; /*quadratic regression*/
symbol4 v=none c=cyan I=splines WIDTH=2; /*linear-log model*/
symbol5 v=none c=green I=splines WIDTH=2; /*log-linear model*/

legend1 label=none value=("companies" "Linear Regr." "Quadratic Regr." "Linear-Log Regr." "Log-linear Regr.")
        position=(top left inside) frame across=2 offset=(,-1.5)
        mode=share;
proc gplot data=companies(where=(profits>0));
plot mark_val*profits=1 mark_val*profits=2 mark_val*profits=3 yhat_linlog1*profits=4 yhat_loglin1*profits=5/ 
                          overlay legend=legend1;
run;
quit;

goptions reset=(symbol legend);


/*---/*---/*---/*---/*---/*---*/
/*--- MODELLO LOG-LOG-LINEARE ---*/
/*---/*---/*---/*---/*---/*---*/

title 'mark_val vs. profits: Modello log-log-lineare';
ods graphics on;
proc reg data=companies plots(only label)=(diagnostics FitPlot(NOCLI));
    LogLogLin: model lnmark=lnprof   / clb spec dwprob;  
output out=companies predicted=lnyhat_loglog1;
run;
quit;
ods graphics off;


/*Determinazione della trasformazione inversa:

ln(y) = beta0 + beta1*ln(x) => y = exp(beta0)*x^beta1
*/

data companies;
set companies;
yhat_loglog1 = exp(lnyhat_loglog1);
run;


proc print data=companies(obs=5);
var mark_val lnmark yhat_linlog1 yhat_loglin1 yhat_loglog1;
id company;
run;


/*RAPPRESENTAZIONE GRAFICA CON PROC GPLOT*/
symbol1 v=circle c=black I=none;
symbol2 v=none c=black I=RL WIDTH=2; /*linear regression*/
symbol3 v=none c=red I=RQ WIDTH=2; /*quadratic regression*/
symbol4 v=none c=cyan I=splines WIDTH=2; /*linear-log model*/
symbol5 v=none c=green I=splines WIDTH=2; /*log-linear model*/
symbol6 v=none c=blue I=splines WIDTH=2; /*log-log-linear model*/


legend1 label=none value=("companies" "Log-log-linear Regr.")
        position=(top left inside) frame across=2 offset=(,-1.5)
        mode=share;

title 'mark_val vs. profits: Modello log-log-lineare';
proc gplot data=companies(where=(profits>0));
plot mark_val*profits=1 yhat_loglog1*profits=6 / overlay legend=legend1;
run;


legend2 label=none value=("companies" "Linear Regr." "Quadratic Regr." "Linear-Log Regr." "Log-linear Regr."
                           "Log-log-linear Regr.")
        position=(top left inside) frame across=2 offset=(,-1.5)
        mode=share;

title 'mark_val vs. profits: Modelli lineare, quadratico e con trasformazioni logaritmiche';
proc gplot data=companies(where=(profits>0));
plot mark_val*profits=1 mark_val*profits=2 mark_val*profits=3 yhat_linlog1*profits=4 yhat_loglin1*profits=5 
                          yhat_loglog1*profits=6 / overlay legend=legend2;
run;


legend3 label=none value=("companies" "Linear Regr." "Quadratic Regr." "Log-linear Regr.")
        position=(top left inside) frame across=2 offset=(,-1.5)
        mode=share;

title 'mark_val vs. profits: Modelli lineare, quadratico e log-lineare';
title2 "Compagnie con profitti negativi incluse nell'analisi";
proc gplot data=companies;
plot mark_val*profits=1 mark_val*profits=2 mark_val*profits=3 yhat_loglin1*profits=5 
                           / overlay legend=legend3;
run;
quit;

goptions reset=(symbol legend);

/*Dalle analisi svolte sopra il modello piu' convincente e' il modello di regressione quadratico,
  poiche' garantisce il migliore adattamento ai dati. Inoltre, sembra interpretare meglio la situazione
  delle compagnie dal profitto negativo.*/

/***************************************************************************************
  ESERCIZIO: RIPETERE LE ANALISI CONSIDERANDO GLI ALTRI POSSIBILI MODELLI CHE DESCRIVONO
  MARK_VAL CON SALES, EMPLOY E ASSETS.
****************************************************************************************/
