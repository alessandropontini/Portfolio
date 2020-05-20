/************************************************************************/
/************************ DIAGNOSTICHE SUL MODELLO **********************/
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

LIBNAME User 'C:\Users\daniele.riggi\Desktop\Corso Bicocca Vittadini\07_Esercitazioni Vitta\1a_Esercitazione - Modello Lineare\SAS'; run;

/*APRIRE LA VISUALIZZAZIONE DELL'OUTPUT IN HTML*/


/*****************************************/
/********* DIAGNOSTICHE SUL MODELLO ******/
/*****************************************/

/*ESEMPIO: PM10 

Il data set contiene informazioni relative all’inquinamento dell’aria rilevato in corrispondenza delle strade e
degli snodi stradali principali, e contiene le seguenti variabili:
	1. PM10: concentrazione di particelle di pm10
	2. CARS: numero di auto che transitano in un’ora
	3. TEMP: temperatura misurata a 2 metri dal suolo
	4. WIND: velocità del vento (metri/secondo)
	5. D_TEMP: differenza tra temperatura misurata a 25 metri dal suolo e a 2 metri dal suolo
	6. HOURS: numero di ore trascorse dalla mezzanotte del giorno di rilevazione


 /*MATRICE DI CORRELAZIONE insieme ad alcune statistiche descrittive*/
 /*COSTRUZIONE DELLA MATRICE DEI GRAFICI A DISPERSIONE*/

 title ' PM_10';
 ods graphics on; 
  proc corr data=PM_10 noprob plots=matrix(histogram nvar=all ) ;
  run;
 ods graphics off; 

/*1) DIAGNOSTICHE DI COLLINEARITA'*/
title "REG: diagnostiche di collinearità";
proc reg data=PM_10; 
 mod1: model  pm10 = cars temp  wind  / TOL VIF COLLIN ; 
 mod2: model  pm10 = cars temp  wind D_TEMP HOURS / TOL VIF COLLIN ; 
 
  /*Opzioni:
	'tol' = indice di tolleranza (normalizzato in [0, 1]);
	'vif' = 'Variance-Inflation Factors', ossia fattori di inflazione della varianza
	  		o anche varianza multifattoriale (reciproco dell'indice di tolleranza. 
	        Valore soglia = 20: se 'vif' è maggiore o uguale a 20, vi è un eccessivo
	  	    grado di collinearità);
	'collin' = produce un'analisi di collinearità, con il computo dell'indice di condizione
	           (valore soglia = 30) e della proporzione di variazione.
	  */
  run;
  QUIT;


/*2) DIAGNOSTICHE DI INFLUENZA*/

title "REG: diagnostiche di influenza";
ods graphics on; 
proc reg 
/* OPZIONI PER LA VISUALIZZAZIONE DEI RISULTATI:*/
data=PM_10; 
	  mod1: model  pm10 = cars temp  wind  / INFLUENCE; 
	  mod2: model  pm10 = cars temp  wind D_TEMP HOURS / INFLUENCE;
	  /*Opzione 'influence': per il calcolo delle varie statistiche di influenza,
	  	come i resid_residui studentizzati e i valori 'leverage'. Permettono di 
	    stabilire l'influenza di ciascuna unità sulle stime dei parametri.*/
	  run;
ods graphics off; 

/* PER LO STUDIO DEI LEVERAGES:
Valore soglia: 2p/n, dove p= n.ro di parametri nel modello (intercetta inclusa) e n = n.ro di unità.
Nel caso del modello 1: p=5, n=500, da cui: 2p/n = 0.02. 

Le osservazioni a cui corrisponde un leverage che supera la soglia dovrebbero essere analizzate ulteriormente.

N.B.: per l'indicazione dei valori soglia sulle altre statistiche di influenza
consultare l'help on-line di SAS.
*/

title "REG: grafici per le diagnostiche di influenza";
proc reg data=PM_10; 
	 /*mod1: Modello a tre regressori*/
	 mod1: model  pm10 = cars temp  wind   / INFLUENCE noprint; 
	  /* Statement 'output': per salvare come file di dati esterni le principali
	     statistiche diagnostiche: 
	  		h=per i leverages; 
	  		covratio=per l'influenza sul determinante della matrice di var-cov; 
	  		student=per i resid_residui studentizzati;
	     	rstudent=per i resid_residui studentizzati 'jackknife'; 
	     	dffits=per la misura dell'influenza della singola osservazione sulla previsione dei valori della variabile dipendente.
	  */
	  output out=PM_10Diagn 
				h=hatdiag
				covratio=cov_rat
				rstudent=rst_res
				dffits=pred_infl;
	  run;
	  quit;

/*GRAFICI PER id_roadENTIFICARE LE UNITA'*/
  goptions reset=(symbol);
  symbol1 i=needle;
  /*INTERPOL=NEEDLE: draws a vertical line from each data point to a horizontal line at the 0 value 
    on the vertical axis or the minimum value on the vertical axis. The horizontal line is drawn automatically.
  */
  axis1 label=none
      order = 1 to 500 by 10
      offset=(2);

  axis2 label=(angle=90);
 
 /*OFFSET=(<n1><,n2>)<units > | (<n1<units>><,n2<units>>) 
   specifies the distance from the first and last major tick marks or bars to the ends of the axis line.
  */

   title 'Leverage plot';
   proc gplot data=PM_10Diagn;
   plot hatdiag*id_road=1 / vref=0.02 cvref=red lvref=3 
                       haxis=axis1 hminor=0;
   /*HMINOR=number-of-minor-ticks: specifies the number of minor tick marks drawn between 
					                each major tick mark on the horizontal axis. */
   run;
   quit;
   

   title 'Studentized resid_roadual - jackknife';
   proc gplot data=PM_10Diagn;
   plot rst_res*id_road=1 / vref=-2 2 cvref=(red red) lvref=3
                       haxis=axis1 hminor=0
                       vaxis=axis2 vminor=0;
   run;
   quit;

   title 'Cov Ratio plot';
   proc gplot data=PM_10Diagn;
   plot cov_rat*id_road=1 / vref=0.97 1.03 cvref=(green green) lvref=3
                       haxis=axis1 hminor=0
                       vaxis=axis2 vminor=0;
   run;
   quit;

   /*Valori soglia per il Cov-ratio plot: 
     1) covratio <= 1 - 3p/n = 1 - 3(5)/500 = 0.97
     2) covratio >= 1 + 3p/n = 1 + 3(5)/500 = 1.03
   */


   title 'DFFITS plot';
   proc gplot data=PM_10Diagn;
   plot pred_infl*id_road=1 / vref= -0.2 0.2 cvref=(red red) lvref=3
                         haxis=axis1 hminor=0
                         vaxis=axis2 vminor=0;
   run;
   quit;
/*Valore soglia (in valore assoluto): 2*sqrt(p/n) = 2*sqrt(5/500) = 0.2 
*/

   goptions reset=(axis);

/*ALTRI GRAFICI COSTRUITI CON LA PROCEDURA ODS GRAPHICS*/
ods graphics on;
proc reg data=PM_10Diagn plots(only label)=(RStudentByLeverage DFFITS );
id id_road;
model pm10 = cars temp  wind   ;
run;
ods graphics off;

ods graphics on;
proc reg data=PM_10Diagn plots(only)=(DFBETAS(label));
id id_road;
model pm10 = cars temp  wind   ;
run;
ods graphics off;

/* Valore soglia per DFBETAS:
Observations whose DFBETAS’ statistics for a regressor are greater in magnitude than 
2/sqrt(n), where n is the number of observations used, are deemed to be influential for that regressor.

In questo caso: 2/sqrt(500) = 44.72
*/


/*GRAFICO COSTRUITO CON LO STATEMENT PLOT*/
goptions reset=(symbol);
symbol v=dot h=1 c=green; 
/*
HEIGHT=symbol-height<units> 
Alias H= 
specifies the height in number of units of plot symbols. 
*/

proc reg data=PM_10; 
	 mod1: model  pm10 = cars temp  wind  ; 
	  /*GRAFICO PER id_roadENTIFICARE LE UNITA' sulla statistica COVRATIO*/
      plot COVRATIO.*obs. / href= 1 to 500 by 10 chref=red lhref=3 cframe=ligr; 
	  	  run;
		  quit;


/* DISTANZA DI COOK */
title "REG: distanza di Cook";
ods graphics on;
proc reg data=PM_10Diagn plots(only)=(COOKSD(label));
id id_road;
model pm10 = cars temp  wind  ;;
run;
ods graphics off;

/* Valore soglia per COOKSD:
COOKSD <(LABEL)> plots Cook’s D statistic by observation number. Observations whose Cook’s D 
statistic lies above the horizontal reference line at value 4/n, where n is the number of observations used, 
are deemed to be influential. 

In questo caso: 4/n = 4/500 = 0.008.
*/

proc reg data=PM_10; 
	 mod1: model  pm10 = cars temp  wind   / R; 
	  /* Opzione 'R': per richiedere un'analisi dei resid_residui, 
	  	  che comprende il calcolo della distanza di Cook.
	  */
/*GRAFICO PER id_roadENTIFICARE LE UNITA' sulla distanza di Cook*/
      plot cookd.*obs. / href= 1 to 500 by 10 chref=red lhref=3 cframe=ligr; 
	  	  run;
		  quit;

/* N.B.: Se dai risultati precedenti si dovesse ritenere che una o più unità statistiche
		 influiscano eccessivamente sull'analisi di regressione, sarebbe opportuno eliminare
		 tali unità dall'insieme complessivo e ripetere le analisi sui dati restanti.
		 Ad esempio, se si vuole eliminare l'unità n.ro 277 poiché è caratterizzata sia da un leverage
		 che da una distanza di Cook troppo elevati, è possibile creare un nuovo dataset che non la
		 contenga e ripetere su quest'ultimo la costruzione del modello di regressione.
		  */

title "Eliminazione unità n.ro 277";
data PM_10_rid_road;
set PM_10;
if _N_=277 then delete;
run;

title "Modello senza l'unità n.ro 277";
ods graphics on;
proc reg data=PM_10_rid_road plots(only)=(COOKSD(label));
id id_road;
model pm10 = cars temp  wind   ;
run;
quit;
ods graphics off;

/******************************************************************************
ESERCIZIO: ripetere l'analisi sul dataset PM_10 rid_roadotto eliminando l'unità 208 e 14.
********************************************************************************/


/*3) DIAGNOSTICHE PER LA NORMALITA' DEGLI ERRORI*/

/* TEST DI NORMALITA' SUI RESid_residui */
   /*Salviamo in un file esterno i resid_residui di regressione*/
title 'Test di normalità';
ods graphics on;
proc REG data=PM_10 plots(only label)=(RESIDUALBOXPLOT RESIDUALHISTOGRAM);
     mod1: model pm10 = cars temp  wind   ; 
OUTPUT OUT=RESPM_10 R=resid_residui; 
/*Opzione: 
'r=nome': per inserire nel file 'RESPM_10' la colonna dei resid_residui di regressione. 
*/
run;
quit;
ods graphics off;


proc print data=RESPM_10(obs=5) label; /*con (obs=5) vengono visualizzate in output le prime 5 unità*/
run;


/*PROC UNIVARIATE: Sintassi essenziale

PROC UNIVARIATE  <options> ; 
HISTOGRAM  <variables> < / options> ; 
ID  variables ; 
INSET  keyword-list </ options> ; 
OUTPUT  <OUT=SAS-data-set> <keyword1=names …keywordk=names> <percentile-options> ; 
PPPLOT  <variables> < / options> ; 
QQPLOT  <variables> < / options> ; 
VAR  variables ; 
*/

/*ISTOGRAMMA e test di normalità; calcolo dell'indice di asimmetria 
   e di curtosi*/
proc univariate data=RESPM_10 normaltest;
var resid_residui;
histogram  / cfill=lightblue normal(color=red w=3) kernel(color=green w=3);
inset mean std="Std Dev" median min max kurtosis skewness NORMALTEST="Shapiro-Wilk test" PNORMAL="p-value" / cfill=lightgray;
qqplot / normal(mu=est sigma=est color=red w=2);
cdfplot / normal(color=red w=2); /*cdf = cumulative distribution function*/
ppplot;
ID id_road;
run;


/*Per avere anche i test di curtosi e di skewness si deve impiegare la PROC MODEL
  del prodotto di SAS/ETS - Econometric and Time Series*/

title2 'test di asimmetria e curtosi';
proc model data=PM_10;
   pm10 = beta0 + beta1*cars + beta2*temp  + beta3*wind  + beta4*d_temp  + beta5*hours  ;
   fit pm10 / normal;
run;
quit;
title2;


/*4) DIAGNOSTICHE SUI RESid_residui - VERIFICA DELLE IPOTESI DI OMOSCHEDASTICITA' E INCORRELAZIONE*/
title "REG: diagnosi su omoschedasticità e incorrelazione dei resid_residui";
ods graphics on;
proc reg data=PM_10; 
	 mod1: model  pm10 = cars temp  wind   / SPEC DWPROB; 
	  /* Opzioni:
	    'spec': test per la verifica della presenza di eteroschedasticità (Test di White);
	    'dwprob': test di Durbin-Watson per la verifica della presenza di correlazione seriale fra
	              i resid_residui del modello.
	    'dw': fornisce solo il valore della statistica di Durbin-Watson
	  */
	  run;
ods graphics off;

/*Grafici di interesse:*/
title "Res and Obs by predicted";
ods graphics on;
proc reg data=PM_10 
         plots(only label)=(ResidualPlot RESIDUALBYPREDICTED Observedbypredicted OBSERVEDBYPREDICTED); 
	  mod1: model pm10 = cars temp  wind   ;
	  ID id_road;
	  run;
quit;
ods graphics off;

/*
OBSERVEDBYPREDICTED <(LABEL)> 
plots dependent variable values by the predicted values. 
If you specify the LABEL option, then points deemed as outliers or influential are labeled. 

RESid_roadUALBYPREDICTED <(LABEL)> 
plots resid_roaduals by predicted values. If you specify the LABEL option, then points deemed as outliers or influential 
are labeled.

*/


/****************************/
/*-------- ESERCIZI --------*/
/****************************/
/*
  1. Dataset 'PM_10': Ripetere le analisi di cui sopra con il modello completo (mod2).
  2. Dataset 'companies': Procedere allo studio completo di diagnostica su più modelli costruiti a piacere.
*/

