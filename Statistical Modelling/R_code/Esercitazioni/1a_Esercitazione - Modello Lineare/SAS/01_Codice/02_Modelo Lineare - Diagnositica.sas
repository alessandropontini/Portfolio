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
lm(LENGTH ~ DIAMETER +HEIGHT +WHOLE_WEIGHT+SHUCKED_WEIGHT+VISCERA_WEIGHT+SHELL_WEIGHT+RINGS

/* DIRECTORY DI LAVORO: */
/*Modificare inserendo il percorso opportuno:*/

LIBNAME User 'C:\Users\daniele.riggi\Desktop\Corso Bicocca Vittadini\07_Esercitazioni Vitta\1a_Esercitazione - Modello Lineare\SAS'; run;

/*APRIRE LA VISUALIZZAZIONE DELL'OUTPUT IN HTML*/


/*****************************************/
/********* DIAGNOSTICHE SUL MODELLO ******/
/*****************************************/

/*ESEMPIO: AEROBIC FITNESS PREDICTION 
  (esempio nei lucidi di teoria tratto dal manuale di SAS/STAT) */


   *-------------------Data on Physical Fitness-------------------* 
   | These measurements were made on men involved in a physical   | 
   | fitness course at N.C.State Univ. The variables are Age      | 
   | (years), Weight (kg), Oxygen intake rate (ml per kg body     | 
   | weight per minute), time to run 1.5 miles (minutes), heart   | 
   | rate while resting, heart rate while running (same time      | 
   | Oxygen rate measured), and maximum heart rate recorded while | 
   | running.                                                     | 
   | ***Certain values of MaxPulse were changed for this analysis.| 
   *--------------------------------------------------------------*; 

/*
Variabili:
Age: età (in anni)
Weight: peso (in kg.)
Oxygen: tasso di ossigenazione (ml/kg. al minuto)
RunTime: tempo di percorrenza di 1.5 miglia (minuti)
RestPulse: battito cardiaco a riposo
RunPulse: battito cardiaco durante la corsa
MaxPulse: battito cardiaco massimo durante la corsa
*/


 /*MATRICE DI CORRELAZIONE insieme ad alcune statistiche descrittive*/
 /*COSTRUZIONE DELLA MATRICE DEI GRAFICI A DISPERSIONE*/

 title 'Aerobic Fitness';
 ods graphics on; 
  proc corr data=fitness(drop=id) noprob plots=matrix(histogram nvar=all);
  run;
 ods graphics off; 

/*1) DIAGNOSTICHE DI COLLINEARITA'*/
title "REG: diagnostiche di collinearità";
proc reg data=Fitness; 
 mod1: model  Oxygen= RunTime MaxPulse RunPulse  / TOL VIF COLLIN ; 
 mod2: model  Oxygen= Age Weight RunTime RestPulse RunPulse MaxPulse / TOL VIF COLLIN ; 
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
data=Fitness; 
	  mod1: model  Oxygen= RunTime MaxPulse RunPulse / INFLUENCE; 
	  mod2: model  Oxygen= Age Weight RunTime RestPulse RunPulse MaxPulse / INFLUENCE;
	  /*Opzione 'influence': per il calcolo delle varie statistiche di influenza,
	  	come i residui studentizzati e i valori 'leverage'. Permettono di 
	    stabilire l'influenza di ciascuna unità sulle stime dei parametri.*/
	  run;
ods graphics off; 

/* PER LO STUDIO DEI LEVERAGES:
Valore soglia: 2p/n, dove p= n.ro di parametri nel modello (intercetta inclusa) e n = n.ro di unità.
Nel caso del modello 1: p=4, n=31, da cui: 2p/n = 0.258. 
Nel caso del modello 2: p=7, n=31, da cui: 2p/n = 0.452.
Le osservazioni a cui corrisponde un leverage che supera la soglia dovrebbero essere analizzate ulteriormente.

N.B.: per l'indicazione dei valori soglia sulle altre statistiche di influenza
consultare l'help on-line di SAS.
*/

title "REG: grafici per le diagnostiche di influenza";
proc reg data=Fitness; 
	 /*mod1: Modello a tre regressori*/
	  mod1: model  Oxygen=RunTime MaxPulse RunPulse / INFLUENCE noprint; 
	  /* Statement 'output': per salvare come file di dati esterni le principali
	     statistiche diagnostiche: 
	  		h=per i leverages; 
	  		covratio=per l'influenza sul determinante della matrice di var-cov; 
	  		student=per i residui studentizzati;
	     	rstudent=per i residui studentizzati 'jackknife'; 
	     	dffits=per la misura dell'influenza della singola osservazione sulla previsione dei valori della variabile dipendente.
	  */
	  output out=FitnessDiagn 
				h=hatdiag
				covratio=cov_rat
				rstudent=rst_res
				dffits=pred_infl;
	  run;
	  quit;

/*GRAFICI PER IDENTIFICARE LE UNITA'*/
  goptions reset=(symbol);
  symbol1 i=needle;
  /*INTERPOL=NEEDLE: draws a vertical line from each data point to a horizontal line at the 0 value 
    on the vertical axis or the minimum value on the vertical axis. The horizontal line is drawn automatically.
  */
  axis1 label=none
      order = 1 to 31 by 1
      offset=(2);

  axis2 label=(angle=90);
 
 /*OFFSET=(<n1><,n2>)<units > | (<n1<units>><,n2<units>>) 
   specifies the distance from the first and last major tick marks or bars to the ends of the axis line.
  */

   title 'Leverage plot';
   proc gplot data=FitnessDiagn;
   plot hatdiag*id=1 / vref=0.258 cvref=red lvref=3 
                       haxis=axis1 hminor=0;
   /*HMINOR=number-of-minor-ticks: specifies the number of minor tick marks drawn between 
					                each major tick mark on the horizontal axis. */
   run;
   quit;
   

   title 'Studentized residual - jackknife';
   proc gplot data=FitnessDiagn;
   plot rst_res*id=1 / vref=-2 2 cvref=(red red) lvref=3
                       haxis=axis1 hminor=0
                       vaxis=axis2 vminor=0;
   run;
   quit;

   title 'Cov Ratio plot';
   proc gplot data=FitnessDiagn;
   plot cov_rat*id=1 / vref=0.613 1.387 cvref=(green green) lvref=3
                       haxis=axis1 hminor=0
                       vaxis=axis2 vminor=0;
   run;
   quit;

   /*Valori soglia per il Cov-ratio plot: 
     1) covratio <= 1 - 3p/n = 1 - 3(4)/31 = 0.613
     2) covratio >= 1 + 3p/n = 1 + 3(4)/31 = 1.387
   */


   title 'DFFITS plot';
   proc gplot data=FitnessDiagn;
   plot pred_infl*id=1 / vref= -0.718 0.718 cvref=(red red) lvref=3
                         haxis=axis1 hminor=0
                         vaxis=axis2 vminor=0;
   run;
   quit;
/*Valore soglia (in valore assoluto): 2*sqrt(p/n) = 2*sqrt(4/31) = 0.718 
*/

   goptions reset=(axis);

/*ALTRI GRAFICI COSTRUITI CON LA PROCEDURA ODS GRAPHICS*/
ods graphics on;
proc reg data=FitnessDiagn plots(only label)=(RStudentByLeverage DFFITS );
id id;
model Oxygen=RunTime MaxPulse RunPulse;
run;
ods graphics off;

ods graphics on;
proc reg data=FitnessDiagn plots(only)=(DFBETAS(label));
id id;
model Oxygen=RunTime MaxPulse RunPulse;
run;
ods graphics off;

/* Valore soglia per DFBETAS:
Observations whose DFBETAS’ statistics for a regressor are greater in magnitude than 
2/sqrt(n), where n is the number of observations used, are deemed to be influential for that regressor.

In questo caso: 2/sqrt(31) = 0.359
*/


/*GRAFICO COSTRUITO CON LO STATEMENT PLOT*/
goptions reset=(symbol);
symbol v=dot h=1 c=green; 
/*
HEIGHT=symbol-height<units> 
Alias H= 
specifies the height in number of units of plot symbols. 
*/

proc reg data=Fitness; 
	  mod1: model  Oxygen=RunTime MaxPulse RunPulse; 
	  /*GRAFICO PER IDENTIFICARE LE UNITA' sulla statistica COVRATIO*/
      plot COVRATIO.*obs. / href= 1 to 31 by 1 chref=red lhref=3 cframe=ligr; 
	  	  run;
		  quit;


/* DISTANZA DI COOK */
title "REG: distanza di Cook";
ods graphics on;
proc reg data=FitnessDiagn plots(only)=(COOKSD(label));
id id;
model Oxygen=RunTime MaxPulse RunPulse;
run;
ods graphics off;

/* Valore soglia per COOKSD:
COOKSD <(LABEL)> plots Cook’s D statistic by observation number. Observations whose Cook’s D 
statistic lies above the horizontal reference line at value 4/n, where n is the number of observations used, 
are deemed to be influential. 

In questo caso: 4/n = 4/31 = 0.129.
*/

proc reg data=Fitness; 
	  mod1: model  Oxygen=RunTime MaxPulse RunPulse / R; 
	  /* Opzione 'R': per richiedere un'analisi dei residui, 
	  	  che comprende il calcolo della distanza di Cook.
	  */
/*GRAFICO PER IDENTIFICARE LE UNITA' sulla distanza di Cook*/
      plot cookd.*obs. / href= 1 to 31 by 1 chref=red lhref=3 cframe=ligr; 
	  	  run;
		  quit;

/* N.B.: Se dai risultati precedenti si dovesse ritenere che una o più unità statistiche
		 influiscano eccessivamente sull'analisi di regressione, sarebbe opportuno eliminare
		 tali unità dall'insieme complessivo e ripetere le analisi sui dati restanti.
		 Ad esempio, se si vuole eliminare l'unità n.ro 10 poiché è caratterizzata sia da un leverage
		 che da una distanza di Cook troppo elevati, è possibile creare un nuovo dataset che non la
		 contenga e ripetere su quest'ultimo la costruzione del modello di regressione.
		  */

title "Eliminazione unità n.ro 10";
data fitness_rid;
set fitness;
if _N_=10 then delete;
run;

title "Modello senza l'unità n.ro 10";
ods graphics on;
proc reg data=fitness_rid plots(only)=(COOKSD(label));
id id;
model Oxygen=RunTime MaxPulse RunPulse;
run;
quit;
ods graphics off;

/******************************************************************************
ESERCIZIO: ripetere l'analisi sul dataset fitness ridotto eliminando l'unità 4.
********************************************************************************/


/*3) DIAGNOSTICHE PER LA NORMALITA' DEGLI ERRORI*/

/* TEST DI NORMALITA' SUI RESIDUI */
   /*Salviamo in un file esterno i residui di regressione*/
title 'Test di normalità';
ods graphics on;
proc REG data=fitness plots(only label)=(RESIDUALBOXPLOT RESIDUALHISTOGRAM);
     mod1: model Oxygen=RunTime MaxPulse RunPulse ; 
OUTPUT OUT=RESfitness R=residui; 
/*Opzione: 
'r=nome': per inserire nel file 'RESfitness' la colonna dei residui di regressione. 
*/
run;
quit;
ods graphics off;


proc print data=RESfitness(obs=5) label; /*con (obs=5) vengono visualizzate in output le prime 5 unità*/
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
proc univariate data=RESfitness normaltest;
var residui;
histogram  / cfill=lightblue normal(color=red w=3) kernel(color=green w=3);
inset mean std="Std Dev" median min max kurtosis skewness NORMALTEST="Shapiro-Wilk test" PNORMAL="p-value" / cfill=lightgray;
qqplot / normal(mu=est sigma=est color=red w=2);
cdfplot / normal(color=red w=2); /*cdf = cumulative distribution function*/
ppplot;
id id;
run;


/*Per avere anche i test di curtosi e di skewness si deve impiegare la PROC MODEL
  del prodotto di SAS/ETS - Econometric and Time Series*/

title2 'test di asimmetria e curtosi';
proc model data=fitness;
   Oxygen=beta0 + beta1*RunTime + beta2*MaxPulse + beta3*RunPulse;
   fit Oxygen / normal;
run;
quit;
title2;


/*4) DIAGNOSTICHE SUI RESIDUI - VERIFICA DELLE IPOTESI DI OMOSCHEDASTICITA' E INCORRELAZIONE*/
title "REG: diagnosi su omoschedasticità e incorrelazione dei residui";
ods graphics on;
proc reg data=Fitness; 
	  mod1: model  Oxygen=RunTime MaxPulse RunPulse / SPEC DWPROB; 
	  /* Opzioni:
	    'spec': test per la verifica della presenza di eteroschedasticità (Test di White);
	    'dwprob': test di Durbin-Watson per la verifica della presenza di correlazione seriale fra
	              i residui del modello.
	    'dw': fornisce solo il valore della statistica di Durbin-Watson
	  */
	  run;
ods graphics off;

/*Grafici di interesse:*/
title "Res and Obs by predicted";
ods graphics on;
proc reg data=Fitness 
         plots(only label)=(ResidualPlot RESIDUALBYPREDICTED Observedbypredicted OBSERVEDBYPREDICTED); 
	  mod1: model Oxygen=RunTime MaxPulse RunPulse; 
	  id id;
	  run;
quit;
ods graphics off;

/*
OBSERVEDBYPREDICTED <(LABEL)> 
plots dependent variable values by the predicted values. 
If you specify the LABEL option, then points deemed as outliers or influential are labeled. 

RESIDUALBYPREDICTED <(LABEL)> 
plots residuals by predicted values. If you specify the LABEL option, then points deemed as outliers or influential 
are labeled.

*/


/****************************/
/*-------- ESERCIZI --------*/
/****************************/
/*
  1. Dataset 'fitness': Ripetere le analisi di cui sopra con il modello completo (mod2).
  2. Dataset 'companies': Procedere allo studio completo di diagnostica su più modelli costruiti a piacere.
*/

