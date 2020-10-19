/*********************************************************************/
/*************************** MODELLO SURE ****************************/
/*********************************************************************/


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

LIBNAME User 'C:\Users\daniele.riggi\Desktop\Corso Bicocca Vittadini\07_Esercitazioni Vitta\3a_Esercitazione\SAS - Modelli Sure'; run;


/*APRIRE LA VISUALIZZAZIONE DELL'OUTPUT IN HTML*/


/*********************************************************
* MODELLO SURE (Seemingly Unrelated Regression Equations) *
*********************************************************/

/* Procedura di SAS: PROC SYSLIN 

Sintassi essenziale:

PROC SYSLIN options ; 
MODEL response = regressors / options ; 
OUTPUT PREDICTED= variable RESIDUAL= variable ; 
STEST equations ; 
TEST equations ; 
*/

/*ESEMPIO 1: dall'help on-line di SAS*/

   *---------Zellner's Seemingly Unrelated Technique------------* 
   | A. Zellner, "An Efficient Method of Estimating Seemingly   | 
   | Unrelated Regressions and Tests for Aggregation Bias,"     | 
   | JASA 57(1962) pp.348-364                                   | 
   |                                                            | 
   | J.C.G. Boot, "Investment Demand: an Empirical Contribution | 
   | to the Aggregation Problem," IER 1(1960) pp.3-30.          | 
   |                                                            | 
   | Y. Grunfeld, "The Determinants of Corporate Investment,"   | 
   | Unpublished thesis, Chicago, 1958                          | 
   *------------------------------------------------------------*; 

/* Questa applicazione si trova nel lavoro di Zellner (1962). 
   Il modello economico di riferimento, sviluppato da Grunfeld, 
   riguarda la domanda di investimenti:
   la domanda corrente di investimenti lordi di un'azienda e' funzione
   (lineare) del suo capitale sociale all'inizio dell'anno e del valore
   delle azioni in circolazione all'inizio dell'anno.
*/
    
/* DESCRIZIONE DEI DATI 'GRUNFELD':
Serie storica (1935 - 1954) relativa a due societa': General Electric e Westinghouse.
Variabili considerate per entrambe le societa':
i = current gross investment (investimento lordo corrente, ossia al tempo t);
c = beginning-of-year capital stock (capitale all'inizio dell'anno, o anche al tempo t-1);
f = value of outstanding shares at the beginning of the year 
   (valore delle azioni all'inizio dell'anno, o anche al tempo t-1).

Per ciascuna societa' la domanda di investimenti lordi e' rappresentata dall'equazione:
i(t) = beta0 + beta1*c(t-1) + beta2*f(t-1) + u(t),     t=1,...., T .
*/

   data grunfeld; 
   /*The prefix GE stands for General Electric and WH stands for Westinghouse */
      input year ge_i ge_f ge_c wh_i wh_f wh_c; 
      label ge_i = 'Gross Investment, GE' 
            ge_c = 'Capital Stock Lagged, GE' 
            ge_f = 'Value of Outstanding Shares Lagged, GE' 
            wh_i = 'Gross Investment, WH' 
            wh_c = 'Capital Stock Lagged, WH' 
            wh_f = 'Value of Outstanding Shares Lagged, WH'; 
      datalines; 
   1935     33.1      1170.6    97.8      12.93     191.5     1.8 
   1936     45.0      2015.8    104.4     25.90     516.0     .8 
   1937     77.2      2803.3    118.0     35.05     729.0     7.4 
   1938     44.6      2039.7    156.2     22.89     560.4     18.1 
   1939     48.1      2256.2    172.6     18.84     519.9     23.5 
   1940     74.4      2132.2    186.6     28.57     628.5     26.5 
   1941     113.0     1834.1    220.9     48.51     537.1     36.2 
   1942     91.9      1588.0    287.8     43.34     561.2     60.8 
   1943     61.3      1749.4    319.9     37.02     617.2     84.4 
   1944     56.8      1687.2    321.3     37.81     626.7     91.2 
   1945     93.6      2007.7    319.6     39.27     737.2     92.4 
   1946     159.9     2208.3    346.0     53.46     760.5     86.0 
   1947     147.2     1656.7    456.4     55.56     581.4     111.1 
   1948     146.3     1604.4    543.4     49.56     662.3     130.6 
   1949     98.3      1431.8    618.3     32.04     583.8     141.8 
   1950     93.5      1610.5    647.4     32.24     635.2     136.7 
   1951     135.2     1819.4    671.3     54.38     723.8     129.7 
   1952     157.3     2079.7    726.1     71.78     864.1     145.5 
   1953     179.5     2371.6    800.3     90.08     1193.5    174.8 
   1954     189.6     2759.9    888.9     68.60     1188.9    213.5 
   ;
run;

  title 'Grunfeld data';
  proc print data=grunfeld(obs=5) label;
  id year;
  run;

 proc corr data=grunfeld(drop=year) noprob;
 run;

 /*NOTA: la procedura SYSLIN consente di applicare vari metodi di stima,
 fra i quali anche il metodo dei minimi quadrati ordinari (OLS), 
 che rappresenta il metodo di stima di default.
 */

 /*Stima dei parametri dei due modelli con il metodo OLS (un'equazione alla volta)*/

 title 'OLS estimation';
 proc syslin data=grunfeld simple out=OLSgrun_resid; 
 /*Nello statment di PROC SYSLIN e' possibile specificare fra le opzioni:
  'out=': per salvare in un file esterno i valori previsti e i residui di regressione
  (questa opzione richede che successivamente venga specificato lo statement 'output',
  ripetuto tante volte quante sono le equazioni, mediante il quale viene dato un nome
  alle colonne che riportano i residui e i valori previsti);
  'sur', 'ols': sono fra i possibili metodi di stima supportati da PROC SYSLIN.
                Se non si specifica nulla, il metodo di stima considerato e' 'ols':
  'simple': per visualizzare le principali statistiche descrittive per le variabili dipendenti;
  */
  /**** EQUAZIONE PER GE ****/
      ge:      model ge_i = ge_f ge_c /stb dw ; 
	  		   output predicted= OLSge_pred residual=OLSge_res; 
  /**** EQUAZIONE PER WH ****/
      westing: model wh_i = wh_f wh_c /stb dw ; 
  	  		   output predicted= OLSwes_pred residual=OLSwes_res; 
 /* Statement 'model': per specificare le M equazioni dei modelli di regressione.
	  Uso: <label>: MODEL var. dip. = regressori / opzioni;
	Fra le possibili opzioni:
	  'dw': per visualizzare la statistica di Durbin-Watson e i coefficienti di autocorrelazione
	        fra i residui. Conviene usare questa opzione nel caso di serie storiche;
	  'noint': per sopprimere l'intercetta dal modello;
	  'stb': per produrre i coefficienti di regressione standardizzati.
 */
   run;


 /*Specificazione del modello SURE con PROC SYSLIN.
   N.B.: il metodo di stima SUR e' anche detto JGLS,
   ossia Joint Generalized Least Squares*/
 title 'SUR estimation';
 proc syslin data=grunfeld sur simple out=SURgrun_resid; 
 /**** EQUAZIONE PER GE ****/
       ge:     model ge_i = ge_f ge_c /stb dw ; 
	  		   output predicted= SURge_pred residual=SURge_res; 
  /*STATEMENT TEST: Test F sui parametri del singolo modello. 
	Verifica di ipotesi di tipo lineare. Si utilizza dopo aver specificato
	l'equazione del singolo modello.
  */
  /*test F per la verifica dell'uguaglianza dei due coeff. di regr. entro GE: 
			   H0: beta(ge_f) = beta(ge_c)*/
			   ge: test ge_f=ge_c;
 /**** EQUAZIONE PER WH ****/
      westing: model wh_i = wh_f wh_c /stb dw ; 
  	  		   output predicted= SURwes_pred residual=SURwes_res; 
   /*test F per la verifica dell'uguaglianza dei due coeff. di regr. entro WH: 
			   H0: beta(wh_f) = beta(wh_c)*/
			   wh: test wh_f=wh_c;
  /*STATEMENT STEST: Test F sui parametri del modello congiunto.
	Verifica di ipotesi di tipo lineare. Si utilizza dopo aver specificato
	tutte le equazioni del modello SURE.
  */
   /*test F per la verifica dell'uguaglianza dei due coeff. di regr. riferiti alla stessa variabile
	 esplicativa ma sulle due equazioni: 
			   ESEMPIO 1:
			   H0: beta(ge_f) = beta(wh_f) & beta(ge_c) = beta(wh_c) 
			   (due ipotesi verificate congiuntamente => 2 gradi di liberta') */
	 joint: stest ge.ge_f=westing.wh_f, ge.ge_c = westing.wh_c;
	          /*ESEMPIO 2: Verifiche di singole ipotesi sulle due equazioni
              	H0: beta(ge_f) = beta(wh_f) 
	            H0: beta(ge_c) = beta(wh_c) 
	          */
	 jointF: stest ge.ge_f=westing.wh_f;
	 jointC: stest ge.ge_c = westing.wh_c;
   run;


   proc print data=OLSgrun_resid(obs=5); 
   title2 'OLS predicted and residual values';
   id year;
   run;
   proc print data=SURgrun_resid(obs=5); 
   title2 'SUR predicted and residual values';
   id year;
   run;

/*Costruzione di grafici per l'analisi dei residui*/

goptions reset=global gunit=pct border cback=white
         colors=(black red)
         ftitle=swissb ftext=swiss htitle=6 htext=4;
 
symbol color=red
        interpol=none
        value=dot
       POINTLABEL=("#year");
 
   
proc gplot data=OLSgrun_resid;
  plot 	OLSge_res*OLSge_pred / vref=0;
  title 'Grunfeld data';
  title2 'GE: OLS Residuals versus Predicted Values';
run;

proc gplot data=SURgrun_resid;
  plot 	SURge_res*SURge_pred / vref=0;
  title 'Grunfeld data';
  title2 'GE: SUR Residuals versus Predicted Values';
run;

   
proc gplot data=OLSgrun_resid;
  plot 	OLSwes_res*OLSwes_pred / vref=0;
  title 'Grunfeld data';
  title2 'WH: OLS Residuals versus Predicted Values';
run;

proc gplot data=SURgrun_resid;
  plot 	SURwes_res*SURwes_pred / vref=0;
  title 'Grunfeld data';
  title2 'WH: SUR Residuals versus Predicted Values';
run;
quit;

/*Grafici sovrapposti*/
goptions reset=global gunit=pct border cback=white
         colors=(black red blue)
         ftitle=swissb ftext=swiss htitle=6 htext=4;
 
symbol1 color=red
        interpol=none
        value=dot
       POINTLABEL=("#year");

symbol2 color=blue
        interpol=none
        value=triangle
       POINTLABEL=("#year");

legend1 label=none
        value=("GE" "WH")
        shape=symbol(4,2)
        position=(top center outside)
        mode=share;


proc gplot data=SURgrun_resid;
  plot 	SURge_res*SURge_pred=1 SURwes_res*SURwes_pred=2 / overlay vref=0 legend=legend1;
  title 'Grunfeld data';
  title2 'GE-WH: SUR Residuals versus Predicted Values';
run;
quit;

goptions reset=all;

/*---------- ESEMPIO DI EQUAZIONI CON REGRESSORI DIVERSI --------------*/
 title 'SUR estimation - different regressors';
 proc syslin data=grunfeld sur; 
 /**** EQUAZIONE PER GE ****/
       ge:     model ge_i = ge_f ge_c; 
  /*test F per la verifica dell'uguaglianza dei due coeff. di regr. entro GE a valori prefissati: 
			   H0: beta(ge_f) = 0.05 e H0: beta(ge_c) = 0.15 */
			   geF: test ge_f=0.05;
			   geC: test ge_c=0.15;
 /**** EQUAZIONE PER WH ****/
      westing: model wh_i = wh_f; /*qui si e' omesso il regressore C*/
   /*test F per la verifica dell'uguaglianza del coeff. di regr. di F entro WH ad un valore prefissato: 
			   H0: beta(wh_f) = 0.05*/
			   whF: test wh_f=0.05;
   /*test F per la verifica dell'uguaglianza dei due coeff. di regr. riferiti alla variabile F
              	H0: beta(ge_f) = beta(wh_f) */
	 jointF: stest ge.ge_f=westing.wh_f;
   run;


/******************************** MODELLO SURE *********************************/
/*						               				         		           */		
/* Come organizzare i dati da una matrice di dati con struttura 'tradizionale' */
/*******************************************************************************/

/* Se i dati non sono organizzati come nell'esempio precedente, 
   dove la matrice di dati costruita si puo'
   vedere come l'unione di due matrici che riguardano l'una la General
   Electric, l'altra la Westinghouse, ma piuttosto hanno questa forma: */

title 'Grunfeld matrix data';
proc format;
value socname
 	1='GE'
   	2='WH';
data grunfeld2; 
    input soc year i f c; 
	/*format soc socname.;*/
    label i = 'Gross Investment' 
          c = 'Capital Stock Lagged' 
          f = 'Value of Outstanding Shares Lagged'; 
datalines; 
 1  1935     33.1      1170.6    97.8
 1  1936     45.0      2015.8    104.4
 1  1937     77.2      2803.3    118.0
 1  1938     44.6      2039.7    156.2
 1  1939     48.1      2256.2    172.6
 1  1940     74.4      2132.2    186.6
 1  1941     113.0     1834.1    220.9
 1  1942     91.9      1588.0    287.8
 1  1943     61.3      1749.4    319.9
 1  1944     56.8      1687.2    321.3
 1  1945     93.6      2007.7    319.6
 1  1946     159.9     2208.3    346.0
 1  1947     147.2     1656.7    456.4
 1  1948     146.3     1604.4    543.4
 1  1949     98.3      1431.8    618.3
 1  1950     93.5      1610.5    647.4
 1  1951     135.2     1819.4    671.3
 1  1952     157.3     2079.7    726.1
 1  1953     179.5     2371.6    800.3
 1  1954     189.6     2759.9    888.9
 2   1935     12.93     191.5     1.8 
 2   1936     25.90     516.0     0.8 
 2   1937     35.05     729.0     7.4 
 2   1938     22.89     560.4     18.1 
 2   1939     18.84     519.9     23.5 
 2   1940     28.57     628.5     26.5 
 2   1941     48.51     537.1     36.2 
 2   1942     43.34     561.2     60.8 
 2   1943     37.02     617.2     84.4 
 2   1944     37.81     626.7     91.2 
 2   1945     39.27     737.2     92.4 
 2   1946     53.46     760.5     86.0 
 2   1947     55.56     581.4     111.1 
 2   1948     49.56     662.3     130.6 
 2   1949     32.04     583.8     141.8 
 2   1950     32.24     635.2     136.7 
 2   1951     54.38     723.8     129.7 
 2   1952     71.78     864.1     145.5 
 2   1953     90.08     1193.5    174.8 
 2   1954     68.60     1188.9    213.5
   ;
run;

title2 'stacked data';
proc print data=grunfeld2; 
id soc;
run;
 
/*per riorganizzare i dati ai fini della costruzione
  del modello SURE si puo' procedere come nel seguito:
*/

/* 1. Si creano due datasets, uno per ogni societa',
      che poi verranno uniti come accostamento di colonne.
      E' necessario rinominare le variabili.
*/
data GEdata(drop=soc rename=(i=ge_i c=ge_c f=ge_f)); 
   set grunfeld2; 
   if soc=2 then delete; 
run; 

data WHdata(drop=soc rename=(i=wh_i c=wh_c f=wh_f)); 
   set grunfeld2; 
   if soc=1 then delete; 
run; 

/*Visualizzazione dei nuovi nomi delle variabili*/
proc contents data=Gedata short; run;
proc contents data=WHdata short; run;


/*2. Si effettua il 'merging' dei due datasets. 
     Conviene tenere una variabile in comune ai due datasets
     come chiave di riferimento per l'accostamento
     in colonna. In questo caso: 'year'.
*/

data grunfeld3; 
  merge GEdata WHdata;
  by year;
run; 
 
proc print data=grunfeld3; 
   title2 'Merging GE and WH'; 
   title3 'by year'; 
run; 
title2;
title3;

/* Il dataset appena creato 'grunfeld3' e' identico a quello usato
   in origine: 'grunfeld'.
*/

/*----------------------------------------------------------------------*/
/*ESERCIZIO - EFFETTUARE LE ANALISI PROPOSTE NELL'ESEMPIO 2 ED ESAMINARE*/
/*            I RISULTATI OTTENUTI                                      */
/*----------------------------------------------------------------------*/

/*ESEMPIO 2: Dataset 'Cigar' */
title 'Cigar data';
proc print data=cigar(obs=5) label;
run;

/*Si eliminano le colonne che contengono le elaborazioni di precedenti analisi*/
proc contents data=cigar short;
run;

data cigar;
set cigar(keep=STATE YR price pop pop_sup16 CPI NDI C PMIN);
run;

proc print data=cigar(obs=5) label;
run;


/*Numero di osservazioni per stato */
proc freq data=cigar;
table state;
run;

/*Matrice di correlazione*/
title2 'Correlation matrix';
proc corr data=cigar(drop=STATE YR) noprob; run;

title2 'Within-state correlation matrix';
proc corr data=cigar(drop=YR) nosimple noprob; 
by STATE;
run;

/*Consideriamo l'analisi sui primi 5 stati*/
/*Costruzione del dataset di lavoro*/
data cigar1(drop=state rename=(yr=yr price=price_1 pop=pop_1 pop_sup16=pop_sup16_1 CPI=CPI_1 NDI=NDI_1 C=C_1 PMIN=PMIN_1)); 
   set cigar; 
   if state=1; 
run; 

data cigar3(drop=state rename=(yr=yr price=price_3 pop=pop_3 pop_sup16=pop_sup16_3 CPI=CPI_3 NDI=NDI_3 C=C_3 PMIN=PMIN_3)); 
   set cigar; 
   if state=3; 
run; 

data cigar4(drop=state rename=(yr=yr price=price_4 pop=pop_4 pop_sup16=pop_sup16_4 CPI=CPI_4 NDI=NDI_4 C=C_4 PMIN=PMIN_4)); 
   set cigar; 
   if state=4; 
run; 

data cigar5(drop=state rename=(yr=yr price=price_5 pop=pop_5 pop_sup16=pop_sup16_5 CPI=CPI_5 NDI=NDI_5 C=C_5 PMIN=PMIN_5)); 
   set cigar; 
   if state=5; 
run; 

data cigar7(drop=state rename=(yr=yr price=price_7 pop=pop_7 pop_sup16=pop_sup16_7 CPI=CPI_7 NDI=NDI_7 C=C_7 PMIN=PMIN_7)); 
   set cigar; 
   if state=7; 
run; 

title2 'First 5 states';
/*Unione dei cinque datasets separati*/
data cigar_state1_5; 
  merge cigar1 cigar3 cigar4 cigar5 cigar7;
  by yr;
run; 
 
proc print data=cigar_state1_5(obs=5); 
id yr;
run;

proc corr data=cigar_state1_5(drop=YR) nosimple noprob;
run;


/*Primo esempio: equazioni con gli stessi regressori*/
   title2 'SURE model - same regressors';
 proc syslin data=cigar_state1_5 sur outest=state1_5_par; 
      STATE_1:   model C_1 = price_1 pop_1 pop_sup16_1 CPI_1 NDI_1 PMIN_1 / dw ; 
      STATE_3:   model C_3 = price_3 pop_3 pop_sup16_3 CPI_3 NDI_3 PMIN_3 / dw ; 
	  STATE_4:   model C_4 = price_4 pop_4 pop_sup16_4 CPI_4 NDI_4 PMIN_4 / dw ; 
	  STATE_5:   model C_5 = price_5 pop_5 pop_sup16_5 CPI_5 NDI_5 PMIN_5 / dw ; 
      STATE_7:   model C_7 = price_7 pop_7 pop_sup16_7 CPI_7 NDI_7 PMIN_7 / dw ; 
	/*Test complessivo per la verifica congiunta di ipotesi su sottoinsiemi di parametri*/
JOINTALL: stest STATE_1.price_1=STATE_3.price_3=STATE_4.price_4=STATE_5.price_5=STATE_7.price_7,
			STATE_1.pop_1=STATE_3.pop_3=STATE_4.pop_4=STATE_5.pop_5=STATE_7.pop_7,
			STATE_1.pop_sup16_1=STATE_3.pop_sup16_3=STATE_4.pop_sup16_4=STATE_5.pop_sup16_5=STATE_7.pop_sup16_7,
			STATE_1.CPI_1=STATE_3.CPI_3=STATE_4.CPI_4=STATE_5.CPI_5=STATE_7.CPI_7,
			STATE_1.NDI_1=STATE_3.NDI_3=STATE_4.NDI_4=STATE_5.NDI_5=STATE_7.NDI_7,
			STATE_1.PMIN_1=STATE_3.PMIN_3=STATE_4.PMIN_4=STATE_5.PMIN_5=STATE_7.PMIN_7;
	/*Tests per la verifica di ipotesi su sottoinsiemi di parametri considerati uno alla volta*/
Jprice:      stest STATE_1.price_1=STATE_3.price_3=STATE_4.price_4=STATE_5.price_5=STATE_7.price_7;
Jpop:        stest STATE_1.pop_1=STATE_3.pop_3=STATE_4.pop_4=STATE_5.pop_5=STATE_7.pop_7;
Jpop_sup16:  stest STATE_1.pop_sup16_1=STATE_3.pop_sup16_3=STATE_4.pop_sup16_4=STATE_5.pop_sup16_5=STATE_7.pop_sup16_7;
JCPI:        stest STATE_1.CPI_1=STATE_3.CPI_3=STATE_4.CPI_4=STATE_5.CPI_5=STATE_7.CPI_7;
JNDI:   	 stest STATE_1.NDI_1=STATE_3.NDI_3=STATE_4.NDI_4=STATE_5.NDI_5=STATE_7.NDI_7;
JPMIN:       stest STATE_1.PMIN_1=STATE_3.PMIN_3=STATE_4.PMIN_4=STATE_5.PMIN_5=STATE_7.PMIN_7;
run;


/*Secondo esempio: equazioni formate con regressori diversi*/
   title2 'SURE model - different regressors';
 proc syslin data=cigar_state1_5 sur outest=state1_5_par; 
      STATE_1:   model C_1 = pop_sup16_1  / dw ; 
      STATE_3:   model C_3 = CPI_3 / dw ; 
	  STATE_4:   model C_4 = pop_sup16_4 / dw ; 
	  STATE_5:   model C_5 = price_5 pop_5 / dw ; 
      STATE_7:   model C_7 = price_7  / dw ; 
	  /*Test complessivo per la verifica congiunta di ipotesi su sottoinsiemi di parametri*/
	  stest STATE_5.price_5=STATE_7.price_7,
			STATE_1.pop_sup16_1=STATE_4.pop_sup16_4;
	  /*Tests per la verifica di ipotesi su sottoinsiemi di parametri considerati uno alla volta*/
Jprice_state57:      stest STATE_5.price_5=STATE_7.price_7;
Jpop_sup16_state14:  stest	STATE_1.pop_sup16_1=STATE_4.pop_sup16_4;
run;

