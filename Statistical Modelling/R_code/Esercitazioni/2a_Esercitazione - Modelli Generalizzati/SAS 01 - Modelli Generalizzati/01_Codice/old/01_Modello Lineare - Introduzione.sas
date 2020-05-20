/************************************************************************/
/******************** ANALISI ESPLORATIVE DESCRITTIVE *******************/
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


*----------------------------------------------------------------*
/******************* ANALISI ESPLORATIVE DESCRITTIVE *************/
*----------------------------------------------------------------*;

  /* Matrice di dati 'companies'*/ 
  /*Caricamento del file esterno: companies.txt come "Tab delimited file"
    mediante la procedura: "File => Importa dati", attivabile dal menu*/

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
proc format;  
      value macroname 
         1='sett. terziario' 
         2='sett. servizi' 
         3='sett. commercio' 
         4='sett. finanza'
         5='altri settori'; 
run;

title 'Dataset: companies';
 data companies; 
 set companies;
 /*assegnazione di etichette alle variabili*/
 label assets = "attivo in bilancio"
	   sales = "fatturato"
	   mark_val = "valore di mercato"
       profits = "profitto"
       cash = "flusso di cassa"
       employ = "numero di dipendenti"
	   sector = "settore di mercato"
	  ;
 run;


  proc print data=companies label;
  id company;
  run;

/*----------- STATISTICHE DESCRITTIVE -----------*/
/*------ (ANALISI ESPLORATIVA PRELIMINARE) ------*/

/*MEDIE E DEVIAZIONI STANDARD delle variabili quantitative con PROC MEANS*/

/*SINTASSI ESSENZIALE di PROC MEANS*/
/*
PROC MEANS <option(s)> <statistic-keyword(s)>;  
 VAR variable(s) < / WEIGHT=weight-variable>;  
 WEIGHT variable;  
*/

title2 'Medie, deviazioni standard, minimo e massimo valori osservati';
proc means data=companies;
var mark_val assets sales profits cash employ;
run;

/*Osservazione 1: Se non si specifica l'elenco delle variabili quantitative nello statement 'var': */
proc means data=companies;
run;

/*si ottiene lo stesso risultato ottenuto in precedenza, ossia la variabile 'sector' non viene
  considerata dall'algoritmo. Questo perche' il formato della variabile è 'Char' ossia 'carattere':*/

proc contents data=companies;
run;

/*Osservazione 2: Se non si specifica il nome dei dati: */
proc means;
run;

/*si ottiene ancora una volta lo stesso risultato. In generale, se in una PROC si omette il nome del dataset 
  SAS automaticamente esegue i comandi specificati nella PROC sull'ultimo dataset utilizzato.*/


/*MATRICE DI CORRELAZIONE (VARIABILI QUANTITATIVE)*/
/*SINTASSI ESSENZIALE di PROC CORR*/
/*
PROC CORR <options> ; 
VAR variables ; 
WITH variables ; 
*/

/*Con l'opzione 'drop=' si puo' eliminare la variabile qualitativa*/
/*Matrice con i grafici a dispersione.*/

  title2 'Matrice di correlazione';
  /*ODS = Output Delivery System, necessario per la creazione dei grafici richiesta all'interno di una PROC */
   ods graphics on;
   proc corr data=companies(drop=sector) noprob plots=matrix(histogram NVAR=6);
   /*n.b.: per costruire la matrice con tutte le variabili si puo' usare
    l'opzione: plots=matrix(NVAR=ALL)*/
   /*opzione 'noprob': suppresses displaying the probabilities associated with each correlation coefficient. */
   run;
   ods graphics off;

 /*Osservazione: per la stessa ragione di cui sopra, la matrice di correlazione si puo' costruire anche 
   omettendo 'drop=sector':*/
   proc corr data=companies;
   run;

 /*APPROFONDIMENTO RIGUARDANTE I GRAFICI A DISPERSIONE*/
   /*Dalla matrice dei grafici a dispersione si possono osservare situazioni riguardo alla relazione
     fra coppie di variabili. Ad esempio:

   - profits vs. cash: i punti tendono a disporsi lungo una retta. In particolare vi e' una intensa relazione lineare
                       fra le due variabili (il coefficiente di correlazione lineare e' pari a 0.8556)
                       con una dispersione contenuta dei punti intorno a tale retta.
                       Inoltre la direzione della relazione e' positiva, nel senso che all'aumentare dei valori
                       di cash i valori di profits tendono ad aumentare. 
                       Infine, non si rileva la presenza di punti isolati, ossia unita' anomale (outlier).
   - assets vs. profits: i punti tendono a disporsi secondo una relazione parabolica con concavita' verso l'alto,
                         ad eccezione di un piccolo gruppo di punti corrispondente a valori piu' elevati di assets 
                         e intermedi per profits. Inoltre, si possono osservare altre due unita' isolate
                         nella parte sinistra inferiore del grafico, ossia in corrispondenza di valori bassi di profits.
   - assets vs. employ: il nucleo maggioritario dei dati tende a disporsi secondo una forma circolare.
                        Si notano in particolare alcune unita' anomale in corrispondenza di valori elevati di assets
                        e bassi valori di employ, e una unita' anomala in corrispondenza del valore massimo assunto da 
                        employ.
   */

/*-------------------------------------------------*/
/*--- GRAFICO A DISPERSIONE DI PROFITS VS. CASH ---*/
/*-------------------------------------------------*/

title 'Profits vs. Cash';
proc sgplot data=companies;
  scatter x=cash y=profits;
run;

/*con il nome delle compagnie*/
proc sgplot data=companies;
  scatter x=cash y=profits / datalabel=company;
run;

/*con un simbolo e un colore diverso; indicazione del numero di unita', del valore del coefficiente di correlazione
  e del p-value del test per la verifica della nullita' del coefficiente di correlazione*/
proc sgplot data=companies;
  scatter x=cash y=profits / datalabel=company MARKERATTRS=(symbol=CircleFilled color=red);
  inset ("n" = "= 64"
         "(*ESC*){unicode rho}" = "= 0.8556" 
         "p-value" = "<.0001")/ border;
run;

/*RIPRENDI DA QUA DOPO IMPORTAZIONE COMPANIES*/

/*Costruzione della retta di regressione: PROC REG*/
/* Sintassi essenziale di PROC REG:

PROC REG < options > ; 
< label: > MODEL dependents=<regressors> < / options > ; 
WEIGHT variable ; 
PLOT <yvariable*xvariable> <=symbol> 
      < ...yvariable*xvariable> <=symbol> < / options > ; 
*/

title 'Modello di regressione lineare semplice: Profits vs. Cash';
proc reg data=companies corr simple;
/* 'corr': per visualizzare la matrice di correlazione; 
   'simple': per visualizzare gli indici di statistica descrittiva;
*/
/* Opzioni dello statement 'MODEL': 
	'STB' = per visualizzare in output le stime standardizzate dei parametri;
	'CLB': intervalli di confidenza per i parametri del modello.
*/
      model  profits=cash /STB CLB;  
	  run;


/*Inserimento della retta di regressione nel grafico a dispersione di Profits vs. Cash*/
title 'Profits vs. Cash: Linear Fit Function';
proc sgplot data=companies;
  reg x=cash y=profits;
run;

/*cambiando i parametri grafici:*/
proc sgplot data=companies noautolegend;
  reg x=cash y=profits / MARKERATTRS=(symbol=CircleFilled color=red) LINEATTRS=(THICKNESS=.5mm color=black)
                         /*inserimento dell'equazione della retta di regressione stimata con OLS*/
                         CURVELABEL="profits=-7.49748 + 0.55014 cash";
run;

/*Posizionamento dell'equazione al di fuori del grafico*/
proc sgplot data=companies noautolegend;
  reg x=cash y=profits / MARKERATTRS=(symbol=CircleFilled color=red) LINEATTRS=(THICKNESS=.5mm color=black)
                         CURVELABEL="profits=-7.49748 + 0.55014 cash" CURVELABELLOC=OUTSIDE;
run;

/*oppure:*/
proc sgplot data=companies noautolegend;
  reg x=cash y=profits / MARKERATTRS=(symbol=CircleFilled color=red) LINEATTRS=(THICKNESS=.5mm color=black)
                         CURVELABEL="profits=-7.49748 + 0.55014 cash" CURVELABELPOS=MIN CURVELABELLOC=OUTSIDE;
run;

/*altra possibilita':*/
proc sgplot data=companies noautolegend;
  reg x=cash y=profits / MARKERATTRS=(symbol=CircleFilled color=red) LINEATTRS=(THICKNESS=.5mm color=black);
  inset "Retta di regressione OLS:" "profits=-7.49748 + 0.55014 cash" "R(*ESC*){sup '2'} = 0.7321"/ border;
run;


/*---------------------------------------------------*/
/*--- GRAFICO A DISPERSIONE DI PROFITS VS. ASSETS ---*/
/*---------------------------------------------------*/

title 'Assets vs. Profits';
proc sgplot data=companies;
  scatter x=profits y=assets / MARKERATTRS=(symbol=SquareFilled color=green);
  inset ("n" = "= 64"
         "(*ESC*){unicode rho}" = "= 0.28308"
         "p-value" = "= 0.0234") / border;
run;


/*Nella precedente analisi svolta sulla matrice dei grafici a dispersione si erano gia' notati
  due gruppi di outliers, l'uno in corrispondenza di valori elevati di assets, l'altro di valori bassi di profits. 
  I grafici boxplot costruiti per ciascuna delle due variabili permettono di evidenziare ulteriormente questo aspetto.
*/

title 'Boxplot di assets';
proc sgplot data=companies;
  vbox assets / datalabel=company;
run;

title 'Boxplot di profits';
proc sgplot data=companies;
  vbox profits / datalabel=company;
run;

/*Nel grafico bivariato:*/
title 'Assets vs. Profits - identificazione delle unità anomale';
proc sgplot data=companies;
  scatter x=profits y=assets / datalabel=company MARKERATTRS=(symbol=SquareFilled color=blue);
  inset ("n" = "= 64"
         "(*ESC*){unicode rho}" = "= 0.28308"
         "p-value" = "= 0.0234") / border;
run;

/*Identificazione dei valori estremi con PROC UNIVARIATE*/
title 'Valori estremi di profits e assets';
ods select BasicMeasures Quantiles ExtremeObs; /*per visualizzare in output solo alcune specifiche tabelle*/
proc univariate data=companies;
   var profits assets;
   id company;
run;

/*ESEMPIO DI RICODIFICA DELLE VARIABILI CON IF... THEN...; ELSE...; */
/*Costruzione di due variabili "dummy outlier", una per ciascuna variabile,
  rispetto all'identificazione effettuata dal grafico bivariato*/
/*Due procedure alternative che portano agli stessi risultati*/
/*Prima procedura: usando i valori delle variabili*/
  title2 'Prima procedura: Var. dummy outlier';
   data companies;
   set companies;
   if profits < -170 then dum_out_prof=1;
      else dum_out_prof=0;
   if assets > 8900 then dum_out_ass=1;
      else dum_out_ass=0;
   run;
/*
Operatori di confronto: > (maggiore), < (minore), >= (maggiore o uguale), 
    <= (minore o uguale), = (uguale), ^= (diverso) 
*/


/*DISTRIBUZIONE DI FREQUENZE delle due variabili dummy con PROC FREQ*/
/*SINTASSI ESSENZIALE di PROC FREQ*/
/*
PROC FREQ <options> ; 
TABLES requests </ options> ; 
*/
   title3 'Distribuzioni di frequenze';
   proc freq data=companies;
   tables dum_out_prof dum_out_ass;
   run;

 /*Seconda procedura: usando il nome delle compagnie individuate*/
  title2 'Seconda procedura: Var. dummy outlier';
   data companies;
   set companies;
   if company='TWA' | company='IU International' then dum_out_prof2=1;
      else dum_out_prof2=0;
   if company='Norstar Bancorp' | company='Sovran Financial' | company='Textron' | company='Southeast Banking' | 
      company='National City' then dum_out_ass2=1; 
      else dum_out_ass2=0;
   run;
  /*Operatori logici: | (OR logico), & (AND logico), ^ (NOT logico)*/

   title3 'Distribuzioni di frequenze';
   proc freq data=companies;
   tables dum_out_prof2 dum_out_ass2;
   run;

  /*Per eliminare i sottotitoli*/
   title2;
   title3;

 /*Costruzione della variabile complessiva 'outlier'*/
  title2 'Variabile var_outlier';
   data companies;
   set companies;
   var_outlier = dum_out_prof + dum_out_ass;
   run;

   proc freq data=companies;
   tables var_outlier;
   run;

   /*Stampa in output del report con i valori degli outliers*/
   /*Statement: where*/
   proc print data=companies(where=(var_outlier^=0));
   var profits assets var_outlier;
   id company;
   run;
   title2;

/*Identificazione nel grafico a dispersione delle 7 unita' anomale:*/
title 'Assets vs. Profits - identificazione delle unità anomale';
proc sgplot data=companies;
  scatter x=profits y=assets / group=var_outlier MARKERATTRS=(symbol=SquareFilled);
  inset ("n" = "= 64"
         "(*ESC*){unicode rho}" = "= 0.28308"
         "p-value" = "= 0.0234") / border;
run;

title 'Modello di regressione lineare semplice: Assets vs. Profits';
proc reg data=companies corr simple;
      model assets=profits /STB CLB;  
	  run;
quit;

proc reg data=companies ;
      model assets=profits;  
	  run;
quit;

/*L'indice R2 del modello e' prossimo a zero. Si considera il modello parabolico:

	  assets= beta0 + beta1*profits + beta2*profits^2 + epsilon

  Si tratta di un polinomio di regressione di grado 2.
  Il modello e' comunque lineare nei parametri.
*/

/*Costruzione della variabile profits^2*/
data companies;
set companies;
label profits2 = 'profitto al quadrato';
profits2 = profits**2;
run;
/*Operazioni matematiche: + (addizione), - (sottrazione), * (moltiplicazione), / (divisione), 
  ** (elevamento a potenza)*/


title 'Polinomio di regressione di grado 2: Assets vs. Profits';
proc reg data=companies corr simple;
model assets=profits profits2 / CLB;  
run;
quit;


/*Inserimento della retta e della parabola di regressione nel grafico a dispersione di Assets vs. Profits*/
title 'Assets vs. Profits: Linear & Quadratic Fit Function';
proc sgplot data=companies;
/*Polinomio di grado 1 (retta di regressione)*/
  reg x=profits y=assets / LINEATTRS=(THICKNESS=.75mm color=black);
/*Polinomio di grado 2 (parabola di regressione)*/
  reg x=profits y=assets / DEGREE=2 MARKERATTRS=(symbol=SquareFilled color=blue) LINEATTRS=(THICKNESS=.75mm color=red);
run;

/*Inserimento della legenda:*/
title 'Assets vs. Profits: Linear & Quadratic Fit Function';
proc sgplot data=companies;
/*Polinomio di grado 1 (retta di regressione)*/
  reg x=profits y=assets / LINEATTRS=(THICKNESS=.75mm color=black) name="lin" legendlabel="Linear Regression";
/*Polinomio di grado 2 (parabola di regressione)*/
  reg x=profits y=assets / DEGREE=2 MARKERATTRS=(symbol=SquareFilled color=blue) LINEATTRS=(THICKNESS=.75mm color=red)
        name="quadr" legendlabel="Quadratic Regression";
  keylegend "lin" "quadr";
run;

/*VALUTAZIONE DELL'IMPATTO DELLE UNITA' ANOMALE*/
/*Eliminazione delle 7 unita' anomale dal dataset*/
data companies_rid; /*nuovo dataset, ridotto poiche' non conterra' i 7 outliers*/
set companies;
if var_outlier=1 then delete;
run;

/*Ricostruzione dei due modelli di regressione*/
title 'Polinomi di regressione senza outliers: Assets vs. Profits';
proc reg data=companies_rid corr simple;
    Linear:  model assets=profits / CLB;  
    Quadratic:  model assets=profits profits2 / CLB;  
	run;
quit;

proc sgplot data=companies_rid;
  reg x=profits y=assets / LINEATTRS=(THICKNESS=.75mm color=black) name="lin" legendlabel="Linear Regression";
  reg x=profits y=assets / DEGREE=2 MARKERATTRS=(symbol=SquareFilled color=blue) LINEATTRS=(THICKNESS=.75mm color=red)
        name="quadr" legendlabel="Quadratic Regression";
  keylegend "lin" "quadr";
run;


/*--------------------------------------------------*/
/*--- GRAFICO A DISPERSIONE DI ASSETS VS. EMPLOY ---*/
/*--------------------------------------------------*/

title 'Assets vs. Employ';
proc sgplot data=companies;
  scatter x=employ y=assets / MARKERATTRS=(symbol=DiamondFilled color=gray60);
  inset ("n" = "= 64"
         "(*ESC*){unicode rho}" = "= 0.11052"
         "p-value" = "= 0.3846") / border;
run;

/*Costruzione delle variabili employ^2, employ^3, employ^4, employ^5*/
data companies;
set companies;
employ2 = employ**2;
employ3 = employ**3;
employ4 = employ**4;
employ5 = employ**5;
run;

title 'Polinomi di regressione fino al 5° grado: Assets vs. Employ';
proc reg data=companies corr simple;
    Linear:     model assets=employ / CLB;  
    Quadratic:  model assets=employ employ2 / CLB;  
	Cubic:      model assets=employ employ2 employ3 / CLB;  
    Quartic:    model assets=employ employ2 employ3 employ4 / CLB;  
    Quintic:    model assets=employ employ2 employ3 employ4 employ5 / CLB;  
	run;
quit;

/*Adattamento di vari polinomi di regressione*/
proc sgplot data=companies;
  reg x=employ y=assets / LINEATTRS=(THICKNESS=.75mm color=black) name="lin" legendlabel="Linear Regression";
  reg x=employ y=assets / DEGREE=2 LINEATTRS=(THICKNESS=.75mm color=red)
        name="quadr" legendlabel="Quadratic Regression";
  reg x=employ y=assets / DEGREE=3 LINEATTRS=(THICKNESS=.75mm color=darkgreen)
        name="cub" legendlabel="Cubic Regression";
  reg x=employ y=assets / DEGREE=4 LINEATTRS=(THICKNESS=.75mm color=blue)
        name="quart" legendlabel="Quartic Regression";
  reg x=employ y=assets / DEGREE=5 MARKERATTRS=(symbol=DiamondFilled color=gray60) 
        LINEATTRS=(THICKNESS=.75mm color=cyan)
        name="quint" legendlabel="Quintic Regression";
  keylegend "lin" "quadr" "cub" "quart" "quint";
run;
title;

/*Tutti i polinomi di regressione considerati hanno un basso adattamento ai dati.*/
















/*--------------------------------------------------------------*/
/*-------- VARIABILE QUALITATIVA (O CATEGORIALE) "sector" ------*/
/*--------------------------------------------------------------*/

/*DISTRIBUZIONE DI FREQUENZE CON PROC FREQ*/
   title "Variabile categoriale 'sector'";
   title2 'Distribuzione di frequenze e bar chart';
   ods graphics on;
   proc freq data=companies;
   tables sector / nocum plots=freqplot;
   /*NOCUM: Suppresses display of cumulative frequencies and percentages */
   run;
   ods graphics on;

   /*Per costruire il bar chart con le frequenze percentuali:*/
   /*Opzione SCALE=FREQ | LOG | PERCENT | SQRT
     specifies the scale of the frequencies to display. The SCALE= plot-option applies 
     to the frequency plot (FREQPLOT) and the cumulative frequency plot (CUMFREQPLOT). 
     The default is SCALE=FREQ, which displays unscaled frequencies. 
     SCALE=LOG displays log (base 10) frequencies. SCALE=PERCENT displays percentages (relative frequencies). 
     SCALE=SQRT displays square roots of the frequencies, which produces a plot known as a rootogram. 
   */

   ods graphics on;
   proc freq data=companies;
   tables sector / nocum plots=freqplot(scale=percent);
   run;
   ods graphics off;


 /*ESEMPIO DI RICODIFICA DELLE VARIABILI*/
 /*Costruzione di variabili dummy (o variabili indicatrici) per insiemi di settori*/
   /*I settori vengono dunque aggregati in macro-settori poiche' alcuni di essi hanno
     associate frequenze molto basse*/
  title2 'Variabili dummy di sector';
   data companies;
   set companies;
   /*macro-settore del terziario*/
   if sector='Communication' | sector='HiTech' then dum_terz=1;
      else dum_terz=0;
   /*macro-settore dei servizi*/
   if sector='Energy' | sector='Transportation' then dum_serv=1;
      else dum_serv=0;
   /*macro-settore del commercio*/
   if sector='Manufacturing' | sector='Retail' then dum_comm=1;
      else dum_comm=0;
   /*settore finanziario*/
   if sector='Finance' then dum_fin=1; 
      else dum_fin=0;
   /*macro-settore medico e altri settori*/
   if sector='Medical' | sector='Other' then dum_other=1;
      else dum_other=0;
   run;
  /*Operatori logici: | (OR logico), & (AND logico), ^ (NOT logico)*/

  /*Assegnazione delle label alle variabili dummy*/
   data companies;
   set companies;
    label dum_terz = 'sett. terziario'
	   dum_serv = 'sett. servizi'
	   dum_comm = 'sett. commercio'
       dum_fin = 'sett. finanziario'
       dum_other = 'altri settori'
	  ;
   run;


proc print data=companies label;
var dum_terz dum_serv dum_comm dum_other dum_fin;
id company;
run;























