/*************************************************************/
/*---------------- MODELLI MULTILIVELLO ---------------------*/
/*************************************************************/


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

LIBNAME User 'C:\Users\daniele.riggi\Desktop\Esame Vitta - Statistical Modeling\esercitazioni\4a_Esercitazione - Modello Multilevel\SAS - Multilevel\02_Dati'; run;


/*APRIRE LA VISUALIZZAZIONE DELL'OUTPUT IN HTML*/


/*************************************************************/
/*---------------- MODELLI MULTILIVELLO ---------------------*/
/*************************************************************/
/*Importazione del file 'nlschools.txt' come 'File delimitato'*/

/* DATASET DESCRIPTION:

This dataset contains 2287 rows and the following columns: 
lang = language test score (dependent variable)
IQ = Verbal IQ 
class = class ID (133 classi in tutto)
GS = Class size: number of eighth-grade pupils recorded in the class 
   (there may be others: see COMB, and some may have been omitted with missing values). 
SES = Social-economic status of pupil's family. 
COMB = were the pupils taught in a multi-grade class (0/1)? 
   Classes which contained pupils from grades 7 and 8 are coded 1, but only eighth-graders were tested. 

Ruolo delle variabili:
 
lang = variabile dipendente di primo livello;
IQ = variabile indipendente di primo livello;
SES = variabile indipendente di primo livello;
GS = variabile indipendente di secondo livello;
COMB = variabile indipendente di secondo livello.
   */

title 'Eighth-Grade Pupils in the Netherlands';
data nlschools;
set nlschools;
label lang = "language test score" 
      IQ = "verbal IQ"
	  class = "class ID"
	  GS = "class size"
	  SES = "family socio-economic status"
	  COMB= "dummy of multi-grade class";
run;

proc print data=nlschools(obs=10) label; 
run;

/*---------- ANALISI PRELIMINARE ---------*/

/*Distribuzione di frequenze delle classi*/
proc freq data=nlschools nlevels;
/*
'nlevels'= per visualizzare il numero di livelli (=valori o categorie)
           della variabile di cui si costruisce la distribuzione di frequenze.
*/
table class;
run;


/*Determinazione degli indici di statistica descrittiva*/
title2 'Descriptive indices';
 proc means data= nlschools;
 run;
/*n.b.: poiche' nel file di origine le colonne "id", "class", "COMB" 
contengono valori scritti fra virgolette, sono interpretate automaticamente da SAS
come variabili in formato stringa. Pertanto non vengono coinvolte nel computo
degli indici di statistica descrittiva.*/

/*Determinazione delle medie di gruppo*/
 proc means data= nlschools;
 class class /ascending;
 run;

/*Matrice di correlazione*/
title2 'Correlation matrix';
proc corr data=nlschools; 
run;


/* BOXPLOTS di lang entro classi*/
/*Procedura preliminare: ordinamento dei dati in base a 'class'*/
title2 'Box plots of lang distribution within classes';
 proc sort data=nlschools;
by class;
run;

   proc boxplot data=nlschools;
      plot lang*class;
      inset nobs mean / header = 'Overall Stats' pos=sw;
   run;


/*------------------- MODELLI MULTILIVELLO CON PROC MIXED --------------------*/

/* SINTASSI ESSENZIALE:

PROC MIXED < options > ; 
CLASS variables ; 
ID variables ; 
MODEL dependent = < fixed-effects > < / options > ; 
RANDOM random-effects < / options > ; 
*/

/*-------- Modello preliminare: 1-way ANOVA ad effetti fissi ------------*/
   /*N.B.: non si tratta di un modello multilivello, poiche' in esso
   la struttura gerarchica dei dati, in questo caso a due livelli, 
   non viene rappresentata mediante l'introduzione degli effetti casuali.
   Tuttavia puo' rappresentare un passo preliminare importante
   per comprendere al meglio le analisi successive. 
   La procedura MIXED permette comunque di costruire anche modelli che
   contengono solo effetti fissi. */

title '1-way fixed-effects ANOVA';
proc mixed data = nlschools;
  class class;
  model lang = class / solution chisq cl;
 /*Alcune opzioni dello statement 'model':
  'solution' = per visualizzare le stime degli effetti fissi;
  'chisq' = per visualizzare i test di significativita' per gli effetti fissi;
  'cl' = per visualizzare gli intervalli di confidenza per gli effetti fissi basati sulla
         statistica t di Student.
  */
run;


/*--- Costruzione di alcuni modelli multilivello ---*/
/*-------- Empty model: 1-way ANOVA ad effetti casuali ------------*/

/* 
LANGij =  0j + rij   
where 
0j =  00 + u0j 

rij ~ N(0, 2) 
u0j ~ N(0, 2)


*/


title 'Empty model';
proc mixed data = nlschools cl covtest noitprint;
/* Alcune opzioni:
'cl' = per visualizzare gli intervalli di confidenza per le componenti di varianza
		(metodo di Wald, basato sulla distribuzione Chi-Quadrato);
'covtest' = per visualizzare gli standard error asintotici e il test Z di Wald per
            le componenti di varianza;
'noitprint' = per sopprimere in output la visualizzazione della cronologia delle iterazioni.
'method=' = per specificare il metodo di stima. Per default è REML (REstricted Maximum Likelihood).
            Altra possibile opzione: ML.
*/
  ods output SolutionR =RandomEffects;
  /*'ods output SolutionR=' = per salvare in un file di dati esterni le previsioni BLUP degli
     effetti casuali con gli intervalli di previsione.*/
  class class;
  model lang =  / solution chisq cl;
  random intercept / subject = class type = un solution cl;  
 /*Alcune opzioni dello statement 'random' (per indicare i parametri casuali del modello):
  'subject=' = per specificare l'unità di secondo livello; 
  'type=' = per specificare il tipo di struttura della matrice di var-cov degli effetti casuali.
            Opzione: 'un'= matrice di var-cov non strutturata;
  'solution' = per visualizzare le previsioni puntuali degli effetti casuali, ossia i valori previsti
               dei residui di secondo livello;
  'cl' = per visualizzare gli intervalli di confidenza per gli effetti casuali costruiti con
         la distributione t di Student.
  */
run;
quit;


/*


The estimated between variance,  2 corresponds to the term INTERCEPT in the output of Covariance Parameter Estimates 
UN(1,1)  = 19.5370
and the estimated within variance, 2, corresponds to the term RESIDUAL in the same output section.
Residual = 64.5124


INTRACLASS CORRELATION
Tells us the portion of the total variance that occurs between class.

19.5370 / (19.5370  + 64.5124) = 0.232446633

To measure the magnitude of the variation among schools in their language test score , 
we can calculate the plausible values range for these means, based on the between variance we obtained from the model: 

40.3650 ± 1.96*(19.5370)^1/2 = (21.21874, 59.51126).

*/



/*ANALISI SUGLI EFFETTI CASUALI (spesso sono interpretati come un "parametro" di efficacia o di efficienza
  delle unita' di secondo livello, in questo caso le classi.)*/
proc print data=RandomEffects(obs=9);
run;
/* Estimate Lower Upper */

/*Con l'obiettivo di costruire un grafico che riporti i valori previsti
degli effetti casuali e i loro intervalli di previsione, il dataset "RandomEffects"
viene ordinato in senso crescente rispetto alle previsioni degli effetti casuali.
In questo modo verranno evidenziate le classi dall'efficacia superiore rispetto
al Language Test Score.*/

proc sort data=RandomEffects;
by Estimate;
run;


/*Per comodita' si costruisce una colonna con l'ID delle classi
ordinato rispetto alle previsioni ottenute degli effetti casuali.*/
data RandomEffects;
set RandomEffects;
ID =_N_;
run;

proc print data=RandomEffects(obs=9);
run;


/*--- HIGH-LOW PLOT ----*/
/*1. Prepare the data for a high-low plot. 
RANEFF generates three records for each CLASS, storing each classes' high, low, and estimate values 
in variable raneff. */

data RanEff;
   set RandomEffects;
   drop Alpha DF Effect Estimate Lower Probt StdErrPred Upper tValue;
   raneff=Upper; output;
   raneff=Lower; output;
   raneff=Estimate; output;
run;

proc print data=RanEff(obs=9);
run;

/*2. Define titles and footnote. 
JUSTIFY=RIGHT in the FOOTNOTE statement causes the footnote to be displayed in the bottom right. */

goptions reset=all;
title1 "Random-Effects Prediction - 1-way Random-effects ANOVA";
title2 "Class Efficacy on the Language Test Score";
footnote justify=right "NLSCHOOLS ";

/*3. Define symbol characteristics. 
INTERPOL=HILOCTJ specifies that the minimum and maximum values of RANEFF are joined 
by a vertical line with a horizontal tick mark at each end. 
The Estimate values are joined by straight lines. The CV= option controls the color of the symbol (vertical axis). 
The CI= and WIDTH= options control the color and the thickness of the line that joins the Estimates points. */

symbol interpol=hiloctj
       cv=black
       ci=blue
       width=1.25;

/*4. Define characteristics of the horizontal axis. 
	   The ORDER= option uses a SAS date value to set the major tick marks. 
	   The OFFSET= option moves the first and last tick marks to make room for the tick mark value. */

axis1 order=(1 to 133 by 2)
      offset=(3,3)
      label=("ID classes");

/*5. Define characteristics of the vertical axis. 
	  LABEL=NONE suppresses the AXIS label. */

axis2 label=none
      offset=(2,2);

/*6. Generate the plot and assign AXIS definitions. 
The HAXIS= option assigns AXIS1 to the horizontal axis, 
	  and the VAXIS= option assigns AXIS2 to the vertical axis. */

proc gplot data=raneff;
   plot raneff*ID /  haxis=axis1 vaxis=axis2 vref=0 lvref=2;
run;
quit;
goptions reset=all;


/*---------- Random intercept model con una var. esplicativa: IQ ----------*/


/* 
The motivation of this model is the question on if the class with high IQ also have language test score ,
In other words, we want to understand why there is a class difference on language test score . 
In terms of regression equations, we have the following.

LANGij =  0j + rij   
where 
0j =  00 + 01(IQ) + u0j

rij ~ N(0, 2) 
u0j ~ N(0, 2)

Combining the two equations into one by substituting the level-2 equation to level-1 equation, we have

LANGij  =  00 + 01(IQ) + u0j + rij
*/


title 'Random-intercept model';
  proc mixed data = nlschools cl covtest noitprint;
  ods output SolutionR = RandomEffects2;
  class class;
  model lang = 	IQ / solution chisq cl ;
  random intercept / subject = class type = un solution cl;  
  run;
  quit;

/* 
The coefficient for the constant is the predicted language test score 
when all predictors are 0, so when the average class IQ is 0, the students language score achievement is predicted to be 11.1669

The variance component representing variation between class decreases greatly (from  19.5370  to 9.4908). 
This means that the level-2 variable IQ explains a large portion of the Class-to-Class variation in mean of Language test score. 
More precisely, the proportion of variance explained by IQ is (19.5370 - 9.4908 )/19.5370 = .514, that is about 51% of the explainable variation in class mean Language test score
is explained by IQ.

A range of plausible values for CLASS means, given that all CLASS have IQ of zero, is 11.1669  ± 1.96 *(9.4908)1/2 = (1.865916, 20.467884).

We can also calculate the conditional intraclass correlation conditional on the values of IQ. 
9.4908/ (9.4908+42.2542) = .183414823
measures the degree of dependence among observations within class that are of the same IQ

Do class achievement means still vary significantly once IQ is controlled? 
From the output of Covariance Parameter Estimates, we see that the test that between variance is zero is highly significant. 
Therefore, we conclude that after controlling for IQ, significant variation among class Language test score still remains to be explained.

Covariance Parameter Estimates 
Cov Parm Subject Estimate Standard
				Error 	Z 	Value Pr > Z Alpha Lower Upper 
UN(1,1) class 	9.4908 	1.5672 6.06 <.0001 0.05 7.0372 13.5025 
Residual   		42.2542 1.2900 32.75 <.0001 0.05 39.8355 44.9011 
*/

/*High-Low Plot*/
proc sort data=RandomEffects2;
by Estimate;
run;

data RandomEffects2;
set RandomEffects2;
ID2 =_N_;
run;


data RanEff2;
   set RandomEffects2;
   drop Alpha DF Effect Estimate Lower Probt StdErrPred Upper tValue;
   raneff=Upper; output;
   raneff=Lower; output;
   raneff=Estimate; output;
run;


title1 "Random-Effects Prediction";
title2 "Class Efficacy on the Language Test Score - Rnd-Intc model with IQ";
footnote justify=right "NLSCHOOLS ";

symbol interpol=hiloctj
       cv=black
       ci=blue
       width=1.25;

axis1 order=(1 to 133 by 2)
      offset=(3,3)
      label=("ID classes");
axis2 label=none
      offset=(2,2);


proc gplot data=raneff2;
   plot raneff*ID2 /  haxis=axis1 vaxis=axis2 vref=0 lvref=2;
run;
quit;

goptions reset=all;


/*--------- Random intercept model con due var. esplicative: IQ e SES ------------*/



/* 
The motivation of this model is the question on if the class with high IQ and SES also have language test score ,
In other words, we want to understand why there is a class difference on language test score . 
In terms of regression equations, we have the following.

LANGij =  0j + rij   
where 
0j =  00 + 01(IQ) + 02(SES) + u0j

rij ~ N(0, 2) 
u0j ~ N(0, 2)

Combining the two equations into one by substituting the level-2 equation to level-1 equation, we have

LANGij  =  00 + 01(IQ) + 02(SES) + u0j + rij
*/


title 'Random-intercept model';
  proc mixed data = nlschools cl covtest noitprint;
  ods output SolutionR = RandomEffects3;
  class class;
  model lang = 	IQ SES / solution chisq cl;
  random intercept / subject = class type = un solution cl;  
run;



  

/* 
The coefficient for the constant is the predicted language test score 
when all predictors are 0, so when the average class IQ is 0 AND SES is 0 , the students language score achievement is predicted to be 9.3933

The variance component representing variation between class decreases greatly (from  19.5370  to 9.1503 ). 
This means that the level-2 variables IQ and SES explains a large portion of the Class-to-Class variation in mean of Language test score. 
More precisely, the proportion of variance explained by IQ and SES is (19.5370 - 9.1503  )/19.5370 = .0.532, 
that is about 53% of the explainable variation in class mean Language test score
is explained by IQ and SES.

A range of plausible values for CLASS means, given that all CLASS have IQ and SES of zero, is 9.3933 ± 1.96 *(9.1503)1/2 = (0.426006, 18.360594).

We can also calculate the conditional intraclass correlation conditional on the values of IQ and SES. 
9.1503/ (9.1503 + 40.0329) = .186
measures the degree of dependence among observations within class that are of the same IQ and SES

Do class achievement means still vary significantly once IQ and SES is controlled? 
From the output of Covariance Parameter Estimates, we see that the test that between variance is zero is highly significant. 
Therefore, we conclude that after controlling for IQ and SES , significant variation among class Language test score still remains to be explained.


Covariance Parameter Estimates 
Cov Parm Subject Estimate Standard
Error 			Z Value Pr > Z Alpha Lower Upper 
UN(1,1) class 	9.1503 1.4869 6.15 <.0001 0.05 6.8147 12.9387 
Residual   		40.0329 1.2216 32.77 <.0001 0.05 37.7425 42.5392 


*/



 /*------- Random-coefficients model: IQ random -----------------*/

/* 
This model is referred as a random-coefficient model. Pretend that we run regression of lang on IQ on each Class, that is we are going to run 133 regressions.

What would be the average of the 133 regression equations (both intercept and slope)?
How much do the regression equations vary from class to class ?
What is the correlation between the intercepts and slopes?
These are some of the questions that motivates the following model.

LANGij =  0j + 1j IQ + rij   
where 
0j =  00 + 01 (SES) + u0j

1j =  10  + u1j

rij ~ N(0, 2) 
u0j ~ N(0, 2)

Combining the two equations into one by substituting the level-2 equation to level-1 equation, we have

LANGij  =  00 + 01 (SES) + u0j   + 10 (IQ)  + u1j(IQ)  + rij

*/

  title 'Random-coefficient model with IQ';
  proc mixed data = nlschools cl covtest noitprint;
  ods output SolutionR = RandomEffects4;
  class class;
  model lang = 	IQ SES / solution chisq cl;
  random intercept IQ / subject = class type = un solution cl gcorr;  
run;


/*
UN(1,1) indica la prima componente random
UN(2,2) indica la seconda compomente random
UN(2,1) indica la covarianza tra le due compenti random 

Specifying level-1 predictor IQ as random effect, we formulate that effect of IQ can vary across schools.

The option type = un in the random statement allows us to estimate from the data the three parameters 
	- the variance of intercept 
	- the variance of slopes for IQ 
	- the covariance between them

Option gcorr displays the correlation matrix corresponding to the estimated variance-covariance matrix, called G matrix.

The covariance estimate is -3.6087  with standard error 1.4039 
That yields a p-value of 0.0102 . This is saying that there is evidence that the effect of IQ depending up
on the average language score achievement in the school.

In the output of Covariance Parameter Estimates,  the parameter corresponding to UN(2,2) is the variability in slopes of cses. 
The estimate is  0.2186 with standard error  0.1017. That yields a p-value of 0.0158 for 1-tailed test. 
The test being significant tells us that we can not accept the hypothesis that there is no difference in slopes among class.

The 95% plausible value range for the class means is 9.0074 ± 1.96 *(63.9552)1/2 = (-53.668696, 71.683496).

The 95% plausible value range for the IQ-achievement slope is  2.3022 ± 1.96 *(0.2186)1/2 = (2.087972, 2.516428).

Notice that the residual variance is now 39.2398, comparing with the residual variance of 64.5124 in the one-way ANOVA with random effects model. 
We can compute the proportion variance explained at level 1 by (64.5124 – 39.2398) / 64.5124 = 0.39174794. 
This means using student-level IQ and SES as a predictor of language score achievement in the school reduced the within-class variance by 39.2%.






 /*High-Low Plots*/
/*Separazione in due del dataset "RandomEffects4":
1. Dataset che riporta le previsioni per l'effetti casuali associati all'intercetta (RanEff_Intc);
2. Dataset che riporta le previsioni per l'effetti casuali associati al coefficiente di regressione
   di IQ (RanEff_IQ).
*/
data RanEff_Intc;
set RandomEffects4;
if Effect="IQ" then delete;
run;

data RanEff_IQ;
set RandomEffects4;
if Effect="Intercept" then delete;
run;

/*High-plot per gli effetti casuali dell'intercetta*/
proc sort data=RanEff_Intc;
by Estimate;
run;

data RanEff_Intc;
set RanEff_Intc;
ID4 = _N_;
run;


data RanEff_Intc2;
   set RanEff_Intc;
   drop Alpha DF Effect Estimate Lower Probt StdErrPred Upper tValue;
   raneff_intc=Upper; output;
   raneff_intc=Lower; output;
   raneff_intc=Estimate; output;
run;


title1 "Random-Effects Prediction for the Intercept";
title2 "Class Efficacy on the Language Test Score - Rnd-Coeff model with IQ";
footnote justify=right "NLSCHOOLS ";

symbol interpol=hiloctj
       cv=black
       ci=blue
       width=1.25;

axis1 order=(1 to 133 by 2)
      offset=(3,3)
      label=("ID classes");
axis2 label=none
      offset=(2,2);


proc gplot data=RanEff_Intc2;
   plot raneff_intc*ID4 /  haxis=axis1 vaxis=axis2 vref=0 lvref=2;
run;
quit;
goptions reset=all;


/*High-Low plot per gli effetti casuali del coefficiente di regressione di IQ*/
proc sort data=RanEff_IQ;
by Estimate;
run;

data RanEff_IQ;
set RanEff_IQ;
ID5 = _N_;
run;


data RanEff_IQ2;
   set RanEff_IQ;
   drop Alpha DF Effect Estimate Lower Probt StdErrPred Upper tValue;
   raneff_coeff=Upper; output;
   raneff_coeff=Lower; output;
   raneff_coeff=Estimate; output;
run;


title1 "Random-Effects Prediction for the Regr.Coeff. of IQ";
title2 "Class Efficacy on the Language Test Score - Rnd-Coeff model with IQ";
footnote justify=right "NLSCHOOLS ";

symbol interpol=hiloctj
       cv=black
       ci=blue
       width=1.25;

axis1 order=(1 to 133 by 2)
      offset=(3,3)
      label=("ID classes");
axis2 label=none
      offset=(2,2);

proc gplot data=RanEff_IQ2;
   plot raneff_coeff*ID5 /  haxis=axis1 vaxis=axis2 vref=0 lvref=2;
run;
quit;
goptions reset=all;


 /*Random-coefficients model: IQ e SES random */
  title 'Random-coefficient model with IQ and SES';
  proc mixed data = nlschools cl covtest noitprint;
  class class;
  model lang = 	IQ SES / solution chisq cl;
  random intercept IQ SES/ subject = class type = un solution cl;  
  run;

/*N.B.: nel file log compare il messaggio: 'Estimated G matrix is not positive definite',
  ossia la matrice di var-cov degli effetti casuali non e' definitiva positiva.
  Si nota in particolare che e' la struttura casuale associata a SES a creare problemi. 
  Pertanto, questo modello non pare adeguato.
*/

  title 'Random-coefficients model, level-2 expl. var.';
 /*Random-coefficients model: IQ random, GS level-2 espl. */
  proc mixed data = nlschools cl covtest noitprint;
  class class;
  model lang = 	IQ SES GS/ solution chisq cl;
  random intercept IQ / subject = class type = un solution cl;  run;


  title 'Random-coefficients model, level-2 expl. var., cross-level int.';
 /*Cross-level interaction, GS level-2 espl. */
  proc mixed data = nlschools cl covtest noitprint;
  ods output SolutionR = RandomEffects5;
  class class;
  model lang = 	IQ SES GS IQ*GS / solution chisq cl;
  random intercept IQ / subject = class type = un solution cl;  run;

/*Dall'analisi svolta il modello piu' adeguato appare il random-coefficients model
  con intercetta e coeff. di regr. di IQ casuali e SES presente con un effetto fisso.*/

   /*------- SELECTED MODEL -----------------*/
  title 'Random-coefficient model with IQ';
  title2 'Selected Model';
  proc mixed data = nlschools cl covtest noitprint;
  /*  ods output SolutionR = RandomEffects4; */
  class class;
  model lang = 	IQ SES / solution chisq cl;
  random intercept IQ/ subject = class type = un solution cl;  run;

  /*Confronto fra la previsione degli effetti casuali ottenuta con l'empty model
  e quella ottenuta con il modello prescelto.*/

  proc sort data=RanEff;
  by class;
  run;

  proc sort data=RanEff_intc2;
  by class;
  run;

  data RanEff_comparison;
   merge RanEff RanEff_intc2;
   by class;
run;

/*High-Low Plots sovrapposti per gli effetti casuali dell'intercetta*/
/*Ordinamento in base ai risultati ottenuti con l'empty model ('by ID')*/
proc sort data=RanEff_comparison;
by ID;
run;


title1 "Random-Effects Prediction for the Intercept";
title2 "Comparison between the empty model and the random-coefficients model with random IQ";
title3 "Prediction intervals ordered according to the empty model results";
footnote justify=right "NLSCHOOLS ";

/*Definizione dei simboli per l'empty model*/
symbol1 interpol=hiloctj
       cv=black
       ci=blue
       width=2;

/*Definizione dei simboli per il random-coefficient model*/
symbol2 interpol=hiloctj
       cv=grayc0
       ci=red
       width=2.5;

axis1 order=(1 to 133 by 2)
      offset=(3,3)
      label=("ID classes");
axis2 label=none
      offset=(2,2);

legend1 label=none value=("Rnd-coeff model" "Empty model")
        position=(top left inside) frame across=2 offset=(,-1.5)
        mode=share;


proc gplot data=RanEff_comparison;
   plot raneff_intc*ID=2 raneff*ID=1 / overlay haxis=axis1 vaxis=axis2 vref=0 lvref=2 legend=legend1;
run;
quit;
goptions reset=all;



/*
SITO UTILE

https://stats.idre.ucla.edu/sas/


*/


