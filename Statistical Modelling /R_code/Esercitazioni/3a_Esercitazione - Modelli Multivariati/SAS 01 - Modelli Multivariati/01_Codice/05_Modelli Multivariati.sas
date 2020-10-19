data companies;
set companies;
KEEP COMPANY assets sales mark_val profits cash employ sector;
run;
/*********************************************************************/
/******************** MODELLO LINEARE MULTIVARIATO *******************/
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

LIBNAME User 'C:\Users\daniele.riggi\Desktop\Esame Vitta - Statistical Modeling\esercitazioni\3a_Esercitazione\SAS 01 - Modelli Multivariati\02_Dati'; run;

/* MORE DETAILS TO THE FOLLOWING PAGES
https://stats.idre.ucla.edu/sas/dae/multivariate-regression-analysis/
*/

/*APRIRE LA VISUALIZZAZIONE DELL'OUTPUT IN HTML*/



/*********************************************************************/
/******************** MODELLO LINEARE MULTIVARIATO *******************/
/*********************************************************************/


/* Procedura di SAS: PROC GLM

Sintassi essenziale:

PROC GLM < options > ; 
MODEL dependents=independents < / options > ; 
MANOVA < test-options >< / detail-options > ; 
OUTPUT < OUT=SAS-data-set > 
      keyword=names < ... keyword=names > < / option > ; 
*/


/*----------  Matrice di dati 'companies' -----*/ 

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

/*Riattribuzione delle etichette alle modalita' di macrosector*/



proc format;  
      value macroname 
         1='sett. terziario' 
         2='sett. servizi' 
         3='sett. commercio' 
         4='sett. finanza'
         5='altri settori'; 
run;


title 'Dataset: companies';
proc print data=companies(obs=5) label;
id company;
run;


/*Assumiamo che le variabili 'assets', 'mark_val' e 'profits' costituiscano l'insieme 
  delle variabili dipendenti oggetto di interesse */

/*Correlazioni fra le variabili dipendenti*/
  title 'Matrice di correlazione per le var. dipendenti';
   proc corr data=companies;
   var assets mark_val profits;
   run;

   /*Correlazioni fra le variabili dipendenti e le variabili esplicative*/
  title 'Matrice di correlazione delle var. dip. con le var. esplicative';
   proc corr data=companies;
   var assets mark_val profits;
   with sales cash employ dum_terz dum_serv dum_comm dum_other dum_fin;
   run;

   /*Correlazioni fra le variabili esplicative*/
  title 'Matrice di correlazione per le var. esplicative';
   proc corr data=companies;
   var sales cash employ dum_terz dum_serv dum_comm dum_other dum_fin;
   run;


/* Costruzione di modelli di regressione lineare separati,
   uno per ciascuna variabile dipendente */

title 'Modelli di regressione multipla univariati';
  proc reg data=companies;
      mod_mark_val: model mark_val=sales cash employ / stb clb spec dwprob;
	  mod_assets: model assets=sales cash employ / stb clb spec dwprob;
  	  mod_profits: model profits=sales cash employ / stb clb spec dwprob;
	run;
quit;


/* Costruzione di modelli di regressione lineare separati
   e introduzione delle variabili dummy. 
   Si considera quale categoria di riferimento il settore finanziario.
   Pertanto, la variabile dummy ad essa relativa non verra' inserita nei modelli.*/

title 'Modelli di regressione multipla univariati con var. dummies';
  proc reg data=companies;
      mod_mark_val: model mark_val=sales cash employ dum_terz dum_serv dum_comm dum_other/ stb clb;
	  mod_assets: model assets=sales cash employ dum_terz dum_serv dum_comm dum_other / stb clb;
  	  mod_profits: model profits=sales cash employ dum_terz dum_serv dum_comm dum_other / stb clb;
	run;
quit;

/*In realta', procedendo in questo modo, non si tiene conto del fatto 
	che le variabili dipendenti sono fra loro correlate*/

/*Costruzione del modello lineare multivariato*/
/*Senza le variabili dummies*/
title 'Modello lineare multivariato';
PROC GLM DATA=companies;
MODEL assets mark_val profits = sales cash employ ;
/* Statement MANOVA: per condurre l'analisi multivariata della varianza rispetto alle variabili
   specificate in: 'H=', o in altri termini per sottoporre a verifica l'ipotesi di nullita' congiunta
   dei parametri associati alle variabili esplicative indicate in 'H='.
   Opzioni:
   'printe' = per visualizzare la matrice E delle somme dei quadrati dei residui del modello
              (SSCP - Sum of Squares Cross-Products). Inoltre, vengono visualizzati i coefficienti
              di correlazione parziali delle variabili dipendenti corretti per le variabili indipendenti.
   'printh' = per visualizzare la matrice H delle somme dei quadrati associata all'ipotesi sottoposta
              a verifica.
*/
MANOVA H=sales cash employ/PRINTE PRINTH;
MANOVA H=_ALL_/PRINTE PRINTH;
/*To produce tests for all effects listed in the MODEL statement, 
  use the keyword _ALL_ in place of a list of effects. */
RUN;
quit;


/*
- Characteristic Root – 
	These are the eigenvalues of the product of the sum-of-squares matrix of the model and the sum-of-squares matrix of the error. 
	There is one eigenvalue for each of the eigenvectors of the product of the model sum of squares matrix and the error sum of squares matrix, a 3×3 matrix. 
	The percents listed next to the characteristic roots indicate the amount of variability in the outcomes a given root and vector account for. 
	In this example, the first root and vector account for 100% of the variability in the outcomes and the second for .58% of the variability in the outcomes.

- Characteristic Vector – 
	These are the eigenvectors of the product of the sum-of-squares matrix of the model and the sum-of-squares matrix of the error.
	The three numbers that compose a vector can be read across a row (one under useful, one under difficulty, and one under importance).

- S=1 M=0.5 N=28 – 
	These are intermediate results that are used in computing the multivariate test statistics and their associated degrees of freedom. 
	If P is the number of dependent variables, Q is the hypothesis degrees of freedom, and NE is the residual or error degrees of freedom, 
	then S = min(P, Q), M = .5(abs(P-Q)-1) and N = .5(NE-P-1).

- Statistic – 
	MANOVA calculates four multivariate test statistics. All four are based on the characteristic roots . 
	The null hypothesis for each of these tests is the same: the independent variable (group) has no effect on any of the dependent variables 
	(assets mark_val profits ).

- Wilks’ Lambda – 
	This can be interpreted as the proportion of the variance in the outcomes that is not explained by an effect.  To calculate Wilks’ Lambda, 
	for each characteristic root,calculate 1/(1 + the characteristic root), then find the product of these ratios.  So in this example, 
	you would first calculate 
	1/(1+0.17533473) = 0.85082145, 
	1/(1+4.40471907) = 0.18502349, 
	1/(1+0.11565910) = 0.89633115
Then multiply 0.85082145 * 0.18502349 * 0.89633115 = 0.141102203

- Pillai’s Trace – 
	This is another one of the four multivariate test statistics used in MANOVA.  To calculate Pillai’s trace, divide each characteristic root 
	by 1 + the characteristic root, 
	then sum these ratios.  So in this example, you would first calculate 
	0.17533473/(1+0.17533473) = 0.14917855
	4.40471907/(1+4.40471907) = 0.81497651, 
	0.11565910/(1+0.11565910) = 0.10366885 
When these are added we arrive at Pillai’s trace: (0.14917855 +  0.81497651 + 0.10366885) = 1.06782391


- Hotelling-Lawley Trace – 
	This is very similar to Pillai’s Trace. It is the sum of the roots of the product of the sum-of-squares matrix of the model and the 
	sum-of-squares matrix of the error for the two linear regression functions and is a direct generalization of the F statistic in ANOVA.  
	We can calculate the Hotelling-Lawley Trace by summing
	the characteristic roots listed in the output: 0.17533473 + 4.40471907 + 0.11565910 = 4.6957129

- Roy’s Greatest Root – 
	This is the largest of the roots of the product of the sum-of-squares matrix of the model and the sum-of-squares matrix of the error 
	for the two linear regression functions. We can see that the value of Roy’s Greatest Root is the largest of the characteristic roots 
	(see superscript q). Because it is a maximum, it can behave differently from the other three test statistics.  
	In instances where the other three are not significant and Roy’s is significant, the effect should be considered non-significant. 
	For further information on the calculations underlying MANOVA results, consult SAS online documentation .

- F Value – 
	This is the F statistic for the given predictor and test statistic.

- Num DF –  
	This is the number of degrees of freedom in the model.

- Den DF – 
	This is the number of degrees of freedom associated with the model errors.  Note that there are instances in MANOVA when the degrees of freedom 
	may be a non-integer (here, the DF associated with Hotelling-Lawley Trace is a non-integer) because these degrees of freedom are calculated using the 
	mean squared errors, which are often non-integers.

- Pr > F – 
	This is the p-value associated with the F statistic of a given effect and test statistic.  The null hypothesis that a given predictor has no effect on 
	either of the outcomes is evaluated with regard to this p-value. For a given alpha level, if the p-value is less than alpha, the null hypothesis is rejected. 
	If not, then we fail to reject the null hypothesis.  In this example,  
		- we reject the null hypothesis that sales has no effect on assets mark_val profits  at alpha level .05 because the p-values are all less than .05.
		- we reject the null hypothesis that cash  has no effect on assets mark_val profits  at alpha level .05 because the p-values are all less than .05.
		- we cannot reject the null hypothesis that employ has no effect on assets mark_val profits  at alpha level .05 because the p-values are greater than .05.

*/


/*Con le variabili dummies, settore finanziario quale categoria di riferimento*/
title 'Modello lineare multivariato con var. dummies';
PROC GLM DATA=companies;
MODEL assets mark_val profits = sales cash employ dum_terz dum_serv dum_comm dum_other;
MANOVA H=_ALL_ / PRINTE PRINTH MSTAT=EXACT;
RUN;
quit;

/*Test MANOVA esatti - opzione MSTAT=EXACT*/
/*
MSTAT=FAPPROX | EXACT 
specifies the method of evaluating the multivariate test statistics. 
The default is MSTAT=FAPPROX, which specifies that the multivariate tests are evaluated 
using the usual approximations based on the F distribution, as discussed in the section "Multivariate Tests" 
in Chapter 4, Introduction to Regression Procedures. 
Alternatively, you can specify MSTAT=EXACT to compute exact p-values for three of the four tests 
(Wilks’ lambda, the Hotelling-Lawley trace, and Roy’s greatest root) and an improved F approximation 
for the fourth (Pillai’s trace). While MSTAT=EXACT provides better control of the significance probability 
for the tests, especially for Roy’s greatest root, computations for the exact p-values can be appreciably 
more demanding, and are in fact infeasible for large problems (many dependent variables). 
Thus, although MSTAT=EXACT is more accurate for most data, it is not the default method. 
For more information about the results of MSTAT=EXACT, see the section Multivariate Analysis of Variance. 
*/


/*PROCEDURA ALTERNATIVA PER LA COSTRUZIONE DEL MODELLO MULTIVARIATO - PROC REG*/
/*RISPETTO ALLA PROC GLM POSSONO ESSERE COSTRUITI ULTERIORI TEST PER LA VERIFICA DI IPOTESI
  SUI COEFFICIENTI DI REGRESSIONE.*/
title 'REG Modelli di regressione multivariato';
  proc reg data=companies;
     mv_mod: model assets mark_val profits = sales cash employ / stb clb;
	 sales: mtest sales / print; /*stesso risultato di proc GLM*/
	  /*The MTEST statement is used to test hypotheses in multivariate regression models 
	    where there are several dependent variables fit to the same regressors. 
	    If no equations or options are specified, the MTEST statement tests the hypothesis 
	    that all estimated parameters except the intercept are zero. 
	  */
	run;

	  cash: mtest cash / print; /*stesso risultato di proc GLM*/
    run;

      employ: mtest employ / print; /*stesso risultato di proc GLM*/
    run;

	/*verifica di ipotesi simultanea sulla nullita' congiunta dei coefficienti delle tre variabili esplicative*/
	  all_var_expl: mtest sales, cash, employ / print; /*non fattibile con PROC GLM*/
    run;
    
    /*verifica di ipotesi simultanea sulla nullita' congiunta dei coefficienti di sales e cash*/
	  two_var_expl: mtest sales, cash / print; /*non fattibile con PROC GLM*/
    run;

	/*verifica di ipotesi sull'uguaglianza di un coefficiente ad un valore prefissato in tutte le equazioni.
	  Ad esempio: H0: beta_cash = 3
	*/
	 cash_val: mtest cash=3 / print; /*non fattibile con PROC GLM*/
    run;

    /*verifica di ipotesi sull'uguaglianza fra due coefficienti in tutte le equazioni. Ad esempio:
      H0: beta_sales = beta_cash -> H0: beta_sales - beta_cash = 0
	*/
	 sales_eq_cash: mtest sales - cash=0 / print; /*non fattibile con PROC GLM*/
    run;
quit;


/********************************************************************************************************
  ESERCIZIO: UTILIZZANDO IL MODELLO CHE CONTIENE ANCHE LE VARIABILI DUMMY, PROPORRE UN INSIEME DI ANALISI
  CON PROC REG PER LA VERIFICA DI IPOTESI SUI COEFFICIENTI DI REGRESSIONE FISSATE A PIACERE.
**********************************************************************************************************/

