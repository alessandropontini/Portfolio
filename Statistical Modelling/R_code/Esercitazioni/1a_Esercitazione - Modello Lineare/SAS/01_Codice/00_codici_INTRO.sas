/*********************************************************************/
/*-------------- BREVE INTRODUZIONE AL SISTEMA SAS ------------------*/
/*********************************************************************/
/*SAS: Statistical Analysis System*/

/***********  SAS 9.4 **********/

/*
++ Testo di riferimento per il software SAS: Manuale SAS/STAT:
http://support.sas.com/documentation/onlinedoc/stat/index.html#stat121
http://support.sas.com/documentation/cdl/en/statug/66103/HTML/default/viewer.htm#titlepage.htm
http://support.sas.com/documentation/cdl/en/statug/66103/PDF/default/statug.pdf

++ Altri manuali sono disponibili alla pagina:
http://support.sas.com/documentation/94/index.html
*/

/********* PER LE ALTRE VERSIONI DI SAS ******************/

/***********  SAS 9.3 **********/

/*
 Testo di riferimento per il software SAS: Manuale SAS/STAT vs. 9.3
http://support.sas.com/documentation/onlinedoc/stat/index.html#stat93

Altri manuali sono disponibili alla pagina:
http://support.sas.com/documentation/93/index.html
*/

/*** WHAT'S NEW IN SAS 9.3:
In SAS 9.3, output in the SAS windowing environment is created by default in HTML. 
In addition, ODS Graphics is enabled by default.
***/

/***********  SAS 9.2 **********/
/*
 Testo di riferimento per il software SAS: Manuale SAS/STAT
http://support.sas.com/documentation/cdl/en/statug/63033/PDF/default/statug.pdf

Altri manuali sono disponibili alla pagina:
http://support.sas.com/documentation/onlinedoc/bookshelf/92/
*/


/*********************************************************************/
/*E' necessario definire la LIBRERIA di lavoro,
ossia la cartella che conterrà i datasets.*/


/* LIBRERIA (o DIRECTORY) DI LAVORO: */

/*
SAS memorizza i dati secondo due modalità: temporanea e permanente.
I dati temporanei sono i dati creati durante una sessione di lavoro, che non essendo
espressamente salvati dall'utente vengono persi alla chiusura della sessione stessa. 
Di default SAS li memorizza 'temporaneamente' nella libreria WORK, ma alla chiusura del
programma questi dati vengono cancellati.
Per creare dati permanenti, ossia dati che rimarranno archiviati alla chiusura del
programma, è necessario che l'utente crei una libreria di lavoro.
*/

/*PER CREARE UNA LIBRERIA DI LAVORO SI MANDA IN ESECUZIONE LA SINTASSI:*/

/*Modificare inserendo il percorso opportuno:*/

LIBNAME User 'working-directory'; run;


/*Ad esempio:*/

LIBNAME User 'C:\IntroSAS'; run;


/*'User' è il nome dato alla libreria di lavoro.*/


/* PRINCIPALI FORMATI PER LA VISUALIZZAZIONE DELL'OUTPUT:
1) formato 'listato' (formato testo);
2) formato 'html';
3) formato 'rtf' (formato di Word).

Il formato 'html' è il formato di default della versione 9.3;
il formato 'listato' è il formato di default delle precedenti versioni di SAS.
Per attivare il formato in html e sopprimere il formato listato nelle precedenti
versioni di SAS:
*/

ods listing close;
ods html;

/*Mandando in esecuzione le istruzioni di cui sopra compare
nel file log l'avviso:

NOTA: Scrittura body file HTML: sashtml.htm

Ciò significa che SAS crea automaticamente un file nominato
'sashtml.htm' in una directory dell'hard disk predefinita
durante l'installazione del programma.
*/

/* N.B.: Se non si hanno pieni diritti di accesso su tutto l'hard disk
(come nel caso dei PC dei laboratori) è necessario utilizzare 
una procedura alternativa per visualizzare l'output nel formato
desiderato. Si tratta di attivare dalla barra degli strumenti superiore
con il percorso:

Strumenti => Opzioni => Preferenze

una finestra di dialogo. 
Si seleziona quindi la parte "Risultati". 
In essa è già attivata l'opzione "Crea listato" 
(come detto, è l'opzione di default, che risulta
pertanto già selezionata).
Per richiedere l'attivazione del formato 'html'
basta selezionare l'opzione: 'Crea HTML'.
Quindi, se si attiva anche l'opzione: 
'Utilizza la cartella WORK', è necessario disattivarla
(altrimenti il file html andrebbe perduto alla chiusura
del programma, poiché sarebbe trattato come file temporaneo).
Disattivando l'opzione di cui sopra, si attiva la parte
a fianco a 'Cartella'. In essa si inserirà
il percorso che porta alla directory di lavoro
definita con il comando 'LIBNAME'.

In ultimo, per avere esclusivamente la visualizzazione
dell'output in html, conviene deselezionare l'opzione
'Crea listato'.
*/

/*----------------DUE PASSI PRINCIPALI DELLA PROGRAMMAZIONE IN SAS -----------------|
| 1) DATA STEP: è la fase vera e propria di inserimento dei dati, di gestione       |
|               e di eventuale trasformazione dei dati stessi                       |
|               (ad es., la costruzione di nuove variabili).                        |
| 2) PROC STEP: è la fase successiva di elaborazione e di analisi dei dati.         |
|----------------------------------------------------------------------------------*/


/*------------------INSERIMENTO DEI DATI IN SAS -----------------|
| Definizione del DATA STEP, che costituisce il primo passo      |
| fondamentale per eseguire le analisi in SAS.                   |
|---------------------------------------------------------------*/


/*----------------------------------------------------------------*/
/* 1. E' possibile scrivere i dati direttamente nel Program Editor*/
/*----------------------------------------------------------------*/

/*Struttura principale della SINTASSI per definire un DATA STEP*/

/*
data 'nome-matrice-di-dati';
input 'nome+tipologia-variabili';
datalines; (oppure il comando 'cards;')
'scrivere i records con i dati'
;
*/

/*
ESEMPIO 1: Inseriamo i dati della matrice 'Test', le cui informazioni
sono tutte numeriche.
*/

data test;	/*nome della matrice di dati*/
input id x y; /*nome delle variabili; la tipologia del dato è numerica, quindi non dobbiamo specificare altro!*/
cards; 
1 3 8
2 6 2
3 7 4
4 4 3
5 9 3
;
run;

/*
I dati sono stati caricati nella libreria di lavoro 'User'.
E' possibile vedere i dati nel VIEWTABLE (cliccando sopra il nome del
file di SAS 'Test.sas').
Nella finestra di output non si vede ancora niente.
Per visualizzare i dati, si può chiedere al SAS una stampa,
con la procedura 'PROC PRINT'. Questo è un esempio del secondo
passo fondamentale in SAS, il PROC STEP. In questa fase
è possibile eseguire tutte le analisi di interesse.
*/

title 'Esempio1: matrice test'; /*Con 'title' si inserisce un titolo nell'output*/
proc print data=test;
run; 

/*Per stampare il contenuto informativo della matrice 'test'
si può usare 'PROC CONTENTS'*/

proc contents data=test;
run;

/*
ESEMPIO 2: Inseriamo le informazioni della matrice di dati 'individui'*/
/*Ora dobbiamo specificare l'inserimento di due colonne (NOME e SESSO)
che sono date in formato stringa. Usiamo quindi il comando '$' dopo il nome
della variabile.
Per indicare la presenza di un dato mancante, inseriamo un punto.
*/
data individui;
input NOME $ SESSO $ ETA ALTEZZA PESO;
datalines;
Andrea		M	25	185	73
Roberto		M	31	167	80
Cristina	F	24	163	60
Paola		F	25	155	45
Giovanni	M	27	172	75
Sara		F	20	170	60
Mario		M	19	182	85
Annalisa	F	22	160	49
Fabio		M	22	175	65
Luca		M	21	184	73
Marco		M	26	173	79
Davide		M	27	190	90
Susanna		F	.	159	55
Claudia		F	25	162	52
Laura		F	21	163	55
Elisabetta	F	18	172	60
Massimo		M	29	170	60
Dario		M	20	181	92
;
run;

title 'Esempio2: Matrice di dati individui'; 
proc contents data=individui;
run;

proc print data=individui;
run;

/*E' possibile stampare anche solo un sottoinsieme delle colonne*/

proc print data=individui;
var NOME SESSO ETA;
run;

/*
Un altro esempio di procedura di analisi è: PROC FREQ,
mediante la quale si possono costruire le distribuzioni di frequenze.
*/

/*SINTASSI ESSENZIALE di PROC FREQ*/
/*
PROC FREQ < options > ; 
OUTPUT < OUT=SAS-data-set > options ; 
TABLES requests < / options > ; NOPERCENT
WEIGHT variable < / option > ; 
*/

/*---------------------------------------------------------|
|N.B.: PER CONOSCERE LA SINTASSI DELLE 'PROC' IMPLEMENTATE |
|IN SAS SI PUO' ATTIVARE L'HELP ON-LINE. Dalla barra degli |
| strumenti superiore si segue il percorso:                |
| ? => Guida e documentazione SAS                          |
|---------------------------------------------------------*/

/* Esempio: distribuzione di frequenze di 'SESSO' */

proc freq data=individui;
tables SESSO;
run;

/* Esempio: distribuzione di frequenze di 'SESSO' senza percentuali. */
proc freq data=individui;
tables SESSO / nopercent;
run;

/* Esempio: distribuzione di frequenze di 'SESSO' e 'ETA' */
proc freq data=individui;
tables SESSO ETA;
run;


/* Esempio: distribuzione bivariata di 'SESSO' e 'ETA'
(ossia, costruzione di una tabella a doppia entrata)*/
proc freq data=individui;
tables SESSO*ETA;
run;

proc freq data=individui;
tables SESSO*ETA / nopercent;
run;
title; /*per eliminare da qui in poi dall'output il titolo precedentemente specificato*/

/*
ESEMPIO 3: Inserimento di dati tabellari, ossia di dati con struttura aggregata
entro modalità o combinazioni di modalità delle variabili.
Non tutti i dati sono organizzati necessariamente in una matrice di dati.
E' possibile scrivere nell'Editor anche i dati organizzati in tabelle
a entrata multipla. Nelle celle della tabella non sono riportati i
valori di variabili, ma le frequenze congiunte.
Per l'inserimento in SAS è necessario scrivere la struttura tabellare
riportando tutte le combinazioni di modalità che si ottengono incrociando
le variabili e aggiungendo in una colonna a parte le frequenze congiunte
che corrispondono alle varie combinazioni di modalità.
*/
data cas_con;    
input stato $ espos $ count; /*nome delle 2 variabili + colonna per le frequenze congiunte 'count'*/    
cards;    
control es 16
control ne 48 
caso 	es 40 
caso 	ne 20
   ; 
run;


/*
La PROC PRINT però non consente di visualizzare i risultati
nel formato tabellare.
*/
title 'Esempio3: Tabella a doppia entrata';
proc print data=cas_con;
run;

/*
Piuttosto dobbiamo usare 'PROC FREQ' per ricostruire
la struttura tabellare.
*/

proc freq data=cas_con;
   tables stato*espos; /*richiediamo la costruzione di una tabella a doppia entrata*/
   weight count;	/*indichiamo che entro ciascuna cella vi saranno le freq. congiunte*/
run;


/* 
ALTRO METODO PER SCRIVERI I DATI: Nell'impostazione data fino a qui
il SAS riconosceva automaticamente i records poiché li abbiamo scritti
su righe distinte. E' possibile però scrivere più records di seguito.
Basta infatti inserire in 'input', dopo il nome di tutte le colonne,
il comando '@@'. SAS interpreta infatti questo comando come la fine
di un record.
*/

/*ESEMPIO 4: Inseriamo le informazioni della matrice di dati 'individui',
ma senza scrivere i records su righe separate*/

data individui2;
input NOME $ SESSO $ ETA ALTEZZA PESO @@;
datalines;
Andrea		M	25	185	73 Roberto		M	31	167	80 Cristina		F	24	163	60
Paola		F	25	155	45 Giovanni		M	27	172	75 Sara			F	20	170	60
Mario		M	19	182	85 Annalisa		F	22	160	49 Fabio		M	22	175	65
Luca		M	21	184	73 Marco		M	26	173	79 Davide		M	27	190	90
Susanna		F	.	159	55 Claudia		F	25	162	52 Laura		F	21	163	55
Elisabetta	F	18	172	60 Massimo		M	29	170	60 Dario		M	20	181	92
;
run;

title 'Esempio4: Matrice di dati individui2';
proc print; 
run;

/*Abbiamo omesso 'data=individui2'. Infatti, SAS mantiene in memoria l'ultimo
dataset caricato. Quindi, se la PROC che si usa è relativa proprio a quell'ultimo
dataset, si può omettere il nome dei dati.*/

/*Notiamo inoltre che 'individui2' è esattamente uguale a 'individui'.*/


/*---------------------------------------------------*/
/* 2. E' possibile caricare i dati da un file esterno*/
/*---------------------------------------------------*/

/*
Possiamo distinguere due procedure:
A. Caricare i dati usando la sintassi;
B. Caricare i dati con la procedura 'Import Wizard'.
*/

/*------------------- Procedura A -----------------------*/

/*
Dobbiamo usare due comandi: 
FILENAME : indichiamo a SAS il nome da dare al dataset che
vogliamo importare e il percorso dove il file di dati è collocato;
INFILE : nel DATA STEP informiamo il SAS che i dati da usare
sono quelli che provengono da un file esterno.
*/
/*Sintassi di FILENAME*/
/*FILENAME nome-da-assegnare 'percorso della cartella che contiene il file'*/

/*ESEMPIO 5: inseriamo il file di dati esterno 'test.txt'*/

filename testin 'percorso-working-directory\nome-file'; run;


/*Ad esempio:*/

filename testin 'C:\IntroSAS\test.txt'; run;


/*In questo modo il SAS ha collegato il nome 'testin' al file esterno 'test.txt'.
L'importazione non è ancora ultimata. Ora si deve specificare la vera e propria
importazione nel DATA STEP inserendo la dichiarazione INFILE*/

data test2; /*creiamo un dataset di SAS che si chiama test2*/
infile testin; /*specifichiamo che i dati si trovano nel file collegato con il nome 'testin'*/
input id x y; /*nome delle variabili*/
run; 

title 'Esempio5: importazione test';
proc print data=test2;
run;

/*ESEMPIO 6: importiamo i file di dati esterni 'auto.xls' e 'citta.xls' con la procedura
'Import Wizard': File => Import Data... => 'seguire le istruzioni'*/

/*******************************************************************************************************************
AVVERTENZA: Dalla versione 9.4, il SAS distingue fra file excel creati con Microsoft Office a 32-bit oppure a 64-bit:

http://support.sas.com/documentation/cdl/en/acpcref/67382/HTML/default/viewer.htm#n0yvsbyzbrjmstn1gy49c4kjrdlp.htm

Nel caso specifico di Microsoft Office a 32-bit installato sul sistema operativo Windows a 64-bit,
l'opzione di default "Microsoft Excel Workbook"(*.xls,...) dell'Import Wizard non funziona.
Pertanto, per importare un file excel è necessario utilizzare il prodotto di SAS "SAS PC Files Server".
Tale prodotto deve essere installato sul proprio PC insieme all'installazione base di SAS.
Il file excel deve essere dunque importato con l'opzione "Microsoft Excel Workbook on PC Files Server"
attivabile dalla prima finestra di dialogo dell'Import Wizard sotto a "Select a data source from the list below"
(è la terza scelta). Quindi, si clicca su "Next".
Nella finestra di dialogo successiva si deve indicare in corrispondenza di Workbook il nome del file con il relativo
percorso. Lo si può inserire cliccando su Browse e cercando il file con il suo percorso.
Esempio: C:\IntroSAS\auto.xls
Nella parte sotto a "PC Files Server" si deve lasciare tutto invariato.
Si clicca su OK, e si prosegue nell'importazione.
********************************************************************************************************************/

title 'Esempio6: Matrice di dati auto';
proc print data=auto; 
run;

title 'Esempio6: Matrice di dati città';
proc print data=citta; 
run;


/*-------------OPERAZIONI SULLE VARIABILI (COLONNE DELLA MATRICE DI DATI)----------*/

/* Possiamo eseguire tutte le operazioni matematiche di interesse 
direttamente nel DATA STEP, cioè mentre stiamo caricando i dati;
oppure, se abbiamo già caricato i dati, possiamo creare un nuovo dataset
che conterrà le variabili nuove, frutto di trasformazioni applicate su
variabili di origine, oppure possiamo riscrivere sul dataset stesso.
*/

/*STRUTTURA DELLA SINTASSI:
data newdata;
set mydata;
< altre istruzioni >
run;
*/

/*ESEMPIO 7: Da 'auto' costruiamo la variabile 'prezzo espresso in euro' */
title 'Matrice auto: operazioni';
data auto2; /*nuovo dataset*/
set auto; /*richiamiamo il dataset di origine*/
pr_euro = prezzo / 1927.36; 
/*
Operazioni aritmetiche: + (addizione), - (sottrazione), * (moltiplicazione), / (divisione)
*/
run;

/*Calcoliamo le medie delle variabili quantitative con la PROC MEANS*/

/*SINTASSI ESSENZIALE di PROC MEANS*/
/*
PROC MEANS <option(s)> <statistic-keyword(s)>;  
 VAR variable(s) < / WEIGHT=weight-variable>;  
 WEIGHT variable;  
*/

/*Statistiche descrittive*/
proc means data=auto2;
var lungh bag peso cavalli serb	pr_euro;
run;

/*SELEZIONE DI UNITA' STATISTICHE*/
/*Consideriamo le unità che soddisfano solo alcune condizioni*/


/*ESEMPIO 8: Da 'auto2' consideriamo solo i modelli di auto che costano più
 di 15.000 euro*/
title 'Auto: Selezione di unità';

data auto3; /*nuovo dataset*/
set auto2; 
/*selezioniamo le auto con pr_euro <= 15.000, che poi cancelleremo*/
if pr_euro <= 15000 then delete; 
/*
Operatori relazionali: > (maggiore), < (minore), >= (maggiore o uguale), 
    <= (minore o uguale), = (uguale), ^= (diverso) 
*/
run;

proc print data=auto3;
var modello pr_euro;
run;

/*oppure:*/
proc print data=auto3;
var pr_euro;
id modello;
run;


/*Possiamo procedere anche ad operazioni di ricodifica.*/

/*ESEMPIO 9: Da 'auto2' consideriamo la ricodifica della variabile 'vend',
che ha modalità A="Alta richiesta", B="Richiesta media" e C="bassa richiesta",
con i codici numerici: A=1, B=2, C=3.
*/ 

title 'Auto: ricodifica';
data auto3; /*nuovo dataset*/
set auto2; 
if vend = 'A' then vend_ric=1; 
if vend = 'B' then vend_ric=2;
if vend = 'C' then vend_ric=3;
run;


proc print data=auto3;
var vend vend_ric;
id modello;
run;


/*
N.B.: Alla chiusura della sessione di lavoro di SAS i file di dati
creati man mano rimangono nella cartella di lavoro 'User'. 
Ciò perché la creazione di una libreria di lavoro 
specificata dall'utente comporta implicitamente che i file di dati
creati siano a tutti gli effetti dei file definitivi
(mentre lavorare nella libreria di default di SAS 'Work' comporta
che i file siano considerati come file temporanei, così che non
vengano 'trattenuti' sul disco fisso alla chiusura del programma).

I file di dati di SAS hanno estensione '.sas7bdat'.
Per caricare questi file di dati in una prossima sessione di lavoro 
basterà copiarli in una nuova cartella di lavoro, e poi caricare la cartella stessa
con il comando 'LIBNAME' (come fatto all'inizio dei questa sessione di lavoro).
*/

