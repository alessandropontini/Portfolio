# %% markdown
# #   <span style="color:#5F9EA0">Progetto Python Alessandro Pontini Matricola: 852793</span>
# %% markdown
# ***
# %% codecell
# importo librerie
import pandas as pd
import datetime
import numpy as np
# %% codecell
# scrivo directory
dire = '/Users/alessandropontini/Documents/db/progetto_python/additional-kiva-snapshot/loans_lenders.csv'
dire1 = '/Users/alessandropontini/Documents/db/progetto_python/additional-kiva-snapshot/loans.csv'
dire2 = '/Users/alessandropontini/Documents/db/progetto_python/additional-kiva-snapshot/lenders.csv'
dire3 = '/Users/alessandropontini/Documents/db/progetto_python/additional-kiva-snapshot/country_stats.csv'
# %% codecell
# df temporanei

temp_df3 = pd.DataFrame()
temp_df4 = pd.DataFrame()
temp_df_NA_lenders = pd.DataFrame()
total_lenders_amount = pd.DataFrame()

# %% markdown
# ***
# %% markdown
# ##  <span style="color:red">1) Normalize the loan_lenders table. In the normalized table, each row must have one loan_id and one lender.</span>
# %% markdown
# ***
# %% codecell
# leggo il file csv
df_lenders_not_normalized = pd.read_csv(dire)
# %% codecell
df_lenders_not_normalized.head()
# %% codecell
# impongo index=loan_id per poter usare explode che per ogni loan_id mi prende ogni elemento della lista creata con la strip
# e mi crea un nuove righe in un nuovo data set
df_lenders_normalized = df_lenders_not_normalized.set_index('loan_id')['lenders'].str.split(pat=',').explode().reset_index()
# %% codecell
# mostro risultato
df_lenders_normalized.head()
# %% markdown
# ***
# %% markdown
# ##  <span style="color:red">2) For each loan, add a column duration corresponding to the number of days between the disburse time and the planned expiration time. If any of those two dates is missing, also the duration must be missing.</span>
# %% markdown
# ***
# %% codecell
# leggo il file loans, ma per la quantità di dati che devo leggere
# preferisco leggere le date come tali e non come stringhe.
df_loans = pd.read_csv(dire1, parse_dates=['disburse_time', 'planned_expiration_time'])
# %% codecell
# ci sono delle date che non hanno disburse_time > planned_expiration_time, non ha senso un prestito che si estingue
# prima della data di sborso, perciò controllo se ce ne sono e in effetti si ci sono.
# creo un df che li contiene df_temp
temp_df = df_loans.loc[df_loans['disburse_time'] > df_loans['planned_expiration_time']]
# %% codecell
x = temp_df.loc[:, ('planned_expiration_time')].copy()
temp_df.loc[:, ('disburse_time')] = temp_df.planned_expiration_time
temp_df.loc[:, ('planned_expiration_time')] = x
# %% codecell
# uso funzione isin per vedere i loan_id che coincidano e cambio df_loan con i nuovi di df_temp corretti
df_loans.loc[df_loans.loan_id.isin(temp_df.loan_id), ['disburse_time', 'planned_expiration_time']] = temp_df[['disburse_time', 'planned_expiration_time']]
# %% codecell
# creo una colonna per la duration e faccio differenza, dopo di che mostro il risultato
df_loans['duration'] = df_loans['planned_expiration_time'] - df_loans['disburse_time']
df_loans[['loan_id','duration']]
# %% markdown
# ***
# %% markdown
# ## <span style="color:red">3) Find the lenders that have funded at least twice.</span>
# %% markdown
# ***
# %% codecell
temp_df = df_loans[['loan_id', 'status']]
# %% codecell
temp_df = df_lenders_normalized.merge(temp_df, how='left', left_on='loan_id', right_on='loan_id')
# %% codecell
# creo lista temporanea in cui salvo tutti i loan_name con il value_counts che mi conta quante volte è presente
# e poi chiedo alla lista solo quelli >1
temp_df = temp_df.loc[temp_df.status == 'funded']
temp_df = temp_df.pivot_table(index=['lenders'], aggfunc='size').sort_values(ascending=False)
# %% codecell
temp_df[temp_df>1]
# %% markdown
# ***
# %% markdown
# ## <span style="color:red">4) For each country, compute how many loans have involved that country as borrowers.</span>
#
# %% markdown
# ***
# %% codecell
# suppongo che in loans che country_code identifichi le nazionalità delle persone perciò con una groupby posso raggrupparle
# e con una size so il numero presente per ogni nazione
df_loans.groupby('country_code').size()
# %% markdown
# ***
# %% markdown
# ## <span style="color:red"> 5) For each country, compute the overall amount of money borrowed.</span>
# %% markdown
# ***
# %% codecell
# raggruppo in base allo stato di appartenenza scegliendo di mostrare la colonna loan_amount e sommo i risultati.
df_loans.groupby('country_code')['loan_amount'].sum()
# %% markdown
# ***
# %% markdown
# ## <span style="color:red">6) Like the previous point, but expressed as a percentage of the overall amount lent.</span>
# %% markdown
# ***
# %% codecell
temp = df_loans.funded_amount.sum()
# %% codecell
df_loans.groupby('country_code')['loan_amount'].sum()/temp*100
# %% markdown
# ***
# %% markdown
# ## <span style="color:red">7) Like the three previous points, but split for each year (with respect to disburse time).</span>
# %% markdown
# ***
# %% codecell
temp_df = df_loans.set_index('disburse_time')
# %% codecell
# vedo quante persone all'anno
temp_df.groupby(['country_code', pd.Grouper(freq='Y')]).size()
# %% codecell
# faccio le somme
temp_df.groupby(['country_code', pd.Grouper(freq='Y')])['loan_amount'].sum()
# %% codecell
# eseguo operazioni punto 6
temp_df2 = df_loans['loan_amount'].sum()
# %% codecell
temp_df.groupby(['country_code', pd.Grouper(freq='Y')])['loan_amount'].sum()/temp_df2*100
# %% markdown
# ***
# %% markdown
# ## <span style="color:red">8) For each lender, compute the overall amount of money lent.</span>
# %% markdown
# ***
# %% codecell
# uso un df temporaneo per immagazzinare la join tra df_lenders_normalized e df_loans con solo i campi loan_id
# e loan_amount che usero per i conti successivamente
temp_df = df_lenders_normalized.merge(df_loans[['loan_id', 'loan_amount']], how='left', on='loan_id')
# %% codecell
# salvo in un df temp 2 il raggruppamento secondo count, per contare il numero totale di ogni loan id
temp_df2 = temp_df.groupby('loan_id')['lenders'].count()
# %% codecell
# faccio ultima merge per avere nel mio data set accanto alla colonna di loan id la colonna dei lenders_y
# che corrisponde al mio count
temp_df = temp_df.merge(temp_df2, how='left', on='loan_id')
# %% codecell
temp_df
# %% codecell
# creo nuova colonna per segnare le singole quote per ogni loan_id
temp_df['single_amount']= temp_df['loan_amount']/temp_df['lenders_y']
# %% codecell
# ora raggruppo per ogni lender
temp_df.groupby('lenders_x')['single_amount'].sum()
# %% markdown
# ***
# %% markdown
# ## <span style='color:red'> 9) For each country, compute the difference between the overall amount of money lent and the overall amount of money borrowed. Since the country of the lender is often unknown, you can assume that the true distribution among the countries is the same as the one computed from the rows where the country is known.</span>
# %% markdown
# ***
# %% markdown
# ## Problema di fondo che abbiamo trovato che il data set estremamente scarno con molti NULL, perciò dovevamo distribuire i soldi borrowed amolti valori NULL. Personalmento ho optato per la distribuzione del totale loan_amount dei null secondo percentuali di nazionalità
# %% markdown
# ***
# %% codecell
# inizio aggiungendo ai miei dataset anche le informazioni dei lenders_info
df_lenders_info = pd.read_csv(dire2)
# %% codecell
# merge in una temporanea di df_lenders con la normalizzata
# per avere loan_id
temp_df = df_lenders_info[['permanent_name', 'country_code']].merge(df_lenders_normalized, how='left', left_on='permanent_name', right_on='lenders')
# %% codecell
# temp_df2 contiene tutte le righe in cui lenders è presente in loan e non ha lenders == null
temp_df2 = temp_df.dropna(subset=['lenders'])
# %% codecell
# contiene tutti i borrowed
temp_df3 = temp_df.loc[temp_df.lenders.isna()]
# %% codecell
# analizzando il temp df 3 mi accorgo che ho qualche country code ma non ho nessun loan amount
# per tanto decido di non eseguire altri compiti
temp_df3
# %% codecell
temp_df2
# %% codecell
# aggiungiamo anche la colonna loan_amount per avere un totale dei lenders/loan/country ( potrebbero esserci dei NULL)
temp_df2=temp_df2.merge(df_loans[['loan_id', 'loan_amount']],how='left',on='loan_id')
# %% codecell
temp_df2.info()
# %% codecell
# metto i null a 0 di loan amount che sono 4
temp_df2['loan_amount'] = temp_df2['loan_amount'].fillna(0)
# %% codecell
# inizio controllando quelli con country valore che esiste, perdo quasi 3000000 righe
df_lenders_loans_country = temp_df2.dropna(subset=['country_code'])
# %% codecell
df_lenders_loans_country = df_lenders_loans_country.groupby('country_code')['loan_amount'].sum()
# %% codecell
total_lent = df_lenders_loans_country.sum()
# %% codecell
df_lenders_loans_country = df_lenders_loans_country.reset_index()
# %% codecell
# cosi facendo ho percentuale del totale per ogni country che è il punto che mi serve
df_lenders_loans_country['percentage'] = round(df_lenders_loans_country['loan_amount']/total_lent,10)
# %% codecell
df_lenders_loans_country
# %% codecell
temp_df2 = temp_df2.loc[temp_df2.country_code.isna()]
# %% codecell
temp_df2
# %% codecell
total_amount = temp_df2.loan_amount.sum()
# %% codecell
total_amount
# %% codecell
# li moltiplico per la percentuale dei country che conosco lenders e poi li sommo con i precedenti
x = total_amount * df_lenders_loans_country.percentage

df_lenders_loans_country['total_lenders_amount_with_no_country'] = x
# %% codecell
df_lenders_loans_country['total_lent'] = df_lenders_loans_country.loan_amount + df_lenders_loans_country.total_lenders_amount_with_no_country
# %% codecell
df_lenders_loans_country
# %% codecell
# no duplicates
a = df_loans[df_loans.duplicated()]
print(a)
# %% codecell
temp_df = df_loans.groupby('country_code')['loan_amount'].sum()
# %% codecell
temp_df = temp_df.reset_index()
# %% codecell
del df_lenders_loans_country['total_lenders_amount_with_no_country']
# %% codecell
df_lenders_loans_country = df_lenders_loans_country.merge(temp_df,how='left', on='country_code')
# %% codecell
df_lenders_loans_country.loan_amount_y = df_lenders_loans_country.loan_amount_y.fillna(0)
# %% codecell
df_lenders_loans_country['lent_borrowed'] = df_lenders_loans_country.total_lent - df_lenders_loans_country.loan_amount_y
# %% codecell
df_lenders_loans_country
# %% markdown
# ***
# %% markdown
# ## <span style='color:red'>10) Which country has the highest ratio between the difference computed at the previous point and the population?</span>
# %% markdown
# ***
# %% codecell
# importo data set che necessito
df_country_stat = pd.read_csv(dire3)
# %% codecell
temp_df = df_country_stat.groupby('country_code')['population'].sum()
# %% codecell
temp_df = temp_df.reset_index()
# %% codecell
df_lenders_loans_country = df_lenders_loans_country.merge(temp_df,how='left', on='country_code')
# %% codecell
# seh ho popolazione nan metto il primo quantile
first_quantile = df_lenders_loans_country.population.quantile(0.25)
df_lenders_loans_country.population = df_lenders_loans_country.population.fillna(first_quantile)
# %% codecell
df_lenders_loans_country
# %% codecell
#calcolo percentuale su i lent-borrowed e la popolazione, successivamente trovo l'idmax per la percentuale che mi dice essere CANADA
df_lenders_loans_country['percentage'] = round(df_lenders_loans_country['lent_borrowed']/df_lenders_loans_country['population']*100, 2)
#temp_df3=temp_df3.reset_index()
# %% codecell
max_ratio = df_lenders_loans_country.loc[df_lenders_loans_country['percentage'].idxmax()]
# %% codecell
max_ratio
# %% markdown
# ***
# %% markdown
# ## <span style='color:red'>11) Which country has the highest ratio between the difference computed at point 9 and the population that is not below the poverty line?</span>
# %% markdown
# ***
# %% codecell
temp_df2 = df_country_stat.groupby('country_code')[['country_code','population','population_below_poverty_line']].head()
# %% codecell
temp_df2['population_below_poverty_line'] = temp_df2['population_below_poverty_line']/100
# %% codecell
mean = temp_df2.population_below_poverty_line.mean()
temp_df2.population_below_poverty_line = temp_df2.population_below_poverty_line.fillna(mean)
# %% codecell
temp_df2
# %% codecell
temp_df2['population_below_poverty_line'] = temp_df2['population'] - (temp_df2['population_below_poverty_line']*temp_df2['population'])
# %% codecell
temp_df2 = temp_df2.merge(df_lenders_loans_country[['country_code', 'lent_borrowed']], left_on='country_code', right_on='country_code')
# %% codecell
mean = temp_df2.lent_borrowed.mean()
temp_df2.lent_borrowed = temp_df2.lent_borrowed.fillna(mean)
# %% codecell
temp_df2['percentage_poverty'] = round(temp_df2['lent_borrowed']/temp_df2['population_below_poverty_line']*100, 2)
# %% codecell
max_ratio = temp_df2.loc[temp_df2['percentage_poverty'].idxmax()]
# %% codecell
max_ratio
# %% markdown
# ***
# %% markdown
# ## <span style='color:red'>12) For each year, compute the total amount of loans. Each loan that has planned expiration time and disburse time in different years must have its amount distributed proportionally to the number of days in each year. For example, a loan with disburse time December 1st, 2016, planned expiration time January 30th 2018, and amount 5000USD has an amount of 5000USD * 31 / (31+365+30) = 363.85 for 2016, 5000USD * 365 / (31+365+30) = 4284.04 for 2017, and 5000USD * 30 / (31+365+30) = 352.11 for 2018.</span>
# %% markdown
# ***
# %% markdown
# ### Decido di eliminare tutte le colonne con NaN, in particolar modo tutte le colonne che non presentano disburse_time oppure planned_expiration_time
# %% markdown
# ***
# %% codecell
temp_df = df_loans.dropna()
# %% codecell
temp_df.loc[:,('disburse_time')]=temp_df.loc[:,('disburse_time')].dt.tz_localize(None)
temp_df.loc[:,('planned_expiration_time')]=temp_df.loc[:,('planned_expiration_time')].dt.tz_localize(None)
# %% codecell
temp_df['differenza'] = temp_df['planned_expiration_time'].dt.year - temp_df['disburse_time'].dt.year
# %% codecell
temp_df = temp_df[['planned_expiration_time', 'disburse_time', 'differenza', 'loan_amount', 'duration']]
# %% codecell
# mi accorgo che ho solo 1 o 0 come differenza perciò preparo programma apposta
temp_df.groupby('differenza').size()
# %% codecell
# tengo solo i giorni
temp_df.duration = temp_df.duration.dt.days
# %% codecell
def func(k):
        x = round(( k.loan_amount * k.duration)/(31+365+30),2)
        y = k['planned_expiration_time'].year
        return [y,x]

def func2(k):
        lim_inf = datetime.datetime(k.planned_expiration_time.year, 1,1)
        z = k.planned_expiration_time - lim_inf
        z = z.days
        x = round(( k.loan_amount * z)/(31+365+30),2)
        y = k.planned_expiration_time.year
        lim_sup = datetime.datetime(k.disburse_time.year, 12, 31)
        f = lim_sup - k.disburse_time
        f = f.days
        u = round(( k.loan_amount * f)/(31+365+30),2)
        v = k.disburse_time.year
        return [[y,x],[v,u]]

# %% codecell
temp_df1 = temp_df.loc[temp_df.differenza == 0]
listx = []
# %% codecell
# applico funzione per gli 0 anni di differenza
listx = temp_df1.apply(lambda x : func(x), axis=1)
# %% codecell
d1
# %% codecell
d1 = {'tot' : listx}
df2 = pd.DataFrame(d1)
df3 = df2.tot.apply(pd.Series)
df3.columns = ['year', 'total']
# %% codecell
df3.info()
# %% codecell
# applico funzione per gli 1 anni
temp_df1 = temp_df.loc[temp_df.differenza == 1]
listy = []
# %% codecell
listy = temp_df1.apply(lambda x : func2(x), axis=1)
# %% codecell
# ottengo due df di liste con i dati immagazzinati dal limite superiore e dal limite inferiore
d1 = {'tot' : listy}
df2 = pd.DataFrame(d1)
df4 = df2.tot.apply(pd.Series)
df4.columns = ['data1', 'data2']
df2 = df4['data1']
df4 = df4['data2']
# %% codecell
# primo limite
d1 = {'tot' : df2}
df2 = pd.DataFrame(d1)
df5 = df2.tot.apply(pd.Series)
df5.columns = ['year', 'total']
# %% codecell
# secondo limite
d1 = {'tot' : df4}
df2 = pd.DataFrame(d1)
df6 = df2.tot.apply(pd.Series)
df6.columns = ['year', 'total']
# %% codecell
# appendo entrambi i data set creati al primo creato
df3 = df3.append(df5, ignore_index=True)
# %% codecell
df3 = df3.append(df6, ignore_index=True)
# %% codecell
df3.year = df3.year.astype('int')
# %% codecell
df3.groupby('year').size()
# %% codecell
df3.groupby('year').sum()
# %% markdown
# # TEMPO IMPIEGATO 12 PUNTI : 5.30min
# # COMPUTER UTILIZZATO : MACBOOK PRO 19
# # PROCESSORE : 1,4 GHz Intel Core i5 quad-core
# # MEMORIA : 8 GB 2133 MHz LPDDR3
# # SCHEDA GRAFICA : Intel Iris Plus Graphics 645 1536 MB
# 
