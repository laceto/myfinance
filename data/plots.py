import sys  
import os  
import pandas as pd  
from AT_funs import *

dgt = 5
bm_col = 'close'

filename_bm = 'data/FTSEMIB.MI.xlsx'
bm_df = read_xlsx(filename_bm)
bm_name = filename_bm.replace('xlsx', '')

exclude_files = {"FTSEMIB.MI.xlsx", "marginabili.xlsx", "sectors.xlsx", "output_signals.xlsx", "PTF.xlsx"}
include_files = {"A2A.MI.xlsx"}
files = [file for file in os.listdir('data') if file.endswith(".xlsx") and file not in exclude_files and file in include_files]  
print(len(files))

dfs = []
for file in files:
    file_path = os.path.join('data', file)
    df = pd.read_excel(file_path)
    if df.shape[0] >= 1000:
        dfs.append(df)
        
print('letti tutti i files')

def save_plot(df, ticker):
    close_price = df[['close','rclose']].plot(figsize=(20,8),grid=True, title= ticker +  ' Absolute vs relative to ' + bm_name + ' rebased' )
    plt.savefig('plots/' + ticker + '.png') 
    return df

for i in range(len(dfs)):
    print(i)
    ohlc = ['open','high','low','close']
    _o,_h,_l,_c = [ohlc[h] for h in range(len(ohlc))]

    ticker = dfs[i]['ticker'][0]
    try:
        dfs[i] = relative(dfs[i],_o,_h,_l,_c, bm_df, bm_col, dgt, rebase=True)
        dfs[i] = save_plot(dfs[i], ticker)


    except:
        failed.append(i)

print('for signal done')
