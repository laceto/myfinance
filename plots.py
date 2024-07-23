import sys  
import os  
import pandas as pd  


### RELATIVE
def relative(df,_o,_h,_l,_c, bm_df, bm_col, dgt,rebase=True):
    '''
    df: df
    bm_df, bm_col: df benchmark dataframe & column name
    dgt: rounding decimal
    # start/end: string or offset
    rebase: boolean rebase to beginning or continuous series
    '''
    bm_df.rename(columns={bm_col:'bm'},inplace=True)
    
    df = pd.merge(df, bm_df[['date', 'bm']],how='left', on='date') 
    # df = pd.merge(df, bm_df[['date', 'bm']],how='inner', on='date') 
    
    df['bmfx'] = round(df['bm'],dgt).ffill()
    if rebase == True:
        df['bmfx'] = df['bmfx'].div(df['bmfx'][0])
    else:
        df['bmfx'] = df['bmfx']


    # Divide absolute price by fxcy adjustment factor and rebase to first value
    df['r' + str(_o)] = round(df[_o].div(df['bmfx']),dgt)
    df['r' + str(_h)] = round(df[_h].div(df['bmfx']),dgt)
    df['r'+ str(_l)] = round(df[_l].div(df['bmfx']),dgt)
    df['r'+ str(_c)] = round(df[_c].div(df['bmfx']),dgt)
    df = df.drop(['bm','bmfx'],axis=1)
    
    return df

### RELATIVE ###

def read_xlsx(filename):      
    bm_df = pd.read_excel(filename)
    bm_df.columns= bm_df.columns.str.strip().str.lower()
    bm_df['date'] = pd.to_datetime(bm_df['date'])  
    bm_df = clean_column_names(bm_df)  
    bm_df['close'] = pd.to_numeric(bm_df['close'], errors='coerce')
    
    
    return bm_df

filename_bm = 'FTSEMIB.MI.xlsx'
bm_df = read_xlsx(filename_bm)
bm_name = filename_bm.replace('xlsx', '')

exclude_files = {"FTSEMIB.MI.xlsx", "marginabili.xlsx", "sectors.xlsx", "output_signals.xlsx", "PTF.xlsx"}
include_files = {"A2A.MI.xlsx"}
files = [file for file in os.listdir('/home/laceto/AT/screaning_data') if file.endswith(".xlsx") and file not in exclude_files and file in include_files]  
print(len(files))

def save_plot(df, ticker):
    close_price = df[['close','rclose']].plot(figsize=(20,8),grid=True, title= ticker +  ' Absolute vs relative to ' + bm_name + ' rebased' )
    plt.savefig('plots' + ticker + '.png') 
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
