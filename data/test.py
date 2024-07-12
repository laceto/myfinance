import sys  
import os  
import pandas as pd  

# sys.path.insert(0, '..')
from AT_funs import *


filename_bm = 'data/FTSEMIB.MI.xlsx'
bm_df = read_xlsx(filename_bm)
bm_name = filename_bm.replace('xlsx', '')

files = [file for file in os.listdir('data') if file.endswith(".xlsx") and file != "FTSEMIB.MI.xlsx" and file != "marginabili.xlsx" and file != "sectors.xlsx" and file != "output_signals.xlsx" and file != "PTF.xlsx"]  
print(len(files))

window_bo = 100
fast = 20
slow = 50
st = 50
mt = 100
lt = 150
lvl = 3
bm_col = 'close'
dgt = 5
window_exp = 100
starting_capital = 100000
lot = 100
mn = -0.0025
mx = -0.0075
# avg = (mn + mx) / 2
tolerance= -0.1
equal_weight = 0.05
span= 5
fx = 1

# Read all xlsx files
dfs = []
last_row_dfs = [None] * len(files)
failed = []
signal_df = pd.DataFrame()
signal_list = []

regime_df = pd.DataFrame()
last_row_df = pd.DataFrame()
signal_df = pd.DataFrame()
dfs_list = []
last_row_list = []
failed = []


for file in files:
    file_path = os.path.join('data', file)
    df = pd.read_excel(file_path)
    if df.shape[0] >= 1000:
        dfs.append(df)

print('letti tutti i files')

def last_row_dictionary(df, ticker):

    df_cols = list(df.columns)
    col_dict = {'Symbol':str.upper(ticker),'date':df['date'].max().strftime('%Y%m%d')}
    for i, col_name in enumerate(df_cols):
        if pd.isnull(df.iloc[-1,i]):
            try:
                last_index = df[pd.notnull(df.iloc[:,i])].index[-1]
                len_last_index = len(df[:last_index]) - 1
                col_dict.update({col_name + '_dt': last_index.strftime('%Y%m%d')})
                col_dict.update({col_name : df.iloc[len_last_index,i]})
            except:
                col_dict.update({col_name + '_dt':np.nan})
                col_dict.update({col_name : np.nan})
        else:
            col_dict.update({col_name : df.iloc[-1,i]})
    return col_dict

for i in range(len(dfs)):
    print(i)
    ohlc = ['open','high','low','close']
    _o,_h,_l,_c = [ohlc[h] for h in range(len(ohlc))]

    ticker = dfs[i]['ticker'][0]
    try:
        dfs[i] = relative(dfs[i],_o,_h,_l,_c, bm_df, bm_col, dgt, rebase=True)
        dfs[i] = signal_rbo(dfs[i], window_bo, relative=True)
        dfs[i] = signal_rtt(dfs[i], fast = fast, slow = slow, relative=True)
        dfs[i] = signal_rema(dfs[i], st, mt, lt, relative=True)
        dfs[i] = signal_rsma(dfs[i], st, mt, lt, relative=True)
        dfs[i] = detect_regime(dfs[i], bm_df)
        # dfs[i] = relative(dfs[i],_o,_h,_l,_c, bm_df, bm_col, dgt, rebase=True)
        # dfs[i] = signal_rbo(dfs[i], window_bo, relative=True)
        # dfs[i] = get_returns(dfs[i], 'rbo_100')
        # dfs[i] = get_expectancies(dfs[i], window_exp, 'rbo_100_log_returns', 'rbo_100')
        # dfs[i] = get_shares(dfs[i], starting_capital, lot, mn, mx, tolerance, equal_weight, span, fx, 'rbo_100_chg1D_fx', 'rbo_100')
        # dfs[i] = signal_rtt(dfs[i], fast = fast, slow = slow, relative=True)
        # dfs[i] = get_returns(dfs[i], 'rtt_5020')
        # dfs[i] = get_expectancies(dfs[i], window_exp, 'rtt_5020_log_returns', 'rtt_5020')
        # dfs[i] = get_shares(dfs[i], starting_capital, lot, mn, mx, tolerance, equal_weight, span, fx, 'rtt_5020_chg1D_fx', 'rtt_5020')
        # dfs[i] = signal_rema(dfs[i], st, mt, lt, relative=True)
        # dfs[i] = get_returns(dfs[i], 'rema_50100150')
        # dfs[i] = get_expectancies(dfs[i], window_exp, 'rema_50100150_log_returns', 'rema_50100150')
        # dfs[i] = get_shares(dfs[i], starting_capital, lot, mn, mx, tolerance, equal_weight, span, fx, 'rema_50100150_chg1D_fx', 'rema_50100150')
        # dfs[i] = signal_rsma(dfs[i], st, mt, lt, relative=True)
        # dfs[i] = get_returns(dfs[i], 'rsma_50100150')
        # dfs[i] = get_expectancies(dfs[i], window_exp, 'rsma_50100150_log_returns','rsma_50100150')
        # dfs[i] = get_shares(dfs[i], starting_capital, lot, mn, mx, tolerance, equal_weight, span, fx, 'rsma_50100150_chg1D_fx','rsma_50100150')
        # dfs[i] = detect_regime(dfs[i], bm_df)
        # dfs[i] = get_returns(dfs[i], 'rrg')        
        # dfs[i] = get_expectancies(dfs[i], window_exp, 'rrg_log_returns', 'rrg')    
        # dfs[i] = get_shares(dfs[i], starting_capital, lot, mn, mx, tolerance, equal_weight, span, fx, 'rrg_chg1D_fx', 'rrg')
        dfs[i].to_csv(os.path.join('data_proc/' + ticker + '.txt') , sep='\t', index=False) 

    except:
        failed.append(i)

print('for signal done')
