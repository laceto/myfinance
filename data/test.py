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


def get_returns(df, signal):
    
    df[signal] = df[signal].fillna(0)
    fast = 20 
    df[str(signal) + '_chg1D'] = df['close'].diff() * df[signal].shift()
    df[str(signal) + '_chg1D_fx'] = df['close'].diff() * df[signal].shift()
    df[str(signal) + '_PL_cum'] = df[str(signal) + '_chg1D'].cumsum()
    df[str(signal) + '_returns'] = df['close'].pct_change() * df[signal].shift()
    df[str(signal) + '_log_returns'] = np.log(df['close']/df['close'].shift()) * df[signal].shift()
    df[str(signal) + '_cumul'] = df[str(signal) + '_log_returns'].cumsum().apply(np.exp) - 1 
    df['stop_loss'] = np.where(df[signal] == 1, df['low'].rolling(fast).min(),
                        np.where(df[signal] == -1, df['high'].rolling(fast).max(),np.nan))
    df[str(signal) + '_PL_cum_fx'] = df[str(signal) + '_chg1D_fx'].cumsum()
    
    # df = df.set_index('date')
    
    return df

def get_shares(df, starting_capital, lot, mn, mx, tolerance, equal_weight, span, fx, chg1D_fx, signal):
    
    
    
    shs_fxd = shs_ccv = shs_cvx = shs_eql = 0
    df.loc[df.index[0], str(signal) + '_constant'] = df.loc[df.index[0], str(signal) + '_concave'] = starting_capital
    df.loc[df.index[0], str(signal) + '_convex'] = df.loc[df.index[0], str(signal) + '_equal_weight'] = starting_capital
    ccy_name = 'EUR'
    df[ccy_name] = 1
    shs_fxd = shs_ccv = shs_cvx = shs_eql = 0
    avg = (mn + mx) / 2
    
    # df.loc[df.index[0], str(signal) + '_concave'] = starting_capital

    for i in range(1,len(df)):
        df[str(signal) + '_equal_weight'].iat[i] = df[str(signal) + '_equal_weight'].iat[i-1] + df[chg1D_fx].iloc[i] * shs_eql
        df[str(signal) + '_constant'].iat[i] = df[str(signal) + '_constant'].iat[i-1] + df[chg1D_fx].iloc[i] * shs_fxd
        df[str(signal) + '_concave'].iat[i] = df[str(signal) + '_concave'].iat[i-1] + df[chg1D_fx].iloc[i] * shs_ccv
        df[str(signal) + '_convex'].iat[i] = df[str(signal) + '_convex'].iat[i-1] + df[chg1D_fx].iloc[i] * shs_cvx

        ccv = risk_appetite(eqty= df[str(signal) + '_concave'][:i], tolerance=tolerance, 
                            mn= mn, mx=mx, span=5, shape=-1)
        cvx = risk_appetite(eqty= df[str(signal) + '_convex'][:i], tolerance=tolerance, 
                            mn= mn, mx=mx, span=5, shape=1)

        
        if (df[signal].iloc[i-1] ==0) & (df[signal].iloc[i] !=0):
            px = df['close'].iloc[i]
            sl = df['stop_loss'].iloc[i]
            fx  = df[ccy_name].iloc[i]
            shs_eql = (df[str(signal) + '_equal_weight'].iloc[i]  * equal_weight  *fx//(px * lot)) * lot

            

            if px != sl:
                shs_fxd = eqty_risk_shares(px,sl,eqty= df[str(signal) + '_constant'].iloc[i],
                                            risk= avg,fx=fx,lot=lot)
                shs_ccv = eqty_risk_shares(px,sl,eqty= df[str(signal) + '_concave'].iloc[i],
                                                risk= ccv.iloc[-1],fx=fx,lot=lot)
                shs_cvx = eqty_risk_shares(px,sl,eqty= df[str(signal) + '_convex'].iloc[i],
                                                risk= cvx.iloc[-1],fx=fx,lot=lot)

    
    return df
    # return sl
    
def get_expectancies(df, window, log_return, signal):
    
    # Separate profits from losses
    tt_log_returns = df[[log_return]]
    
    loss_roll = tt_log_returns.copy()
    loss_roll[loss_roll > 0] = np.nan
    win_roll = tt_log_returns.copy()
    win_roll[win_roll < 0] = np.nan

    # window= 100
    win_rate = win_roll.rolling(window).count() / window
    loss_rate = loss_roll.rolling(window).count() / window
    avg_win = win_roll.fillna(0).rolling(window).mean()
    avg_loss = loss_roll.fillna(0).rolling(window).mean()

    df[str(signal) + '_trading_edge'] = expectancy(win_rate,avg_win,avg_loss).ffill()
    df[str(signal) + '_geometric_expectancy'] = george(win_rate,avg_win,avg_loss).ffill()
    df[str(signal) + '_kelly'] = kelly(win_rate,avg_win,avg_loss).ffill()
    # df = df.set_index('date')
    
    return df



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
<<<<<<< HEAD
        dfs[i].to_csv(os.path.join('data_proc/' + ticker + '.txt') , sep='\t', index=False)  
=======
        dfs[i].to_csv(os.path.join('data_proc/' + ticker + '.txt') , sep='\t', index=False) 
>>>>>>> 830b57b2c2f1d665d99e19ee78b9aa683cf0efc5

    except:
        failed.append(i)

print('for signal done')
