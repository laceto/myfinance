import os
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from scipy.signal import *
from scipy.signal import find_peaks
import json
from itertools import chain


from relative import *
from utils import *
from lower_upper_OHLC import *
from regime_ma import *
from regime_args import *
from turtle_trader import *
from regime_breakout import *
from historical_swings import *
from regime_floor_ceiling import *
from trading_edge import *
from position_sizing import *
from graph_regime_combo import *

def read_csv(filename): 
    ''' 
    position is the number of positions 
    power is root n.  

    Conservative = 1, aggressive = position, default = 2 
    ''' 
    df = pd.read_csv(filename)
    df.columns = map(str.lower, df.columns)
    
    # Clean the file names  
    df = clean_column_names(df)  
    df['date'] = pd.to_datetime(df['date']) 
    
    return df


def read_xlsx(filename):      
    bm_df = pd.read_excel(filename)
    bm_df.columns= bm_df.columns.str.strip().str.lower()
    bm_df['date'] = pd.to_datetime(bm_df['date'])  
    bm_df = clean_column_names(bm_df)  
    bm_df['close'] = pd.to_numeric(bm_df['close'], errors='coerce')
    
    
    return bm_df

def plot_abs_rel(df, ticker, bm_df, bm_name):
    
    ohlc = ['open','high','low','close']
    _o,_h,_l,_c = [ohlc[h] for h in range(len(ohlc))]

    bm_col = 'close'
    dgt = 5

    df = relative(df,_o,_h,_l,_c, bm_df, bm_col, dgt, rebase=True)
    df = df.set_index('date')
    plot1 = df[['close','rclose']].plot(figsize=(20,8),grid=True, title= ticker +  ' Absolute vs relative to ' + bm_name + ' rebased' )
    plt.show(plot1)
    
def signal_bo(df, window):
    
    df['hi_'+str(window)] = df['high'].rolling(window).max()
    df['lo_'+str(window)] = df['low'].rolling(window).min()
    df['bo_'+ str(window)]= regime_breakout(df= df,_h= 'high',_l= 'low',window= window)
    return df

def signal_rbo(df, window, relative=False):
    
    _o,_h,_l,_c = lower_upper_OHLC(df,relative = relative)
    
    prefix_h = 'hi_'
    prefix_l = 'lo_'
    prefix_bo = 'bo_'
    if relative:
        prefix_h = 'rhi_'
        prefix_l = 'rlo_'
        prefix_bo = 'rbo_'  
    
    df[prefix_h + str(window)] = df[_h].rolling(window).max()
    df[prefix_l + str(window)] = df[_l].rolling(window).min()
    df[prefix_bo + str(window)]= regime_breakout(df= df,_h= _h,_l= _l,window= window)
    return df

# def plot_signal_bo(df, window, ticker):
    
#     df[['close','hi_'+str(window),'lo_'+str(window),'bo_'+ str(window)]].plot(
#         secondary_y= ['bo_'+ str(window)], figsize=(20,5), style=['k','g:','r:','b-.'], 
#         title = str.upper(ticker)+' '+str(window) +' days high/low')
#     plt.show()

def plot_signal_bo(df, window, ticker, relative):

    _o,_h,_l,_c = lower_upper_OHLC(df,relative = relative)
    
    prefix_h = 'hi_'
    prefix_l = 'lo_'
    prefix_bo = 'bo_'
    close = 'close'
    if relative:
        prefix_h = 'rhi_'
        prefix_l = 'rlo_'
        prefix_bo = 'rbo_'  
        close = 'rclose'
    df = df.set_index('date')
    df[[close, prefix_h + str(window), prefix_l + str(window), prefix_bo + str(window)]].plot(
        secondary_y= [prefix_bo + str(window)], figsize=(20,5), style=['k','g:','r:','b-.'], 
        title = str.upper(ticker)+' '+str(window) +' days high/low')
    plt.show()
    
def signal_tt(df, slow, fast):
    
    _o,_h,_l,_c = lower_upper_OHLC(df,relative = False)

    df['bo_'+ str(slow)] = regime_breakout(df,_h,_l,window = slow)
    df['bo_'+ str(fast)] = regime_breakout(df,_h,_l,window = fast)
    df['turtle_'+ str(slow) + str(fast)] = turtle_trader(df, _h='high', _l='low', slow=slow, fast=fast) 
    
    return df

def signal_rtt(df, slow, fast, relative=False):
    
    _o,_h,_l,_c = lower_upper_OHLC(df,relative = relative)

    prefix_bo = 'tt_'
    if relative:
        prefix_bo = 'rtt_'  

    df[prefix_bo + str(slow)] = regime_breakout(df,_h,_l,window = slow)
    df[prefix_bo + str(fast)] = regime_breakout(df,_h,_l,window = fast)
    df[prefix_bo + str(slow) + str(fast)] = turtle_trader(df, _h='high', _l='low', slow=slow, fast=fast) 
    
    return df

def plot_signal_tt(df, fast, slow):
    
    rg_cols = ['turtle_'+ str(sselow)+str(fast)]

    df[['close','turtle_'+ str(slow)+str(fast)] ].plot(
        secondary_y= rg_cols,figsize=(20,5), style=['k','b-.'], 
        title = str.upper('')+' '+str(rg_cols))
    plt.show()  
    
def signal_sma(df, st, mt, lt):
    df['sma_' + str(st) + str(mt)] = regime_sma(df, _c='close', st= st, lt= mt)
    df['sma_' + str(mt) + str(lt)] = regime_sma(df, _c='close', st= mt, lt= lt)
    df['sma_' + str(st) + str(mt) + str(lt)] = df['sma_' + str(st) + str(mt)] * df['sma_' + str(mt) + str(lt)]
    
    return df

def signal_ema(df, st, mt, lt):
    df['ema_' + str(st) + str(mt)] = regime_ema(df, _c='close', st= st, lt= mt)
    df['ema_' + str(mt) + str(lt)] = regime_ema(df, _c='close', st= mt, lt= lt)
    df['ema_' + str(st) + str(mt) + str(lt)] = df['ema_' + str(st) + str(mt)] * df['ema_' + str(mt) + str(lt)]
    
    return df

def signal_rema(df, st, mt, lt, relative=False):  
    prefix = 'ema_'  
    _c = 'close'  
    if relative:  
        prefix = 'rema_'  
        _c = 'rclose'  
          
    df[prefix + str(st) + str(mt)] = regime_ema(df, _c=_c, st=st, lt=mt)  
    df[prefix + str(mt) + str(lt)] = regime_ema(df, _c=_c, st=mt, lt=lt)  
    df[prefix + str(st) + str(mt) + str(lt)] = df[prefix + str(st) + str(mt)] * df[prefix + str(mt) + str(lt)]  
      
    return df 

def signal_rsma(df, st, mt, lt, relative=False):  
    prefix = 'sma_'  
    _c = 'close'  
    if relative:  
        prefix = 'rsma_'  
        _c = 'rclose'  
          
    df[prefix + str(st) + str(mt)] = regime_sma(df, _c=_c, st=st, lt=mt)  
    df[prefix + str(mt) + str(lt)] = regime_sma(df, _c=_c, st=mt, lt=lt)  
    df[prefix + str(st) + str(mt) + str(lt)] = df[prefix + str(st) + str(mt)] * df[prefix + str(mt) + str(lt)]  
    
    return df
  


def plot_signal_ma(df, st, mt, lt):

    df[['close','sma_'+ str(st) + str(mt) + str(lt)] ].plot(
        secondary_y= 'sma_'+ str(st) + str(mt) + str(lt),figsize=(20,5), style=['k','b-.'], 
        title = str.upper('')+' '+str(['sma_'+ str(st) + str(mt) + str(lt)]))
    
    df[['close','ema_'+ str(st) + str(mt) + str(lt)] ].plot(
        secondary_y= 'ema_'+ str(st) + str(mt) + str(lt),figsize=(20,5), style=['k','b-.'], 
        title = str.upper('')+' '+str(['ema_'+ str(st) + str(mt) + str(lt)]))
        
    plt.show() 
    
def detect_regime(df, bm_df):
    
    _o,_h,_l,_c = lower_upper_OHLC(df,relative = False)
    rhs = ['hi1', 'lo1','hi2', 'lo2', 'hi3', 'lo3']
    rt_hi,rt_lo,_hi,_lo,shi,slo = [rhs[h] for h in range(len(rhs))]
    bm_col = 'close'
    dgt = 5
    df= relative(df,_o,_h,_l,_c, bm_df, bm_col, dgt= dgt, rebase=True)
    
    # for a in np.arange(0,2):  
    #     df = historical_swings(df,_o,_h,_l,_c, dist= None, hurdle= None)
    #     df = cleanup_latest_swing(df, shi, slo, rt_hi, rt_lo)
    #     ud, bs, bs_dt, _rt, _swg, hh_ll, hh_ll_dt = latest_swing_variables(df, shi, slo,rt_hi,rt_lo,_h, _l,_c)
    #     vlty = round(average_true_range(df=df, _h= _h, _l= _l, _c= _c , n=63)[hh_ll_dt],2)
    #     dist_vol = 5 * vlty
    #     dist_pct = 0.05
    #     _sign = test_distance(ud,bs, hh_ll, dist_vol, dist_pct)
    #     df = retest_swing(df, _sign, _rt, hh_ll_dt, hh_ll, _c, _swg)
    #     retrace_vol = 2.5 * vlty
    #     retrace_pct = 0.05
    #     df = retracement_swing(df,_sign,_swg,_c,hh_ll_dt,hh_ll, vlty,retrace_vol, retrace_pct)
    #     _o,_h,_l,_c = lower_upper_OHLC(df,relative = True)
    #     rrhs = ['rh1', 'rl1','rh2', 'rl2', 'rh3', 'rl3']
    #     rt_hi,rt_lo,_hi,_lo,shi,slo = [rrhs[h] for h in range(len(rrhs))]

    # params = [63, 0.05, 0.05, 1.5, 2]
    # vlty_n,dist_pct,retrace_pct,threshold,dgt= [params[h] for h in range(len(params))]
    
    params = ['2014-12-31', None, 63, 0.05, 0.05, 1.5, 2,5,2.5,3]
    start,end,vlty_n,dist_pct,retrace_pct,threshold,dgt,d_vol,r_vol,lvl= [params[h] for h in range(len(params))]

    swing_val = ['rg','lo1','hi1','lo3','hi3','clg','flr','rg_ch']
    rg,rt_lo,rt_hi,slo,shi,clg,flr,rg_ch = [swing_val[s] for s in range(len(swing_val))]   

    for a in np.arange(0,2):
        df = round(historical_swings(df,_o,_h,_l,_c, dist= None, hurdle= None),dgt)
        df = cleanup_latest_swing(df,shi,slo,rt_hi,rt_lo)
        ud, bs, bs_dt, _rt, _swg, hh_ll, hh_ll_dt = latest_swing_variables(df,shi,slo,rt_hi,rt_lo,_h,_l, _c)
        vlty = round(average_true_range(df,_h,_l,_c, n= vlty_n)[hh_ll_dt],dgt)
        dist_vol = d_vol * vlty
        _sign = test_distance(ud,bs, hh_ll, dist_vol, dist_pct)
        df = retest_swing(df, _sign, _rt, hh_ll_dt, hh_ll, _c, _swg)
        retrace_vol = r_vol * vlty
        df = retracement_swing(df, _sign, _swg, _c, hh_ll_dt, hh_ll, vlty, retrace_vol, retrace_pct)
        stdev = df[_c].rolling(vlty_n).std(ddof=0)
        df = regime_floor_ceiling(df,_h,_l,_c,slo, shi,flr,clg,rg,rg_ch,stdev,threshold)    

        _o,_h,_l,_c = lower_upper_OHLC(df,relative = True)
        rswing_val = ['rrg','rl1','rh1','rl3','rh3','rclg','rflr','rrg_ch']
        rg,rt_lo,rt_hi,slo,shi,clg,flr,rg_ch = [rswing_val[s] for s in range(len(rswing_val))]
    
    return df



def plot_signal_abs(df, ticker):
    
    plot_abs_cols = ['close','hi3', 'lo3','clg','flr','rg_ch','rg']
    plot_abs_style = ['k', 'ro', 'go', 'kv', 'k^','b:','b--']
    y2_abs = ['rg']
    plot_rel_cols = ['rclose','rh3', 'rl3','rclg','rflr','rrg_ch','rrg']
    plot_rel_style = ['grey', 'ro', 'go', 'yv', 'y^','m:','m--']
    y2_rel = ['rrg']

    # df['date'] = pd.to_datetime(df['date'])
    df = df.set_index('date')

    df[plot_abs_cols].plot(secondary_y= y2_abs,figsize=(15,8),
                title = str.upper(ticker)+ ' Absolute',# grid=True,
                style=plot_abs_style)
    plt.show() 

    
def plot_signal_rel(df, ticker):
    
    plot_abs_cols = ['close','hi3', 'lo3','clg','flr','rg_ch','rg']
    plot_abs_style = ['k', 'ro', 'go', 'kv', 'k^','b:','b--']
    y2_abs = ['rg']
    plot_rel_cols = ['rclose','rh3', 'rl3','rclg','rflr','rrg_ch','rrg']
    plot_rel_style = ['grey', 'ro', 'go', 'yv', 'y^','m:','m--']
    y2_rel = ['rrg']

    # df['date'] = pd.to_datetime(df['date'])
    df = df.set_index('date')

    df[plot_rel_cols].plot(secondary_y=y2_rel,figsize=(15,8),
            title = str.upper(ticker)+ ' Relative',# grid=True,
            style=plot_rel_style)
    plt.show() 
    
def plot_regime_abs(df, ticker):
    
    ma_st = ma_mt = ma_lt = lt_lo = lt_hi = st_lo = st_hi = 0
    df = df.set_index('date')

    rg_combo = ['close','rg','lo3','hi3','lo3','hi3','clg','flr','rg_ch']
    _c,rg,lo,hi,slo,shi,clg,flr,rg_ch =[rg_combo[r] for r in range(len(rg_combo)) ]

    graph_regime_combo(ticker,df,_c,rg,lo,hi,slo,shi,clg,flr,rg_ch,ma_st,ma_mt,ma_lt,lt_lo,lt_hi,st_lo,st_hi)
    plt.show()

    
def plot_regime_rel(df, ticker):
    
    ma_st = ma_mt = ma_lt = lt_lo = lt_hi = st_lo = st_hi = 0
    df = df.set_index('date')
    rrg_combo = ['rclose','rrg','rl3','rh3','rl3','rh3','rclg','rflr','rrg_ch']
    _c,rg,lo,hi,slo,shi,clg,flr,rg_ch =[rrg_combo[r] for r in range(len(rrg_combo)) ]

    graph_regime_combo(ticker,df,_c,rg,lo,hi,slo,shi,clg,flr,rg_ch,ma_st,ma_mt,ma_lt,lt_lo,lt_hi,st_lo,st_hi)
    plt.show()
    
    
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

def plot_PL(df, ticker, m):
    
    df[['tt_PL_cum','tt_chg1D']].plot(secondary_y=['tt_chg1D'],figsize=(20,8),style= ['b','c:'],title= str(ticker) +' Daily P&L & Cumulative P&L ' + str(m))
    plt.show()
    
def plot_price_signal_cumreturns(df, ticker, signal, m):
    df[['close','stop_loss',signal,'tt_cumul']].plot(secondary_y=[signal,'tt_cumul'],figsize=(20,8),style= ['k','r--','b:','b'],
                                                     title= str(ticker) + ' Close Price, signal, cumulative returns ' + str(m))
    plt.show()
    
def get_equity_risk(df, tolerance, mn, mx, span):
    
    avg = (mn + mx) / 2
    
    df['peak_eqty'] = df['close'].cummax()
    df['constant_risk'] = -df['close'] * avg
    df['convex_risk'] = -risk_appetite(df['close'], tolerance, mn, mx, span, shape=1) * df['peak_eqty']
    df['concave_risk'] = -risk_appetite(df['close'], tolerance, mn, mx, span, shape=-1) * df['peak_eqty']
    df['tolerance'] = df['peak_eqty'] * (1 + tolerance )
    df['drawdown'] = df['close'] / df['peak_eqty'] -1
    # df = df.set_index('date')
    
    return df

def plot_equity_risk(df, ticker, m):
    
    df[['close','peak_eqty','tolerance', 'drawdown'] ].plot(style = ['k','g-.','r-.','m:'] ,
            secondary_y=['drawdown'], figsize=(20,8),grid=True)
    
    df[['close', 'peak_eqty', 'tolerance',
        'constant_risk','convex_risk','concave_risk']].plot(figsize= (20,8),grid=True,
    secondary_y=['constant_risk','convex_risk','concave_risk'],
    style= ['k','g-.','r-.','b:','y-.', 'orange'], 
    title= str(ticker) + ' equity risk ' + str(m))
    
    plt.show()
    
    
def get_expectancies(df, window, log_returns, signal):
    
    # Separate profits from losses
    tt_log_returns = df[[log_returns]]
    
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

def plot_expectancies(df, window, m):
    
    df[window*2:][['trading_edge', 'geometric_expectancy', 'kelly']].plot(
    secondary_y = ['kelly'], figsize=(20,6),style=['b','y','g'], 
    title= 'trading_edge, geometric_expectancy, kelly ' + str(m))
    
    
def get_shares(df, starting_capital, lot, mn, mx, tolerance, equal_weight, span, fx, s):
    
    # df[s] = df[s].fillna(0)
    
    shs_fxd = shs_ccv = shs_cvx = shs_eql = 0
    df.loc[df.index[0],'constant'] = df.loc[df.index[0],'concave'] = starting_capital
    df.loc[df.index[0],'convex'] = df.loc[df.index[0],'equal_weight'] = starting_capital
    ccy_name = 'EUR'
    df[ccy_name] = 1
    shs_fxd = shs_ccv = shs_cvx = shs_eql = 0
    avg = (mn + mx) / 2

    for i in range(1,len(df)):
        df['equal_weight'].iat[i] = df['equal_weight'].iat[i-1] + df['tt_chg1D_fx'].iloc[i] * shs_eql
        df['constant'].iat[i] = df['constant'].iat[i-1] + df['tt_chg1D_fx'].iloc[i] * shs_fxd
        df['concave'].iat[i] = df['concave'].iat[i-1] + df['tt_chg1D_fx'].iloc[i] * shs_ccv
        df['convex'].iat[i] = df['convex'].iat[i-1] + df['tt_chg1D_fx'].iloc[i] * shs_cvx

        ccv = risk_appetite(eqty= df['concave'][:i], tolerance=tolerance, 
                            mn= mn, mx=mx, span=5, shape=-1)
        cvx = risk_appetite(eqty= df['convex'][:i], tolerance=tolerance, 
                            mn= mn, mx=mx, span=5, shape=1)

        
        if (df[s].iloc[i-1] ==0) & (df[s].iloc[i] !=0):
            px = df['close'].iloc[i]
            sl = df['stop_loss'].iloc[i]
            fx  = df[ccy_name].iloc[i]
            shs_eql = (df['equal_weight'].iloc[i]  * equal_weight  *fx//(px * lot)) * lot

            

            if px != sl:
                shs_fxd = eqty_risk_shares(px,sl,eqty= df['constant'].iloc[i],
                                            risk= avg,fx=fx,lot=lot)
                shs_ccv = eqty_risk_shares(px,sl,eqty= df['concave'].iloc[i],
                                                risk= ccv.iloc[-1],fx=fx,lot=lot)
                shs_cvx = eqty_risk_shares(px,sl,eqty= df['convex'].iloc[i],
                                                risk= cvx.iloc[-1],fx=fx,lot=lot)
    
    
    df['shs_eql'] = -(df['equal_weight'] * equal_weight  *fx//(df['close'].iloc[i] * lot)) * lot
    df['shs_fxd'] = ((df['constant'] -df['constant'].shift())  / df['tt_chg1D_fx'])
    df['shs_ccv'] = ((df['concave'] -df['concave'].shift())  / df['tt_chg1D_fx'])
    df['shs_cvx'] = ((df['convex'] -df['convex'].shift())  / df['tt_chg1D_fx'])
    
    df['peak_eqty'] = df['close'].cummax()
    df['constant_risk'] = -df['close'] * avg
    df['convex_risk'] = -risk_appetite(df['close'], tolerance, mn, mx, span, shape=1) * df['peak_eqty']
    df['concave_risk'] = -risk_appetite(df['close'], tolerance, mn, mx, span, shape=-1) * df['peak_eqty']
    df['tolerance'] = df['peak_eqty'] * (1 + tolerance )
    df['drawdown'] = df['close'] / df['peak_eqty'] -1
    
    return df

def plot_shares_signal(df, ticker, signal, m):
    df[['shs_eql','shs_fxd','shs_ccv','shs_cvx', signal]].plot(secondary_y=[signal],figsize=(20,8),style= ['k','r--','b:','b', 'y'],
                                                         title= str(ticker)+' shares ' + str(m))
    plt.show()

def plot_equity_amount(df, ticker, m):
    # df[['constant','convex','concave','equal_weight']].plot(figsize=(20,8),style= ['k','r--','b:','b'],
    #                                                  title= str(ticker) + ' equity amount ' + str(m))
    
    df[['constant','concave','convex','equal_weight', 'tt_PL_cum_fx']].plot(
        figsize = (20,10), 
        grid=True,
        style=['y.-','m--','g-.','b:', 'b'],
        secondary_y='tt_PL_cum_fx', title= str(ticker) + ' cumulative P&L, concave, convex, constant equity at risk, equal weight ' + str(m))
    plt.show()
