from scipy.signal import find_peaks
import pandas as pd
import numpy as np


#### hilo_alternation(hilo, dist= None, hurdle= None) ####
def hilo_alternation(hilo, dist= None, hurdle= None):
    i=0    
    while (np.sign(hilo.shift(1)) == np.sign(hilo)).any(): # runs until duplicates are eliminated

        # removes swing lows > swing highs
        hilo.loc[(np.sign(hilo.shift(1)) != np.sign(hilo)) &  # hilo alternation test
                 (hilo.shift(1)<0) &  # previous datapoint:  high
                 (np.abs(hilo.shift(1)) < np.abs(hilo) )] = np.nan # high[-1] < low, eliminate low 

        hilo.loc[(np.sign(hilo.shift(1)) != np.sign(hilo)) &  # hilo alternation
                 (hilo.shift(1)>0) &  # previous swing: low
                 (np.abs(hilo ) < hilo.shift(1))] = np.nan # swing high < swing low[-1]

        # alternation test: removes duplicate swings & keep extremes
        hilo.loc[(np.sign(hilo.shift(1)) == np.sign(hilo)) & # same sign
                 (hilo.shift(1) < hilo )] = np.nan # keep lower one

        hilo.loc[(np.sign(hilo.shift(-1)) == np.sign(hilo)) & # same sign, forward looking 
                 (hilo.shift(-1) < hilo )] = np.nan # keep forward one

        # removes noisy swings: distance test
        if pd.notnull(dist):
            hilo.loc[(np.sign(hilo.shift(1)) != np.sign(hilo))&\
                 (np.abs(hilo + hilo.shift(1)).div(dist, fill_value=1)< hurdle)] = np.nan

        # reduce hilo after each pass
        hilo = hilo.dropna().copy() 
        i+=1
        if i == 4: # breaks infinite loop
            break 
        return hilo
#### hilo_alternation(hilo, dist= None, hurdle= None) ####


#### historical_swings(df,_o,_h,_l,_c, dist= None, hurdle= None) #### 
def historical_swings(df,_o,_h,_l,_c, dist= None, hurdle= None):
    
    reduction = df[[_o,_h,_l,_c]].copy() 
    reduction['avg_px'] = round(reduction[[_h,_l,_c]].mean(axis=1),2)
    highs = reduction['avg_px'].values
    lows = - reduction['avg_px'].values
    reduction_target =  len(reduction) // 100
#     print(reduction_target )

    n = 0
    while len(reduction) >= reduction_target: 
        highs_list = find_peaks(highs, distance = 1, width = 0)
        lows_list = find_peaks(lows, distance = 1, width = 0)
        hilo = reduction.iloc[lows_list[0]][_l].sub(reduction.iloc[highs_list[0]][_h],fill_value=0)

        # Reduction dataframe and alternation loop
        hilo_alternation(hilo, dist= None, hurdle= None)
        reduction['hilo'] = hilo

        # Populate reduction df
        n += 1        
        reduction[str(_h)[:2]+str(n)] = reduction.loc[reduction['hilo']<0 ,_h]
        reduction[str(_l)[:2]+str(n)] = reduction.loc[reduction['hilo']>0 ,_l]

        # Populate main dataframe
        df[str(_h)[:2]+str(n)] = reduction.loc[reduction['hilo']<0 ,_h]
        df[str(_l)[:2]+str(n)] = reduction.loc[reduction['hilo']>0 ,_l]
        
        # Reduce reduction
        reduction = reduction.dropna(subset= ['hilo'])
        # reduction.fillna(method='ffill', inplace = True)
        reduction = reduction.ffill()
        highs = reduction[str(_h)[:2]+str(n)].values
        lows = -reduction[str(_l)[:2]+str(n)].values
        
        if n >= 9:
            break
            
    return df
#### historical_swings(df,_o,_h,_l,_c, dist= None, hurdle= None) ####


#### cleanup_latest_swing(df, shi, slo, rt_hi, rt_lo) ####
def cleanup_latest_swing(df, shi, slo, rt_hi, rt_lo): 
    '''
    removes false positives
    '''
    # latest swing
    shi_dt = df.loc[pd.notnull(df[shi]), shi].index[-1]
    s_hi = df.loc[pd.notnull(df[shi]), shi].iloc[-1]  
    slo_dt = df.loc[pd.notnull(df[slo]), slo].index[-1] 
    s_lo = df.loc[pd.notnull(df[slo]), slo].iloc[-1]  
    len_shi_dt = len(df[:shi_dt])
    len_slo_dt = len(df[:slo_dt])
    

    # Reset false positives to np.nan
    for i in range(2):
        
        if (len_shi_dt > len_slo_dt) & ((df.loc[shi_dt:,rt_hi].max()> s_hi) | (s_hi<s_lo)):
            df.loc[shi_dt, shi] = np.nan
            len_shi_dt = 0
        elif (len_slo_dt > len_shi_dt) & ((df.loc[slo_dt:,rt_lo].min()< s_lo)| (s_hi<s_lo)):
            df.loc[slo_dt, slo] = np.nan 
            len_slo_dt = 0
        else:
            pass
    
    return df
#### cleanup_latest_swing(df, shi, slo, rt_hi, rt_lo) ####


#### latest_swings(df, shi, slo, rt_hi, rt_lo, _h, _l, _c, _vol) ####
def latest_swing_variables(df, shi, slo, rt_hi, rt_lo, _h, _l, _c):
    '''
    Latest swings dates & values
    '''
    shi_dt = df.loc[pd.notnull(df[shi]), shi].index[-1]
    slo_dt = df.loc[pd.notnull(df[slo]), slo].index[-1]
    s_hi = df.loc[pd.notnull(df[shi]), shi].iloc[-1] 
    s_lo = df.loc[pd.notnull(df[slo]), slo].iloc[-1]
    
    if slo_dt > shi_dt: 
        swg_var = [1,s_lo,slo_dt,rt_lo,shi, df.loc[slo_dt:,_h].max(), df.loc[slo_dt:, _h].idxmax()]         
    elif shi_dt > slo_dt: 
        swg_var = [-1,s_hi,shi_dt,rt_hi,slo, df.loc[shi_dt:, _l].min(),df.loc[shi_dt:, _l].idxmin()]        
    else: 
        ud = 0
    ud, bs, bs_dt, _rt, _swg, hh_ll, hh_ll_dt = [swg_var[h] for h in range(len(swg_var))]   
        
    return ud, bs, bs_dt, _rt, _swg, hh_ll, hh_ll_dt
#### latest_swings(df, shi, slo, rt_hi, rt_lo, _h, _l, _c, _vol) ####


#### test_distance(ud, bs, hh_ll, vlty, dist_vol, dist_pct) ####
def test_distance(ud,bs, hh_ll, dist_vol, dist_pct): 
    
    # priority: 1. Vol 2. pct 3. dflt
    if (dist_vol > 0):    
        distance_test = np.sign(abs(hh_ll - bs) - dist_vol)
    elif (dist_pct > 0):
        distance_test = np.sign(abs(hh_ll / bs - 1) - dist_pct)
    else:
        distance_test = np.sign(dist_pct)
        
    return int(max(distance_test,0) * ud)
#### test_distance(ud, bs, hh_ll, vlty, dist_vol, dist_pct) ####

#### ATR ####
def average_true_range(df, _h, _l, _c, n):
    '''
    http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:average_true_range_atr
    '''
    atr =  (df[_h].combine(df[_c].shift(), max) - df[_l].combine(df[_c].shift(), min)).rolling(window=n).mean()
    return atr

#### ATR ####


#### retest_swing(df, _sign, _rt, hh_ll_dt, hh_ll, _c, _swg) ####
def retest_swing(df, _sign, _rt, hh_ll_dt, hh_ll, _c, _swg):
    rt_sgmt = df.loc[hh_ll_dt:, _rt] 

    if (rt_sgmt.count() > 0) & (_sign != 0): # Retests exist and distance test met    
        if _sign == 1: # 
            rt_list = [rt_sgmt.idxmax(),rt_sgmt.max(),df.loc[rt_sgmt.idxmax():, _c].cummin()]
            
        elif _sign == -1:
            rt_list = [rt_sgmt.idxmin(), rt_sgmt.min(), df.loc[rt_sgmt.idxmin():, _c].cummax()]
        rt_dt,rt_hurdle, rt_px = [rt_list[h] for h in range(len(rt_list))]

        if str(_c)[0] == 'r':
            df.loc[rt_dt,'rrt'] = rt_hurdle
        elif str(_c)[0] != 'r':
            df.loc[rt_dt,'rt'] = rt_hurdle    

        if (np.sign(rt_px - rt_hurdle) == - np.sign(_sign)).any():
            df.at[hh_ll_dt, _swg] = hh_ll 
    return df
#### retest_swing(df, _sign, _rt, hh_ll_dt, hh_ll, _c, _swg) ####


#### retracement_swing(df, _sign, _swg, _c, hh_ll_dt, hh_ll, vlty, retrace_vol, retrace_pct) ####
def retracement_swing(df, _sign, _swg, _c, hh_ll_dt, hh_ll, vlty, retrace_vol, retrace_pct):
    if _sign == 1: #
        retracement = df.loc[hh_ll_dt:, _c].min() - hh_ll

        if (vlty > 0) & (retrace_vol > 0) & ((abs(retracement / vlty) - retrace_vol) > 0):
            df.at[hh_ll_dt, _swg] = hh_ll
        elif (retrace_pct > 0) & ((abs(retracement / hh_ll) - retrace_pct) > 0):
            df.at[hh_ll_dt, _swg] = hh_ll

    elif _sign == -1:
        retracement = df.loc[hh_ll_dt:, _c].max() - hh_ll
        if (vlty > 0) & (retrace_vol > 0) & ((round(retracement / vlty ,1) - retrace_vol) > 0):
            df.at[hh_ll_dt, _swg] = hh_ll
        elif (retrace_pct > 0) & ((round(retracement / hh_ll , 4) - retrace_pct) > 0):
            df.at[hh_ll_dt, _swg] = hh_ll
    else:
        retracement = 0
    return df
#### retracement_swing(df, _sign, _swg, _c, hh_ll_dt, hh_ll, vlty, retrace_vol, retrace_pct) ####


#### latest_swings(df, shi, slo, rt_hi, rt_lo, _h, _l, _c, _vol) ####
def latest_swing_variables(df, shi, slo, rt_hi, rt_lo, _h, _l, _c):
    '''
    Latest swings dates & values
    '''
    shi_dt = df.loc[pd.notnull(df[shi]), shi].index[-1]
    slo_dt = df.loc[pd.notnull(df[slo]), slo].index[-1]
    s_hi = df.loc[pd.notnull(df[shi]), shi].iloc[-1]
    s_lo = df.loc[pd.notnull(df[slo]), slo].iloc[-1]
    
    if slo_dt > shi_dt: 
        swg_var = [1,s_lo,slo_dt,rt_lo,shi, df.loc[slo_dt:,_h].max(), df.loc[slo_dt:, _h].idxmax()]         
    elif shi_dt > slo_dt: 
        swg_var = [-1,s_hi,shi_dt,rt_hi,slo, df.loc[shi_dt:, _l].min(),df.loc[shi_dt:, _l].idxmin()]        
    else: 
        ud = 0
    ud, bs, bs_dt, _rt, _swg, hh_ll, hh_ll_dt = [swg_var[h] for h in range(len(swg_var))]   
        
    return ud, bs, bs_dt, _rt, _swg, hh_ll, hh_ll_dt
#### latest_swings(df, shi, slo, rt_hi, rt_lo, _h, _l, _c, _vol) ####
