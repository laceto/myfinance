import pandas as pd
import numpy as np



#### regime_floor_ceiling(df, hi,lo,cl, slo, shi,flr,clg,rg,rg_ch,stdev,threshold) ####
def regime_floor_ceiling(df, _h,_l,_c,slo, shi,flr,clg,rg,rg_ch,stdev,threshold):
    # Lists instantiation
    threshold_test,rg_ch_ix_list,rg_ch_list = [],[], []
    floor_ix_list, floor_list, ceiling_ix_list, ceiling_list = [],[],[],[]

    ### Range initialisation to 1st swing
    floor_ix_list.append(df.index[0])
    ceiling_ix_list.append(df.index[0])
    
    ### Boolean variables
    ceiling_found = floor_found = breakdown = breakout = False

    ### Swings lists
    swing_highs = list(df[pd.notnull(df[shi])][shi])
    swing_highs_ix = list(df[pd.notnull(df[shi])].index)
    swing_lows = list(df[pd.notnull(df[slo])][slo])
    swing_lows_ix = list(df[pd.notnull(df[slo])].index)
    loop_size = np.maximum(len(swing_highs),len(swing_lows))

    ### Loop through swings
    for i in range(loop_size): 

        ### asymetric swing list: default to last swing if shorter list
        try:
            s_lo_ix = swing_lows_ix[i]
            s_lo = swing_lows[i]
        except:
            s_lo_ix = swing_lows_ix[-1]
            s_lo = swing_lows[-1]

        try:
            s_hi_ix = swing_highs_ix[i]
            s_hi = swing_highs[i]
        except:
            s_hi_ix = swing_highs_ix[-1]
            s_hi = swing_highs[-1]

        swing_max_ix = np.maximum(s_lo_ix,s_hi_ix) # latest swing index

        ### CLASSIC CEILING DISCOVERY
        if (ceiling_found == False):   
            top = df[floor_ix_list[-1] : s_hi_ix][_h].max()
            ceiling_test = round((s_hi - top) / stdev[s_hi_ix] ,1)  

            ### Classic ceiling test
            if ceiling_test <= -threshold: 
                ### Boolean flags reset
                ceiling_found = True 
                floor_found = breakdown = breakout = False                
                threshold_test.append(ceiling_test)

                ### Append lists
                ceiling_list.append(top)
                ceiling_ix_list.append(df[floor_ix_list[-1]: s_hi_ix][_h].idxmax())           
                rg_ch_ix_list.append(s_hi_ix)
                rg_ch_list.append(s_hi) 

        ### EXCEPTION HANDLING: price penetrates discovery swing
        ### 1. if ceiling found, calculate regime since rg_ch_ix using close.cummin
        elif (ceiling_found == True):
            close_high = df[rg_ch_ix_list[-1] : swing_max_ix][_c].cummax()
            df.loc[rg_ch_ix_list[-1] : swing_max_ix, rg] = np.sign(close_high - rg_ch_list[-1])

            ### 2. if price.cummax penetrates swing high: regime turns bullish, breakout
            if (df.loc[rg_ch_ix_list[-1] : swing_max_ix, rg] >0).any():
                ### Boolean flags reset
                floor_found = ceiling_found = breakdown = False
                breakout = True

        ### 3. if breakout, test for bearish pullback from highest high since rg_ch_ix
        if (breakout == True):
            brkout_high_ix = df.loc[rg_ch_ix_list[-1] : swing_max_ix, _c].idxmax()
            brkout_low = df[brkout_high_ix : swing_max_ix][_c].cummin()
            df.loc[brkout_high_ix : swing_max_ix, rg] = np.sign(brkout_low - rg_ch_list[-1])


        ### CLASSIC FLOOR DISCOVERY        
        if (floor_found == False): 
            bottom = df[ceiling_ix_list[-1] : s_lo_ix][_l].min()
            floor_test = round((s_lo - bottom) / stdev[s_lo_ix],1)

            ### Classic floor test
            if (floor_test >= threshold): 
                
                ### Boolean flags reset
                floor_found = True
                ceiling_found = breakdown = breakout = False
                threshold_test.append(floor_test)

                ### Append lists
                floor_list.append(bottom)
                floor_ix_list.append(df[ceiling_ix_list[-1] : s_lo_ix][_l].idxmin())           
                rg_ch_ix_list.append(s_lo_ix)
                rg_ch_list.append(s_lo)

        ### EXCEPTION HANDLING: price penetrates discovery swing
        ### 1. if floor found, calculate regime since rg_ch_ix using close.cummin
        elif(floor_found == True):    
            close_low = df[rg_ch_ix_list[-1] : swing_max_ix][_c].cummin()
            df.loc[rg_ch_ix_list[-1] : swing_max_ix, rg] = np.sign(close_low - rg_ch_list[-1])

            ### 2. if price.cummin penetrates swing low: regime turns bearish, breakdown
            if (df.loc[rg_ch_ix_list[-1] : swing_max_ix, rg] <0).any():
                floor_found = floor_found = breakout = False
                breakdown = True                

        ### 3. if breakdown,test for bullish rebound from lowest low since rg_ch_ix
        if (breakdown == True):
            brkdwn_low_ix = df.loc[rg_ch_ix_list[-1] : swing_max_ix, _c].idxmin() # lowest low  
            breakdown_rebound = df[brkdwn_low_ix : swing_max_ix][_c].cummax() # rebound
            df.loc[brkdwn_low_ix : swing_max_ix, rg] = np.sign(breakdown_rebound - rg_ch_list[-1])
#             breakdown = False
#             breakout = True  

    ### POPULATE FLOOR,CEILING, RG CHANGE COLUMNS
    df.loc[floor_ix_list[1:], flr] = floor_list
    df.loc[ceiling_ix_list[1:], clg] = ceiling_list
    df.loc[rg_ch_ix_list, rg_ch] = rg_ch_list
    df[rg_ch] = df[rg_ch].ffill()

    ### regime from last swing
    df.loc[swing_max_ix:,rg] = np.where(ceiling_found, # if ceiling found, highest high since rg_ch_ix
                                         np.sign(df[swing_max_ix:][_c].cummax() - rg_ch_list[-1]),
                                        np.where(floor_found, # if floor found, lowest low since rg_ch_ix
                                                 np.sign(df[swing_max_ix:][_c].cummin() - rg_ch_list[-1]),
                                                 np.sign(df[swing_max_ix:][_c].rolling(5).mean() - rg_ch_list[-1]))) 
    df[rg] = df[rg].ffill()
#     df[rg+'_no_fill'] = df[rg]
    return df

#### regime_floor_ceiling(df, hi,lo,cl, slo, shi,flr,clg,rg,rg_ch,stdev,threshold) ####
