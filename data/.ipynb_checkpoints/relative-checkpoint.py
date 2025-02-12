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
