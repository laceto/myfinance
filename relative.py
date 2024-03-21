### RELATIVE
def relative(df,_o,_h,_l,_c, bm_df, bm_col, dgt, start, end,rebase=True):
    '''
    df: df
    bm_df, bm_col: df benchmark dataframe & column name
    ccy_df,ccy_col: currency dataframe & column name
    dgt: rounding decimal
    start/end: string or offset
    rebase: boolean rebase to beginning or continuous series
    '''
    # Slice df dataframe from start to end period: either offset or datetime
    df = df[start:end] 
    
    # inner join of benchmark & currency: only common values are preserved
    df = df.join(bm_df[[bm_col]],how='inner') 
    # df = df.join(ccy_df[[ccy_col]],how='inner')

    # rename benchmark name as bm and currency as ccy
    # df.rename(columns={bm_col:'bm', ccy_col:'ccy'},inplace=True)
    df.rename(columns={bm_col:'bm'},inplace=True)

    # Adjustment factor: calculate the scalar product of benchmark and currency
    # df['bmfx'] = round(df['bm'].mul(df['ccy']),dgt).fillna(method='ffill')
    df['bmfx'] = round(df['bm'],dgt).fillna(method='ffill')
    if rebase == True:
        df['bmfx'] = df['bmfx'].div(df['bmfx'][0])

    # Divide absolute price by fxcy adjustment factor and rebase to first value
    df['r' + str(_o)] = round(df[_o].div(df['bmfx']),dgt)
    df['r' + str(_h)] = round(df[_h].div(df['bmfx']),dgt)
    df['r'+ str(_l)] = round(df[_l].div(df['bmfx']),dgt)
    df['r'+ str(_c)] = round(df[_c].div(df['bmfx']),dgt)
    df = df.drop(['bm','bmfx'],axis=1)
    
    return (df)

### RELATIVE ###
