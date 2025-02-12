import sys  
import os  
import pandas as pd  
from AT_funs import *

current_directory = os.getcwd()  
exclude_files = {"FTSEMIB.MI.xlsx", "marginabili.xlsx", "sectors.xlsx", "output_signals.xlsx", "PTF.xlsx", "NDX.xlsx"}  
  
files = []  
for file in os.listdir(current_directory):  
    if os.path.isfile(os.path.join(current_directory, file)) and file not in exclude_files and not file.endswith(".MI.xlsx") and file.endswith(".xlsx") and not file.endswith(".DE.xlsx"):  
        files.append(file)  
  
print("Files in current directory (excluding specified files and files ending with '.MI.xlsx'):")  
for file in files:  
    print(file)  

filename_bm = 'NDX.xlsx'
bm_df = read_xlsx(filename_bm)
bm_name = filename_bm.replace('xlsx', '')

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
    file_path = file
    df = pd.read_excel(file_path)
    if df.shape[0] >= 500:
        dfs.append(df)

print('letti tutti i files')

from AT_funs import *

dfs_list = []

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
    except:
        failed.append(i)
