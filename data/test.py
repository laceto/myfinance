import sys  
import os  
import pandas as pd  

# sys.path.insert(0, '..')
from AT_funs import *


filename_bm = 'data/FTSEMIB.MI.xlsx'
bm_df = read_xlsx(filename_bm)
bm_name = filename_bm.replace('xlsx', '')

files = [file for file in os.listdir('data') if file.endswith(".xlsx") and file != "FTSEMIB.MI.xlsx" and file != "marginabili.xlsx" and file != "sectors.xlsx" and file != "output_signals.xlsx" and file != "PTF.xlsx"]  

window_bo = 100
fast = 20
slow = 50
st = 50
mt = 100
lt = 150
lvl = 3
bm_col = 'close'
dgt = 5

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
