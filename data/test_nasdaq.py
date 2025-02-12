import sys  
import os  
import pandas as pd  

# sys.path.insert(0, '..')
from AT_funs import *


filename_bm = 'data/FTSEMIB.MI.xlsx'
bm_df = read_xlsx(filename_bm)
bm_name = filename_bm.replace('xlsx', '')

# files = [file for file in os.listdir('data') if file.endswith(".xlsx") and file != "FTSEMIB.MI.xlsx" and file != "marginabili.xlsx" and file != "sectors.xlsx" and file != "output_signals.xlsx" and file != "PTF.xlsx", and file == "A2A.MI.xlsx"]
# print(len(files))

exclude_files = {"FTSEMIB.MI.xlsx", "marginabili.xlsx", "sectors.xlsx", "output_signals.xlsx", "PTF.xlsx", "NDX.xlsx"}
# include_files = {"A2A.MI.xlsx"}
# files = [file for file in os.listdir('data') if file.endswith(".xlsx") and file not in exclude_files and file in include_files]
files = [file for file in os.listdir('data') if file.endswith("MI.xlsx") and file not in exclude_files]
print(files)