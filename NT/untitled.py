import pandas as pd  
import openpyxl  
from nbformat.v4 import new_notebook, new_markdown_cell, new_code_cell  
from nbconvert import PythonExporter  
from nbformat import NotebookNode  
import os  
import nbformat  

import os  
import sys  
import pandas as pd
import openpyxl
 
cwd = os.getcwd()  
os.chdir("..")  

sys.path.append(os.path.abspath("data"))
#from relative import *
#from AT_funs import *

import pandas as pd  
import openpyxl  
from nbformat.v4 import new_notebook, new_markdown_cell, new_code_cell  
from nbconvert import PythonExporter  
import os  
  
# Get a list of all Excel files in the `data` folder  
data_files = os.listdir('data')  
data_files = [f for f in data_files if f.endswith('.xlsx')]  
  
# Only process the first three Excel files  
data_files = data_files[:3]  
  
for data_file in data_files:  
    # Read one Excel file from the `data` folder and create a pandas DataFrame  
    df = pd.read_excel(f'data/{data_file}')  
  
    # Extract the unique value from the `ticker` column and create a `name` variable  
    name = df['ticker'].unique()[0]  
  
    # Create a copy of the Jupyter notebook template from the `NT` folder, renaming it with the `name` variable from `ticker`  
    new_nb_name = f'{name}.ipynb'  
    os.system(f'cp NT/template.ipynb NT/{new_nb_name}')  
  
    # Update the Jupyter notebook with the DataFrame you just read  
    nb = new_notebook()  
    nb.cells.append(new_markdown_cell(f'# {name} Financial Analysis'))  
    nb.cells.append(new_code_cell('import pandas as pd'))  
    nb.cells.append(new_code_cell(f"df = pd.read_excel('data/{data_file}')"))  
    nb.cells.append(new_code_cell('display(df)'))  
  
    # Create a NotebookNode object from the notebook  
    nb_node = NotebookNode(notebook=nb)  
  
    # Write the notebook to a file  
    with open(f'NT/{new_nb_name}', 'w') as f:  
        f.write(nbformat.writes(nb_node))  
  
    # Save the Jupyter notebook  
    os.system(f'jupyter nbconvert --to notebook --inplace --execute NT/{new_nb_name}')  
