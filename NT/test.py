import sys  
import os  
  
# Add the 'data' directory to sys.path  
sys.path.insert(0, 'data')  
  
# Get a list of all Python files in the 'data' directory  
py_files = [f for f in os.listdir('data') if f.endswith('.py')]  
  
# Loop over the list of Python files and import each one  
for f in py_files:  
    module_name = os.path.splitext(f)[0]  # Extract the module name from the file name  
    module = __import__(module_name)  # Import the module  

sys.path.insert(0, '..')
from AT_funs import *
