# for one file at time
# import papermill as pm
# import os
  
# # Set the name of your notebook  
# notebook_name = 'template.ipynb'  

# # Set the path to your file  
# path_to_file = 'data/A2A.MI.xlsx'

# # Define the parameters to pass to the notebook  
# parameters = {'path_to_file': path_to_file}

# # Set the path for the output notebook  
# output_notebook_name = os.path.basename(path_to_file).replace('.xlsx', '') + '.ipynb'
  
# # Execute the notebook using subprocess  
# # subprocess.run(['jupyter', 'nbconvert', notebook_name, '--to', 'html', '--execute']) 
# pm.execute_notebook(notebook_name, output_notebook_name, parameters=parameters)  



import os  
import papermill as pm  
  
# Set the name of your notebook  
notebook_name = 'template.ipynb'  
# os.chdir("..")   
# Set the path to the folder containing the files  
folder_path = 'data/'  
  
# Get a list of the first 3 files in the folder  
# file_list = os.listdir(folder_path)
exclude_files = {"FTSEMIB.MI.xlsx", "marginabili.xlsx", "sectors.xlsx", "output_signals.xlsx", "PTF.xlsx"}
# file_list = [file for file in os.listdir('data') if file.endswith(".xlsx") and file not in exclude_files]
file_list = [file for file in os.listdir(folder_path) if file.endswith(".xlsx") and file not in exclude_files and not file.endswith(".py")]  

os.chdir('NT')

# for file_name in file_list:  
#     # Set the path to the file  
#     path_to_file = os.path.join(folder_path, file_name)  
#     print(file_name)
  
#     # Define the parameters to pass to the notebook  
#     parameters = {'path_to_file': path_to_file}  
  
#     # Set the path for the output notebook  
#     output_notebook_name = os.path.splitext(file_name)[0] + '_output.ipynb'  
  
#     # # Execute the notebook using Papermill and save the output to a new notebook  
#     # pm.execute_notebook(notebook_name, output_notebook_name, parameters=parameters) 

#     # Ignore the .ipynb_checkpoints folder  
#     if not file_name.startswith('.'):  
#         # Execute the notebook using Papermill and save the output to a new notebook  
#         pm.execute_notebook(notebook_name, output_notebook_name, parameters=parameters)  


for file_name in file_list[:1]:    
    # Set the path to the file    
    path_to_file = os.path.join(folder_path, file_name)    
    print(file_name)  
    
    # Define the parameters to pass to the notebook    
    parameters = {'path_to_file': path_to_file}    
    
    # Set the path for the output notebook    
    output_notebook_name = os.path.splitext(file_name)[0] + '_output.ipynb'    
      
    try:  
        # Ignore the .ipynb_checkpoints folder    
        if not file_name.startswith('.'):    
            # Execute the notebook using Papermill and save the output to a new notebook    
            pm.execute_notebook(notebook_name, output_notebook_name, parameters=parameters)   
    except Exception as e:  
        print(f"An error occurred while executing the notebook {output_notebook_name}: {e}")  
