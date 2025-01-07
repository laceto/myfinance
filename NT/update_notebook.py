import nbformat  
from nbconvert.preprocessors import ClearOutputPreprocessor  
from nbconvert.exporters import NotebookExporter  
  
# Load the notebook from file  
with open('template.ipynb', 'r') as f:  
    nb = nbformat.read(f, as_version=4)  
  
# Clear the notebook output  
clear_output = ClearOutputPreprocessor()  
nb, _ = clear_output.preprocess(nb, {})  
  
# Export the notebook to the latest format  
exporter = NotebookExporter()  
exporter.exclude_input_prompt = True  
exporter.exclude_output_prompt = True  
exporter.exporter_name = 'template'  
body, resources = exporter.from_notebook_node(nb)  
  
# Save the notebook back to file  
with open('template.ipynb', 'w') as f:  
    f.write(body)  