import re  
  
def clean_column_names(df):  
    """  
    Cleans the column names of a Pandas DataFrame by replacing whitespaces with underscores,  
    replacing one or more dots with underscores, converting names to lowercase,  
    removing special characters, and removing trailing underscores from the names.  
    """  
    # Regular expression pattern to match non-alphanumeric characters  
    pattern = re.compile(r'[^a-zA-Z0-9_]+')  
    new_columns = []  
    for col in df.columns:  
        # Replace whitespaces with underscores  
        col = col.replace(' ', '_')  
        # Replace one or more dots with underscores  
        col = re.sub(r'\.+', '_', col)  
        # Convert to lowercase  
        col = col.lower()  
        # Remove special characters  
        col = pattern.sub('', col)  
        # Remove trailing underscores  
        col = col.rstrip('_')  
        new_columns.append(col)  
    df.columns = new_columns  
    return df  
