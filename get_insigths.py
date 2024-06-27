import os  
import glob  
import openpyxl  

files_stocks = glob.glob(os.path.join("data_proc", "*"))  

output_signal = []  
for file in files_stocks:  
    data = pd.read_csv(file, sep="\t", header=0, decimal=".")  
    output_signal.append(data)  
  
output_signal = pd.concat(output_signal, axis=0, ignore_index=True)  


  
marginabili = pd.read_excel(next(iter(glob.glob("*marginabili.xlsx")), None))  
marginabili = marginabili.groupby("Descrizione").size().reset_index(name="count")  
marginabili = marginabili.assign(marginabile="si")  
marginabili = marginabili.sort_values("count", ascending=False)

sectors = pd.read_excel(next(iter(glob.glob("*sectors.xlsx")), None))

output_signal = output_signal.merge(sectors, how="left", on="ticker")  
output_signal = output_signal.merge(marginabili, how="left", left_on="name", right_on="Descrizione")  

def get_last_swing(output_signal, swing):  
    output_signal = output_signal.dropna(subset=[swing])  
    output_signal = output_signal.groupby("ticker").tail(1)  
    output_signal = output_signal.sort_values("date", ascending=False)  
    output_signal = output_signal.loc[:, ["ticker", "date", swing]]  
    output_signal = output_signal.rename(columns={"date": "date_last_swing"})  
    return output_signal  
  
def detect_change(df, regime):  
    df = df.loc[:, ["name", "ticker", "sector", "marginabile", "date", "volume", "rrg", regime]]  
    df = df.assign(change=0)  
    df.loc[df[regime] != df[regime].shift(), "change"] = 1  
    df = df.query("change == 1 and rrg == @regime").tail(3)  
    return df  

bull = output_signal.groupby(["ticker", "name"]).tail(1)  
bull = bull.query("rrg == 1")  
bull = bull.loc[:, ["ticker"]]  
