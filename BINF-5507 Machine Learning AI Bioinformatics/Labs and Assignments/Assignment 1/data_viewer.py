import pandas as pd
import numpy as np

data4 = pd.read_csv("weatherstats_toronto_daily.csv")
# print(data4.info())
# print(data4.head(10))
data4 = data4.replace(["NA", "N/A", "na", "NaN", ""], np.nan)
print(data4.head(15))
print("There are "+str(len(data4.columns))+" features in the dataset")
print("There are "+str(len(data4.select_dtypes(include=['int64', 'float64']).columns.tolist()))+" numerical features")
cat_cols = data4.select_dtypes(include=['object']).columns.tolist()
print("There are "+str(len(cat_cols))+" catagorical features:")
for col in cat_cols:
    print("\t"+col)
print(data4.info())

