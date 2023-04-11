import pandas as pd

file = "data.csv"

# Read in the data
data = pd.read_csv(file).astype('int32')
print(data.head())

# save the data
data.to_csv("processed_data.csv", index=False)