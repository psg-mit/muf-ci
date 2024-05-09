import pandas as pd
import numpy as np

wrong_r = 1
correct_r = 2.45

with open('results.csv') as f:
  data = pd.read_csv(f)
  
# wrong function to compute r
  # data['r'] = (values - wrong_r) ** 2

# correct function to compute r
  # data['r'] = (values - correct_r) ** 2

# given the wrong data['r'], compute "values"
values = np.sqrt(data['r']) + wrong_r

# now, with "values", compute the correct data['r']
print((values - correct_r) ** 2)
print((-values - correct_r) ** 2)
# print(data['r'])