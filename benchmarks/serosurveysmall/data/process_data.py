import pandas as pd

file = "data.csv"

# Read in the data
data = pd.read_csv(file)
drop_cols = ['ind_id', 'bus_sante', 'IgG_Ratio', 'IFA', 'neg']
age_cat = [(5, 10), (10,20), (20,50), (50, 65), (65,105)]
age_cutoff = [4, 9, 19, 49, 64, 200]

week_ref = 2
sex_ref = 0
age_cat_ref = 3

data.drop(drop_cols, axis=1, inplace=True)

print(data.head())

# assign household id to each individual
all_households = set(data["new_household_id"])
mapper = {k: i for i, k in enumerate(all_households)}
data["new_household_id"] = data["new_household_id"].map(mapper)

print(data.head())

# assign age category to each individual
data["age_cat"] = pd.cut(data["age"], age_cutoff, labels=False)
data["age_cat"] = data["age_cat"].replace({0: 1, 1: 2, 2: 3, 3: 4, 4: 5})
one_hot = pd.get_dummies(data['age_cat']).replace({True: 1, False: 0})
one_hot.columns = [
  'age_cat[5,10)', 
  'age_cat[10,20)', 
  'age_cat[20,50)', 
  'age_cat[50,65)',
  'age_cat[65,105)'
]
data = data.drop('age_cat',axis = 1)
data = data.join(one_hot)
data.drop("age", axis=1, inplace=True)
data.drop("age_cat[20,50)", axis=1, inplace=True)

print(data.head())

one_hot = pd.get_dummies(data['week']).replace({True: 1, False: 0})
one_hot.columns = [
  'week_1', 
  'week_2',
  'week_3',
  'week_4',
  'week_5',
]
data = data.drop('week',axis = 1)
data = data.join(one_hot)
data.drop("week_2", axis=1, inplace=True)

# drop if ind
data = data[data["ind"] != 1]
data.drop("ind", axis=1, inplace=True)

data.rename(columns={'Sex': 'sex'}, inplace=True)

data = data[[
  'pos', 
  'new_household_id',
  'sex',
  "age_cat[5,10)",
  "age_cat[10,20)",
  "age_cat[50,65)",
  "age_cat[65,105)",
  'week_1',
  'week_3',
  'week_4',
  'week_5'
]]

print(data.head())

# save the data
data.to_csv("processed_data.csv", index=False)

