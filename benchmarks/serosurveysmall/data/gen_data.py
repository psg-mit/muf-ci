import numpy as np
import pandas as pd

n_survey = 100

n_pos_control = 181
n_neg_control = 176
control_tp_result = 154
control_fp_result = 0

intercept = 2.162511
sex = 0.4638138
age_cat = 0.6693432
# age_cat_5_10 = 0.050155
# age_cat_10_20 = -0.0583128
# age_cat_50_65 = 0.6693432
# age_cat_65_105 = 0.1790346
week = 0.2492106
# week1 = 0.2492106
# week3 = -0.2200088
# week4 = -0.0593874
# week5 = 0.3817401
# sigma_h = 0.9161439
sens = 0.808969
spec = 0.9941081

fpr = 1 - spec

b = np.array([
  intercept,
  sex,
  age_cat,
  week,
  # age_cat_5_10,
  # age_cat_10_20,
  # age_cat_50_65,
  # age_cat_65_105,
  # week1,
  # week3,
  # week4,
  # week5
])

def sigmoid(x):
  return 1 / (1 + np.exp(-x))

with open('original_processed_data.csv') as f:
  data = pd.read_csv(f)

# renumber household ids
# household_ids = data["new_household_id"].unique()
# hh = household_ids.shape[0]

# print("hh: ", hh)

# new_household_ids = np.arange(hh, dtype=int)
# mapper = dict(zip(household_ids, new_household_ids))
# data["new_household_id"] = data["new_household_id"].map(mapper)
# data["new_household_id"] = data["new_household_id"].astype(int)

# eta = np.random.normal(0, 1, hh)

# calculate new pos 
for index, row in data.iterrows():
  # eta_h = eta[row["new_household_id"]]

  # turn one-hot encoded age cat to single value
  row_age_cat = 2
  if row["age_cat[5,10)"] == 1:
    row_age_cat = 0
  elif row["age_cat[10,20)"] == 1:
    row_age_cat = 1
  elif row["age_cat[50,65)"] == 1:
    row_age_cat = 3
  elif row["age_cat[65,105)"] == 1:
    row_age_cat = 4

  # turn one-hot encoded week to single value
  row_week = 1
  if row["week_1"] == 1:
    row_week = 0 
  elif row["week_3"] == 1:
    row_week = 2
  elif row["week_4"] == 1:
    row_week = 3
  elif row["week_5"] == 1:
    row_week = 4

  x = np.array([
    1,
    row["sex"],
    row_age_cat,
    # row["age_cat[5,10)"],
    # row["age_cat[10,20)"],
    # row["age_cat[50,65)"],
    # row["age_cat[65,105)"],
    # row["week_1"],
    # row["week_3"],
    # row["week_4"],
    # row["week_5"],
    row_week,
  ])

  # calculate probability of being positive
  p = sigmoid(np.dot(b, x)) # + sigma_h*eta_h

  true_pos = np.random.random() < p 
  survey_res = np.random.random() < sens if true_pos else np.random.random() < fpr

  data.at[index, "pos"] = 1 if survey_res else 0
  data.at[index, "age_cat"] = row_age_cat
  data.at[index, "week"] = row_week
data.drop(columns="new_household_id", inplace=True)
data.drop(columns=["age_cat[5,10)", "age_cat[10,20)", "age_cat[50,65)", "age_cat[65,105)", "week_1", "week_3", "week_4", "week_5"], inplace=True)
# write to file
data.to_csv("processed_data.csv", index=False)

