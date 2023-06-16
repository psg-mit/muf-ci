import numpy as np

n = 100
h = 2
f = 1.001
# g = 1

q = 0.5
r = 1

prev_x = 0
for i in range(n):
  x = np.random.normal(f * prev_x, np.sqrt(1/q))
  z = np.random.normal(h * prev_x, np.sqrt(1/r))

  prev_x = x

  with open('data.csv', 'a') as out:
    out.write(str(x) + ', ' + str(z) + '\n')
