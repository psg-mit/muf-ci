import numpy as np

n = 400
h = 2
f = 1.001
# g = 1

x = 0

r = 1
q = 0.5


for i in range(n):
  
  z = np.random.normal(h * x, 1/q)
  x = np.random.normal(f * x, 1/r)

  with open('data.csv', 'a') as out:
    out.write(str(x) + ', ' + str(z) + '\n')
