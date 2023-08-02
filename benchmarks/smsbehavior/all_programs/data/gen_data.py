import numpy as np

lambda1 = 17.749241151314294
lambda2 = 22.72325027312415
tau = 44

n = 74

for i in range(n):
  
  if i < tau:
    counts = np.random.poisson(lambda1)
  else:
    counts = np.random.poisson(lambda2)

  with open('data.csv', 'a') as out:
    out.write(str(counts) + '\n')
