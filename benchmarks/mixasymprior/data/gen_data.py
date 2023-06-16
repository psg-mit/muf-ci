import numpy as np

n = 100

theta = 0.3
mu1 = np.random.normal(2.75, np.sqrt(0.5))
mu2 = np.random.normal(-2.75, np.sqrt(0.5))
var1 = np.random.gamma(1, 1)
var2 = np.random.gamma(1, 1)

with open('data.csv', 'w') as f:
    f.write('mu1: {}\n'.format(mu1))
    f.write('mu2: {}\n'.format(mu2))
    f.write('var1: {}\n'.format(var1))
    f.write('var2: {}\n'.format(var2))

for i in range(n):
    if np.random.random_sample() < theta:
        y = np.random.normal(mu1, np.sqrt(1/var1))
    else:
        y = np.random.normal(mu2, np.sqrt(1/var2))
    with open('data.csv', 'a') as f:
        f.write('{}\n'.format(y))
