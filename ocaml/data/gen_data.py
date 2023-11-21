import numpy as np

n = 100

theta = np.random.random_sample()
mu1 = np.random.normal(-10, 1)
mu2 = np.random.normal(10, 1)

with open('data.csv', 'w') as f:
    f.write('theta: {}\n'.format(theta))
    f.write('mu1: {}\n'.format(mu1))
    f.write('mu2: {}\n'.format(mu2))

for i in range(n):
    if np.random.random_sample() < theta:
        y = np.random.normal(mu1, 1)
    else:
        y = np.random.normal(mu2, 1)
    with open('data.csv', 'a') as f:
        f.write('{}\n'.format(y))
