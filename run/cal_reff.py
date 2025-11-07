import numpy as np


rvs = [4.71e-1, 2.12e-2, 1.18e-2, 2.09e-1, 1.75e0, 
       7.00e-2, 3.90e-1, 1.9e0,   5.00e-1, 6.95e-2]
      
sigmas = [2.51, 2.24, 2.00, 2.03, 2.03, 
          1.95, 2.00, 2.15, 2.20, 2.03]
         
for rv, sigma in zip(rvs, sigmas):
	reff = rv * np.exp(-0.5 * np.log(sigma)**2)
	rg   = rv * np.exp(-3.0 * np.log(sigma)**2)
	print(rv,sigma,reff, rg)