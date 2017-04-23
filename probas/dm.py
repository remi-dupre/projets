import numpy as np
from numpy.random import rand
#from scipy.stats import norm, chi2
from scipy.stats import *

author = "Rémi Dupré"

# ----- Question 2.1 -----

def gaussian_ci_nu(samples_x, alpha):
    def a(m = 10**4) :
        # Approxime le quantile associé à la proba (1-alpha) pour la distribution N(0, 1)
        N = norm(0, 1)
        tirage = N.rvs(m) # m défini une précision
        return mstats.mquantiles(tirage, [1 - alpha/2])[0]
    
    # On a trouvé un intervale de confiance sous la forme :
    #   moyenne +- a / sqrt(n)
    n = len(samples_x)
    x = np.mean(samples_x)
    d = a() / np.sqrt(n)
    return [x-d, x+d]

def test_gaussian_ci_nu(n = 100, alpha = 0.05) :
    # Cherche 1000 fois l'intervale de confiance pour des nouvelles valeures des x_1 ... x_n
    # On compte le nombre de fois où mu est dedans
    N = norm(42, 1)
    
    bons = mauvais = 0
    for i in range(1000) :
        samples_x = N.rvs(n)
        r = gaussian_ci_nu(samples_x, alpha)
        if 42 < r[0] or 42 > r[1] :
            mauvais += 1
        else :
            bons += 1
        
    print(str(bons) + " bons contre " + str(mauvais) + " mauvais")


def chi2_noncentral_df1_ci_lambda(samples_y, alpha):
    return [-1, -1] #TODO

def chi2_noncentral_dfp_ci_lambda(sample_z, p, alpha):
    return [-1, -1] #TODO


def rejection_sampling(f, g, g_sampler, c, n):
    return [] #TODO

def truncated_gaussian_sampling(mu, sigma, n, a):
    return [] #TODO

def truncated_gaussian_sampling_simple(mu, sigma, n, a):
    return [] #TODO

def test_inequalities():
    return #TODO
