import numpy as np
from numpy.random import rand
#from scipy.stats import norm, chi2
from scipy.stats import *

author = "Rémi Dupré"

def quartile_N0(alpha, m = 10**4) :
    # Approxime le quantile associé à la proba alpha pour la distribution N(0, 1)
    N = norm(0, 1)
    tirage = N.rvs(m) # m défini une précision
    return mstats.mquantiles(tirage, [alpha])[0]
    

# ----- Question 2.1 -----

def gaussian_ci_nu(samples_x, alpha):
    # On a trouvé un intervale de confiance sous la forme :
    #   moyenne +- a / sqrt(n)
    n = len(samples_x)
    a = quartile_N0(1 - alpha/2)
    x = np.mean(samples_x)
    d = a / np.sqrt(n)
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

# ----- Question 2.2 -----

def chi2_noncentral_df1_ci_lambda(samples_y, alpha):
    # On refait à peu près la même chose sur les sqrt(y_i)
    # Le résultat est plus grossier au voisinage de 0 (à cause de l'inégalité triangulaire)
    n = len(samples_y)
    a = quartile_N0(1 - alpha)
    x = np.mean([ np.sqrt(y) for y in samples_y ])
    d = a / np.sqrt(n)
    return [0, (x+d)**2]
    
def test_chi2_noncentral_df1_ci_lambda(n = 100, cent = 0.5, alpha = 0.05) :
    # Cherche 1000 fois l'intervale de confiance pour des nouvelles valeures des x_1 ... x_n
    # On compte le nombre de fois où lambda est dedans
    N = norm(-np.sqrt(cent), 1) # lambda = cent
    
    bons = mauvais = 0
    for i in range(1000) :
        samples_y = np.square(N.rvs(n))
        r = chi2_noncentral_df1_ci_lambda(samples_y, alpha)
        if cent < r[0] or cent > r[1] :
            mauvais += 1
        else :
            bons += 1
        
    print(str(bons) + " bons contre " + str(mauvais) + " mauvais")

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
