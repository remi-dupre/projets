import numpy as np
import matplotlib.pyplot as plt
from numpy.random import rand
#from scipy.stats import norm, chi2
from scipy.stats import *

author = "Rémi Dupré"

# def quartile_N0(alpha, m = 10**4) :
#     # Approxime le quantile associé à la proba alpha pour la distribution N(0, 1)
#     N = norm(0, 1)
#     tirage = N.rvs(m) # m défini une précision
#     return mstats.mquantiles(tirage, [alpha])[0]

# J'ai volontairement lu votre mail après avoir commencé le DM
# Du coup je n'avait pas vu que c'était déjà implémenté
quartile_N0 = norm.ppf

# ----- Question 2.1 -----

def gaussian_ci_nu(samples_x, alpha):
    # On a trouvé un intervalle de confiance sous la forme :
    #   moyenne +- a / sqrt(n)
    n = len(samples_x)
    a = quartile_N0(1 - alpha/2)
    x = np.mean(samples_x)
    d = a / np.sqrt(n)
    return [x-d, x+d]

def test_gaussian_ci_nu(n = 100, alpha = 0.05) :
    # Cherche 1000 fois l'intervalle de confiance pour des nouvelles valeurs des x_1 ... x_n
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
    # Cherche 1000 fois l'intervale de confiance pour des nouvelles valeurs des x_1 ... x_n
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

# ----- Question 2.4 -----

def chi2_noncentral_dfp_ci_lambda(sample_z, p, alpha):
    return [-1, -1] #TODO


# ----- Question 3.2 -----

def truncated_gaussian_pdf(mu, sigma, a) :
    # Densité de la loi gaussienne tronquée
    K = 1 / norm(mu, sigma).cdf(a)    
    return lambda x : K * norm(mu, sigma).pdf(x) * (x <= a)

def test_trucated_gaussian_pdf(mu = 42, sigma = 4.2, a = 53) :
    # Trace la densité de la loi gaussienne tronquée, vérifie qu'elle est bien normalisée
    f = truncated_gaussian_pdf(mu, sigma, a)
    
    from scipy import integrate
    sum_all = integrate.quad(f, -float('inf'), float('inf'))
    if abs(1 - sum_all[0]) <= sum_all[1] :
        print('Correctement normée')
    else :
        print('Cette fonction est mal normalisée (de somme ' + str(sum_all[0]) + ')')
    
    g, d = mu - 5*sigma, mu + 5*sigma
    vals_x = np.linspace(g, d, 1000)
    vals_y = [ f(x) for x in vals_x ]
    plt.plot(vals_x, vals_y)
    plt.show()

# ----- Question 3.3 -----

def rejection_sampling(f, g, g_sampler, c, n):
    # Implémentation de l'algorithme décrit dans le sujet
    ret = [] # Les valeurs correspondant à notre tirage aléatoire
    while len(ret) < n :
        x = g_sampler()
        u = uniform(0, 1).rvs()
        if u <= f(x) / (c*g(x)) :
            ret.append(x)
    return ret

def truncated_gaussian_sampling(mu, sigma, n, a):
    f = truncated_gaussian_pdf(mu, sigma, a)
    g = norm(mu, sigma).pdf
    g_sampler = norm(mu, sigma).rvs
    c = 1 / norm(mu, sigma).cdf(a) # Dans notre cas la constante c optimale est K
    return rejection_sampling(f, g, g_sampler, c, n)

def test_truncated_gaussian_sampling(mu = 42, sigma = 4.2, n = 10**3, a = 50) :
    g, d = mu - 5*sigma, mu + 5*sigma

    # Trace la densité de la gaussienne tronquée
    sample = truncated_gaussian_sampling(mu, sigma, n, a)
    plt.hist(sample, 20, normed=1/n)

    # Trace la répartition d'un tirage
    f = truncated_gaussian_pdf(mu, sigma, a)
    vals_x = np.linspace(g, d, 1000)
    vals_y = [ f(x) for x in vals_x ]
    plt.plot(vals_x, vals_y)
    
    plt.show()

# ----- Question 3.4 -----

def truncated_gaussian_sampling_simple(mu, sigma, n, a):
    # Un algorithme plus naïf pour la loi uniforme tronquée
    ret = [] # Les valeurs correspondant à notre tirage aléatoire
    while len(ret) < n :
        x = norm(mu, sigma)
        if x <= a :
            ret.append(x)
    return ret

def test_truncated_gaussian_sampling_simple(mu = 42, sigma = 4.2, n = 10**3, a = 50) :
    g, d = mu - 5*sigma, mu + 5*sigma

    # Trace la densité de la gaussienne tronquée
    sample = truncated_gaussian_sampling(mu, sigma, n, a)
    plt.hist(sample, 20, normed=1/n)

    # Trace la répartition d'un tirage
    f = truncated_gaussian_pdf(mu, sigma, a)
    vals_x = np.linspace(g, d, 1000)
    vals_y = [ f(x) for x in vals_x ]
    plt.plot(vals_x, vals_y)
    
    plt.show()

def test_inequalities():
    return #TODO
