import numpy as np
import numpy.random
import numpy.linalg
from scipy.stats import norm, binom, expon, poisson

import matplotlib.pyplot as plt


# Utiliser un seed fixe pour permettre la reproductibilité
seed = 2017

def sample_normal(N):
    return np.random.normal(size=N)

def sample_normal49(N):
    return np.random.normal(loc=4., scale=3, size=N)

def sample_ber13(N):
    return (np.random.rand(N) <= (1./3.)) * 1.

def sample_bin514(N):
    return np.random.binomial(5, 0.25, size=N)

def sample_exp2(N):
    return np.random.exponential(scale=0.5, size=N)

def sample_poi2(N):
    return np.random.poisson(lam=2, size=N)


def min_max_over_M_N_simulations(M, N, samp):
    mean_samples = [np.mean(samp(N)) for i in range(M)]
    return np.min(mean_samples), np.max(mean_samples)

# Vérification qualitative de la LGN

M = 1000

print ("distr_name\t: (max_over, samples)\tMin      \tMax      \tMax-Min")
for (samp, name) in [(sample_normal, "N(0,1)\t\t"), (sample_normal49, "N(4, 9)\t\t"),
                     (sample_ber13, "Ber(1/3)\t"), (sample_bin514, "Bin(5, 1/4)\t"),
                     (sample_exp2, "exp(2)\t\t"), (sample_poi2, "Poi(2)\t\t")]:
    for N in [10, 1000, 10000, 100000]:
        np.random.seed(seed)
        min_, max_ = min_max_over_M_N_simulations(M, N, samp)
        print (name + ": (" + str(M) +",\t" + str(N) + ")\t" + ("%.9f" % min_) + "\t"+ ("%.9f" % max_) + "\t" + ("%.9f" % (max_-min_)))


# Vérification du TCL
# Vérification que la CDF converge simplement
#   -> prendre des samples aléatoire (ou dÃ©terministes) de la loi normale,
#      et vÃ©rifier que la valeur de la CDF converge en ces points
#   -> vÃ©rifier la convergence d'histogramme avec un pas d'histogramme de plus en plus faible
#      Question: A t'on convergence uniforme de la CDF ici ?
# VÃ©rification pour plusieurs f bornÃ©s que E[f(X_n)] converge vers E(f(X))
#   -> Choix de f ?
# Test d'adéquation à une gaussienne
#   -> voir cours de Stats

step = 0.05
bins = np.arange(-10, 10, step)
gauss_histogram = np.asarray([norm.cdf(i+step) - norm.cdf(i) for i in bins])
gauss_histogram = gauss_histogram[:-1]

for (samp, name, esp, var) in [(sample_normal, "N(0,1)", 0., 1.), (sample_normal49, "N(4, 9)", 4., 9.),
                     (sample_ber13, "Ber(1/3)", 1./3., 2./9.), (sample_bin514, "Bin(5, 1/4)", 1.25, 5.*3./16),
                     (sample_exp2, "exp(2)", 0.5, 0.25), (sample_poi2, "Poi(2)", 2., 2.)]:
    f, axarr = plt.subplots(4, 2)
    for k, N in zip(range(4), [200, 1000, 5000, 10000]):
        np.random.seed(seed)
        # On tire N fois N lancés.
        samples = np.asarray([np.sum(samp(N)) for i in range(N)], dtype=np.float64)
        samples -= N * esp
        samples /= np.sqrt(var*N)
        # On fait un histogramme de (Xbarre - N E[X]) / sqrt(N Var(X))
        hist,_ = np.histogram(samples, bins=bins)
        hist = np.asarray(hist, dtype=np.float64)
        hist = hist / N
        # On compare visuellement l'histogramme à celui d'une gaussienne
        axarr[k, 0].plot(bins[:-1], hist, 'b', bins[:-1], gauss_histogram, 'r')
        axarr[k, 0].set_title(str(name) + "(" + str(N) + ")")
        # Idem pour les fonctions de répartitions
        axarr[k, 1].plot(bins[:-1], np.cumsum(hist), 'b', bins[:-1], np.cumsum(gauss_histogram), 'r')
        axarr[k, 1].set_title(str(name) + "(" + str(N) + ")")
        print (name, " : (", N, ") ", np.linalg.norm(hist-gauss_histogram), np.linalg.norm(hist-gauss_histogram, np.inf), np.linalg.norm(np.cumsum(hist)-np.cumsum(gauss_histogram)), np.linalg.norm(np.cumsum(hist)-np.cumsum(gauss_histogram), np.inf))
plt.show()

def est_esperance_phi(samp, N, phi):
    samples = samp(N)
    phi_samples = [phi(s) for s in samples]
    return np.mean(phi_samples)

def est_esperance_phi_with_subtract(samp, N, phi, sb):
    samples = samp(N)-sb
    phi_samples = [phi(s) for s in samples]
    return np.mean(phi_samples)

def cdf_normal(x):
    return norm.cdf(x)

def cdf_normal49(x):
    return norm.cdf(x, loc = 4., scale=3.)

def cdf_ber13(x):
    return binom.cdf(x, 1, 1./3.)

def cdf_bin514(x):
    return binom.cdf(x, 5, 0.25)

def cdf_exp2(x):
    return expon.cdf(x, scale=0.5)

def cdf_poi2(x):
    return poisson.cdf(x, 2)

N = 10000

for (samp, cdf, name, esp, var) in [(sample_ber13, cdf_ber13, "Ber(1/3)", 1./3., 2./9.),
                                    (sample_bin514, cdf_bin514, "Bin(5, 1/4)", 1.25, 5.*3./16),
                                    (sample_exp2, cdf_exp2, "exp(2)", 0.5, 0.25),
                                    (sample_poi2, cdf_poi2, "Poi(2)", 2., 2.)]:
    for a in [0.1, 0.3, 0.5, 0.8, 1., 2., 5.]:
        # Markov
        phi = lambda x: x
        print ("phi(x)=x", name, "a=",a, "P(X>a)=", 1.-cdf(a), "E(phi(X))/phi(a)=",est_esperance_phi(samp, N, phi)/phi(a))
        phi = lambda x: x*x
        print ("phi(x)=x^2", name, "a=",a, "P(X>a)=", 1.-cdf(a), "E(phi(X))/phi(a)=",est_esperance_phi(samp, N, phi)/phi(a))
        # Tchebychev
        # P(|X-E[X]|>a) = P(X-E[X]>a) + P(X-E[X]<-a) = P(X>a+E[X]) + P(X<E[X]-a)
        phi = lambda x: x*x
        print ("phi(x)=x^2", name, "a=",a, "P(|X-E[X]|>a)=", 1.-cdf(a+esp) + cdf(esp-a), "Var(X)/a^2=",est_esperance_phi_with_subtract(samp, N, phi, esp)/phi(a))
        phi = lambda x: x*x*x*x
        print ("phi(x)=x^4", name, "a=",a, "P(|X-E[X]|>a)=", 1.-cdf(a+esp) + cdf(esp-a), "E((X-E[X])^4)/a^4=",est_esperance_phi_with_subtract(samp, N, phi, esp)/phi(a))

# On ne fait pas Markov pour ces lois car non positives
for (samp, cdf, name, esp, var) in [(sample_normal, cdf_normal, "N(0, 1)", 0., 1.),
                                    (sample_normal49, cdf_normal49, "N(4, 9)", 4., 9.)]:
    for a in [0.1, 0.3, 0.5, 0.8, 1., 2., 5.]:

        # Tchebychev
        # P(|X-E[X]|>a) = P(X-E[X]>a) + P(X-E[X]<-a) = P(X>a+E[X]) + P(X<E[X]-a)
        phi = lambda x: x*x
        print ("phi(x)=x^2", name, "a=",a, "P(|X-E[X]|>a)=", 1.-cdf(a+esp) + cdf(esp-a), "Var(X)/a^2=",est_esperance_phi_with_subtract(samp, N, phi, esp)/phi(a))
        phi = lambda x: x*x*x*x
        print ("phi(x)=x^4", name, "a=",a, "P(|X-E[X]|>a)=", 1.-cdf(a+esp) + cdf(esp-a), "E((X-E[X])^4)/a^4=",est_esperance_phi_with_subtract(samp, N, phi, esp)/phi(a))

def sample_binn13(n, N):
    return np.random.binomial(n, 1./3., size=N)

N = 1000000

print ("\t\t\t\tEstimated\tHoeffding\tBernstein\tBennett\n")

for n in [1, 10, 100, 1000, 10000]:
    samples = sample_binn13(n, N)
    abs_samples_minus_mean = np.abs(samples - n /3.)
    variance = n * 2./9.
    for t in [0, 0.1, 0.5, 0.9, 3., 9., 50., 80.]:
        estimated_proba = np.sum(abs_samples_minus_mean >= t)/((float)(N))
        hoeffding = 2. * np.exp(-2. * t * t / n)
        b = 1. # On peut affiner la borne
        bernstein = 2 * np.exp(-t*t / ( 2 * (variance + t * b)))
        b = 2./3.
        x = t * b / variance
        bennett = 2 * np.exp(-variance/(b*b) * ((1. + x) * np.log(1 + x) - x))
        print ("n: " + str(n) + ("\t" if (n < 1000) else "") + " \tt: " + str(t) + "\t\t" +  ("%.9f" % estimated_proba) + "\t" + ("%.9f" % hoeffding) + "\t" + ("%.9f" % bernstein) + "\t" + ("%.9f" % bennett))
    print ("")

def sample_binnp(n, p, N):
    return np.random.binomial(n, p, size=N)

print ("\t\t\t\t\tEstimated\tHoeffding\tEx3\t\tEx4\n")

for p in [0.5, 0.7, 0.9]:
    for n in [1, 10, 100, 1000, 10000]:
        samples = sample_binnp(n, p, N)
        samples_minus_mean = samples - n * p
        variance = n * p * (1.-p)
        for t in [0, 0.1, 0.5, 0.9, 3., 9., 50., 80.]:
            estimated_proba = np.sum(samples_minus_mean >= t)/((float)(N))
            hoeffding = np.exp(-2. * t * t / n)
            ex3 = np.exp(-(t*t/n) / (2. * p * (1. - p)))
            ex4 = 1.
            if (t/(n*p) <= 1):
                ex4 = np.exp(-t * t/(3*n*p))
            print ("p:" + str(p) + "\t" + "n: " + str(n) + ("\t" if (n < 1000) else "") + " \tt: " + str(t) + "\t\t" +  ("%.9f" % estimated_proba) + "\t" + ("%.9f" % hoeffding) + "\t" + ("%.9f" % ex3) + "\t" + ("%.9f" % ex4))
        print ("")
