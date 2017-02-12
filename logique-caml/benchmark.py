# Bon : vu la tête de ce fichier, j'aurais peut être mieux fait de faire un bash, mais au moins python je connais

from os import listdir, system
from time import time

test_cases = ["tests/100-no/" + f for f in listdir("tests/100-no")][::20]
#test_cases = ["bench/picross/" + f for f in listdir("bench/picross")]
modes = ["random", "max-pres", "up"]

for mode in modes :
	t_begin = time()
	for f in test_cases :
		system("cat " + f + "|./satsol solve " + mode) + " > /dev/null")
	print("-> ", mode, ": ", (time() - t_begin)/len(test_cases), "s")

