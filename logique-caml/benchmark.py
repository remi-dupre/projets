from os import listdir, system
from time import time

test_cases = ["tests/100-ok/" + f for f in listdir("tests/100-ok")][::10]
modes = ["random", "max-pres", "ok"]

for mode in modes :
	t_begin = time()
	for f in test_cases :
		system("cat " + f + "|./satsol " + mode + " > /dev/null")
	print("-> ", mode, ": ", (time() - t_begin)/len(test_cases), "s")

