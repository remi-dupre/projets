from os import listdir, system
from time import clock

test_cases = ["tests/100-ok/" + f for f in listdir("tests/100-ok")][::10]
modes = ["random", "max-pres"]

for mode in modes :
	t_begin = clock()
	for f in test_cases :
		system("cat " + f + "|./satsol " + mode + " > /dev/null")
	print("-> ", mode, ": ", (clock() - t_begin)/len(test_cases), "s")

