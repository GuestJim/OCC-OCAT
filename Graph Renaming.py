import sys, os, shutil

droppedPath = sys.argv[1].rsplit("\\", 1)[0]+"\\"
droppedGPU = droppedPath.rsplit("\\",3)[1]

os.chdir(droppedPath)

if "Locations.txt" in os.listdir(droppedPath):
	LOCs = open(droppedPath + "Locations.txt", 'r').readlines()
	LOCs = [line.rstrip('\n') for line in LOCs]
else:
	loc = ["Recording "] * countCSV
	for i in range(countCSV):
		loc[i] = loc[i] + str(i+1)

GPUs = [\
'RX 580',\
'RX Vega 64',\
'GTX 770',\
'GTX 980',\
'GTX 1070',\
'GTX 1080',\
'RTX 2060',\
'RTX 2080']

DATAs = [\
'Frame',\
'Display Time']

TYPEs = [\
'Course',\
'Rate',\
'Freq',\
'QQ',\
'Diff',\
'Averages']

def numFind	(filename, list):
	for i in range(len(list)):
		if list[i] in filename:
			return(i+1)
	return(0)

def numGen (filename, GPU=droppedGPU):
	if GPU not in GPUs:
		gpu = 0
	else:
		gpu	=	numFind(droppedGPU, GPUs)
	loc		=	numFind(filename, LOCs)
	data	=	numFind(filename, DATAs)
	type	=	numFind(filename, TYPEs)
	
	if gpu == 0:
		return('%02d'%loc + '%02d'%data + '%02d'%type)
	else:
		return('%02d'%gpu + '%02d'%loc + '%02d'%data + '%02d'%type)

if not os.path.exists("Graphs"):
	os.mkdir("Graphs")

for file in os.listdir(droppedPath):
	if file.endswith(".png"):
		# print(numGen(file) + " -- " + file)
		shutil.copyfile(file, "Graphs\\" + numGen(file) + ".png")

# os.system("pause")