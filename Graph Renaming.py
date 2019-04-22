import sys, os, shutil

droppedPath = sys.argv[1].rsplit("\\", 1)[0]+"\\"
for place in range(len(droppedPath.split("\\"))):
	if 'OCAT Data' == droppedPath.split("\\")[place]:
		droppedGPU = droppedPath.split("\\")[place+1]
		droppedOCAT = droppedPath.rsplit("\\",len(droppedPath.split("\\")) - place-1)[0]+"\\"

os.chdir(droppedPath)

Z = 1
#	zero-padding width

if "Locations.txt" in os.listdir(droppedOCAT):
	LOCs = open(droppedOCAT + "Locations.txt", 'r').readlines()
	LOCs = [line.rstrip('\n') for line in LOCs]
else:
	LOCs = ["Recording "] * countCSV
	for i in range(countCSV):
		LOCs[i] = LOCs[i] + str(i+1)

if "APIs.txt" in os.listdir(droppedOCAT):
	APIs = open(droppedOCAT + "APIs.txt", 'r').readlines()
	APIs = [line.rstrip('\n') for line in APIs]
else:
	APIs = [""]

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
	if list == [""]:
		return(0)
	for i in range(len(list)):
		if list[i] in filename:
			return(i+1)
	return(0)

def numGen (filename, GPU=droppedGPU):
	if GPU not in GPUs:
		gpu = ""
	else:
		gpu	=	numFind(droppedGPU, GPUs)
	if APIs == [""]:
		api = 0
	else:
		api		=	numFind(filename, APIs)	
	loc		=	numFind(filename, LOCs)
	data	=	numFind(filename, DATAs)
	type	=	numFind(filename, TYPEs)
	
	code = ""
	for x in [gpu, api, loc, data, type]:
		if x != "":
			code = code + '%0*d'%(Z,x)
	
	return(code)

if not os.path.exists("Graphs"):
	os.mkdir("Graphs")

for file in os.listdir(droppedPath):
	if file.endswith(".png"):
		# print(numGen(file) + " -- " + file)
		shutil.copyfile(file, "Graphs\\" + numGen(file) + ".png")

# os.system("pause")