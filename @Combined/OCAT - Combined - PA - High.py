import sys, os

droppedPath = sys.argv[1].rsplit("\\", 1)[0]+"\\"

scriptPath=sys.argv[0].rsplit("\\", 1)[0]

def listclean (list):
	return str(list).replace("[", "").replace("]", "").replace("\'", "\"").replace(", ", ",\n").replace(".csv", "");

def CSVlistR (GPU, API, QUA, CSVlist):
	if API == "NA":
		API = ""
	return str("\
GPU = \"" + GPU + "\"\n\
CSV = c(\n"\
	+ listclean(CSVlist) + \
"\n)\n\
CSV = paste0(CSV, \".csv\")\n\
OCATcomb = READ(\"\", \"" + QUA + "\", \"" + API + "\")\n"
);

def blankAPI (list):
	newlist = []
	for file in list:
		file.append(file[2])
		file[2] = file[1]
		file[1] = "NA"
		print(file)
		newlist.append(file)
		# print(file[0:4])
	return newlist;

# os.chdir("E:\\Users\\Jim\\My Documents\\OCC\\@Reviews\\@Performance Analyses\\Shadow of the Tomb Raider Performance Analysis\\OCAT Data")

listfile = []

for paths, folders, files in os.walk(droppedPath):
	for file in files:
		if file.startswith("OCAT-"):
			listfile.append(str(paths).replace(droppedPath, "") + "\\" + str(file))
#	produces a list of all OCAT CSVs with the directory information

listsplit = [file.split("\\") for file in listfile]

listmap = []
for line in listsplit:
	listmapL = [line[0], "", "", ""]
	for i in range(1, len(line)):
		listmapL[len(listmapL)-i] = line[len(line)-i]
	listmap.append(listmapL)
#	this will map the values I need to the appropriate locations in a list
#		[GPU, API, Quality, File]
#	if no API change is made, the element will be blank

GPUs, APIs, QUAs = [], [], []

for item in listmap:
	GPUs.append(item[0])
	APIs.append(item[1])
	QUAs.append(item[2])
APIs = list(set(APIs))

GPUs = list(set(GPUs))
GPUs = [\
'RX 580',\
'RX Vega 64',\
'GTX 770',\
'GTX 980',\
'GTX 1070',\
'GTX 1080',\
'RTX 2060',\
'RTX 2080']
QUAs = list(set(QUAs))
QUAs = ["High"]

GROUPS = []

for GPU in GPUs:
	GROUPS.append([GPU, APIs])

grouped = []

for gGPU, gAPI in GROUPS:
	for API in gAPI:
		for QUA in QUAs:
			temp = []
			filelist = []
			for file in listmap:
				if file[0] == gGPU and file[1] == API and file[2] == QUA:
					filelist.append(file[3])
				temp.append(filelist)
			grouped.append([gGPU, [API, [QUA, filelist]]])

out = ""

for file in grouped:
	if file[1][1][1] != []:
		for GPU in GPUs:
			for API in APIs:
				for QUA in QUAs:
					if file[0] == GPU and file[1][0] == API and file[1][1][0] == QUA:
						out = out + "\n" + CSVlistR(GPU, API, QUA, file[1][1][1])

for place in droppedPath.split("\\"):
	if " Performance Analysis" in place:
		droppedGame = place.split(" Performance Analysis")[0]


scriptType = "OCAT"
scriptName = "Combined - PA"
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"

outputName = scriptName + " - " + droppedGame + ".r"
outputFull = droppedPath + "@" + outputName

RPath = droppedPath.replace("\\", "/")


if not os.path.exists(outputFull):
	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
		for line in fref:
			fout.write(line.replace("!PATH!", RPath).replace("!GAME!", droppedGame).replace("!LONG!", out))
		fout.close()


scriptType = "OCAT"
scriptName = "Combined Processing - PA"
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"

outputName = scriptName + " - " + droppedGame + ".r"
outputFull = droppedPath + "@" + outputName

if not os.path.exists(outputFull):
	print(outputFull)
	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
		for line in fref:
			fout.write(line.replace("!PATH!", RPath).replace("!GAME!", droppedGame).replace("!API!", listclean(APIs)))
		fout.close()

# os.system("pause")