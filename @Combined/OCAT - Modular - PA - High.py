import sys, os, shutil

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

RelPath = ""

for Fname in droppedPath.split("\\"):
	RelPath = RelPath + Fname + "\\"
	if "OCAT Data" in Fname:
		break

listfile = []

for paths, folders, files in os.walk(droppedPath):
	countCSV = 0
	for file in files:
		if file.startswith("OCAT-"):
			listfile.append((str(paths).replace(RelPath, "") + "\\" + str(file)).replace("\\\\", "\\"))
			countCSV = countCSV + 1
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

if "APIs.txt" in os.listdir(RelPath):
	APIs = open(RelPath + "APIs.txt", 'r').readlines()
	APIs = [line.rstrip('\n') for line in APIs]
else:
	APIs = list(set(APIs))

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

if "Locations.txt" in os.listdir(RelPath):
	loc = open(RelPath + "Locations.txt", 'r').readlines()
	loc = [line.rstrip('\n') for line in loc]
else:
	loc = ["Recording "] * countCSV
	for i in range(countCSV):
		loc[i] = loc[i] + str(i+1)

locStr = listclean(loc)

scriptType = "OCAT"
scriptName = "Combined - PA"
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"

outputName = scriptName + " - " + droppedGame + ".r"
outputFull = droppedPath + "@" + outputName

RPath = droppedPath.replace("\\", "/")

if not os.path.exists(outputFull):
	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
		for line in fref:
			fout.write(line.replace("!PATH!", RPath).replace("!GAME!", droppedGame).replace("!LONG!", out).replace("!QUA!", QUAs[0]).replace("!LOC!", locStr))
		fout.close()


scriptType = "OCAT"
scriptName = "Combined - Input"
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"

outputName = scriptName + " - " + droppedGame + ".r"
outputFull = droppedPath + "@" + outputName

if not os.path.exists(outputFull):
	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
		for line in fref:
			fout.write(line.replace("!PATH!", RPath).replace("!GAME!", droppedGame).replace("!API!", listclean(APIs)).replace("!QUA!", QUAs[0]).replace("!TYPE!", "PA").replace("!LOC!", locStr))
		fout.close()

print(outputFull)
os.system("pause")

for file in os.listdir(droppedPath):
	if file.endswith(".r") and file.startswith("Processing"):
		fileText = open(droppedPath + file,'r').read()

		fout = open(droppedPath + file, 'w')

		fileText = fileText.replace("!LOC!", locStr)

		fout.write(fileText)
		fout.close()

if not os.path.exists(droppedPath + "OCAT - Combined - Output - High.r"):
	shutil.copyfile(scriptPath + "\\OCAT - Combined - Output - High.r", droppedPath + "@Combined - Output - High.r")

# os.system("pause")
