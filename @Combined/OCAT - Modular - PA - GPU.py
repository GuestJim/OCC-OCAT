import sys, os, shutil
#	loads different modules for Python

droppedPath = sys.argv[1].rsplit("\\", 1)[0]+"\\"
#	the path to the files dropped onto the Python script
scriptPath=sys.argv[0].rsplit("\\", 1)[0]
#	the path to the Python and reference R scripts

def listclean (list):
	return str(list).replace("[", "").replace("]", "").replace("\'", "\"").replace(", ", ",\n").replace(".csv", "");
#	 a custom function that will take a list and generate a string with line breaks and commas between elements

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
#	produces the R code that will take a list of CSV files and combine them into a data frame to eventually be written to a new @Combined CSV

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
#	unsure what this does as it is apparently not used, but do not want to remove it yet

RelPath = ""
#	creates the variable for the Relative Path

for Fname in droppedPath.split("\\"):
	RelPath = RelPath + Fname + "\\"
	if "OCAT Data" in Fname:
		break
#	finds the relative path to the OCAT Data folder, so it can be removed from the path information
#		it works by appending elements of the droppedPath variable to the variable until it gets to where it needs to stop

listfile = []
#	creates the variable

for paths, folders, files in os.walk(droppedPath):
	countCSV = 0
	for file in files:
		if file.startswith("OCAT-"):
			listfile.append((str(paths).replace(RelPath, "") + "\\" + str(file)).replace("\\\\", "\\"))
			countCSV = countCSV + 1
#	produces a list of all OCAT CSVs with the directory information
#		in some scenarios the slashes for the directories were not present, but this will now make sure to add them, and remove duplicates if necessary
#	it also counts the number of CSVs within a single folder for use with setting names for each run
#		it does assume it wil be the same number of CSVs for each scenario

listsplit = [file.split("\\") for file in listfile]
#	takes the list of CSVs and separates them by the \ symbol between folders

listmap = []
#	creates the variable

for line in listsplit:
	listmapL = [line[0], "", "", ""]
#	the first element of the path withouth the RelPath portion is the GPU, so its value can always be placed there
	for i in range(1, len(line)):
		listmapL[len(listmapL)-i] = line[len(line)-i]
#		it mean seem odd, but using -i is necessary, as working backwards will keep the API position empty if it is not present
	listmap.append(listmapL)
#	this will map the values I need to the appropriate locations in a list
#		[GPU, API, Quality, File]
#	if no API change is made, the element will be blank

GPUs, APIs, QUAs = [], [], []
#	creates multiple variables

for item in listmap:
	GPUs.append(item[0])
	APIs.append(item[1])
	QUAs.append(item[2])
#	generates lists for the GPU, API, and quality from the mapped information from above

GPUs = [\
'RX 580',\
'RX Vega 64',\
'GTX 770',\
'GTX 980',\
'GTX 1070',\
'GTX 1080',\
'RTX 2060',\
'RTX 2080']
#	I know the GPU so I set it here manually anyway
GPUs = list(set(GPUs))
APIs = list(set(APIs))
QUAs = list(set(QUAs))
#	removes duplicates from the lists created earlier and makes them into lists for these variables

GROUPS = []
#	creates the variable

for GPU in GPUs:
	GROUPS.append([GPU, APIs])
#	for each GPU in the GPUs list it creates a multi-level list of the GPU and the API

grouped = []
#	creates the variable

for gGPU, gAPI in GROUPS:
#	will go through the GPU and API groups in the GROUPS variable
	for API in gAPI:
#		starts by going through the APIs in the GROUPS variable
		for QUA in QUAs:
#			works through the list of qualities
			temp = []
			filelist = []
			for file in listmap:
				if file[0] == gGPU and file[1] == API and file[2] == QUA:
#					this will check if the combination of GPU, API, and Quality exists before doing anything
					filelist.append(file[3])
#						adds the file name to the grouped list of files
				temp.append(filelist)
#					this is the lsit fo the found file names for each group
			grouped.append([gGPU, [API, [QUA, filelist]]])
#				actually builds the multi-level grouped list that sorts down what each CSV is for

out = ""
#	creates the variable

for file in grouped:
#	works through each file in the grouped list
	if file[1][1][1] != []:
#		checks to make sure there is something there to work with
		for GPU in GPUs:
			for API in APIs:
				for QUA in QUAs:
#					works down through the GPU, APIs run on the GPU, and the Qualities run each API for each GPU
					if file[0] == GPU and file[1][0] == API and file[1][1][0] == QUA:
#						checks to make sure we are looking at the current combination of GPU, API, and Quality we are interested in
						out = out + "\n" + CSVlistR(GPU, API, QUA, file[1][1][1])
#							generates the R code for importing each CSV and creating a single @Combined CSV and saves it as a string

if " Performance Analysis" in droppedPath:
	for i in range(len(droppedPath.split("\\"))):
		if " Performance Analysis" in droppedPath.split("\\")[i]:
			droppedGame = droppedPath.split("\\")[i].replace(" Performance Analysis", "")
			droppedGPU = droppedPath.split("\\")[i+2]
#				finds the game name and the GPU, as this is the script to generate the files for each GPU

if "Locations.txt" in os.listdir(RelPath):
	loc = open(RelPath + "Locations.txt", 'r').readlines()
	loc = [line.rstrip('\n') for line in loc]
#		checks for a Locations.txt file so it can use its content for labelling the runs
else:
	loc = ["Recording "] * countCSV
	for i in range(countCSV):
		loc[i] = loc[i] + str(i+1)
#		if no Locations.txt file is found, this will generate generic Recording # labels

locStr = listclean(loc)
#	creates a string version of the Locations list

scriptType = "OCAT"
scriptName = "Combined - PA"
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"
#	sets which script to use and where to find it

outputName = scriptName + " - GPU - " + droppedGame + ".r"
outputFull = droppedPath + "@" + outputName
#	sets the desired name for the script that will be produced

RPath = droppedPath.replace("\\", "/")
#	creates the R-formatted version of the path for the file dropped onto the script

if not os.path.exists(outputFull):
	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
		for line in fref:
			fout.write(line.replace("!PATH!", RPath).replace("!GAME!", droppedGame).replace("!GPU!", droppedGPU).replace("!LONG!", out).replace("!QUA!", QUAs[0]).replace("!LOC!", locStr))
		fout.close()
#	reads the reference file and goes line by line, replacing certain text as desired

scriptType = "OCAT"
scriptName = "Combined - Input"
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"
#	sets which script to use and where to find it

outputName = scriptName + " - GPU - " + droppedGame + ".r"
outputFull = droppedPath + "@" + outputName
#	sets the desired name for the script that will be produced

if not os.path.exists(outputFull):
	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
		for line in fref:
			fout.write(line.replace("!PATH!", RPath).replace("!GAME!", droppedGame).replace("!GPU!", droppedGPU).replace("!API!", listclean(APIs)).replace("!QUA!", QUAs[0]).replace("!TYPE!", "GPU").replace("!LOC!", locStr))
		fout.close()
#	reads the reference file and goes line by line, replacing certain text as desired

for file in os.listdir(droppedPath):
	if file.endswith(".r") and file.startswith("Processing"):
		fileText = open(droppedPath + file,'r').read()

		fout = open(droppedPath + file, 'w')

		fileText = fileText.replace("!LOC!", locStr)

		fout.write(fileText)
		fout.close()
#	goes through each R file in the droppedPath that starts with Processing and will stick the Locations list information in the !LOC! position
#		this works be reading the entire file into a variable and then replacing the text, instead of going line by line
#		it also edits the file instead of working from a reference

if not os.path.exists(droppedPath + "OCAT - Combined - Output - GPU.r"):
	shutil.copyfile(scriptPath + "\\OCAT - Combined - Output - GPU.r", droppedPath + "@Combined - Output - GPU.r")
#	copies the reference R script for processing the Combined data to the droppedPath

if not os.path.exists(droppedPath + "OCAT - Processing - Output.r"):
	shutil.copyfile(scriptPath.rsplit("\\", 1)[0] + "\\OCAT - Processing - Output.r", droppedPath + "OCAT - Processing - Output.r")
#	copies the reference R script for processing the individual data runs to the droppedPath
	
# os.system("pause")
