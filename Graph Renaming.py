import sys, os, shutil
#	loads different modules for Python

droppedPath = sys.argv[1].rsplit("\\", 1)[0]+"\\"
#	gets the path to the file dropped onto the script
for place in range(len(droppedPath.split("\\"))):
	if 'OCAT Data' == droppedPath.split("\\")[place]:
		droppedGPU = droppedPath.split("\\")[place+1]
		droppedOCAT = droppedPath.rsplit("\\",len(droppedPath.split("\\")) - place-1)[0]+"\\"
#	works through the path to identify the current GPU and the OCAT Data folder that holds all of the folders
#		to be clear the folder structure is:
#			GAME Performance Analysis\OCAT Data\GPU\API\Quality
#		the API is not always present

os.chdir(droppedPath)
#	 it is necessary to set the current working directory to the droppedPath for later stuff to work

Z = 1
#	zero-padding width
#	though I suspect for my uses this can remain 1, which means they will not be any zero padding, but still want this, just in case

if "Locations.txt" in os.listdir(droppedOCAT):
	LOCs = open(droppedOCAT + "Locations.txt", 'r').readlines()
	LOCs = [line.rstrip('\n') for line in LOCs]
#	this checks for if there is a Locations.txt file in the OCAT Data folder
#	the Locations.txt file is used by this and other scripts to identify the locations for the tests, allowing me to not have to manually type it into each script
else:
	LOCs = ["Recording "] * countCSV
	for i in range(countCSV):
		LOCs[i] = LOCs[i] + str(i+1)
#	if there is no Location.txt file, it will use the generic Recording # label

if "APIs.txt" in os.listdir(droppedOCAT):
	APIs = open(droppedOCAT + "APIs.txt", 'r').readlines()
	APIs = [line.rstrip('\n') for line in APIs]
#	this checks if there is an APIs.txt file in the OCAT Data folder
#	the APIs.txt file is used by this and other scripts to identify the API for the tests, if multiple were used, allowing me to not have to manually type it into each script
else:
	APIs = [""]
#	if there is no APIs.txt script, it assumes the API never changed


if "Qualities.txt" in os.listdir(droppedOCAT):
	QUAs = open(droppedOCAT + "Qualities.txt", 'r').readlines()
	QUAs = [line.rstrip('\n') for line in QUAs]
#	this checks if there is a Qualities.txt file in the OCAT Data folder
#	only this script uses this file to identify what quality options may have been used
else:
	QUAs =	[\
		"Minimum Acceptable",\
		"High",\
		"Max"]
#	if no Qualities.txt file is present, it uses a default list that will cover most circumstances for my testing

GPUs = [\
'RX 580',\
'RX Vega 64',\
'GTX 770',\
'GTX 980',\
'GTX 1070',\
'GTX 1080',\
'RTX 2060',\
'RTX 2080']
#	ordered list of the GPUs I test with

DATAs = [\
'Frame',\
'Display Time']
#	I will use frame time/rate and display time data based graphs, and this list is necessary for distinguishing between the graphs

TYPEs = [\
'Course',\
'Rate',\
'Freq',\
'QQ',\
'Diff',\
'Averages']
#	the different types of graphs

def numFind	(filename, list):
	if list == [""]:
		return(0)
	for i in range(len(list)):
		if list[i] in filename:
			return(i+1)
	return(0)
#	will search a file name for the occurence of items on the lists above, and then return the number for the position in the list
#		if no item from the list is found, 0 is returned.

def numGen (filename, GPU=droppedGPU):
	if GPU not in GPUs:
		gpu = ""
#	not yet tested, but should allow this to work with Reviews, where GPU is not specified
#	when the GPU is not found in the list of GPUs, then it will be an empty value and not be in the code
	else:
		gpu	=	numFind(droppedGPU, GPUs)
#	if the GPU is in the list of GPUs, then it will find and return the number for the list
	if APIs == [""]:
		api = ""
#	if the list of APIs is empty, then the API will be an empty value and not be int he code
	else:
		api		=	numFind(filename, APIs)	
#	if there is a list of APIs, it will find and return the number for the list
	loc		=	numFind(filename, LOCs)
	qua		=	numFind(filename, QUAs)
	data	=	numFind(filename, DATAs)
	type	=	numFind(filename, TYPEs)
#	these will read the filename and return the code number for the appropriate piece of information
	
	code = ""
#	just creating an empty variable to hold the code information
	for x in [gpu, api, qua, loc, data, type]:
#	the for loop will work through each code numbers found above in the order GPU, API, Quality, Location, Data type, Graph type
		if x != "":
#		if the code number is empty, this will skip it
			code = code + '%0*d'%(Z,x)
#			appends the code string with the code number that is padded to Z characters
	
	return(code)
#	returns the code name

if not os.path.exists("Graphs"):
	os.mkdir("Graphs")
#	checks if there is a folder to hold the graphs and creates it if necessary

for file in os.listdir(droppedPath):
#	works through the files in same folder as the file dropped onto the list
	if file.endswith(".png"):
#		checks if the files are PNGs, which is how I save the graphs
		# print(numGen(file) + " -- " + file)
#		was just here for troubleshooting and I am leaving it
		shutil.copyfile(file, "Graphs\\" + numGen(file) + ".png")
#		copies the original graph PNG to the Graphs folder with the new code name for its filename

# os.system("pause")