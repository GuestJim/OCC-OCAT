import sys, os, shutil
#	loads the sys, os, and shutil modules for Python to use

droppedPath	=	sys.argv[1].rsplit("\\", 1)[0] + "\\"
#	sys.argv is a list of the arguments passed to the script
#		the second and subsequent items are the paths of files dropped onto the script
#	rsplit is a command to split a string at the designated pattern, optionally into a specified number of pieces
#		it is used here to remove the file name of what was dropped onto the script

droppedGPU	=	droppedPath.split("OCAT Data")[1].split("\\")[1]
droppedOCAT	=	droppedPath.split("OCAT Data")[0] + "OCAT Data\\"
#	by splitting the path at OCAT Data, we can get the current GPU and the path to OCAT Data

os.chdir(droppedPath)
#	changes the current directory to droppedPath location

Z	=	1
#	zero-padding width

if	"Locations.txt"	in	os.listdir(droppedOCAT):
	LOCs	=	open(droppedOCAT + "Locations.txt", 'r').readlines()
	LOCs	=	[line.rstrip('\n') for line in LOCs]
#	if a Locations.txt file exists, it is opened and its contents read in as a list

if	"APIs.txt"		in	os.listdir(droppedOCAT):
	APIs	=	open(droppedOCAT + "APIs.txt", 'r').readlines()
	APIs	=	[line.rstrip('\n') for line in APIs]
	APIs	=	[API + " -" for API in APIs]
#		this last bit makes it check for the hyphen that separates the parts of the filename
else:
	APIs	=	[""]
#	if a APIs.txt file exists, it is opened and its contents read in as a list
#	if a APIs.txt file does not exist, the list is an empty string


if	"Qualities.txt"	in	os.listdir(droppedOCAT):
	QUAs	=	open(droppedOCAT + "Qualities.txt", 'r').readlines()
	QUAs	=	[line.rstrip('\n') for line in QUAs]
else:
	QUAs =	[\
		"Minimum Acceptable",\
		"High",\
		"Max"]
#	if a Qualities.txt file exists, it is opened and its contents read in as a list
#	if a Qualities.txt file does not exists, a default list is created

GPUs	=	[\
'RX 580',\
'RX Vega 64',\
'GTX 770',\
'GTX 980',\
'GTX 1070',\
'GTX 1080',\
'RTX 2060',\
'RTX 2080']
#	the list of GPUs, in order

DATAs	=	[\
'Frame',\
'Display',\
'Rend']
#	list of data types

TYPEs	=	[\
'Means',\
'Course',\
'Freq',\
'QQ',\
'Diff']
#	list of graph types

def numFind	(filename, list):
	if list == [""]:
		return(0)
	for i in range(len(list)):
		if list[i] in filename:
			return(i+1)
	return(0)
#	function for finding which item from a list is present in the file name and returning the index
#	if the list is empty or no element in the list is found in the file name, 0 is returned

def numGen (filename, GPU = droppedGPU):
	if GPU not in GPUs:
		gpu	=	""
	else:
		gpu	=	numFind(droppedGPU, GPUs)
#		checks if the GPU is in ther list of GPUs, then finds the index for that GPU
	if APIs == [""]:
		api	=	""
	else:
		api		=	numFind(filename, APIs)	
#		checks if the API list has contents, then finds the API in the file name and gets its list index
	loc		=	numFind(filename, LOCs)
	qua		=	numFind(filename, QUAs)
	data	=	numFind(filename, DATAs)
	type	=	numFind(filename, TYPEs)
#		gets the index numbers for the location, quality, data type, and graph type
	
	code	=	""
#		creates an empty string to hold the code number
	for x in [gpu, api, qua, loc, data, type]:
#		loop to go through the numbers found above
		if x != "":
#			catches GPU not being in list, so a code number is not added
			code	=	code + str(x).zfill(Z)
#				concatenates onto the code name the current number, with some amount of zero padded
	
	return(code)
#	generates the code number for the graph based on the file name

if not os.path.exists("Graphs"):
	os.mkdir("Graphs")
#	if a Graphs folder does not already exists, it makes it

for file in os.listdir(droppedPath):
	if file.endswith(".png"):
#		loop going through droppedPath, checking just for the PNG files
		shutil.copyfile(file, "Graphs\\" + numGen(file) + ".png")
#			copies the PNG file found to the Graphs folder with the code name

# os.system("pause")