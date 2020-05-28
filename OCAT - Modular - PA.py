import sys, os, shutil
#	loads the sys, os, and shutil modules for Python to use

droppedPath	=	sys.argv[1].rsplit("\\", 1)[0] + "\\"

scriptPath	=	sys.argv[0].rsplit("\\", 1)[0] + "\\"
#	sys.argv is a list of the arguments passed to the script
#		the first item in the list is the path to the script itself
#		the second and subsequent items are the paths of files dropped onto the script
#	rsplit is a command to split a string at the designated pattern, optionally into a specified number of pieces
#		it is used here to remove the file names, getting just their paths (and the slashes are added back for droppedPath, as they are needed later)

if "OCAT Data" in droppedPath.rsplit("\\", 4)[2:4] and "Review" not in droppedPath.rsplit("\\", 3)[1]:
	TYPE	=	"HIGH"
else:
	TYPE	=	"GPU"
#	this switch is to determine if I am trying to work on multi-GPU, High Quality data, single GPU data, or single-GPU, multi-API (High Quality) data
#	instead of requiring a different path for the file that is dragged onto the script, this also checks if Review is in the appropriate folder name
#		this way the same folder structure can be used for Reviews and Performance Analyses and the same behavior for activating this script
#	by checking more folders in the structure, this can work with the single-GPU, multi-API scenario

def	listclean	(list):
	if list == ['']:
		return "NULL"
	return str(list).replace("[", "").replace("]", "").replace("\'", "\"").replace(", ", ",\n").replace(".csv", "");
#	creates a function that will take a Python list, make it a string, and then remove and convert the appropriate substrings to make it what I want in R

def	CSVlistR	(GPU, API, QUA, CSVlist):
	if API	==	"NA":
		API	=	""
	return str("\
GPU = \"" + GPU + "\"\n\
CSV = c(\n"\
	+ listclean(CSVlist) + \
"\n)\n\
CSV = paste0(CSV, \".csv\")\n\
OCATcomb = READ(\"\", \"" + QUA + "\", \"" + API + "\")\n"
);
#	creates a function that will generate the desired multi-line string that will be placed in the R script for creating the combined CSV
#	it looks like this:
#		GPU = "CURRENT GPU"
#		CSV = c(
#		"FILENAME1",
#		"FILENAME2",
#		"FILENAME3"
#		)
#		CSV = paste0(CSV, ".csv")
#		OCATcomb = READ("", "QUALITY", "API")

RelPath	=	droppedPath.split("OCAT Data")[0] + "OCAT Data\\"
#	splits the path at the OCAT Data folder, grabs the first portion, then adds the folder name back to the path

listfile	=	[]
#	creates an empty list for appending things too

for paths, folders, files in os.walk(droppedPath):
#	os.walk is a generator function, which means it does not itself produce an output but works with the for loop
#	os.walk will generate lists for three variables; the paths, folders, and file names
	for file in files:
#		for loop that goes through each file in the list of files
		if file.startswith("OCAT-"):
#			the OCAT CSVs all start with OCAT-, so this makes sure it is only these files that are used
			listfile.append((str(paths).replace(RelPath, "") + "\\" + str(file)).replace("\\\\", "\\"))
#				adds on to the listfile list path strings for each CSV file
#					removes the RelPath from the path strings, so the path is just from the GPU folder down
#	produces a list of all OCAT CSVs with the directory information

listsplit	=	[file.split("\\") for file in listfile]
#	creates a multi-dimensional list by spliting the path lists according to the folder names

listmap	=	[]
#	creates an empty list that will be used to map the information to the appropriate places
for line in listsplit:
#	loop to go through each line in the listsplit list
	listmapL	=	[line[0], "", "", ""]
#		listmap Line list variable created with four places
#			order is GPU, API, Quality, File name
#			first element of the listsplit lines is always the GPU
	for i in range(1, len(line)):
#		loop going through the places in the line
		listmapL[len(listmapL) - i]	=	line[len(line) - i]
#			this applies the value from the line to the listmapL variable
#			it goes backwards, and the reason is so that if the API value is not present, it will be left as an empty string
#			a key thing is that it uses the length of the line from listsplit, because when there is no API folder, the length is shorter, so the API place in listmap is untouched
#				it is also important that Python starts its indexing at 0, so by using a range starting at 1, it will not include the first element here
	listmap.append(listmapL)
#		adds the listmapL line to the listmap variable
	
#	this will map the values I need to the appropriate locations in a list
#		[GPU, API, Quality, File]
#	if no API change is made, the element will be blank

GPUs, APIs, QUAs	=	[], [], []
GPUsread	=	[]
#	create empty lists for storing the lists of relevant values

for item in listmap:
	GPUs.append(item[0])
	APIs.append(item[1])
	QUAs.append(item[2])
#	goes through the listmap variable above and builds the lists from its items

GPUsread	=	list(set(GPUs))
#	makes and saves a list of the GPUs read from the folder names
#		this is for use when collecting the data on one GPU and multiple APIs
if 		TYPE	==	"HIGH":
#	checks if the type is multi-GPU for High Quality results
	GPUs	=	[\
	'RX 580',\
	'RX Vega 64',\
	'GTX 770',\
	'GTX 980',\
	'GTX 1070',\
	'GTX 1080',\
	'RTX 2060',\
	'RTX 2080']
	QUAs	=	["High"]
#	instead of letting it pull from list map, both GPU and Quality lists are set
elif	TYPE	==	"GPU":
#	else if, so if the previous switch was false, it will check if this is true
	GPUs	=	list(set(GPUs))
	QUAs	=	list(set(QUAs))
#		create a list from the set of values in the GPU and Quality lists.

if "APIs.txt" in os.listdir(RelPath):
#	checks if an APIs.txt file exists in the OCAT Data folder
	APIs	=	open(RelPath + "APIs.txt", 'r').readlines()
	APIs	=	[line.rstrip('\n') for line in APIs]
#		if APIs.txt exists, it will read it in, strip out the line breaks, and make a list of the provided values
#			this is useful for controlling the order of the APIs later
else:
	APIs	=	list(set(APIs))
#	generates the list of APIs from what listmap items

grouped	=	[]
#	creates empty list for holding the information when grouped by GPU, API, Quality configuration
out		=	""
#	creates an empty string for holding the R commands to place in the script

for GPU in GPUs:
	for API in APIs:
		for QUA in QUAs:
#			loops to go through the configurations of GPU, API, and Quality
			filelist	=	[]
#				first creates and then wipes a variable that will hold the list of files for the current configuration
			for file in listmap:
#				loop to go through the CSV files in the folders
				if file[0] == GPU	and file[1] == API	and file[2] == QUA:
#					checks to make sure the file is of the current configuration
					filelist.append(file[3])
#						places the file onto the current file list
			if filelist != []:
#				sometimes there are configurations without any files to them, and this checks for that situation
				grouped.append([GPU, API, QUA, filelist])
#					adds the configuration and its list of files to the grouped variable
				countCSV	=	len(filelist)
#					creates a variable to store the number of CSV files for each configuration, which is the number of locations tested
				out	=	out + "\n" + CSVlistR(GPU, API, QUA, filelist)
#					adds onto the out string the R code necessary for making the combined CSV
#	technically grouped is not necessary, but I am keeping it as it may be a good object to check when troubleshooting

droppedGame	=	RelPath.rsplit("\\", 3)[1]	\
	.replace(" Performance Analysis", "")	\
	.replace(" Review", "")
#	always the folder above OCAT data is the name for the project
#	the replace functions then remove the article type from the name, if it is a performance analysis or review, getting just the game name
#		for miscellaneous artiles though, the article's name is appropriate to use

if	"Locations.txt" in os.listdir(RelPath):
	loc	=	open(RelPath + "Locations.txt", 'r').readlines()
	loc	=	[line.strip('\n') for line in loc]
else:
	loc	=	["Recording "] * countCSV
	for i in range(countCSV):
		loc[i]	=	loc[i] + str(i+1)
#	checks for a Locations.txt file and either reads in its content or generates a generic "Recording #" if there is no file
locStr	=	listclean(loc)
#	makes a cleaned up string of the Locations to be inserted into the appropriate R files

if	"Locations Short.txt" in os.listdir(RelPath):
	locsho		=	open(RelPath + "Locations Short.txt", 'r').readlines()
	locsho		=	[line.strip('\n') for line in locsho]
	locshoStr	=	listclean(locsho)
else:
	locshoStr	=	"NULL"
#	checks for a Locations Short.txt file, for the times the Locations are too long for good looking graphs
#	if the Locations Short.txt file does not exist, the string will be NULL and do nothing in the R scripts

if	"APIs Short.txt" in os.listdir(RelPath):
	APIsho		=	open(RelPath + "APIs Short.txt", 'r').readlines()
	APIsho		=	[line.strip('\n') for line in APIsho]
	APIshoStr	=	listclean(APIsho)
else:
	APIshoStr	=	"NULL"
#	checks for an APIs Short.txt file, in case the names of the APIs are too long
#	if the file does not exist, the string will be NULL and do nothing in the R scripts

if		TYPE	==	"HIGH" and len(GPUsread) != 1:
	cGPU	=	"NULL"
elif	TYPE	==	"GPU" or len(GPUsread) == 1:
	cGPU	=	"\"" + str(GPUsread[0]) + "\""
#	sets the cGPU, current GPU, variable based on if this is a single or multi-GPU situation
#		quotes are added to the string here, which is why \" is present
#	GPUsread is a list of the read GPUs from the folder names and useful for single-GPU, multi-API situations

scriptFull	=	scriptPath + "OCAT - Combined - PA.r"
#	sets the name and path of the reference R script to be loaded in

outputName	=	"Combined - PA - " + droppedGame + ".r"
outputFull	=	droppedPath + "@" + outputName
#	sets the name and path of the output file, derived from the reference R script

RPath		=	droppedPath.replace("\\", "/")
#	R uses / instead of \ for paths, so this replaces those characters in the string

if not os.path.exists(outputFull):
#	checks if the output script already exists, so as to not overwrite it
	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
#		loads the reference script to the variable fref
#		opens an output file to write to, to the variable fout
		for line in fref:
#			loops through the lines in the reference file
			fout.write(line	\
				.replace("!PATH!", RPath)		\
				.replace("!GAME!", droppedGame)	\
				.replace("!LONG!", out)			\
				.replace("!QUA!", QUAs[0])		\
				.replace("!LOC!", locStr)		\
				.replace("!LOCSHO!", locshoStr)	\
			)
#				writes each line from the reference file to the output file, but runs the replace function to change the appropriate things
#					the \ are necessary to have the line breaks within the command list
		fout.close()
#			closes the output file


scriptFull	=	scriptPath + "OCAT - Combined - Input.r"
#	sets the name and path of the reference R script to be loaded in

outputName	=	"Combined - Input - " + droppedGame + ".r"
outputFull	=	droppedPath + "@" + outputName
#	sets the name and path of the output file, derived from the reference R script

if not os.path.exists(outputFull):
#	checks if the output script already exists, so as to not overwrite it
	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
#		loads the reference script to the variable fref
#		opens an output file to write to, to the variable fout
		for line in fref:
#			loops through the lines in the reference file
			fout.write(line
				.replace("!PATH!", RPath)			\
				.replace("!GAME!", droppedGame)		\
				.replace("!API!", listclean(APIs))	\
				.replace("!APISHO!", APIshoStr)		\
				.replace("!QUA!", QUAs[0])			\
				# .replace("!TYPE!", "Max")			\	not used anymore
				.replace("!LOC!", locStr)			\
				.replace("!LOCSHO!", locshoStr)		\
				.replace("!GPU!", cGPU)
			)
#				writes each line from the reference file to the output file, but runs the replace function to change the appropriate things
		fout.close()
#			closes the output file

if not os.path.exists(droppedPath + "OCAT - Combined - Output.r"):
#	checks if the output script already exists, so as to not overwrite it
	shutil.copyfile(scriptPath + "\\OCAT - Combined - Output.r", droppedPath + "@Combined - Output.r")
#		copies the reference file to the appropriate location, and changes the name

# os.system("pause")
#	just used to pause the script instead of closing, for troubleshooting purposes