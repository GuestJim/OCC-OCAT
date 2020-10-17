import sys, os, shutil
#	loads the sys, os, and shutil modules for Python to use

droppedPath	=	sys.argv[1].rsplit("\\", 1)[0] + "\\"

scriptPath	=	sys.argv[0].rsplit("\\", 1)[0] + "\\"
#	sys.argv is a list of the arguments passed to the script
#		the first item in the list is the path to the script itself
#		the second and subsequent items are the paths of files dropped onto the script
#	rsplit is a command to split a string at the designated pattern, optionally into a specified number of pieces
#		it is used here to remove the file names, getting just their paths (and the slashes are added back for droppedPath, as they are needed later)

# switches to determine if this calls for a single-GPU or multi-GPU configuration
if "Review" in droppedPath.rsplit("OCAT Data")[0].rsplit("\\", 2)[1]:
	TYPE	=	"SINGLE"
#	when working with review data, it will always be a single-GPU situation
elif "OCAT Data" in droppedPath.rsplit("\\", 4)[2:4]:
	TYPE	=	"MULTI"
#	if the dropped file is in the OCAT Data folder, it is the multi-GPU situation of collecting together all data for a specific quality level
#	if the dropped file is in a folder one below OCAT Data, then this may be a multi-API situation and so I want the quality level fixed
else:
	TYPE	=	"SINGLE"
#	all other non-review situations are single-GPU so just using else
#		there is technically the situation of collecting all data for a single-GPU and single-API (if applicable) but this is one I rarely use so it is not configured here
#		plus it would require checking the presence of multiple APIs or not

mQUA	=	"High"
#	the quality to filter by for multi-GPU situations

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


GPUs, QUAs	=	[], []
#	create empty lists for storing the lists of relevant values

for item in listmap:
	GPUs.append(item[0])
	QUAs.append(item[2])
#	goes through the listmap variable above and builds the lists from its items

GPUs	=	list(set(GPUs))
#	for Python to return just the unique elements to a list, you make it a set
#	as sets are different to work with, the unique elements are converted back to being a list and stored to these variables
if 		TYPE	==	"MULTI":
#	checks if the type is multi-GPU, or multi-API
	QUAs	=	[mQUA]
#	instead of letting it pull from list map, the Quality is set to be a specific value provided earlier
elif	TYPE	==	"SINGLE":
#	else if, so if the previous switch was false, it will check if this is true
	QUAs	=	list(set(QUAs))
#		create a list from the set of values in the GPU and Quality lists.

SUBS	=	"NULL"
#	SUBS is for setting the COLUMN value in the R script
#		the default of NULL ensures the default subsetting written into the file.

droppedGame	=	RelPath.rsplit("\\", 3)[1]	\
	.replace(" Performance Analysis", "")	\
	.replace(" Review", "")
#	always the folder above OCAT data is the name for the project
#	the replace functions then remove the article type from the name, if it is a performance analysis or review, getting just the game name
#		for miscellaneous artiles though, the article's name is appropriate to use

if		TYPE	==	"MULTI" and len(GPUs) != 1:
	cGPU	=	"NULL"
elif	TYPE	==	"SINGLE" or len(GPUs) == 1:
	cGPU	=	"\"" + str(GPUs[0]) + "\""
	SUBS	=	"\"Quality\""
#	sets the cGPU, current GPU, variable based on if this is a single or multi-GPU situation
#		quotes are added to the string here, which is why \" is present
#	GPUsread is a list of the read GPUs from the folder names and useful for single-GPU, multi-API situations
#	a single-GPU situation is most likely to call for a specific column to be subset by, and the most likely column is "Quality" then
#		the quotation marks are needed in the R script, so I need to escape them here so they will be placed in script.

scriptFull	=	scriptPath + "OCAT - Search - PA.r"
#	sets the name and path of the reference R script to be loaded in

outputName	=	"Search - PA - " + droppedGame + ".r"
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
				.replace("!PATH!",		RPath)			\
				.replace("!GAME!",		droppedGame)	\
				.replace("!QUA!",		QUAs[0])
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
				.replace("!PATH!",	RPath)			\
				.replace("!GAME!",	droppedGame)	\
				.replace("!QUA!",	QUAs[0])		\
				.replace("!GPU!",	cGPU)			\
				.replace("!SUBS!",		SUBS
			)
#				writes each line from the reference file to the output file, but runs the replace function to change the appropriate things
		fout.close()
#			closes the output file

if not os.path.exists(droppedPath + "@Combined - Output.r"):
#	checks if the output script already exists, so as to not overwrite it
	shutil.copyfile(scriptPath + "\\OCAT - Combined - Output.r", droppedPath + "@Combined - Output.r")
#		copies the reference file to the appropriate location, and changes the name

# os.system("pause")
#	just used to pause the script instead of closing, for troubleshooting purposes