#	it is necessary to name the files appropriately 
#		OCAT and ADRN to identify the type of file
#		similar naming for those recordings from the same run

import sys, os
#	loads different modules for Python

scriptPath = sys.argv[0].rsplit("\\",1)[0]
#	gets the path of the Python script, which is the same path for the R source scripts
droppedPath = sys.argv[1].rsplit("\\",1)[0] + "\\"
#	assigns the values of the Batch Parameters passed to the Python script to these variables

droppedFiles = sys.argv[1:]
#	saves a list of the dropped files

droppedOCAT = []
droppedADRN = []
#	empty lists are created so entries can be appended to them

for file in droppedFiles:
#	goes through the list of droppedFiles
	if "OCAT" in file:
#		checks if OCAT is in the file name
		droppedOCAT.append(file.rsplit("\\",1)[1].split(".")[0])
#			appends the file name to the list
	if "ADRN" in file:
#		checks if ADRN is in the file name
		droppedADRN.append(file.rsplit("\\",1)[1].split(".")[0])
#			appends the file name to the list

droppedOCAT.sort()
droppedADRN.sort()
#	sorts the list so the order of the files dropped onto the Python script does not matter

scriptType = "OCAT-ADRN"
#	sets the script type is for OCAT files
scriptName = "Processing"
#	sets the specific script to be used
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"
#	constructs the complete path to the desired script

RPath = droppedPath.replace("\\", "/")
#	R needs to use / instead of \ for file paths, hence this conversion

for place in range(0, len(droppedOCAT)):
#	loops through the list of file names for one type
	outputName = scriptName + " " + scriptType + " - " + droppedOCAT[place] + ".r"
#		constructs the name for the output file
	outputFull = droppedPath + outputName
#		constructs the complete path for the output file

	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
#		opens and reads the reference R script to the fref variable
#		opens the output R script, and calls it fout
		for line in fref:
#			reads through each line from the reference file
			fout.write(line.replace("!PATH!", RPath).replace("!FILEADRN!", droppedADRN[place]).replace("!FILEOCAT!", droppedOCAT[place]).replace("!FILEADRNX!", droppedADRN[place] + ".csv").replace("!FILEOCATX!", droppedOCAT[place] + ".csv"))
#				replaces the !PATH!, !FILE!, !FILEX!, etc. text in the reference file 
#					note it is writing to fout, not fref, so the reference file is never changed
		fout.close()
#			closes fout, which finishes the file so it can be used

#os.system("pause")