import sys, os
#	loads different modules for Python

scriptPath = sys.argv[0].rsplit("\\",1)[0]
#	gets the path to the Python script, which is the same location as the reference R scripts

scriptType = "OCAT"
#	sets the script type is for OCAT files
scriptName = "Processing"
#	sets the specific script to be used
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"
#	constructs the complete path to the desired script

droppedFiles = sys.argv[1:]
#	this is a list of all of the dropped files
droppedPath = droppedFiles[0].rsplit("\\",1)[0] + "\\"
#	the path to the files dropped onto the Python script

for droppedFile in droppedFiles:
#	works through the files dropped onto the Python script
	droppedName = droppedFile.rsplit("\\",1)[1].split(".csv")[0]
#		gets just the file name of the dropped file

	outputName = scriptName + " " + scriptType + " - " + droppedName + ".r"
#		constructs the name for the output file
	outputFull = droppedPath + outputName
#		constructs the complete path for the output file

	RPath = droppedPath.replace("\\", "/")
#	R needs to use / instead of \ for file paths, hence this conversion

	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
#		opens and reads the reference R script to the fref variable
#		opens the output R script, and calls it fout
		for line in fref:
#			reads through each line from the reference file
			fout.write(line.replace("!PATH!", RPath).replace("!FILE!", droppedName).replace("!FILEX!", droppedName + ".csv"))
#				replaces the !PATH!, !FILE!, and !FILEX! text in the reference file
#					note it is writing to fout, not fref, so the reference file is never changed
		fout.close()
#			closes fout, which finishes the file so it can be used

#os.system("pause")