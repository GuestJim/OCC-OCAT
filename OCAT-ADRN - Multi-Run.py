import sys, os
#	loads different modules for Python

scriptPath = sys.argv[0].rsplit("\\",1)[0]
#	the path to the Python and reference R scripts
scriptType = "OCAT-ADRN"
#	sets the script type is for OCAT files
scriptName = "Multi-Run"
#	sets the specific script to be used
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"
#	constructs the complete path to the desired script

droppedFile = sys.argv[1]
#	the path to the files dropped onto the Python script
droppedName = droppedFile.rsplit("\\",1)[1].split(".")[0]
#	gets just the file name of the dropped file
droppedPath = droppedFile.rsplit("\\",1)[0] + "\\"
#	the path to the files dropped onto the Python script

folderName = droppedPath.split('\\')[-2]
#	gets the folder name that is necessary for the R script
outputName = scriptName + " " + scriptType + " - " + folderName + ".r"
#	constructs the name for the output file
outputFull = droppedPath + outputName
#	constructs the complete path for the output file

RPath = droppedPath.replace("\\", "/")
#	R needs to use / instead of \ for file paths, hence this conversion

with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
#		opens and reads the reference R script to the fref variable
#		opens the output R script, and calls it fout
	for line in fref:
#		reads through each line from the reference file
		fout.write(line.replace("!PATH!", RPath).replace("!FILE!", droppedName).replace("!FILEX!", droppedName + ".csv").replace("!NAME!", folderName))
#			replaces the !PATH!, !FILE!, and !FILEX! text in the reference file 
#				note it is writing to fout, not fref, so the reference file is never changed
	fout.close()
#		closes fout, which finishes the file so it can be used

#os.system("pause")