import sys, os
#	loads different modules for Python

scriptPath = sys.argv[0].rsplit("\\",1)[0]
#	gets the path of the Python script, which is the same path for the R source scripts

scriptType = "OCAT"
#	sets the script type is for OCAT files
scriptNameF = "Overlay Frame"
#	sets the specific script to be used
scriptFullF = scriptPath + "\\" + scriptType + " - " + scriptNameF + ".r"
#	constructs the complete path to the desired script

droppedFile = sys.argv[1]
#	the path to the files dropped onto the Python script
droppedName = droppedFile.rsplit("\\",1)[1].split(".")[0]
#	gets just the file name of the dropped file
droppedPath = droppedFile.rsplit("\\",1)[0] + "\\"
#	the path to the files dropped onto the Python script

outputNameF = scriptNameF + " " + scriptType + " - " + droppedName + ".r"
#	constructs the name for the output file
outputFullF = droppedPath + outputNameF
#	constructs the complete path for the output file

RPath = droppedPath.replace("\\", "/")
#	R needs to use / instead of \ for file paths, hence this conversion

with open(scriptFullF, 'r') as fref, open(outputFullF, 'w') as fout:
#		opens and reads the reference R script to the fref variable
#		opens the output R script, and calls it fout
	for line in fref:
#		reads through each line from the reference file
		fout.write(line.replace("!PATH!", RPath).replace("!FILE!", droppedName).replace("!FILEX!", droppedName + ".csv"))
#			replaces the !PATH!, !FILE!, and !FILEX! text in the reference file 
#				note it is writing to fout, not fref, so the reference file is never changed
	fout.close()
#		closes fout, which finishes the file so it can be used

#os.system("pause")