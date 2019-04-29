import sys, os
#	loads different modules for Python

droppedPath = sys.argv[1].rsplit("\\", 1)[0]+"\\"
#	the path to the files dropped onto the Python script
scriptPath=sys.argv[0].rsplit("\\", 1)[0]
#	the path to the Python and reference R scripts

for place in droppedPath.split("\\"):
#	works through the directory names in the droppedPath information
	if " Review" in place:
#		finds the directory with " Review" in the name, which would be the name for the review's folder
		droppedGame = place.replace(" Review", "")
#			removes the " Review" to get just the name of the game

for files in os.listdir(droppedPath):
#	loops through the list of files in the directory of the dropped file
	if files.endswith(".csv") and "OCAT" in files:
#		will only work with the CSV files with OCAT in the name
		droppedFile = files
#			instead of changing the droppedFile variable later, I just set it to be the files variable from the loop
		droppedName = droppedFile.split(".csv")[0]
#			grabs just the file name from the CSV
	
		scriptType = "OCAT"
#			sets the script type is for OCAT files
		scriptName = "Processing"
#			sets the specific script to be used
		scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"
#			constructs the complete path to the desired script
		
		outputName = scriptName + " " + scriptType + " - " + droppedName + ".r"
#			constructs the name for the output file
		outputFull = droppedPath + outputName
#			constructs the complete path for the output file

		RPath = droppedPath.replace("\\", "/")
#			R needs to use / instead of \ for file paths, hence this conversion

		if not os.path.exists(outputFull):
#			to prevent overwriting already created files
			with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
#				opens and reads the reference R script to the fref variable
#				opens the output R script, and calls it fout
				for line in fref:
#					reads through each line from the reference file
					fout.write(line.replace("!PATH!", RPath).replace("!FILE!", droppedName).replace("!FILEX!", droppedName + ".csv"))
#						replaces the !PATH!, !FILE!, and !FILEX! text in the reference file 
#						note it is writing to fout, not fref, so the reference file is never changed
				fout.close()
#				closes fout, which finishes the file so it can be used

#os.system("pause")
