import sys, os, shutil
#	loads different modules for Python

droppedPath = sys.argv[1].rsplit("\\", 1)[0]+"\\"
#	the path to the files dropped onto the Python script
scriptPath=sys.argv[0].rsplit("\\", 1)[0]
#	the path to the Python and reference R scripts

if " Review" in droppedPath:
	for place in droppedPath.split("\\"):
		if " Review" in place:
			droppedGame = place.replace(" Review", "")
			droppedQUA = "Review"
#	identifies if the recordings are within a folder for a Review or a Performance Analysis
#	it will grab the game name from the file path, so it can be added to the scripts for you
#		dropped QUA is not actually needed for this, but I worked from the Python script for Performance Analyses, which do need it

if " Performance Analysis" in droppedPath:
	for i in range(len(droppedPath.split("\\"))):
		if " Performance Analysis" in droppedPath.split("\\")[i]:
			droppedGame = droppedPath.split("\\")[i].replace(" Performance Analysis", "") + " (" + droppedPath.split("\\")[i+2] + ")"
#	identifies if the recordings are within a folder for a Review or a Performance Analysis
#	if it is a Performance Analysis, it will get both the game name and the GPU name, and put both pieces of information into the droppedGame variable, which is desired for such articles

rec = 0
#	finally decided to make it possible for it to change the recording number for me
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
		scriptName = "Processing - Input"
#			sets the specific script to be used
		scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"
#			constructs the complete path to the desired script

		outputName = "Processing - " + droppedName + ".r"
#			constructs the name for the output file
		outputFull = droppedPath + outputName
#			constructs the complete path for the output file

		RPath = droppedPath.replace("\\", "/")
#			R needs to use / instead of \ for file paths, hence this conversion
		rec = rec + 1
#			increments the recording number

		if not os.path.exists(outputFull):
#			to prevent overwriting already created files
			with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
#				opens and reads the reference R script to the fref variable
#				opens the output R script, and calls it fout
				for line in fref:
#					reads through each line from the reference file
					fout.write(line.replace("!PATH!", RPath).replace("!FILE!", droppedName).replace("!FILEX!", droppedName + ".csv").replace("!GAME!", droppedGame).replace("!REC!", str(rec)))
#						replaces the !PATH!, !FILE!, !FILEX!, !GAME!, and !REC! text in the reference file 
#						note it is writing to fout, not fref, so the reference file is never changed
				fout.close()
#				closes fout, which finishes the file so it can be used

if not os.path.exists(droppedPath + "OCAT - Processing - Output.r"):
#	checks if the Output.r script is already present
	shutil.copyfile(scriptPath + "\\OCAT - Processing - Output.r", droppedPath + "OCAT - Processing - Output.r")
#		copies the Output.r script to the data directory

# os.system("pause")