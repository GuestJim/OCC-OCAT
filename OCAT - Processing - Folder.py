import sys, os, fileinput
#	loads different modules for Python

droppedPath = sys.argv[1].rsplit("\\", 1)[0]+"\\"
#	identifies the path of the file dropped onto this Python script

for files in os.listdir(droppedPath):
#	will go through all of the files in the folder
	if files.endswith(".csv") and files != "perf_summary.csv":
#		only does the following code if the file is a CSV and if it is not the 'perf_summary.csv' file OCAT will make

		droppedFile = files
#	not strictly necessary, though it keeps the variable name used in the other files
		droppedName = droppedFile.split(".csv")[0]
#	is necessary to get the file name without extension

		os.chdir(droppedPath)
#	changes the current working directory to where the CSVs are

		scriptPath=sys.argv[0].rsplit("\\", 1)[0]
#	gets the path of the Python script, which is where the reference R scripts are
		scriptType = "OCAT"
		scriptName = "Processing"
#	separate Type and Name variables for use with the outputName variable below
		scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"
#	full path and name of the source R script
		outputName = scriptName + " " + scriptType + " - " + droppedName + ".r"
#	name of output R script
		outputFull = droppedPath + outputName
#	full path and name of output R script

		RPath = droppedPath.replace("\\", "/")
#	R needs to use / instead of \ for file paths, hence this conversion

		from shutil import copyfile
		copyfile(scriptFull, outputFull)
#	copies the source R script to the output R script location and name

		with fileinput.FileInput(outputName, inplace=True) as file:
			for line in file:
				print(line.replace("!PATH!", RPath).replace("!FILE!", droppedName).replace("!FILEX!", droppedName + ".csv"), end='')
#	reads the lines of the outpur R script and replaces specific strings with the correct references