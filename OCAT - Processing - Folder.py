import sys, os, fileinput

droppedPath = sys.argv[1].rsplit("\\", 1)[0]+"\\"

for files in os.listdir(droppedPath):
	if files.endswith(".csv") and files != "perf_summary.csv":

		droppedFile = files
		droppedName = droppedFile.split(".csv")[0]

		os.chdir(droppedPath)
		
		scriptPath=sys.argv[0].rsplit("\\", 1)[0]
		scriptType = "OCAT"
		scriptName = "Processing"
		scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"
		outputName = scriptName + " " + scriptType + " - " + droppedName + ".r"
		outputFull = droppedPath + outputName

		RPath = droppedPath.replace("\\", "/")

		from shutil import copyfile
		copyfile(scriptFull, outputFull)

		with fileinput.FileInput(outputName, inplace=True) as file:
			for line in file:
				print(line.replace("!PATH!", RPath).replace("!FILE!", droppedName).replace("!FILEX!", droppedName + ".csv"), end='')