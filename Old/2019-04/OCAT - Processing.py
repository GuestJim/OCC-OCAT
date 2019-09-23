import sys, os

scriptPath = sys.argv[0].rsplit("\\",1)[0]

scriptType = "OCAT"
scriptName = "Processing"
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"

droppedFiles = sys.argv[1:]
#	this is a list of all of the dropped files
droppedPath = droppedFiles[0].rsplit("\\",1)[0] + "\\"

for place in droppedPath.split("\\"):
	if " Review" in place:
		droppedGame = place.split(" Review")[0]

for droppedFile in droppedFiles:
	droppedName = droppedFile.rsplit("\\",1)[1].split(".csv")[0]

	outputName = scriptName + " " + scriptType + " - " + droppedName + ".r"
	outputFull = droppedPath + outputName
	
	RPath = droppedPath.replace("\\", "/")

	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
		for line in fref:
			fout.write(line.replace("!PATH!", RPath).replace("!FILE!", droppedName).replace("!FILEX!", droppedName + ".csv").replace("!GAME!", droppedGame))
		fout.close()

#os.system("pause")