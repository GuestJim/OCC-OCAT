import sys, os

droppedPath = sys.argv[1].rsplit("\\", 1)[0]+"\\"

scriptPath=sys.argv[0].rsplit("\\", 1)[0]

for place in droppedPath.split("\\"):
	if " Review" in place:
		droppedGame = place.split(" Review")[0]

for files in os.listdir(droppedPath):
	if files.endswith(".csv") and "OCAT" in files:
		droppedFile = files
		droppedName = droppedFile.split(".csv")[0]
		
		scriptType = "OCAT"
		scriptName = "Processing"
		scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"
		
		outputName = scriptName + " " + scriptType + " - " + droppedName + ".r"
		outputFull = droppedPath + outputName

		RPath = droppedPath.replace("\\", "/")
		
		if not os.path.exists(outputFull):
			with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
				for line in fref:
					fout.write(line.replace("!PATH!", RPath).replace("!FILE!", droppedName).replace("!FILEX!", droppedName + ".csv").replace("!GAME!", droppedGame))
				fout.close()
				
#os.system("pause")