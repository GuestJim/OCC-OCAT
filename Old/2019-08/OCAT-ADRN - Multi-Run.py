import sys, os

scriptPath = sys.argv[0].rsplit("\\",1)[0]
scriptType = "OCAT-ADRN"
scriptName = "Multi-Run"
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"

droppedFile = sys.argv[1]
droppedName = droppedFile.rsplit("\\",1)[1].split(".")[0]
droppedPath = droppedFile.rsplit("\\",1)[0] + "\\"

folderName = droppedPath.split('\\')[-2]
outputName = scriptName + " " + scriptType + " - " + folderName + ".r"
outputFull = droppedPath + outputName

RPath = droppedPath.replace("\\", "/")

with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
	for line in fref:
		fout.write(line.replace("!PATH!", RPath).replace("!FILE!", droppedName).replace("!FILEX!", droppedName + ".csv").replace("!NAME!", folderName))
	fout.close()

#os.system("pause")