import sys, os, fileinput
droppedFile = sys.argv[1]
droppedName = sys.argv[2]
droppedPath = sys.argv[3]

scriptPath = os.path.abspath('')
scriptType = "OCAT-ADRN"
scriptName = "Multi-Run"
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"
folderName = droppedPath.split('\\')[-2]
outputName = scriptName + " " + scriptType + " - " + folderName + ".r"
outputFull = droppedPath + outputName

RPath = droppedPath.replace("\\", "/")

os.chdir(droppedPath)

from shutil import copyfile
copyfile(scriptFull, outputFull)

with fileinput.FileInput(outputName, inplace=True) as file:
	for line in file:
		print(line.replace("!PATH!", RPath).replace("!FILE!", droppedName).replace("!FILEX!", droppedName + ".csv").replace("!NAME!", folderName), end='')