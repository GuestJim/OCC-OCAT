import sys, os, fileinput
droppedFile = sys.argv[1]
droppedName = sys.argv[2]
droppedPath = sys.argv[3]

scriptPath = os.path.abspath('')
scriptType = "OCAT"
scriptName = "Overlay Display"
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"
outputName = scriptName + " " + scriptType + " - " + droppedName + ".r"
outputFull = droppedPath + outputName

RPath = droppedPath.replace("\\", "/")

os.chdir(droppedPath)

from shutil import copyfile
copyfile(scriptFull, outputFull)

with fileinput.FileInput(outputName, inplace=True) as file:
	for line in file:
		print(line.replace("!PATH!", RPath).replace("!FILE!", droppedName).replace("!FILEX!", droppedName + ".csv"), end='')