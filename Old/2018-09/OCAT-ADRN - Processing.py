import sys, os, fileinput
droppedFile = sys.argv[1]
droppedADRN = sys.argv[2]
droppedOCAT = sys.argv[3]
droppedPath = sys.argv[4]

scriptPath = os.path.abspath('')
scriptType = "OCAT-ADRN"
scriptName = "Processing"
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"
outputName = scriptName + " " + scriptType + " - " + droppedOCAT + ".r"
outputFull = droppedPath + outputName

RPath = droppedPath.replace("\\", "/")

os.chdir(droppedPath)

from shutil import copyfile
copyfile(scriptFull, outputFull)

with fileinput.FileInput(outputName, inplace=True) as file:
	for line in file:
		print(line.replace("!PATH!", RPath).replace("!FILEADRN!", droppedADRN).replace("!FILEOCAT!", droppedOCAT).replace("!FILEADRNX!", droppedADRN + ".csv").replace("!FILEOCATX!", droppedOCAT + ".csv"), end='')