import sys, os

scriptPath = sys.argv[0].rsplit("\\",1)[0]
droppedPath = sys.argv[1].rsplit("\\",1)[0] + "\\"

droppedFiles = sys.argv[1:]

droppedOCAT = []
droppedADRN = []

for file in droppedFiles:
	if "OCAT" in file:
		droppedOCAT.append(file.rsplit("\\",1)[1].split(".")[0])
	if "ADRN" in file:
		droppedADRN.append(file.rsplit("\\",1)[1].split(".")[0])

droppedOCAT.sort()
droppedADRN.sort()

scriptType = "OCAT-ADRN"
scriptName = "Processing"
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"

	RPath = droppedPath.replace("\\", "/")

for place in range(0, len(droppedOCAT)):
	outputName = scriptName + " " + scriptType + " - " + droppedOCAT[place] + ".r"
	outputFull = droppedPath + outputName

	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
		for line in fref:
			fout.write(line.replace("!PATH!", RPath).replace("!FILEADRN!", droppedADRN[place]).replace("!FILEOCAT!", droppedOCAT[place]).replace("!FILEADRNX!", droppedADRN[place] + ".csv").replace("!FILEOCATX!", droppedOCAT[place] + ".csv"))
		fout.close()
	
#os.system("pause")