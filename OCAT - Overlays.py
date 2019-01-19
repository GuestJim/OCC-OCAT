import sys, os

scriptPath = sys.argv[0].rsplit("\\",1)[0]

scriptType = "OCAT"
scriptNameF = "Overlay Frame"
scriptFullF = scriptPath + "\\" + scriptType + " - " + scriptNameF + ".r"
scriptNameD = "Overlay Display"
scriptFullD = scriptPath + "\\" + scriptType + " - " + scriptNameD + ".r"

droppedFile = sys.argv[1]
droppedName = droppedFile.rsplit("\\",1)[1].split(".")[0]
droppedPath = droppedFile.rsplit("\\",1)[0] + "\\"

outputNameF = scriptNameF + " " + scriptType + " - " + droppedName + ".r"
outputFullF = droppedPath + outputNameF
outputNameD = scriptNameD + " " + scriptType + " - " + droppedName + ".r"
outputFullD = droppedPath + outputNameD

RPath = droppedPath.replace("\\", "/")

with open(scriptFullF, 'r') as fref, open(outputFullF, 'w') as fout:
	for line in fref:
		fout.write(line.replace("!PATH!", RPath).replace("!FILE!", droppedName).replace("!FILEX!", droppedName + ".csv"))
	fout.close()
	
with open(scriptFullD, 'r') as fref, open(outputFullD, 'w') as fout:
	for line in fref:
		fout.write(line.replace("!PATH!", RPath).replace("!FILE!", droppedName).replace("!FILEX!", droppedName + ".csv"))
	fout.close()
	
#os.system("pause")