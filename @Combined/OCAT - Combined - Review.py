import sys, os

droppedPath = sys.argv[1].rsplit("\\", 1)[0]+"\\"

scriptPath=sys.argv[0].rsplit("\\", 1)[0]

GPUName = input("GPU (default RX Vega 64): ") or "RX Vega 64"

for place in droppedPath.split("\\"):
	if " Review" in place:
		droppedGame = place.split(" Review")[0]

def listclean (list):
	return str(list).replace("[", "").replace("]", "").replace("\'", "\"").replace(", ", ",\n");

CSV = []
LOC = ["Recording "]

for files in os.listdir(droppedPath):
	if files.endswith(".csv") and "OCAT" in files:
		CSV.append(files.split(".csv")[0])

LOC = LOC * len(CSV)
for n in range(0, len(CSV)):
	LOC[n] = LOC[n] + str(n + 1)

scriptType = "OCAT"
scriptName = "Combined"
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"

outputName = scriptName + " " + scriptType + " - " + droppedGame + ".r"
outputFull = droppedPath + "@" + outputName

RPath = droppedPath.replace("\\", "/")

if not os.path.exists(outputFull):
	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
		for line in fref:
			fout.write(line.replace("!PATH!", RPath).replace("!GAME!", droppedGame).replace("!GPU!", GPUName).replace("!LIST!", listclean(CSV)).replace("!LOC!", listclean(LOC)))
		fout.close()

scriptType = "OCAT"
scriptName = "Processing Combined"
scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"

outputName = scriptName + " " + scriptType + " - " + droppedGame + ".r"
outputFull = droppedPath + "@" + outputName

if not os.path.exists(outputFull):
	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
		for line in fref:
			fout.write(line.replace("!PATH!", RPath).replace("!GAME!", droppedGame).replace("!LIST!", listclean(CSV)).replace("!LOC!", listclean(LOC)))
		fout.close()

# os.system("pause")