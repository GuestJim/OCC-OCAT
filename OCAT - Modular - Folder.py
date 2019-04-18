import sys, os, shutil

droppedPath = sys.argv[1].rsplit("\\", 1)[0] + "\\"

scriptPath = sys.argv[0].rsplit("\\", 1)[0]

if " Review" in droppedPath:
	for place in droppedPath.split("\\"):
		if " Review" in place:
			droppedGame = place.replace(" Review", "")
			droppedQUA = "Review"

if " Performance Analysis" in droppedPath:
	for i in range(len(droppedPath.split("\\"))):
		if " Performance Analysis" in droppedPath.split("\\")[i]:
			droppedGame = droppedPath.split("\\")[i].replace(" Performance Analysis", "") + " (" + droppedPath.split("\\")[i+2] + ")"
			droppedQua = droppedPath.rsplit("\\", 2)[1]

rec = 0
for files in os.listdir(droppedPath):
	if files.endswith(".csv") and "OCAT" in files:
		droppedFile = files
		droppedName = droppedFile.split(".csv")[0]

		scriptType = "OCAT"
		scriptName = "Processing - Input"
		scriptFull = scriptPath + "\\" + scriptType + " - " + scriptName + ".r"

		outputName = "Processing - " + droppedName + ".r"
		outputFull = droppedPath + outputName

		RPath = droppedPath.replace("\\", "/")
		rec = rec + 1

		if not os.path.exists(outputFull):
			with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
				for line in fref:
					fout.write(line.replace("!PATH!", RPath).replace("!FILE!", droppedName).replace("!FILEX!", droppedName + ".csv").replace("!GAME!", droppedGame).replace("!REC!", str(rec)))
				fout.close()

if not os.path.exists(droppedPath + "OCAT - Processing - Output.r"):
	shutil.copyfile(scriptPath + "\\OCAT - Processing - Output.r", droppedPath + "OCAT - Processing - Output.r")
	
for file in os.listdir(droppedPath):
	if "Locations.txt" in file:
		loc = open(droppedPath + file, 'r').readlines()
		loc = [line.rstrip('\n') for line in loc]

try:	loc
except:	sys.exit()

for file in os.listdir(droppedPath):
	if file.endswith(".r") and file.startswith("Processing"):
		fileText = open(droppedPath + file,'r').read()

		fout = open(droppedPath + file, 'w')

		for i in range(len(loc)):
			fileText = fileText.replace("Recording " + str(i + 1), droppedQua + " - " + loc[i])

		fout.write(fileText)
		fout.close()

# os.system("pause")
