import sys, os

droppedPath = sys.argv[1].rsplit("\\", 1)[0]
os.chdir(droppedPath)

droppedQua = droppedPath.rsplit("\\", 1)[1]

# print(os.getcwd())

if "Locations.txt" in os.listdir(droppedPath):
	loc = open(droppedPath + "Locations.txt", 'r').readlines()
		loc = [line.rstrip('\n') for line in loc]

try:	loc
except:	sys.exit()


for file in os.listdir(droppedPath):
	if file.endswith(".r") and file.startswith("Processing"):
		fileText = open(file,'r').read()

		fout = open(file, 'w')

		for i in range(len(loc)):
			fileText = fileText.replace("Recording " + str(i + 1), droppedQua + " - " + loc[i])

		fout.write(fileText)
		fout.close()


# os.system("pause")