import os, sys

R_script	=	"C:\\Program Files\\R\\R-3.6.1\\bin\\x64\\Rscript.exe"

scriptPath	=	sys.argv[0].rsplit("\\", 1)[0] + "\\"
droppedPath	=	sys.argv[1].rsplit("\\", 1)[0] + "\\"


for file in os.listdir(droppedPath):
	path	=	droppedPath.replace("\\", "/")
	if file.startswith("OCAT-") and file.endswith(".csv"):
		# print("call \"" + scriptPath + "OCAT - perc_summary.bat\" \"" + path + "\" \"" + file + "\"")
		os.system("call \"" + "OCAT - perc_summary.bat\" \"" + path + "\" \"" + file + "\" \"" + R_script)

# os.system("pause")