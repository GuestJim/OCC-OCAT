import os, sys
#	loads the sys and os modules for Python to use

R_script	=	"C:\\Program Files\\R\\R-3.6.1\\bin\\x64\\Rscript.exe"
#	variable for the locatin of the Rscript.exe file
#		as the folder this is placed in depends on the version of R, I want it as a variable for easy changing
#		it could also be set as an environment variable, but I decided for local variable instead

scriptPath	=	sys.argv[0].rsplit("\\", 1)[0] + "\\"
droppedPath	=	sys.argv[1].rsplit("\\", 1)[0] + "\\"
#	variables for the locations of this script, which is where other relevant scripts are, and the path to the file dropped onto this one

for file in os.listdir(droppedPath):
#	goes through the files in the same folder as the dropped file
	path	=	droppedPath.replace("\\", "/")
#		converts the droppedPath string to R's preferred formatting.
#			I have it here instead of outside the loop in case one wants to adapt this for os.walk() with subfolders
	if file.startswith("OCAT-") and file.endswith(".csv"):
#		checks to make sure the files to be worked on are the recordings from OCAT
		# print("call \"" + scriptPath + "OCAT - perc_summary.bat\" \"" + path + "\" \"" + file + "\"")
#			just here for testing, hence why it is commented
		os.system("call \"" + "OCAT - perc_summary.bat\" \"" + path + "\" \"" + file + "\" \"" + R_script)
#			tells the system to open the OCAT - perc_summary.bat file with three arguments
#				the arguments are the path to the dropped file, the name of the current file from the list, and the location of Rscript.exe

# os.system("pause")
#	will pause the script at its end, which helps with troubleshooting