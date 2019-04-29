import sys, os, pandas

droppedPath = sys.argv[1].rsplit("\\", 1)[0]+"\\"

keep_col = ['Application','Runtime','AllowsTearing','PresentMode','Dropped','TimeInSeconds','MsBetweenPresents','MsBetweenDisplayChange','MsInPresentAPI','MsUntilRenderComplete','MsUntilDisplayed']

for files in os.listdir(droppedPath):
	if files.endswith(".csv") and "OCAT" in files:
		droppedFile = files
		droppedName = droppedFile.split(".csv")[0]
		
		file = pandas.read_csv(droppedPath + droppedFile, dtype = object)
		#	it is necessary to specify the path as well
		
		new_file = file[keep_col]
		new_file.to_csv(droppedPath + droppedFile, index=False)
				
#os.system("pause")