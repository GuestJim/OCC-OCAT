import sys, os, pandas

droppedFiles = sys.argv[1:]
#	this is a list of all of the dropped files
droppedPath = droppedFiles[0].rsplit("\\",1)[0] + "\\"

keep_col = ['Application','Runtime','AllowsTearing','PresentMode','Dropped','TimeInSeconds','MsBetweenPresents','MsBetweenDisplayChange','MsInPresentAPI','MsUntilRenderComplete','MsUntilDisplayed']

for droppedFile in droppedFiles:
	file = pandas.read_csv(droppedFile, dtype = object)
	new_file = file[keep_col]
	new_file.to_csv(droppedFile, index=False)

#os.system("pause")