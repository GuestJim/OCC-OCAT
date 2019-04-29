import sys, os, pandas
#	loads different modules for Python
#		pandas is for importing CSVs

droppedFiles = sys.argv[1:]
#	this is a list of all of the dropped files

keep_col = ['Application','Runtime','AllowsTearing','PresentMode','Dropped','TimeInSeconds','MsBetweenPresents','MsBetweenDisplayChange','MsInPresentAPI','MsUntilRenderComplete','MsUntilDisplayed']
#	the list of the columnn names to be kept

for droppedFile in droppedFiles:
#	loops through the list of files dropped onto the script
	file = pandas.read_csv(droppedFile, dtype = object)
	#	reads the CSV to the file variable
	#		it likes throwing an error message without the 'dtype = object' option
	#		because it wants to have the data be of one type
	new_file = file[keep_col]
	#	selects just the columns to be kept and then saves them to a new file
	new_file.to_csv(droppedFile, index=False)
	#	saves the cleaned table to the original file's location, replacing it

#os.system("pause")