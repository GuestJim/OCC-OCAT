import sys, os, pandas
#	loads different modules for Python
#		pandas is for importing CSVs

droppedPath = sys.argv[1].rsplit("\\", 1)[0]+"\\"
#	the path to the files dropped onto the Python script

keep_col = ['Application','Runtime','AllowsTearing','PresentMode','Dropped','TimeInSeconds','MsBetweenPresents','MsBetweenDisplayChange','MsInPresentAPI','MsUntilRenderComplete','MsUntilDisplayed']
#	the list of the columnn names to be kept

for files in os.listdir(droppedPath):
#	loops through the list of files in the directory of the dropped file
	if files.endswith(".csv") and "OCAT" in files:
	#	will only work with the CSV files with OCAT in the name
		droppedFile = files
		#	instead of changing the droppedFile variable later, I just set it to be the files variable from the loop
		droppedName = droppedFile.split(".csv")[0]
		#	grabs just the file name from the CSV

		file = pandas.read_csv(droppedPath + droppedFile, dtype = object)
		#	reads the CSV to the file variable
		#	it is necessary to specify the path as well
		#		it likes throwing an error message without the 'dtype = object' option
		#		because it wants to have the data be of one type

		new_file = file[keep_col]
		#	selects just the columns to be kept and then saves them to a new file
		new_file.to_csv(droppedPath + droppedFile, index=False)
		#	saves the cleaned table to the original file's location, replacing it

#os.system("pause")