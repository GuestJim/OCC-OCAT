# OCC-OCAT
My scripts for processing OCAT data and generating the scripts.

The way these work is to have the OCAT CSVs in this folder heirarchy: \[Article Title\]\OCAT Data\\\[GPU\]\\\[API\]\\\[Quality\]\\\[CSV files\] with GPU, Quality, and if appropriate API named correctly.
The "OCAT" Python script can then have a file dropped on it from the "OCAT Data" folder or from the lowest folder where the CSVs are kept and it will generate the R scripts to process that data from the reference scripts kept here.
It is necessary the reference R scripts are kept in the same folder as the Python script, as it looks for them there.
The "@*** - PA - \[Article Title\].r" script should then be run to create a "@Combined - \[Quality\].csv" file that the "@Combined - Input - \[Article Title\].r" script will read in when executed.
Various controls are present in this Input script to set things such as data type, graph dimensions, and more.

Following what I am calling the 2020 Overhaul (purely because I did it in 2020 and not as a commitment to yearly overhauls) there are new "Search" scripts.
These will have the R scripts do some of the searching for OCAT CSVs, instead of requiring the Python script to do all the work.
I personally will use "OCAT - Search - PA.py" and the resulting "@Search - PA - \[Article Title\].r" script in the future for this functionality, but the "OCAT - Modular - PA.py" and "@Combined - PA - \[Article Title\].r" script it will produce still work without issue.
The Input and Output R scripts will work with either process.

The Fully Commented branch has versions of the scripts where I comment almost line-by-line to explain how it all works.
Article covering all of the statistics, graphs, and scripts can be found here: https://github.com/GuestJim/Serious-Statistics-Reprocessed

The Old folders have an older version of these, where a Batch file is used to call a Python script to create the desired R script
