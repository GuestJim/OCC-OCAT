texTable	=	function(file, TAB, r = 2)	{
	if(!(endsWith(file, ".tex") | endsWith(file, ".ltx")))	file	=	paste0(file, ".tex")

	#	round numerics
	if(!is.null(r))	{
		numeCOL			=	which(sapply(TAB, is.numeric))
		TAB[numeCOL]	=	round(TAB[numeCOL], r)
	}
	
	write.table(TAB,
		file	=	file,
		sep		=	"\t&\t",
		eol		=	"\t\\\\\n",
		quote	=	FALSE,
		row.names	=	FALSE,
		col.names	=	FALSE
		)
}
#	write a table with LaTeX formatting, but only the part within the environment. The table environment will still need to be created, so this is just for exporting the contents.