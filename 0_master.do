/*------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
						  ECEC decomposition 
							Master Dofile
 ------------------------------------------------------------------------------
------------------------------------------------------------------------------*/

/*
* To install if not available: 
ssc install fre, replace all
ssc install coefplot, replace all
ssc install blindschemes, replace all
ssc install grstyle, replace all
ssc install palettes, replace all
ssc install estout, replace all
net install nepstools, from(http://nocrypt.neps-data.de/stata)
ssc install reshape8, replace all 
ssc install mdesc, replace all
ssc install ebalance, replace all
ssc install egenmore, replace all
ssc install mipolate, replace all
ssc install palettes, replace
ssc install colrspace, replace
ssc install polychoricpca // otherwise findit 
ssc install grc1leg // otherwise findit 
ssc install sensemakr, replace all
*/

********************************************************************************
* Directories 
********************************************************************************


if "`c(username)'" == "..." { 
	global original_dataset_NEPS "..../Data/NEPS/Original Data/SC1_D_10-1-0"
	global working_dataset_NEPS "..../Data/NEPS/Working Data"
	global version 10-1-0
	global output_figures  "..../Output/Figures"
	global output_figures_desc  "..../Output/Figures/DescriptiveResults"
	global output_figures  "..../Output/Figures"
	global output_tables "...../Output/Tables"	
}

********************************************************************************
* Graphs scheme 
********************************************************************************

	set scheme white_tableau
			
	
	