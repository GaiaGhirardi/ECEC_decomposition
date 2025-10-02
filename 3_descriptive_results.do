/*------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
						  ECEC decomposition 
						Descriptive results Dofile
 ------------------------------------------------------------------------------
------------------------------------------------------------------------------*/


clear all
set more off, perm
set cformat %3.2f

u "$working_dataset_NEPS/xDataset_models.dta", clear

********************************************************************************
* The relationship between SES and competencies (SES --> competencies)
********************************************************************************
	
* Change labels for graphs 
lab var math5_sd "Mathematics (4 years old)"
lab var voc6_sd "Vocabulary (5 years old)"

#delimit; 
 
  tw 
  kdensity math5_wle_100 if eduM_B1==0, recast(area) fcolor(%40) lwidth(*1.10)  ||
  kdensity math5_wle_100 if eduM_B1==1, recast(area) fcolor(%40) lwidth(*1.10)  ||
  kdensity math5_wle_100, lwidth(*1.25)              || 
  ,
     legend(label(1 "Low educated mothers") label(2 "High educated mothers") label(3 "Overall") rowgap(0.25) size(4) pos(6) row(1)) 
     title("Mathematics (4 years old)", pos(18) size(5)) 
     ylabel(0(.005).02, nogrid labsize(2)) 
     ytitle("Density", size(2) orient(vertical))   
     xtitle("Mathematics in percentiles", size(3)) 
     xlabel(, nogrid labsize(2)) name(math, replace)
 ;
 #delimit cr
 
 #delimit; 
 
  tw 
  kdensity voc6_sum_100 if eduM_B1==0, recast(area) fcolor(%40) lwidth(*1.10)  ||
  kdensity voc6_sum_100 if eduM_B1==1, recast(area) fcolor(%40) lwidth(*1.10)  ||
  kdensity voc6_sum_100, lwidth(*1.25)              || 
  ,
     legend(label(1 "Low educated mothers") label(2 "High educated mothers") label(3 "Overall") rowgap(0.25) size(4) pos(6) row(1)) 
     title("Vocabulary (5 years old)", pos(15) size(5)) 
     ylabel(0(.005).02, nogrid labsize(2)) 
     ytitle("Density", size(2) orient(vertical))   
     xtitle("Vocabulary in percentiles", size(3)) 
     xlabel(, nogrid labsize(2)) name(voc, replace)
 ;
 #delimit cr

grc1leg math voc, col(2) ycom pos(6)		
graph export "$output_figures_desc/distribution_Y2.png", replace width(1600) height(1100)
graph export "$output_figures_desc/distribution_Y2.pdf", replace 
graph export "$output_figures_desc/distribution_Y2.tif", width(3900) replace	
	

	


























