/*------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
						  ECEC decomposition 
					 Figure Childcare Rate Dofile
 ------------------------------------------------------------------------------
------------------------------------------------------------------------------*/


u "$original_dataset_NEPS/Stata14\SC1_pParent_D_$version.dta", clear
	label language en	
	sort ID_t wave
	keep ID_t wave pa0100a pa0100b pa0100c pa0100d pa0100e pa0100f 	pa01100

nepsmiss _all

* Duration 
	recode pa01100 (-98 -97 = .) , gen (ecec_cb_dur)		
	
* Change title and lables of the variables 	
	recode pa0100a (0 .y =0 "No") (1=1 "Yes"), gen (ecec_cb_attendance)	
	recode pa0100b (0 .y =0 "No") (1=1 "Yes"), gen (ecec_fdc_attendance)	
	recode pa0100c (0 .y =0 "No") (1=1 "Yes"), gen (nanny_attendance)	
	recode pa0100d (0 .y =0 "No") (1=1 "Yes"), gen (aupair_attendance)	
	recode pa0100e (0 .y =0 "No") (1=1 "Yes"), gen (grandparents_attendance)	
	recode pa0100f (0 .y =0 "No") (1=1 "Yes"), gen (relatives_attendance)	
	
* Parental care only
	gen pa =.
	replace pa = 1 if pa0100a==.y 
	replace pa = 0 if ecec_cb_attendance==1 | ecec_fdc_attendance==1 |		 ///
	nanny_attendance==1 | aupair_attendance==1 | grandparents_attendance==1 | ///
	relatives_attendance==1
	lab define pa 1 "Only" 0 "Not Only"
	lab value pa pa
	lab var pa "Parental care only"	
	
	drop pa0100*

* ECEC + FDC attendance
	gen ecec_cd_fdc = ecec_cb_attendance
	replace ecec_cd_fdc = 1 if ecec_fdc_attendance == 1
	lab define ecec_cd_fdc 1 "Yes" 0 "No"
	lab value ecec_cd_fdc ecec_cd_fdc
	lab var ecec_cd_fdc "ECEC CB + FDC"
	
* Aupair + nanny 
	gen aupair_nanny = nanny_attendance
	replace aupair_nanny = 1 if aupair_attendance == 1
	replace aupair_nanny = 0 if aupair_attendance ==.y 	
	lab define aupair_nanny 1 "Yes" 0 "No"
	lab value aupair_nanny aupair_nanny
	lab var aupair_nanny "Aupair + nanny"
	
* Grandparents + relatives
	gen grandp_relatives = grandparents_attendance
	replace grandp_relatives = 1 if relatives_attendance == 1
	replace grandp_relatives = 0 if relatives_attendance ==.y 	
	lab define grandp_relatives 1 "Yes" 0 "No"
	lab value grandp_relatives grandp_relatives
	lab var grandp_relatives "Grandparents + relatives"	
	
	
keep ID_t wave ecec_cb_attendance ecec_fdc_attendance nanny_attendance 		 ///
	aupair_attendance grandparents_attendance relatives_attendance pa 		 ///
	ecec_cd_fdc grandp_relatives aupair_nanny ecec_cb_dur
	
	reshape8 wide ecec_cb_attendance ecec_fdc_attendance nanny_attendance 	 ///
		aupair_attendance grandparents_attendance relatives_attendance pa 	 ///
		ecec_cd_fdc grandp_relatives aupair_nanny ecec_cb_dur, i(ID_t) j(wave)
	
* Graph
preserve

keep ID_t ecec_cb_attendance* grandp_relatives* pa* ecec_fdc_attendance*

reshape long ecec_cb_attendance@ grandp_relatives@ pa@ ecec_fdc_attendance@, i(ID_t) j(wave)

* Drop observations for waves 7, 8, and 9
drop if wave >= 7

collapse ecec_cb_attendance grandp_relatives ecec_fdc_attendance pa, by(wave)

tw (connect ecec_cb_attendance wave, lp(dash) lc(navy))                ///
   (connect ecec_fdc_attendance wave, lp(dash) lc(orange))              ///
   (connect grandp_relatives wave, lp(dash) lc(dkgreen))                ///
   (connect pa wave, lp(dash) lc(red))                                  ///
   , legend(size(small) col(2) position(6)                              ///
   lab(1 "ECEC or (pre)primary education") lab(2 "Family daycare")      ///
   lab(3 "Grandparents and relatives") lab(4 "Parental care only"))     ///
   xlabel(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6")                          ///
   ylabel(0 "0" .25 "25" .50 "50" .75 "75" 1.00 "100")                  ///
   ytitle("%")                                                          ///
   xtitle("Wave")                                                       ///
   subtitle("", size(small))                                            ///
   name(attendance_specific, replace)                                   ///
   xline(3, lpattern(solid) lcolor(gray))                             ///
   text(0.9 3 "Childcare arrangements", size(vsmall) color(black))            ///
   xline(5, lpattern(solid) lcolor(gray))                               ///
   xline(6, lpattern(solid) lcolor(gray))                               ///
   text(0.9 5 "Mathematics", size(vsmall) color(black))                  ///
   text(0.9 6 "Vocabulary", size(vsmall) color(black))                   

graph export "$output_figures/descriptive_attendance.jpg", quality(100) replace         

restore
