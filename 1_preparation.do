/*------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
						  ECEC decomposition 
						  Preparation Dofile
 ------------------------------------------------------------------------------
------------------------------------------------------------------------------*/

********************************************************************************
* Children's demographic & socio-economic background information 
********************************************************************************

use "$original_dataset_NEPS/Stata14\SC1_pParent_D_${version}.dta", clear
	label language en
	
keep ID_t wave p751001_g1 p700010 p529000_D pb10000 p731802_g2 p731852_g2 	 ///
		  p731802_g1 p731852_g1 p731802_g3 p731852_g3 p731111 p400500_g1v1	 ///
		  p731904_g14 p731954_g14 p510005 p741001 p742001 p521000
	
isid ID_t wave
xtset ID_t wave
sort ID_t wave

nepsmiss _all, 

* Rename variables
	rename p751001_g1 place_residence       // Place of Residence (RS West/East)
	rename p700010 c_gender                 // Gender 
	rename p529000_D c_weight               // Birth weight
	rename pb10000 n_siblings        		// Number of siblings
	rename p731802_g2 p1_casmin				// Mother's education (CASMIN)
	rename p731852_g2 p2_casmin 			// Father's education (CASMIN)
	rename p731802_g1 p1_isced 				// Mother's education (ISCED)
	rename p731852_g1 p2_isced 				// Father's education (ISCED)
	rename p731802_g3 p1_edu_years 			// Mother's education (years)
	rename p731852_g3 p2_edu_years 			// Father's education (years)
	rename p731111 single_parent 			// Family composition
	rename p400500_g1v1 c_migr 				// Migration background 
	rename p731904_g14 p1_isei1 			// Mothers Occupation
	rename p731954_g14 p2_isei1 			// Fathers Occupation
	rename p510005 income1 					// Household income
	rename p741001 n_people_household 		// n. people in the house
	rename p742001 n_under14 				// n. people under 14
	rename p521000 c_health					// Child's health
	
* Child health
	fre c_health
	recode c_health (1 2 = 0) (3 4 5 =1), gen(c_healthD)
	lab def c_healthD 0 "Good" 1 "Not good"
	lab value c_healthD c_healthD

	recode c_health (1 = 0) (2 3 4 5 =1), gen(c_healthDD)
	lab def c_healthDD 0 "Very good" 1 "Not very good"
	lab value c_healthDD c_healthDD
	lab var c_healthDD "Child health"
		
* Equalized family income 
	fre income1 if wave==1 // n:2926
	bys ID_t (wave): gen ID_pynr = _n
	mipolate income1 ID_pynr, gen(income2) nearest ties (a) by(ID_t)
	drop ID_pynr
	gen n_adult= n_people_household - n_under14
	gen eqs =.
	replace eqs = 1+(n_adult - 1)*0.5 + n_under14 * 0.3 if n_adult>=1
	gen income = income2*eqs 
	fre income if wave==1 // n:3321
	lab var income "Income categorical"
	
* Mothers' education (CASMIN)		
	fre p1_casmin if wave==1 // n: 3472 
	bysort ID_t: egen p1_edu=max(p1_casmin) 
	bysort ID_t: replace p1_casmin=p1_edu if p1_casmin==.l
	bysort ID_t: replace p1_casmin=p1_edu if p1_casmin==.
	fre p1_casmin if wave==1 // n: 3479 
	lab var p1_casmin "Mothers education CASMIN"	
	
	recode p1_casmin (0/7 = 0 "Less than University qualification") (8 =1 "University qualification"), gen(eduM_A)
	recode p1_casmin (0/6 = 0 "Less than Degree from university of applied sciences") (7/8 =1 "Degree from university of applied sciences e University qualification" ), gen(eduM_B)
	lab var eduM_A "Mother's education dummy (more selective)"
	lab var eduM_B "Mother's education dummy (less selective)"
			
	recode p1_casmin 0/3=1 4/6=2 7/8=3
	lab def p1_casmin 1 "Low" 2 "Medium" 3 "High"
	lab value p1_casmin p1_casmin
	drop p1_edu

* Fathers' education (CASMIN)	
	fre p2_casmin if wave==1 // n: 3224 
	bysort ID_t: egen p2_edu=max(p2_casmin) 
	bysort ID_t: replace p2_casmin=p2_edu if p2_casmin==.l
	bysort ID_t: replace p2_casmin=p2_edu if p2_casmin==.
	fre p2_casmin if wave==1 // n: 3300 
	recode p2_casmin 0/3=1 4/6=2 7/8=3
	lab def p2_casmin 1 "Low" 2 "Medium" 3 "High"
	lab value p2_casmin p2_casmin
	drop p2_edu
	lab var p2_casmin "Father education CASMIN"	

* Place of Residence (RS West/East)	
	recode place_residence 2=1 1=0
	lab def place_residence 0 "Western Germany" 1 "Eastern Germany incl. Berlin", replace
	lab val place_residence place_residence
	lab var place_residence "Place of residence"
	
* Child's gender
	recode c_gender 2=0 0=1
	lab def c_gender 0 "Female" 1 "Male", replace
	lab val c_gender c_gender
	lab var c_gender "Gender (child)"	
	
* Partnership status
	recode single_parent 2=0 1=1
	lab def single_parent 1 "Yes" 0 "No"
	lab value single_parent single_parent
	lab var single_parent "Cohabitation family (! attention not single)" 
	preserve
	drop if wave>=4
	xtset
	xttrans single_parent
	restore
	
* N. of siblings 
	recode n_siblings 0=0 1=1 2/12=2
	lab def n_siblings 0 "0 siblings" 1 "1 sibling" 2 "2 or more siblings", replace
	lab val n_siblings n_siblings

* Migration background 
	gen c_migr_n = c_migr
	recode c_migr_n 0=0 7/9=0 2/6=1
	recode c_migr 0=0 2/9=1
	lab def c_migr 0 "No" 1 "Yes", replace
	lab def c_migr_n 0 "No" 1 "Yes", replace
	lab val c_migr c_migr	
	lab val c_migr_n c_migr_n	
	lab var c_migr "Migration background"
	lab var c_migr_n "Migration background"
	
* Birth weight 
	recode c_weight 1/2=1 3/7=0
	lab def c_weight 1 "under 2500g" 0 "more than 2500g", replace 
	lab val c_weight c_weight	

keep ID_t  wave c_gender place_residence n_siblings c_weight single_parent	 ///
	income p1_casmin p2_casmin c_migr_n c_healthDD eduM_A eduM_B 
	 
reshape8 wide c_gender place_residence n_siblings c_weight single_parent	 ///
	income p1_casmin p2_casmin c_migr_n c_healthDD eduM_A eduM_B , i(ID_t) j(wave)
	
keep ID_t c_gender1 place_residence1 n_siblings1 c_weight1 single_parent1	 ///
	single_parent2 single_parent3 income1 p1_casmin1 p2_casmin1 			 ///
	c_migr_n1 c_healthDD2 n_siblings3 eduM_A1 eduM_B1
	
* Data management of more type-varying variable: change in number of siblings
	gen n_sib_delta= n_siblings3 - n_siblings1
	recode n_sib_delta (-4/0=0 "No more siblings born") (1/4=1 "One or more siblings born"), gen (n_sib_d)
	lab var n_sib_d "Change in number of siblings"
	drop  n_sib_delta	

* Income: from quantitative variable -> categorical variable
	gquantiles incomeX = income1 , xtile nq(5)
	gquantiles income2 = income1 , xtile nq(2)
	//egen incomeX = xtile (income1), n(5)
	//egen income2 = xtile (income1), n(2)
	recode income2 (1=0) (2=1) 
	lab var income2 "Income dummy"
	
* Income dummy taking only 60-100 (high-SES) and 0-40 (low-SES)	
	//egen income100 = xtile (income1), n(100)	
	gquantiles income100 = income1 , xtile nq(100)	
	recode income100 (0/40=0 "Low-SES") (60/100=1 "High-SES") (41/59 = .) , gen (income100_2)
	lab var income100_2 "Income dummy 60-100 (high-SES) and 0-40 (low-SES)"
	lab var income100 "Income index percentiles"
	
* To create SES variables: edu mother, edu father and income  
	polychoricpca p1_casmin1 p2_casmin1 incomeX, score(ses_incomeX) nscore(1)
	drop incomeX 
	lab var ses_incomeX "SES index categorical"
	
* SES index dummy taking only 60-100 (high-SES) and 0-40 (low-SES)	
	//egen SES_100 = xtile (ses_incomeX), n(100)
	gquantiles SES_100 = ses_incomeX , xtile nq(100)
	recode SES_100 (0/40=0 "Low-SES") (60/100=1 "High-SES") (41/59 = .) , gen (SES_100_2)
	lab var SES_100_2 "SES index dummy 60-100 (high-SES) and 0-40 (low-SES)"
	lab var SES_100 "SES index percentiles"
			
* save
order _all, alphabetic
save "$working_dataset_NEPS/xDemo.dta", replace
	
********************************************************************************
*  Children's competencies 
********************************************************************************

* 1. Cognitive measures 

u "$original_dataset_NEPS/Stata14\SC1_xTargetCompetencies_D_$version", clear 
	label language en

keep ID_t man5_sc1 wave_w* can4_sc1 von6_sc3 man7_sc1 den40002 den40001_c den40002  den60001_c den60002 dsn40001_sc3a dsn70001_sc3a
	
nepsmiss _all

	rename man5_sc1 math5_wle 				// Mathematics
	rename man7_sc1 mat7_wle 				// Mathematics
	rename can4_sc1 can4_wle 				// Categorization: SON-R Subtest 
	rename von6_sc3 voc6_sum 				// Vocabulary: useful for MICE
	rename den40002 DEL4_sum 				// Delayed gratification
	rename den60001_c DEL6_sum 				// Delayed gratification
	rename dsn40001_sc3a DIG4_sum 			// Digit span 
	rename dsn70001_sc3a DIG6_sum 			// Digit span 
	
* Competencies in 100 
foreach var in math5_wle mat7_wle can4_wle voc6_sum  DEL4_sum DEL6_sum DIG4_sum DIG6_sum {
	//egen `var'_100 = xtile (`var'), n(100)
	gquantiles `var'_100 = `var' , xtile nq(100)	
}

lab var math5_wle_100 "Mathematics age 5 in percentiles"
lab var mat7_wle_100 "Mathematics age 7 in percentiles"
lab var can4_wle_100 "Categorization age 4 in percentiles"
lab var voc6_sum_100 "Vocabulary age 6 in percentiles"
lab var DEL4_sum_100 "Delayed gratification age 4 in percentiles"
lab var DEL6_sum_100 "Delayed gratification age 6 in percentiles"
lab var DIG4_sum_100 "Digit span age 4 in percentiles"
lab var DIG6_sum_100 "Digit span age 6 in percentiles"

* save 
order _all, alphabetic 
save "$working_dataset_NEPS/xCognitive_after.dta", replace

* 2. Socio-emotional measures

use "$original_dataset_NEPS/Stata14\SC1_pParent_D_${version}.dta", clear
	lab lang en
	isid ID_t wave
	xtset ID_t wave	
keep ID_t wave p67801c_g1 p67801a_g1 p67801k_g1 p67801l_g1
nepsmiss _all

	rename p67801c_g1 SDQ_ppb 					// Peer problems behaviour
	rename p67801a_g1 SDQ_pb 					// Pro-social behaviour
	rename p67801k_g1 SDQ_h 					// Hyperactivity
	rename p67801l_g1 SDQ_bp 					// Behavioural problems	

reshape8 wide SDQ_ppb SDQ_pb SDQ_h SDQ_bp, i(ID_t) j(wave)

* save 
order _all, alphabetic
save "$working_dataset_NEPS/xNoncognitive_after.dta", replace

* 3. Children's competencies before the treatment

use "$original_dataset_NEPS/Stata14\SC1_xDirectMeasures_D_${version}.dta", clear
	lab lang en
keep if wave_w1==1 
keep ID_t cdn1_sc1
nepsmiss _all

rename cdn1_sc1 sensori1_wle // Sensorimotor development

* Competencies in 10
gquantiles sensori1_wle_10 = sensori1_wle , xtile nq(10)	
gquantiles sensori1_wle_5 = sensori1_wle , xtile nq(5)	

* Save
keep ID_t sensori1_wle sensori1_wle_10 sensori1_wle_5
order _all, alphabetic
save "$working_dataset_NEPS/xCompetencies_before.dta", replace

********************************************************************************
* Childcare
********************************************************************************

u "$original_dataset_NEPS/Stata14\SC1_pParent_D_$version.dta", clear
	label language en	
	sort ID_t wave
	keep ID_t wave pa0100a pa0100b pa0100c pa0100d pa0100e pa0100f 	pa01100

nepsmiss _all

* Duration 
	recode pa01100 (-98 -97 = .) , gen (ecec_cb_dur)		
	
* -----------------------------------------------------------------------------*
* OPTION 1 
* -----------------------------------------------------------------------------*

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

	keep ID_t ecec_cb_attendance3  ecec_fdc_attendance3 nanny_attendance3 	 ///
		aupair_attendance3 grandparents_attendance3 relatives_attendance3 	 ///
		ecec_cb_attendance2 ecec_cb_attendance1 ecec_cd_fdc3 ecec_cd_fdc2 	 ///
		grandp_relatives3 aupair_nanny3 ecec_cb_dur3  pa3 
	
* save
order _all, alphabetic
save "$working_dataset_NEPS/xChildCare_attendance", replace	

u "$working_dataset_NEPS/xChildCare_attendance.dta", clear

* -----------------------------------------------------------------------------*
* OPTION 2 
* -----------------------------------------------------------------------------*

* Grandparents & Relatives only 
	gen grandp_relatives_only = . 
	
	// children who received Grandparents & Relatives care ONLY (x=1)	
	replace grandp_relatives_only=1 if grandparents_attendance==1  | relatives_attendance==1 & ///
	(ecec_fdc_attendance==0 & aupair_attendance==0 & nanny_attendance==0 & ecec_cb_attendance3==0 & pa3==0)
	
	// reference category children who did not received Grandparents & Relatives care but other forms of care (x=0)	
	replace grandp_relatives_only=0 if grandparents_attendance==0  & relatives_attendance==0 & ///
	(ecec_fdc_attendance==1 | aupair_attendance==1 | nanny_attendance==1 | ecec_cb_attendance3==1 | pa3==1)
	
	// reference category children who received Grandparents & Relatives care and other forms of care (x=0)	
	replace grandp_relatives_only = 0 if grandp_relatives3==1 & grandp_relatives_only==.
	
	lab define grandp_relatives_only 1 "Only granparents or relatives" 0 "Not Only or No grandparents or relatives"
	lab value grandp_relatives_only grandp_relatives_only		
	lab var grandp_relatives_only "Grandparents + relatives only"
		
* Aupair & Nanny only 
	gen aupair_nanny_only = . 
	
	// children who received Aupair & Nanny ONLY (x=1)		
	replace aupair_nanny_only=1 if aupair_attendance==1  | nanny_attendance==1 & ///
	(ecec_fdc_attendance==0 & grandparents_attendance==0 & relatives_attendance==0 & ecec_cb_attendance3==0 & pa3==0)
	
	// reference category children who did not received Aupair & Nanny but other forms of care (x=0)		
	replace aupair_nanny_only=0 if aupair_attendance==0  & nanny_attendance==0 & ///
	(ecec_fdc_attendance==1 | grandparents_attendance==1 | relatives_attendance==1 | ecec_cb_attendance3==1 | pa3==1)
	
	// reference category children who received Aupair & Nanny and other forms of care (x=0)		
	replace aupair_nanny_only = 0 if aupair_nanny3==1 & aupair_nanny_only==.
	
	lab define aupair_nanny_only 1 "Only aupair or nanny" 0 "Not Only or no aupair or nanny"
	lab value aupair_nanny_only aupair_nanny_only	
	lab var aupair_nanny_only "Aupair + nanny only"
	
* Family day care only
	gen fdc_only=.
	
	// children who attend ONLY FDC (x=1)	
	replace fdc_only=1 if ecec_fdc_attendance==1 & nanny_attendance==0 		 ///
	& aupair_attendance==0 & grandparents_attendance==0  					 ///
	& relatives_attendance==0 & ecec_cb_attendance3==0 & pa3==0	
	
	// reference category children who did not attend FDC but other forms of care (x=0)	
	replace fdc_only=0 if ecec_fdc_attendance==0 & nanny_attendance==1 		 ///
	| aupair_attendance==1 | grandparents_attendance==1  				  	 ///
	| relatives_attendance==1 | ecec_cb_attendance3==1 | pa3==1	

	// reference category children who attended FDC and other forms of care (x=0)		
	replace fdc_only = 0 if ecec_fdc_attendance3==1 & fdc_only==.
	
	lab define fdc_only 1 "Only FDC" 0 "Not Only or No FDC"
	lab value fdc_only fdc_only
	lab var fdc_only "FDC only"

* ECEC only
	gen ecec_cb_only=.
	
	// children who attend ONLY ECEC (x=1)
	replace ecec_cb_only=1 if ecec_cb_attendance3==1 & nanny_attendance==0	 ///
	& aupair_attendance==0 & grandparents_attendance==0 					 ///
	& relatives_attendance==0  & ecec_fdc_attendance==0 & pa3==0	
	
	// reference category children who attended ECEC and other forms of care (x=0)
	replace ecec_cb_only=0 if ecec_cb_attendance3==1 & nanny_attendance==1   ///
	| aupair_attendance==1 | grandparents_attendance==1  					 ///
	| relatives_attendance==1 | ecec_fdc_attendance==1 
	
	// reference category children who did not attend ECEC but other forms of care (x=0)
	replace ecec_cb_only=0 if ecec_cb_attendance3==0 & nanny_attendance==1   ///
	| aupair_attendance==1 | grandparents_attendance==1  					 ///
	| relatives_attendance==1 | ecec_fdc_attendance==1 | pa3==1	
	
	lab define ecec_cb_only 1 "Only ECEC" 0 "Not Only or No ECEC"
	lab value ecec_cb_only ecec_cb_only
	lab var ecec_cb_only "ECEC only"

* -----------------------------------------------------------------------------*
* OPTION 3
* -----------------------------------------------------------------------------*
	
* ECEC 
// ECEC only vs with children that did not attend ECEC
	clonevar ecec_cb_only_A = ecec_cb_only
	replace ecec_cb_only_A = . if ecec_cb_attendance3==1 & ecec_cb_only==0 
	// those who were 
	// in the reference category in ecec_cb_only that attended ECEC but not only, 
	// in this new variable are recoded as missing in order to "isolate" as much as 
	// possible the effect of ECEC. 
	lab define ecec_cb_only_A 1 "Only ECEC" 0 "Not only ECEC"
	lab value ecec_cb_only_A ecec_cb_only_A
	lab var ecec_cb_only_A "ECEC only vs no ecec attendance (Option A)"
	
* Family day care only
// FDC only vs with children that did not attend FDC
	clonevar fdc_only_A = fdc_only	
	replace fdc_only_A = . if ecec_fdc_attendance3==1 & fdc_only==0 
	// those who were 
	// in the reference category in fdc_cb_only that attended FDC but not only, 
	// in this new variable are recoded as missing in order to "isolate" as much as 
	// possible the effect of FDC. 
	lab define fdc_only_A 1 "Only FDC" 0 "Not only FDC"
	lab value fdc_only_A fdc_only_A
	lab var fdc_only_A "FDC only"
	
	lab var fdc_only_A "Family day care only vs no family day care (Option A)"	
	
* Granparents & Relatives only 	clonevar ecec_cb_only_A = ecec_cb_only
	clonevar grandp_relatives_only_A = grandp_relatives_only	
	replace grandp_relatives_only_A = . if grandp_relatives_only==0 & (grandparents_attendance3==1  | relatives_attendance3==1)
	// those who were 
	// in the reference category in grandp_relatives_only that received grandparents+relatives care but not only, 
	// in this new variable are recoded as missing in order to "isolate" as much as 
	// possible the effect of thr treatment. 	
	lab define grandp_relatives_only_A 1 "Only granparents or relatives" 0 "Not only granparents or relatives"
	lab value grandp_relatives_only_A grandp_relatives_only_A
	lab var grandp_relatives_only_A "Grandparents + relatives Only vs no grandparents + relatives (Option A)"
	
	/* same code
	gen  grandp_relatives_only_A = . 
	replace grandp_relatives_only_A=1 if grandparents_attendance==1  | relatives_attendance==1 & ///
	(ecec_fdc_attendance==0 & aupair_attendance==0 & nanny_attendance==0 & ecec_cb_attendance3==0 & pa3==0)
	replace grandp_relatives_only_A=0 if grandparents_attendance==0  & relatives_attendance==0 & ///
	(ecec_fdc_attendance==1 | aupair_attendance==1 | nanny_attendance==1 | ecec_cb_attendance3==1 | pa3==1)
	*/

* Aupair & Nanny only 
// Aupair & Nanny only only vs with children that did not attend Aupair & Nanny

	clonevar aupair_nanny_only_A = aupair_nanny_only	
	replace aupair_nanny_only_A = . if aupair_nanny_only==0 & (aupair_attendance==1  | nanny_attendance==1) 	
	// those who were 
	// in the reference category in grandp_relatives_only that received nanny+aupair care but not only, 
	// in this new variable are recoded as missing in order to "isolate" as much as 
	// possible the effect of thr treatment. 		
	lab define aupair_nanny_only_A 1 "Only aupair or nanny" 0 "Not only aupair or nanny"
	lab value aupair_nanny_only_A aupair_nanny_only_A	
	lab var aupair_nanny_only_A "Aupair + nanny only vs no aupair + nanny  (Option A)"	
	
	/* same code:
	gen aupair_nanny_only_A = . 	
	replace aupair_nanny_only_A=1 if aupair_attendance==1  | nanny_attendance==1 & ///
	(ecec_fdc_attendance==0 & grandparents_attendance==0 & relatives_attendance==0 & ecec_cb_attendance3==0 & pa3==0)
	replace aupair_nanny_only_A=0 if aupair_attendance==0  & nanny_attendance==0 & ///
	(ecec_fdc_attendance==1 | grandparents_attendance==1 | relatives_attendance==1 | ecec_cb_attendance3==1 | pa3==1)
	*/	
	
drop ecec_cb_attendance1 ecec_cb_attendance2 ecec_cd_fdc2 
	
* save
order _all, alphabetic
save "$working_dataset_NEPS/xChildCare_attendance", replace	

********************************************************************************
* Weights 
********************************************************************************

u "$original_dataset_NEPS/Stata14\SC1_Weights_D_$version.dta", clear
	label language en	

keep ID_t stratum psu w_t5comp w_t6comp w_t1to6comp w_t1 w_t2 w_t3

gen w_math5 = w_t5comp*w_t1*w_t2*w_t3
gen w_voc6 = w_t6comp*w_t1*w_t2*w_t3

* save
order _all, alphabetic
save "$working_dataset_NEPS/xWeights_attrition", replace	

********************************************************************************
* Merge
********************************************************************************

u "$working_dataset_NEPS/xDemo.dta", clear
merge 1:1 ID_t using "$working_dataset_NEPS/xCognitive_after.dta", 			 ///
	keep(master match) nogen assert(master match)
merge 1:1 ID_t using "$working_dataset_NEPS/xNoncognitive_after.dta",		 ///
	keep(master match) nogen assert(master match)
merge 1:1 ID_t using "$working_dataset_NEPS/xCompetencies_before.dta",		 ///
	keep(master match) nogen assert(master match)
merge 1:1 ID_t using "$working_dataset_NEPS/xChildCare_attendance.dta",		 ///
	keep(master match) nogen assert(master match)
merge 1:1 ID_t using "$working_dataset_NEPS/xWeights_attrition.dta",		 ///
	keep(master match) nogen assert(master match)	
save "$working_dataset_NEPS/xDataset.dta", replace

* last cleaning 
drop __ttp1_casmin1 __ttp2_casmin1 __ttincomeX 
save "$working_dataset_NEPS/xDataset.dta", replace

********************************************************************************
********************************************************************************

drop wave_w4 wave_w5 wave_w6 wave_w7 
//SDQ_bp1 SDQ_bp2 SDQ_bp3 SDQ_h1 SDQ_h2 SDQ_h3 SDQ_h4 SDQ_pb1 SDQ_pb2 SDQ_pb3 SDQ_pb4 SDQ_ppb1 SDQ_ppb2 SDQ_ppb3 SDQ_ppb4 sensori1_wle

save "$working_dataset_NEPS/data_selected.dta", replace



