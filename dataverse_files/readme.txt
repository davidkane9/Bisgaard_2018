-------------------------------
	READ ME
-------------------------------
This repository contains the data and scripts necessary for reproducing the results in the article "Partisan Elites as Culprits: How Partisan Elite Rhetoric Shapes Partisan Perceptual Gaps" by Martin Bisgaard and Rune Slothuus. 

Note that the original survey data, denoted "original_data.dta" in the scripts "build_study1.R" and "build_study2.R", are not included in the repository. 

The Repository contains the following files:


DOCUMENTATION
- coder_instruction.pdf		(description of instructions used for coding statements from the Prime minister)
- codebook_statements.pdf	(description of variables contained in "statements.RData")
- codebook_media.pdf		(description of variables contained in "media.RData")
- codebook_media_daily.pdf	(description of variables contained in "media_daily.RData")
- codebook_study1.pdf		(description of variables contained in "study1.RData", "study1_unbalanced.RData" and "study1_wide.RData")
- codebook_study2.pdf 		(description of variables contained in "study2.RData")
- questionnaire_study1_wave1.pdf (full questionnaire for first panel wave in study 1)	
- questionnaire_study1_wave1_originalDanish.pdf (full questionnaire for first panel wave in study 1 in original language)	
- questionnaire_study1_wave2.pdf (full questionnaire for second panel wave in study 1)
- questionnaire_study1_wave2_originalDanish.pdf (full questionnaire for first panel wave in study 1 in original language)	
- questionnaire_study1_wave3.pdf (full questionnaire for third panel wave in study 1)
- questionnaire_study1_wave3_originalDanish.pdf (full questionnaire for first panel wave in study 1 in original language)	
- questionnaire_study1_wave4.pdf (full questionnaire for fourth panel wave in study 1)
- questionnaire_study1_wave4_originalDanish.pdf (full questionnaire for first panel wave in study 1 in original language)	
- questionnaire_study1_wave5.pdf (full questionnaire for fifth panel wave in study 1)
- questionnaire_study1_wave5_originalDanish.pdf (full questionnaire for first panel wave in study 1 in original language)	
- questionnaire_study2.pdf (full questionnaire for study 2)
- questionnaire_study2_originalDanish.pdf (full questionnaire for study 2 in original language)

DATA FILES
- statements.RData 			(coded statements from the Prime Minister)
- study1.RData 				(the 5-wave panel data used for Study 1)
- study1_unbalanced.RData 	(unbalanced version of the 5-wave panel data)
- study1_wide.RData 		(the 5-wave panel data in WIDE format)
- study2.RData 				(the experimental data used for Study 2)
- media.RData 				(results from an online media database, http://infomedia.dk used for supplemental analyses)
- media_daily.RData 		(daily results from an online media database, http://infomedia.dk, used for supplemental analyses)

R SCRIPTS
- build_study1.R		(generates the data files for Study 1 from the original data)
- build_study2.R		(generates the data file for Study 2 from the original data)
- results_statements.R 	(reproduces Figure 1 in the main article)
- results_study1.R 		(reproduces Figure 2 and 3, and Table 1 in the main article)
- results_study2.R 		(reproduces Figure 4 and Table 2 in the main article)
- results_SI.R 			(reproduces the supplemental analyses reported in the Supporting Information material)