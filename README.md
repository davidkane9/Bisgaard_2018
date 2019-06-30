# Bisgaard_2018

Attempt to replicate "Partisan Elites as Culprits? How Party Cues Shape Partisan Perceptual Gaps" by Martin Bisgaard and Rune Slothuus from the April 2018 AJPS. Using [data/code](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/TJ1ZXX) from the Dataverse.

Note that you can not replicate the results from scratch. Two scripts (build_study1.R and build_study2.R) make use of a data set (original_data.dta) which is not present. I could not find an explanation of why this is the case. But, those two scripts produce data sets which are already present in the repo, so we don't need to run them ourselves.

I was able to replicate the three key figures. I could not get the supplementary analysis to run. This line:

order<-c("C","V","DF","LA","RV","Kr","S","SF","?","None","Other","Refuse","tot") #note: set order of parties as it appears in the SI (note that some party labels are letters whereas others are abbreviations, fixed below)

Produces this error:

Error in source("~/Desktop/projects/Bisgaard_2018/dataverse_files/results_SI.R",  : 
  invalid multibyte character in parser at line 626

There is, presumably, a solution, but I don't want to figure it out.

