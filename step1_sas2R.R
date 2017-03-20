######PSG data was in SAS format so first need to convert sas7bdat to .CSV

library(sas7bdat)

##creates list of all sas files
sas=Sys.glob("~/Dropbox/Studies/hixon_stats_project/sas_actigraph_dataset/*")
sas
##in order to save the sas files as .csv and in another folder we substituted 
file_new_folder=gsub("sas_actigraph_dataset","data",sas)
file_csv=gsub("sas7bdat","csv",file_new_folder)

#created a loop to read the sas file in R and then change the path and name to save out as a .csv file
for (i in 1:length(sas)){
	
s=read.sas7bdat(sas[i])

write.csv(s, file_csv[i],row.names=FALSE)
}

