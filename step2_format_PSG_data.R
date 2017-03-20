#####in order to compare PSG data (scored in 30 second epochs) to actigraph data (data collected and scored in 1 min epochs) the PSG data much be converted into 1 min epochs
files=Sys.glob("~/Dropbox/studies/hixon_stats_project/actigraph_psg_data/?????.csv")
files_r=gsub("data/","data/processed/r",files)
for (s in 1:length(files)){
p=read.csv(files[s])

#made sure time in right format where every time has 2 characters for hours
p$TimeStart=as.character(p$TimeStart)
for (i in 1:dim(p)[1]){
if (nchar(p$TimeStart[i])<8){
	p$TimeStart[i]=paste("0",(substr(p$TimeStart[i],1,1)),":",(substr(p$TimeStart[i],3,7)),sep="")}
	}

#create new data that reduces 30 second epochs to 1 minute
results=data.frame(c())
for (i in 1:(dim(p)[1]-1)){
if(substr(p$TimeStart[i],1,5)==substr(p$TimeStart[i+1],1,5)){
epoch=mean(c(p$Epoch[i],p$Epoch[i+1]))
time=substr(p$TimeStart[i],1,5)
stage_min=min(c(p$Stage[i],p$Stage[i+1]))
sleep_wake=ifelse(stage_min==0,0,1)
date=p$Date[i]
minute=c(epoch,as.character(time),stage_min,sleep_wake,as.character(date))
newline = data.frame(t(minute))
  results = rbind(results, newline)}
  }
colnames(results)=c("epoch","time","stage_min","sleep_wake","date")

write.csv(results,files_r[s],row.names=FALSE)	
}


