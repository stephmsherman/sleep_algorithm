
ids= c("00560","00893","01719","01960","05481","05782","10025","10305")
for (i in 1:length(ids)){
p=read.csv(paste("~/Dropbox/Studies/hixon_stats_project/actigraph_psg_data/processed/r",ids[i],".csv",sep=""))

p$date1=as.Date(p$date,format="%m/%d/%y")

p$time=as.character(p$time)
p$date_time=as.POSIXct(paste(p$date1,p$time),format="%Y-%m-%d %H:%M")

a=read.csv(paste("~/Dropbox/Studies/hixon_stats_project/actigraph_psg_data/processed/ebe",ids[i],"_proc.csv",sep=""))
a$date_time=as.POSIXct(a$date_time,format="%Y-%m-%d %H:%M")

all=merge(p,a,by=c("date_time"))

write.csv(all, paste("~/Dropbox/Studies/hixon_stats_project/actigraph_psg_data/processed/combined/sub",ids[i],"_combined.csv",sep=""), row.names=FALSE)
}