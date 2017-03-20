#convert actigraph data to correct date_time format
##########CLOCK TIME TO SECONDS

t=read.csv("~/Dropbox/Studies/hixon_stats_project/actigraph_psg_data/time.csv")
#want to convert clock time to seconds so matched clock time from 0 to 86320
t$ARTIME=seq(0,86340,by=60)

files=Sys.glob("~/Dropbox/Studies/hixon_stats_project/actigraph_psg_data/ebe*.csv")
files_proc=gsub(".csv","_proc.csv",files)

for (i in 1:length(files)){
w=read.csv(files[i])
a=merge(w,t,by="ARTIME")

a=a[order(a$ARDATETM),]

##SAS numbering system is a second for every date since jan 1st 1960
#ref: http://stackoverflow.com/questions/24413673/sas-datetime-to-r-date-format
a$date_time=as.POSIXct(a$ARDATETM,origin='1960-01-01',tz="UTC")
write.csv(a,files_proc[i],row.names=FALSE)	
	
}

#May help convert dates and time from PSG data
p=read.csv("~/Dropbox/Studies/hixon_stats_project/actigraph_psg_data/560.csv")
p$Date=as.character(p$Date)
p$Date=as.Date(p$Date,format="%m/%d/%y")
p$date_time=as.POSIXct(paste(p$Date,p$TimeStart),format="%Y-%m-%d %H:%M:%S")
