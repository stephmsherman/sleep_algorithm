#library that created lagged variable columns
library(DataCombine)
options(warn=-1)
#read in combined data

all=data.frame(c())
files=Sys.glob("~/Dropbox/Studies/hixon_stats_project/actigraph_psg_data/processed/combined/sub?????_combined.csv")
for (i in 1:length(files)){
c=read.csv(files[i])

#create lagged data with 4 previous minutes and 2 subsequent minutes
lagged_data = slide(c, Var = "ARACTIVP", slideBy = c(-1,-2,-3,-4,1,2))

#rename lagged variable columns that are subsequent because the format does not read into the models
names(lagged_data)[names(lagged_data)=="ARACTIVP-1"] <- "ARACTIVP_prev1"
names(lagged_data)[names(lagged_data)=="ARACTIVP-2"] <- "ARACTIVP_prev2"
names(lagged_data)[names(lagged_data)=="ARACTIVP-3"] <- "ARACTIVP_prev3"
names(lagged_data)[names(lagged_data)=="ARACTIVP-4"] <- "ARACTIVP_prev4"

#look at data to check the lag
lagged_data[,c("ARACTIVP","ARACTIVP_prev1","ARACTIVP_prev2","ARACTIVP_prev3","ARACTIVP_prev4","ARACTIVP1","ARACTIVP2")]

#trimmed the data 
l=lagged_data[5:(length(lagged_data$sleep_wake)-2),]
all=rbind(l,all)
}

write.csv(all,"~/Dropbox/Studies/hixon_stats_project/actigraph_psg_data/processed/combined/all_combined.csv",row.names=FALSE)

#make sure sleep_wake column from EEG data is a factor
lagged_data$sleep_wake= as.factor(lagged_data$sleep_wake)


#create models

#model1 = include 4 previous minutes and 2 subsequent minutes to predict whether the person was awake or asleep at that minute
model=glm(sleep_wake~ (ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev2 + ARACTIVP_prev3 + ARACTIVP_prev4 +ARACTIVP1+ARACTIVP2)^2,family=binomial, lagged_data) 

summary(model)

fitted.values_model1=round(predict(model, type="response"),0)

#same as predict function
#fitted.values_model1=round(fitted.values(model),0)

EEG=lagged_data$sleep_wake[5:(length(lagged_data$sleep_wake)-2)]

lagged_data$ARSLEEPP
good_fit1=ifelse(EEG== lagged_data$fitted.values_model1,1,0)
standard_fit=ifelse(EEG== lagged_data$ARSLEEPP,1,0)

print(mean(good_fit1))

print(AIC(model))
}

options(warn=0)




#model2 = include 4 previous minutes and 2 subsequent minutes to predict whether the person was awake or asleep at that minute
model2=glm(sleep_wake~ ARACTIVP_prev1+ ARACTIVP_prev2 + ARACTIVP_prev3 + ARACTIVP_prev4 +ARACTIVP1+ARACTIVP2,family=binomial, lagged_data)

summary(model2)

fitted.values_model2=round(fitted.values(model2),0)

good_fit2=ifelse(EEG== fitted.values_model2,1,0)
mean(good_fit2)

AIC(model2)