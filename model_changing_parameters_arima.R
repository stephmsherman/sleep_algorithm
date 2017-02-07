#function to output the % minute by minute agreement
model_fit=function(model,data){
pred_vals=round(predict(model, type="response"),0)
good_fit=ifelse(data$sleep_wake== pred_vals, 1,0)	
print(c(mean(good_fit),AIC(model)))
}

#model(percent correct, AIC)

###DATA COMBINING ALL PARTICIPANTS ACTIGRAPH AND PSG DATA
a=read.csv("~/Dropbox/Studies/hixon_stats_project/actigraph_psg_data/processed/combined/all_combined.csv")
a$seq=seq(1:dim(a)[1])

#this the output of the standard algorithm for sleep/wake 
standard_fit=ifelse(a$sleep_wake== a$ARSLEEPP,1,0)
mean(standard_fit)


#########################
#leave one out
#with a loop take one data point
#take subset exclude row i 
#use predict function make 
#predict allows to do to new dataset of our one row

#leave one person out

###trying cross validation

###helpful library 
library(caret)

# define training control
#using leave one out cross validation
train_control <- trainControl(method="LOOCV")

# train the model 
model2 <- train(sleep_wake~ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev2 + ARACTIVP_prev3 + ARACTIVP_prev4 +ARACTIVP1+ARACTIVP2,family=binomial, data=a, trControl=train_control, method="glm")

# make predictions
predictions <- predict(model2, a)

a$p=round(predictions,0)
fit=ifelse(a$sleep_wake== a$p, 1,0)
mean(fit)
#.8217
a_sleep=a[a$sleep_wake==1,]
a_wake=a[a$sleep_wake==0,]


#RESULTS
fit_sleep=ifelse(a_sleep$sleep_wake== a_sleep$p, 1,0)
mean(fit_sleep)
#.965

#sensitivity
fit_sleep_standard=ifelse(a_sleep$sleep_wake== a_sleep$ARSLEEPP, 1,0)
mean(fit_sleep_standard)
#.863

fit_wake=ifelse(a_wake$sleep_wake== a_wake$p, 1,0)
mean(fit_wake)
#.527

#specificity
fit_wake_standard=ifelse(a_wake$sleep_wake== a_wake$ARSLEEPP, 1,0)
mean(fit_wake_standard)
#.627



###
#leave one participant out cross validation
d=a[,c("PDRID","date_time","sleep_wake","ARSLEEPP","ARACTIVP","ARACTIVP_prev1","ARACTIVP_prev2","ARACTIVP_prev3","ARACTIVP_prev4","ARACTIVP1","ARACTIVP2")]
#sink("/Volumes/schnyer/stephanie/sleep_algorithm_project/results/model_results.txt")

#create time series column
d$date_time=ts(d$date_time)
str(d$date_time)

results=data.frame(c())
sub=c(560,893,1719,1960,5481,5782,10025,10305) 
for (i in 1:8){

#removes the one participant from model before running the model 
d1=d[d$PDRID!=sub[i],]

#m1 <- glm(sleep_wake~ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev2 + ARACTIVP_prev3 + ARACTIVP_prev4 +ARACTIVP1+ARACTIVP2,family=binomial, data=d1)

#arima - arima(p,d,q)
#p = number of autoregressive terms
#d = number of non-seasonal differences
#q = number of lagged forecast error in the prediction equation
#number of preceding terms in a moving average used to smooth time series

#m1 <- glm(sleep_wake~ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev2 + ARACTIVP_prev3 + ARACTIVP_prev4 +ARACTIVP1+ARACTIVP2,family=binomial, data=d1)
#AIC(m1)
#arima(m1$residuals, order = c(1,0,0), include.mean=FALSE)

#arima(d1$sleep_wake, order = c(1,0,0))
#arima(d1$ARACTIVP, order = c(1,0,0))
library(nlme)

#autocorrelation 1
#m1 <- gls(sleep_wake~ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev2 + ARACTIVP_prev3 + ARACTIVP_prev4 +ARACTIVP1+ARACTIVP2, correlation=corAR1(form = ~1| ARACTIVP), data=d1)
#summary(m1)

m1 <- gls(sleep_wake~ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev2 + ARACTIVP_prev3 + ARACTIVP_prev4 +ARACTIVP1+ARACTIVP2, correlation=corARMA(p=1,q=0), data=d1)
summary(m1)


table(d1$sleep_wake)
pred=predict(m1, d1,type="response")
#here is where to round
#to_round=c(.729,.745,.758,.714,.739,.731,.764,.737)
to_round=c(.5,.5,.5,.5,.5,.5,.5,.5)
pred_round=ifelse(pred>to_round[i], 1,0)
table(pred_round)
#future model
#m1 <- glm(sleep_wake~ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev2 + ARACTIVP_prev3 + ARACTIVP_prev4,family=binomial, data=d1)

#m1 <- glm(sleep_wake~ARACTIVP+ARACTIVP_prev1,family=binomial, data=d1)

print(m1)
print(summary(m1))
print(AIC(m))

#the participant's data we are trying to predict
participant=d[d$PDRID==sub[i],]

#get predicted values from the model and then round the probabilities
p1=predict(m1, participant,type="response")

participant$p=ifelse(p1>to_round[i], 1,0)

cat('what we predict 0 = wake 1 = sleep')
print(table(participant$p))
cat('actual')
print(table(participant$sleep_wake))

#subsetted data when participant is actually asleep and actually awake
participant_sleep=participant[participant$sleep_wake==1,]
participant_wake=participant[participant$sleep_wake==0,]

#fit = our model
#find out accuracy of model looking across both sleep and wake
fit=ifelse(participant$sleep_wake== participant$p, 1,0)

#fit standard = the standard sleep algorithm
fit_standard=ifelse(participant$sleep_wake== participant$ARSLEEPP, 1,0)

fit_sensitivity= ifelse(participant_sleep$sleep_wake== participant_sleep$p, 1,0)
fit_sensitivity_standard= ifelse(participant_sleep$sleep_wake== participant_sleep$ARSLEEPP, 1,0)

fit_specificity= ifelse(participant_wake$sleep_wake== participant_wake$p, 1,0)
fit_specificity_standard= ifelse(participant_wake$sleep_wake== participant_wake$ARSLEEPP, 1,0)

#plot(a[a$PDRID==sub[i],]$seq,jitter(fit),col=ifelse(fit==0,"red","white"),pch=20,xaxt="n",yaxt="n",ylim=c(-.2,.3),xlab="Clock Time",ylab="Incorrectly labelled minutes across the night")
#points(a[a$PDRID==sub[i],]$seq, jitter(fit_standard),col=ifelse(fit_standard==0,"black","white"),pch=20)
#axis(side=1,at = a[a$PDRID==sub[i],]$seq ,labels=a[a$PDRID==sub[i],]$time.x,cex.axis=.5,las=1)
#legend("topright",c("Our Algorithm", "Standard Algorithm"),fill=c("red","black"))

algorithm=c(as.character(sub[i]),round(mean(fit),3),round(mean(fit_standard),3),round(mean(fit_sensitivity),3),round(mean(fit_sensitivity_standard),3),round(mean(fit_specificity),3),round(mean(fit_specificity_standard),3),length(participant_sleep$sleep_wake),length(participant_wake$sleep_wake))
newline=data.frame(t(algorithm))
results=rbind(results, newline)
}
#sink()
colnames(results)=c("subject","accuracy","accuracy_standard","sensitivity",'sensitivity_standard',"specificity","specificity_standard","min_sleep","min_wake")

#write.csv(results,"/Volumes/schnyer/stephanie/sleep_algorithm_project/results/model_fit_change_probs.csv",row.names=FALSE)
#graph
row.names(results)=results$subject
results$subject=NULL
results$accuracy=as.numeric(as.character(results$accuracy))
results$accuracy_standard =as.numeric(as.character(results$accuracy_standard))
results$sensitivity =as.numeric(as.character(results$sensitivity))
results$sensitivity_standard =as.numeric(as.character(results$sensitivity_standard))
results$specificity =as.numeric(as.character(results$specificity))
results$specificity_standard =as.numeric(as.character(results$specificity_standard))

##graph by subject
tresults=t(results[,1:6])
x=barplot(as.matrix(t(results[,1:6])),beside=TRUE,args.legend=row.names(results),las=1, main = "Model Fit- Changing Probabilities", ylab="Proportion Correctly Identified",ylim=c(0,1), xlab= "Subjects",legend=TRUE,xpd=TRUE)
#add values
text(x = x[,1], y = tresults[,1], label = round((tresults[,1]*100),0), pos = 3, cex = 0.6, col = "red")
text(x = x[,2], y = tresults[,2], label = round((tresults[,2]*100),0), pos = 3, cex = 0.6, col = "red")
text(x = x[,3], y = tresults[,3], label = round((tresults[,3]*100),0), pos = 3, cex = 0.6, col = "red")
text(x = x[,4], y = tresults[,4], label = round((tresults[,4]*100),0), pos = 3, cex = 0.6, col = "red")
text(x = x[,5], y = tresults[,5], label = round((tresults[,5]*100),0), pos = 3, cex = 0.6, col = "red")
text(x = x[,6], y = tresults[,6], label = round((tresults[,6]*100),0), pos = 3, cex = 0.6, col = "red")
text(x = x[,7], y = tresults[,7], label = round((tresults[,7]*100),0), pos = 3, cex = 0.6, col = "red")
text(x = x[,8], y = tresults[,8], label = round((tresults[,8]*100),0), pos = 3, cex = 0.6, col = "red")

#graph by parameter
x=barplot(as.matrix(results[,1:6]),beside=TRUE,args.legend=row.names(results),las=1, main = "Model Fit- Changing Probabilities", ylab="Proportion Correctly Identified",ylim=c(0,1), xlab= "Subjects")
#add values
text(x = c(x[,1],x[,2],x[,3],x[,4],x[,5],x[,6]), y = c(results[,1],results[,2],results[,3],results[,4],results[,5],results[,6]), label = c(round((results[,1]*100),0),round((results[,2]*100),0),round((results[,3]*100),0),round((results[,4]*100),0),round((results[,5]*100),0),round((results[,6]*100),0)), pos = 3, cex = 0.6, col = "red", xpd=TRUE)


