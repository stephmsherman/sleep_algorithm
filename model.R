#function to output the % minute by minute agreement
model_fit=function(model,data){
pred_vals=round(predict(model, type="response"),0)
good_fit=ifelse(data$sleep_wake== pred_vals, 1,0)	
print(c(mean(good_fit),AIC(model)))
}

#model(percent correct, AIC)

###DATA COMBINING ALL PARTICIPANTS ACTIGRAPH AND PSG DATA
a=read.csv("~/Dropbox/Studies/hixon_stats_project/actigraph_psg_data/processed/combined/all_combined.csv")
a=a[a$PDRID!="5782",]

#this the output of the standard algorithm for sleep/wake 
standard_fit=ifelse(a$sleep_wake== a$ARSLEEPP,1,0)
mean(standard_fit)

##looking at different models
#models = include 4 previous minutes and 2 subsequent minutes to predict whether the person was awake or asleep at that minute
model=glm(sleep_wake~ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev2 + ARACTIVP_prev3 + ARACTIVP_prev4 +ARACTIVP1+ARACTIVP2,family=binomial, a) 

summary(model)

model_fit(model,a)

model_interactions=glm(sleep_wake~ (ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev2 + ARACTIVP_prev3 + ARACTIVP_prev4 +ARACTIVP1+ARACTIVP2)^2,family=binomial, a) 
summary(model_interactions)

model_fit(model_interactions,a)

model_interactions3=glm(sleep_wake~ (ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev2 + ARACTIVP_prev3 + ARACTIVP_prev4 +ARACTIVP1+ARACTIVP2)^3,family=binomial, a) 
summary(model_interactions3)

model_fit(model_interactions3,a)


model_no_prev2=glm(sleep_wake~ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev3 + ARACTIVP_prev4 +ARACTIVP1+ARACTIVP2,family=binomial, a) 
summary(model_no_prev2)

model_fit(model_no_prev2,a)

model_no_prev2_interaction=glm(sleep_wake~ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev3 + ARACTIVP_prev4 +ARACTIVP1+ARACTIVP2+ARACTIVP:ARACTIVP1,family=binomial, a) 
summary(model_no_prev2_interaction)

model_fit(model_no_prev2_interaction,a)


model_no_prev2_interaction2=glm(sleep_wake~ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev3 + ARACTIVP_prev4 +ARACTIVP1+ARACTIVP2+ARACTIVP:ARACTIVP1+ARACTIVP:ARACTIVP_prev3:ARACTIVP2,family=binomial, a) 
summary(model_no_prev2_interaction2)

model_fit(model_no_prev2_interaction2,a)

model_no_prev2_interaction3=glm(sleep_wake~ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev3 + ARACTIVP_prev4 +ARACTIVP1+ARACTIVP2+ARACTIVP:ARACTIVP1+ARACTIVP_prev1:ARACTIVP_prev4:ARACTIVP2,family=binomial, a) 
summary(model_no_prev2_interaction3)

model_fit(model_no_prev2_interaction3,a)

basic_model=glm(sleep_wake~ ARACTIVP,family=binomial, a)
summary(basic_model)
model_fit(basic_model,a)

basic_model2=glm(sleep_wake~ ARACTIVP2,family=binomial, a)
summary(basic_model2)
model_fit(basic_model2,a)

basic_model3=glm(sleep_wake~ ARACTIVP_prev4,family=binomial, a)
summary(basic_model3)
model_fit(basic_model3,a)


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
p=round(predictions,0)
fit=ifelse(a$sleep_wake== p, 1,0)
mean(fit)


###
#leave one participant out cross validation
d=a[,c("PDRID","sleep_wake","ARSLEEPP","ARACTIVP","ARACTIVP_prev1","ARACTIVP_prev2","ARACTIVP_prev3","ARACTIVP_prev4","ARACTIVP1","ARACTIVP2")]
#sink("~/Dropbox/Studies/hixon_stats_project/actigraph_psg_data/results/LGOCV_glm.txt")

results=data.frame(c())
sub=c(560,893,1719,1960,5481,10025,10305) #5782
for (i in 1:7){

#removes the one participant from model before running the model 
d1=d[a$PDRID!=sub[i],]
m1 <- glm(sleep_wake~ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev2 + ARACTIVP_prev3 + ARACTIVP_prev4 +ARACTIVP1+ARACTIVP2,family=binomial, data=d1)
print(m1)
print(summary(m1))

#the participant's data we are trying to predict
participant=a[a$PDRID==sub[i],]

#get predicted values from the model and then round the probabilities
p1=predict(m1, participant,type="response")
participant$p=round(p1,0)

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

# plot(a[a$PDRID==sub[i],]$seq,jitter(fit),col=ifelse(fit==0,"red","white"),pch=20,xaxt="n",yaxt="n",ylim=c(-.2,.3),xlab="Clock Time",ylab="Incorrectly labelled minutes across the night")
# points(a[a$PDRID==sub[i],]$seq, jitter(fit_standard),col=ifelse(fit_standard==0,"black","white"),pch=20)
# axis(side=1,at = a[a$PDRID==sub[i],]$seq ,labels=a[a$PDRID==sub[i],]$time.x,cex.axis=.5,las=1)
# legend("topright",c("Our Algorithm", "Standard Algorithm"),fill=c("red","black"))

algorithm=c(as.character(sub[i]),round(mean(fit),3),round(mean(fit_standard),3),round(mean(fit_sensitivity),3),round(mean(fit_sensitivity_standard),3),round(mean(fit_specificity),3),round(mean(fit_specificity_standard),3))
newline=data.frame(t(algorithm))
results=rbind(results, newline)
}

colnames(results)=c("subject","accuracy","accuracy_standard","sensitivity",'sensitivity_standard',"specificity","specificity_standard")

#sink()


###
#leave one participant out cross validation - no future minutes
d=a[,c("PDRID","sleep_wake","ARSLEEPP","ARACTIVP","ARACTIVP_prev1","ARACTIVP_prev2","ARACTIVP_prev3","ARACTIVP_prev4","ARACTIVP1","ARACTIVP2")]
sink("~/Dropbox/Studies/hixon_stats_project/actigraph_psg_data/results/LGOCV_glm_no_future_min.txt")
sub=c(560,893,1719,1960,5481,5782,10025,10305) 
for (i in 1:8){

d1=d[a$PDRID!=sub[i],]
m1 <- glm(sleep_wake~ARACTIVP+ARACTIVP_prev1+ ARACTIVP_prev2 + ARACTIVP_prev3 + ARACTIVP_prev4,family=binomial, data=d1)
#print(m1)
#summary(m1)
p1=predict(m1,a[a$PDRID==sub[i],],type="response")
p=round(p1,0)
fit=ifelse(a[a$PDRID==sub[i],]$sleep_wake== p, 1,0)

fit_standard=ifelse(a[a$PDRID==sub[i],]$sleep_wake==a[a$PDRID==sub[i],]$ARSLEEPP, 1,0)


print(c(as.character(sub[i]),round(mean(fit),3),round(mean(fit_standard),3)))

}

sink()





