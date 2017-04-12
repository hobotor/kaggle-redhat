#задача   - Red Hat
#источник - kaggle.com
#данные   - https://www.kaggle.com/c/predicting-red-hat-business-value/data


#model 8 версия 4
#валидация:
#  5% train
# 

#показатель            - AUC
#достигнутый результат - 0.989

##################################################################################################################


# load requied libraries --------------------------------------------------
library(data.table)
library(Matrix)
library(FNN)
library(ROCR)

# load and transform  data ------------------------------------------
# load people table
ppl <- fread("../input/people.csv")

ppl[,date:=as.Date(as.character(date), format = "%Y-%m-%d")]
ppl[,group.1:=group_1]
activs <- fread("../input/act_train.csv")
activs[,date:=as.Date(as.character(date), format = "%Y-%m-%d")]
date.min = activs[,min(date)]
date.max = activs[,max(date)]

#--------------------------------------- конкурс
 #activstest <- fread("../input/act_test.csv")
 #activstest[,date:=as.Date(as.character(date), format = "%Y-%m-%d")]
 #date.min = min(date.min,activs[,min(date)])
 #date.max = max(date.max,activs[,max(date)])
#---------------------------------------------------------------------
#activs

#--------------------------------------- обучение 
#разобъем трайн на две части 95 и  5%
test.index=sample.int(nrow(activs),0.05*nrow(activs),replace = F)

# строим train
train=activs[-test.index]

#------------------------------------- конкурс
# строим train
  #train=activs
#---------------------------------------

#подключаем группы покупателя
train=merge(train,ppl[,.(people_id,group_1)],by="people_id",all.x=T)

#находим моменты смены outcome  и задаем точки разбиения интервалов
setkey(train,group_1,date,outcome)
train[,':='(s_outcome=shift(outcome))]
train[,date.int2:=cumsum(abs(outcome-s_outcome)),by=group_1]

train[,group.n:=min(date),by=.(group_1,date.int2)]
train[,group.breaks:=paste(unique(group.n),collapse=","),by=.(group_1)]

#разбиваем период на интервалы
#train[,group.date:=cut(date,as.Date(c("2022-07-16",unlist(strsplit(group.breaks[1],","))[-1],"2023-09-30"), format = "%Y-%m-%d"),labels=F,include.lowest = T),by=group_1]
train[,group.date:=cut(date,as.Date(c(as.character(date.min-1),unlist(strsplit(group.breaks[1],","))[-1],as.character(date.max+1)), format = "%Y-%m-%d"),labels=F,include.lowest = T),by=group_1]

ppl[group_1!="group 17304"&!group_1 %in%  train[,unique(group_1)],new.sh:=1,by=eval(names(ppl)[c(2,4,6:41)])]    # так правильней - все кто не в трайн - новые
ppl.old=copy(ppl)

#*******************************************
ppl=copy(ppl.old)

#------------------------------------------- обучение
test=activs[test.index]
test[,outcome.old:=outcome]
test[,outcome:=NULL]

#-------------------------------------------- конкурс
 #test=activstest
#--------------------------------------------

#вариации с ключом при обучении
setkeyv(ppl,names(ppl)[c(2,4,6:41)])
#setkeyv(ppl,names(ppl)[c(6:41,4,2)])
#setkeyv(ppl,names(ppl)[c(41,2,4,6:40)])

#находим соседей
ppl[group_1 %in% train[,unique(group_1)]|new.sh==1,':='(group.new1=shift(group_1),group.new2=shift(group_1,type="lead"),new.sh1=shift(new.sh),new.sh2=shift(new.sh,type="lead"))]
ppl[group_1 %in% train[,unique(group_1)]|new.sh==1,':='(group.new3=shift(group.new1),group.new4=shift(group.new2,type="lead"),new.sh3=shift(new.sh1),new.sh4=shift(new.sh2,type="lead"))]

#подключаем только известные группы покупателя
test=merge(test,ppl[,.(people_id,group_1=ifelse(is.na(new.sh),group_1,ifelse(is.na(new.sh1),group.new1,ifelse(is.na(new.sh2),group.new2,ifelse(is.na(new.sh3),group.new3,group.new4)))))]
,by="people_id",all.x=T)

#подключаем найденые разбивки на интервалы для групп
test=merge(test,train[,.(group.breaks=group.breaks[1]),by=group_1],by="group_1",all.x=T)

#разбиваем даты в тест таблице по интервалам обучающей таблицы (сами даты в тест и в трайн не совпадают...)
test[,group.date:=cut(date,as.Date(c(as.character(date.min-1),unlist(strsplit(group.breaks[1],","))[-1],as.character(date.max+1)), format = "%Y-%m-%d"),labels=F,include.lowest = T),by=group_1]

#собственно классификация
test=merge(test,train[,.(outcome=mean(outcome,na.rm=T)),by=.(group_1,group.date)],by=c("group_1","group.date"),all.x=T)
#----------------------------------------------обучение
prop.table(table(test[,.(outcome.old,outcome)]))
#воспользуемся ROCR
pred <- prediction(test$outcome, test$outcome.old)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
performance(pred,"auc")@y.values[[1]]

#--------------------------------------------- конкурс 
#submit=test[,.(activity_id,outcome)]
#setkey(submit,activity_id)
#write.csv(submit,"Submission.csv", row.names = FALSE)
