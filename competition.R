library(plyr)
library(gbm)

#set the directory location
setwd("C:\\Users\\spirit\\Desktop\\competition")
user <- read.csv("tianchi_mobile_recommend_train_user.csv")
item <- read.csv("tianchi_mobile_recommend_train_item.csv")

#Our objective is to predict what users will buy on 19th December
#19th December is Friday


#step 1: understand the data

#important statistics

#14% items which has view behavior will be bought in the following day
#18% items which has collect behavior will be bought in the following day
#7% items which has add behavior will be bought in the following day

#it's 25% on 12 December

#6% items which has behavior will be bought in the third day
#4.5% items which has behavior will be bought in the fourth day, but 8% on 12 December
#2% items which has behavior will be bought in the fifth day, view behavior 2%, add behavior 1%, collect behavior 0.3%
#2% items which has behavior will be bought in the sixth day
#1% items which has behavior will be bought in the tenth day
#0.8% items which has behavior will be bought in the 15th day
#0.4% items which has behavior will be bought in the 25th day

#above statistic is obtained from the code below
day=31
for(i in 0:day-2){
    dayone <- user[user$time %in%levels(user$time)[(i*24+1):((i+1)*24)],]
#    dayone <-dayone[dayone$behavior==3,]
    lastday <- user[user$time %in%levels(user$time)[((i+1)*24+1):((i+2)*24)],]
    lastday <-lastday[lastday$behavior_type==4,]
    print(length(intersect(paste(dayone$user_id, dayone$item_id), paste(lastday$user_id,lastday$item_id)))/length(paste(lastday$user_id,lastday$item_id)))
}

#sorting function by user_id, time
#user <- arrange(user,user_id, time) # 12312542 sort by user_id
#user <- arrange(user, time, user_id) #sort by time
#item <- arrange(item, item_id, item_category) # 445623

nrow(user[user$behavior_type==4,])
length(unique(user[user$behavior_type==4,]$item_id))
#120236 items are bought, 12 items for each person on average
#93397 different items

#first week from Sunday to Thursday
start = 5
end = 10
#behaviour in a week before Friday
userweekone <- user[user$time %in% levels(user$time)[(start * 24+1): (end * 24)],]
userweekone <- arrange(userweekone, user_id, time)
userweekonebefore <- user[user$time %in% levels(user$time)[(start * 24+1): (end * 24-7)],]
userweekoneafter <- user[user$time %in% levels(user$time)[(end * 24-7): (end * 24)],]
useronefriday <- user[user$time %in% levels(user$time)[(end * 24+1): ((end+1) * 24)],]
useronefriday <- useronefriday[useronefriday$behavior_type==4,]

#second week from Sunday to Thursday
start = 12
end = 17
userweektwo <- user[user$time %in% levels(user$time)[(start * 24+1): (end * 24)],]
userweektwo <- arrange(userweektwo, user_id, time)
userweektwobefore <- user[user$time %in% levels(user$time)[(start * 24+1): (end * 24-7)],]
userweektwoafter <- user[user$time %in% levels(user$time)[(end * 24-7): (end * 24)],]
usertwofriday <- user[user$time %in% levels(user$time)[(end * 24+1): ((end+1) * 24)],]
usertwofriday <- usertwofriday[usertwofriday$behavior_type==4,]

length(user[user$behavior_type==4,]$item_category)
length(unique(user[user$behavior_type==4,]$item_category))
#120236 categories
#4663 different categories

#extract behavior of user himself in a period of time
#the number of behaviors on behavior_type for users on items
#behavior_type: 1 view, 2 collect, 3 add, 4 buy
feature.extractor <- function(user, behavior_type){
    user.item <- unique(user[, c("user_id", "item_id","item_category")])
    user.behavior <- user[user$behavior_type==behavior_type,]
    user.behavior$behavior_type = 1
    user.behavior_item <- ddply(user.behavior, .(user_id,item_id), summarise, total=sum(behavior_type))
    user.behavior_category <- ddply(user.behavior, .(user_id,item_category), summarise, total=sum(behavior_type))
    user.item <- data.frame(user.item, behavior_type=0)

    user.item <- merge(user.item,user.behavior_item,by=c('user_id','item_id'))
    user.item <- merge(user.item, user.behavior_category[,c('user_id','item_category','total')],by=c('user_id','item_category'))
    
    item_behavior <- ''
    category_behavior <- ''
    if(behavior_type==1){
        item_behavior <- 'item_view'
        category_behavior <- 'category_view'
    }else if(behavior_type==2){
        item_behavior <- 'item_collect'
        category_behavior <- 'category_collect'
    }else if(behavior_type==3){
        item_behavior <- 'item_add'
        category_behavior <- 'category_add'
    }else{
        item_behavior <- 'item_buy'
        category_behavior <- 'category_buy'
    }
    colnames(user.item)[5:6] <- c(item_behavior,category_behavior)
    user.item[,c(1,3,5,6)]
}


#This function hasn't been finished and can't be used now
#how similar is the geo of item and the geo of user?
#extract behavior of other users similar to this user
feature.extractor_geohash <- function(user, behavior_type, aggregation){
    user.sub <- user
    user.sub$user_geohash <- substr(user.sub$user_geohash, 1, 7-aggregation)
    user.item <- unique(user.sub[, c("user_id", "item_id")])
    user.behavior <- user.sub[user.sub$behavior_type==behavior_type,]
    user.behavior$behavior_type <- 1
    user.behavior_geohash <- ddply(user.behavior, .(item_id,user_geohash), summarise, total_behavior=sum(behavior_type))
    user.geohash_number <- ddply(user.behavior,.(user_geohash),summarise, total_number=length(user_geohash))
    
    user.behavior_geohash <- merge(user.behavior_geohash, user.geohash_number, by='user_geohash')
    user.behavior_geohash <- user.behavior_geohash[,c('item_id','total_behavior','total_number')]
 
    total_behavior <- ''
    total_number <- ''
    if(behavior_type==1){
        total_behavior <- 'other_item_view'
        total_number <- 'people_view_number'
    }else if(behavior_type==2){
        total_behavior <- 'other_item_collect'
        total_number <- 'people_view_number'
    }else if(behavior_type==3){
        total_behavior <- 'other_item_add'
        total_number <- 'people_view_number'
    }else{
        total_behavior <- 'other_item_buy'
        total_number <- 'people_view_number'
    }
    
    names(user.item)[4:5] <- c(total_behavior,total_number)
    user.item <- data.frame(user.item, behavior_type=0)
    user.item <-merge(user.item, user.behavior_geohash, by=c('item_id'), all.x=TRUE)
    
    user.item
}


#step 2: extract features

#train dataset
#features of users and items in week one from Sunday to Thursday
useronefridaypredict <- read.csv("useronefridaypredict.csv")
if(!exists('useronefridaypredict')){
    useronefridaypredict <- unique(userweekone[,c('user_id','item_id')])
    useronefridaypredict = data.frame(useronefridaypredict, label=0)

    #give label=1 to those who bought something on Friday
    useronefridaypredict[paste(useronefridaypredict$user_id,useronefridaypredict$item_id) %in% paste(useronefriday$user_id, useronefriday$item_id),'label']=1
    useronefridaypredict <- arrange(useronefridaypredict,user_id,item_id)


    a <- feature.extractor(userweekonebefore, 1)
    b <- feature.extractor(userweekonebefore, 2)
    c <- feature.extractor(userweekonebefore, 3)
    d <- feature.extractor(userweekonebefore, 4)
    
    aa <- feature.extractor(userweekoneafter, 1)
    ba <- feature.extractor(userweekoneafter, 2)
    ca <- feature.extractor(userweekoneafter, 3)
    da <- feature.extractor(userweekoneafter, 4)
    
#    e <- feature.extractor_geohash(userweekone,1,3)
#    f <- feature.extractor_geohash(userweekone,2,3)
#    g <- feature.extractor_geohash(userweekone,3,3)
#    h <- feature.extractor_geohash(userweekone,4,3)
    
    useronefridaypredict <- merge(useronefridaypredict,a,by =c('user_id','item_id'),all.x=TRUE)
    useronefridaypredict <- merge(useronefridaypredict,b,by =c('user_id','item_id'),all.x=TRUE)
    useronefridaypredict <- merge(useronefridaypredict,c,by =c('user_id','item_id'),all.x=TRUE)
    useronefridaypredict <- merge(useronefridaypredict,d,by =c('user_id','item_id'),all.x=TRUE)
    useronefridaypredict <- merge(useronefridaypredict,aa,by =c('user_id','item_id'),all.x=TRUE)
    useronefridaypredict <- merge(useronefridaypredict,ba,by =c('user_id','item_id'),all.x=TRUE)
    useronefridaypredict <- merge(useronefridaypredict,ca,by =c('user_id','item_id'),all.x=TRUE)
    useronefridaypredict <- merge(useronefridaypredict,da,by =c('user_id','item_id'),all.x=TRUE)
    
#    useronefridaypredict <- merge(useronefridaypredict,e,by =c('user_id','item_id'),all.x=TRUE)
#    useronefridaypredict <- merge(useronefridaypredict,f,by =c('user_id','item_id'),all.x=TRUE)
#    useronefridaypredict <- merge(useronefridaypredict,g,by =c('user_id','item_id'),all.x=TRUE)
#    useronefridaypredict <- merge(useronefridaypredict,h,by =c('user_id','item_id'),all.x=TRUE)
    useronefridaypredict[is.na(useronefridaypredict)]=0

#select variables here using t.test anova
#make two groups based on label, and analyze which variables have influence on the label
#group1 <- useronefridaypredict[useronefridaypredict$label==0,]
#group2 <- useronefridaypredict[useronefridaypredict$label==1,]

#t.test(group1[,19], group2[,19])
#useless:7 category_collect 18 item_buy  19 category_buy
#useronefridaypredict <- useronefridaypredict[,c(-7,-18,-19)]

    write.csv(useronefridaypredict,"useronefridaypredict.csv")
}

#test dataset
#features of users and items in week two from Sunday to Thursday
usertwofridaypredict <- read.csv("usertwofridaypredict.csv")
if(!exists('usertwofridaypredict')){
    usertwofridaypredict <- unique(userweektwo[,c('user_id','item_id')])
    usertwofridaypredict = data.frame(usertwofridaypredict, label=0)
    
    #give label=1 to those who bought something on Friday
    usertwofridaypredict[paste(usertwofridaypredict$user_id,usertwofridaypredict$item_id) %in% paste(usertwofriday$user_id, usertwofriday$item_id),'label']=1
    usertwofridaypredict <- arrange(usertwofridaypredict,user_id,item_id)
    
    a2 <- feature.extractor(userweektwobefore, 1)
    b2 <- feature.extractor(userweektwobefore, 2)
    c2 <- feature.extractor(userweektwobefore, 3)
    d2 <- feature.extractor(userweektwobefore, 4)
    
    aa2 <- feature.extractor(userweektwoafter, 1)
    ba2 <- feature.extractor(userweektwoafter, 2)
    ca2 <- feature.extractor(userweektwoafter, 3)
    da2 <- feature.extractor(userweektwoafter, 4)
    
    usertwofridaypredict <- merge(usertwofridaypredict,a2,by =c('user_id','item_id'),all.x=TRUE)
    usertwofridaypredict <- merge(usertwofridaypredict,b2,by =c('user_id','item_id'),all.x=TRUE)
    usertwofridaypredict <- merge(usertwofridaypredict,c2,by =c('user_id','item_id'),all.x=TRUE)
    usertwofridaypredict <- merge(usertwofridaypredict,d2,by =c('user_id','item_id'),all.x=TRUE)
    usertwofridaypredict <- merge(usertwofridaypredict,aa2,by =c('user_id','item_id'),all.x=TRUE)
    usertwofridaypredict <- merge(usertwofridaypredict,ba2,by =c('user_id','item_id'),all.x=TRUE)
    usertwofridaypredict <- merge(usertwofridaypredict,ca2,by =c('user_id','item_id'),all.x=TRUE)
    usertwofridaypredict <- merge(usertwofridaypredict,da2,by =c('user_id','item_id'),all.x=TRUE)
    
    usertwofridaypredict[is.na(usertwofridaypredict)]=0
    write.csv(usertwofridaypredict,"usertwofridaypredict.csv")
}


#step 3: build model

#Generalized Boosted Regression Modeling
model.gbm <- gbm(label ~ item_view.x+category_view.x+item_collect.x
           +item_add.x+category_add.x+item_buy.x+category_buy.x+item_view.y+
               category_view.y+item_collect.y+category_collect.y+
               item_add.y+category_add.y
           , data = useronefridaypredict, cv.folds=3)

F1<-test(model.gbm, useronefridaypredict,useronefriday)

test <- function(model, userfridaypredict, userfriday){
    best.iter <- gbm.perf(model,method="cv")
    pr<-predict.gbm(model, userfridaypredict,best.iter)
    a<-paste(userfridaypredict[pr>(-6),]$user_id,userfridaypredict[pr>(-6),]$item_id)
    b<- paste(userfriday$user_id,userfriday$item_id)
    precision <- length(intersect(a,b))/length(a)
    recall <- length(intersect(a,b))/length(b)
    F1= 2*precision*recall/(precision+recall)
    print(F1)
}


#logistic regression
model.glm <- glm(label ~ item_view.x+category_view.x+item_collect.x
               +item_add.x+category_add.x+item_buy.x+category_buy.x+item_view.y+
                   category_view.y+item_collect.y+category_collect.y+
                   item_add.y+category_add.y, data = useronefridaypredict, family = "binomial")

res<-predict(model.glm, useronefridaypredict, type='response')

a<-paste(useronefridaypredict[res > 0.01-0.0001*i,'user_id'],useronefridaypredict[res > 0.01 -0.0001*i,'item_id'])
b<- paste(useronefriday$user_id,useronefriday$item_id)
precision <- length(intersect(a,b))/length(a)
recall <- length(intersect(a,b))/length(b)
F1= 2*precision*recall/(precision+recall)
print(F1)