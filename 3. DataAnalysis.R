


library(openxlsx)
library(tableone)
library(reshape2)

out.dir<-file.path("C:\\Users\\Brett Carpenter\\Documents")
dat0<-read.csv(file.path(out.dir,"DataTask_sanitized.csv"))
names(dat)

dat<-dat0[,c("id","casenumber","age","race","offense","sex","transient","treatment","actiondescription","actionamount",
             "actionstatus","appeared")]

tst <- dat[duplicated(dat$id),]
tst2<-tst[order(tst$id),]  #looks like some ids repeat but the case numbers are unique, 
                            #for this study, will elect to treat case numbers separately 
                            #so doesn't matter whether id repeat or not

dat$transient<-ifelse(dat$transient=="TRANSIENT","TRANSIENT","MISSING")
dat$appeared<-as.character(as.factor(dat$appeared))
length(unique(dat$offense))
# [1] 284 different offenses, so will not summarize by offense
length(unique(dat$actiondescription))
# [1] 132

vars<-c("age","race","sex","transient","actionamount",
          "actionstatus","appeared")

catvars<-c("race","transient","actionstatus","appeared")

summ.overall<-CreateTableOne(vars = vars, data = dat, factorVars = catvars)
# write.csv(print(summ.overall),file.path("DataTask_sanitize_overall_summ.csv"))
summ.bytrt <- CreateTableOne(vars = vars, strata = "treatment" , data = dat, factorVars = catvars)

write.csv(cbind(print(summ.overall),print(summ.bytrt)),file.path("DataTask_sanitize_summ.csv"))

#visualizations

png(file.path("DataTask_sanitize-race.png"),res=300,unit="in",width=5,height=5)
ggplot(dat, aes(x = race, fill = appeared)) + geom_bar(position = "dodge")
dev.off()


png(file.path("DataTask_sanitize-transient.png"),res=300,unit="in",width=5,height=5)
ggplot(dat, aes(x = transient, fill = appeared)) + geom_bar(position = "dodge")
dev.off()

png(file.path("DataTask_sanitize-sex.png"),res=300,unit="in",width=5,height=5)
ggplot(dat, aes(x = sex, fill = appeared)) + geom_bar(position = "dodge")
dev.off()


##models

fit1 <- glm(as.numeric(appeared) ~treatment,family=binomial(link='logit'),data=dat)
summary(fit1)  #treatment is significant

exp(coef(fit1))
#
fit2<-glm(as.numeric(appeared) ~treatment+age+race+sex+transient+actionamount+offense+actionstatus,
          family=binomial(link='logit'),data=dat)  

summary(fit2)##offense and actionstatus are not significant so remove from model


fit3<-glm(as.numeric(appeared) ~treatment+age+race+sex+transient+actionamount,
          family=binomial(link='logit'),data=dat)  

summary(fit3)#remove sex, not significant

fit4<-glm(as.numeric(appeared) ~treatment+age+race+transient+actionamount,
            family=binomial(link='logit'),data=dat) 

summary(fit4)

#gets odds ratio = exp(coefficient)
write.csv(exp(coef(fit4)),file.path("ODDS.csv"))



