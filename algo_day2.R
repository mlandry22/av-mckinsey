trainLast5<-trainUser[,.N,.(c6,c7,c8,c9,c10,c11,c12,c13)]
trainLast4<-trainUser[,.N,.(c7,c8,c9,c10,c11,c12,c13)]
trainLast3<-trainUser[,.N,.(c8,c9,c10,c11,c12,c13)]
trainLast2<-trainUser[,.N,.(c9,c10,c11,c12,c13)]
trainLast3_1<-trainUser[,.N,.(c8,c10,c11,c12,c13)]
trainAny4<-rbind(
  trainUser[,.N,.(c7=c6,c8=c7,c9=c8,c10=c9,c11=c10,c12=c11,c13=c12)]
  ,trainUser[,.N,.(c7=c5,c8=c6,c9=c7,c10=c8,c11=c9,c12=c10,c13=c11)]
  ,trainUser[,.N,.(c7=c4,c8=c5,c9=c6,c10=c7,c11=c8,c12=c9,c13=c10)]
  ,trainUser[,.N,.(c7=c3,c8=c4,c9=c5,c10=c6,c11=c7,c12=c8,c13=c9)]
  ,trainUser[,.N,.(c7=c2,c8=c3,c9=c4,c10=c5,c11=c6,c12=c7,c13=c8)]
  ,trainUser[,.N,.(c7=c1,c8=c2,c9=c3,c10=c4,c11=c5,c12=c6,c13=c7)]
)[,.(N=sum(N)),.(c7,c8,c9,c10,c11,c12,c13)]
trainAny3<-rbind(
  trainUser[,.N,.(c8=c7,c9=c8,c10=c9,c11=c10,c12=c11,c13=c12)]
  ,trainUser[,.N,.(c8=c6,c9=c7,c10=c8,c11=c9,c12=c10,c13=c11)]
  ,trainUser[,.N,.(c8=c5,c9=c6,c10=c7,c11=c8,c12=c9,c13=c10)]
  ,trainUser[,.N,.(c8=c4,c9=c5,c10=c6,c11=c7,c12=c8,c13=c9)]
  ,trainUser[,.N,.(c8=c3,c9=c4,c10=c5,c11=c6,c12=c7,c13=c8)]
  ,trainUser[,.N,.(c8=c2,c9=c3,c10=c4,c11=c5,c12=c6,c13=c7)]
  ,trainUser[,.N,.(c8=c1,c9=c2,c10=c3,c11=c4,c12=c5,c13=c6)]
)[,.(N=sum(N)),.(c8,c9,c10,c11,c12,c13)]
trainAny2<-rbind(
  trainUser[,.N,.(c9=c8,c10=c9,c11=c10,c12=c11,c13=c12)]
  ,trainUser[,.N,.(c9=c7,c10=c8,c11=c9,c12=c10,c13=c11)]
  ,trainUser[,.N,.(c9=c6,c10=c7,c11=c8,c12=c9,c13=c10)]
  ,trainUser[,.N,.(c9=c5,c10=c6,c11=c7,c12=c8,c13=c9)]
  ,trainUser[,.N,.(c9=c4,c10=c5,c11=c6,c12=c7,c13=c8)]
  ,trainUser[,.N,.(c9=c3,c10=c4,c11=c5,c12=c6,c13=c7)]
  ,trainUser[,.N,.(c9=c2,c10=c3,c11=c4,c12=c5,c13=c6)]
  ,trainUser[,.N,.(c9=c1,c10=c2,c11=c3,c12=c4,c13=c5)]
)[,.(N=sum(N)),.(c9,c10,c11,c12,c13)]
trainAny1<-rbind(
  trainUser[,.N,.(c10=c9,c11=c10,c12=c11,c13=c12)]
  ,trainUser[,.N,.(c10=c8,c11=c9,c12=c10,c13=c11)]
  ,trainUser[,.N,.(c10=c7,c11=c8,c12=c9,c13=c10)]
  ,trainUser[,.N,.(c10=c6,c11=c7,c12=c8,c13=c9)]
  ,trainUser[,.N,.(c10=c5,c11=c6,c12=c7,c13=c8)]
  ,trainUser[,.N,.(c10=c4,c11=c5,c12=c6,c13=c7)]
  ,trainUser[,.N,.(c10=c3,c11=c4,c12=c5,c13=c6)]
  ,trainUser[,.N,.(c10=c2,c11=c3,c12=c4,c13=c5)]
  ,trainUser[,.N,.(c10=c1,c11=c2,c12=c2,c13=c3)]
)[,.(N=sum(N)),.(c10,c11,c12,c13)]

idCount<-c("start"=0)

testMatches5<-merge(testUser,trainLast5,by=c("c6","c7","c8","c9","c10"),allow.cartesian = TRUE)[order(user_id,-N)]
testMatches5<-testMatches5[!apply(testMatches5[,.(allCourses,c11.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches5<-testMatches5[!apply(testMatches5[,.(allCourses,c12.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches5<-testMatches5[!apply(testMatches5[,.(allCourses,c13.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches5[,id:=.I]
testMatches5[,minId:=min(id),user_id]
testMatches5<-testMatches5[minId==id]
ids<-testMatches5[,unique(user_id)]
idCount<-c(idCount,"testMatches5"=length(unique(ids)))

testMatches4<-merge(testUser,trainLast4,by=c("c7","c8","c9","c10"),allow.cartesian = TRUE)[order(user_id,-N)]
testMatches4<-testMatches4[!apply(testMatches4[,.(allCourses,c11.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches4<-testMatches4[!apply(testMatches4[,.(allCourses,c12.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches4<-testMatches4[!apply(testMatches4[,.(allCourses,c13.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches4[,id:=.I]
testMatches4[,minId:=min(id),user_id]
testMatches4<-testMatches4[minId==id]
ids<-c(ids,testMatches4[,unique(user_id)])
idCount<-c(idCount,"testMatches4"=length(unique(ids)))

testMatches4a<-merge(testUser[!user_id %in% ids]
                     ,trainAny4,by=c("c7","c8","c9","c10"),allow.cartesian = TRUE)[order(user_id,-N)]
testMatches4a<-testMatches4a[!apply(testMatches4a[,.(allCourses,c11.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches4a<-testMatches4a[!apply(testMatches4a[,.(allCourses,c12.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches4a<-testMatches4a[!apply(testMatches4a[,.(allCourses,c13.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches4a[,id:=.I]
testMatches4a[,minId:=min(id),user_id]
testMatches4a<-testMatches4a[minId==id]
ids<-c(ids,testMatches4a[,unique(user_id)])
idCount<-c(idCount,"testMatches4a"=length(unique(ids)))

testMatches3<-merge(testUser[!user_id %in% ids]
                    ,trainLast3,by=c("c8","c9","c10"),allow.cartesian = TRUE)[order(user_id,-N)]
testMatches3<-testMatches3[!apply(testMatches3[,.(allCourses,c11.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches3<-testMatches3[!apply(testMatches3[,.(allCourses,c12.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches3<-testMatches3[!apply(testMatches3[,.(allCourses,c13.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches3[,id:=.I]
testMatches3[,minId:=min(id),user_id]
testMatches3<-testMatches3[minId==id]
ids<-c(ids,testMatches3[,unique(user_id)])
idCount<-c(idCount,"testMatches3"=length(unique(ids)))

testMatches3a<-merge(testUser[!user_id %in% ids]
                     ,trainAny3,by=c("c8","c9","c10"),allow.cartesian = TRUE)[order(user_id,-N)]
testMatches3a<-testMatches3a[!apply(testMatches3a[,.(allCourses,c11.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches3a<-testMatches3a[!apply(testMatches3a[,.(allCourses,c12.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches3a<-testMatches3a[!apply(testMatches3a[,.(allCourses,c13.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches3a[,id:=.I]
testMatches3a[,minId:=min(id),user_id]
testMatches3a<-testMatches3a[minId==id]
ids<-c(ids,testMatches3a[,unique(user_id)])
idCount<-c(idCount,"testMatches3a"=length(unique(ids)))

testMatches2<-merge(testUser[!user_id %in% ids]
                    ,trainLast2,by=c("c9","c10"),allow.cartesian = TRUE)[order(user_id,-N)]
testMatches2<-testMatches2[!apply(testMatches2[,.(allCourses,c11.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches2<-testMatches2[!apply(testMatches2[,.(allCourses,c12.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches2<-testMatches2[!apply(testMatches2[,.(allCourses,c13.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches2[,id:=.I]
testMatches2[,minId:=min(id),user_id]
testMatches2<-testMatches2[minId==id]
ids<-c(ids,testMatches2[,unique(user_id)])
idCount<-c(idCount,"testMatches2"=length(unique(ids)))

testMatches2b<-merge(testUser[!user_id %in% ids]
                     ,trainLast3_1,by=c("c8","c10"),allow.cartesian = TRUE)[order(user_id,-N)]
testMatches2b<-testMatches2b[!apply(testMatches2b[,.(allCourses,c11.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches2b<-testMatches2b[!apply(testMatches2b[,.(allCourses,c12.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches2b<-testMatches2b[!apply(testMatches2b[,.(allCourses,c13.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches2b[,id:=.I]
testMatches2b[,minId:=min(id),user_id]
testMatches2b<-testMatches2b[minId==id]
ids<-c(ids,testMatches2b[,unique(user_id)])
idCount<-c(idCount,"testMatches2b"=length(unique(ids)))

testMatches2a<-merge(testUser[!user_id %in% ids]
                     ,trainAny2,by=c("c9","c10"),allow.cartesian = TRUE)[order(user_id,-N)]
testMatches2a<-testMatches2a[!apply(testMatches2a[,.(allCourses,c11.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches2a<-testMatches2a[!apply(testMatches2a[,.(allCourses,c12.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches2a<-testMatches2a[!apply(testMatches2a[,.(allCourses,c13.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches2a[,id:=.I]
testMatches2a[,minId:=min(id),user_id]
testMatches2a<-testMatches2a[minId==id]
ids<-c(ids,testMatches2a[,unique(user_id)])
idCount<-c(idCount,"testMatches2a"=length(unique(ids)))

testMatches2f<-merge(testUser[!user_id %in% ids]
                     ,trainAny2,by=c("c9","c10"),allow.cartesian = TRUE)[order(user_id,-N)]
testMatches2f[,id:=.I]
testMatches2f[,minId:=min(id),user_id]
testMatches2f<-testMatches2f[minId==id]
ids<-c(ids,testMatches2f[,unique(user_id)])
idCount<-c(idCount,"testMatches2f"=length(unique(ids)))

testMatches1<-merge(testUser[!user_id %in% ids],trainAny1
                       ,by=c("c10"),allow.cartesian = TRUE)[order(user_id,-N)]
testMatches1<-testMatches1[!apply(testMatches1[,.(allCourses,c11.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches1<-testMatches1[!apply(testMatches1[,.(allCourses,c12.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches1<-testMatches1[!apply(testMatches1[,.(allCourses,c13.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches1[,id:=.I]
testMatches1[,minId:=min(id),user_id]
testMatches1<-testMatches1[minId==id]
ids<-c(ids,testMatches1[,unique(user_id)])
idCount<-c(idCount,"testMatches1"=length(unique(ids)))

testMatches3_789<-merge(testUser[!user_id %in% ids]
                        ,trainAny3[,.(c7=c8,c8=c9,c9=c10,c11,c12,c13,N)]
                        ,by=c("c7","c8","c9"),allow.cartesian = TRUE)[order(user_id,-N)]
testMatches3_789<-testMatches3_789[!apply(testMatches3_789[,.(allCourses,c11.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches3_789<-testMatches3_789[!apply(testMatches3_789[,.(allCourses,c12.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches3_789<-testMatches3_789[!apply(testMatches3_789[,.(allCourses,c13.y)],1,function(x) grepl(x[2],x[1],fixed=TRUE))]
testMatches3_789[,id:=.I]
testMatches3_789[,minId:=min(id),user_id]
testMatches3_789<-testMatches3_789[minId==id & N>1]
ids<-c(ids,testMatches3_789[,unique(user_id)])
idCount<-c(idCount,"testMatches3_789"=length(unique(ids)))

testMatches1all<-merge(testUser[!user_id %in% ids],trainAny1
                    ,by=c("c10"),allow.cartesian = TRUE)[order(user_id,-N)]
testMatches1all[,id:=.I]
testMatches1all[,minId:=min(id),user_id]
testMatches1all<-testMatches1all[minId==id]
ids<-c(ids,testMatches1all[,unique(user_id)])
idCount<-c(idCount,"testMatches1all"=length(unique(ids)))

testMatchRows<-testMatches4[,.(user_sequence=paste0(user_id,"_11"),challenge=c11.y)]
testMatchRows<-rbind(testMatchRows,testMatches4[,.(user_sequence=paste0(user_id,"_12"),challenge=c12.y)])
testMatchRows<-rbind(testMatchRows,testMatches4[,.(user_sequence=paste0(user_id,"_13"),challenge=c13.y)])

testMatchRows<-rbind(testMatchRows,testMatches4a[,.(user_sequence=paste0(user_id,"_11"),challenge=c11.y)])
testMatchRows<-rbind(testMatchRows,testMatches4a[,.(user_sequence=paste0(user_id,"_12"),challenge=c12.y)])
testMatchRows<-rbind(testMatchRows,testMatches4a[,.(user_sequence=paste0(user_id,"_13"),challenge=c13.y)])

testMatchRows<-rbind(testMatchRows,testMatches3[,.(user_sequence=paste0(user_id,"_11"),challenge=c11.y)])
testMatchRows<-rbind(testMatchRows,testMatches3[,.(user_sequence=paste0(user_id,"_12"),challenge=c12.y)])
testMatchRows<-rbind(testMatchRows,testMatches3[,.(user_sequence=paste0(user_id,"_13"),challenge=c13.y)])

testMatchRows<-rbind(testMatchRows,testMatches3a[,.(user_sequence=paste0(user_id,"_11"),challenge=c11.y)])
testMatchRows<-rbind(testMatchRows,testMatches3a[,.(user_sequence=paste0(user_id,"_12"),challenge=c12.y)])
testMatchRows<-rbind(testMatchRows,testMatches3a[,.(user_sequence=paste0(user_id,"_13"),challenge=c13.y)])

testMatchRows<-rbind(testMatchRows,testMatches2[,.(user_sequence=paste0(user_id,"_11"),challenge=c11.y)])
testMatchRows<-rbind(testMatchRows,testMatches2[,.(user_sequence=paste0(user_id,"_12"),challenge=c12.y)])
testMatchRows<-rbind(testMatchRows,testMatches2[,.(user_sequence=paste0(user_id,"_13"),challenge=c13.y)])

testMatchRows<-rbind(testMatchRows,testMatches2b[,.(user_sequence=paste0(user_id,"_11"),challenge=c11.y)])
testMatchRows<-rbind(testMatchRows,testMatches2b[,.(user_sequence=paste0(user_id,"_12"),challenge=c12.y)])
testMatchRows<-rbind(testMatchRows,testMatches2b[,.(user_sequence=paste0(user_id,"_13"),challenge=c13.y)])

testMatchRows<-rbind(testMatchRows,testMatches2a[,.(user_sequence=paste0(user_id,"_11"),challenge=c11.y)])
testMatchRows<-rbind(testMatchRows,testMatches2a[,.(user_sequence=paste0(user_id,"_12"),challenge=c12.y)])
testMatchRows<-rbind(testMatchRows,testMatches2a[,.(user_sequence=paste0(user_id,"_13"),challenge=c13.y)])

testMatchRows<-rbind(testMatchRows,testMatches2f[,.(user_sequence=paste0(user_id,"_11"),challenge=c11.y)])
testMatchRows<-rbind(testMatchRows,testMatches2f[,.(user_sequence=paste0(user_id,"_12"),challenge=c12.y)])
testMatchRows<-rbind(testMatchRows,testMatches2f[,.(user_sequence=paste0(user_id,"_13"),challenge=c13.y)])

testMatchRows<-rbind(testMatchRows,testMatches1[,.(user_sequence=paste0(user_id,"_11"),challenge=c11.y)])
testMatchRows<-rbind(testMatchRows,testMatches1[,.(user_sequence=paste0(user_id,"_12"),challenge=c12.y)])
testMatchRows<-rbind(testMatchRows,testMatches1[,.(user_sequence=paste0(user_id,"_13"),challenge=c13.y)])

testMatchRows<-rbind(testMatchRows,testMatches3_789[,.(user_sequence=paste0(user_id,"_11"),challenge=c11.y)])
testMatchRows<-rbind(testMatchRows,testMatches3_789[,.(user_sequence=paste0(user_id,"_12"),challenge=c12.y)])
testMatchRows<-rbind(testMatchRows,testMatches3_789[,.(user_sequence=paste0(user_id,"_13"),challenge=c13.y)])

testMatchRows<-rbind(testMatchRows,testMatches1all[,.(user_sequence=paste0(user_id,"_11"),challenge=c11.y)])
testMatchRows<-rbind(testMatchRows,testMatches1all[,.(user_sequence=paste0(user_id,"_12"),challenge=c12.y)])
testMatchRows<-rbind(testMatchRows,testMatches1all[,.(user_sequence=paste0(user_id,"_13"),challenge=c13.y)])

testMatchRows[,id:=.I]
testMatchRows[,minId:=min(id),user_sequence]
testMatchRows<-testMatchRows[minId==id]

submission<-merge(ss[,.(user_sequence,default_challenge=challenge)],testMatchRows,by="user_sequence",all.x=TRUE)
t(t(idCount))
dim(submission)  ##119196
fwrite(submission[,.(user_sequence,challenge=ifelse(is.na(challenge),default_challenge,challenge))]
       ,"sub20.csv")
system("zip code.zip data_prep.R algo_day2.R")
