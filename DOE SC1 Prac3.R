#Q3)
stud <- c(60,72,55,62,50,66,78,58,69)
stream1 <- c(rep("Arts",3),rep("Science",3),rep("Commerce",3))
stream <- as.factor(stream1)
tm1 <- c(rep(c('1','2','3'),3))
tm <- as.factor(tm1)
collg1 <- c('N','S','R','R','N','S','S','R','N')
collg <- as.factor(collg1)
data.frame(stud,stream,tm,collg)
result1 <- aov(stud~stream+tm+collg)
result1
summary(result1)
TukeyHSD(result1)
plot(result1)

#Q1)

y <- c(42,47,55,51,44,45,54,52,44,50,41,46,57,47,48,56,52,49,50,43,47,49,45,54,46)
row2 <- as.factor(c(rep("1",5),rep("2",5),rep("3",5),rep("4",5),rep("5",5)))
col2 <- as.factor(rep(c('1','2','3','4','5'),5))
tret2 <- as.factor(c('A','C','B','D','E','E','B','C','A','D','C','A','D','E','B','B','D','E','C','A','D','E','A','B','C'))
data.frame(y,row2,col2,tret2)
result2 <- aov(y~row2+col2+tret2)
summary(result2)
TukeyHSD(result2)
plot(result2)

#Q2)
y <- c(7.9,8.7,7.4,7.4,7.1,8.2,6.1,8.2,7.7,7.1,8.1,5.9,7.5,8.1,6,6.4,6.2,7.5,6.9,8.5,6.8,7.7,8.5,8.5,6.7,9.9,7.3,6.4,6.4,7.3,7.3,8.3,7.3,5.8,6.4,7.7)

locat <- as.factor(c(rep("a",6),rep("b",6),rep("c",6),rep("d",6),rep("e",6),rep("f",6)))
col3 <- as.factor(rep(c('1','2','3','4','5','6'),6))
tret3 <- as.factor(c('3','5','4','1','6','2','4','2','6','5','3','1','1','3','5','6','2','4','6','1','3','2','4','5','2','4','1','3','5','6','5','6','2','4','1','3'))
data.frame(y,locat,col3,tret3)
result3 <- aov(y~locat+col3+tret3)
summary(result3)
TukeyHSD(result3)
plot(result3)















