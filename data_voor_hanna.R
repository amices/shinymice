# load data
D <- read.csv("C:/Users/User/Downloads/results-survey278539.csv")

# make subset
dat <- D[,6:25]

# rename variables
names(dat) <- paste("Q", sep = "", 1:20)

# compute means
colMeans(dat)

model<-'f1=~Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8
        f2=~Q9+Q10+Q11+Q12+Q13+Q14
        f3=~Q15+Q16+Q17+Q18+Q19+Q20'

fit<-lavaan::cfa(model,data=dat)
