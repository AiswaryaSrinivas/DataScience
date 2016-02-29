# The most common kind of crime is Larceny/Theft, so predict Larceny/Theft for all the test dat
train<-read.csv("train.csv")
test<-read.csv("test.csv")
sub<-read.csv("sampleSubmission.csv")
sub$Id<-test$Id
sub$WARRANTS<-0
sub$LARCENY.THEFT<-1
write.csv(sub,"basic_submission.csv")

