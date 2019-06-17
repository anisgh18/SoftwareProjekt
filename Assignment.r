library(ggplot2)
setwd("C:/Users/zag/Desktop")
currentDataSet <- read.table("om1701_rttask_all.dat", header=TRUE)
DataSetCleaned <- currentDataSet[currentDataSet$key_error=="correct",]
DataSetCleaned <- DataSetCleaned[DataSetCleaned$time_error=="Normal",]
count1 <- length(currentDataSet$trial_no)
count2 <- length(which(currentDataSet$key_error == "error",))
count3 <- length(which(currentDataSet$time_error=="Late",))
cat("The total number of trials is: ", count1, ". The total number of error trials is: ", count2, ". The total number of time latencies is : ", count3)
p100 <- DataSetCleaned[DataSetCleaned$sub == "100",]
p101 <- DataSetCleaned[DataSetCleaned$sub == "101",]
p102 <- DataSetCleaned[DataSetCleaned$sub == "102",]
p103 <- DataSetCleaned[DataSetCleaned$sub == "103",]
p104 <- DataSetCleaned[DataSetCleaned$sub == "104",]
p105 <- DataSetCleaned[DataSetCleaned$sub == "105",]
p200 <- DataSetCleaned[DataSetCleaned$sub == "200",]
p201 <- DataSetCleaned[DataSetCleaned$sub == "201",]
p202 <- DataSetCleaned[DataSetCleaned$sub == "202",]
p203 <- DataSetCleaned[DataSetCleaned$sub == "203",]
p204 <- DataSetCleaned[DataSetCleaned$sub == "204",]
p205 <- DataSetCleaned[DataSetCleaned$sub == "205",]
p206 <- DataSetCleaned[DataSetCleaned$sub == "206",]
p207 <- DataSetCleaned[DataSetCleaned$sub == "207",]
p208 <- DataSetCleaned[DataSetCleaned$sub == "208",]
p209 <- DataSetCleaned[DataSetCleaned$sub == "209",]
p210 <- DataSetCleaned[DataSetCleaned$sub == "210",]
p211 <- DataSetCleaned[DataSetCleaned$sub == "211",]
subject <- c(100,101,102,103,104,105,200,201,202,203,204,205,206,207,208,209,210,211)
meanRToverall <- c(mean(p100$RT),mean(p101$RT),mean(p102$RT),mean(p103$RT),mean(p104$RT),mean(p105$RT),mean(p200$RT),mean(p201$RT),mean(p202$RT),mean(p203$RT),mean(p204$RT),mean(p205$RT),mean(p206$RT),mean(p207$RT),mean(p208$RT),mean(p209$RT),mean(p210$RT),mean(p211$RT))
fetchCongruent <- function (dataSet){
  dataSet[dataSet$congruency == "Congruent",]
}
fetchIncongruent <- function (dataSet){
  dataSet[dataSet$congruency == "Incongruent",]
}
p100con <- fetchCongruent(p100)
p100incon <- fetchIncongruent(p100)
p101con <- fetchCongruent(p101)
p101incon <- fetchIncongruent(p101)
p102con <- fetchCongruent(p102)
p102incon <- fetchIncongruent(p102)
p103con <- fetchCongruent(p103)
p103incon <- fetchIncongruent(p103)
p104con <- fetchCongruent(p104)
p104incon <- fetchIncongruent(p104)
p105con <- fetchCongruent(p105)
p105incon <- fetchIncongruent(p105)
p200con <- fetchCongruent(p200)
p200incon <- fetchIncongruent(p200)
p201con <- fetchCongruent(p201)
p201incon <- fetchIncongruent(p201)
p202con <- fetchCongruent(p202)
p202incon <- fetchIncongruent(p202)
p203con <- fetchCongruent(p203)
p203incon <- fetchIncongruent(p203)
p204con <- fetchCongruent(p204)
p204incon <- fetchIncongruent(p204)
p205con <- fetchCongruent(p205)
p205incon <- fetchIncongruent(p205)
p206con <- fetchCongruent(p206)
p206incon <- fetchIncongruent(p206)
p207con <- fetchCongruent(p207)
p207incon <- fetchIncongruent(p207)
p208con <- fetchCongruent(p208)
p208incon <- fetchIncongruent(p208)
p209con <- fetchCongruent(p209)
p209incon <- fetchIncongruent(p209)
p210con <- fetchCongruent(p210)
p210incon <- fetchIncongruent(p210)
p211incon <- fetchIncongruent(p211)
p211con <- fetchCongruent(p211)
meanRTcon <- c(mean(p100con$RT),mean(p101con$RT),mean(p102con$RT),mean(p103con$RT),mean(p104con$RT),mean(p105con$RT),mean(p200con$RT),mean(p201con$RT),mean(p202con$RT),mean(p203con$RT),mean(p204con$RT),mean(p205con$RT),mean(p206con$RT),mean(p207con$RT),mean(p208con$RT),mean(p209con$RT),mean(p210con$RT),mean(p211con$RT))
meanRTincon <- c(mean(p100incon$RT),mean(p101incon$RT),mean(p102incon$RT),mean(p103incon$RT),mean(p104incon$RT),mean(p105incon$RT),mean(p200incon$RT),mean(p201incon$RT),mean(p202incon$RT),mean(p203incon$RT),mean(p204incon$RT),mean(p205incon$RT),mean(p206incon$RT),mean(p207incon$RT),mean(p208incon$RT),mean(p209incon$RT),mean(p210incon$RT),mean(p211incon$RT))
meanRTdiff <- c(mean(p100incon$RT)-mean(p100con$RT),mean(p101incon$RT)-mean(p101con$RT),mean(p102incon$RT)-mean(p102con$RT),mean(p103incon$RT)-mean(p103con$RT),mean(p104incon$RT)-mean(p104con$RT),mean(p105incon$RT)-mean(p105con$RT),mean(p200incon$RT)-mean(p200con$RT),mean(p201incon$RT)-mean(p201con$RT),mean(p202incon$RT)-mean(p202con$RT),mean(p203incon$RT)-mean(p203con$RT),mean(p204incon$RT)-mean(p204con$RT),mean(p205incon$RT)-mean(p205con$RT),mean(p206incon$RT)-mean(p206con$RT),mean(p207incon$RT)-mean(p207con$RT),mean(p208incon$RT)-mean(p208con$RT),mean(p209incon$RT)-mean(p209con$RT),mean(p210incon$RT)-mean(p210con$RT),mean(p211incon$RT)-mean(p211con$RT))
nOfTrialsTotal <- c(length(which(currentDataSet$sub == "100")),length(which(currentDataSet$sub == "101")),length(which(currentDataSet$sub == "102")),length(which(currentDataSet$sub == "103")),length(which(currentDataSet$sub == "104")),length(which(currentDataSet$sub == "105")),length(which(currentDataSet$sub == "200")),length(which(currentDataSet$sub == "201")),length(which(currentDataSet$sub == "202")),length(which(currentDataSet$sub == "203")),length(which(currentDataSet$sub == "204")),length(which(currentDataSet$sub == "205")),length(which(currentDataSet$sub == "206")),length(which(currentDataSet$sub == "207")),length(which(currentDataSet$sub == "208")),length(which(currentDataSet$sub == "209")),length(which(currentDataSet$sub == "210")),length(which(currentDataSet$sub == "211")))
nOfTrialsNoErrors <- c(length(which(DataSetCleaned$sub == "100")),length(which(DataSetCleaned$sub == "101")),length(which(DataSetCleaned$sub == "102")),length(which(DataSetCleaned$sub == "103")),length(which(DataSetCleaned$sub == "104")),length(which(DataSetCleaned$sub == "105")),length(which(DataSetCleaned$sub == "200")),length(which(DataSetCleaned$sub == "201")),length(which(DataSetCleaned$sub == "202")),length(which(DataSetCleaned$sub == "203")),length(which(DataSetCleaned$sub == "204")),length(which(DataSetCleaned$sub == "205")),length(which(DataSetCleaned$sub == "206")),length(which(DataSetCleaned$sub == "207")),length(which(DataSetCleaned$sub == "208")),length(which(DataSetCleaned$sub == "209")),length(which(DataSetCleaned$sub == "210")),length(which(DataSetCleaned$sub == "211")))
standardDeviationCon <- c(sd(p100con$RT),sd(p101con$RT),sd(p102con$RT),sd(p103con$RT),sd(p104con$RT),sd(p105con$RT),sd(p200con$RT),sd(p201con$RT),sd(p202con$RT),sd(p203con$RT),sd(p204con$RT),sd(p205con$RT),sd(p206con$RT),sd(p207con$RT),sd(p208con$RT),sd(p209con$RT),sd(p210con$RT),sd(p211con$RT))
standardDeviationIncon <- c(sd(p100incon$RT),sd(p101incon$RT),sd(p102incon$RT),sd(p103incon$RT),sd(p104incon$RT),sd(p105incon$RT),sd(p200incon$RT),sd(p201incon$RT),sd(p202incon$RT),sd(p203incon$RT),sd(p204incon$RT),sd(p205incon$RT),sd(p206incon$RT),sd(p207incon$RT),sd(p208incon$RT),sd(p209incon$RT),sd(p210incon$RT),sd(p211incon$RT))
standardDeviationDiff <- c(sd(p100incon$RT)-sd(p100con$RT),sd(p101incon$RT)-sd(p101con$RT),sd(p102incon$RT)-sd(p102con$RT),sd(p103incon$RT)-sd(p103con$RT),sd(p104incon$RT)-sd(p104con$RT),sd(p105incon$RT)-sd(p105con$RT),sd(p200incon$RT)-sd(p200con$RT),sd(p201incon$RT)-sd(p201con$RT),sd(p202incon$RT)-sd(p202con$RT),sd(p203incon$RT)-sd(p203con$RT),sd(p204incon$RT)-sd(p204con$RT),sd(p205incon$RT)-sd(p205con$RT),sd(p206incon$RT)-sd(p206con$RT),sd(p207incon$RT)-sd(p207con$RT),sd(p208incon$RT)-sd(p208con$RT),sd(p209incon$RT)-sd(p209con$RT),sd(p210incon$RT)-sd(p210con$RT),sd(p211incon$RT)-sd(p211con$RT))
sem <- function(x) sd(x)/sqrt(length(x))
SEMmeanRTCon <- sem(meanRTcon)
SEMmeanRTIncon <- sem(meanRTincon)
SEMRTdiff <- sem(meanRTdiff)

argnames <- c("meanRTcon","meanRTincon")
plotRTmean <- barplot(c(mean(meanRTcon),mean(meanRTincon)),names.arg=argnames,beside=true,ylim =c(0, mean(meanRTincon)*1.5),cex.names = 0.75, ylab = "RT", border = "black", axes = TRUE,col = rainbow(4))
arrows(plotRTmean,mean(meanRTcon)+SEMmeanRTCon,
       plotRTmean,mean(meanRTcon)-SEMmeanRTCon,
       angle=90, code=3, length =0.05,lwd = 1.5)


argnames1 <- c("meanRTdiff")
plotRTdiff <- barplot(c(mean(meanRTdiff)),names.arg=argnames1,beside=true,cex.names = 0.75, ylab = "RT", border = "black", axes = TRUE,col = rainbow(4),ylim = c(0,15))
arrows(plotRTdiff,mean(meanRTdiff)+SEMRTdiff,
       plotRTdiff,mean(meanRTdiff)-SEMRTdiff,
       angle=90, code=3, length =0.05,lwd = 1.5)


slowRTindirectp100 <- p100[p100$RT<median(p100$RT),]
fastRTindirectp100 <- p100[p100$RT>=median(p100$RT),]
slowRTindirectConp100 <- slowRTindirectp100[slowRTindirectp100$congruency == "Congruent",]
fastRTindirectInConp100 <- fastRTindirectp100[fastRTindirectp100$congruency == "Incongruent",]
PslowRTindirectConp100 <- length(slowRTindirectConp100$sub)/length(slowRTindirectp100$sub)
PfastRTindirectInconp100 <- length(fastRTindirectInConp100$sub)/length(fastRTindirectp100$sub)
meanPcorrp100 <- mean(PfastRTindirectInconp100,PslowRTindirectConp100)


slowRTindirectp101 <- p101[p101$RT<median(p101$RT),]
fastRTindirectp101 <- p101[p101$RT>=median(p101$RT),]
slowRTindirectConp101 <- slowRTindirectp101[slowRTindirectp101$congruency == "Congruent",]
fastRTindirectInConp101 <- fastRTindirectp101[fastRTindirectp101$congruency == "Incongruent",]
PslowRTindirectConp101 <- length(slowRTindirectConp101$sub)/length(slowRTindirectp101$sub)
PfastRTindirectInconp101 <- length(fastRTindirectInConp101$sub)/length(fastRTindirectp101$sub)
meanPcorrp101 <- mean(PfastRTindirectInconp101,PslowRTindirectConp101)



slowRTindirectp102 <- p102[p102$RT<median(p102$RT),]
fastRTindirectp102 <- p102[p102$RT>=median(p102$RT),]
slowRTindirectConp102 <- slowRTindirectp102[slowRTindirectp102$congruency == "Congruent",]
fastRTindirectInConp102 <- fastRTindirectp102[fastRTindirectp102$congruency == "Incongruent",]
PslowRTindirectConp102 <- length(slowRTindirectConp102$sub)/length(slowRTindirectp102$sub)
PfastRTindirectInconp102 <- length(fastRTindirectInConp102$sub)/length(fastRTindirectp102$sub)
meanPcorrp102 <- mean(PfastRTindirectInconp102,PslowRTindirectConp102)

slowRTindirectp103 <- p103[p103$RT<median(p103$RT),]
fastRTindirectp103 <- p103[p103$RT>=median(p103$RT),]
slowRTindirectConp103 <- slowRTindirectp103[slowRTindirectp103$congruency == "Congruent",]
fastRTindirectInConp103 <- fastRTindirectp103[fastRTindirectp103$congruency == "Incongruent",]
PslowRTindirectConp103 <- length(slowRTindirectConp103$sub)/length(slowRTindirectp103$sub)
PfastRTindirectInconp103 <- length(fastRTindirectInConp103$sub)/length(fastRTindirectp103$sub)
meanPcorrp103 <- mean(PfastRTindirectInconp103,PslowRTindirectConp103)

slowRTindirectp104 <- p104[p104$RT<median(p104$RT),]
fastRTindirectp104 <- p104[p104$RT>=median(p104$RT),]
slowRTindirectConp104 <- slowRTindirectp104[slowRTindirectp104$congruency == "Congruent",]
fastRTindirectInConp104 <- fastRTindirectp104[fastRTindirectp104$congruency == "Incongruent",]
PslowRTindirectConp104 <- length(slowRTindirectConp104$sub)/length(slowRTindirectp104$sub)
PfastRTindirectInconp104 <- length(fastRTindirectInConp104$sub)/length(fastRTindirectp104$sub)
meanPcorrp104 <- mean(PfastRTindirectInconp104,PslowRTindirectConp104)

slowRTindirectp105 <- p105[p105$RT<median(p105$RT),]
fastRTindirectp105 <- p105[p105$RT>=median(p105$RT),]
slowRTindirectConp105 <- slowRTindirectp105[slowRTindirectp105$congruency == "Congruent",]
fastRTindirectInConp105 <- fastRTindirectp105[fastRTindirectp105$congruency == "Incongruent",]
PslowRTindirectConp105 <- length(slowRTindirectConp105$sub)/length(slowRTindirectp105$sub)
PfastRTindirectInconp105 <- length(fastRTindirectInConp105$sub)/length(fastRTindirectp105$sub)
meanPcorrp105 <- mean(PfastRTindirectInconp105,PslowRTindirectConp105)

slowRTindirectp200 <- p200[p200$RT<median(p200$RT),]
fastRTindirectp200 <- p200[p200$RT>=median(p200$RT),]
slowRTindirectConp200 <- slowRTindirectp200[slowRTindirectp200$congruency == "Congruent",]
fastRTindirectInConp200 <- fastRTindirectp200[fastRTindirectp200$congruency == "Incongruent",]
PslowRTindirectConp200 <- length(slowRTindirectConp200$sub)/length(slowRTindirectp200$sub)
PfastRTindirectInconp200 <- length(fastRTindirectInConp200$sub)/length(fastRTindirectp200$sub)
meanPcorrp200 <- mean(PfastRTindirectInconp200,PslowRTindirectConp200)

slowRTindirectp201 <- p201[p201$RT<median(p201$RT),]
fastRTindirectp201 <- p201[p201$RT>=median(p201$RT),]
slowRTindirectConp201 <- slowRTindirectp201[slowRTindirectp201$congruency == "Congruent",]
fastRTindirectInConp201 <- fastRTindirectp201[fastRTindirectp201$congruency == "Incongruent",]
PslowRTindirectConp201 <- length(slowRTindirectConp201$sub)/length(slowRTindirectp201$sub)
PfastRTindirectInconp201 <- length(fastRTindirectInConp201$sub)/length(fastRTindirectp201$sub)
meanPcorrp201 <- mean(PfastRTindirectInconp201,PslowRTindirectConp201)

slowRTindirectp202 <- p202[p202$RT<median(p202$RT),]
fastRTindirectp202 <- p202[p202$RT>=median(p202$RT),]
slowRTindirectConp202 <- slowRTindirectp202[slowRTindirectp202$congruency == "Congruent",]
fastRTindirectInConp202 <- fastRTindirectp202[fastRTindirectp202$congruency == "Incongruent",]
PslowRTindirectConp202 <- length(slowRTindirectConp202$sub)/length(slowRTindirectp202$sub)
PfastRTindirectInconp202 <- length(fastRTindirectInConp202$sub)/length(fastRTindirectp202$sub)
meanPcorrp202 <- mean(PfastRTindirectInconp202,PslowRTindirectConp202)

slowRTindirectp203 <- p203[p203$RT<median(p203$RT),]
fastRTindirectp203 <- p203[p203$RT>=median(p203$RT),]
slowRTindirectConp203 <- slowRTindirectp203[slowRTindirectp203$congruency == "Congruent",]
fastRTindirectInConp203 <- fastRTindirectp203[fastRTindirectp203$congruency == "Incongruent",]
PslowRTindirectConp203 <- length(slowRTindirectConp203$sub)/length(slowRTindirectp203$sub)
PfastRTindirectInconp203 <- length(fastRTindirectInConp203$sub)/length(fastRTindirectp203$sub)
meanPcorrp203 <- mean(PfastRTindirectInconp203,PslowRTindirectConp203)

slowRTindirectp204 <- p204[p204$RT<median(p204$RT),]
fastRTindirectp204 <- p204[p204$RT>=median(p204$RT),]
slowRTindirectConp204 <- slowRTindirectp204[slowRTindirectp204$congruency == "Congruent",]
fastRTindirectInConp204 <- fastRTindirectp204[fastRTindirectp204$congruency == "Incongruent",]
PslowRTindirectConp204 <- length(slowRTindirectConp204$sub)/length(slowRTindirectp204$sub)
PfastRTindirectInconp204 <- length(fastRTindirectInConp204$sub)/length(fastRTindirectp204$sub)
meanPcorrp204 <- mean(PfastRTindirectInconp204,PslowRTindirectConp204)

slowRTindirectp205 <- p205[p205$RT<median(p205$RT),]
fastRTindirectp205 <- p205[p205$RT>=median(p205$RT),]
slowRTindirectConp205 <- slowRTindirectp205[slowRTindirectp205$congruency == "Congruent",]
fastRTindirectInConp205 <- fastRTindirectp205[fastRTindirectp205$congruency == "Incongruent",]
PslowRTindirectConp205 <- length(slowRTindirectConp205$sub)/length(slowRTindirectp205$sub)
PfastRTindirectInconp205 <- length(fastRTindirectInConp205$sub)/length(fastRTindirectp205$sub)
meanPcorrp205 <- mean(PfastRTindirectInconp205,PslowRTindirectConp205)

slowRTindirectp206 <- p206[p206$RT<median(p206$RT),]
fastRTindirectp206 <- p206[p206$RT>=median(p206$RT),]
slowRTindirectConp206 <- slowRTindirectp206[slowRTindirectp206$congruency == "Congruent",]
fastRTindirectInConp206 <- fastRTindirectp206[fastRTindirectp206$congruency == "Incongruent",]
PslowRTindirectConp206 <- length(slowRTindirectConp206$sub)/length(slowRTindirectp206$sub)
PfastRTindirectInconp206 <- length(fastRTindirectInConp206$sub)/length(fastRTindirectp206$sub)
meanPcorrp206 <- mean(PfastRTindirectInconp206,PslowRTindirectConp206)

slowRTindirectp207 <- p207[p207$RT<median(p207$RT),]
fastRTindirectp207 <- p207[p207$RT>=median(p207$RT),]
slowRTindirectConp207 <- slowRTindirectp207[slowRTindirectp207$congruency == "Congruent",]
fastRTindirectInConp207 <- fastRTindirectp207[fastRTindirectp207$congruency == "Incongruent",]
PslowRTindirectConp207 <- length(slowRTindirectConp207$sub)/length(slowRTindirectp207$sub)
PfastRTindirectInconp207 <- length(fastRTindirectInConp207$sub)/length(fastRTindirectp207$sub)
meanPcorrp207 <- mean(PfastRTindirectInconp207,PslowRTindirectConp207)

slowRTindirectp208 <- p208[p208$RT<median(p208$RT),]
fastRTindirectp208 <- p208[p208$RT>=median(p208$RT),]
slowRTindirectConp208 <- slowRTindirectp208[slowRTindirectp208$congruency == "Congruent",]
fastRTindirectInConp208 <- fastRTindirectp208[fastRTindirectp208$congruency == "Incongruent",]
PslowRTindirectConp208 <- length(slowRTindirectConp208$sub)/length(slowRTindirectp208$sub)
PfastRTindirectInconp208 <- length(fastRTindirectInConp208$sub)/length(fastRTindirectp208$sub)
meanPcorrp208 <- mean(PfastRTindirectInconp208,PslowRTindirectConp208)

slowRTindirectp209 <- p209[p209$RT<median(p209$RT),]
fastRTindirectp209 <- p209[p209$RT>=median(p209$RT),]
slowRTindirectConp209 <- slowRTindirectp209[slowRTindirectp209$congruency == "Congruent",]
fastRTindirectInConp209 <- fastRTindirectp209[fastRTindirectp209$congruency == "Incongruent",]
PslowRTindirectConp209 <- length(slowRTindirectConp209$sub)/length(slowRTindirectp209$sub)
PfastRTindirectInconp209 <- length(fastRTindirectInConp209$sub)/length(fastRTindirectp209$sub)
meanPcorrp209 <- mean(PfastRTindirectInconp209,PslowRTindirectConp209)

slowRTindirectp210 <- p210[p210$RT<median(p210$RT),]
fastRTindirectp210 <- p210[p210$RT>=median(p210$RT),]
slowRTindirectConp210 <- slowRTindirectp210[slowRTindirectp210$congruency == "Congruent",]
fastRTindirectInConp210 <- fastRTindirectp210[fastRTindirectp210$congruency == "Incongruent",]
PslowRTindirectConp210 <- length(slowRTindirectConp210$sub)/length(slowRTindirectp210$sub)
PfastRTindirectInconp210 <- length(fastRTindirectInConp210$sub)/length(fastRTindirectp210$sub)
meanPcorrp210 <- mean(PfastRTindirectInconp210,PslowRTindirectConp210)

slowRTindirectp211 <- p211[p211$RT<median(p211$RT),]
fastRTindirectp211 <- p211[p211$RT>=median(p211$RT),]
slowRTindirectConp211 <- slowRTindirectp211[slowRTindirectp211$congruency == "Congruent",]
fastRTindirectInConp211 <- fastRTindirectp211[fastRTindirectp211$congruency == "Incongruent",]
PslowRTindirectConp211 <- length(slowRTindirectConp211$sub)/length(slowRTindirectp211$sub)
PfastRTindirectInconp211 <- length(fastRTindirectInConp211$sub)/length(fastRTindirectp211$sub)
meanPcorrp211 <- mean(PfastRTindirectInconp211,PslowRTindirectConp211)


meanPcorr_indir <- c(meanPcorrp100,meanPcorrp101,meanPcorrp102,meanPcorrp103,meanPcorrp104,meanPcorrp105,meanPcorrp200,meanPcorrp201,meanPcorrp202,meanPcorrp203,meanPcorrp204,meanPcorrp205,meanPcorrp206,meanPcorrp207,meanPcorrp208,meanPcorrp209,meanPcorrp210,meanPcorrp211)







currentDataSet1 <- read.table("om1701_perc_all.dat", header=TRUE)
DataSetCleanedd <- currentDataSet1[currentDataSet1$key_error=="correct",]
DataSetCleanedd <- DataSetCleanedd[DataSetCleanedd$time_error=="Normal",]
nOfTrialsTotalDirect <- c(length(which(currentDataSet1$sub == "100")),length(which(currentDataSet1$sub == "101")),length(which(currentDataSet1$sub == "102")),length(which(currentDataSet1$sub == "103")),length(which(currentDataSet1$sub == "104")),length(which(currentDataSet1$sub == "105")),length(which(currentDataSet1$sub == "200")),length(which(currentDataSet1$sub == "201")),length(which(currentDataSet1$sub == "202")),length(which(currentDataSet1$sub == "203")),length(which(currentDataSet1$sub == "204")),length(which(currentDataSet1$sub == "205")),length(which(currentDataSet1$sub == "206")),length(which(currentDataSet1$sub == "207")),length(which(currentDataSet1$sub == "208")),length(which(currentDataSet1$sub == "209")),length(which(currentDataSet1$sub == "210")),length(which(currentDataSet1$sub == "211")))
p100d <- DataSetCleanedd[DataSetCleanedd$sub == "100",]
p101d <- DataSetCleanedd[DataSetCleanedd$sub == "101",]
p102d <- DataSetCleanedd[DataSetCleanedd$sub == "102",]
p103d <- DataSetCleanedd[DataSetCleanedd$sub == "103",]
p104d <- DataSetCleanedd[DataSetCleanedd$sub == "104",]
p105d <- DataSetCleanedd[DataSetCleanedd$sub == "105",]
p200d <- DataSetCleanedd[DataSetCleanedd$sub == "200",]
p201d <- DataSetCleanedd[DataSetCleanedd$sub == "201",]
p202d <- DataSetCleanedd[DataSetCleanedd$sub == "202",]
p203d <- DataSetCleanedd[DataSetCleanedd$sub == "203",]
p204d <- DataSetCleanedd[DataSetCleanedd$sub == "204",]
p205d <- DataSetCleanedd[DataSetCleanedd$sub == "205",]
p206d <- DataSetCleanedd[DataSetCleanedd$sub == "206",]
p207d <- DataSetCleanedd[DataSetCleanedd$sub == "207",]
p208d <- DataSetCleanedd[DataSetCleanedd$sub == "208",]
p209d <- DataSetCleanedd[DataSetCleanedd$sub == "209",]
p210d <- DataSetCleanedd[DataSetCleanedd$sub == "210",]
p211d <- DataSetCleanedd[DataSetCleanedd$sub == "211",]

slowRTindirectp100d <- p100d[p100d$RT<median(p100d$RT),]
fastRTindirectp100d <- p100d[p100d$RT>=median(p100d$RT),]
slowRTindirectConp100d <- slowRTindirectp100d[slowRTindirectp100d$congruency == "Congruent",]
fastRTindirectInConp100d <- fastRTindirectp100d[fastRTindirectp100d$congruency == "Incongruent",]
PslowRTindirectConp100d <- length(slowRTindirectConp100d$sub)/length(slowRTindirectp100d$sub)
PfastRTindirectInconp100d <- length(fastRTindirectInConp100d$sub)/length(fastRTindirectp100d$sub)
meanPcorrp100d <- mean(PfastRTindirectInconp100d,PslowRTindirectConp100d)


slowRTindirectp101d <- p101d[p101d$RT<median(p101d$RT),]
fastRTindirectp101d <- p101d[p101d$RT>=median(p101d$RT),]
slowRTindirectConp101d <- slowRTindirectp101d[slowRTindirectp101d$congruency == "Congruent",]
fastRTindirectInConp101d <- fastRTindirectp101d[fastRTindirectp101d$congruency == "Incongruent",]
PslowRTindirectConp101d <- length(slowRTindirectConp101d$sub)/length(slowRTindirectp101d$sub)
PfastRTindirectInconp101d <- length(fastRTindirectInConp101d$sub)/length(fastRTindirectp101d$sub)
meanPcorrp101d <- mean(PfastRTindirectInconp101d,PslowRTindirectConp101d)



slowRTindirectp102d <- p102d[p102d$RT<median(p102d$RT),]
fastRTindirectp102d <- p102d[p102d$RT>=median(p102d$RT),]
slowRTindirectConp102d <- slowRTindirectp102d[slowRTindirectp102d$congruency == "Congruent",]
fastRTindirectInConp102d <- fastRTindirectp102d[fastRTindirectp102d$congruency == "Incongruent",]
PslowRTindirectConp102d <- length(slowRTindirectConp102d$sub)/length(slowRTindirectp102d$sub)
PfastRTindirectInconp102d <- length(fastRTindirectInConp102d$sub)/length(fastRTindirectp102d$sub)
meanPcorrp102d <- mean(PfastRTindirectInconp102d,PslowRTindirectConp102d)

slowRTindirectp103d <- p103d[p103d$RT<median(p103d$RT),]
fastRTindirectp103d <- p103d[p103d$RT>=median(p103d$RT),]
slowRTindirectConp103d <- slowRTindirectp103d[slowRTindirectp103d$congruency == "Congruent",]
fastRTindirectInConp103d <- fastRTindirectp103d[fastRTindirectp103d$congruency == "Incongruent",]
PslowRTindirectConp103d <- length(slowRTindirectConp103d$sub)/length(slowRTindirectp103d$sub)
PfastRTindirectInconp103d <- length(fastRTindirectInConp103d$sub)/length(fastRTindirectp103d$sub)
meanPcorrp103d <- mean(PfastRTindirectInconp103d,PslowRTindirectConp103d)

slowRTindirectp104d <- p104d[p104d$RT<median(p104d$RT),]
fastRTindirectp104d <- p104d[p104d$RT>=median(p104d$RT),]
slowRTindirectConp104d <- slowRTindirectp104d[slowRTindirectp104d$congruency == "Congruent",]
fastRTindirectInConp104d <- fastRTindirectp104d[fastRTindirectp104d$congruency == "Incongruent",]
PslowRTindirectConp104d <- length(slowRTindirectConp104d$sub)/length(slowRTindirectp104d$sub)
PfastRTindirectInconp104d <- length(fastRTindirectInConp104d$sub)/length(fastRTindirectp104d$sub)
meanPcorrp104d <- mean(PfastRTindirectInconp104d,PslowRTindirectConp104d)

slowRTindirectp105d <- p105d[p105d$RT<median(p105d$RT),]
fastRTindirectp105d <- p105d[p105d$RT>=median(p105d$RT),]
slowRTindirectConp105d <- slowRTindirectp105d[slowRTindirectp105d$congruency == "Congruent",]
fastRTindirectInConp105d <- fastRTindirectp105d[fastRTindirectp105d$congruency == "Incongruent",]
PslowRTindirectConp105d <- length(slowRTindirectConp105d$sub)/length(slowRTindirectp105d$sub)
PfastRTindirectInconp105d <- length(fastRTindirectInConp105d$sub)/length(fastRTindirectp105d$sub)
meanPcorrp105d <- mean(PfastRTindirectInconp105d,PslowRTindirectConp105d)

slowRTindirectp200d <- p200d[p200d$RT<median(p200d$RT),]
fastRTindirectp200d <- p200d[p200d$RT>=median(p200d$RT),]
slowRTindirectConp200d <- slowRTindirectp200d[slowRTindirectp200d$congruency == "Congruent",]
fastRTindirectInConp200d <- fastRTindirectp200d[fastRTindirectp200d$congruency == "Incongruent",]
PslowRTindirectConp200d <- length(slowRTindirectConp200d$sub)/length(slowRTindirectp200d$sub)
PfastRTindirectInconp200d <- length(fastRTindirectInConp200d$sub)/length(fastRTindirectp200d$sub)
meanPcorrp200d <- mean(PfastRTindirectInconp200d,PslowRTindirectConp200d)

slowRTindirectp201d <- p201d[p201d$RT<median(p201d$RT),]
fastRTindirectp201d <- p201d[p201d$RT>=median(p201d$RT),]
slowRTindirectConp201d <- slowRTindirectp201d[slowRTindirectp201d$congruency == "Congruent",]
fastRTindirectInConp201d <- fastRTindirectp201d[fastRTindirectp201d$congruency == "Incongruent",]
PslowRTindirectConp201d <- length(slowRTindirectConp201d$sub)/length(slowRTindirectp201d$sub)
PfastRTindirectInconp201d <- length(fastRTindirectInConp201d$sub)/length(fastRTindirectp201d$sub)
meanPcorrp201d <- mean(PfastRTindirectInconp201d,PslowRTindirectConp201d)

slowRTindirectp202d <- p202d[p202d$RT<median(p202d$RT),]
fastRTindirectp202d <- p202d[p202d$RT>=median(p202d$RT),]
slowRTindirectConp202d <- slowRTindirectp202d[slowRTindirectp202d$congruency == "Congruent",]
fastRTindirectInConp202d <- fastRTindirectp202d[fastRTindirectp202d$congruency == "Incongruent",]
PslowRTindirectConp202d <- length(slowRTindirectConp202d$sub)/length(slowRTindirectp202d$sub)
PfastRTindirectInconp202d <- length(fastRTindirectInConp202d$sub)/length(fastRTindirectp202d$sub)
meanPcorrp202d <- mean(PfastRTindirectInconp202d,PslowRTindirectConp202d)

slowRTindirectp203d <- p203d[p203d$RT<median(p203d$RT),]
fastRTindirectp203d <- p203d[p203d$RT>=median(p203d$RT),]
slowRTindirectConp203d <- slowRTindirectp203d[slowRTindirectp203d$congruency == "Congruent",]
fastRTindirectInConp203d <- fastRTindirectp203d[fastRTindirectp203d$congruency == "Incongruent",]
PslowRTindirectConp203d <- length(slowRTindirectConp203d$sub)/length(slowRTindirectp203d$sub)
PfastRTindirectInconp203d <- length(fastRTindirectInConp203d$sub)/length(fastRTindirectp203d$sub)
meanPcorrp203d <- mean(PfastRTindirectInconp203d,PslowRTindirectConp203)

slowRTindirectp204d <- p204d[p204d$RT<median(p204d$RT),]
fastRTindirectp204d <- p204d[p204d$RT>=median(p204d$RT),]
slowRTindirectConp204d <- slowRTindirectp204d[slowRTindirectp204d$congruency == "Congruent",]
fastRTindirectInConp204d <- fastRTindirectp204d[fastRTindirectp204d$congruency == "Incongruent",]
PslowRTindirectConp204d <- length(slowRTindirectConp204d$sub)/length(slowRTindirectp204d$sub)
PfastRTindirectInconp204d <- length(fastRTindirectInConp204d$sub)/length(fastRTindirectp204d$sub)
meanPcorrp204d <- mean(PfastRTindirectInconp204d,PslowRTindirectConp204d)

slowRTindirectp205d <- p205d[p205d$RT<median(p205d$RT),]
fastRTindirectp205d <- p205d[p205d$RT>=median(p205d$RT),]
slowRTindirectConp205d <- slowRTindirectp205d[slowRTindirectp205d$congruency == "Congruent",]
fastRTindirectInConp205d <- fastRTindirectp205d[fastRTindirectp205d$congruency == "Incongruent",]
PslowRTindirectConp205d <- length(slowRTindirectConp205d$sub)/length(slowRTindirectp205d$sub)
PfastRTindirectInconp205d <- length(fastRTindirectInConp205d$sub)/length(fastRTindirectp205d$sub)
meanPcorrp205d <- mean(PfastRTindirectInconp205d,PslowRTindirectConp205d)

slowRTindirectp206d <- p206d[p206d$RT<median(p206d$RT),]
fastRTindirectp206d <- p206d[p206d$RT>=median(p206d$RT),]
slowRTindirectConp206d <- slowRTindirectp206d[slowRTindirectp206d$congruency == "Congruent",]
fastRTindirectInConp206d <- fastRTindirectp206d[fastRTindirectp206d$congruency == "Incongruent",]
PslowRTindirectConp206d <- length(slowRTindirectConp206d$sub)/length(slowRTindirectp206d$sub)
PfastRTindirectInconp206d <- length(fastRTindirectInConp206d$sub)/length(fastRTindirectp206d$sub)
meanPcorrp206d <- mean(PfastRTindirectInconp206d,PslowRTindirectConp206d)

slowRTindirectp207d <- p207d[p207d$RT<median(p207d$RT),]
fastRTindirectp207d <- p207d[p207d$RT>=median(p207d$RT),]
slowRTindirectConp207d <- slowRTindirectp207d[slowRTindirectp207d$congruency == "Congruent",]
fastRTindirectInConp207d <- fastRTindirectp207d[fastRTindirectp207d$congruency == "Incongruent",]
PslowRTindirectConp207d <- length(slowRTindirectConp207d$sub)/length(slowRTindirectp207d$sub)
PfastRTindirectInconp207d <- length(fastRTindirectInConp207d$sub)/length(fastRTindirectp207d$sub)
meanPcorrp207d <- mean(PfastRTindirectInconp207d,PslowRTindirectConp207d)

slowRTindirectp208d <- p208d[p208d$RT<median(p208d$RT),]
fastRTindirectp208d <- p208d[p208d$RT>=median(p208d$RT),]
slowRTindirectConp208d <- slowRTindirectp208d[slowRTindirectp208d$congruency == "Congruent",]
fastRTindirectInConp208d <- fastRTindirectp208d[fastRTindirectp208d$congruency == "Incongruent",]
PslowRTindirectConp208d <- length(slowRTindirectConp208d$sub)/length(slowRTindirectp208d$sub)
PfastRTindirectInconp208d <- length(fastRTindirectInConp208d$sub)/length(fastRTindirectp208d$sub)
meanPcorrp208d <- mean(PfastRTindirectInconp208d,PslowRTindirectConp208d)

slowRTindirectp209d <- p209d[p209d$RT<median(p209d$RT),]
fastRTindirectp209d <- p209d[p209d$RT>=median(p209d$RT),]
slowRTindirectConp209d <- slowRTindirectp209d[slowRTindirectp209d$congruency == "Congruent",]
fastRTindirectInConp209d <- fastRTindirectp209d[fastRTindirectp209d$congruency == "Incongruent",]
PslowRTindirectConp209d <- length(slowRTindirectConp209d$sub)/length(slowRTindirectp209d$sub)
PfastRTindirectInconp209d <- length(fastRTindirectInConp209d$sub)/length(fastRTindirectp209d$sub)
meanPcorrp209d <- mean(PfastRTindirectInconp209d,PslowRTindirectConp209d)

slowRTindirectp210d <- p210d[p210d$RT<median(p210d$RT),]
fastRTindirectp210d <- p210d[p210d$RT>=median(p210d$RT),]
slowRTindirectConp210d <- slowRTindirectp210d[slowRTindirectp210d$congruency == "Congruent",]
fastRTindirectInConp210d <- fastRTindirectp210d[fastRTindirectp210d$congruency == "Incongruent",]
PslowRTindirectConp210d <- length(slowRTindirectConp210d$sub)/length(slowRTindirectp210d$sub)
PfastRTindirectInconp210d <- length(fastRTindirectInConp210d$sub)/length(fastRTindirectp210d$sub)
meanPcorrp210d <- mean(PfastRTindirectInconp210d,PslowRTindirectConp210d)

slowRTindirectp211d <- p211d[p211d$RT<median(p211d$RT),]
fastRTindirectp211d <- p211d[p211d$RT>=median(p211d$RT),]
slowRTindirectConp211d <- slowRTindirectp211d[slowRTindirectp211d$congruency == "Congruent",]
fastRTindirectInConp211d <- fastRTindirectp211d[fastRTindirectp211d$congruency == "Incongruent",]
PslowRTindirectConp211d <- length(slowRTindirectConp211d$sub)/length(slowRTindirectp211d$sub)
PfastRTindirectInconp211d <- length(fastRTindirectInConp211d$sub)/length(fastRTindirectp211d$sub)
meanPcorrp211d <- mean(PfastRTindirectInconp211d,PslowRTindirectConp211d)

meanPcorr_dir <- c(meanPcorrp100d,meanPcorrp101d,meanPcorrp102d,meanPcorrp103d,meanPcorrp104d,meanPcorrp105d,meanPcorrp200d,meanPcorrp201d,meanPcorrp202d,meanPcorrp203d,meanPcorrp204d,meanPcorrp205d,meanPcorrp206d,meanPcorrp207d,meanPcorrp208d,meanPcorrp209d,meanPcorrp210d,meanPcorrp211d)
SEMPcorr_dir <- sem(meanPcorr_dir)
argnames2 <- c("meanPcorr_dir")
plotmeanPCorr_dir <- barplot(c(mean(meanPcorr_dir)),names.arg=argnames2,beside=true,cex.names = 0.75,  border = "black", axes = TRUE,col = rainbow(4),ylim = c(0,0.8))
arrows(plotmeanPCorr_dir,mean(meanPcorr_dir)+SEMPcorr_dir,
       plotmeanPCorr_dir,mean(meanPcorr_dir)-SEMPcorr_dir,
       angle=90, code=3, length =0.05,lwd = 1.5)


argnames3 <- c("meanPcorr_dir VS meanPcorr_indir")
plotmeanPCorr_dir_indir <- barplot(c(mean(meanPcorr_dir),mean(meanPcorr_indir)),names.arg=argnames3,beside=true,cex.names = 0.75, border = "black", axes = TRUE,col = rainbow(4),ylim = c(0,1))
argnames4 <- c("meanPcorr_dir")
plotmeanPCorr_dir_indir <- barplot(c(mean(meanPcorr_dir)),names.arg=argnames4,beside=true,cex.names = 0.75, border = "black", axes = TRUE,col = rainbow(4),ylim = c(0,1))

test1 <- t.test(meanRTdiff)
test2 <- t.test(meanRTcon,meanRTincon)
test3 <- t.test(meanPcorr_dir,meanPcorr_indir,paired=TRUE,conf.level=0.95)


indirect_task <- data.frame(subject,nOfTrialsTotal,nOfTrialsNoErrors,meanRToverall,meanRTcon,meanRTincon,meanRTdiff,standardDeviationCon,standardDeviationIncon,standardDeviationDiff,meanPcorr_indir)
direct_task <- data.frame(subject,nOfTrialsTotalDirect,meanPcorr_indir)
