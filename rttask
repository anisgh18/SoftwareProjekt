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


rttask <- data.frame(subject,nOfTrialsTotal,nOfTrialsNoErrors,meanRToverall,meanRTcon,meanRTincon,meanRTdiff)
