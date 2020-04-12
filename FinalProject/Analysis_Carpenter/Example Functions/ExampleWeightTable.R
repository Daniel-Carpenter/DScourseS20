
stockList <- c('a', 'b', 'c')
wtTable <- c()
setWeight <- 1 / length(stockList)

for (i in 1:length(stockList))
{
  wtTable[i] <- setWeight
}
wtTable <- as.data.frame(wtTable)
colnames(wtTable) <- c("portfolioWeight")
rownames(wtTable) <- stockList


#colnames(wtTable) <- c(stockList)



