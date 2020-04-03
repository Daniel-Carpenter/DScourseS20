listOfNames <- c('AMZN', 'AAPL')

listLength <- NROW(listOfNames)
listLength

for (i in 1:listLength)
{
  append <- cat(listOfNames[i], and = ",", listOfNames[i - 1])
}


#cat(listOfNames[1], and = ",", listOfNames[2])

paste0(listOfNames, collapse = ", ")
# [1] "stat-1, stat-2, stat-3, stat-4, stat-5"