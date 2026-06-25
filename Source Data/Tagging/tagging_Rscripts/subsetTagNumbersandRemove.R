



data = read.csv("TagReturnsMaster.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
dim(data)

ids=c(456746,442944,449321,445535,445246,450388,445544,450664,445087,442822,450032,442954,449380,442727,442000,451582,450627,450104,415483)

test = data[which((data$TAG_NUMBER %in% ids)), ]
x = rownames(test)


test <- data[-c(34,54,56,58,59,60,66,68,71,72,77,78,81,82,83,85,87,89,158),] 
dim(test)


write.table(test, file= "test.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
