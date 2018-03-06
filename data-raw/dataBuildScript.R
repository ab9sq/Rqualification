# script to build data sets

# Univarent
path1 <- "~/R/workspace/rqualification/data-raw/"
path2 <- "univerient/"

Lew <- read.table(file=paste0(path1, path2, "Lew.txt"))
devtools::use_data(Lew)
rm(Lew)

Lottery <- read.table(file=paste0(path1, path2, "Lottery.txt"))
devtools::use_data(Lottery)
rm(Lottery)


Mavro <- read.table(file=paste0(path1, path2, "Mavro.txt"))
devtools::use_data(Mavro)
rm(Mavro)


Michelso <- read.table(file=paste0(path1, path2, "Michelso.txt"))
devtools::use_data(Michelso)
rm(Michelso)


NumAcc1 <- read.table(file=paste0(path1, path2, "NumAcc1.txt"))
devtools::use_data(NumAcc1)
rm(NumAcc1)


NumAcc2 <- read.table(file=paste0(path1, path2, "NumAcc2.txt"))
devtools::use_data(NumAcc2)
rm(NumAcc2)


NumAcc3 <- read.table(file=paste0(path1, path2, "NumAcc3.txt"))
devtools::use_data(NumAcc3)
rm(NumAcc3)


NumAcc4 <- read.table(file=paste0(path1, path2, "NumAcc4.txt"))
devtools::use_data(NumAcc4)
rm(NumAcc4)


PiDigits <- read.table(file=paste0(path1, path2, "PiDigits.txt"))
devtools::use_data(PiDigits)
rm(PiDigits)


# Linear
path2 <- "linear/"

Longley <- read.table(file=paste0(path1, path2, "Longley.txt"), header = TRUE)
devtools::use_data(Longley)
rm(Longley)

NoInt1 <- read.table(file=paste0(path1, path2, "NoInt1.txt"), header = TRUE)
devtools::use_data(NoInt1)
rm(NoInt1)

NoInt2 <- read.table(file=paste0(path1, path2, "NoInt2.txt"), header = TRUE)
devtools::use_data(NoInt2)
rm(NoInt2)

Norris <- read.table(file=paste0(path1, path2, "Norris.txt"), header = TRUE)
devtools::use_data(Norris)
rm(Norris)

Pontius <- read.table(file=paste0(path1, path2, "Pontius.txt"), header = TRUE)
devtools::use_data(Pontius)
rm(Pontius)

Wampler1 <- read.table(file=paste0(path1, path2, "Wampler1.txt"), header = TRUE)
devtools::use_data(Wampler1)
rm(Wampler1)

Wampler2 <- read.table(file=paste0(path1, path2, "Wampler2.txt"), header = TRUE)
devtools::use_data(Wampler2)
rm(Wampler2)


Wampler3 <- read.table(file=paste0(path1, path2, "Wampler3.txt"), header = TRUE)
devtools::use_data(Wampler3)
rm(Wampler3)

Wampler4 <- read.table(file=paste0(path1, path2, "Wampler4.txt"), header = TRUE)
devtools::use_data(Wampler4)
rm(Wampler4)

Wampler5 <- read.table(file=paste0(path1, path2, "Wampler5.txt"), header = TRUE)
devtools::use_data(Wampler5)
rm(Wampler5)

Filip <- read.table(file=paste0(path1, path2, "Filip.txt"), header = TRUE)
devtools::use_data(Filip)
rm(Filip)


rm(path1)
rm(path2)

#<- read.table(file=paste0(path1, path2, ".txt"), header = TRUE)
#devtools::use_data()
#rm()
