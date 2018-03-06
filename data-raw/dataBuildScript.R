# script to build data sets

# Univarent
path1 = "~/R/workspace/rqualification/data-raw/"
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
rm(path1)
rm(path2)

# Linear
