

path1 <- "~/R/workspace/rqualification/data-raw/"
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
