Norris <- read.table(file = "./data-raw/linear/Norris.txt",
                     header = TRUE)
options(digits = 22)
Norris.lm <- lm(y~x, data = Norris)
Norris.anova <- anova(Norris.lm)

(abs(Norris.anova$`F value`[1] - 5436385.54079785) > (.Machine$double.eps ^ 0.5))

abs(Norris.lm$coefficients[1] - -0.262323073774029) > (.Machine$double.eps ^ 0.5)

abs(Norris.lm$coefficients[2] - 1.00211681802045) > (.Machine$double.eps ^ 0.5)

abs(summary(Norris.lm)$r.squared - 0.999993745883712) > (.Machine$double.eps ^ 0.5)


?summary.lm


###### more explorations

path1 <- "~/R/workspace/rqualification/data-raw/"
# Linear
path2 <- "linear/"
Norris <- read.table(file=paste0(path1, path2, "Norris.txt"), header = TRUE)

Norris.lm <- lm(y~x, data=Norris)

options(digits = 22)

Norris.lm$coefficients
Norris.lm$coefficients[1]  #intercept
Norris.lm$coefficients[2]  # x
coef(Norris.lm)


test <- summary(Norris.lm)


test$r.squared
test$fstatistic


test$coefficients
test$coefficients["x","Std. Error"]

test$sigma #risidual standard error

test2 <- anova(Norris.lm)

test2$`Sum Sq`
test2$`Mean Sq`
test2$`F value`
