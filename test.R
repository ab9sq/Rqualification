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
