library(plotrix)

otter_obsv <- c(3896, 2963, 3334, 3318, 2275, 3248, 2592, 2906, 3247, 3252)
sd(otter_obsv)

se <- 446.9/sqrt(10)
df <- length(otter_obsv) - 1

t <- (mean(otter_obsv) - 3090) / se

1-pt(.09, df = 9)#default pt gives area to the left of the origin; change lower.tail to false (below)
pt(.09, df = 9, lower.tail = F)#gives only upper tail
#p of .46 means that our calculated t stat is not significantly different than that of the null; should NOT delist otters as endangered. Very high probabulity (.46) of getting a sample mean (3103) like this when the null is true
2*pt(.09, df = 9, lower.tail = F)#do if hypothesis is non directional (assumes a symmetrical null distribution)

#' _Jackson's Work_
#

andersonDat <- c(3602, 3169, 2343, 3657 ,3149, 2897, 2745, 3503, 2990, 2241)

mean <- mean(andersonDat)
sd <- sd(andersonDat)
se <- sd/sqrt(length(andersonDat))
df <- length(andersonDat) - 1

t <- (mean - 3090) / se

1-pt(t, df = 9)#65% chance that we will see our observed mean(3029) if the null were true; should NOT delist otters
