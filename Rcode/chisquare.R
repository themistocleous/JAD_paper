library(pwr)
M <- as.table(rbind(c(56,66), c(77.77, 80)))
Xsq <- chisq.test(M)
Xsq
df <- min(dim(M)) - 1

phi = ((56*66) - (77.77*80))/sqrt((56+66)*(77.77+80)*(56+77.77)*(66+80))
-0.1302617


