library(gplots)
library(ggplot2)

initial = getWordListEntropy(d.wh.possible.initial.m, T)
noninitial = getWordListEntropy(d.wh.possible.non.initial.m, T)

rx = data.frame(e= c(initial,noninitial), l=c(rep("Initial", length(initial)), rep("Non-initial", length(noninitial))))

pdf("../Results/SimplifiedPhonology/Graphs/InterrogativeOrder/InitialVsNonInitial_firstSegments.pdf")
g = ggplot(rx, aes(y=e,x=l))
g + geom_violin() + geom_boxplot(width=0.1)+  xlab("") + ylab("Wh-word diversity")
dev.off()

initial = getWordListEntropy(d.unanalyzable.wh.initial.m, T)
noninitial = getWordListEntropy(d.unanalyzable.wh.non.initial.m, T)

rx = data.frame(e= c(initial,noninitial), l=c(rep("Initial", length(initial)), rep("Non-initial", length(noninitial))))

pdf("../Results/SimplifiedPhonology/Graphs/InterrogativeOrder/InitialVsNonInitial_firstSegments_unanalyzable.pdf",
    width=5, height=3.5)
g = ggplot(rx, aes(y=e,x=l))
g + geom_violin() + geom_boxplot(width=0.1)+  xlab("") + ylab("Wh-word diversity")
dev.off()
