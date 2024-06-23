ntc <- read_excel('NTC PWSS Results Data.xlsx')

colnames(ntc)[1] = 'Name'

plot1 <- ggplot(ntc, aes(x = `General focus`), xlab = "") + geom_density() + geom_vline(aes(xintercept=median(`General focus`)),
                                                                        color="blue", linetype="dashed", size=1) +scale_x_continuous(limits = c(8,32))

plot2 <- ggplot(ntc, aes(x = `Task focus`), xlab = "") + geom_density() + geom_vline(aes(xintercept=median(`Task focus`)),
                                                                             color="blue", linetype="dashed", size=1) +scale_x_continuous(limits = c(8,32))

plot3 <- ggplot(ntc, aes(x = `Processing strategy`), xlab = "") + geom_density() + geom_vline(aes(xintercept=median(`Processing strategy`)),
                                                                             color="blue", linetype="dashed", size=1)+scale_x_continuous(limits = c(8,32))
plot4 <- ggplot(ntc, aes(x = `Structure orientation`), xlab = "") + geom_density() + geom_vline(aes(xintercept=median(`Structure orientation`)),
                                                                            color="blue", linetype="dashed", size=1)+scale_x_continuous(limits = c(8,32))
plot5 <- ggplot(ntc, aes(x = `Action orientation`), xlab = "") + geom_density() + geom_vline(aes(xintercept=median(`Action orientation`)),
                                                                             color="blue", linetype="dashed", size=1)+scale_x_continuous(limits = c(8,32))

ggpubr::ggarrange(plot1, plot2, plot3, plot4, plot5, ncol = 1) +scale_x_continuous(limits = c(8,32))


