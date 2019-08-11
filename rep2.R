a<- readLines("./input/pagode20162018.txt")
b<- read.csv(text=a, skip=15, 
             header=TRUE, stringsAsFactors = FALSE)
b<- b[complete.cases(b),]

b1<- b[b$screen == "Pagode", -4]
b2<- b[b$screen == "Stevenson", -4]
names(b1) <- names(b2) <- c("yy", "mm", "dd", 
                            "TG", "TX", "TN")
require(myLib)
df <- data.frame(x= b2$TX, y= b1$TX-b2$TX)
pretty_plot( df , kleur=4, type="p", cex=0.9, main=
               "Xtremes Stevenson vs diff")

l<- lm(y ~x, data=df)
pretty_abline(kleur=2, l)

bb1<- quantile(b1$TX, seq(0.05, 0.975, by=0.05))
bb2<- quantile(b2$TX, seq(0.05, 0.975, by=0.05))

df2 <- data.frame(x= seq(0.05, 0.975, by=0.05), y=bb2-bb1)

df2 <- data.frame(x=bb1, y=bb2-bb1)

pretty_plot(df2, type="b", cex=0.9, kleur = 1, main=
              "2016-2018 \npagode vs stevenson correction", 
            xlab="quantile", ylab="difference")

d<- loess(y~x, data=df2, span=0.6)
df2$y <- d$fitted
pretty_plot(df2, kleur=2, add=T, lwd=2)

# -----------------------------------

a<- readLines("./input/pagode19471950.txt")
b<- read.csv(text=a, skip=16, 
             header=TRUE, stringsAsFactors = FALSE)
b<- b[complete.cases(b),]
bp1<- b[b$period== "period 1",]

b1<- bp1[,c(1:5)]
b2<- bp1[, c(1:3,6,7)]
names(b1) <- names(b2) <- c("yy", "mm", "dd", 
                            "TN", "TX")
df <- data.frame(x= b2$TX, y= b1$TX-b2$TX)
pretty_plot( df , kleur=4, type="p", cex=0.9, main=
               "Xtremes Stevenson vs diff")

l<- lm(y ~x, data=df)
pretty_abline(kleur=2, l)

bb1<- quantile(b1$TX, seq(0.05, 0.975, by=0.05))
bb2<- quantile(b2$TX, seq(0.05, 0.975, by=0.05))

df2 <- data.frame(x= seq(0.05, 0.975, by=0.05), y=bb2-bb1)

df2 <- data.frame(x=bb1, y=bb2-bb1)

pretty_plot(df2, type="b", cex=0.9, kleur = 1, main=
              "1947=1950 period 1 vs stevenson correction", 
            xlab="quantile", ylab="difference")

d<- loess(y~x, data=df2, span=0.6)
df2$y <- d$fitted
pretty_plot(df2, kleur=2, add=T, lwd=2)

# =====================================

bp1<- b[b$period== "period 2",]

b1<- bp1[,c(1:5)]
b2<- bp1[, c(1:3,6,7)]
names(b1) <- names(b2) <- c("yy", "mm", "dd", 
                            "TN", "TX")
df <- data.frame(x= b2$TX, y= b1$TX-b2$TX)
pretty_plot( df , kleur=4, type="p", cex=0.9, main=
               "Xtremes Stevenson vs diff")

l<- lm(y ~x, data=df)
pretty_abline(kleur=2, l)

bb1<- quantile(b1$TX, seq(0.05, 0.975, by=0.05))
bb2<- quantile(b2$TX, seq(0.05, 0.975, by=0.05))

df2 <- data.frame(x= seq(0.05, 0.975, by=0.05), y=bb2-bb1)

df2 <- data.frame(x=bb1, y=bb2-bb1)

pretty_plot(df2, type="b", cex=0.9, kleur = 1, main=
              "1947=1950 period 2 vs stevenson correction", 
            xlab="quantile", ylab="difference")

d<- loess(y~x, data=df2, span=0.6)
df2$y <- d$fitted
pretty_plot(df2, kleur=2, add=T, lwd=2)

