s1 <- 3.2
s2 <- 1.5

a<- dbeta(seq(0,1, by=0.001), s1, s2)

df <- data.frame(x=125*seq(0,1, by=0.001),y= a)
pretty_plot(df
      , type="l", ccloc=4
     , xlab="% door mens veroorzaakt"
     , ylab="density"
     , main="'Extremely likely that at least half...'\n'Best estimate is 100%'")

pretty_abline(kleur=4 , v=125*qbeta(0.1,s1, s2))

df2<- df[df$x >= 125*qbeta(0.1,s1, s2), ]
df2$z<- 0

pretty_plot(add=T, type="a", df2, kleur=2)
pretty_text(100, 1, "90%", kleur=2, cex=1.6)