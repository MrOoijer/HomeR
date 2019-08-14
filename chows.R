# why we need to be careful
set.seed(127)
require(myLib)
A=30; B=30
range=seq( -50, 50 , length.out = 101)
values <- rnorm(101, 0, 0.9)+ (range-B)*range*(range+B) /A^3

df<- data.frame(x=range, y=values)

l<- lm(y ~ x, data=df)
l1<- lm(y ~ x, data=df[1:50,])
l2<- lm(y ~ x, data=df[51:101,])

pretty_plot(df, kleur=4, typ="p", 
            cex=1.1, main="How Chow's test might fail \nfor non linear trends")
pretty_abline(kleur=1, l, lwd=4)
pretty_plot(add=T, kleur=2, lwd=3,
      rbind(data.frame(x=range[1:50],y=l1$fit),
            data.frame(x=range[51:101],y=l2$fit)))


require(strucchange)
b<- sctest(y ~ x, data=df , type = "Chow", point = 51)
print(b) # what is meaning of point?
