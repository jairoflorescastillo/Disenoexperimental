df=read.csv("cap5p19.csv")
df

str(df)
df$Molde=factor(df$Molde)
df$Catalizador=factor(df$Catalizador)

modelo=aov(Y~Molde+Catalizador,data=df)
summary(modelo)

boxplot(Y~Molde,data=df)
boxplot(Y~Catalizador,data=df)
boxplot(Y~Molde*Catalizador,data=df)
interaction.plot(df$Molde,df$Catalizador,df$Y)

tk=TukeyHSD(modelo)
tk
plot(tk)

qqnorm(modelo$residuals)
qqline(modelo$residuals)
shapiro.test(modelo$residuals)

require(car)
leveneTest(Y~Molde,data=df)
leveneTest(Y~Catalizador,data=df)

plot(modelo$residuals)
abline(h=0)