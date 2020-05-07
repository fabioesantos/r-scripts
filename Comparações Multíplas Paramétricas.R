# Pacotes usados aqui
install.packages('car')
install.packages('sciplot')

data("PlantGrowth")
dados <- PlantGrowth


# Analise de Variância(ANOVA)

# ANOVA para a variável resposta "response"
anova <- aov(weight ~ group, data = dados)
# Ver o resumo da ANOVA
summary(anova)

### Análise dos resíduos da Anova/Testes dos Presupostos da ANOVA ###

# shapiro-wilk normality test
shapiro.test(resid(anova))

# Histograma dos resíduos da ANOVA
hist(resid(anova))

# qqplot dos resíduos da ANOVA
qqnorm(resid(anova))
qqline(resid(anova))

# Bartlett test of homogeneity of variances
bartlett.test(residuals(anova) ~ dados$group)
# análise gráfica dos resíduos
par(mfrow=c(2,2))
plot(anova)

# Levene Test of homogeneity of variances (Outro teste de homocedasticidade)
library(car)
leveneTest(anova)

# Caso quiser visualizar o boxplot
boxplot(weight ~ group, data=dados)

### Multiple Comparisons

# Pela anova vimos que os grupos diferem, porém não sabemos quais dos grupos direriram entre sí. Há vários testes para comparar as médias dos tratamentos, aqui veremos alguns:

#tukey test with agricolae package
library(agricolae)
tukey.test <- HSD.test(anova, trt = 'group')
tukey.test

# Fazer o gráfico de barras já com a barra de erro do desvio padrão
library(sciplot)
bargraph.CI(x.factor = factor(group), response = weight, data = data,
            main = "Plant Growth", xlab = "Group",
            ylab = "Weight", col = "grey",  legend = TRUE)

