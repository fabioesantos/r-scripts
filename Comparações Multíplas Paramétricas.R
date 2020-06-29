# Pacotes usados aqui
install.packages('car')
install.packages('sciplot')
install.packages('agricolae')
install.packages('ScottKnott')
install.packages('multcomp')

data("PlantGrowth")
dados <- PlantGrowth


### Analise de Variância(ANOVA)

# ANOVA para a variável resposta "weight"
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

# análise gráfica dos resíduos
par(mfrow=c(2,2))
plot(anova)

# Bartlett test of homogeneity of variances
bartlett.test(residuals(anova) ~ dados$group)

# Levene Test of homogeneity of variances (Outro teste de homocedasticidade)
library(car)
leveneTest(anova)

# Caso quiser visualizar o boxplot
boxplot(weight ~ group, data=dados)

### Multiple Comparisons

# Pela anova vimos que os grupos diferem, porém não sabemos quais dos grupos direriram entre sí. Há vários testes para comparar as médias dos tratamentos, aqui veremos alguns:

#Teste de Tukey
library(agricolae)
tukey.test <- HSD.test(anova, trt = 'group')
tukey.test


#Test de Scott-Knoot para agrupamento de médias
require(ScottKnott)
sk <- SK(anova, sig.level = 0.05)
summary(sk)

#Teste de Dunnet (diferente dos demais esse test não compara todos os grupos entre si, ele compara um grupo controle com os demais. Logo esse teste se aplica quando se tem um grupo controle e vc quer saber apenas quais grupos diferiram do grupo controle)
require(multcomp)
duntest <- glht(anova, linfct = mcp(group = "Dunnett"))
summary(duntest)

# Fazer o gráfico de barras já com a barra de erro do desvio padrão
library(sciplot)
bargraph.CI(x.factor = factor(group), response = weight, data = dados,
            main = "Plant Growth", xlab = "Group",
            ylab = "Weight", col = "grey",  legend = TRUE)

