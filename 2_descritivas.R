### DESCRIÇÃO DAS VARIÁVEIS CRIADAS
stargazer(dfMerge, type = "text", digits = 2, 
          keep = c("mulher", "branco", "urbano", "metro", "trab", "ativ", "ocup", "exper", "sal.hr"),
          nobs = TRUE, mean.sd = TRUE, median = TRUE, min.max = FALSE, iqr = FALSE)

dfHomem <- subset(dfMerge, mulher == 0)
stargazer(dfHomem, type = "text", digits = 2, 
          keep = c("branco", "urbano", "metro", "trab", "ativ", "ocup", "exper", "sal.hr"),
          nobs = TRUE, mean.sd = TRUE, median = TRUE, min.max = FALSE, iqr = FALSE)

dfMulher <- subset(dfMerge, mulher == 1)
stargazer(dfMulher, type = "text", digits = 2, 
          keep = c("branco", "urbano", "metro", "trab", "ativ", "ocup", "exper", "sal.hr"),
          nobs = TRUE, mean.sd = TRUE, median = TRUE, min.max = FALSE, iqr = FALSE)

rm(list = "dfHomem", "dfMulher")

### TABELAS CRUZADAS

# Salário/hora por idade e gênero
dfMerge %>%
  group_by(idade, mulher) %>%
  summarize(sal.medio = mean(sal.hr, na.rm = TRUE))

# Salário/hora por escolaridade e gênero
dfMerge %>%
  group_by(escol, mulher) %>%
  summarize(sal.medio = mean(sal.hr, na.rm = TRUE))

# Salário/hora por cor e gênero
dfMerge %>%
  group_by(branco, mulher) %>%
  summarize(sal.medio = mean(sal.hr, na.rm = TRUE))

# Salário/hora por idade, escolaridade e gênero
dfMerge %>%
  group_by(idade, escol, mulher) %>%
  summarize(sal.medio = mean(sal.hr, na.rm = TRUE))

# Salário/hora por cor, escolaridade e gênero
dfMerge %>%
  group_by(branco, escol, mulher) %>%
  summarize(sal.medio = mean(sal.hr, na.rm = TRUE))

# Salário/hora por região metropolina, escolaridade e gênero
dfMerge %>%
  group_by(metro, escol, mulher) %>%
  summarize(sal.medio = mean(sal.hr, na.rm = TRUE))

# Salário/hora por urbano, escolaridade e gênero
dfMerge %>%
  group_by(urbano, escol, mulher) %>%
  summarize(sal.medio = mean(sal.hr, na.rm = TRUE))

# Logaritmo natural do salário/hora por percentil e gênero
dfMerge$quintile <- ntile(dfMerge$ln.sal.hr, 5)
dfMerge %>%
  group_by(quintile, mulher) %>%
  summarize(sal.medio = mean(sal.hr, na.rm = TRUE))

### ANÁLISE GRÁFICA
theme_set(theme_classic())

# Gráfico de barras salário hora por grupo de escolaridade
g1 <- ggplot(dfMerge, aes(escol, sal.hr))
g1 + stat_summary(fun.y = "mean", geom = "bar", fill = "tomato3") +
  labs(title = "Salário/hora por grupo de escolaridade",
       y = "Salário/hora", x = "Escolaridade",
       caption = "1 - Nenhuma escolaridade; 2 - Fundamental; 3 - Médio; 4 - Superior")

# Gráfico boxplot de salário hora por gênero
g2 <- ggplot(subset(dfMerge, sal.hr <= 100), aes(factor(mulher), sal.hr))
g2 + geom_boxplot(varwidth = T, fill = "tomato3") +
  labs(title = "Salário/hora por gênero",
       y = "Salário/hora", x = "Gênero") +
  scale_x_discrete(labels = c('Homem','Mulher'))

# Histograma do salário hora por gênero e raça
g3 <- ggplot(dfMerge, aes(sal.hr))
g3 + geom_histogram(data = subset(dfMerge, mulher == 0), binwidth = 10, alpha = 0.7, aes(fill = "Homem")) +
  geom_histogram(data = subset(dfMerge, mulher == 1), binwidth = 10, alpha = 0.7, aes(fill = "Mulher")) +
  labs(title = "Distribuição do salário/hora por gênero", x = "Salário/hora") +
  scale_fill_manual("Legenda", values = c("darkblue", "firebrick2"), labels = c("Homem", "Mulher"))

g4 <- ggplot(dfMerge, aes(sal.hr))
g4 + geom_histogram(data = subset(dfMerge, branco == 0), binwidth = 10, alpha = 0.7, aes(fill = "Homem")) +
  geom_histogram(data = subset(dfMerge, branco == 1), binwidth = 10, alpha = 0.7, aes(fill = "Mulher")) +
  labs(title = "Distribuição do salário/hora por gênero", x = "Salário/hora") +
  scale_fill_manual("Legenda", values = c("darkblue", "firebrick2"), labels = c("Não brancos", "Brancos"))

# Violinplot do salário hora por gênero e por grupo de escolaridade
g5 <- ggplot(subset(dfMerge, sal.hr <= 100), aes(factor(mulher), sal.hr))
g5 + geom_violin() +
  labs(title = "Violin plot do salário/hora por gênero",
       x = "Gênero", y = "Salário/hora") +
  scale_x_discrete(labels = c('Homem','Mulher'))

g6 <- ggplot(subset(dfMerge, sal.hr <= 100 & !is.na(escol)), aes(factor(escol), sal.hr))
g6 + geom_violin() +
  labs(title = "Violin plot do salário/hora por grupo de escolaridade",
       x = "Escolaridade", y = "Salário/hora",
       caption = "1 - Nenhuma escolaridade; 2 - Fundamental; 3 - Médio; 4 - Superior")

# Violinplot da experiência por gênero
g7 <- ggplot(dfMerge, aes(factor(mulher), exper))
g7 + geom_violin() +
  labs(title = "Violin plot da experiência por gênero",
       x = "Gênero", y = "Experiência") +
  scale_x_discrete(labels = c('Homem','Mulher'))

# Matriz scatterplot entre salário hora, idade e experiência
pairs(~sal.hr + v8005 + exper, subset(dfMerge, sal.hr <= 100),
      main = "Matriz Scatterplot entre salário/hora, idade e experiência",
      col = "tomato3", labels = c("Salário/hora", "Idade", "Experiência"))

# Densidade do ln salário/hora por gênero
g8 <- ggplot(dfMerge, aes(ln.sal.hr))
g8 + geom_density(aes(fill = factor(mulher)), alpha = 0.7) +
  labs(title = "Densidade do ln do salário/hora por gênero",
       x = "Log. do Salário/hora", y = "Densidade") +
  scale_fill_manual("Legenda", values = c("darkblue", "firebrick2"), labels = c("Homem", "Mulher"))

