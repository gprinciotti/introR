### ANÁLISE ESTATÍSTICA

# Matriz de correlação entre variáveis contínuas
vars <- c("sal.hr", "exper", "v4803", "v8005", "v9892")
dfMergeC <- dfMerge[vars]
corr <- cor(dfMergeC, use="complete.obs")
print(corr)
              
ggcorrplot(corr, hc.order = TRUE, lab = TRUE, lab_size = 3,
           colors = c("tomato3", "white", "springgreen3"), 
           title="Matriz de correlação") +
  scale_x_discrete(labels = c('Exp.','Idade','Sal/hr', 'Estudo', 'Trab.')) + 
  scale_y_discrete(labels = c('Exp.','Idade','Sal/hr', 'Estudo', 'Trab.')) + 
  
rm(list = "corr", "vars", "dfMergeC")

# Teste T do salário hora entre homens e mulheres total
with(dfMerge, t.test(sal.hr ~ mulher))

# Teste T do salário hora entre homens e mulheres considerando grupo de idade
for (i in 1:5){
  ttest <- with(subset(dfMerge, idade == i), t.test(sal.hr ~ mulher))
  print(ttest)
  rm(list = "i", "ttest")
}

# Teste T do salário hora entre homens e mulheres considerando cor
with(subset(dfMerge, branco == 0), t.test(sal.hr ~ mulher))
with(subset(dfMerge, branco == 1), t.test(sal.hr ~ mulher))

# Teste T do salário hora entre homens e mulheres considerando grupo de escolaridade
for (i in 1:4){
  ttest <- with(subset(dfMerge, escol == i), t.test(sal.hr ~ mulher))
  print(ttest)
  rm(list = "i", "ttest")
}

# Teste T do salário hora entre homens e mulheres considerando grupo de escolaridade e cor
for (i in 1:4){
  ttest1 <- with(subset(dfMerge, escol == i & branco == 0), t.test(sal.hr ~ mulher))
  print(ttest1)
  ttest2 <- with(subset(dfMerge, escol == i & branco == 1), t.test(sal.hr ~ mulher))
  print(ttest2)
  rm(list = "i", "ttest1", "ttest2")
}

# Regressão múltipla
dfMerge$exper_2 <- dfMerge$exper^2
reg <- lm(sal.hr ~ idade + branco + mulher + escol + exper + exper_2, dfMerge)
summary(reg)

# Oaxaca
oaxaca <- oaxaca(sal.hr ~ idade + branco + escol + exper + exper_2 | mulher, dfMerge, R= NULL)
summary(oaxaca)
