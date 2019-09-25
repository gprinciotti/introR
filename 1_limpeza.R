### LIMPEZA DO BANCO DE DADOS

# Sexo (mulher == 1)
dfMerge$mulher[dfMerge$v0302 == 4] <- 1
dfMerge$mulher[dfMerge$v0302 == 2] <- 0

# Raça (brancos == 1)
dfMerge$branco[dfMerge$v0404 == 2 | dfMerge$v0404 == 6] <- 1
dfMerge$branco[dfMerge$v0404 == 4 | dfMerge$v0404 == 8 | dfMerge$v0404 == 0] <- 0

# Urbano (área urbana == 1)
dfMerge$urbano[dfMerge$v4728 == 1 | dfMerge$v4728 == 2 | dfMerge$v4728 == 3] <- 1
dfMerge$urbano[dfMerge$v4728 == 4 | dfMerge$v4728 == 5 | dfMerge$v4728 == 6 | dfMerge$v4728 == 7 | dfMerge$v4728 == 8] <- 0

# Região Metropolitana (região metropolitana == 1)
dfMerge$metro[dfMerge$v4727 == 1] <- 1
dfMerge$metro[dfMerge$v4727 == 2 | dfMerge$v4727 == 3] <- 0

# Trabalho (trabalhou na semana de ref. == 1)
dfMerge$trab[dfMerge$v9001 == 1] <- 1
dfMerge$trab[dfMerge$v9001 == 3] <- 0

# Atividade (economicamente ativo == 1)
dfMerge$ativ[dfMerge$v4713 == 1] <- 1
dfMerge$ativ[dfMerge$v4713 == 2] <- 0

# Ocupada (ocupado == 1)
dfMerge$ocup[dfMerge$v4814 == 1] <- 1
dfMerge$ocup[dfMerge$v4814 == 2] <- 0

# Grupo de escolaridade
dfMerge$escol[dfMerge$v4803 == 1] <- 1 # nunca estudou
dfMerge$escol[dfMerge$v4803 >= 2 & dfMerge$v4803 <= 10] <- 2 # ensino fundamental
dfMerge$escol[dfMerge$v4803 >= 11 & dfMerge$v4803 <= 13] <- 3 # ensino médio
dfMerge$escol[dfMerge$v4803 >= 14 & dfMerge$v4803 <= 16] <- 4 # ensino superior

# Grupo de idade
dfMerge$idade[dfMerge$v8005 >= 25 & dfMerge$v8005 <= 30] <- 1 # entre 25 e 30
dfMerge$idade[dfMerge$v8005 >= 31 & dfMerge$v8005 <= 35] <- 2 # entre 31 e 35
dfMerge$idade[dfMerge$v8005 >= 36 & dfMerge$v8005 <= 40] <- 3 # entre 36 e 40
dfMerge$idade[dfMerge$v8005 >= 41 & dfMerge$v8005 <= 45] <- 4 # entre 41 e 45
dfMerge$idade[dfMerge$v8005 >= 46 & dfMerge$v8005 <= 50] <- 5 # entre 46 e 50

# Experiência (idade do morador - idade em que começou a trabalhar)
dfMerge$exper <- (dfMerge$v8005 - dfMerge$v9892)

# Salário/hora e log do salário
dfMerge$v9532[dfMerge$v9532 == 999999999999] <- NA
dfMerge$sal.hr <- dfMerge$v9532 / (dfMerge$v9058*4)
dfMerge$ln.sal.hr <- log(dfMerge$sal.hr)
