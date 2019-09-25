### ABERTURA DA BASE
dfPessoas <- as.data.frame(fread("dados/pessoa_2015.csv",
                                 header = TRUE, sep = ",", integer64='double',
                                 select=c('v0102','v0103','v0403','v0301',
                                          'uf','v9532','v9058','v0302',
                                          'v0404','v4803','v9001','v4713',
                                          'v4814','v8005','v9892','v4817',
                                          'v0711','v4727','v4728'))[which(uf == '12' & v8005 >= 25 & v8005 <= 50)])

dfDomicilios <- as.data.frame(fread("dados/dom_2015.csv", integer64='double',
                                 header = TRUE, sep = ",", select = c('v0102','v0103','v0207','uf'))[which(uf == '12')])

### CRIANDO IDENTIFICAÇÃO ÚNICA DOS ENTREVISTADOS NA BASE DE PESSOAS
dfPessoas$idFamilia <- dfPessoas$v0102*10000+10*dfPessoas$v0103+dfPessoas$v0403
dfPessoas$idPessoa <- dfPessoas$v0102*100000+dfPessoas$v0103*100+dfPessoas$v0301

### MERGE DOS BANCOS DE DADOS
dfMerge <- merge(dfPessoas, dfDomicilios,
                 by=c("v0102","v0103","uf"), keep.all=TRUE)

### ANALISANDO TAXA DE OCUPAÇÃO E TAXA DE ATIVIDADE
ocup <- prop.table(table(dfMerge$v4814))
ocup

ativ <- prop.table(table(dfMerge$v4713))
ativ 

rm(list = "ocup", "ativ")

### MANTENDO APENAS PESSOAS QUE ESTÃO TRABALHANDO
dfMerge <- dfMerge[ which(dfMerge$v4814 == 1), ]
