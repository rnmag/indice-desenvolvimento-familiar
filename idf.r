## SCRIPT PARA CALCULO DO IDF

# install.packages(c("foreign", "dplyr"))
library(foreign)
library(dplyr)

# pessoas <- read.spss("pesssoas3.sav",
#                      trim.factor.names=T,
#                      to.data.frame=T,
#                      use.missings=T)
# 
# propriedades <- read.spss("propriedades5.sav",
#                           trim.factor.names=T,
#                           to.data.frame=T,
#                           use.missings=T)
# save.image("idf.RData")

load("idf.RData")


## Organização e limpeza do banco ----
# Eliminação de rótulos do SPSS em variáveis numéricas, não suportados pelo dplyr
attr(pessoas$X.3.2.24, "value.labels") <- attr(pessoas$X.3.2.25, "value.labels") <- 
attr(pessoas$X.3.2.26, "value.labels") <- attr(pessoas$X.3.2.27, "value.labels") <- 
attr(pessoas$X.3.2.28, "value.labels") <- attr(pessoas$X.3.2.29, "value.labels") <- 
attr(pessoas$X.3.2.30, "value.labels") <- attr(pessoas$X.3.2.31, "value.labels") <- 
attr(pessoas$X.3.2.32, "value.labels") <- attr(pessoas$X.3.2.33, "value.labels") <- 
attr(pessoas$X.3.2.34, "value.labels") <- attr(pessoas$X.3.2.35, "value.labels") <- NULL


## Cálculo das variáveis de apoio ----
# p = número de pessoas por domicílio
pessoas <- pessoas %>%
  group_by(ID) %>%
  mutate(p = n())

# o = dummy ocupado / não-ocupado
pessoas$o <- ifelse(pessoas$X.3.2.18 == "Autônomo com INSS       ", 1,
              ifelse(pessoas$X.3.2.18 == "Assalariado sem registro", 1,
               ifelse(pessoas$X.3.2.18 == "Autônomo sem INSS       ", 1,
                ifelse(pessoas$X.3.2.18 == "Empregador              ", 1,
                 ifelse(pessoas$X.3.2.18 == "Bico                    ", 1,
                  ifelse(pessoas$X.3.2.18 == "Funcionário público     ", 1,
                   ifelse(pessoas$X.3.2.18 == "Registrado              ", 1, 0)))))))

# r = renda familiar (incluindo benefícios)
pessoas <- pessoas %>%
  group_by(ID) %>%
  mutate(r = sum(renda_total, na.rm=T))

# b = renda familiar proveniente de benefícios (Bolsa Família e BPC)
pessoas$b <- rowSums(cbind(pessoas$X.3.2.33,
                           pessoas$X.3.2.34), na.rm=T)
pessoas <- pessoas %>%
 group_by(ID) %>%
 mutate(b = sum(b, na.rm=T))

# i = idade esperada com base na escolaridade (para cálculo do atraso escolar)
pessoas$i <- ifelse(pessoas$X.3.2.14 == "Pré-escola: (faixa etária 4 e 5 anos de idade)", 5,
              ifelse(pessoas$X.3.2.14 == "1º ano do ensino fundamental                  ", 6,
               ifelse(pessoas$X.3.2.14 == "2º ano/ 1ª série do ensino fundamental        ", 7,
                ifelse(pessoas$X.3.2.14 == "3º ano/2ª série do ensino fundamental         ", 8,
                 ifelse(pessoas$X.3.2.14 == "4º ano/3ª série do ensino fundamental         ", 9,
                  ifelse(pessoas$X.3.2.14 == "5º ano/4ª série do ensino fundamental         ", 10,
                   ifelse(pessoas$X.3.2.14 == "6º ano/5ª série do ensino fundamental         ", 11,
                    ifelse(pessoas$X.3.2.14 == "7º ano/6ª série do ensino fundamental         ", 12,
                     ifelse(pessoas$X.3.2.14 == "8º ano/7ª série do ensino fundamental         ", 13,
                      ifelse(pessoas$X.3.2.14 == "9º ano/8ª série do ensino fundamental         ", 14, 99))))))))))

# d = total de despesas do domicílio
propriedades$d <- rowSums(cbind(propriedades$X.4.3.1.3.18.1,
                                propriedades$X.4.3.2.3.18.1,
                                propriedades$X.4.3.3.3.18.1,
                                propriedades$X.4.3.4.3.18.1,
                                propriedades$X.4.3.5.3.18.1,
                                propriedades$X.4.3.6.3.18.1,
                                propriedades$X.4.3.7.3.18.1,
                                propriedades$X.4.3.8.3.18.1,
                                propriedades$X.4.3.9.3.18.1,
                                propriedades$X.4.3.10.3.18.1,
                                propriedades$X.4.3.11.3.18.1), na.rm=T)


## Indicadores de vulnerabilidade da família ----
# V1 = Ausência de gestantes
# V2 = Ausência de mãe amamentando
# V3 = Ausência de crianças (até 6 anos) 
# V4 = Ausência de crianças ou adolescentes (até 14 anos)
# V5 = Ausência de crianças, adolescentes ou jovens (até 17 anos)
# V6 = Ausência de portadores de deficiência
# v7 = Ausência de idosos
# V8 = Presença de cônjuge
# V9 = Mais da metade dos membros da família encontra-se em idade ativa (15 anos ou mais)

pessoas$V1 <- ifelse(pessoas$X.3.2.10 == "Sim", 0, 1)
pessoas$V2 <- ifelse(pessoas$X.3.2.11 == "Sim", 0, 1)
pessoas$V3 <- ifelse(pessoas$X.3.2.5.1 <= 6, 0, 1)
pessoas$V4 <- ifelse(pessoas$X.3.2.5.1 <= 14, 0, 1)
pessoas$V5 <- ifelse(pessoas$X.3.2.5.1 <= 17, 0, 1)
pessoas$V6 <- ifelse(pessoas$X.3.2.8.2 == "Sim", 0,
               ifelse(pessoas$X.3.2.8.3 == "Sim", 0,
                ifelse(pessoas$X.3.2.8.4 == "Sim", 0,
                 ifelse(pessoas$X.3.2.8.5 == "Sim", 0,
                  ifelse(pessoas$X.3.2.8.6 == "Sim", 0,
                   ifelse(pessoas$X.3.2.8.7 == "Sim", 0, 1))))))
pessoas$V7 <- ifelse(pessoas$X.3.2.5.1 >= 60, 0, 1)
pessoas$V8 <- ifelse(pessoas$X.3.2.7 == "União consensual sem registro em cartório", 1,
               ifelse(pessoas$X.3.2.7 == "Casado                                   ", 1,
                ifelse(pessoas$X.3.2.7 == "União consensual com registro em cartório", 1, 0)))
pessoas <- pessoas %>%
  group_by(ID) %>%
  mutate(V9 = as.numeric((sum(X.3.2.5.1 >= 15, na.rm=T) / p) > 0.5))


##  Indicadores de acesso ao conhecimento ----
# A1 = Ausência de adultos analfabetos
# A2 = Ausência de adultos analfabetos funcionais
# A3 = Presença de pelo menos um adulto com fundamental completo
# A4 = Presença de pelo menos um adulto com secundário completo
# A5 = Presença de pelo menos um adulto com educação superior
 
pessoas$A1 <- ifelse(pessoas$X.3.2.14 == "Nenhuma (não sabe ler/escrever)               ", 0, 1)
pessoas$A2 <- ifelse(pessoas$X.3.2.14 == "Nenhuma (lê e escreve)                        ", 0, 1)
pessoas$A3 <- ifelse(pessoas$X.3.2.5.1 < 18, 0,
               ifelse(pessoas$X.3.2.14 == "5º ano/4ª série do ensino fundamental         ", 1,
                ifelse(pessoas$X.3.2.14 == "6º ano/5ª série do ensino fundamental         ", 1,
                 ifelse(pessoas$X.3.2.14 == "7º ano/6ª série do ensino fundamental         ", 1,
                  ifelse(pessoas$X.3.2.14 == "8º ano/7ª série do ensino fundamental         ", 1,
                   ifelse(pessoas$X.3.2.14 == "9º ano/8ª série do ensino fundamental         ", 1,
                    ifelse(pessoas$X.3.2.14 == "1º série do ensino médio                      ", 1,
                     ifelse(pessoas$X.3.2.14 == "2º série do ensino médio                      ", 1,
                      ifelse(pessoas$X.3.2.14 == "3º série do ensino médio                      ", 1,
                       ifelse(pessoas$X.3.2.14 == "Superior incompleto                           ", 1,              
                        ifelse(pessoas$X.3.2.14 == "Superior completo                             ", 1, 0)))))))))))
pessoas$A4 <- ifelse(pessoas$X.3.2.5.1 < 18, 0,
               ifelse(pessoas$X.3.2.14 == "3º série do ensino médio                      ", 1,
                ifelse(pessoas$X.3.2.14 == "Superior incompleto                           ", 1,              
                 ifelse(pessoas$X.3.2.14 == "Superior completo                             ", 1, 0))))
pessoas$A5 <- ifelse(pessoas$X.3.2.14 == "Superior completo                             ", 1, 0)


## Indicadores de acesso ao trabalho ----
# T1 = Mais da metade dos membros em idade ativa encontra-se ocupado
# T2 = Presença de pelo menos um ocupado em setor formal
# T3 = Presença de pelo menos um ocupado em atividade não agrícola
# T4 = Presença de pelo menos um ocupado com rendimento superior a 1 SM
# T5 = Presença de pelo menos um ocupado com rendimento superior a 2 SM

pessoas <- pessoas %>%
  group_by(ID) %>%
  mutate(T1 = as.numeric((sum(o == 1, na.rm=T) / p) > 0.5))
pessoas$T2 <- ifelse(pessoas$X.3.2.18 == "Registrado              ", 1,
               ifelse(pessoas$X.3.2.18 == "Autônomo com INSS       ", 1,
                ifelse(pessoas$X.3.2.18 == "Empregador              ", 1,
                 ifelse(pessoas$X.3.2.18 == "Funcionário público     ", 1, 0))))
pessoas$T3 <- ifelse(pessoas$o == 0 | pessoas$X.3.2.21 == "Agricultura     ", 0, 1)
pessoas <- pessoas %>%
  group_by(ID) %>%
  mutate(T4 = as.numeric((renda_total >= 724)))
pessoas <- pessoas %>%
  group_by(ID) %>%
  mutate(T5 = as.numeric((renda_total >= 1448)))

 
## Indicadores de desenvolvimento infantil ----
# D1 = Ausência de pelo menos uma criança de menos de 10 anos trabalhando
# D2 = Ausência de pelo menos uma criança de menos de 16 anos trabalhando
# D3 = Ausência de pelo menos uma criança de 0-6 anos  fora da escola
# D4 = Ausência de pelo menos uma criança de 7-14 anos fora da escola
# D5 = Ausência de pelo menos uma criança de 7-17 anos fora da escola
# D6 = Ausência de pelo menos uma criança com até 14 anos com mais de 2 anos de atraso
# D7 = Ausência de pelo menos um adolescente de 10 a 14 anos analfabeto
# D8 = Ausência de pelo menos um jovem de 15 a 17 anos analfabeto

pessoas$D1 <- ifelse(pessoas$X.3.2.5.1 <= 10 & pessoas$o == 1, 0, 1)
pessoas$D2 <- ifelse(pessoas$X.3.2.5.1 <= 16 & pessoas$o == 1, 0, 1)
pessoas$D3 <- ifelse(pessoas$X.3.2.5.1 <= 6 & pessoas$X.3.2.13 != "Sim                   ", 0, 1)
pessoas$D4 <- ifelse((pessoas$X.3.2.5.1 >= 7 & pessoas$X.3.2.5.1 <= 14) & pessoas$X.3.2.13 != "Sim                   ", 0, 1)
pessoas$D5 <- ifelse((pessoas$X.3.2.5.1 >= 7 & pessoas$X.3.2.5.1 <= 17) & pessoas$X.3.2.13 != "Sim                   ", 0, 1)
pessoas$D6 <- ifelse(pessoas$X.3.2.5.1 > 14, 1, as.numeric(pessoas$X.3.2.5.1 - pessoas$i <= 2))
pessoas$D7 <- ifelse((pessoas$X.3.2.5.1 >= 10 & pessoas$X.3.2.5.1 <= 14) & pessoas$X.3.2.14 == "Nenhuma (não sabe ler/escrever)               ", 0, 1)
pessoas$D8 <- ifelse((pessoas$X.3.2.5.1 >= 15 & pessoas$X.3.2.5.1 <= 17) & pessoas$X.3.2.14 == "Nenhuma (não sabe ler/escrever)               ", 0, 1)


## Indicadores de condições habitacionais ----
# H1 = Domicílio próprio
# H2 = Domicílio próprio, cedido ou invadido
# H3 = *** CALCULADA NO FIM DO SCRIPT, DEPENDE DO MERGE DOS BANCOS DE PROPRIEDADES E PESSOAS
# H4 = Material de construção permanente
# H5 = Acesso adequado à agua
# H6 = Esgotamento sanitário adequado
# H7 = Lixo é coletado
# H8 = Acesso à eletricidade

propriedades$H1 <- ifelse(propriedades$X.4.3.1 == " Próprio ", 1, 0)
propriedades$H2 <- ifelse(propriedades$X.4.3.1 == " Próprio ", 1,
                    ifelse(propriedades$X.4.3.1 == "Cedido   ", 1, 0))
propriedades$H4 <- ifelse(propriedades$X.4.3.5 == "Alvenaria com revestimento     ", 1,
                    ifelse(propriedades$X.4.3.5 == "Alvenaria sem revestimento     ", 1, 0))
propriedades$H5 <- ifelse(propriedades$X.4.3.9.1 == "Sim", 1, 0)
propriedades$H6 <- ifelse(propriedades$X.4.3.8.1 == "Sim", 1, 0)
propriedades$H7 <- ifelse(propriedades$X.4.3.13.1 == "Sim", 1,
                    ifelse(propriedades$X.4.3.13.2 == "Sim", 1,
                     ifelse(propriedades$X.4.3.13.3 == "Sim", 1, 0)))
propriedades$H8 <- ifelse(propriedades$X.4.3.12 == "Sim", 1, 0)


## Agregamento do banco de pessoas e merge com banco de propriedades ----
# Nota: são utilizadas duas funções para agregar as variáveis, max() e min(). A
# primeira é utilizada para as variáveis que assumem valor 1 na PRESENÇA de uma 
# característica (ex: presença de cônjuge). A segunda, para variáveis que assumem
# valor 1 na AUSÊNCIA de uma característica (ex: ausência de gestantes)


# Criaçao do banco de dados com os indicadores agregados por domicílio
max.agreg <- pessoas %>%
 group_by(ID) %>%
 select(p:i, V8:V9, A3:A5, T1:T5) %>%
 summarise_each(funs(max))

min.agreg <- pessoas %>%
  group_by(ID) %>%
  select(V1:V7, A1:A2, D1:D8) %>%
  summarise_each(funs(min))

agreg <- merge(max.agreg, min.agreg, "ID")


# Criação do banco master, juntando pessoas e propriedades
master <- merge(propriedades, agreg, "ID")

## Indicadores dependentes do merge entre pessoas e propriedades ----
# H3 = Densidade de até 2 moradores por dormitório
# R1 = Despesa familiar per capita superior à linha de extrema pobreza (R$77,00)
# R2 = Renda familiar per capita superior à linha de extrema pobreza (R$77,00)
# R3 = Despesa com alimentos superior à linha de extrema pobreza (R$77,00)
# R4 = Despesa familiar per capita superior à linha de pobreza (R$154,00)
# R5 = Renda familiar per capita superior à linha de pobreza (R$154,00)
# R6 = Maior parte da renda familiar não advém de transferências

master <- master %>%
 mutate(H3 = as.numeric(p / X.4.3.6.3.8.1 <= 2))
master$R1 <- as.numeric((master$d / master$p) > 77)
master$R2 <- as.numeric((master$r / master$p) > 77)
master$R3 <- as.numeric(master$X.4.3.4.3.18.1 > 77)
master$R4 <- as.numeric((master$d / master$p) > 154)
master$R5 <- as.numeric((master$r / master$p) > 154)
master$R6 <- as.numeric((master$b / master$r) < 0.5)

## Criação dos índices por dimensão e do índice geral (IDF) ----
# Índice de vulnerabilidade
master$vulnerabilidade <- rowMeans(cbind(master$V1,
                                         master$V2,
                                         master$V3,
                                         master$V4,
                                         master$V5,
                                         master$V6,
                                         master$V7,
                                         master$V8,
                                         master$V9), na.rm=T)

# Índice de acesso ao conhecimento
master$acesso <- rowMeans(cbind(master$A1,
                                master$A2,
                                master$A3,
                                master$A4,
                                master$A5), na.rm=T)

# Índice de acesso ao trabalho
master$trabalho <- rowMeans(cbind(master$T1,
                                  master$T2,
                                  master$T3,
                                  master$T4,
                                  master$T5), na.rm=T)

# Índice de desenvolvimento infantil
master$des.infantil <- rowMeans(cbind(master$D1,
                                      master$D2,
                                      master$D3,
                                      master$D4,
                                      master$D5,
                                      master$D6,
                                      master$D7,
                                      master$D8), na.rm=T)

# Índice de condições habitacionais
master$habitacao <- rowMeans(cbind(master$H1,
                                   master$H2,
                                   master$H3,
                                   master$H4,
                                   master$H5,
                                   master$H6,
                                   master$H7,
                                   master$H8), na.rm=T)

# Índice de disponibilidade de recursos
master$recursos <- rowMeans(cbind(master$R1,
                                  master$R2,
                                  master$R3,
                                  master$R4,
                                  master$R5,
                                  master$R6), na.rm=T)

# ÍDF - Índice de desenvolvimento da Família
master$IDF <- rowMeans(cbind(master$vulnerabilidade,
                             master$acesso,
                             master$trabalho,
                             master$des.infantil,
                             master$habitacao,
                             master$recursos), na.rm=T)
                                
## Tabulação cruzada ----
(idf.medio <- master %>%
 group_by(Localidades) %>%
 summarise(vulnerabilidade = mean(vulnerabilidade, na.rm=T),
           acesso = mean(acesso, na.rm=T),
           trabalho = mean(trabalho, na.rm=T),
           des.infantil = mean(des.infantil, na.rm=T),
           habitacao = mean(habitacao, na.rm=T),
           recursos = mean(recursos, na.rm=T),
           IDF = mean(IDF, na.rm=T)))
           
# write.csv(idf.medio, file = "idf.csv")
          
