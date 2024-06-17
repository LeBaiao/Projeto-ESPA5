#Bibliotecas Utilizadas

install.packages("readr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("vcd")

library(readr)
library(readxl)
library(ggplot2)
library(dplyr)
library(readr)
library(vcd)

#importando base de dados
dados <- read.csv("fa_casoshumanos_1994-2023.csv", sep = ";")
dadosPrimatas <- read.csv("fa_epizpnh_1999-2023.csv", sep = ";")

#Importando e tratando os dados
dados <- na.omit(dados)
dados$IDADE <- as.numeric(dados$IDADE)
dados$DT_OBITO <- dmy(dados$DT_OBITO)
dados$SEXO <- factor(dados$SEXO, levels = c("M", "F"))
dados$MACRORREG_LPI <- factor(dados$MACRORREG_LPI)
dados$UF_LPI <- factor(dados$UF_LPI, levels = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"))
dados$OBITO <- factor(dados$OBITO, levels = c("SIM"))

#Análise geral dos dados
summary(dados)
summary(dadosPrimatas)


#Qual a faixa etária com o maior número de casos de humanos afetados pela febre amarela entre 1994 e 2023, independentemente do resultado do óbito?
hist(dados$IDADE, main = 'Distribuição de idades', xlab = 'Idade', ylab = 'Frequência')



#Quais estados tiveram mais humanos afetados pela febre amarela no período de 1994 a 2023?
# Separando casos por estado
casos_x_estado <- count(dados, dados$UF_LPI, sort = TRUE)
# Adicionando frequência relativa
casos_x_estado <- casos_x_estado %>%
  mutate(frequencia_relativa = n/sum(n))
names(casos_x_estado) <- c('UF', 'NUM_OCORRENCIAS', 'FREQUENCIA_RELATIVA')
ggplot(casos_x_estado, aes(x = reorder(UF, -NUM_OCORRENCIAS), y = NUM_OCORRENCIAS, fill = FREQUENCIA_RELATIVA)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient(low = "lightpink", high = "darkred") +
  labs(title = "Ocorrências por Estado",
       x = "Estado",
       y = "Número de Ocorrências") +
  theme_minimal()



#Qual a proporção de humanos homens e mulheres atingidos pela febre amarela?
#grafico 1
casos_x_sexo <- count(dados, dados$SEXO)
casos_x_sexo <- casos_x_sexo %>%
  mutate(frequencia_relativa = n/sum(n))
names(casos_x_sexo) <- c('SEXO', 'OCORRENCIAS', 'FREQ_RELATIVA')
ggplot(casos_x_sexo, aes(x = reorder(SEXO, -OCORRENCIAS), y = OCORRENCIAS, fill = FREQ_RELATIVA)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Ocorrências por Sexo",
       x = "Sexo",
       y = "Número de Ocorrências") +
  theme_minimal()
#grafico 2
ggplot(dados, aes(x = dados$IDADE, fill = dados$SEXO)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribuição de Idade por Sexo",
       x = "Idade",
       y = "Densidade",
       fill = "Sexo") +
  theme_minimal()




#Quantos humanos foram afetados pela febre amarela em cada região do Brasil?
casos_por_macrorregiao <- as.data.frame(table(dados$MACRORREG_LPI))  
# Renomear colunas para legibilidade
colnames(casos_por_macrorregiao) <- c("Macrorregião", "Número de Casos")
# Gráfico de barras para distribuição de casos por macrorregião
ggplot(casos_por_macrorregiao, aes(x = reorder(Macrorregião, -`Número de Casos`), y = `Número de Casos`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Distribuição de Casos por Macrorregião",
       x = "Macrorregião",
       y = "Número de Casos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#Quantos casos de primatas não humanos afetados pela febre amarela em cada região do Brasil?
casos_por_macrorregiao_primatas <- as.data.frame(table(dadosPrimatas$MACRORREG_OCOR))
#Renomear as colunas para legibilidade
colnames(casos_por_macrorregiao_primatas) <- c("Macrorregião", "Número de Casos")
#Gráfico de barras para distribuição de casos por macrorregião
ggplot(casos_por_macrorregiao_primatas, aes(x = reorder(Macrorregião, -`Número de Casos`), y = `Número de Casos`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Distribuição de Casos por Macrorregião",
       x = "Macrorregião",
       y = "Número de Casos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






#Qual o número de casos fatais de humanos afetados pela febre amarela, independentemente do gênero?
contagem_obito <- table(dados$OBITO)
# Acessar a quantidade de dados na categoria "SIM"
quantidade_sim <- contagem_obito["SIM"]
quantidade_nao <- contagem_obito["N\xc3O"]
quantidade_ignorado <- contagem_obito["IGN"]
# Vetor com os dados
quantidades <- c(quantidade_sim, quantidade_nao, quantidade_ignorado)
nomes_variaveis <- c("SIM", "NÃO", "IGNORADO")
# Criar o gráfico de barras
barplot(quantidades, names.arg = nomes_variaveis, col = "skyblue", 
        main = "Comparação de casos de Mortes e sobrevivência",
        ylab = "Quantidade de Dados", ylim =c(0, max(quantidades) * 1.2))






#Qual a probabilidade de uma mulher, afetada pela febre amarela, vir a óbito?
#P(A∣B)=P(A∩B)/ P(B)
#P(A) = Pessoa vir a óbito
#P(B) = Pessoa com febre amarela ser do sexo feminino
#P(A|B) = pessoa vir a obito e ser do sexo feminino

table(dados$SEXO)
prob_M <- mean(dados$SEXO == "M")
prob_F <- mean(dados$SEXO == "F")
mean(dados$SEXO == "F" & dados$OBITO == "SIM")






#Qual a probabilidade de um homem, afetado pela febre amarela, vir a óbito?
table(dados$SEXO)
prob_M <- mean(dados$SEXO == "M")
mean(dados$SEXO == "M" & dados$OBITO == "SIM")





#Há diferença na média de idades entre os humanos afetados pela febre amarela, para ambos os gêneros?
##Determinando se a distribuição de idade é normal:
hist(dados$IDADE)
boxplot(dados$IDADE)
qqnorm(dados$IDADE)
qqline(dados$IDADE)
shapiro.test(dados$IDADE)
t.test(IDADE ~ SEXO, data = dados)





#Há diferença na proporção de casos e óbitos por gênero para humanos?
#Tabela de contingência para a variável OBITO em relação à SEXO
table(dados$OBITO, dados$SEXO)
assoc_measure <- assocstats(table(dados$OBITO, dados$SEXO))
assoc_measure$chisq

#ESTADO
table(dados$OBITO, dados$UF_LPI)
assoc_measure <- assocstats(table(dados$OBITO, dados$UF_LPI))
assoc_measure$chisq

#Faixa_etaria
table(dados$OBITO, dados$IDADE)
assoc_measure <- assocstats(table(dados$OBITO, dados$IDADE))
assoc_measure$chisq

df <- as.data.frame(assoc_measure)
df$IDADE <- rownames(df)


chisq.test(data$SEXO, data$OBITO, data = data)



  
