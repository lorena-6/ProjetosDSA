# Projeto 01 - Detecção de Fraudes no Tráfego de Cliques em Propagandas de Aplicações Mobile
# Lorena França Almeida


## Definição de Diretório e Importação da Amostra

# Definindo o diretório de trabalho
setwd("C:/FCD/BigDataRAzure/Cap20/Projeto01")

# Checando o diretório atual de trabalho
getwd()

# Importando arquivo da amostra reduzida
amostra1 <- read.csv("train_sample.csv")
head(amostra1)

## Data Munging e Análise Exploratória

# Explorando os dados
summary(amostra1)

str(amostra1)

# Convertendo a variável target para fator
amostra1$is_attributed <- as.factor(amostra1$is_attributed)

# Capturando a hora da variável click_time
library("lubridate")

amostra1$click_time <- as_datetime(amostra1$click_time)
amostra1$click_time <- format(amostra1$click_time,"%H")

# Visualizando a frequência de algumas variáveis

library("dplyr")
head(count(amostra1, app, sort = TRUE), 25)
head(count(amostra1, channel, sort = TRUE), 25)
head(count(amostra1, ip, sort = TRUE), 25)

nrow(distinct(amostra1, ip))
round((nrow(distinct(amostra1, ip))/100000)*100, 1)
100000-34857
round((65143/100000)*100, 1)
# Temos 34.857 ips distintos nessa amostra, em um total de 100.000 registros,
# representando 34,9% da amostra.

# Ips e suas respectivas quantidades de clicks
contador <- count(amostra1, ip, sort = TRUE)
head(contador)
nrow(contador)

# Quantidade de ips que clicaram mais de uma vez
IpMaiorQueUm <- subset(contador, n > 1, sort = TRUE)
head(IpMaiorQueUm)
nrow(IpMaiorQueUm)

# Quantidade de ips que clicaram somente uma vez
IpIgualAUm <- subset(contador, n == 1, sort = TRUE)
head(IpIgualAUm)
nrow(IpIgualAUm)

# Confirmando total de ips da divisão
nrow(IpIgualAUm) + nrow(IpMaiorQueUm)

# Inserindo a coluna is_attributed ao subset de ips que clicaram somente 1vez
juncao <- left_join(IpIgualAUm, amostra1 %>% select(ip, is_attributed),
                    by = c("ip" = "ip"))

head(juncao)
nrow(juncao)

# Quantidade de ips que clicaram só uma vez e não fizeram download
unicoNok <- subset(juncao, is_attributed == 0, sort = TRUE)
head(unicoNok)
nrow(unicoNok)
           
# Quantidade de ips que clicaram só uma vez e fizeram download  
unicoOk <- subset(juncao, is_attributed == 1, sort = TRUE)
head(unicoOk)
nrow(unicoOk)

# Confirmando subsets das quantidade de ips que clicaram somente 1vez
nrow(unicoNok) + nrow(unicoOk)

# Percentual de ips que clicaram só uma vez e fizeram download
(nrow(unicoOk)/nrow(IpIgualAUm))*100

# Dos ips que clicaram mais de uma vez, quantas foram as vezes que fizeram e que
# não fizeram download?
head(IpMaiorQueUm)
nrow(IpMaiorQueUm)

SemDown <- as.data.frame(amostra1 %>%
  filter(is_attributed == 0) %>%
  group_by(ip) %>%
  summarise(is_attributed = n()) %>%
              arrange(desc(is_attributed)))

head(SemDown)

ComDown <- as.data.frame(amostra1 %>%
                           filter(is_attributed == 1) %>%
                           group_by(ip) %>%
                           summarise(is_attributed = n()) %>%
                           arrange(desc(is_attributed)))

head(ComDown)

IpsRepetidos <- left_join(
                          left_join(IpMaiorQueUm, SemDown %>%
                          select(ip, "SemDown" = is_attributed),
                          by = c("ip" = "ip")),
                          ComDown %>%
                          select(ip, "ComDown" = is_attributed),
                          by = c("ip" = "ip"))
                         
head(IpsRepetidos)
nrow(IpsRepetidos)

nrow(subset(IpsRepetidos, ComDown >= 1))
(nrow(subset(IpsRepetidos, ComDown >= 1))/
    nrow(IpsRepetidos))*100
# Dos IPs que clicaram mais de 1 vez, 0,42% fizeram ao menos 1 download.

# Quantidade total de downloads da amostra
nrow(subset(IpsRepetidos, ComDown >= 1))+nrow(unicoOk)

# Do total de downloads, percentual dos que foram feitos por ips que clicaram
# somente uma vez:
(nrow(unicoOk)/(nrow(subset(IpsRepetidos, ComDown >= 1))+nrow(unicoOk)))*100

# Do total de downloads, percentual dos que foram feitos por ips que clicaram
# mais de uma vez:
(nrow(subset(IpsRepetidos, ComDown >= 1))/
(nrow(subset(IpsRepetidos, ComDown >= 1))+nrow(unicoOk)))*100

# Isso mostra que da nossa quantidade de downloads feitos, 67% foram realizados
# por IPs que clicaram somente 1vez. E 33% realizados por IPs que clicaram mais
# de uma vez. No entanto, destes IPs que clicaram mais de uma vez, de forma
# geral, a quantidade de clicks sem download é muito maior do que a quantidade
# de clicks com download, podendo supor que os IPs que clicam mais de uma vez
# possuem uma probabilidade maior de clicks fraudulentos do que os IPs que
# clicam apenas 1 vez. Tivemos 73 IPs nessa amostra que clicaram mais de uma vez
# e realizaram pelo menos 1 download:
clicsokipsduplos <- head(IpsRepetidos[order(IpsRepetidos$ComDown,
                                            decreasing = TRUE),],73)
clicsokipsduplos

# A quantidade de acessos dos 73 IPs com mais de um click sem download pode ser
# comparada com a quantidade de acessos com download dos mesmos IPs através dos
# gráficos de barras a seguir:
barplot(clicsokipsduplos$SemDown, ylim = c(0,700),
        main = "Acessos sem download por IPs com mais de 1 click,
        com pelo menos 1 download realizado")
barplot(clicsokipsduplos$ComDown, ylim = c(0,700),
        main = "Acessos com download por IPs com mais de 1 click")

# Ou seja, a classificação de um IP como repetido ou não nesse espaço de tempo
# pode ser significativa para a criação do modelo preditivo. Portanto, é criada
# uma coluna ao dataset da amostra classificando o IP como repetido = TRUE ou
# não repetido (único) = FALSE.

amostra1$IpRepetido <- amostra1$ip %in% IpMaiorQueUm$ip
head(amostra1)
nrow(amostra1[amostra1$IpRepetido == FALSE,])

# Convertendo a variável IpRepetido para fator
amostra1$IpRepetido <- as.factor(amostra1$IpRepetido)

# Observando os clicks por horário
library("ggplot2")

# Convertendo a variável click_time para integer
amostra1$click_time <- as.integer(amostra1$click_time)

ggplot(amostra1, aes(click_time)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=1,
  breaks = seq(0, 23, by = 1)) +
  labs(x = "Hora",
       title = "Hora dos clicks",
       y = "Quantidade de clicks") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 23, by=1))

# Convertendo a variável click_time para fator
amostra1$click_time <- as.factor(amostra1$click_time)

# Checando as conversões das variáveis
str(amostra1)

## Feature Selection

library("randomForest")

modelo <- randomForest(is_attributed ~ . - ip  - attributed_time, 
                        data = amostra1, 
                        ntree = 100, nodesize = 10, importance = T)

varImpPlot(modelo, main = "Importância das Variáveis")

# Variáveis selecionadas para construção do modelo
selectedSample <-
  amostra1[, c('app', 'device', 'os', 'channel', 'IpRepetido', 'is_attributed')]


## Balanceamento de Classe

# Checando proporção da variável target
round(prop.table(table(selectedSample$is_attributed))*100, 2)

df <- as.data.frame(round(prop.table(table(selectedSample$is_attributed))*100, 2))

ggplot(data=df, aes(x=Var1, y=Freq)) +
  geom_text(aes(x = Var1, y = Freq), label = df$Freq, vjust = -1) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = "Valores da variável target (is_attributed)",
       title = "Proprorção da variável target",
       y = "Percentual (%)") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,110))

# Nota-se que a variável está bastante desbalanceada, tendo cerca de 99,77% de registros para a classe 0 e 0,23% para a classe 1.

# Dividindo os dados em treino e teste - 70:30 ratio
set.seed(2021)
indexes <- sample(1:nrow(selectedSample), size = 0.7 * nrow(selectedSample))
train.data <- selectedSample[indexes,]
test.data <- selectedSample[-indexes,]

# Checando proporção da divisão para cada valor da classe
prop.table(table(train.data$is_attributed))
prop.table(table(test.data$is_attributed))

# Balanceando a classe
library(ROSE)

# Aplicando ROSE em dados de treino e checando a proporção de classes
rose_treino <- ROSE(is_attributed ~ ., data = train.data, seed = 1)$data
prop.table(table(rose_treino$is_attributed))

# Aplicando ROSE em dados de teste e checando a proporção de classes
rose_teste <- ROSE(is_attributed ~ ., data = test.data, seed = 1)$data
prop.table(table(rose_teste$is_attributed))

# Com uma proporção mais balanceada, segue-se com a criação do modelo.


## Criação e Score do Modelo Supervisionado de Machine Learning

# Ativando pacote para modelo de árvore de decisão
library(C50)

# Criando o modelo com os dados de treino balanceados
set.seed(2021)
modelotree <- C5.0(is_attributed ~ ., data = rose_treino)

# Gerando previsões nos dados de teste balanceados
set.seed(2021)
previsoestree <- data.frame(observado = rose_teste$is_attributed,
                            previsto = predict(modelotree, newdata = rose_teste))

## Avaliação do Modelo

# Avaliando o modelo
library(caret)
confusionMatrix(previsoestree$observado, previsoestree$previsto)
# Acurácia de 89,28%

# Gerando as classes de dados para curva ROC
classe1 <- predict(modelotree, newdata = rose_teste, type = 'prob')
classe2 <- rose_teste$is_attributed

# Gerando a curva ROC
library("ROCR")
pred <- prediction(classe1[,2], classe2)
perf <- performance(pred, "tpr","fpr") 
curva <- plot(perf, col = rainbow(10))

# Verificando AUC:
library("pROC")
calcAUC <- roc(classe2, classe1[,2])
auc(calcAUC)
# AUC de 95,27%

# Conclusão: Foi criado um modelo de árvore de decisão com acurácia de 89% e
# área abaixo da curva de 95%.

