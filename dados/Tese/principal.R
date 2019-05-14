#install.packages("knitr")
#install.packages("kableExtra")
#install.packages("corrplot")
#install.packages("RMariaDB")
#install.packages("outliers")

library(knitr)
library(kableExtra)
library(corrplot)
library(RMariaDB)
library(outliers)

diretorio_trabalho = "C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_estudo_caso/analise_exploratoria/"

#Obtem os dados
DADOS_BRUTOS_PROJETOS <- read.delim2("C:/Users/jandi/Dropbox/DOUTORADO/DADOS_EXPERIMENTO/DADOS_BRUTOS_PROJETOS.txt", stringsAsFactors = F, sep="\t", dec=",")

# Numero de pesquisas divida tecnica
numero_pesquisas <- read.delim2("C:/Users/jandi/Dropbox/DOUTORADO/DADOS_EXPERIMENTO/pesquisa_divida.txt", stringsAsFactors = F, sep="\t")
pdf("C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_introducao/numero_citacoes.pdf",width = 6.7, height = 5 )
par(mar=c(6, 3 ,1,1))
numero_citacoes = barplot(numero_pesquisas$citacoes,names = numero_pesquisas$ano, las=2,cex.axis=0.75,cex.names=0.75)

dev.off()

barplot(table(numero_pesquisas$ano), las=2,xlim=c(0,35),ylim=c(0,1200),cex.axis=0.75,cex.names=0.75)


#Separa os dados numericos
dados_numericos = DADOS_BRUTOS_PROJETOS[c("SUM_PAGERANK_USERS","COMMITS","AUTHORS","WATCHERS","COMMITS","PULL_REQUESTS","IC","CLASSES_5","CODE_SMELLS_5","COGNITIVE_COMPLEXITY_5","COMMENT_LINES_DENSITY_5","COMPLEXITY_5","DIRECTORIES_5","DUPLICATED_BLOCKS_5","DUPLICATED_FILES_5","DUPLICATED_LINES_5","FILES_5","FUNCTIONS_5","NCLOC_5","SQALE_DEBT_RATIO_5","VIOLATIONS_5")]

#Evita notação científica
options(scipen=999)

#Cria gráfico de barras para a quantidade de projetos em cada tópico
pdf(paste(diretorio_trabalho,"projetos_por_topicos.pdf", sep=""),width = 6.7, height = 5 )
par(mar=c(5, 3 ,2 ,1))
bar_topic = barplot(sort(table(DADOS_BRUTOS_PROJETOS$TOPIC),decreasing = TRUE), las=2,xlim=c(0,35),ylim=c(0,300),cex.axis=0.75,cex.names=0.75)
text(x = bar_topic, y = sort(table(DADOS_BRUTOS_PROJETOS$TOPIC),decreasing = TRUE), label = sort(table(DADOS_BRUTOS_PROJETOS$TOPIC),decreasing = TRUE), pos = 3, cex = 0.5, col = "black")
dev.off()

#Cria gráfico de barras para a quantidade de projetos em cada dominio
pdf("C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_estudo_caso/analise_exploratoria/projetos_por_dominio.pdf",width = 6.7, height = 5 )
par(mar=c(6, 3 ,1,1))
bar_domain = barplot(sort(table(DADOS_BRUTOS_PROJETOS$DOMAIN),decreasing = TRUE), las=2,cex.axis=0.75,cex.names=0.75)
text(x = bar_domain, y = sort(table(DADOS_BRUTOS_PROJETOS$DOMAIN),decreasing = TRUE), label = sort(table(DADOS_BRUTOS_PROJETOS$DOMAIN),decreasing = TRUE), pos = 1, cex = 0.8, col = "black")
dev.off()

#Cria boxplot da dívida técnica
pdf("C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_estudo_caso/analise_exploratoria/divida_tecnica_sozinha.pdf",width = 4, height = 4 )
par(mar=c(6, 3 ,2 ,2))
boxplot(DADOS_BRUTOS_PROJETOS$SQALE_DEBT_RATIO_5,outline=FALSE,las=2,cex.axis=0.75,cex.names=0.75)
dev.off()

#Cria boxplot da dívida técnica por topico
pdf("C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_estudo_caso/analise_exploratoria/divida_tecnica_por_topico.pdf",width = 6.7, height = 4 )
par(mar=c(6, 3 ,2 ,2))
boxplot(SQALE_DEBT_RATIO_5~TOPIC,data=DADOS_BRUTOS_PROJETOS,outline=FALSE,las=2,cex.axis=0.75,cex.names=0.75)
dev.off()

#Cria boxplot da dívida técnica por dominio
pdf("C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_estudo_caso/analise_exploratoria/divida_tecnica_por_dominio.pdf",width = 6.7, height = 5 )
par(mar=c(6, 3 ,2 ,2))
boxplot(SQALE_DEBT_RATIO_5~DOMAIN,data=DADOS_BRUTOS_PROJETOS,outline=FALSE,las=2,cex.axis=0.75,cex.names=0.75)
dev.off()

#Cria boxplot da quantidade de linhas de código por dominio
pdf("C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_estudo_caso/analise_exploratoria/linhas_de_codigo_por_dominio.pdf",width = 6.7, height = 4 )
par(mar=c(6, 4 ,2 ,2))
boxplot(NCLOC_5~DOMAIN,data=DADOS_BRUTOS_PROJETOS,outline=FALSE,las=2,cex.axis=0.75,cex.names=0.75)
dev.off()

#Cria boxplot da quantidade de linhas de código por topico
pdf("C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_estudo_caso/analise_exploratoria/linhas_de_codigo_por_topico.pdf",width = 6.7, height = 4 )
par(mar=c(6, 4 ,2 ,2))
boxplot(NCLOC_5~TOPIC,data=DADOS_BRUTOS_PROJETOS,outline=FALSE,las=2,cex.axis=0.75,cex.names=0.75)
dev.off()

#cria um quadro com a evolução das linhas de código por dominio 
pdf("C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_estudo_caso/analise_exploratoria/evolucao_linhas_de_codigo_dominio.pdf",width = 6.7, height = 8 )
points = 1:5
par(mar=c(2, 2 ,2 ,2))
par(mfrow=c(4,2))
for (dominio in unique(DADOS_BRUTOS_PROJETOS$DOMAIN)){
  dominio = DADOS_BRUTOS_PROJETOS[which(DADOS_BRUTOS_PROJETOS$DOMAIN== dominio),]
  dados = c(mean(dominio$NCLOC_1),mean(dominio$NCLOC_2),mean(dominio$NCLOC_3),mean(dominio$NCLOC_4),mean(dominio$NCLOC_5))
  plot(dados~points, main=unique(dominio$DOMAIN), type="b", ylab="NLOC" , xlab="Leitura"  ,pch = 2 )
}
dev.off()



#cria um quadro com a evolução da dívida técnica por dominio 
pdf("C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_estudo_caso/analise_exploratoria/evolucao_divida_dominio.pdf",width = 6.7, height = 8 )
leituras = 1:5
par(mar=c(2, 2 ,2 ,2))
par(mfrow=c(4,2))
for (dominio in unique(DADOS_BRUTOS_PROJETOS$DOMAIN)){
  dominio = DADOS_BRUTOS_PROJETOS[which(DADOS_BRUTOS_PROJETOS$DOMAIN== dominio),]
  dados = c(mean(dominio$SQALE_DEBT_RATIO_1),mean(dominio$SQALE_DEBT_RATIO_2),mean(dominio$SQALE_DEBT_RATIO_3),mean(dominio$SQALE_DEBT_RATIO_4),mean(dominio$SQALE_DEBT_RATIO_5))
  plot(dados~leituras, main=unique(dominio$DOMAIN), type="b", ylab="NLOC" , xlab="Leitura"  ,pch = 2 )
}
par(mfrow=c(1,1))
dev.off()

#cria um quadro com o histograma da dívida técnica por leitura
pdf("C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_estudo_caso/analise_exploratoria/histograma_divida_leitura.pdf",width = 6.7, height = 8 )
par(mar=c(2, 2 ,2 ,2))
par(mfrow=c(3,2))

hist(DADOS_BRUTOS_PROJETOS$SQALE_DEBT_RATIO_1, main="Leitura 1" , ylab =  "Frequência", xlab = "SQALE_DEBT_RATIO",breaks= seq(0,30,0.5))
hist(DADOS_BRUTOS_PROJETOS$SQALE_DEBT_RATIO_2, main="Leitura 2" , ylab =  "Frequência", xlab = "SQALE_DEBT_RATIO",breaks= seq(0,30,0.5))
hist(DADOS_BRUTOS_PROJETOS$SQALE_DEBT_RATIO_3, main="Leitura 3" , ylab =  "Frequência", xlab = "SQALE_DEBT_RATIO",breaks= seq(0,30,0.5))
hist(DADOS_BRUTOS_PROJETOS$SQALE_DEBT_RATIO_4, main="Leitura 4" , ylab =  "Frequência", xlab = "SQALE_DEBT_RATIO",breaks= seq(0,30,0.5))
hist(DADOS_BRUTOS_PROJETOS$SQALE_DEBT_RATIO_5, main="Leitura 5" , ylab =  "Frequência", xlab = "SQALE_DEBT_RATIO",breaks= seq(0,30,0.5))

par(mfrow=c(1,1))
dev.off()


#Cria um histograma de NLOC_5
pdf("C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_estudo_caso/analise_exploratoria/histograma_nloc.pdf",width = 5, height = 4 )
par(mar=c(4, 3 ,2 ,2))
hist(DADOS_BRUTOS_PROJETOS$NCLOC_5, main="" , ylab =  "Frequência", xlab = "", breaks= seq(0,2000000,1000), las=2,cex.axis=0.75 )
dev.off()

#cria um quadro com a correlação entre a dívida técnica e o tamanho do software 
pdf("C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_estudo_caso/analise_exploratoria/correlacao_divida_nloc.pdf",width = 6.7, height = 8 )

par(mar=c(2, 2 ,2 ,2))
par(mfrow=c(4,2))
for (dominio in unique(DADOS_BRUTOS_PROJETOS$DOMAIN)){
  dominio = DADOS_BRUTOS_PROJETOS[which(DADOS_BRUTOS_PROJETOS$DOMAIN== dominio),]
  plot(dominio$NCLOC_5~dominio$SQALE_DEBT_RATIO_5,main=unique(dominio$DOMAIN) )
  abline(lm(NCLOC_5 ~ SQALE_DEBT_RATIO_5, data = dominio), col = "blue")
  
}
par(mfrow=c(1,1))
dev.off()

#cria um quadro com a correlação entre a dívida técnica e a quantidade de pull_requests
pdf("C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_estudo_caso/analise_exploratoria/correlacao_divida_pull_request.pdf",width = 6.7, height = 8 )

par(mar=c(2, 2 ,2 ,2))
par(mfrow=c(4,2))
for (dominio in unique(DADOS_BRUTOS_PROJETOS$DOMAIN)){
  dominio = DADOS_BRUTOS_PROJETOS[which(DADOS_BRUTOS_PROJETOS$DOMAIN== dominio),]
  plot(dominio$PULL_REQUESTS~dominio$SQALE_DEBT_RATIO_5,main=unique(dominio$DOMAIN) )
  abline(lm(PULL_REQUESTS ~ SQALE_DEBT_RATIO_5, data = dominio), col = "blue")
  
}
par(mfrow=c(1,1))
dev.off()


#cria um quadro com a correlação entre a dívida técnica e a quantidade de watchers
pdf("C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_estudo_caso/analise_exploratoria/correlacao_divida_watchers.pdf",width = 6.7, height = 8 )

par(mar=c(2, 2 ,2 ,2))
par(mfrow=c(4,2))
for (dominio in unique(DADOS_BRUTOS_PROJETOS$DOMAIN)){
  dominio = DADOS_BRUTOS_PROJETOS[which(DADOS_BRUTOS_PROJETOS$DOMAIN== dominio),]
  plot(dominio$WATCHERS~dominio$SQALE_DEBT_RATIO_5,main=unique(dominio$DOMAIN) )
  abline(lm(WATCHERS ~ SQALE_DEBT_RATIO_5, data = dominio), col = "blue")
  
}
par(mfrow=c(1,1))
dev.off()


#Cria boxplot da produtividade por dominio
#pdf("C:/Users/jandi/Dropbox/DOUTORADO/TESE/figuras/capitulo_estudo_caso/analise_exploratoria/produtividade_por_topico.pdf",width = 6.7, height = 4 )
#par(mar=c(6, 4 ,2 ,2))
#boxplot(Productiviy~TOPIC,data=DADOS_BRUTOS_PROJETOS,outline=FALSE,las=2,cex.axis=0.75,cex.names=0.75)
#dev.off()





#Calcula o sumário das principais variaveis
summary(DADOS_BRUTOS_PROJETOS[c("WATCHERS","COMMITS","PULL_REQUESTS","NCLOC_5","SQALE_DEBT_RATIO_5")])

