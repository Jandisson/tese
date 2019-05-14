#install.packages("sqldf")
#install.packages("xlsx")
library(sqldf)
library(xlsx)

debt_percentile = .05;


remove_outliers <- function(x){
  quantiles <- quantile( x, c(.05, .95 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

DADOS_BRUTOS_PROJETOS$Productivity_authors = remove_outliers(DADOS_BRUTOS_PROJETOS$Productivity_authors)
DADOS_BRUTOS_PROJETOS$Productivity_collaboration = remove_outliers(DADOS_BRUTOS_PROJETOS$Productivity_collaboration)
DADOS_BRUTOS_PROJETOS$Productivity_days = remove_outliers(DADOS_BRUTOS_PROJETOS$Productivity_days)




#General SQALE 
debt_threshold = quantile(DADOS_BRUTOS_PROJETOS$AVERAGE_DEBT_SQALE, c(debt_percentile));
DADOS_BRUTOS_PROJETOS = within(DADOS_BRUTOS_PROJETOS,{DEBT_RATE_GENERAL<- ifelse(AVERAGE_DEBT_SQALE <= debt_threshold,'LOW','NORMAL') })  ;
sqldf("select DEBT_RATE_GENERAL, avg(Productivity_authors),avg(Productivity_collaboration),avg(Productivity_days)  from DADOS_BRUTOS_PROJETOS group by DEBT_RATE_GENERAL")



DADOS_BRUTOS_PROJETOS$DEBT_RATE_DOMAIN= NA
DADOS_BRUTOS_PROJETOS$DEBT_DOMAIN_THRESHOLD= NA
DADOS_BRUTOS_PROJETOS$DEBT_RATE_TOPIC= NA
DADOS_BRUTOS_PROJETOS$DEBT_TOPIC_THRESHOLD= NA
  
#Domain SQALE 

dataframe_resultado = DADOS_BRUTOS_PROJETOS[0,];
for (dominio in unique(DADOS_BRUTOS_PROJETOS$DOMAIN)){
  dominio = DADOS_BRUTOS_PROJETOS[which(DADOS_BRUTOS_PROJETOS$DOMAIN== dominio),]
  debt_threshold = quantile(dominio$AVERAGE_DEBT_SQALE, c(debt_percentile));
  dominio$DEBT_DOMAIN_THRESHOLD=debt_threshold
  dominio = within(dominio,{DEBT_RATE_DOMAIN<- ifelse(AVERAGE_DEBT_SQALE <= debt_threshold,'LOW','NORMAL') })  ;
  dataframe_resultado= rbind(dataframe_resultado,dominio)
}

DADOS_BRUTOS_PROJETOS = dataframe_resultado


juros_dominio = sqldf("select DOMAIN,DEBT_RATE_DOMAIN, avg(Productivity_authors),avg(Productivity_collaboration),avg(Productivity_days)  from DADOS_BRUTOS_PROJETOS group by DEBT_RATE_DOMAIN,DOMAIN")


#Topic SQALE 


dataframe_resultado = DADOS_BRUTOS_PROJETOS[0,];
for (topic in unique(DADOS_BRUTOS_PROJETOS$TOPIC)){
  topic = DADOS_BRUTOS_PROJETOS[which(DADOS_BRUTOS_PROJETOS$TOPIC== topic),]
  debt_threshold = quantile(topic$AVERAGE_DEBT_SQALE, c(debt_percentile));
  topic$DEBT_TOPIC_THRESHOLD=debt_threshold
  topic = within(topic,{DEBT_RATE_TOPIC<- ifelse(AVERAGE_DEBT_SQALE <= debt_threshold,'LOW','NORMAL') })  ;
  dataframe_resultado= rbind(dataframe_resultado,topic)
}

DADOS_BRUTOS_PROJETOS = dataframe_resultado
juros_topico = sqldf("select DOMAIN,TOPIC,DEBT_RATE_TOPIC, avg(Productivity_authors),avg(Productivity_collaboration),avg(Productivity_days)  from DADOS_BRUTOS_PROJETOS group by DOMAIN,TOPIC,DEBT_RATE_TOPIC")

write.xlsx(DADOS_BRUTOS_PROJETOS, "C:/Users/jandi/Dropbox/DOUTORADO/DADOS_EXPERIMENTO/dadosProcessados.xlsx")
write.xlsx(juros_dominio, "C:/Users/jandi/Dropbox/DOUTORADO/DADOS_EXPERIMENTO/jurosDominio.xlsx")
write.xlsx(juros_topico, "C:/Users/jandi/Dropbox/DOUTORADO/DADOS_EXPERIMENTO/jurosTopico.xlsx")



