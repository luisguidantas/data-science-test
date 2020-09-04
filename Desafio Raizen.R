##########################################################################################
##                            DATA SCIENCE CHALLENGE RAIZEN                             ##
##                                                                                      ##
## Autor: Luis Guilherme de O.Dantas                                                    ##
## Data: 2020-09-03                                                                     ##
##                                                                                      ##      
##########################################################################################

# mapeamento do diretorio das bases
  setwd("C:/Users/luis.dantas/OneDrive - emDia/Área de Trabalho/MBA/Raizen")

# pacotes utilizados
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(tidyr)
  library(zoo)
  library(forecast)
  library(Metrics)
  library(quantmod)
  library(randomForest)
  library(nnet)
  library(rnn)
  library(fastDummies)
  options(scipen=999)

# leitura e consolidacao das bases de dados. 
  # Devido a limitacoes de espaço e processamento, considerarei as bases com o historico dos ultimos tres
  # anos para realizar este estudo. Caso os modelos de previsao nao estejam adequados considerando apenas
  # este intervalo, incomporarei demais bases.

  referencias = 2015:2017
  dados = NULL
  for(i in referencias){
    dados = as_tibble(rbind(dados,data.table::fread(paste0(i,".csv.gz"))))
    print(i)
  }

# Analise Exploratoria dos Dados  

  # Limpeza e Tratamento dos dados
  
    # Neste estudo estamos interessados em analisar e predizer o numero de pessoas que utilizam
    # o metro de Nova York. Sendo assim, pela descricao dos campos no dicionario dos dados e pelas 
    # visoes que sero analisados,algumas colunas serao desconsideradas, reduzindo o tamanho da base 
    # de estudo
    dados = dados %>% select(-division,-linename)
  
    # Para criar uma chave de identifacao das catracas, unificarei alguns campos.
    # Serao consideradas as colunas de area de controle (CA), unidade remota (unit) 
    # e scp (localização da catraca). 
    dados = dados %>% mutate(id=paste0(ca,unit,scp)) %>%
      select(-ca,-unit,-scp)
    
    # conversao do campo time em datetime
    dados$time2 = ymd_hms(dados$time)
    
    # total de catracas consideradas
    length(unique(dados$id))
    
    # total de estacoes consideradas
    length(unique(dados$station))
  
    # verificar se não existem duplicidades para esta visão que foi construida 
    # Para isso agruparemos a base em id_catraca e horario e contaremos dentro de cada grupo o total de observaçoes.
    # Em teoria não deve haver uma mesma catraca em um mesmo horario com mais de um registro
    duplicidades = dados %>% group_by(time,id) %>% count() %>% filter(n>1)
    
    # verificando um caso de duplicidade para entendimento
    caso = dados %>% filter(id=="R249R17901-00-06",time%in%c("2015-01-14 16:00:00",
                                                             "2015-01-14 12:00:00",
                                                             "2015-01-14 20:00:00"))
    
    # Analisando o caso acima vemos que ha mais de um registro no mesmo horario, referente a 
    # correcao realizada pela auditoria, que de acordo com a descricao dos dados consiste de uma
    # recuperacao da informacao, coluna desc="RECOVR AUD". Sendo assim, para qualquer catraca que 
    # tenha desc="RECOVR AUD" em um mesmo horário iremos utilizar as informações de entrada e saida 
    # da situacao "REGULAR". No caso acima podemos ver que o registro da linha desc="RECOVR AUD" esta
    # muito distante da quantidade acumulada de entradas que estava sendo observada e dos horarios subsequentes
    # Para este ajuste faremos uma ordenação dos dados e manter as linhas desejadas
    dados = dados %>% mutate(flag_recuperado=ifelse(desc=="REGULAR",1,0)) %>%
        arrange(id,time,desc(flag_recuperado)) %>% group_by(id,time) %>%
        mutate(rank=row_number()) %>% filter(rank==1) %>% 
        select(-rank,-desc,-flag_recuperado)
    
    # Como os dados de entrada e saida sao valores acumulados das pessoas que passaram pelas
    # catracas ate o horario especificado, construirei duas colunas que trarão o numero absoluto
    # de pessoas que entraram ate aquele horario, aplicando uma diferenca da observacao atual com a 
    # anterior. Alem disso, os dados deverao estar ordenados por catraca e horario para aplicar
    # as diferencas corretamente
    dados = dados %>% ungroup() %>% arrange(id,station,time) %>% 
      group_by(id,station) %>%
      mutate(entradas=entries-lag(entries),
             saidas=exits-lag(exits))
    
    # Identificar possiveis erros/anomalias nos dados, como quantidades de entradas ou saidas menores
    # do que zero
    erros = dados %>% filter(entradas<0|saidas<0)
    
    # No caso abaixo, ao olhar na base de dados vemos que repentinamente o numero acumulado de entradas
    # sofre uma quedra brusca e começa a crescer no mesmo patamar. Possivelmente a catraca do local pode 
    # ter sido "resetada" e por isso ocorre essa queda para um novo cenario.
    caso2 = dados %>% filter(id=="A010R08000-00-00",time%in%c("2015-02-25 08:00:00",
                                                             "2015-02-25 12:00:00",
                                                             "2015-02-25 20:00:00",
                                                             "2015-02-26 00:00:00"))
    
    # No caso abaixo, ao olhar na base de dados vi que o numero acumulado ao inves de aumentar sempre
    # diminui de uma linha para outra. Possivelmente os registros dessas catracas sao contabilizados ao 
    # contrario, pois existem outras catracas em que esse comportamento ocorre
    caso3 = dados %>% filter(id=="A011R08001-00-00",time%in%c("2015-02-24 12:00:00",
                                                              "2015-02-24 12:43:17",
                                                              "2015-02-24 16:00:00",
                                                              "2015-02-24 20:00:00"))
    
    # Para os casos outliers dentro do histórico de analisado, criarei neste primeiro momento uma
    # marcação destas observacoes para serem tratadas pois provavelmente sao decorrentes de alguma mudanca/reset
    # das catracas ou erro dos dados e se mantermos desta forma podemos viesar as analises futuras.
    # O criterio de marcacao de outiler serao as observacoes que estao acima/abaixo de 5 vezes o quantil
    # 99,5%. As primeiras entradas e saidas do dia que estao com NA, colocarei o valor 0.
    dados = dados %>% ungroup() %>%
      mutate(flag_out_ent=ifelse(abs(entradas)>=5*(abs(quantile(entradas,0.995,na.rm=T))),1,0),
             flag_out_saida=ifelse(abs(saidas)>=5*(abs(quantile(saidas,0.995,na.rm = T))),1,0)) %>%
      mutate(entradas=case_when(is.na(entradas) ~ 0,
                                flag_out_ent==1 ~ NA_real_,
                                TRUE ~ entradas),
             saidas=case_when(is.na(saidas) ~ 0,
                              flag_out_saida==1 ~ NA_real_,
                              TRUE ~ saidas)) 
    
    # Feitas as marcações verei quantas catracas que apresentam valores negativos e a quantidade de 
    # registros em cada uma delas
    vl_negativos = dados %>% filter(entradas<0|saidas<0) %>%
      group_by(id) %>% count() %>% arrange(desc(n))
    
    # Consultando os casos com mais de 100 valores negativos, observei que os valores acumulados das entradas
    # vao decrescendo ao longo de todo o historico, ao inves de aumentar. Uma possivel conjectura, conforme
    # levantado anteriormente seria que estas catracas estao registrando as entradas ao contrario, e neste caso, assumindo este erro,
    # seria razoavel deixar as quantidades positivas ao inves de negativas. Abaixo mais dois exemplos para ilustrar
    # este cenario
    dados %>% filter(id=="N508R45300-00-02")
    dados %>% filter(id=="N103R12700-06-00")
    
    # Para as catracas que possuem menos que 10 valores negativos considerarei como um erro/ajuste de medicao e
    # tratarei como NA estas observacoes negativas da base. Para os casos exemplificados acima deixarei 
    # os valores positivos
    dados = dados %>% ungroup() %>% 
      mutate(entradas=case_when(entradas<0 & 
                               id%in%vl_negativos$id[vl_negativos$n>=10] ~ abs(entradas),
                               entradas<0 & 
                                 id%in%vl_negativos$id[vl_negativos$n<10] ~ NA_real_,
                                TRUE ~ entradas),
             saidas=case_when(saidas<0 & 
                                id%in%vl_negativos$id[vl_negativos$n>=10] ~ abs(saidas),
                              saidas<0 & 
                                id%in%vl_negativos$id[vl_negativos$n<10] ~ NA_real_,
                              TRUE ~ saidas)) %>%
      select(-entries,-exits)
    
    # Como a maioria dos horarios sao 00,04,08,12,16,20 (quatro em quatro horas) sumarizarei os demais
    # horarios dentro destes mesmos intervalos
    dados = dados %>% ungroup() %>%
      mutate(time3=case_when(hour(time2)<=0 ~ paste0(substr(time,1,10)," 00:00:00"),
                             hour(time2)<=4 ~ paste0(substr(time,1,10)," 04:00:00"),
                             hour(time2)<=8 ~ paste0(substr(time,1,10)," 08:00:00"),
                             hour(time2)<=12 ~ paste0(substr(time,1,10)," 12:00:00"),
                             hour(time2)<=16 ~ paste0(substr(time,1,10)," 16:00:00"),
                             hour(time2)<=20 ~ paste0(substr(time,1,10)," 20:00:00"),
                             hour(time2)<=23 ~ paste0(substr(time,1,10)," 00:00:00"))) %>%
      select(-time)
    dados$time3 = ymd_hms(dados$time3)
    
    # Para os casos entre 21h e meia noite precisa ajustar para que o rotulo da data seja no dia seguinte
    # as 0hrs
    dados = dados %>% mutate(time=case_when(hour(time2)>20 & hour(time2)<=23 ~ time3+days(1),
                                              TRUE ~ time3)) 
    
    # Remover duplicidades para o agrupamento de horario realizado acima
    dados = dados %>% ungroup() %>% group_by(id,time,station) %>%
      summarise(entradas=sum(entradas),
                saidas=sum(saidas),
                flag_out_ent=max(flag_out_ent),
                flag_out_saida=max(flag_out_saida)) %>% ungroup() 
    
    # Abaixo crio uma matriz com todas as combinacoes de horarios (de 4 em 4h) e catracas possiveis. A data 
    # maxima dos dados considerados sera na maior data disponivel 2017-09-23.
    # No final deste processo fazemos um join para trazer os nomes das estacoes de cada catraca.
    horarios = seq(min(dados$time),by=60*60*4,to=ymd_hms("2017-09-23 00:00:00"))
    aux=data.frame(time=rep(horarios,length(unique(dados$id))),
              id=sort(rep(unique(dados$id),length(horarios)))) %>%          
    mutate_at(vars(id),as.character) %>% 
    inner_join(.,dados%>%distinct(id,station),by="id")
  
    # Tabela consolidada de horarios, sem duplicidades
    dados = left_join(aux,dados,by=c("id","time","station")) 
    rm(aux)
    
  # Interpolacao para os dados outliers, erros e que nao temos informacao em todos os horarios considerados
    
    # Calcular nova soma acumulada de entradas e saidas, considerando os dados ajustados.
    # Para os extremos que estiverem com NA colocaremos os menores valores de entrada e saida acumulada
    # no inicio e os maiores valores de entrada e saida acumulada no final, para que seja possivel calcular todas as 
    # interpolacoes
    dados = dados %>% group_by(id) %>%
      mutate(entrada_acum=cumsum(ifelse(is.na(entradas),0,entradas)),
             saida_acum=cumsum(ifelse(is.na(saidas),0,saidas))) %>%
      mutate(entrada_acum=case_when(is.na(entradas) & 
                                      time==min(time,na.rm = T) ~ min(entrada_acum,na.rm = T),
                                    is.na(entradas) & 
                                      time==max(time,na.rm = T) ~ max(entrada_acum,na.rm = T),
                                    is.na(entradas) ~ NA_real_,
                                    TRUE ~ entrada_acum),
             saida_acum=case_when(is.na(saidas) & 
                                    time==min(time,na.rm = T) ~ min(saida_acum,na.rm = T),
                                  is.na(saidas) & 
                                    time==max(time,na.rm = T) ~ max(saida_acum,na.rm = T),
                                  is.na(saidas) ~ NA_real_,
                                  TRUE ~ saida_acum))
    
    # calcular interpolacoes para os dados faltantes
    dados = dados %>% group_by(id) %>%
      mutate(saida_acum=round(na.approx(saida_acum,na.rm = F)),
             entrada_acum=round(na.approx(entrada_acum,na.rm = F))) %>%
      select(-flag_out_ent,-flag_out_saida)
    
    # Verificando se ainda existem NAs na base
    sum(is.na(dados$entrada_acum))
    sum(is.na(dados$saida_acum))
    
  # Transformacoes finais nos dados apos tratamento e correcoes
    
    # Calculo das entradas e saidas absolutas em cada horario (subtraindo os valores acumulados) e
    # remocao de colunas que nao serao mais utilizadas
    dados = dados %>% ungroup() %>% arrange(id,station,time) %>% 
      group_by(id,station) %>%
      mutate(entradas=entrada_acum-lag(entrada_acum),
             saidas=saida_acum-lag(saida_acum)) %>%
      select(-entrada_acum,saida_acum)
    
    # Para quantificar o trafego de pessoas pelas catracas (fluxo total), farei a soma das entradas e saidas
    dados$trafego = dados$entradas+dados$saidas
    
  
  # Visualizacoes e Sumarizacoes
    
    # estacoes com maior fluxo de pessoas
    fluxo_estacao = dados %>% group_by(station) %>% summarise(Entrada=sum(entradas,na.rm = T),
                                                               Saida=sum(saidas,na.rm = T),
                                                               Fluxo=sum(trafego,na.rm = T)) %>%
      arrange(desc(Fluxo)) %>% gather("Sentido","Total",2:3) 
    
    # resumos do fluxo total de pessoas
    summary(fluxo_estacao$Fluxo)
    
    # grafico de barras das 15 estacoes com o maior numero de pessoas
    ggplot(fluxo_estacao[fluxo_estacao$station%in%fluxo_estacao$station[1:15],],
           aes(x=reorder(station,Fluxo),y=Total,fill=Sentido)) +
      geom_bar(stat = "identity",color="black") + 
      scale_fill_manual(values=c("#800080", "#E69F00"))+
      coord_flip() +
      scale_y_continuous(breaks=seq(0,400000000,50000000))+
      labs(title="Top 15 Estacoes com Maior Fluxo",x="Estacao",y="Fluxo")  
    
    # grafico de barras das 15 estacoes com o menor numero de pessoas
    ggplot(fluxo_estacao[fluxo_estacao$station%in%
                        fluxo_estacao$station[(nrow(fluxo_estacao)-15):nrow(fluxo_estacao)],],
           aes(x=reorder(station,Fluxo),y=Total,fill=Sentido)) +
      geom_bar(stat = "identity",color="black") + 
      scale_fill_manual(values=c("#800080", "#E69F00"))+
      coord_flip() +
      labs(title="Top 15 Estacoes com Maior Fluxo",x="Estacao",y="Fluxo")  
    
    # catracas com maior fluxo de pessoas
    fluxo_catraca = dados %>% group_by(id) %>% 
      summarise(Entrada=sum(entradas,na.rm = T),
                Saida=sum(saidas,na.rm = T),
                Fluxo=sum(trafego,na.rm = T)) %>%
      arrange(desc(Fluxo)) %>% gather("Sentido","Total",2:3) 
    
    # resumos do fluxo total de pessoas
    summary(fluxo_catraca$Fluxo)
    
    # grafico de barras das 15 estacoes com o maior numero de pessoas
    ggplot(fluxo_catraca[fluxo_catraca$id%in%fluxo_catraca$id[1:15],],
           aes(x=reorder(id,Fluxo),y=Total,fill=Sentido)) +
      geom_bar(stat = "identity",color="black") + 
      scale_fill_manual(values=c("#800080", "#E69F00"))+
      coord_flip() +
      labs(title="Top 15 Catracas com Maior Fluxo",x="catraca",y="Fluxo")  
    
    # grafico de barras das 15 estacoes com o menor numero de pessoas
    ggplot(fluxo_estacao[fluxo_estacao$station%in%
                           fluxo_estacao$station[(nrow(fluxo_estacao)-15):nrow(fluxo_estacao)],],
           aes(x=reorder(station,Fluxo),y=Total,fill=Sentido)) +
      geom_bar(stat = "identity",color="black") + 
      scale_fill_manual(values=c("#800080", "#E69F00"))+
      coord_flip() +
      scale_y_continuous(breaks=seq(0,100000,5000))+
      labs(title="Top 15 Estacoes com Maior Fluxo",x="Estacao",y="Fluxo")
    
    # Total de entradas por dia/mes/semana ao longo dos anos
    
      # Grafico de Total de Entradas por ano ao longo dos meses
      mes = dados %>% group_by(month(time),year(time)) %>% 
        summarise(Entradas=sum(entradas,na.rm = T)) %>%
        rename(Mes=`month(time)`,Ano=`year(time)`) %>% ungroup() %>%
        filter(!(Ano==2017&Mes==9)) %>% mutate_at(vars(Ano),as.character)
      
      ggplot(mes, aes(x=Mes, y=Entradas, group=Ano)) +
        geom_line(aes(color=Ano),size=1) +
        scale_color_brewer(palette="Dark2") +
        scale_x_discrete(limits=as.character(seq(1,12,1)))+
        labs(title="Total de Entradas ao longo dos meses por ano",x="Mes",y="Entradas")+
        theme(legend.position = "bottom")
      
      # Grafico de Total de Entradas por ano ao longo das semanas 
      semana = dados %>% 
        group_by(week(time),year(time)) %>% 
        summarise(Entradas=sum(entradas,na.rm = T)) %>%
        rename(Semana=`week(time)`,Ano=`year(time)`) %>% ungroup() %>%
        mutate_at(vars(Ano),as.character)
      
      ggplot(semana, aes(x=Semana, y=Entradas, group=Ano)) +
        geom_line(aes(color=Ano),size=1) +
        scale_color_brewer(palette="Dark2") +
        labs(title="Total de Entradas ao longo das semanas por ano",x="Semana",y="Entradas")+
        theme(legend.position = "bottom")
      
      # Grafico de Total de Entradas por ano ao longo dos dias da semana.
      # Filtrarei ate o mes de setembro pois eh o mes maximo completo disponivel no ano de 2017
      dia_semana = dados %>% filter(month(time)<=8) %>%
        group_by(weekdays(time),year(time)) %>% 
        summarise(Entradas=sum(entradas,na.rm = T)) %>%
        rename(Dia_Semana=`weekdays(time)`,Ano=`year(time)`) %>% ungroup() %>%
        mutate_at(vars(Ano),as.character)
      
      ggplot(dia_semana, aes(x=Dia_Semana, y=Entradas, group=Ano)) +
        geom_line(aes(color=Ano),size=1) +
        scale_color_brewer(palette="Dark2") +
        scale_x_discrete(limits=c("segunda-feira","terça-feira","quarta-feira","quinta-feira",
                                  "sexta-feira","sábado","domingo"))+
        labs(title="Total de Entradas ao longo dos dias da semanas por ano",x="Dia Semana",y="Entradas")+
        theme(legend.position = "bottom")
      
      # Total de entradas por ano ao longo dos dias
      dias = dados %>% filter(!(month(time)>=9 & day(time)>=23)) %>%
               mutate(dia=substr(time,6,10)) %>%
               group_by(dia,year(time)) %>% summarise(Entradas=sum(entradas)) %>%
        ungroup() %>%
        rename(Ano=`year(time)`) %>% mutate_at(vars(Ano),as.character)
      
      ggplot(dias, aes(x=dia, y=Entradas, group=Ano)) +
        geom_line(aes(color=Ano),size=1) +
        scale_color_brewer(palette="Dark2") +
        labs(title="Total de Entradas ao longo dos dias por ano",x="Dias",y="Entradas")+
        theme(legend.position = "bottom")
      
      # Total de entradas por ano ao longo dos horarios
      hora = dados %>% filter(!(month(time)>=9 & day(time)>=23)) %>%
        group_by(hour(time),year(time)) %>% 
        summarise(Entradas=sum(entradas,na.rm = T)) %>%
        rename(Hora=`hour(time)`,Ano=`year(time)`) %>% ungroup() %>%
        mutate_at(vars(Ano),as.character)
      
      ggplot(hora, aes(x=Hora, y=Entradas, group=Ano)) +
        geom_line(aes(color=Ano),size=1) +
        scale_color_brewer(palette="Dark2") +
        labs(title="Total de Entradas ao longo dos horarios por ano",x="Fx Hora",y="Entradas")+
        theme(legend.position = "bottom")
      

# Modelagem - Series Temporais
      
# Serao feitos dois processos de modelagem, sendo:
# a) um modelo para predizer o numero total de pessoas que utilizam o metro em uma dada faixa de
# horario, considerando os totais de entradas no metro
# b) modelos por estacao para predizer o fluxo de pessoas que trafegam em uma dada faixa de horario,
# considerando o fluxo total de pessoas na estação (entradas + saidas)
  
  # a) Modelo de Entradas: para predizer o numero de pessoas que utilizam o metro em um dado horario
  # considerarei o numero de pessoas que entraram no metro durante o intervalo considerado
  
    # Para construir o modelo de series temporais que seja capaz de estimar o numero de pessoas que
    # utilizam o metro de NY considerarei o seguinte cenario:
    # i) os instantes t da serie serao o dia/horario
    # ii) a variavel resposta para esta serie sera o numero de entradas total 
      
    # base agrupada por dia e horario
      dados2 = dados %>% group_by(time) %>% summarise(Entradas=sum(entradas))
      metro_ts = ts(dados2$Entradas,start = as.Date("2015-01-01"),frequency = 6)
      metro_ts = na.omit(metro_ts)
      
      plot(metro_ts,main="Hitorico de Entradas no metro nos anos de 2015 a 2017")
      
    # Construcao das lags da serie temporal (variaveis defasadas, autorregressivo) para utilizarmos 
    # como inputs na modelagem com redes neurais. Serao criadas 180 lags (ult. 30 dias) + 2 lags para
    # o mesmo horario no ultimo ano e no ultimo semestre (para fins de teste)
      
      metro_mtrx = as.numeric(metro_ts)
      for (i in c(1:180,2160,1080)){
        metro_mtrx = cbind(metro_mtrx,Lag(as.numeric(metro_ts),k=i))
      }
      
      # Algoritmo de Random Forest para estudar a importancia das variaveis e conseguirmos
      # escolher quais lags testaremos para predizer o valor futuro
        metro_mtrx2 = na.omit(as.data.frame(metro_mtrx)) %>%
        rename(Y=metro_mtrx)
        
        # Ajuste modelo random forest, sendo Y o valor presente e as variaveis regressoras as lags
        # Parametros do ajuste: foram simulados alguns modelos e observei que a partir de 100 arvores
        # o erro ja se estabilizava;
        set.seed(1234)
        rf = randomForest(Y~.,data=metro_mtrx2,ntree=100,mtry=13)
        plot(rf)
        
        # Grafico das variaveis com maior importancia: Graficamente observei que as lags 84 (duas semanas atras),
        # 168 (quatro semanas), 42 (uma semena) sao as tres variaveis de maior importancia. Demais lags 
        # que tambem se destacaram serao incorporadas na modelagem logo abaixo
        varImpPlot(rf)
        
        # Variaveis de maior importancia
        importancia = importance(rf) %>% 
          data.frame() %>% 
          mutate(feature = row.names(.)) %>%
          arrange(desc(IncNodePurity)) %>% select(feature)
      
    # normalizacao dos dados: (xi-xmin)/(xmax-xmin)
      normaliza = function(x){ (x-min(x))/(max(x)-min(x))}
      metro_mtrx2_norm = as.data.frame(normaliza(data.matrix(metro_mtrx2)))
      
    # separacao das bases de treino e teste (80% e 20%, respectivamente)
      amostra = 1:round((nrow(metro_mtrx2_norm)*0.8))
      train_mt = metro_mtrx2_norm[amostra,]
      test_mt = metro_mtrx2_norm[-amostra,]
      
  
    # algoritmo nnet
      # matriz de treirno com as lags que serao utilizadas para modelagem
      train_mt2 = na.omit(train_mt[,colnames(train_mt)%in%c("Y","Lag.1","Lag.6","Lag.30",
                                                          "Lag.42","Lag.48","Lag.78","Lag.84",
                                                          "Lag.120","Lag.126","Lag.132","Lag.162","Lag.168",
                                                          "Lag.174","Lag.180")])
      test_mt2 = na.omit(test_mt[,colnames(test_mt)%in%c("Y","Lag.1","Lag.6","Lag.30",
                                                         "Lag.42","Lag.48","Lag.78","Lag.84",
                                                         "Lag.120","Lag.126","Lag.132","Lag.162","Lag.168",
                                                         "Lag.174","Lag.180")])
      
      
      # parametros a serem testados: size = # nos na camada oculta e maxit = # interacoes 
      sizes = 1:13
      maxit = c(seq(100,1000,100),2000)
      
      resultados_nnet = data.frame(Size=NA,Maxit=NA,RMSE_train=NA,RMSE_test=NA)
      
      for(i in sizes){
        for(j in maxit){
          set.seed(1234)
          nnet = NULL
          nnet = nnet(Y~.,data=train_mt2,size=i, decay=5e-4, maxit=j)
          
          resultados_nnet = rbind(resultados_nnet,
                                  c(i,j,round(rmse(train_mt2$Y,nnet$fitted.values),5),
                                    round(rmse(test_mt2$Y,predict(nnet,test_mt2)),5)))
        }
      }
      
      resultados_nnet = resultados_nnet %>% na.omit() %>%
        mutate(diff_train_test=abs(RMSE_train-RMSE_test)) %>%
        arrange(RMSE_test)
      
      # modelo escolhido: Size = 13 e maxit=500 (possui um RMSE de teste baixo e a diferenca com a base de 
      # treino eh bem pequena)
      set.seed(1234)
      nnet_2 = nnet(Y~.,data=train_mt2,size=13, decay=5e-4, maxit=500)
      round(rmse(train_mt2$Y,nnet_2$fitted.values),5)
      round(rmse(test_mt2$Y,predict(nnet_2,test_mt2)),5)
      
      # grafico de residuos
      plot(nnet_2$residuals,main="Residuals")
      abline(h=0,col="red")
      
      #unscale: processo para "desnormalizar" os dados e voltar para a escala padrao
      unscale = function(x,max_x,min_x){
        x*(max_x-min_x)+min_x
      }
      min_dados = min(metro_mtrx2)
      max_dados = max(metro_mtrx2)
      
      # Grafico predito vs real para a base de teste
        # salvar valores preditos (teste_predito) e observados (teste_real)
          teste_predito = unscale(predict(nnet_2,test_mt2),max_dados,min_dados)
          teste_real = unscale(test_mt2$Y,max_dados,min_dados)
          
        # tabela para o grafico
          compara = data.frame(Entradas=c(teste_predito,teste_real),
                               t=rep(1:nrow(teste_predito),2),
                               Grupo=c(rep("Predito",nrow(teste_predito)),
                                       rep("Real",nrow(teste_predito))))
          
        # grafico comparativo
          ggplot(compara,aes(x=t,y = Entradas)) + 
            geom_line(aes(color = Grupo), size = 1) +
            scale_color_manual(values = c("#00AFBB", "#E7B800")) +
            theme_minimal()+
            labs(title="Valores observados vs Predito (Amostra Teste)")
        
      # Previsoes para os proximos 30 dias
      # O ultimo horario disponivel na base foi 2017-09-23 00:00:00. Abaixo, para fins de exemplificacao 
      # calculo as previsoes para o numero de pessoas que entrarao no metro nos proximos  dias
      
        prever = metro_mtrx2_norm[nrow(metro_mtrx2_norm),]
        
        #adicionar primeira linha para a previsao
        prever = rbind(prever,
                       as.numeric(c(NA,metro_mtrx2_norm[nrow(metro_mtrx2_norm),
                                  1:(ncol(metro_mtrx2_norm)-1)])))
        for(i in 2:181){
          prever$Y[i]=predict(nnet_2,prever[i,])
          prever=rbind(prever,as.numeric(c(NA,prever[nrow(prever),1:(ncol(prever)-1)])))
        }
        
        # desnormalizar as previsoes
        previsoes = na.omit(unscale(prever$Y,max_dados,min_dados))
        
        # grafico de previsoes
        previsoes_graf = data.frame(t=c(dados2$time[(nrow(dados2)-100):nrow(dados2)],
                                    seq(max(dados2$time)+60*60*4,by=60*60*4,to=ymd_hms("2017-10-23 00:00:00"))),
                                    Entradas=c(dados2$Entradas[(nrow(dados2)-100):nrow(dados2)],
                                               previsoes[-1]),
                                    Tipo=c(rep("Historico",101),rep("Previsao",180)))
        
        ggplot(previsoes_graf,aes(x=t,y = Entradas)) + 
          geom_line(aes(color = Tipo), size = 1) +
          scale_color_manual(values = c("#00AFBB", "#E7B800")) +
          theme_minimal()+
          labs(title="Projecoes para os proximos 30 dias")
      
      
  # b) Modelo de Fluxo por estacao: para predizer o numero de pessoas que trafegam em cada estacao em um dado horario
  # considerarei a soma entre o numero de pessoas que entraram e sairam durante o intervalo considerado.
  # Abaixo proponho construir uma funcao com os passos da modelagem acima em que seja possivel criar modelos
  # com base na estacao desejada (parametro de entrada). Esta funcao fara automaticamente todo o processo de
  # construcao das bases de treino/teste, selecao de variaveis e construcao da matriz de simulacoes de modelos,
  # para que seja possivel fazer uma analise posterior e escolher os melhores parametros para os modelos finais
  # das estacoes.
  #
  # Paramatro de entrada da funcao: station 
  # Outputs da funcao: results: data.frame com os resultados dos modelos simulados (parametros da rede neural);
  #                    importances: as 15 lags mais importantes para a construcao do modelo;
  #                    train: data.frame de treino dos modelos;
  #                    test: data.frame de teste dos modelos;
  #                    total_norm: dados normalizados;
  #                    min: minimo dos dados considerados (para uso de (des)normalizacao);
  #                    max: maximo dos dados considerados (para uso de (des)normalizacao);
  #                    seed: seed das amostras consideradas nas modelagens 
  #                    dados: dados "brutos" filtrados para a estacao considerada
    
    #funcao para modelar para cada estacao
      nn_station_model = function(station){
        # base agrupada por dia e horario
        dados2 = dados %>% ungroup() %>%  
          filter(station==station) %>%
          group_by(time) %>%
          summarise(Fluxo=sum(trafego))
        
        metro_ts = ts(dados2$Fluxo,start = as.Date("2015-01-01"),frequency = 6)
        metro_ts = na.omit(metro_ts)
        
        plot(metro_ts)
  
        # Construcao das lags da serie temporal (variaveis defasadas, autorregressivo) para utilizarmos 
        # como inputs na modelagem com redes neurais. Serao criadas 180 lags, e como os instantes da serie 
        # sao de 4 em 4horas, correspondera a janela dos ultimos 30 dias
        
          metro_mtrx = as.numeric(metro_ts)
            for (i in c(1:180,2160,1080)){
          metro_mtrx = cbind(metro_mtrx,Lag(as.numeric(metro_ts),k=i))
          }
        
        # Algoritmo de Random Forest para estudar a importancia das variaveis e conseguirmos
        # escolher quais lags testaremos para predizer o valor futuro
          metro_mtrx2 = na.omit(as.data.frame(metro_mtrx)) %>%
            rename(Y=metro_mtrx)
          
        # Ajuste modelo random forest, sendo Y o valor presente e as variaveis regressoras as lags
        # Parametros do ajuste.
          set.seed(1234)
          rf = randomForest(Y~.,data=metro_mtrx2,ntree=100,mtry=13)
          plot(rf)
          
        # Variaveis de maior importancia
          importancia = importance(rf) %>% 
            data.frame() %>% 
            mutate(feature = row.names(.)) %>%
            arrange(desc(IncNodePurity)) %>% select(feature)
          
          # normalizacao dos dados: (xi-xmin)/(xmax-xmin)
          normaliza = function(x){ (x-min(x))/(max(x)-min(x))}
          metro_mtrx2_norm = as.data.frame(normaliza(data.matrix(metro_mtrx2)))
          min_dados = min(metro_mtrx2)
          max_dados = max(metro_mtrx2)
          
          # separacao das bases de treino e teste (80% e 20%, respectivamente)
          amostra = 1:round((nrow(metro_mtrx2_norm)*0.8))
          train_mt = metro_mtrx2_norm[amostra,]
          test_mt = metro_mtrx2_norm[-amostra,]
          
          
          # algoritmo nnet
          # matriz de treirno com as lags que serao utilizadas para modelagem
          train_mt2 = na.omit(train_mt[,colnames(train_mt)%in%c("Y",importancia$feature[1:15])])
          test_mt2 = na.omit(test_mt[,colnames(test_mt)%in%c("Y",importancia$feature[1:15])])
          
          
          # parametros a serem testados: size = # nos na camada oculta e maxit = # interacoes 
          sizes = 1:13
          maxit = c(seq(100,1000,100),2000)
          
          resultados_nnet = data.frame(Size=NA,Maxit=NA,RMSE_train=NA,RMSE_test=NA)
          
          for(i in sizes){
            for(j in maxit){
              set.seed(1234)
              nnet = NULL
              nnet = nnet(Y~.,data=train_mt2,size=i, decay=5e-4, maxit=j)
              
              resultados_nnet = rbind(resultados_nnet,
                                      c(i,j,round(rmse(train_mt2$Y,nnet$fitted.values),5),
                                        round(rmse(test_mt2$Y,predict(nnet,test_mt2)),5)))
            }
          }
          
          resultados_nnet = resultados_nnet %>% na.omit() %>%
            mutate(diff_train_test=abs(RMSE_train-RMSE_test)) %>%
            arrange(RMSE_test)
          
          return(list(results=resultados_nnet,importances=importancia$feature[1:15],
                      train=train_mt2,test=test_mt2,total_norm=metro_mtrx2_norm,
                      min=min_dados,max=max_dados,seed=1234,dados=dados2))
      }

    
    # Supondo que queremos fazer um modelo para a estacao mais movimentada ("34 ST-PENN STA")
    testes = nn_station_model(station="34 ST-PENN STA")  
    
      # Extrair data.frame com resultados de cada modelo
      modelos = testes$results
      
      # Pelos resultados observados, escolherei o modelo com 11 units na camada oculta 
      # e com 500 interacoes, pois possue um dos menores RMSE na amostra teste e está muito proximo
      # com o RMSE da base de treino
      
      set.seed(testes$seed)
      modelo_station = nnet(Y~.,data=testes$train,size=11, decay=5e-4, maxit=500)
  
      # grafico de residuos
      plot(modelo_station$residuals,main="Residuals")
      abline(h=0,col="red")
      
      #unscale: processo para "desnormalizar" os dados e voltar para a escala padrao
      unscale = function(x,max_x,min_x){
        x*(max_x-min_x)+min_x
      }
      min_dados = testes$min
      max_dados = testes$max
      
      # Grafico predito vs real para a base de teste
      # salvar valores preditos (teste_predito) e observados (teste_real)
      teste_predito = unscale(predict(modelo_station,testes$test),max_dados,min_dados)
      teste_real = unscale(testes$test[,1],max_dados,min_dados)
      
      # tabela para o grafico
      compara = data.frame(Fluxo=c(teste_predito,teste_real),
                           t=rep(1:nrow(teste_predito),2),
                           Grupo=c(rep("Predito",nrow(teste_predito)),
                                   rep("Real",nrow(teste_predito))))
      
      # grafico comparativo
      ggplot(compara,aes(x=t,y = Fluxo)) + 
        geom_line(aes(color = Grupo), size = 1) +
        scale_color_manual(values = c("#00AFBB", "#E7B800")) +
        theme_minimal()+
        labs(title="Valores observados vs Predito (Amostra Teste)")
      
    # Previsoes para os proximos 30 dias
      # O ultimo horario disponivel na base foi 2017-09-23 00:00:00. Abaixo, para fins de exemplificacao 
      # calculo as previsoes para o numero de pessoas que trafegaram nesta estacao nos proximos 30 dias
      
      prever = testes$total_norm[nrow(testes$total_norm),]
      
      #adicionar primeira linha para a previsao
      prever = rbind(prever,
                     as.numeric(c(NA,testes$total_norm[nrow(testes$total_norm),
                                                      1:(ncol(testes$total_norm)-1)])))
      for(i in 2:181){
        prever$Y[i]=predict(modelo_station,prever[i,])
        prever=rbind(prever,as.numeric(c(NA,prever[nrow(prever),1:(ncol(prever)-1)])))
      }
      
      # desnormalizar as previsoes
      previsoes = na.omit(unscale(prever$Y,max_dados,min_dados))
      
      # grafico de previsoes
      station = testes$dados
      previsoes_graf = data.frame(t=c(station$time[(nrow(station)-100):nrow(station)],
                                      seq(max(station$time)+60*60*4,by=60*60*4,to=ymd_hms("2017-10-23 00:00:00"))),
                                  Fluxo=c(station$Fluxo[(nrow(station)-100):nrow(station)],
                                             previsoes[-1]),
                                  Tipo=c(rep("Historico",101),rep("Previsao",180)))
      
      ggplot(previsoes_graf,aes(x=t,y = Fluxo)) + 
        geom_line(aes(color = Tipo), size = 1) +
        scale_color_manual(values = c("#00AFBB", "#E7B800")) +
        theme_minimal()+
        labs(title="Projecoes para os proximos 30 dias")
    
    
    