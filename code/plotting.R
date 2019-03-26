setwd("/Users/dws/coding/finance/cdbs")

require(tidyverse)
require(ggrepel)

file_ix = "/Users/dws/coding/finance/cdbs/data" %>% 
  list.files(.,full.names = T) %>% 
  sort(.,decreasing = T) %>%
  grep(pattern = "cdbs_lci") %>%
  .[[1]]

file = "/Users/dws/coding/finance/cdbs/data" %>% 
  list.files(.,full.names = T) %>% 
  sort(.,decreasing = T) %>%
  .[[file_ix]]
  

df_all = read.csv(file,stringsAsFactors = F,sep = ",")

df_all$Prazo = df_all$Prazo %>% 
                as.Date

df_all$Dias = (df_all$Prazo - Sys.Date()) %>% as.numeric

df_all$Meses = round(df_all$Dias/30,0)

df_all$IR = ifelse(df_all$Dias <= 180 ,0.225,
                   ifelse(
                     df_all$Dias >= 181 & df_all$Dias <= 360,0.2 ,
                     ifelse(
                       df_all$Dias >= 361 & df_all$Dias <= 720,0.175,0.15
                     )
                   )
                   )

df_all$IR = ifelse(df_all$Modalidade == "LCI",0,df_all$IR)

IPCA_target = 0.04

df_all$RentEff = ifelse(df_all$Tipo != "IPCA", df_all$Rentabilidade*(1-df_all$IR), (df_all$Rentabilidade+IPCA_target)*(1-df_all$IR))


####################################
# analyzing CDI
####################################
df = df_all %>%
        filter(Tipo == "CDI")

fit = loess(RentEff ~ Dias, data=df)

df$Pred = predict(fit)
df$Delta = df$RentEff/df$Pred - 1
df$OrderDelta = -df$Delta %>% rank

Rank = 10

df$OrderDelta = ifelse(df$OrderDelta<=Rank,df$OrderDelta,NA)

# Scatter plot
sp<-ggplot(df,aes(x=Dias, y=RentEff)) +
  geom_text_repel(
    nudge_y      = 0.005
    ,    nudge_x      = 0.0
    ,direction    = "both"
    ,segment.size = 0.2
    ,force=10
    ,size=4
    ,aes(color=Delta,label = OrderDelta)
    ) + 
  geom_point(aes(color=Delta)) + 
  theme_bw() +
  scale_color_gradient2(midpoint=0L, low="red", mid="gray",high="blue", space ="Lab" ) +
  geom_smooth(formula = y~x,span=0.75,colour = "gray") +
  labs(title="CDB/LCI Pos Fixados",x ="Dias", y = "Rentabilidade efetiva") +
  theme(
    plot.title = element_text(size=16, face="bold"),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10)
  )

sp

ggsave("bot/plot_cdi.png",scale=.55,width = 25, height = 18, units = "cm")



df_msg = df %>% 
  arrange(desc(Delta)) %>%
  .[1:Rank,c(1,5,3,6,4,9,8)]

df_msg$RendeBruto = (df_msg$Rentabilidade*100) %>%
  paste(.,"%",sep="")
df_msg$RendeLiquido = (df_msg$RentEff*100) %>%
  paste(.,"%",sep="")
df_msg$IR = (df_msg$IR*100) %>%
  paste(.,"%",sep="")

df_msg$Nome = paste(df_msg$Modalidade,df_msg$Emissor,df_msg$RendeBruto)


names(df_msg)[3] = "Vencimento"
names(df_msg)[4] = "PrazoDias"

df_msg = df_msg[,c(10,9,3,4,7)]

msg = c()
for(i in 1:dim(df_msg)[1]){
  msg = c(msg,paste0("*Posicao ",i,"*"))
  for(j in 1:dim(df_msg)[2]){
    msg = c(msg,(paste0("_",names(df_msg)[j],"_: ",df_msg[i,j])))
  }
  msg = c(msg," ")
}

saveRDS(msg,file="bot/msg_cdi.rds")
saveRDS(Sys.time(),file="bot/timestamp_cdi.rds")

####################################
# analyzing Pre-Fixado
####################################

df = df_all %>%
  filter(Tipo == "Pre-Fixado")

if(nrow(df)>0){
  fit = loess(RentEff ~ Dias, data=df)
  
  df$Pred = predict(fit)
  df$Delta = df$RentEff/df$Pred - 1
  df$OrderDelta = -df$Delta %>% rank
  
  Rank = 10
  
  df$OrderDelta = ifelse(df$OrderDelta<=Rank,df$OrderDelta,NA)
  
  # Scatter plot
  sp<-ggplot(df,aes(x=Dias, y=RentEff)) +
    geom_text_repel(
      nudge_y      = 0.005
      ,    nudge_x      = 0.0
      ,direction    = "both"
      ,segment.size = 0.2
      ,force=10
      ,size=4
      ,aes(color=Delta,label = OrderDelta)
    ) + 
    geom_point(aes(color=Delta)) + 
    theme_bw() +
    scale_color_gradient2(midpoint=0L, low="red", mid="gray",high="blue", space ="Lab" ) +
    geom_smooth(formula = y~x,span=0.75,colour = "gray") +
    labs(title="CDB/LCI Pre Fixados",x ="Dias", y = "Rentabilidade efetiva") +
    theme(
      plot.title = element_text(size=16, face="bold"),
      axis.title.x = element_text(size=10),
      axis.title.y = element_text(size=10)
    )
  
  sp
  
  ggsave("bot/plot_pre.png",scale=.55,width = 25, height = 18, units = "cm")
  
  df_msg = df %>% 
    arrange(desc(Delta)) %>%
    .[1:Rank,c(1,5,3,6,4,9,8)]
  
  df_msg$RendeBruto = (df_msg$Rentabilidade*100) %>%
    paste(.,"%",sep="")
  df_msg$RendeLiquido = (df_msg$RentEff*100) %>%
    paste(.,"%",sep="")
  df_msg$IR = (df_msg$IR*100) %>%
    paste(.,"%",sep="")
  
  df_msg$Nome = paste(df_msg$Modalidade,df_msg$Emissor,df_msg$RendeBruto)
  
  
  names(df_msg)[3] = "Vencimento"
  names(df_msg)[4] = "PrazoDias"
  
  df_msg = df_msg[,c(10,9,3,4,7)]
  
  msg = c()
  for(i in 1:dim(df_msg)[1]){
    msg = c(msg,paste0("*Posicao ",i,"*"))
    for(j in 1:dim(df_msg)[2]){
      msg = c(msg,(paste0("_",names(df_msg)[j],"_: ",df_msg[i,j])))
    }
    msg = c(msg," ")
  }
  
  saveRDS(msg,file="bot/msg_pre.rds")
  saveRDS(Sys.time(),file="bot/timestamp_pre.rds")
}

####################################
# analyzing IPCA
####################################

df = df_all %>%
  filter(Tipo == "IPCA")

if(nrow(df)>0){
  fit = loess(Rentabilidade ~ Dias, data=df)
  
  df$Pred = predict(fit)
  df$Delta = df$Rentabilidade/df$Pred - 1
  df$OrderDelta = -df$Delta %>% rank
  
  Rank = 10
  
  df$OrderDelta = ifelse(df$OrderDelta<=Rank,df$OrderDelta,NA)
  
  # Scatter plot
  sp<-ggplot(df,aes(x=Dias, y=RentEff)) +
    geom_text_repel(
      nudge_y      = 0.005
      ,    nudge_x      = 0.0
      ,direction    = "both"
      ,segment.size = 0.2
      ,force=10
      ,size=4
      ,aes(color=Delta,label = OrderDelta)
    ) + 
    geom_point(aes(color=Delta)) + 
    theme_bw() +
    scale_color_gradient2(midpoint=0L, low="red", mid="gray",high="blue", space ="Lab" ) +
    geom_smooth(formula = y~x,span=0.75,colour = "gray") +
    labs(title="CDB/LCI IPCA",x ="Dias", y = "Rentabilidade efetiva") +
    theme(
      plot.title = element_text(size=16, face="bold"),
      axis.title.x = element_text(size=10),
      axis.title.y = element_text(size=10)
    )
  sp
  
  ggsave("bot/plot_ipca.png",scale=.55,width = 25, height = 18, units = "cm")
  
  df_msg = df %>% 
    arrange(desc(Delta)) %>%
    .[1:Rank,c(1,5,3,6,4,9,8)]
  
  df_msg$RendeBruto = (df_msg$Rentabilidade*100) %>%
    round(.,2) %>%
    paste(.,"%",sep="")
  df_msg$RendeLiquido = (df_msg$RentEff*100) %>%
    round(.,2) %>%
    paste(.,"%",sep="")
  df_msg$IR = (df_msg$IR*100) %>%
    paste(.,"%",sep="")
  
  df_msg$Nome = paste(df_msg$Modalidade,df_msg$Emissor,"IPCA +",df_msg$RendeBruto)
  
  
  names(df_msg)[3] = "Vencimento"
  names(df_msg)[4] = "PrazoDias"
  
  df_msg = df_msg[,c(10,9,3,4,7)]
  
  msg = c()
  for(i in 1:dim(df_msg)[1]){
    msg = c(msg,paste0("*Posicao ",i,"*"))
    for(j in 1:dim(df_msg)[2]){
      msg = c(msg,(paste0("_",names(df_msg)[j],"_: ",df_msg[i,j])))
    }
    msg = c(msg," ")
  }
  
  saveRDS(msg,file="bot/msg_ipca.rds")
  saveRDS(Sys.time(),file="bot/timestamp_ipca.rds")
  
}

##############