rm(list=ls())
gc()

file = "cdbs_lci_2019_02_28_00_20_54"

df_all = read.csv(paste0("data/",file,".csv"),stringsAsFactors = F,sep = ",")

df_all = df_all %>%
            filter(Tipo != "IPCA")

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

df_all$RentEff = df_all$Rentabilidade*(1-df_all$IR)

# analyzing CDI
df = df_all %>%
        filter(Tipo == "CDI")

fit = loess(RentEff ~ Dias, data=df)

df$Pred = predict(fit)
df$Delta = df$RentEff/df$Pred - 1

# Scatter plot
sp<-ggplot(df,aes(x=Dias, y=RentEff)) +
  geom_point(size=2,aes(color=Delta,text = paste(Modalidade
                                                 ,Emissor
                                                 , paste0(Rentabilidade*100
                                                 ,"% ")
                                                 ,"<br>Vencimento:"
                                                 , Prazo
                                                 ,"<br>Meses:"
                                                 ,Meses
                                                 ,"<br>Taxa efetiva"
                                                 ,paste0(round(RentEff*100,1)
                                                         ,"%")
                                                 ,"<br>Delta"
                                                 ,paste0(round(Delta*100,1)
                                                         ,"%")
                                                 ),shape=Modalidade)) + 
  theme_bw() +
  scale_color_gradient2(midpoint=0L, low="red", mid="gray",high="blue", space ="Lab" ) +
  geom_smooth(formula = y~x,span=0.75,colour = "gray")

sp


library(plotly)

p = sp %>%
  ggplotly(tooltip = "text")

setwd("plots/")
htmlwidgets::saveWidget(as_widget(p), file = paste0("plot_cdi_",file,".html"))
