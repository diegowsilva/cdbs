rm(list=ls())
gc()

require(tidyverse)

df = read.csv("~//finance//cdbs_2.csv",stringsAsFactors = F,sep = ";")

fit = loess(rent_eff ~ meses, data=df)

df$pred = predict(fit)
df$Delta = df$rent_eff/df$pred - 1

# Scatter plot
sp<-ggplot(df,aes(x=dias, y=rent_eff)) +
  geom_point(size=2,aes(color=Delta,text = paste(Tipo
                                                 ,Banco
                                                 , paste0(Rentabilidade*100
                                                 ,"% ")
                                                 ,"<br>Vencimento:"
                                                 , Vencimento
                                                 ,"<br>Meses:"
                                                 ,meses
                                                 ,"<br>Taxa efetiva"
                                                 ,paste0(round(rent_eff*100,1)
                                                         ,"%")
                                                 ,"<br>Delta"
                                                 ,paste0(round(Delta*100,1)
                                                         ,"%")
                                                 ),shape=Tipo)) + 
  theme_bw() +
  scale_color_gradient2(midpoint=0L, low="red", mid="gray",high="blue", space ="Lab" ) +
  geom_smooth(formula = y~x,span=0.75,colour = "gray")

sp


library(plotly)

p = sp %>%
  ggplotly(tooltip = "text")

htmlwidgets::saveWidget(as_widget(p), "~//finance//cdbs_20190226_0128.html")
