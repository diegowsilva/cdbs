require(RSelenium)
require(tidyverse)
require(rvest)
require(RJSONIO)
require(ggrepel)

rm(list=ls())
gc()


setwd("/Users/dws/coding/finance/cdbs")


#install.packages(c("RSelenium","tidyverse","rvest","RJSONIO","ggrepel"), repos='http://cran.us.r-project.org')


# this code requires a file named login_data.txt with the following content
#{"user":"12345","password": "xxx"}

login_data = "key/login_data_easynvest.txt" %>%
  RJSONIO::fromJSON(.)

URL = "https://www.easynvest.com.br/autenticacao"

sleep_time = 10

# start the server if one isnt running
#rD <- rsDriver(browser = "chrome",check = TRUE)
#remDr <- rD[["client"]]

eCap <- list(chromeOptions=list(binary="/Applications/Google\ Chrome\ Canary.app/Contents/MacOS/Google\ Chrome\ Canary"))
rD <- rsDriver(extraCapabilities = eCap)
remDr <- rD[["client"]]

remDr$navigate(URL)
Sys.sleep(sleep_time)
#remDr$screenshot(TRUE)

# //*[@id="username"]
webElem_username <- remDr$findElement(using = 'xpath', value = '//*[@id="username"]')
login_data[1] %>% 
  list %>% 
  webElem_username$sendKeysToElement(.)

# //*[@id="password"]
webElem_password <- remDr$findElement(using = 'xpath', value = '//*[@id="password"]')
login_data[2] %>% 
  list %>% 
  webElem_password$sendKeysToElement(.)
#remDr$screenshot(TRUE)
Sys.sleep(sleep_time)

rm(login_data)

# //*[@id="app"]/div[2]/section/div/div/form/div[3]/button
webElem_enter = remDr$findElement(using = 'xpath', value = '//*[@id="app"]/div[2]/section/div/div/form/div[3]/button')
webElem_enter$clickElement()
Sys.sleep(sleep_time)

#remDr$screenshot(TRUE)

# //*[@id="app"]/div[2]/div[1]/div/div/div/nav/ul/li[2]/a
webElem_invest = remDr$findElement(using = 'xpath', value = '//*[@id="app"]/div[2]/div[1]/div/div/div/nav/ul/li[2]/a')
webElem_invest$clickElement()
#remDr$screenshot(TRUE)
Sys.sleep(sleep_time+10)

# //*[@id="app"]/div[2]/div[2]/div/section/div[1]/div/div[2]/div/h1
webElem_cdb = remDr$findElement(using = 'xpath', value = '//*[@id="app"]/div[2]/div[2]/div/section/div[1]/div/div[2]/div/h1')
webElem_cdb$clickElement()
Sys.sleep(sleep_time+15)
#remDr$screenshot(TRUE)

# //*[@id="app"]/div[2]/div[2]/div/section[2]/div/div[1]/div/button[2]
remDr$findElement(using = 'xpath', value ='//*[@id="app"]/div[2]/div[2]/div/section[1]/div[1]')$clickElement()
webElem_scroll <- remDr$findElement("css", "body")
webElem_scroll$sendKeysToElement(list(key = "down_arrow"))
webElem_list_cdb = remDr$findElement(using = 'xpath', value = '//*[@id="app"]/div[2]/div[2]/div/section[2]/div/div[1]/div/button[2]')
webElem_list_cdb$clickElement()

# //*[@id="app"]/div[2]/div[2]/div/section[2]/div/table
source_cdb = remDr$getPageSource()
xpath_table_cdb = '//*[@id="app"]/div[2]/div[2]/div/section[2]/div/table'

# //*[@id="app"]/div[2]/div[1]/div/div/div/nav/ul/li[2]/a
webElem_invest = remDr$findElement(using = 'xpath', value = '//*[@id="app"]/div[2]/div[1]/div/div/div/nav/ul/li[2]/a')
webElem_invest$clickElement()
Sys.sleep(sleep_time+10)

# //*[@id="app"]/div[2]/div[2]/div/section/div[1]/div/div[3]/div/h1
webElem_lci = remDr$findElement(using = 'xpath', value = '//*[@id="app"]/div[2]/div[2]/div/section/div[1]/div/div[3]/div/h1')
webElem_lci$clickElement()
Sys.sleep(sleep_time+15)

# //*[@id="app"]/div[2]/div[2]/div/section[2]/div/table
source_lci = remDr$getPageSource()
xpath_table_lci = '//*[@id="app"]/div[2]/div[2]/div/section[2]/div/table'

remDr$close()
# stop the selenium server
rD[["server"]]$stop() 


rm(list=setdiff(ls(),c("source_cdb","xpath_table_cdb","source_lci","xpath_table_lci")))
gc()

# Working on CDB page
df_cdb = source_cdb[[1]] %>%
  read_html() %>%
  html_nodes(xpath = xpath_table_cdb) %>%
  html_table() %>%
  .[[1]]

names(df_cdb) = df_cdb[1,]
df_cdb = df_cdb[-1,1:5]
df_cdb$Modalidade = "CDB"
df_cdb = df_cdb %>% 
            filter(Liquidez != "D+1") %>%
            select(-matches("Liquidez"))

# Working on LCI page
df_lci = source_lci[[1]] %>%
  read_html() %>%
  html_nodes(xpath = xpath_table_lci) %>%
  html_table() %>%
  .[[1]]

names(df_lci) = df_lci[1,]
df_lci = df_lci[-1,1:5]
df_lci$Modalidade = "LCI"
df_lci = df_lci %>%
            select(-matches("CDB"))

df_all = rbind(df_cdb,df_lci)

df_all$Tipo   = df_all$Tipo %>% 
                  gsub(pattern = "Pr.+-Fixado",replacement = "Pre-Fixado",.)
df_all$Prazo  = df_all$Prazo %>% 
                  as.Date(.,"%d/%m/%Y")
df_all$Rentabilidade = df_all$Rentabilidade %>% 
                          gsub(pattern = ",",replacement = "\\.",.) %>%
                          gsub(pattern = "(% do CDI)|(% a.a.)|(IPCA \\+ )|(%)", replacement = "",.) %>%
                          as.numeric %>%
                          "/"(100)
rm(list = setdiff(ls(),"df_all"))

time_refresh = Sys.time()

write.csv(df_all,paste0("data/cdbs_lci_",gsub(pattern = "[^0-9]", replacement = "_",time_refresh),".csv"),row.names = F)
gc()
