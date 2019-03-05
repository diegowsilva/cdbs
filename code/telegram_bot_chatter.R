require(telegram.bot)
setwd("/Users/dws/finance/cdbs")

TELEGRAM_BOT_TOKEN = readLines("key/telegram_bot_token.txt")

# start handler function
start <- function(bot, update)
{
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("Oi, %s!
Utilize os comandos abaixo para ver a lista de CDB/LCI disponíveis na Easynvest:
/cdi - indexado ao CDI
/pre - pré-fixados
/ipca - IPCA + taxa (considero IPCA=4%%)",
                                 update$message$from$first_name))
}

# echo handler function
unknown <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = "Utilize os comandos abaixo para ver a lista de CDB/LCI disponíveis na Easynvest:
/cdi - indexado ao CDI
/pre - pré-fixados
/ipca - IPCA + % (considero IPCA=4%)")
}

# Example of a 'kill' command
kill <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = "Bye!")
  # Clean 'kill' update
  bot$getUpdates(offset = update$update_id + 1L)
  # Stop the updater polling
  updater$stop_polling()
}
filter_user <- as.BaseFilter(function(message) message$from_user  == "622818266")


# CDI
refresh_cdi <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("Oi, %s! 
                                 Seguem abaixo os melhores CDB/LCI indexados ao CDI disponiveis na Easynvest.", update$message$from$first_name))
  bot$sendPhoto(chat_id = update$message$chat_id,
                photo = "bot/plot_cdi.png")
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = paste0(readRDS("bot/msg_cdi.rds"),collapse = "\n"),
                  parse_mode = "Markdown")
  bot$sendMessage(chat_id = update$message$chat_id,text = paste0("Atualizado em: ",readRDS("bot/timestamp_cdbs.rds"),collapse = ""))
}

# pre-fixado
refresh_pre <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("Oi, %s! 
                                 Seguem abaixo os melhores CDB/LCI pré-fixados disponiveis na Easynvest.", update$message$from$first_name))
  bot$sendPhoto(chat_id = update$message$chat_id,
                photo = "bot/plot_pre.png")
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = paste0(readRDS("bot/msg_pre.rds"),collapse = "\n"),
                  parse_mode = "Markdown")
  bot$sendMessage(chat_id = update$message$chat_id,text = paste0("Atualizado em: ",readRDS("bot/timestamp_cdbs.rds"),collapse = ""))
}

# IPCA
refresh_ipca <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("Oi, %s! 
                                 Seguem abaixo os melhores CDB/LCI indexados ao *IPCA* disponiveis na Easynvest.", update$message$from$first_name),
                  parse_mode = "Markdown")
  bot$sendPhoto(chat_id = update$message$chat_id,
                photo = "bot/plot_ipca.png")
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = paste0(readRDS("bot/msg_ipca.rds"),collapse = "\n"),
                  parse_mode = "Markdown")
  bot$sendMessage(chat_id = update$message$chat_id,text = paste0("Atualizado em: ",readRDS("bot/timestamp_cdbs.rds"),collapse = ""))
}

# build the updater
updater <- Updater(token = TELEGRAM_BOT_TOKEN) +
  CommandHandler("start", start) +
  CommandHandler("cdi", refresh_cdi) +
  CommandHandler("pre", refresh_pre) +
  CommandHandler("ipca", refresh_ipca) +
  CommandHandler("kill", kill, filter_user) +
  MessageHandler(unknown, MessageFilters$command)+
  MessageHandler(unknown, MessageFilters$text)

# start polling
updater$start_polling()