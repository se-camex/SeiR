library(RSelenium)
library(dplyr)
library(purrr)
get_user_sei_unidade <- function(i) {
  cat(i%>%paste0("\n"))
  unidades <- remDr$findElement(value="selInfraUnidades", using='id')
  unidades$clickElement()
  m <- unidades$findChildElement(value=paste0("option[value=\"", i, "\""), using='css')
  m$clickElement()
  processo <- remDr$findElement(value="chkRecebidosItem0", using='id')
  processo$clickElement()
  atribuir <- remDr$findElement(value="#divComandos :nth-child(3) .infraCorBarraSistema", using='css')
  atribuir$clickElement()
  users <- remDr$getPageSource()%>%as.character()%>%get_users_sei()
  users
}

get_users_sei <- function(x) {
  require(rvest)
  x <- read_html(x)
  unidades <- x%>%html_node("#selInfraUnidades")%>%html_nodes("option")
  unidade <- unidades[unidades%>%as.character%>%grep("selected", .)]%>%html_text
  usuarios <- x%>%html_node("#selAtribuicao")%>%html_nodes("option")
  values <- usuarios%>%html_attr("value")#%>%unlist
  values <- values[!values%in%"null"]
  names <- usuarios%>%
    html_text%>%
    strsplit(" - |class")%>%
    purrr::map_dfr(function(x) tibble(user_sei=x[1], nome_sei=x[2])%>%
                     filter(!is.na(nome_sei)))%>%
    arrange(nome)%>%mutate(unidade=unidade, id_sei=values, user_sei)
  names
}


# O chrome bloqueia o acesso ao SEI por conta de certificado.
# Ainda não solucionei o problema.
#
# cprof <- getChromeProfile(
#   #"C:\\Users\\eleon\\AppData\\Local\\Google\\Chrome\\User Data",
#   "/home/seluser/.config/google-chrome",
#   "Profile 1"
# )
#
#
# cprof$chromeOptions$args <- list("--allow-running-insecure-content", "--ignore-ssl-errors=yes", "--ignore-certificate-errors", "--disable-web-security", "--allow-insecure-localhost", "--unsafely-treat-insecure-origin-as-secure")

## Utilizando o Docker (local) do Selenium
## https://grishagin.com/r/rselenium/2017/11/11/setup-rselenium-windows10.html#:~:text=%20How%20to%20Setup%20Environment%20for%20RSelenium%20on,R%20Session%20to%20Selenium%20Server.%20%20More%20
## docker pull selenium/standalone-chrome-debug
## docker run -d -p 5901:5900 -p 4445:4444 selenium/standalone-chrome-debug

remDr<-
  remoteDriver(remoteServerAddr = "192.168.99.100"
               , port = 4445L
               , browserName = "chrome"
              #, extraCapabilities = list(
               # args=cprof#"--allow-running-insecure-content"
                # args="--ignore-certificate-errors"
                # "ignore-ssl-errors"="yes",
                #                          "accept_insecure_certs"=TRUE
                                         #chrome.switches="--ignore-certificate-errors")
                                         )


remDr$closeall()
remDr$open()
remDr$navigate("https://sei.fazenda.gov.br")


##login
user <- remDr$findElement(value = "txtUsuario", using = "id")
user$clearElement()
user$sendKeysToElement(list(user_sei))
pwd <- remDr$findElement(value = "pwdSenha", using = "id")
pwd$clearElement()
pwd$sendKeysToElement(list(pwd_sei))
login <- remDr$findElement(value="sbmLogin", using='id')
login$clickElement()


unidades <- remDr$findElement(value="selInfraUnidades", using='id')
unidades_sel <- unidades$selectTag()
unidades_disp <- unidades_sel$text

library(tictoc)
tic()
usuarios <- map_dfr(unidades_sel$value, possibly(get_user_sei_unidade, otherwise = tibble()))
toc()

unidades_missing <-(tibble(unidade=unidades_disp)%>%anti_join(usuarios))
stopifnot(nrow(unidades_missing%>%nrow)>0)

remDr$closeall()
remDr$closeServer()


#usuarios <- usuarios%>%rename(user_sei=id_sei, id_sei=id_sei_num, nome_sei=nome)

