#' Função para ler arquivos csv do BDF
#'
#' Esta função server para ler arquivos no formato csv gerados pelo BDF
#'
#' @param a Arquivo CSV
#'
#' @examples
#' serv(servicos.txt)
#'
#'@export
ler <- function(a,b,c,d,e,f,g,h){
  ## Atribuindo primeira posição do vetor de leitura a variavel metas
  ## Utilizando sapply pra verificar quais variaveis são numericas e atribuindo o resultado a variavel is.num
  ## Utilizando lapply para arredondar os valores numericos para duas casas decimais
  metas <- read_tsv(a, locale = locale(encoding = "latin1"))
  is.num <- sapply(metas, is.numeric)
  metas[is.num] <- lapply(metas[is.num], round, 2)

  ## Lendo Arquivos
  comissao <- read_csv2(b, locale = locale(encoding = "latin1"))
  servicos <- read_csv2(c) %>% select('CÓDIGO SERVIÇO',NOME...5,'DATA MOV.',QTD,VALOR,'SUB CAIXA','CEP DEST.')
  prod01 <- read_csv2(d) %>% select('CÓDIGO PRODUTO',NOME...4,'DATA MOV.',VALOR,QTD,'TIPO PAG.') %>% mutate(CAIXA=1)
  prod01[is.na(prod01)] <- 0
  prod04 <- read_csv2(e) %>% select('CÓDIGO PRODUTO',NOME...4,'DATA MOV.',VALOR,QTD,'TIPO PAG.') %>% mutate(CAIXA=4)
  prod04[is.na(prod04)] <- 0
  prod10 <- read_csv2(f) %>% select('CÓDIGO PRODUTO',NOME...4,'DATA MOV.',VALOR,QTD,'TIPO PAG.') %>% mutate(CAIXA=10)
  prod10[is.na(prod10)] <- 0
  resgTele <- read_csv2(g) %>% select('CODIGO PRODUTO',NOME...4,'DATA MOVIMENTO','NR.S.CAIXA','QTD RESG.',VALOR)
  adicionais <- read_csv2(h) %>% select('CÓDIGO SERVIÇO',NOME...4,'CODIGO SERV. ADICIONAL',NOME...6,'DATA POSTAGEM','MAT. SUB_CAIXA',QUANTIDADE,'VLR SERVIÇO')
  adicionais[is.na(adicionais)] <- 0

  # Alterando o nome das colunas
  colnames(servicos)[c(1,2,3,6,7)] <-  c('CODIGO','NOME','DIA','CAIXA','CEP')
  colnames(prod01)[c(1,2,3,6)] <- c('CODIGO','NOME','DIA','PAG')
  colnames(prod04)[c(1,2,3,6)] <- c('CODIGO','NOME','DIA','PAG')
  colnames(prod10)[c(1,2,3,6)] <- c('CODIGO','NOME','DIA','PAG')
  colnames(resgTele)[c(1,2,3,4,5)] <- c('CODIGO','NOME','DIA','CAIXA','QTD')
  colnames(adicionais)[c(1,2,3,4,5,6,7,8)] <- c('COD_SERV','NOME_SERV','COD_ADIC','NOME_ADIC','DIA','CAIXA','QTD','VALOR')

  # Convertendo os campos DIA para tipo DATA
  servicos$DIA <- dmy(servicos$DIA)
  prod01$DIA <- dmy(prod01$DIA)
  prod04$DIA <- dmy(prod04$DIA)
  prod10$DIA <- dmy(prod10$DIA)
  resgTele$DIA <- dmy(resgTele$DIA)
  adicionais$DIA <- dmy(adicionais$DIA)

  servicos$CEP <- as.numeric(servicos$CEP)
  servicos[is.na(servicos)] <- 0

  return(list(metas,comissao,servicos,prod01,prod04,prod10,resgTele,adicionais))
}
