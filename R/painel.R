#' Função para criar paineis dos atendentes
#'
#' Esta função server para ler arquivos no formato csv gerados pelo BDF
#'
#' @param a Arquivo CSV
#'
#' @examples
#' serv(servicos.txt)
#'
#'@export
recDiaria <- function(x){
  join_all(list(
    filter(encomendas, CAIXA==x) %>% ddply(~DIA, summarise,Encomenda = sum(VALOR)),
    filter(conveniencia, CAIXA==x) %>% ddply(~DIA, summarise,Conveniencia = sum(VALOR)),
    filter(internacional, CAIXA==x) %>% ddply(~DIA, summarise,Internacional = sum(VALOR)),
    filter(marketing, CAIXA==x) %>% ddply(~DIA, summarise,Marketing = sum(VALOR)),
    filter(mensagem, CAIXA==x) %>% ddply(~DIA, summarise,Mensagem = sum(VALOR))))
}

#'@export
recDiariaMenosInt <- function(x){
  join_all(list(
    filter(encomendas, CAIXA==x) %>% ddply(~DIA, summarise,Encomenda = sum(VALOR)),
    filter(conveniencia, CAIXA==x) %>% ddply(~DIA, summarise,Conveniencia = sum(VALOR)),
    filter(marketing, CAIXA==x) %>% ddply(~DIA, summarise,Marketing = sum(VALOR)),
    filter(mensagem, CAIXA==x) %>% ddply(~DIA, summarise,Mensagem = sum(VALOR))))
}

