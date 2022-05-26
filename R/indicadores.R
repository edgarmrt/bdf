#' Função para criar indicadores dos atendentes
#'
#' Esta função server para ler arquivos no formato csv gerados pelo BDF
#'
#' @param a Arquivo CSV
#'
#' @examples
#' serv(servicos.txt)
#'
#'@export
indicadores <- function(x){
  encomendas <- filter(encomendas, CAIXA==x) %>%
    ddply(CODIGO~NOME, summarise,Receita= sum(VALOR), Quantidade=sum(QTD))
  embalagens <- filter(embalagens, CAIXA==x) %>%
    ddply(CODIGO~NOME, summarise,Receita= sum(VALOR), Quantidade=sum(QTD))

  qntEnc <- sum(encomendas$Quantidade)
  qntSedex <- sum(encomendas$Quantidade)-encomendas$Quantidade[encomendas$CODIGO==4510]
  qntEmb <- sum(embalagens$Quantidade)
  recEmb <- sum(embalagens$Receita)
  qntAR <- adicionais %>% filter(CAIXA==x & COD_ADIC %in% c("001","021","039","046")) %>%
    {sum(.$QUANTIDADE)}
  recAR <- adicionais %>% filter(CAIXA==x & COD_ADIC %in% c("001","021","039","046")) %>%
    {sum(.$RECEITA)}
  qntVD <- adicionais %>% filter(CAIXA==x & COD_ADIC %in% c("032","064")) %>%
    {sum(.$QUANTIDADE)}
  recVD <- adicionais %>% filter(CAIXA==x & COD_ADIC %in% c("032","064")) %>%
    {sum(.$RECEITA)}

  return(c(qntEnc,qntSedex,qntAR,recAR,qntVD,recVD,qntEmb,recEmb))
}

#'@export
indicadoresDVE <- function(ind,caixa){
  # Indice Sedex
  indSedex <- round((ind[2]*100)/ind[1],2)

  # IOE
  indIOE <- round(ioe %>% filter(CAIXA==caixa) %>% {sum(.$QTD[.$CODIGO==4782])*100/sum(.$QTD)},2)

  #Embalagens
  indEmb <- round(sum(embalagens$QTD[embalagens$CAIXA==caixa]*100)/ind[1],2)

  #AR
  indAR <- round(adicionais %>% filter(CAIXA==caixa & COD_ADIC %in% c("001","021","039","046")) %>%
                   {sum(.$QUANTIDADE)*100/ind[1]},2)

  #VD
  indVD <- round(adicionais %>% filter(CAIXA==caixa & COD_ADIC %in% c("032","064")) %>%
                   {sum(.$QUANTIDADE)*100/ind[1]},2)

  #Tiquet Medio VD
  indTqtMed <- round(adicionais %>% filter(CAIXA==caixa & COD_ADIC %in% c("032","064")) %>%
                       {sum(.$RECEITA)/sum(.$QUANTIDADE)},2)

  #Internacional
  indInt <- round(internacional %>% filter(CAIXA==caixa) %>%
                    {sum(.$QTD[.$CODIGO==45110|.$CODIGO==45012|.$CODIGO==45209])/sum(.$QTD)}*100,2)
  if (is.nan(indInt)){indInt <- 0}

  return(c(indSedex,indIOE,indEmb,indAR,indVD,indTqtMed,indInt))
}

