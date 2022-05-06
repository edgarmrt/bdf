#' Função para segmentar serviços categoria Contabil
#'
#' Esta função server filtrar codigos nas tabelas serviço e produtos e segmentar
#'
#' @param a tabelas serviços e produtos
#'
#' @examples
#' segmentos(servicos,produtos)
#'@export
segmentos <- function(servicos,prodVist){
      ## Segmentando serviços categoria Contabil
      caixa <- filter(servicos, CODIGO %in% c(6130,35505))
      caixaPostal <- filter(servicos,CODIGO %in% c(34088,34096))
      carneBau <- filter(prodCont,CODIGO==884001768)
      ceped <- filter(servicos, CODIGO==35483)
      certDigital <- filter(servicos,CODIGO==89036)
      chip <- filter(prodCont,CODIGO==884001733)
      cpf <- filter(servicos, CODIGO %in% c(34207,34223))
      doacao <- filter(servicos, CODIGO %in% c(6483))
      ipsm <- filter(servicos,CODIGO==34843)
      parcBau <- filter(servicos,CODIGO==6122)
      recarga <- filter(servicos,CODIGO %in% c(5070,6181,6211))
      recarga$VALOR <- round(recarga$VALOR)
      resgPostalCap <- filter(servicos, CODIGO %in% c(5940))
      serasa <- filter(servicos, CODIGO %in% c(34355,34800,34797))
      telesena <- filter(prodVist,CODIGO %in% c(881503940))
      vpd <- filter(servicos, CODIGO==49190)
      vpne <- filter(servicos, CODIGO==91537)

      # Embalagens
      embalagens <- filter(prodVist,
                           CODIGO %in% c(116600063,116600993,116600438,116600926,116600934,
                                         116600942,116601000,116601094,116600071,116601019,
                                         116601213,740200798,740500864,760200564,765000652,
                                         765000660,765000903,765000911,760200572,853004307,
                                         853004471))

      selos <- filter(prodVist,
                      CODIGO %in% c(851001246,851001297,851001378,851001432,851001440,
                                    851001530,851001572,851001580))
}
