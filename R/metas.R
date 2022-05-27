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
metasAg <- function(a,b,c,d,e,f,g,h,i,j){
  segmento <- c('Conveniencia','Encomendas','Internacional','Marketing','Mensagens','Total')
  metaTotal <- c(a,b,c,d,e)
  metaTotal <- c(metaTotal,sum(metaTotal))
  metaParcial <- c(metaTotal[1]/diasUteisMes*dia_atual,metaTotal[2]/diasUteisMes*dia_atual,
                   metaTotal[3]/diasUteisMes*dia_atual,metaTotal[4]/diasUteisMes*dia_atual,
                   metaTotal[5]/diasUteisMes*dia_atual)
  metaParcial <- c(metaParcial,sum(metaParcial))
  receita <- c(sum(f$VALOR),sum(g$VALOR),sum(h$VALOR),sum(i$VALOR),sum(j$VALOR))
  receita <- c(receita,sum(receita))
  falta <- c(receita[1]-metaParcial[1],receita[2]-metaParcial[2],receita[3]-metaParcial[3],
             receita[4]-metaParcial[4],receita[5]-metaParcial[5])
  falta <- c(falta,sum(falta))
  porceGeral <- c(receita[1]/metaParcial[1]*100,receita[2]/metaParcial[2]*100,
                  receita[3]/metaParcial[3]*100,receita[4]/metaParcial[4]*100,
                  receita[5]/metaParcial[5]*100,receita[6]/metaParcial[6]*100)
  return(data.frame(Segmento=segmento,'Meta Total'=metaTotal,'Meta Parcial'=round(metaParcial,2),
                    Receita=round(receita,2),Diferenca=round(falta,2),Porcent=round(porceGeral,2)))

}

#'@export
metasAtendentes <- function(x){
  metaTotal <-round(c(metasVista$Meta.Total[1]*metaDivisao$Porcentagem[metaDivisao$Caixa==x]/100,
                      metasVista$Meta.Total[2]*metaDivisao$Porcentagem[metaDivisao$Caixa==x]/100,
                      metasVista$Meta.Total[3]*metaDivisao$Porcentagem[metaDivisao$Caixa==x]/100,
                      metasVista$Meta.Total[4]*metaDivisao$Porcentagem[metaDivisao$Caixa==x]/100,
                      metasVista$Meta.Total[5]*metaDivisao$Porcentagem[metaDivisao$Caixa==x]/100,
                      metasVista$Meta.Total[6]*metaDivisao$Porcentagem[metaDivisao$Caixa==x]/100),2)
  metaParcial <- round(c((metaTotal[1]/diasUteisMes)*dia_atual,
                         (metaTotal[2]/diasUteisMes)*dia_atual,
                         (metaTotal[3]/diasUteisMes)*dia_atual,
                         (metaTotal[4]/diasUteisMes)*dia_atual,
                         (metaTotal[5]/diasUteisMes)*dia_atual,
                         (metaTotal[6]/diasUteisMes)*dia_atual),2)

  receita <-   round(c(conveniencia %>% filter(CAIXA==x) %>% {sum(.$VALOR)},
                       encomendas %>% filter(CAIXA==x) %>% {sum(.$VALOR)},
                       internacional %>% filter(CAIXA==x) %>% {sum(.$VALOR)},
                       marketing %>% filter(CAIXA==x) %>% {sum(.$VALOR)},
                       mensagem %>% filter(CAIXA==x) %>% {sum(.$VALOR)}),2)
  receita <- c(receita,sum(receita))

  dif <- c(receita[1]-metaParcial[1],receita[2]-metaParcial[2],receita[3]-metaParcial[3],
           receita[4]-metaParcial[4],receita[5]-metaParcial[5],receita[6]-metaParcial[6])

  porc <- round(c(receita[1]/metaParcial[1]*100,receita[2]/metaParcial[2]*100,
                  receita[3]/metaParcial[3]*100,receita[4]/metaParcial[4]*100,
                  receita[5]/metaParcial[5]*100,receita[6]/metaParcial[6]*100),2)

  return(data.frame('Meta Total'=metaTotal, 'Meta Parcial'=metaParcial, 'Receita'=receita,
                    'Diferença'=dif, 'Porcent'=porc))
}

