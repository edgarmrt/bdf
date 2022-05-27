#' Função para dve
#'
#' Esta função server para ler arquivos no formato csv gerados pelo BDF
#'
#' @param a Arquivo CSV
#'
#' @examples
#' serv(servicos.txt)
#'
#'@export
dve <- function(indDve){
  dveSedex <- ((indDve[1]*100)/metasDVE$Meta[metasDVE$Indicadores=='Sedex'])* metasDVE$Peso[metasDVE$Indicadores=='Sedex']
  dveIOE <- ((indDve[2]*100)/metasDVE$Meta[metasDVE$Indicadores=='IOE'])* metasDVE$Peso[metasDVE$Indicadores=='IOE']
  dveEmb <- ((indDve[3]*100)/metasDVE$Meta[metasDVE$Indicadores=='Emb'])*metasDVE$Peso[metasDVE$Indicadores=='Emb']
  dveAR <- ((indDve[4]*100)/metasDVE$Meta[metasDVE$Indicadores=='AR'])*metasDVE$Peso[metasDVE$Indicadores=='AR']
  dveVD <- ((indDve[5]*100)/metasDVE$Meta[metasDVE$Indicadores=='VD'])*metasDVE$Peso[metasDVE$Indicadores=='VD']
  dveTqtMedVD <- ((indDve[6]*100)/metasDVE$Meta[metasDVE$Indicadores=='TqtMedVD'])*metasDVE$Peso[metasDVE$Indicadores=='TqtMedVD']
  dveInt <- ((indDve[7]*100)/metasDVE$Meta[metasDVE$Indicadores=='Inter'])*metasDVE$Peso[metasDVE$Indicadores=='Inter']
  dveSedex <-if_else(dveSedex<=metasDVE$Maximo[1],dveSedex,metasDVE$Maximo[1])
  dveIOE <- if_else(dveIOE<=metasDVE$Maximo[2],dveIOE,metasDVE$Maximo[2])
  dveEmb <- if_else(dveEmb<=metasDVE$Maximo[3],dveEmb,metasDVE$Maximo[3])
  dveAR <- if_else(dveAR<=metasDVE$Maximo[4],dveAR,metasDVE$Maximo[4])
  dveVD <- if_else(dveVD<=metasDVE$Maximo[5],dveVD,metasDVE$Maximo[5])
  dveTqtMedVD <- if_else(dveTqtMedVD<=metasDVE$Maximo[6],dveTqtMedVD,metasDVE$Maximo[6])
  dveInt <- if_else(dveInt<=metasDVE$Maximo[7],dveInt,metasDVE$Maximo[7])

  dve <- if_else(dveInt!=0,
                 round(sum(dveSedex,dveIOE,dveEmb,dveAR,dveVD,dveTqtMedVD,dveInt)/
                         (sum(metasDVE$Peso)),2),
                 round(sum(dveSedex,dveIOE,dveEmb,dveAR,dveVD,dveTqtMedVD,dveInt)/
                         (sum(metasDVE$Peso)-2),2))
}


