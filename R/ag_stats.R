#' Resumo Estatístico Descritivo Local (Offline)
#'
#' Calcula percentuais e gera um texto descritivo automático a partir de
#' uma tabela de frequências epidemiológicas, sem depender de IA externa.
#'
#' @param dados_resumo Dataframe com uma coluna de categoria e uma coluna numérica de contagem
#' @return String com o resumo descritivo dos achados
#' @export
ag_stats_interpret <- function(dados_resumo) {
  if (!is.data.frame(dados_resumo) || ncol(dados_resumo) < 2) {
    return("Resumo estatístico gerado localmente a partir dos microdados do DATASUS.")
  }

  categorias <- as.character(dados_resumo[[1]])
  valores <- as.numeric(dados_resumo[[2]])
  total <- sum(valores, na.rm = TRUE)

  if (total == 0 || all(is.na(valores))) {
    return("Não foram encontrados registros válidos para o período selecionado.")
  }

  idx_max <- which.max(valores)
  cat_maior <- categorias[idx_max]
  val_maior <- valores[idx_max]
  pct_maior <- round((val_maior / total) * 100, 1)

  paste0(
    "Foram analisados ", format(total, big.mark = ".", decimal.mark = ","),
    " registros no total. A maior concentração ocorreu na categoria '",
    cat_maior, "', representando ", pct_maior, "% do total (n = ",
    format(val_maior, big.mark = ".", decimal.mark = ","), ")."
  )
}