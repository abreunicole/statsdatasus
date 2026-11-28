#' Resumo Estatístico Descritivo Local (Offline)
#'
#' Calcula percentuais e gera um texto descritivo automático a partir de
#' uma tabela de frequências epidemiológicas, detectando automaticamente
#' as colunas de categoria e de contagem numérica.
#'
#' @param dados_resumo Dataframe contendo pelo menos uma coluna de texto/categoria e uma coluna numérica de contagem
#' @param col_cat Nome ou índice opcional da coluna de categoria (se NULL, usa a primeira coluna não numérica)
#' @param col_val Nome ou índice opcional da coluna de valores (se NULL, usa a primeira coluna numérica)
#' @return String com o resumo descritivo dos achados
#' @export
ag_stats_interpret <- function(dados_resumo, col_cat = NULL, col_val = NULL) {
  if (!is.data.frame(dados_resumo) || ncol(dados_resumo) < 2) {
    return("Resumo estatístico gerado localmente a partir dos microdados do DATASUS.")
  }

  # 1. Identifica automaticamente a coluna numérica de contagem
  if (is.null(col_val)) {
    num_cols <- which(vapply(dados_resumo, is.numeric, logical(1)))
    if (length(num_cols) == 0) {
      return("Nenhuma coluna numérica encontrada na tabela para cálculo estatístico.")
    }
    col_val <- num_cols[1]
  }

  # 2. Identifica automaticamente a coluna de categoria (preferencialmente texto/fator)
  if (is.null(col_cat)) {
    cat_cols <- which(!vapply(dados_resumo, is.numeric, logical(1)))
    col_cat <- if (length(cat_cols) > 0) cat_cols[1] else 1
  }

  categorias <- as.character(dados_resumo[[col_cat]])
  valores <- suppressWarnings(as.numeric(dados_resumo[[col_val]]))

  # Remove eventuais linhas de totalização se existirem ("Total", "Geral")
  validos <- !is.na(valores) & !tolower(trimws(categorias)) %in% c("total", "geral", "total geral")
  categorias <- categorias[validos]
  valores <- valores[validos]

  total <- sum(valores, na.rm = TRUE)

  if (length(valores) == 0 || total == 0) {
    return("Não foram encontrados registros válidos para o período selecionado.")
  }

  idx_max <- which.max(valores)
  cat_maior <- categorias[idx_max]
  val_maior <- valores[idx_max]
  pct_maior <- gsub("\\.", ",", as.character(round((val_maior / total) * 100, 1)))

  paste0(
    "Foram analisados ", format(total, big.mark = ".", decimal.mark = ","),
    " registros no total. A maior concentração ocorreu na categoria '", cat_maior,
    "', representando ", pct_maior, "% do total (n = ",
    format(val_maior, big.mark = ".", decimal.mark = ","), ")."
  )
}