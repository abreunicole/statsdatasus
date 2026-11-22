#' Agente de Limpeza e Higienização (DataCleaner)
#'
#' Limpa e padroniza CIDs, CBOs, datas e idades provenientes do DATASUS (SIM/SIH).
#'
#' @param dados Dataframe contendo os dados brutos ou processados do DATASUS
#' @return Dataframe higienizado e pronto para modelagem estatística
#' @import dplyr stringr lubridate
#' @export
ag_clean_datasus <- function(dados) {
  message("[ag-clean]: Iniciando higienização das variáveis...")

  # Decodificação vetorizada de idade (suporta SIM processado, SIH processado e SIM bruto)
  idade_calc <- if ("IDADEanos" %in% names(dados)) {
    as.numeric(dados$IDADEanos)
  } else if ("IDADE" %in% names(dados) && is.numeric(dados$IDADE)) {
    dados$IDADE
  } else if ("IDADE" %in% names(dados)) {
    x_pad <- stringr::str_pad(as.character(dados$IDADE), width = 3, side = "left", pad = "0")
    unidade <- substr(x_pad, 1, 1)
    valor <- suppressWarnings(as.numeric(substr(x_pad, 2, 3)))
    dplyr::case_when(
      unidade %in% c("0", "1", "2", "3") ~ 0,
      unidade == "4" ~ valor,
      unidade == "5" ~ 100 + valor,
      TRUE ~ NA_real_
    )
  } else {
    rep(NA_real_, nrow(dados))
  }

  dados_limpos <- dados |>
    # 1. Padronização de CIDs (Remove pontos, espaços e converte para maiúsculas)
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("CAUSABAS") | dplyr::contains("DIAG_PRINC"),
        ~ stringr::str_replace_all(stringr::str_to_upper(as.character(.x)), "[^A-Z0-9]", ""),
        .names = "{.col}_clean"
      )
    ) |>

    # 2. Higienização de CBOs (Preserva texto se já decodificado ou limpa dígitos se bruto)
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("OCUP") | dplyr::contains("CBOR"),
        ~ {
          val <- stringr::str_squish(stringr::str_to_upper(as.character(.x)))
          dplyr::if_else(val %in% c("", "000000", "NA", "IGNORADO"), NA_character_, val)
        },
        .names = "{.col}_clean"
      )
    ) |>

    # 3. Idade em anos e Faixas Etárias Epidemiológicas
    dplyr::mutate(
      idade_anos = idade_calc,
      faixa_etaria = cut(
        idade_anos,
        breaks = c(0, 20, 40, 60, 80, Inf),
        right = FALSE,
        labels = c("0-19", "20-39", "40-59", "60-79", "80+")
      )
    )

  message("[ag-clean]: Limpeza concluída com sucesso!")
  return(dados_limpos)
}