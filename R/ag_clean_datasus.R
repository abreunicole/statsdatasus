#' Agente de Limpeza e Higienização Universal (DataCleaner)
#'
#' Limpa e padroniza CIDs, CBOs, datas e idades provenientes do DATASUS (SIM/SIH),
#' permitindo opcionalmente marcar um grupo de CIDs ou grande grupo da CBO de interesse.
#'
#' @param dados Dataframe contendo os dados brutos ou processados do DATASUS
#' @param cid_pattern Expressão regular (RegEx) opcional para marcar o desfecho (ex: `"^B3[5-9]|^B4[0-9]|^B59"`). Padrão NULL.
#' @param cbo_prefix Prefixo numérico opcional da CBO para marcar exposição ocupacional (ex: "^6" para agropecuária). Padrão NULL.
#' @return Dataframe higienizado e pronto para modelagem estatística
#' @import dplyr stringr lubridate
#' @export
ag_clean_datasus <- function(dados, cid_pattern = NULL, cbo_prefix = NULL) {
  message("[ag-clean]: Iniciando higienização universal das variáveis...")

  # 1. Decodificação vetorizada de idade (SIM processado, SIH processado ou SIM bruto)
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

  # 2. Extração universal do ano (DTOBITO no SIM ou ANO_CMPT no SIH)
  ano_calc <- if ("ANO_CMPT" %in% names(dados)) {
    suppressWarnings(as.integer(dados$ANO_CMPT))
  } else if ("DTOBITO" %in% names(dados)) {
    x_dt <- as.character(dados$DTOBITO)
    dplyr::case_when(
      stringr::str_detect(x_dt, "^\\d{8}$") ~ suppressWarnings(as.integer(substr(x_dt, 5, 8))),
      stringr::str_detect(x_dt, "^\\d{4}-\\d{2}-\\d{2}") ~ suppressWarnings(as.integer(substr(x_dt, 1, 4))),
      stringr::str_detect(x_dt, "^\\d{2}/\\d{2}/\\d{4}") ~ suppressWarnings(as.integer(substr(x_dt, 7, 10))),
      TRUE ~ NA_integer_
    )
  } else {
    rep(NA_integer_, nrow(dados))
  }

  # 3. Coluna principal de CID e Ocupação padronizadas
  cid_alvo <- if ("CAUSABAS" %in% names(dados)) {
    as.character(dados$CAUSABAS)
  } else if ("DIAG_PRINC" %in% names(dados)) {
    as.character(dados$DIAG_PRINC)
  } else {
    rep(NA_character_, nrow(dados))
  }

  ocup_alvo <- if ("OCUP" %in% names(dados)) {
    as.character(dados$OCUP)
  } else if ("CBOR" %in% names(dados)) {
    as.character(dados$CBOR)
  } else {
    rep(NA_character_, nrow(dados))
  }

  dados_limpos <- dados |>
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("CAUSABAS") | dplyr::contains("DIAG_PRINC"),
        ~ stringr::str_replace_all(stringr::str_to_upper(as.character(.x)), "[^A-Z0-9]", ""),
        .names = "{.col}_clean"
      ),
      dplyr::across(
        dplyr::contains("OCUP") | dplyr::contains("CBOR"),
        ~ {
          val <- stringr::str_squish(stringr::str_to_upper(as.character(.x)))
          dplyr::if_else(val %in% c("", "000000", "NA", "IGNORADO"), NA_character_, val)
        },
        .names = "{.col}_clean"
      ),
      ano_evento = ano_calc,
      idade_anos = idade_calc,
      faixa_etaria = cut(
        idade_anos,
        breaks = c(0, 20, 40, 60, 80, Inf),
        right = FALSE,
        labels = c("0-19", "20-39", "40-59", "60-79", "80+")
      ),
      cid_principal_clean = stringr::str_replace_all(stringr::str_to_upper(cid_alvo), "[^A-Z0-9]", ""),
      ocup_principal_clean = dplyr::if_else(
        stringr::str_squish(stringr::str_to_upper(ocup_alvo)) %in% c("", "000000", "NA", "IGNORADO", "NAO INFORMADO"),
        NA_character_,
        stringr::str_squish(stringr::str_to_upper(ocup_alvo))
      )
    )

  # 4. Marcador dinâmico opcional de desfecho (CID)
  if (!is.null(cid_pattern)) {
    dados_limpos <- dados_limpos |>
      dplyr::mutate(
        desfecho_alvo = stringr::str_detect(cid_principal_clean, cid_pattern)
      )
  }

  # 5. Marcador dinâmico opcional de ocupação (CBO)
  if (!is.null(cbo_prefix)) {
    dados_limpos <- dados_limpos |>
      dplyr::mutate(
        cbo_digitos = stringr::str_replace_all(ocup_principal_clean, "[^0-9]", ""),
        exposicao_cbo = dplyr::case_when(
          is.na(ocup_principal_clean) ~ NA,
          !is.na(cbo_digitos) & cbo_digitos != "" & stringr::str_detect(cbo_digitos, cbo_prefix) ~ TRUE,
          TRUE ~ FALSE
        )
      )
  }

  message("[ag-clean]: Limpeza concluída com sucesso!")
  return(dados_limpos)
}