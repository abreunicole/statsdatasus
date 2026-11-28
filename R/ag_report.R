#' Agente de Geração de Relatório Epidemiológico Completo (HTML)
#'
#' Gera um relatório HTML local contendo o resumo estatístico, tabelas cruzadas
#' por micose, UF, ano, perfil sociodemográfico, ocupação agropecuária, CID-10
#' e a tabela completa dos registros individuais.
#'
#' @param uf Rótulo da região ou UF analisada
#' @param tabela_resumo Dataframe resumido principal
#' @param parecer_llm Texto descritivo gerado pelo `ag_stats_interpret()`
#' @param output_dir Pasta onde o arquivo HTML será salvo
#' @param dados_completos Dataframe completo higienizado para gerar todas as seções
#' @return Caminho do arquivo HTML gerado
#' @export
ag_report_generate <- function(uf = "Centro_Oeste",
                               tabela_resumo,
                               parecer_llm = "",
                               output_dir = "resultados",
                               dados_completos = NULL) {
  message("[ag-report]: Compilando relatório epidemiológico completo em HTML...")
  
  # Garante que a pasta de resultados existe
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
    # Caminhos dos arquivos R Markdown e HTML
  rmd_temp <- tempfile(fileext = ".Rmd")
  html_saida <- file.path(output_dir, paste0("Relatorio_Completo_", uf, ".html"))
  
  linhas_rmd <- c(
    "---",
    paste0("title: \"Relatório Epidemiológico Completo — ", uf, "\""),
    paste0("date: \"Gerado em: ", format(Sys.time(), "%d/%m/%Y às %H:%M"), "\""),
    "output:",
    "  html_document:",
    "    toc: true",
    "    toc_float: true",
    "    theme: cosmo",
    "---",
    "",
    "<style>",
    "  body { font-size: 14px; }",
    "  table { width: 100%; border-collapse: collapse; margin-bottom: 20px; }",
    "  th { background-color: #2c3e50; color: white; padding: 8px; text-align: left; }",
    "  td { padding: 6px 8px; border-bottom: 1px solid #ddd; }",
    "  tr:nth-child(even) { background-color: #f9f9f9; }",
    "  .caixa-tabela { max-height: 520px; overflow-y: auto; overflow-x: auto; border: 1px solid #ccc; padding: 6px; margin-bottom: 25px; }",
    "  .resumo-box { background: #f4f6f9; border-left: 4px solid #2c3e50; padding: 12px 16px; margin-bottom: 20px; }",
    "</style>",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "library(dplyr)",
    "```",
    "",
    "<div class=\"resumo-box\">",
    paste0("**Síntese Automática:** ", parecer_llm),
    "</div>",
    "",
    "## 1. Óbitos por Micose Sistêmica e Tipo de Registro",
    "",
    "```{r tab-principal}",
    "if (!is.null(dados_completos) && nrow(dados_completos) > 0) {",
    "  tab1 <- dados_completos %>%",
    "    count(grupo_micose, tipo_registro, name = \"obitos\") %>%",
    "    tidyr::pivot_wider(names_from = tipo_registro, values_from = obitos, values_fill = 0) %>%",
    "    mutate(Total = rowSums(across(where(is.numeric)))) %>%",
    "    arrange(desc(Total)) %>%",
    "    mutate(`Percentual (%)` = round(100 * Total / sum(Total), 2))",
    "  knitr::kable(tab1)",
    "} else {",
    "  knitr::kable(tabela_resumo)",
    "}",
    "```",
    "",
    "```{r secoes-completas, results='asis'}",
    "if (!is.null(dados_completos) && nrow(dados_completos) > 0) {",
    "  cat('\\n## 2. Distribuição por Estado (UF) e Ano\\n\\n')",
    "  tab_uf_ano <- dados_completos %>%",
    "    count(uf, ano, name = 'obitos') %>%",
    "    arrange(ano) %>%",
    "    tidyr::pivot_wider(names_from = ano, values_from = obitos, values_fill = 0) %>%",
    "    mutate(Total_UF = rowSums(across(where(is.numeric)))) %>%",
    "    arrange(desc(Total_UF))",
    "  print(knitr::kable(tab_uf_ano))",
    "",
    "  cat('\\n## 3. Perfil Ocupacional (Agropecuária) e Sociodemográfico\\n\\n')",
    "  cat('### 3.1 Ocupação Agropecuária (CBO Grupo 6)\\n\\n')",
    "  tab_agro <- dados_completos %>%",
    "    count(trabalho_agro, name = 'Total_Obitos', sort = TRUE) %>%",
    "    mutate(`Percentual (%)` = round(100 * Total_Obitos / sum(Total_Obitos), 2))",
    "  print(knitr::kable(tab_agro))",
    "",
    "  cat('\\n### 3.2 Sexo\\n\\n')",
    "  tab_sexo <- dados_completos %>%",
    "    count(sexo_padrao, name = 'Total_Obitos', sort = TRUE) %>%",
    "    mutate(`Percentual (%)` = round(100 * Total_Obitos / sum(Total_Obitos), 2))",
    "  print(knitr::kable(tab_sexo))",
    "",
    "  cat('\\n### 3.3 Faixa Etária\\n\\n')",
    "  tab_idade <- dados_completos %>%",
    "    count(faixa_etaria, name = 'Total_Obitos') %>%",
    "    mutate(`Percentual (%)` = round(100 * Total_Obitos / sum(Total_Obitos), 2))",
    "  print(knitr::kable(tab_idade))",
    "",
    "  cat('\\n### 3.4 Raça / Cor\\n\\n')",
    "  tab_raca <- dados_completos %>%",
    "    count(raca_padrao, name = 'Total_Obitos', sort = TRUE) %>%",
    "    mutate(`Percentual (%)` = round(100 * Total_Obitos / sum(Total_Obitos), 2))",
    "  print(knitr::kable(tab_raca))",
    "",
    "  if ('LOCOCOR' %in% names(dados_completos)) {",
    "    cat('\\n### 3.5 Local de Ocorrência do Óbito\\n\\n')",
    "    tab_local <- dados_completos %>%",
    "      count(LOCOCOR, name = 'Total_Obitos', sort = TRUE) %>%",
    "      mutate(`Percentual (%)` = round(100 * Total_Obitos / sum(Total_Obitos), 2))",
    "    print(knitr::kable(tab_local))",
    "  }",
    "",
    "  col_esc <- intersect(c('ESC2010', 'ESC'), names(dados_completos))[1]",
    "  if (!is.na(col_esc)) {",
    "    cat('\\n### 3.6 Escolaridade\\n\\n')",
    "    tab_esc <- dados_completos %>%",
    "      count(.data[[col_esc]], name = 'Total_Obitos', sort = TRUE) %>%",
    "      mutate(`Percentual (%)` = round(100 * Total_Obitos / sum(Total_Obitos), 2))",
    "    print(knitr::kable(tab_esc))",
    "  }",
    "",
    "  cat('\\n## 4. Detalhamento por Código CID-10 Específico\\n\\n')",
    "  tab_cids <- dados_completos %>%",
    "    count(grupo_micose, cid_fungico_principal, CAUSABAS, name = 'Total_Obitos', sort = TRUE) %>%",
    "    mutate(`Percentual (%)` = round(100 * Total_Obitos / sum(Total_Obitos), 2))",
    "  print(knitr::kable(tab_cids))",
    "",
    "  cat('\\n## 5. Base Individual Completa (Todos os Registros)\\n\\n')",
    "  cat('<div class=\"caixa-tabela\">\\n')",
    "  cols_exibir <- intersect(c(",
    "    'uf', 'ano', 'DTOBITO', 'municipio_residencia', 'grupo_micose',",
    "    'cid_fungico_principal', 'cids_fungicos_encontrados', 'tipo_registro',",
    "    'CAUSABAS', 'LINHAA', 'LINHAB', 'LINHAC', 'LINHAD', 'LINHAII',",
    "    'idade_anos', 'faixa_etaria', 'sexo_padrao', 'raca_padrao',",
    "    'trabalho_agro', 'OCUP', 'LOCOCOR'",
    "  ), names(dados_completos))",
    "  print(knitr::kable(dados_completos[, cols_exibir, drop = FALSE]))",
    "  cat('\\n</div>\\n')",
    "}",
    "```"
  )

  writeLines(linhas_rmd, con = rmd_temp, useBytes = TRUE)

    rmarkdown::render(
    input = rmd_temp,
    output_file = basename(html_saida),
    output_dir = dirname(html_saida),
      quiet = TRUE
    )
  
  unlink(rmd_temp)
  message("[ag-report]: Relatório HTML completo gerado em: ", html_saida)
  return(normalizePath(html_saida, mustWork = FALSE))
}