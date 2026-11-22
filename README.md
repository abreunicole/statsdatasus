# statsdatasus <img src="https://img.shields.io/badge/R-%3E%3D4.1.0-blue.svg" align="right" alt="R version" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Data Source: DATASUS](https://img.shields.io/badge/Data%20Source-DATASUS%20(SIM%20%7C%20SIH)-2E8B57.svg)](https://datasus.saude.gov.br/)
[![Epidemiology: STROBE](https://img.shields.io/badge/Standards-STROBE%20Compliant-blueviolet.svg)](https://www.strobe-statement.org/)
<!-- badges: end -->

> **Automação epidemiológica modular para extração, higienização, síntese estatística e geração de relatórios a partir de microdados do DATASUS (SIM-DO e SIH-RD).**

---

## 📋 Visão Geral

O **`statsdatasus`** é um pacote R desenvolvido para padronizar e acelerar pipelines de pesquisa observacional e vigilância epidemiológica com dados secundários do Sistema Único de Saúde (SUS)[cite: 1, 2]. 

Projetado sob uma arquitetura de **agentes modulares desacoplados**, o núcleo do pacote fornece ferramentas universais de engenharia de dados (compatíveis com qualquer agravo da CID-10 e qualquer grande grupo ocupacional da CBO), permitindo que regras clínicas e recortes geográficos específicos fiquem isolados nos scripts de cada pesquisa.

### ✨ Principais Funcionalidades

- **Extração Resiliente (`ag_fetch_datasus`):** Conexão automatizada aos servidores do DATASUS via [`microdatasus`](https://github.com/rfsaldanha/microdatasus), com suporte integrado ao Sistema de Informações sobre Mortalidade (`SIM-DO`) e Sistema de Informações Hospitalares (`SIH-RD`) e tolerância estendida de *timeout*.
- **Higienização Universal (`ag_clean_datasus`):** Padronização vetorizada de códigos CID-10 (`CAUSABAS`, `DIAG_PRINC`), higienização de ocupações (`OCUP`, `CBOR`), extração dinâmica do ano do evento, decodificação nativa da estrutura de idades do SIM/SIH (incluindo registros `< 1 ano` e centenários) e marcadores opcionais via expressões regulares (`cid_pattern`, `cbo_prefix`)[cite: 1, 2].
- **Síntese Estatística Offline (`ag_stats_interpret`):** Geração automatizada de pareceres descritivos a partir de tabelas de contingência e distribuições de frequência, sem dependência de APIs externas.
- **Relatórios Reprodutíveis (`ag_report_generate`):** Compilação dinâmica de documentos R Markdown (`.Rmd`) em relatórios visuais `.html` autocontidos[cite: 2].

---

## 🏗️ Arquitetura do Pipeline

```text
┌──────────────────────┐     ┌──────────────────────┐     ┌──────────────────────┐     ┌──────────────────────┐
│  1. INGESTÃO         │     │  2. HIGIENIZAÇÃO     │     │  3. SÍNTESE          │     │  4. RELATÓRIO        │
│  ag_fetch_datasus()  │ ──▶ │  ag_clean_datasus()  │ ──▶ │ ag_stats_interpret() │ ──▶ │ ag_report_generate() │
└──────────────────────┘     └──────────────────────┘     └──────────────────────┘     └──────────────────────┘
   SIM-DO / SIH-RD              Padronização CID/CBO         Tabelas & Resumo             Relatório HTML + CSV
   (microdatasus)               Idade, Faixa & Ano           Descritivo Offline           (knitr / rmarkdown)
