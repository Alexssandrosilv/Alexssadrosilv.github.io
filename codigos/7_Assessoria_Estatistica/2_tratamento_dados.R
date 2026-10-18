# ==============================================================================
# PARTE 2: CAMINHOS E FUNÇÕES DE TRATAMENTO DE DADOS
# ==============================================================================


# Caminho dos dados relativo à raiz do projeto/repositório
caminho_dados <- "dados/"


# A. Tratamento da base de desembarque pesqueiro
tratar_dados_pesca_consolidado <- function(bd) {
  
  colunas_fixas <- base::c("localidade", "nome_comum", "ordem", "familia", "especie", 
                           "habitat", "migration", "nivel_tropico", "status_mercado", 
                           "periodo", "periodo_2", "dias")
  
  bd_longo <- bd |>
    dplyr::mutate(
      localidade = base::as.factor(stringr::str_trim(localidade)),
      nome_comum = base::as.character(stringr::str_trim(nome_comum)) 
    ) |>
    tidyr::pivot_longer(
      cols = -tidyselect::any_of(colunas_fixas),
      names_to = "ano",
      values_to = "captura_total"
    ) |>
    dplyr::mutate(
      ano              = base::as.integer(ano),
      captura_total    = base::as.numeric(stringr::str_replace(base::as.character(captura_total), ",", ".")),
      
      # Criação do período da barragem
      periodo_barragem = dplyr::case_when(
        ano <= 2010 ~ "Antes",
        ano >= 2011 ~ "Depois"
      ),
      periodo_barragem = base::factor(periodo_barragem, levels = c("Antes","Depois"))
    ) |>
    # Remove anos sem registo e fixa a série de 2002 a 2025
    dplyr::filter(!base::is.na(captura_total), ano >= 2002, ano <= 2025) |>
    # Seleção final das variáveis
    dplyr::select(localidade, ano, periodo_barragem, nome_comum, captura_total)
  
  return(bd_longo)
}

# B. Tratamento da base de cotas fluviométricas
tratar_dados_cotas <- function(bd) {
  
  bd_padronizado <- bd |> 
    dplyr::mutate(
      localidade = base::trimws(localidade),
      localidade = dplyr::case_when(
        base::grepl("Borba", localidade, ignore.case = TRUE) ~ "Borba",
        base::grepl("Manic", localidade, ignore.case = TRUE) ~ "Manicoré",
        TRUE ~ localidade
      ),
      ano    = base::as.integer(ano),
      mes    = base::as.integer(mes),
      media  = base::as.numeric(media),
      minima = base::as.numeric(minima),
      maxima = base::as.numeric(maxima)
    ) |>
    # Filtra o período da tese (2002 a 2025)
    dplyr::filter(ano >= 2002, ano <= 2025) |> 
    
    # Remove duplicatas mensais/horárias da base original da ANA
    dplyr::distinct(localidade, ano, mes, media, minima, maxima, .keep_all = FALSE) |>
    
    # Criação do período da barragem para permitir cruzamento
    dplyr::mutate(
      periodo_barragem = dplyr::case_when(
        ano <= 2010 ~ "Antes",
        ano >= 2011 ~ "Depois"
      ),
      periodo_barragem = base::factor(periodo_barragem, levels = c("Antes", "Depois"))
    ) |>
    
    # Seleção final das variáveis
    dplyr::select(localidade, ano, mes, periodo_barragem, media, minima, maxima)
  
  return(bd_padronizado)
}

# C. Agregação: cotas anuais (para os gráficos)
preparar_cotas_anual_grafico <- function(bd_cotas_limpa) {
  bd_cotas_limpa |>
    dplyr::filter(!is.na(media)) |>
    dplyr::mutate(
      periodo_barragem = dplyr::case_when(
        ano < 2011 ~ "Before",
        ano >= 2011 & ano <= 2024 ~ "After",
        TRUE ~ "Reference" # Para o ano de 2025
      ),
      periodo_barragem = base::factor(periodo_barragem, levels = c("Before", "After", "Reference"))
    ) |>
    dplyr::group_by(localidade, ano, periodo_barragem) |>
    dplyr::summarise(
      cota_anual = base::mean(media, na.rm = TRUE),
      .groups = "drop"
    )
}

# ==============================================================================
# PARTE 3: EXECUÇÃO DA LEITURA E APLICAÇÃO
# ==============================================================================

# Leitura e tratamento da base de pesca
dados_desembarque <- readxl::read_excel(base::file.path(caminho_dados, "dados_desembarque_tratados_lucas.xlsx")) |> 
  tratar_dados_pesca_consolidado()

# Leitura e tratamento da base de cotas
dados_cotas <- readxl::read_excel(base::file.path(caminho_dados, "dados_cotas_tratados.xlsx")) |> 
  tratar_dados_cotas()

# Base agregada pronta para os gráficos anuais de cotas
dados_cotas_anual <- preparar_cotas_anual_grafico(dados_cotas)

base::message("Bases tratadas executadas com sucesso!")