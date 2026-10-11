# Caminho dados

caminho_dados <- "dados/"

# Tratamento (base tratada)
tratar_dados_pesca_consolidado <- function(bd) {
  
  # A. Limpeza de espaços e tipagem correta das variáveis
  bd_padronizado <- bd |>
    dplyr::mutate(
      localidade       = base::as.factor(stringr::str_trim(localidade)),
      nome_comum       = base::as.character(stringr::str_trim(nome_comum)),
      ordem            = base::as.factor(stringr::str_trim(ordem)),
      familia          = base::as.factor(stringr::str_trim(familia)),
      especie          = base::as.character(stringr::str_trim(especie)),
      habitat          = base::as.factor(stringr::str_trim(habitat)),
      migration        = base::as.factor(stringr::str_trim(migration)),
      nivel_tropico    = base::as.factor(stringr::str_trim(nivel_tropico)),
      status_mercado   = base::as.factor(stringr::str_trim(status_mercado)),
      periodo          = base::as.character(stringr::str_trim(periodo)),
      periodo_2        = base::as.factor(stringr::str_trim(periodo_2)),
      dias             = base::as.numeric(dias)
    )
  
  # B. Conversão do formato WIDE para LONG
  colunas_fixas <- c("localidade", "nome_comum", "ordem", "familia", "especie", 
                     "habitat", "migration", "nivel_tropico", "status_mercado", 
                     "periodo", "periodo_2", "dias")
  
  bd_longo <- bd_padronizado |>
    tidyr::pivot_longer(
      cols = -tidyselect::any_of(colunas_fixas),
      names_to = "ano",
      values_to = "captura_total"
    ) |>
    
    # C. Tipagem e estruturação do impacto (Antes, Transição e Depois) sem CPUE
    dplyr::mutate(
      ano              = base::as.integer(ano),
      captura_total    = base::as.numeric(stringr::str_replace(base::as.character(captura_total), ",", ".")),
      
      periodo_barragem = dplyr::case_when(
        ano <= 2010 ~ "Antes",
        ano == 2011 ~ "Transição",
        ano >= 2012 ~ "Depois"
      ),
      periodo_barragem = base::factor(periodo_barragem, levels = c("Antes", "Transição", "Depois"))
    ) |>
    
    # D. Remove anos sem registo (NAs) e corta os dados anteriores a 2002
    dplyr::filter(!is.na(captura_total), ano >= 2002)
  
  return(bd_longo)
}

# Leitura base dados bruta
dados_longos <- readxl::read_excel(paste0(caminho_dados, "dados_tratados.xlsx")) |> 
  tratar_dados_pesca_consolidado()


# Tratamento (base de cotas tratada) - NAs Mantidos
tratar_dados_cotas <- function(bd) {
  
  bd_padronizado <- bd |> 
    dplyr::mutate(
      # A. Tipagem básica das variáveis de identificação e tempo
      localidade         = base::as.factor(stringr::str_trim(localidade)),
      estacao_codigo     = base::as.character(stringr::str_trim(estacao_codigo)),
      data               = base::as.Date(data),
      ano                = base::as.integer(ano),
      mes                = base::as.integer(mes),
      hora               = base::format(hora, "%H:%M:%S"),
      
      # B. Aplicação dos Rótulos (Dicionário de Dados da ANA)
      nivel_consistencia = base::factor(
        nivel_consistencia, 
        levels = base::c(1, 2), 
        labels = base::c("Bruto", "Consistido")
      ),
      
      media_diaria = base::factor(
        media_diaria, 
        levels = base::c(0, 1), 
        labels = base::c("Não", "Sim")
      ),
      
      tipo_medicao_cotas = base::factor(
        tipo_medicao_cotas, 
        levels = base::c(1, 2, 3, 5), 
        labels = base::c("Escala", "Linígrafo", "Data Logger", "Escala SMS")
      )
    ) |> 
    
    # C. Aplicação em massa dos rótulos para TODAS as colunas de status
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::ends_with("_status"),
        .fns  = ~ base::factor(
          .x, 
          levels = base::c(0, 1, 2, 3, 4), 
          labels = base::c("Branco", "Real", "Estimado", "Duvidoso", "Régua Seca")
        )
      )
    ) |> 
    
    # D. Transformação para o formato LONGO (Pivot)
    dplyr::rename_with(
      .fn = ~ stringr::str_replace(.x, "cota_(\\d{2})_status", "status_\\1"),
      .cols = tidyselect::ends_with("_status")
    ) |> 
    
    tidyr::pivot_longer(
      cols = tidyselect::matches("^(cota|status)_\\d{2}$"),
      names_to = base::c(".value", "referencia_dia"),
      names_sep = "_"
    ) |> 
    
    # E. Criação da variável identificadora
    dplyr::mutate(
      identificador_cota = base::as.factor(base::paste0("cota_", referencia_dia)),
      referencia_dia     = base::as.integer(referencia_dia)
    )
  
  # IMPORTANTE: O filtro dplyr::filter(!base::is.na(cota)) foi removido!
  
  return(bd_padronizado)
}

# Leitura base de dados de cotas
dados_cotas <- readxl::read_excel(paste0(caminho_dados, "dados_cotas_tratados.xlsx")) |> 
  tratar_dados_cotas()