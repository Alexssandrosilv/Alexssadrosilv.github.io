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
