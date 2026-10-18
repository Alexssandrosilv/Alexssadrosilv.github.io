# ==============================================================================
# PARTE 1: CONFIGURAÇÃO DE AMBIENTE E BIBLIOTECAS
# ==============================================================================

# Lista de pacotes necessários sincronizada com o GitHub Actions
pacotes <- c(
  # Renderização e relatórios
  "knitr",
  "rmarkdown",
  "officer",       # Geração de documentos Word/PPTX
  "sjPlot",        # Tabelas após a regressão
  "flextable",     # Tabelas para relatórios Word/HTML
  "fdth",          # Tabelas de frequência
  "htmltools",     # Exportação de HTML
  "reactable",     # Tabelas interativas
  "DT",            # Tabelas interativas DataTables
  "reticulate",    # Integração com Python
  
  # Manipulação e organização de dados
  "tidyverse",     # Meta-pacote
  "magrittr",      # Pipe (%>%) alternativo ao |> base
  "dplyr",         # Manipulação de dados (filter, select, mutate, etc.)
  "tidyr",         # Transformação de dados (pivot_longer, pivot_wider)
  "reshape2",      # Funções como melt() para transformação de dados
  "stringr",       # Manipulação de strings
  "stringi",       # Manipulação avançada de strings
  "readr",         # Leitura eficiente de arquivos CSV
  "readxl",        # Leitura de arquivos Excel
  "gsheet",        # Leitura de planilhas Google Sheets
  
  # Estatísticas descritivas, psicometria e modelagem
  "skimr",         # Sumários estatísticos rápidos
  "psych",         # Psicometria e estatísticas descritivas
  "Hmisc",         # Frequências, imputação, descrição
  "trend",         # Testes não-paramétricos de tendência (Sen's Slope)
  "nlme",          # Modelos lineares generalizados (GLS) e mistos
  "lme4",          # Modelos lineares e generalizados de efeitos mistos
  "car",           # Funções de apoio à regressão
  "rstatix",       # Testes estatísticos compatíveis com pipe
  
  # Visualização de dados
  "ggplot2",       # Sistema de visualização gráfica
  "corrplot",      # Matriz de correlação
  "showtext",      # Fontes personalizadas para gráficos
  "ggtext",        # Formatação rica de texto no ggplot2
  "geomtextpath",  # Textos curvados acompanhando linhas no ggplot2
  "viridis",       # Paletas de cores acessíveis
  "ggrepel"        # Rótulos de texto sem sobreposição
)

# Verifica e instala pacotes ausentes (com repositório CRAN fixo para evitar erro no GitHub)
pacotes_nao_instalados <- pacotes[!pacotes %in% utils::installed.packages()[, "Package"]]
if (length(pacotes_nao_instalados) > 0) {
  utils::install.packages(
    pacotes_nao_instalados, 
    dependencies = TRUE, 
    repos = "https://cloud.r-project.org"
  )
}

# Carrega todos os pacotes silenciosamente
invisible(lapply(pacotes, library, character.only = TRUE))