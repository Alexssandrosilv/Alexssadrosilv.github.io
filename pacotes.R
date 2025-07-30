# --- ETAPA 0: PACOTES E FUNÇÕES ---
#| Este primeira parte do script configura o ambiente com as 
#| bibliotecas (pacotes) utilizados neste trabalho
#|
#| 1. Bibliotecas ausentes são instaladas automaticamente.
#| 2. Todas as bibliotecas são carregadas de forma eficiente.
#| 3. Cada biblioteca é comentada para facilitar a consulta.

# Define o repositório CRAN para evitar erro ao instalar pacotes
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Lista de pacotes necessários, organizados por funcionalidade
pacotes <- c(
  # Manipulação e organização de dados
  "magrittr",      # Pipe (%>%) alternativo ao |> base
  "dplyr",         # Manipulação de dados (filter, select, mutate, etc.)
  "tidyr",         # Transformações como pivot_longer(), separate(), etc.
  "tibble",        # Versão moderna dos data.frames
  "readr",         # Leitura eficiente de arquivos CSV
  "readxl",        # Leitura de arquivos Excel (.xls e .xlsx)
  "gsheet",        # Leitura de planilhas Google Sheets
  "lubridate",     # Manipulação de datas
  "hms",           # Manipulação de tempo (horas, minutos, segundos)
  "zoo",           # Séries temporais irregulares
  "reshape2",      # Transformação de dados com melt() e dcast() [⚠️ obsoleto]
  "stringr",       # Manipulação de strings
  "stringi",       # Manipulação avançada de strings
  
  # Estatísticas descritivas e psicometria
  "skimr",         # Sumários estatísticos rápidos
  "psych",         # Psicometria e estatísticas descritivas
  "Hmisc",         # Frequências, imputação, descrição
  "MASS",          # Funções clássicas (ex: lda, glm.nb)
  "MCMCpack",      # Modelos Bayesianos via MCMC
  
  # Testes estatísticos e normalidade
  # "BiocManager",   # Análise multivariada
  "MVN",           # Normalidade multivariada
  "RVAideMemoire", # Testes estatísticos diversos
  "nortest",       # Testes de normalidade univariada
  "hnp",           # QQ plot com bandas de confiança
  "car",           # Ferramentas para regressão e gráficos
  
  # Testes não paramétricos
  "ggpubr",        # Gráficos estatísticos prontos para publicação
  "agricolae",     # Testes não paramétricos como Kruskal-Wallis
  "FSA",           # Dunn’s test para comparações múltiplas
  "PMCMRplus",     # Testes pós‑hoc não paramétricos
  
  # Visualização de dados
  "ggplot2",       # Sistema gráfico baseado em camadas
  "ggtext",        # Estilização de texto em gráficos ggplot
  "corrplot",      # Visualização de matrizes de correlação
  "qgraph",        # Visualização de redes e relações complexas
  "showtext",      # Fontes personalizadas no ggplot2
  
  # Criação e personalização de tabelas
  "flextable",     # Tabelas para relatórios Word/HTML
  "kableExtra",    # Extensões visuais para kable()
  "reactable",     # Tabelas interativas em HTML
  "fdth",          # Tabelas de frequência
  "sjPlot",        # Tabelas e gráficos de modelos estatísticos
  "htmltools",     # Suporte para HTML em documentos
  
  # Geração de relatórios
  "officer",       # Geração de documentos Word, PowerPoint
  "knitr",         # Relatórios dinâmicos com R Markdown
  
  # Coleção integrada de pacotes (engloba vários acima)
  "tidyverse"      # Conjunto de pacotes para ciência de dados
)

# Verifica e instala pacotes ausentes
pacotes_nao_instalados <- pacotes[!pacotes %in% installed.packages()]
if (length(pacotes_nao_instalados) > 0) {
  invisible(install.packages(pacotes_nao_instalados, dependencies = TRUE))
}

# Carrega todos os pacotes de forma silenciosa
invisible(sapply(pacotes, require, character.only = TRUE))

