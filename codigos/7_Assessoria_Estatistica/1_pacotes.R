## Parte 1: Bibliotecas

#| Este primeira parte do script configura o ambiente com as 
#| bibliotecas "pacotes" utilizados neste trabalho

# ------ 

#| 1.  Bibliotecas ausentes são instalados automaticamente.
#| 2.  Todos as bibliotecas são carregadas de forma eficiente.
#| 3.  Cada biblioteca é comentada para facilitar a consulta sobre seu uso e aplicabilidade.

# Lista de pacotes necessários, organizados por funcionalidade
pacotes <- c(
  # Manipulação e organização de dados
  "magrittr",      # Pipe (%>%) alternativo ao |> base
  "dplyr",         # Manipulação de dados (filter, select, mutate, etc.)
  "reshape2",      # Funções como melt() para transformação de dados
  "stringr",       # Manipulação de strings
  "stringi",       # Manipulação avançada de strings
  "readr",         # Leitura eficiente de arquivos CSV
  "readxl",        # Leitura de arquivos Excel
  "gsheet",        # Leitura de planilhas Google Sheets
  
  # Estatísticas descritivas e psicometria
  "skimr",         # Sumários estatísticos rápidos
  "psych",         # Psicometria e estatísticas descritivas
  "Hmisc",         # Frequências, imputação, descrição
  
  # Visualização de dados
  "ggplot2",       # Sistema de visualização gráfica
  "corrplot",      # Matriz de correlação
  "showtext",      # Fontes personalizadas para gráficos
  
  # Criação e personalização de tabelas
  "flextable",     # Tabelas para relatórios Word/HTML
  "fdth",          # Tabelas de frequência
  "htmltools",     # Exportação de HTML
  
  # Tabelas e relatórios
  "officer",        # Geração de documentos Word/PPTX
  "sjPlot",         # Tabelas apos a regressao
  "ggtext"  
)

pacotes_nao_instalados <- pacotes[!pacotes %in% rownames(installed.packages())]

# # Verifica e instala pacotes ausentes
# pacotes_nao_instalados <- pacotes[!pacotes %in% installed.packages()]
# if (length(pacotes_nao_instalados) > 0) {
#   install.packages(pacotes_nao_instalados, dependencies = TRUE)
# }

# Carrega todos os pacotes
sapply(pacotes, require, character.only = TRUE) 
