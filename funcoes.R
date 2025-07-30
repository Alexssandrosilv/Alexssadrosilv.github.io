## Funções Manuais

# Este script contém a definição de funções manuais personalizadas para 
# manipulação e padronização de dados, utilizadas neste trabalho.

# 1. As funções implementadas têm o objetivo de facilitar a padronização de variáveis 
#    categóricas, conversão de valores binários e ajustes na formatação de dados.

# 2. Cada função é projetada para ser reutilizada ao longo do processo de análise,
#    garantindo consistência nos dados.

# 3. As funções são comentadas de forma detalhada para proporcionar compreensão
#    sobre seu propósito e como usá-las corretamente em diferentes cenários.

# Este conjunto de funções tem como objetivo otimizar o processo de limpeza e preparação dos dados para análise.


# Função para padronizar categorias (removendo acentos e uniformizando)
padronizar_nomes <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")   # Remove acentos 
  x <- tolower(x)   # Deixa todas as letras minúsculas antes
  x <- stringr::str_to_title(x)   # Primeira letra de cada palavra maiúscula
}

# Funçaõ para apdronizar os nomes das variaveis
nomes_var <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")   # Remove acentos
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))  # Corrigido o erro aqui
}


# funcao para limpar/padronizar variaveis
limpar_variavel <- function(x) {
  x <- str_trim(x)  # remove espaços no início/fim
  x <- stri_trans_general(x, "Latin-ASCII")  # remove acentos
  x <- tolower(x)  # coloca tudo em minúsculo para padronizar comparações
  
  # Substitui valores "sujos" por NA
  x[x %in% c("", "x", "-", "nao lembra", "nao recorda", "nao lembra o nome", "nao recorda o nome", "nao apresenta")] <- NA
  
  # Padroniza visualmente: título (1ª letra maiúscula), depois de limpar
  x <- str_to_title(x)
  
  # Retorna como fator
  return(factor(x))
}

############################################################################################

# Função para criar tabelas de frequência para todas as variáveis do data.frame fornecido

tabelas_de_frequencia <- function(dados, 
                                  titulo = "Tabela de Frequência", 
                                  arquivo_html = NULL,
                                  arquivo_word = NULL) {
  library(flextable)
  library(htmltools)
  library(officer)  # Para exportar para Word
  
  if (is.null(arquivo_html)) arquivo_html <- "tabelas_de_frequencia.html"
  
  lista <- list()
  
  for (i in seq_along(dados)) {
    nome <- names(dados)[i]
    
    # Apenas fatores ou caracteres
    if (!is.factor(dados[[i]]) && !is.character(dados[[i]])) next
    
    # Se todos os valores são NA ou não há categorias, pula
    if (all(is.na(dados[[i]])) || length(unique(na.omit(dados[[i]]))) == 0) {
      message(paste("Variável", nome, "não possui categorias válidas e será ignorada."))
      next
    }
    
    # Frequências incluindo NA
    freq_abs <- table(dados[[i]], useNA = "ifany")
    categorias <- names(freq_abs)
    
    # Substitui NA visível por "Dados Ausentes (NA's)"
    categorias[is.na(categorias) | categorias == "<NA>"] <- "Dados Ausentes (NA's)"
    
    freq_rel <- prop.table(freq_abs) * 100
    
    tabela <- data.frame(
      Categoria = categorias,
      Frequência = as.numeric(freq_abs),
      `Frequência Relativa (%)` = paste0(round(freq_rel, 2), " %"),
      check.names = FALSE
    )
    
    # Linha título (identificação da variável)
    linha_titulo <- data.frame(
      Variável = nome,
      Categoria = NA,
      Frequência = NA,
      `Frequência Relativa (%)` = NA,
      check.names = FALSE
    )
    
    # Adiciona coluna Variável (vazia nas linhas normais)
    tabela$Variável <- NA
    tabela <- tabela[, c("Variável", "Categoria", "Frequência", "Frequência Relativa (%)")]
    linha_titulo <- linha_titulo[, c("Variável", "Categoria", "Frequência", "Frequência Relativa (%)")]
    
    # Empilha título + tabela
    lista[[length(lista) + 1]] <- rbind(linha_titulo, tabela)
  }
  
  if (length(lista) == 0) {
    stop("Nenhuma variável categórica válida para processar.")
  }
  
  # Junta tudo
  resultado <- do.call(rbind, lista)
  rownames(resultado) <- NULL
  
  # ✅ Ajusta o nome da coluna Frequência para incluir n total
  total_obs <- nrow(dados)
  colnames(resultado)[colnames(resultado) == "Frequência"] <- paste0("Frequência (n = ", total_obs, ")")
  
  # Flextable base
  ft <- flextable(resultado) %>%
    set_caption(titulo) %>%
    bold(part = "header") %>%
    align(part = "all", align = "center") %>%
    align(j = "Categoria", align = "left") %>%
    align(j = "Variável", align = "left") %>%
    autofit()
  
  # Lista das variáveis
  variaveis <- unique(resultado$Variável[!is.na(resultado$Variável)])
  
  # Borda superior e inferior para o cabeçalho
  ft <- border(ft, part = "header", border.top = fp_border(width = 1.5, color = "black"))
  ft <- border(ft, part = "header", border.bottom = fp_border(width = 1.5, color = "black"))
  
  # Borda superior na primeira categoria de cada variável
  for (variavel in variaveis) {
    primeiro_indice <- min(which(resultado$Variável == variavel))
    ft <- border(ft, i = primeiro_indice, part = "body",
                 border.top = fp_border(width = 1.2, color = "black"))
  }
  
  # Modificando a última linha com borda inferior
  ft <- border(ft, i = nrow(resultado), part = "body", 
               border.bottom = fp_border(width = 1.5, color = "black"))
  
  # Fonte para o título da tabela com cor azul escuro e negrito
  ft <- set_caption(
    ft,
    as_paragraph(
      as_chunk(
        titulo, 
        props = fp_text_default(font.family = "Cambria", color = "darkblue", bold = TRUE)
      )
    ), 
    word_stylename = "Table Caption"
  )
  
  # Salva HTML e abre no navegador
  htmltools::save_html(htmltools_value(ft), file = arquivo_html)
  browseURL(arquivo_html)
  message("✅ Tabelas de frequência geradas em HTML:", arquivo_html)
  
  # Se quiser exportar para Word
  if (!is.null(arquivo_word)) {
    # Se passar apenas TRUE, gera nome padrão
    if (identical(arquivo_word, TRUE)) arquivo_word <- "tabelas_de_frequencia.docx"
    
    doc <- read_docx()
    doc <- body_add_par(doc, titulo, style = "heading 1")
    doc <- body_add_flextable(doc, ft)
    print(doc, target = arquivo_word)
    message("✅ Tabelas também exportadas para Word:", arquivo_word)
  }
  
  return(ft)
}

gera_heatmap <- function(df, col_eixo_x, col_eixo_y, titulo, fonte = "Roboto") {
  library(ggplot2)
  library(dplyr)
  library(reshape2)
  library(ggtext)  # <- para rich text nos labels
  
  # --- Função auxiliar para deixar os labels mais bonitos ---
  formata_label <- function(nome) {
    nome %>%
      gsub("_", " ", .) %>%
      tools::toTitleCase()
  }
  
  # --- Se for fornecida uma fonte, registra via Google Fonts ---
  if (!is.null(fonte)) {
    suppressMessages({
      if (!requireNamespace("showtext", quietly = TRUE)) {
        install.packages("showtext")
      }
      library(showtext)
      font_add_google(fonte, fonte)
      showtext_auto()
    })
  }
  
  # --- Pega apenas combinações únicas por ID ---
  eixo_x <- df %>% filter(!is.na(.data[[col_eixo_x]])) %>% distinct(ID, .data[[col_eixo_x]])
  eixo_y <- df %>% filter(!is.na(.data[[col_eixo_y]])) %>% distinct(ID, .data[[col_eixo_y]])
  
  # --- Produto cartesiano por ID ---
  combinado <- inner_join(eixo_x, eixo_y, by = "ID")
  
  # --- Tabela de frequência → formato longo ---
  tab_freq <- table(combinado[[col_eixo_y]], combinado[[col_eixo_x]]) %>% reshape2::melt()
  colnames(tab_freq) <- c(col_eixo_y, col_eixo_x, "Frequencia")
  
  # --- Captura os níveis originais ---
  niveis_x <- sort(unique(tab_freq[[col_eixo_x]]))
  niveis_y <- sort(unique(tab_freq[[col_eixo_y]]))
  
  # Cor do número (mesma do título dos eixos)
  cor_numero <- "#880d1e"
  
  # --- Cria rótulos enumerados com número colorido ---
  niveis_x_enum <- paste0("<span style='color:", cor_numero, "'>", seq_along(niveis_x), ".</span> ", niveis_x)
  niveis_y_enum <- paste0("<span style='color:", cor_numero, "'>", seq_along(niveis_y), ".</span> ", niveis_y)
  
  # --- Mapear os valores originais para os enumerados ---
  tab_freq[[col_eixo_x]] <- factor(tab_freq[[col_eixo_x]], levels = niveis_x, labels = niveis_x_enum)
  tab_freq[[col_eixo_y]] <- factor(tab_freq[[col_eixo_y]], levels = niveis_y, labels = niveis_y_enum)
  
  # --- Mapa de calor ---
  ggplot(tab_freq, aes_string(x = col_eixo_x, y = col_eixo_y, fill = "Frequencia")) +
    geom_tile(color = "gray80") +
    
    coord_fixed(ratio = 0.7) +   
    
    geom_text(aes(label = ifelse(Frequencia > 0, Frequencia, "")), 
              size = 4, color = "black") +
    
    scale_fill_gradient(low = "#fafaff", 
                        high = "#f72585",
                        guide = guide_colorbar(
                          barheight = unit(100, "pt"),
                          barwidth  = unit(25,  "pt")
                        )) +
    
    theme_minimal(base_size = 12, base_family = ifelse(is.null(fonte), "", fonte)) +
    theme(
      legend.title = element_text(size = 15),
      legend.box.margin = margin(l = 15),
      legend.margin     = margin(t = 20),
      legend.spacing    = unit(6, "pt"),
      legend.text  = element_text(size = 13),
      
      # Aqui mudamos para markdown para interpretar HTML
      axis.text.x = ggtext::element_markdown(angle = 45, 
                                             hjust = 1, 
                                             color = "#1A1A1A",
                                             size = 13),
      axis.text.y = ggtext::element_markdown(size = 13, 
                                             color = "#1A1A1A"),
      
      axis.title.x = element_text(size = 14,
                                  color = cor_numero,
                                  face = "bold",
                                  margin = margin(t = 15)),
      axis.title.y = element_text(size = 14,
                                  color = cor_numero,
                                  face = "bold",
                                  margin = margin(r = 15)),
      plot.title  = element_text(face = "bold", 
                                 size = 18,
                                 margin = margin(b = 15))
    ) +
    
    labs(
      title = titulo,
      x = formata_label(col_eixo_x),
      y = formata_label(col_eixo_y),
      fill = "Frequência"
    )
}

# gera_heatmap <- function(df, col_eixo_x, col_eixo_y, titulo, fonte = "Roboto") {
#   
#   # --- Função auxiliar para deixar os labels mais bonitos ---
#   formata_label <- function(nome) {
#     nome %>%
#       gsub("_", " ", .) %>%        # troca underline por espaço
#       tools::toTitleCase()         # capitaliza cada palavra
#   }
#   
#   # --- Se for fornecida uma fonte, registra via Google Fonts ---
#   if (!is.null(fonte)) {
#     suppressMessages({
#       if (!requireNamespace("showtext", quietly = TRUE)) {
#         install.packages("showtext")
#       }
#       library(showtext)
#       font_add_google(fonte, fonte)
#       showtext_auto()
#     })
#   }
#   
#   # --- Pega apenas combinações únicas por ID ---
#   eixo_x <- df %>% filter(!is.na(.data[[col_eixo_x]])) %>% distinct(ID, .data[[col_eixo_x]])
#   eixo_y <- df %>% filter(!is.na(.data[[col_eixo_y]])) %>% distinct(ID, .data[[col_eixo_y]])
#   
#   # --- Produto cartesiano por ID ---
#   combinado <- inner_join(eixo_x, eixo_y, by = "ID")
#   
#   # --- Tabela de frequência → formato longo ---
#   tab_freq <- table(combinado[[col_eixo_y]], combinado[[col_eixo_x]]) %>% reshape2::melt()
#   colnames(tab_freq) <- c(col_eixo_y, col_eixo_x, "Frequencia")
#   
#   # --- Mapa de calor ---
#   ggplot(tab_freq, aes_string(x = col_eixo_x, y = col_eixo_y, fill = "Frequencia")) +
#     geom_tile(color = "gray80") +
#     
#     # Proporção mais ajustada para não ficar largo demais
#     coord_fixed(ratio = 0.7) +   
#     
#     # Frequências escritas nos blocos
#     geom_text(aes(label = ifelse(Frequencia > 0, Frequencia, "")), 
#               size = 4, color = "black") +
#     
#     # Paleta de cores
#     scale_fill_gradient(low = "#fafaff", 
#                         high = "#f72585",
#                         guide = guide_colorbar(
#                           barheight = unit(100, "pt"),  # altura da barra
#                           barwidth  = unit(25,  "pt")   # largura da barra
#                         )) +
#     
#     # Tema minimalista com fonte personalizada (se definida)
#     theme_minimal(base_size = 12, base_family = ifelse(is.null(fonte), "", fonte)) +
#     theme(
#       legend.title = element_text(size = 15),  # tamanho do título da legenda
#       
#       # Espaço entre a legenda e o gráfico
#       legend.box.margin = margin(l = 15),   # margem à esquerda da legenda
#       legend.margin     = margin(t = 20),   # margem superior da legenda
#       legend.spacing    = unit(6, "pt"),     # espaçamento interno dos itens
#       legend.text  = element_text(size = 13),   # tamanho dos valores da legenda
#       
#       axis.text.x = element_text(angle = 90, 
#                                  hjust = 1, 
#                                  color = "#1A1A1A",
#                                  size = 13),
#       axis.text.y = element_text(size = 13, 
#                                  color = "#1A1A1A"),
#       
#       axis.title.x = element_text(size = 14,
#                                   color = "#880d1e",
#                                   face = "bold",
#                                   margin = margin(t = 15)),  # nome do eixo X
#       axis.title.y = element_text(size = 14,
#                                   color = "#880d1e",
#                                   face = "bold",
#                                   margin = margin(r = 15)),  # nome do eixo Y
#       plot.title  = element_text(face = "bold", 
#                                  size = 18,
#                                  margin = margin(b = 15))
#     ) +
#     
#     # Labels formatados bonitos
#     labs(
#       title = titulo,
#       x = formata_label(col_eixo_x),
#       y = formata_label(col_eixo_y),
#       fill = "Frequência"
#     )
# }
# 
# gera_heatmap <- function(df, col_eixo_x, col_eixo_y, titulo, fonte = "Roboto") {
#   
#   # --- Função auxiliar para deixar os labels mais bonitos ---
#   formata_label <- function(nome) {
#     nome %>%
#       gsub("_", " ", .) %>%
#       tools::toTitleCase()
#   }
#   
#   # --- Se for fornecida uma fonte, registra via Google Fonts ---
#   if (!is.null(fonte)) {
#     suppressMessages({
#       if (!requireNamespace("showtext", quietly = TRUE)) {
#         install.packages("showtext")
#       }
#       library(showtext)
#       font_add_google(fonte, fonte)
#       showtext_auto()
#     })
#   }
#   
#   # --- Pega apenas combinações únicas por ID ---
#   eixo_x <- df %>% filter(!is.na(.data[[col_eixo_x]])) %>% distinct(ID, .data[[col_eixo_x]])
#   eixo_y <- df %>% filter(!is.na(.data[[col_eixo_y]])) %>% distinct(ID, .data[[col_eixo_y]])
#   
#   # --- Produto cartesiano por ID ---
#   combinado <- inner_join(eixo_x, eixo_y, by = "ID")
#   
#   # --- Tabela de frequência → formato longo ---
#   tab_freq <- table(combinado[[col_eixo_y]], combinado[[col_eixo_x]]) %>% reshape2::melt()
#   colnames(tab_freq) <- c(col_eixo_y, col_eixo_x, "Frequencia")
#   
#   # --- Captura os níveis originais ---
#   niveis_x <- sort(unique(tab_freq[[col_eixo_x]]))
#   niveis_y <- sort(unique(tab_freq[[col_eixo_y]]))
#   
#   # --- Cria rótulos enumerados apenas para exibição ---
#   niveis_x_enum <- paste0(seq_along(niveis_x), ". ", niveis_x)
#   niveis_y_enum <- paste0(seq_along(niveis_y), ". ", niveis_y)
#   
#   # --- Mapear os valores originais para os enumerados ---
#   tab_freq[[col_eixo_x]] <- factor(tab_freq[[col_eixo_x]], levels = niveis_x, labels = niveis_x_enum)
#   tab_freq[[col_eixo_y]] <- factor(tab_freq[[col_eixo_y]], levels = niveis_y, labels = niveis_y_enum)
#   
#   # --- Mapa de calor ---
#   ggplot(tab_freq, aes_string(x = col_eixo_x, y = col_eixo_y, fill = "Frequencia")) +
#     geom_tile(color = "gray80") +
#     
#     coord_fixed(ratio = 0.7) +   
#     
#     geom_text(aes(label = ifelse(Frequencia > 0, Frequencia, "")), 
#               size = 4, color = "black") +
#     
#     scale_fill_gradient(low = "#fafaff", 
#                         high = "#f72585",
#                         guide = guide_colorbar(
#                           barheight = unit(100, "pt"),
#                           barwidth  = unit(25,  "pt")
#                         )) +
#     
#     theme_minimal(base_size = 12, base_family = ifelse(is.null(fonte), "", fonte)) +
#     theme(
#       legend.title = element_text(size = 15),
#       legend.box.margin = margin(l = 15),
#       legend.margin     = margin(t = 20),
#       legend.spacing    = unit(6, "pt"),
#       legend.text  = element_text(size = 13),
#       
#       axis.text.x = element_text(angle = 90, 
#                                  hjust = 1, 
#                                  color = "#1A1A1A",
#                                  size = 13),
#       axis.text.y = element_text(size = 13, 
#                                  color = "#1A1A1A"),
#       
#       axis.title.x = element_text(size = 14,
#                                   color = "#880d1e",
#                                   face = "bold",
#                                   margin = margin(t = 15)),
#       axis.title.y = element_text(size = 14,
#                                   color = "#880d1e",
#                                   face = "bold",
#                                   margin = margin(r = 15)),
#       plot.title  = element_text(face = "bold", 
#                                  size = 18,
#                                  margin = margin(b = 15))
#     ) +
#     
#     labs(
#       title = titulo,
#       x = formata_label(col_eixo_x),
#       y = formata_label(col_eixo_y),
#       fill = "Frequência"
#     )
# }

# gera_heatmap <- function(df, col_eixo_x, col_eixo_y, titulo) {
#   
#   # Pega apenas combinações únicas por ID
#   eixo_x <- df %>% filter(!is.na(.data[[col_eixo_x]])) %>% distinct(ID, .data[[col_eixo_x]])
#   eixo_y <- df %>% filter(!is.na(.data[[col_eixo_y]])) %>% distinct(ID, .data[[col_eixo_y]])
#   
#   # Produto cartesiano por ID
#   combinado <- inner_join(eixo_x, eixo_y, by = "ID")
#   
#   # Tabela de frequência → formato longo
#   tab_freq <- table(combinado[[col_eixo_y]], combinado[[col_eixo_x]]) %>% melt()
#   colnames(tab_freq) <- c(col_eixo_y, col_eixo_x, "Frequencia")
#   
#   # Mapa de calor
#   ggplot(tab_freq, aes_string(x = col_eixo_x, y = col_eixo_y, fill = "Frequencia")) +
#     geom_tile(color = "gray80") +
#     coord_fixed(ratio = 0.5) + #0.5   
#     geom_text(aes(label = ifelse(Frequencia > 0, Frequencia, "")), size = 3.5, color = "black") +
#     # scale_fill_gradient(low = "#f8f9fa", high = "#c1121f") +
#     scale_fill_gradient(low = "#fafaff", high = "#f72585") +
#     theme_minimal(base_size = 12) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#1A1A1A"),
#           axis.text.y = element_text(size = 9, color = "#1A1A1A")) +
#     labs(title = titulo, x = col_eixo_x, y = col_eixo_y, fill = "Frequência")
# }





# 
# # Função para criar tabelas de frequência para todas as variáveis do data.frame fornecido
# tabelas_de_frequencia <- function(dados, 
#                                   titulo = "Tabela de Frequência", 
#                                   arquivo_html = NULL,
#                                   arquivo_word = NULL) {
#   library(flextable)
#   library(htmltools)
#   library(officer)  # Para exportar para Word
#   
#   if (is.null(arquivo_html)) arquivo_html <- "tabelas_de_frequencia.html"
#   
#   lista <- list()
#   
#   for (i in seq_along(dados)) {
#     nome <- names(dados)[i]
#     
#     # Apenas fatores ou caracteres
#     if (!is.factor(dados[[i]]) && !is.character(dados[[i]])) next
#     
#     # Se todos os valores são NA ou não há categorias, pula
#     if (all(is.na(dados[[i]])) || length(unique(na.omit(dados[[i]]))) == 0) {
#       message(paste("Variável", nome, "não possui categorias válidas e será ignorada."))
#       next
#     }
#     
#     # Frequências incluindo NA
#     freq_abs <- table(dados[[i]], useNA = "ifany")
#     categorias <- names(freq_abs)
#     
#     # Substitui NA visível por "Dados Ausentes (NA's)"
#     categorias[is.na(categorias) | categorias == "<NA>"] <- "Dados Ausentes (NA's)"
#     
#     freq_rel <- prop.table(freq_abs) * 100
#     
#     tabela <- data.frame(
#       Categoria = categorias,
#       Frequência = as.numeric(freq_abs),
#       `Frequência Relativa (%)` = paste0(round(freq_rel, 2), " %"),
#       check.names = FALSE
#     )
#     
#     # Linha título (identificação da variável)
#     linha_titulo <- data.frame(
#       Variável = nome,
#       Categoria = NA,
#       Frequência = NA,
#       `Frequência Relativa (%)` = NA,
#       check.names = FALSE
#     )
#     
#     # Adiciona coluna Variável (vazia nas linhas normais)
#     tabela$Variável <- NA
#     tabela <- tabela[, c("Variável", "Categoria", "Frequência", "Frequência Relativa (%)")]
#     linha_titulo <- linha_titulo[, c("Variável", "Categoria", "Frequência", "Frequência Relativa (%)")]
#     
#     # Empilha título + tabela
#     lista[[length(lista) + 1]] <- rbind(linha_titulo, tabela)
#   }
#   
#   if (length(lista) == 0) {
#     stop("Nenhuma variável categórica válida para processar.")
#   }
#   
#   # Junta tudo
#   resultado <- do.call(rbind, lista)
#   rownames(resultado) <- NULL
#   
#   # Flextable base
#   ft <- flextable(resultado) %>%
#     set_caption(titulo) %>%
#     bold(part = "header") %>%
#     align(part = "all", align = "center") %>%
#     align(j = "Categoria", align = "left") %>%
#     align(j = "Variável", align = "left") %>%
#     autofit()
#   
#   # Lista das variáveis
#   variaveis <- unique(resultado$Variável[!is.na(resultado$Variável)])
#   
#   # Borda superior e inferior para o cabeçalho
#   # Aplica bordas ao cabeçalho (superior e inferior)
#   ft <- border(ft, part = "header", border.top = fp_border(width = 1.5, color = "black"))
#   ft <- border(ft, part = "header", border.bottom = fp_border(width = 1.5, color = "black"))
#   
#   # Borda superior na primeira categoria de cada variável
#   for (variavel in variaveis) {
#     primeiro_indice <- min(which(resultado$Variável == variavel))
#     ft <- border(ft, i = primeiro_indice, part = "body",
#                  border.top = fp_border(width = 1.2, color = "black"))
#   }
#   
#   # Modificando a última linha com borda inferior
#   ft <- border(ft, i = nrow(resultado), part = "body", 
#                border.bottom = fp_border(width = 1.5, color = "black"))
#   
#   # Fonte para o título da tabela com cor azul escuro e negrito
#   ft <- set_caption(
#     ft,
#     as_paragraph(
#       as_chunk(
#         titulo, 
#         props = fp_text_default(font.family = "Cambria", color = "darkblue", bold = TRUE)
#       )
#     ), 
#     word_stylename = "Table Caption"
#   )
#   
#   # Salva HTML e abre no navegador
#   htmltools::save_html(htmltools_value(ft), file = arquivo_html)
#   browseURL(arquivo_html)
#   message("✅ Tabelas de frequência geradas em HTML:", arquivo_html)
#   
#   # Se quiser exportar para Word
#   if (!is.null(arquivo_word)) {
#     # Se passar apenas TRUE, gera nome padrão
#     if (identical(arquivo_word, TRUE)) arquivo_word <- "tabelas_de_frequencia.docx"
#     
#     doc <- read_docx()
#     doc <- body_add_par(doc, titulo, style = "heading 1")
#     doc <- body_add_flextable(doc, ft)
#     print(doc, target = arquivo_word)
#     message("✅ Tabelas também exportadas para Word:", arquivo_word)
#   }
#   
#   return(ft)
# }


# 
# tabelas_de_frequencia <- function(dados, 
#                                   titulo = NULL, 
#                                   arquivo_html = NULL,
#                                   arquivo_word = NULL) {
#   library(flextable)
#   library(htmltools)
#   library(officer)
#   
#   # Definir padrões automáticos
#   if (is.null(titulo)) titulo <- "Tabelas de Frequência"
#   if (is.null(arquivo_html)) arquivo_html <- "tabelas_de_frequencia.html"
#   
#   lista <- list()
#   
#   for (i in seq_along(dados)) {
#     nome <- names(dados)[i]
#     
#     # Apenas fatores ou caracteres
#     if (!is.factor(dados[[i]]) && !is.character(dados[[i]])) next
#     
#     # Se todos os valores são NA ou não há categorias, pula
#     if (all(is.na(dados[[i]])) || length(unique(na.omit(dados[[i]]))) == 0) {
#       message(paste("Variável", nome, "não possui categorias válidas e será ignorada."))
#       next
#     }
#     
#     # Frequências incluindo NA
#     freq_abs <- table(dados[[i]], useNA = "ifany")
#     categorias <- names(freq_abs)
#     
#     # Substitui NA visível por "Dados Ausentes (NA's)"
#     categorias[is.na(categorias) | categorias == "<NA>"] <- "Dados Ausentes (NA's)"
#     
#     freq_rel <- prop.table(freq_abs) * 100
#     
#     tabela <- data.frame(
#       Categoria = categorias,
#       Frequência = as.numeric(freq_abs),
#       `Frequência Relativa (%)` = paste0(round(freq_rel, 2), " %"),
#       check.names = FALSE
#     )
#     
#     # Linha título (identificação da variável)
#     linha_titulo <- data.frame(
#       Variável = nome,
#       Categoria = NA,
#       Frequência = NA,
#       `Frequência Relativa (%)` = NA,
#       check.names = FALSE
#     )
#     
#     # Adiciona coluna Variável (vazia nas linhas normais)
#     tabela$Variável <- NA
#     tabela <- tabela[, c("Variável", "Categoria", "Frequência", "Frequência Relativa (%)")]
#     linha_titulo <- linha_titulo[, c("Variável", "Categoria", "Frequência", "Frequência Relativa (%)")]
#     
#     # Empilha título + tabela
#     lista[[length(lista) + 1]] <- rbind(linha_titulo, tabela)
#   }
#   
#   if (length(lista) == 0) {
#     stop("Nenhuma variável categórica válida para processar.")
#   }
#   
#   # Junta tudo
#   resultado <- do.call(rbind, lista)
#   rownames(resultado) <- NULL
#   
#   # Flextable base
#   ft <- flextable(resultado) %>%
#     set_caption(titulo) %>%
#     bold(part = "header") %>%
#     align(part = "all", align = "center") %>%
#     align(j = "Categoria", align = "left") %>%
#     align(j = "Variável", align = "left") %>%
#     autofit()
#   
#   # Lista das variáveis
#   variaveis <- unique(resultado$Variável[!is.na(resultado$Variável)])
#   
#   # Borda superior na primeira categoria de cada variável
#   for (variavel in variaveis) {
#     primeiro_indice <- min(which(resultado$Variável == variavel))
#     ft <- border(ft, i = primeiro_indice, part = "body",
#                  border.top = fp_border(width = 1.2, color = "black"))
#   }
#   
#   # 🔴 DESTACAR "Dados Ausentes (NA's)" em vermelho
#   indices_na <- which(resultado$Categoria == "Dados Ausentes (NA's)")
#   if (length(indices_na) > 0) {
#     ft <- color(ft, i = indices_na, j = "Categoria", color = "red")
#   }
#   
#   # Salva HTML e abre no navegador
#   htmltools::save_html(htmltools_value(ft), file = arquivo_html)
#   browseURL(arquivo_html)
#   message("✅ Tabelas de frequência geradas em HTML:", arquivo_html)
#   
#   # Se quiser exportar para Word
#   if (!is.null(arquivo_word)) {
#     if (identical(arquivo_word, TRUE)) arquivo_word <- "tabelas_de_frequencia.docx"
#     
#     doc <- read_docx()
#     doc <- body_add_par(doc, titulo, style = "heading 1")
#     doc <- body_add_flextable(doc, ft)
#     print(doc, target = arquivo_word)
#     message("✅ Tabelas também exportadas para Word:", arquivo_word)
#   }
#   
#   return(ft)
# }
