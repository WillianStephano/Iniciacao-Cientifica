library(stringr)
library(syuzhet)
library(tidyr)
library(writexl)

# Carregar o texto do arquivo
arquivo <- "./Dias-Gomes-O-Pagador-de-Promessas.txt"
texto <- readLines(arquivo, warn = FALSE)

# Remover texto entre parênteses
remover_parenteses <- function(linha) {
  str_replace_all(linha, "\\([^\\)]*\\)", "")
}

# Função para separar as falas dos personagens
separar_falas <- function(texto) {
  falas_por_personagem <- list()
  personagem_atual <- NULL
  
  for (linha in texto) {
    linha_trim <- str_trim(linha)
    linha_sem_parenteses <- remover_parenteses(linha_trim)
    
    if (linha_sem_parenteses == "") {
      next
    }
    
    if (str_detect(linha_sem_parenteses, "^[A-ZÁÉÍÓÚÃÕÂÊÎÔÛÇ ]+$")) {
      personagem_atual <- linha_sem_parenteses
      
      if (!personagem_atual %in% names(falas_por_personagem)) {
        falas_por_personagem[[personagem_atual]] <- c()
      }
    } else if (!is.null(personagem_atual)) {
      falas_por_personagem[[personagem_atual]] <- c(falas_por_personagem[[personagem_atual]], linha_sem_parenteses)
    }
  }
  
  return(falas_por_personagem)
}

# Separar as falas dos personagens
falas <- separar_falas(texto)

# Função para calcular o número de manifestações de cada sentimento para cada personagem
calcular_contagem_sentimentos <- function(falas) {
  contagem_sentimentos <- lapply(falas, function(falas_personagem) {
    sentimento_personagem <- lapply(falas_personagem, function(fala) get_nrc_sentiment(fala, lang = "portuguese"))
    colSums(do.call(rbind, sentimento_personagem))
  })
  return(contagem_sentimentos)
}

# Calcular o número de manifestações de cada sentimento para cada personagem
contagem_sentimentos <- calcular_contagem_sentimentos(falas)

# Converter a lista em um dataframe
df_contagem_sentimentos <- as.data.frame(do.call(rbind, contagem_sentimentos))
df_contagem_sentimentos$Personagem <- rownames(df_contagem_sentimentos)
rownames(df_contagem_sentimentos) <- NULL

# Reorganizar o dataframe, excluindo "positive" e "negative"
sentimentos <- setdiff(colnames(df_contagem_sentimentos), c("Personagem", "positive", "negative"))
df_contagem_sentimentos_especificos <- df_contagem_sentimentos[, c("Personagem", sentimentos)]

# Transformar o dataframe em um formato longo
df_contagem_sentimentos_long <- pivot_longer(df_contagem_sentimentos_especificos, -Personagem, names_to = "Sentimento", values_to = "Contagem")

# Salvar os dados em um arquivo Excel na pasta atual
write_xlsx(list(
  SentimentosEspecificos = df_contagem_sentimentos_especificos,
  SentimentosPosNeg = df_contagem_sentimentos[, c("Personagem", "positive", "negative")]
), "contagem_sentimentos.xlsx")
