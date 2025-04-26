
library(RCurl)
library(XML)
library(stringr)
library(tm)
library(topicmodels)
library(httr)
library(jsonlite)
library(dplyr)
library(httr)
library(rvest)
library(stringr)



url <- "https://www.gov.uk/government/announcements?announcement_type_option=press-releases&departments[]=all&from_date=&keywords=&to_date=01%2F07%2F2010&topics[]=all&world_locations[]=all&page=1"
resp <- GET(url)

# Verificar si la respuesta fue correcta
if (status_code(resp) == 200) {
  html <- read_html(resp)
  
  # Filtrar solo los enlaces de los comunicados de prensa
  press_links <- html %>%
    html_nodes("li a[href*='/government/news']") %>%   
    html_attr("href")
  
  # Mostrar los primeros 10 enlaces de comunicados de prensa
  if (length(press_links) > 0) {
    cat("Enlaces encontrados:\n")
    print(head(press_links, 10))
  } else {
    cat("No se encontraron enlaces de comunicados de prensa.\n")
  }
} else {
  cat("Error al cargar la página\n")
}


#Directorio para guardar los comunicados
dir.create("Press_Releases", showWarnings = FALSE)

# Base URL
base_url <- "https://www.gov.uk"

# Descargar cada comunicado de prensa
conflicts(detail = TRUE)
for (i in 1:length(press_links)) {
  # Verifica si el enlace ya incluye la base del URL
  if (substr(press_links[i], 1, 1) == "/") {
    full_url <- paste0(base_url, press_links[i])
  } else {
    full_url <- press_links[i]
  }
  
  cat("Descargando comunicado:", full_url, "\n")
  
  resp <- try(httr::GET(full_url), silent = TRUE)
  
  if (inherits(resp, "try-error") || httr::status_code(resp) != 200) {
    cat("Error al acceder al comunicado", i, "\n")
    next
  }
  
  # Verificar tipo de contenido
  if (httr::http_type(resp) != "text/html") {
    cat("El contenido no es HTML para el comunicado", i, "\n")
    next
  }
  
  # Leer el contenido HTML de la página
  html <- httr::content(resp, as = "text", encoding = "UTF-8")
  
  # Guardar el  HTML en un archivo
  file_path <- file.path("Press_Releases", paste0(i, ".html"))
  writeLines(html, file_path)
}


# 
cat("Total de archivos descargados:", length(list.files("Press_Releases")), "\n")
list.files("Press_Releases")[1:3]

tmp <- readLines("Press_Releases/1.html",warn = FALSE)
tmp <- str_c(tmp, collapse = "")
tmp <- htmlParse(tmp)
parsed_html <- tmp

release <- xpathSApply(parsed_html, "//div[contains(@class,'govspeak')]", xmlValue)



# Extraer el contenido con XPath
title <- xpathSApply(parsed_html, "//title", xmlValue)
title_text <- ifelse(length(title) > 0, title, "No disponible")

# Este es el contenedor principal del texto del comunicado
release <- xpathSApply(parsed_html, "//div[contains(@class,'govspeak')]", xmlValue)
release_text <- ifelse(length(release) > 0, release, "No disponible")

# Extraer la fecha 
publication_date <- xpathSApply(parsed_html, "//meta[@name='pubdate']", xmlGetAttr, "content")
if (length(publication_date) == 0) {
  fecha_raw <- xpathSApply(parsed_html, "//div[contains(@class,'metadata') or contains(@class,'meta-data')]//text()", xmlValue)
  fecha_raw <- paste(fecha_raw, collapse = " ")
  fecha_extraida <- str_extract(fecha_raw, "Published\\s+\\d{1,2}\\s+\\w+\\s+\\d{4}")
  publication_date <- ifelse(!is.na(fecha_extraida), str_remove(fecha_extraida, "Published\\s+"), "No disponible")
}
date_text <- ifelse(length(publication_date) > 0, publication_date, "No disponible")


# Extraer la organización (si aparece)
organisation <- xpathSApply(parsed_html, "//*[contains(@class,'organisation')]", xmlValue)
org_text <- ifelse(length(organisation) > 0, organisation[1], "No disponible")


cat(xpathSApply(parsed_html, "//body", xmlValue))

#Mostrar resultados
cat("Título:", title, "\n")
cat("Fecha de publicación:", ifelse(length(publication_date) > 0, publication_date, "No disponible"), "\n")
cat("Organización:", ifelse(length(organisation) > 0, organisation, "No disponible"), "\n")
cat("Fragmento del contenido:\n", substr(release, 1, 300), "\n")

release_corpus <- VCorpus(VectorSource(c(release)))

meta(release_corpus[[1]], "organisation") <- org_text
meta(release_corpus[[1]], "publication_date") <- date_text

# Verificá
meta(release_corpus[[1]])




# Obtener la lista de archivos HTML reales
files <- list.files("Press_Releases/", pattern = "\\.html$", full.names = TRUE)


n <- 0
for (i in seq_along(files)) {
  cat("Procesando:", files[i], "\n")
  
  tmp <- try(readLines(files[i], warn = FALSE), silent = TRUE)
  if (inherits(tmp, "try-error")) next
  
  tmp <- paste(tmp, collapse = "")
  parsed <- try(htmlParse(tmp, asText = TRUE), silent = TRUE)
  if (inherits(parsed, "try-error")) next
  
  release <- xpathSApply(parsed, "//div[contains(@class, 'govspeak')]", xmlValue)
  organisation <- xpathSApply(parsed, "//span[@class='organisation lead']", xmlValue)
  publication <- xpathSApply(parsed, "//dd[@class='change-notes']", xmlValue)
  
  if (length(release) != 0) {
    n <- n + 1
    tmp_corpus <- VCorpus(VectorSource(release))
    
    # Agregar metadatos de forma segura
    org_text <- if (length(organisation) > 0) organisation[1] else "Desconocida"
    pub_text <- if (length(publication) > 0) publication[1] else "No disponible"
    
    meta(tmp_corpus[[1]], "organisation") <- org_text
    meta(tmp_corpus[[1]], "publication") <- pub_text
    meta(tmp_corpus[[1]], "file") <- basename(files[i])
    
    # Concatenar al corpus general
    if (n == 1) {
      release_corpus <- tmp_corpus
    } else {
      release_corpus <- c(release_corpus, tmp_corpus)
    }
  }
}

release_corpus



# Extraer metadatos del corpus
meta_data <- data.frame(
  organisation = sapply(release_corpus, function(doc) {
    org <- meta(doc, "organisation")
    if (length(org) == 0) NA else org
  }),
  publication = sapply(release_corpus, function(doc) {
    pub <- meta(doc, "publication")
    if (length(pub) == 0) NA else pub
  }),
  stringsAsFactors = FALSE
)

# Mostrar los primeros registros para verificar
head(meta_data)


table(as.character(meta_data[, "organisation"]))




tdm <- TermDocumentMatrix(release_corpus)
tdm



release_corpus <- tm_map(release_corpus, removeNumbers)

release_corpus <- tm_map(release_corpus, content_transformer(function(x) {
  str_replace_all(x, pattern = "[[:punct:]]", replacement = " ")
}))

length(stopwords("en"))
stopwords("en")[1:10]
release_corpus <- tm_map(release_corpus, removeWords, words =
                           stopwords("en"))




 
release_corpus <- Corpus(VectorSource(release_corpus))

release_corpus <- tm_map(release_corpus, content_transformer(tolower))  
release_corpus <- tm_map(release_corpus, removeWords, stopwords("en"))  
release_corpus <- tm_map(release_corpus, content_transformer(stemDocument))  

release_corpus <- tm_map(release_corpus, content_transformer(function(x) {  
  stringr::str_replace_all(x, "[[:punct:]]", " ")  
}))  


class(release_corpus)  
tdm <- TermDocumentMatrix(release_corpus)  

# Si release_corpus es character, reconstrúyelo  
if (is.character(release_corpus)) {  
  release_corpus <- Corpus(VectorSource(release_corpus))  
}  

# Aplica transformaciones  
release_corpus <- tm_map(release_corpus, content_transformer(tolower))  
release_corpus <- tm_map(release_corpus, removeWords, stopwords("en"))  
release_corpus <- tm_map(release_corpus, content_transformer(stemDocument))  

# Crea la matriz término-documento  
tdm <- TermDocumentMatrix(release_corpus)  
tdm



tdm <- removeSparseTerms(tdm, 0.95)
tdm



BigramTokenizer <- function(x){
      NGramTokenizer(x, Weka_control(min = 2, max = 2))}
  tdm_bigram <- TermDocumentMatrix(release_corpus,
                                    control = list(
                                      tokenize =
                                        BigramTokenizer))

  tdm_bigram
  
  
  findAssocs(tdm, "nuclear", .7)
  
  
  

  
  
##10.3.5  
    
  # Create Document-Term Matrix
  dtm <- DocumentTermMatrix(release_corpus)
  n_docs <- length(release_corpus)
  max_terms_to_keep <- 10  
  safe_sparse <- ifelse(n_docs <= max_terms_to_keep, 0.95, 1-(max_terms_to_keep/n_docs))
  dtm_reduced <- removeSparseTerms(dtm, safe_sparse)
  dtm_reduced
  
  
  library(RTextTools)
  dummy_labels <- rep(c("Grupo1", "Grupo2"), length.out = nrow(dtm_reduced))
  
  
  container <- create_container(
    dtm_reduced,  
    labels = dummy_labels,  
    trainSize = 1:nrow(dtm_reduced),
    testSize = NULL,
    virgin = FALSE
  )

  print_algorithms()
  
  
  svm_model <- train_model(container, "SVM")
  tree_model <- train_model(container, "TREE") 
  
  svm_out <- classify_model(container, svm_model)
  tree_out <- classify_model(container, tree_model)
  
  head(svm_out)  
  head(tree_out)
  
  
  
  labels_out <- data.frame(
    document_id = 401:nrow(dtm_reduced),
    svm = as.character(svm_out[,1]),
    tree = as.character(tree_out[,1]),
    stringsAsFactors = F)
  
  table(labels_out[,1] == labels_out[,2])
  
  prop.table(table(labels_out[,1] == labels_out[,2]))
  
  table(labels_out[,1] == labels_out[,3])
  
  prop.table(table(labels_out[,1] == labels_out[,3]))
  
  
 
  
  
  
  
 ### 
  

  library(topicmodels)
  num_topics <- 5
  lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))
 
  terms(lda_model, 10)
  topic_assignments <- topics(lda_model)
   
    



## Outputting rounded matrix
num_topics <- 5
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))

# top 10 palabras por tema
terms(lda_model, 10)  


# más probable a cada documento
topic_assignments <- topics(lda_model)

ctm_out <- CTM(dtm, 6)

terms(ctm_out, 10)









posterior_ctm <- posterior(ctm_out)
ctm_topics <- data.frame(t(posterior_ctm$topics))

par(mfrow = c(2, 2),  
    cex.main = 0.8, 
    pty = "s", 
    mar = c(5, 5, 1, 1))

for(topic in 1:2) {
  for(group in c("Grupo1", "Grupo2")) {
    # Get data for this topic-group combination
    tmp.data <- ctm_topics[topic, dummy_labels == group]
    
    # Only plot if there are documents in this group
    if(length(tmp.data) > 0) {
      plot(
        1:length(tmp.data),
        sort(as.numeric(tmp.data)),
        type = "l",
        ylim = c(0, 1),
        xlab = "Documentos (ordenados)",
        ylab = paste("Prob. Tema", topic),
        main = paste("Grupo:", group, "- Tema", topic),
        col = ifelse(group == "Grupo1", "blue", "red"),
        lwd = 2
      )
    }
  }
}













##==Termina parte del libro corregido












#===============================================================================

