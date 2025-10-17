# term embeddings 

# get CHT terms

library(httr2)
library(jsonlite)
library(data.table)

endpoint <- "https://api.linkeddata.cultureelerfgoed.nl/datasets/thesauri/Cultuurhistorische-Thesaurus-CHT/sparql"  # vervang door jouw endpoint
 

query <- '
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?label WHERE {
  GRAPH <https://data.cultureelerfgoed.nl/term/id/cht/thesaurus> {
    ?concept a skos:Concept .
    ?concept skos:prefLabel ?label .
    FILTER(LANG(?label) = "nl")
  }
}
'

# Verstuur de query (POST is beter bij langere queries)
resp <- request(endpoint) %>%
  req_headers(Accept = "application/sparql-results+json") %>%
  req_body_form(query = query) %>%   # <-- let op: named arg
  req_perform()

# JSON → data.frame
parsed <- fromJSON(resp_body_string(resp))

resp <- request(endpoint) %>%
  req_headers(Accept = "application/sparql-results+json") %>%
  req_body_form(query = query) %>%
  req_perform()

# Parse JSON naar R-lijst
parsed <- resp_body_json(resp, simplifyVector = FALSE)

# Veilige converter van bindings naar data.frame
bindings_to_df_safe <- function(bindings) {
  if (is.null(bindings) || length(bindings) == 0) return(data.frame())
  # bepaal alle variabelen (kolomnamen)
  vars <- unique(unlist(lapply(bindings, names)))
  # bouw rijen
  rows <- lapply(bindings, function(b) {
    sapply(vars, function(v) {
      val <- b[[v]]
      if (is.null(val)) return(NA_character_)
      if (is.list(val) && !is.null(val$value)) return(as.character(val$value))
      if (is.atomic(val)) return(as.character(val))
      # fallback: try to coerce
      tryCatch(as.character(val), error = function(e) NA_character_)
    }, USE.NAMES = TRUE, simplify = TRUE)
  })
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  colnames(df) <- vars
  rownames(df) <- NULL
  df
}

cht <- bindings_to_df_safe(parsed$results$bindings)

## zelfde voor AAT maar dan via online export

aat <- read.csv("C:\\Users\\Admin\\Documents\\Termenentwerk\\aat_preflabels.csv")

# samenvoegen van 2

setDT(aat)
names(aat)[names(aat) == "prefLabel"] <- "label"
aat[, source := "aat"]

setDT(cht)
cht[, source := "cht"]

df_terms <- rbind(cht, aat)

df_terms$term_lower <- tolower(df_terms$label)


### Vrouwenthesaurus ophalen

endpoint <- "https://vrouwenthesaurus.org/PoolParty/sparql/conceptid"  # vervang door jouw endpoint


query <- '
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?label WHERE {
  
    ?concept a skos:Concept .
    ?concept skos:prefLabel ?label .
    FILTER(LANG(?label) = "nl")
  
}
'

# Verstuur de query (POST is beter bij langere queries)
resp <- request(endpoint) %>%
  req_headers(Accept = "application/sparql-results+json") %>%
  req_body_form(query = query) %>%   # <-- let op: named arg
  req_perform()

# JSON → data.frame
parsed <- fromJSON(resp_body_string(resp))

resp <- request(endpoint) %>%
  req_headers(Accept = "application/sparql-results+json") %>%
  req_body_form(query = query) %>%
  req_perform()

# Parse JSON naar R-lijst
parsed <- resp_body_json(resp, simplifyVector = FALSE)

# Veilige converter van bindings naar data.frame
bindings_to_df_safe <- function(bindings) {
  if (is.null(bindings) || length(bindings) == 0) return(data.frame())
  # bepaal alle variabelen (kolomnamen)
  vars <- unique(unlist(lapply(bindings, names)))
  # bouw rijen
  rows <- lapply(bindings, function(b) {
    sapply(vars, function(v) {
      val <- b[[v]]
      if (is.null(val)) return(NA_character_)
      if (is.list(val) && !is.null(val$value)) return(as.character(val$value))
      if (is.atomic(val)) return(as.character(val))
      # fallback: try to coerce
      tryCatch(as.character(val), error = function(e) NA_character_)
    }, USE.NAMES = TRUE, simplify = TRUE)
  })
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  colnames(df) <- vars
  rownames(df) <- NULL
  df
}

vt <- bindings_to_df_safe(parsed$results$bindings)

# WO2 thesaurus

endpoint <- "https://data.niod.nl/PoolParty/sparql/WO2_Thesaurus"  # vervang door jouw endpoint


query <- '
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?label WHERE {
  
    ?concept a skos:Concept .
    ?concept skos:prefLabel ?label .
    FILTER(LANG(?label) = "nl")
  
}
'

# Verstuur de query (POST is beter bij langere queries)
resp <- request(endpoint) %>%
  req_headers(Accept = "application/sparql-results+json") %>%
  req_body_form(query = query) %>%   # <-- let op: named arg
  req_perform()

# JSON → data.frame
parsed <- fromJSON(resp_body_string(resp))

resp <- request(endpoint) %>%
  req_headers(Accept = "application/sparql-results+json") %>%
  req_body_form(query = query) %>%
  req_perform()

# Parse JSON naar R-lijst
parsed <- resp_body_json(resp, simplifyVector = FALSE)

# Veilige converter van bindings naar data.frame
bindings_to_df_safe <- function(bindings) {
  if (is.null(bindings) || length(bindings) == 0) return(data.frame())
  # bepaal alle variabelen (kolomnamen)
  vars <- unique(unlist(lapply(bindings, names)))
  # bouw rijen
  rows <- lapply(bindings, function(b) {
    sapply(vars, function(v) {
      val <- b[[v]]
      if (is.null(val)) return(NA_character_)
      if (is.list(val) && !is.null(val$value)) return(as.character(val$value))
      if (is.atomic(val)) return(as.character(val))
      # fallback: try to coerce
      tryCatch(as.character(val), error = function(e) NA_character_)
    }, USE.NAMES = TRUE, simplify = TRUE)
  })
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  colnames(df) <- vars
  rownames(df) <- NULL
  df
}

wo2 <- bindings_to_df_safe(parsed$results$bindings)

# samenvoegen van 4

setDT(aat)
names(aat)[names(aat) == "prefLabel"] <- "label"
aat[, source := "aat"]

setDT(vt)
vt[, source := "vt"]

setDT(cht)
cht[, source := "cht"]

setDT(wo2)
wo2[, source := "WO2"]

df_terms <- rbind(cht, aat, vt, wo2)

#### met pretrained model

# installeer pre-trained model

#model_url <- "https://sparknlp.org/2021/10/04/dutch_cc_300d_nl.html"

#if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
#devtools::install_github("bmschmidt/wordVectors")

library(wordVectors)
library(dplyr)
library(umap)
library(ggplot2)

# Laad het model
model <- read.vectors("C:\\Users\\Admin\\Documents\\Termenentwerk\\cc.nl.300.vec", binary = FALSE)

labels_in_model <- df_terms$label[df_terms$label %in% rownames(model)] # note: 1/3 van de labels zit in het model, dus de exercitie kan nog beter

df_terms_filtered <- df_terms %>%
  filter(label %in% labels_in_model)

embeddings_labels <- model[labels_in_model, , drop = FALSE]

pca_emb <- prcomp(embeddings_labels, center = TRUE, scale. = TRUE)$x[, 1:50, drop = FALSE]

set.seed(42)
umap_res <- umap(pca_emb, n_neighbors = 30, min_dist = 0.1)

plot_df <- data.frame(
  x = umap_res$layout[, 1],
  y = umap_res$layout[, 2],
  label = labels_in_model,
  source = df_terms_filtered$source[match(labels_in_model, df_terms_filtered$label)]
)

# check overlap door embeddings

sum(duplicated(plot_df$label)) # 4287

plot_df_unique <- plot_df[!duplicated(plot_df$label), ]

plot_df_filtered <- plot_df_unique %>%
  filter(x < 20, y < 20)

p <- ggplot(plot_df_filtered, aes(x = x, y = y, color = source)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_text(aes(label = label), check_overlap = TRUE, size = 3, vjust = 1.5) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Semantische spreiding van labels per bron (UMAP op NL embeddings)",
    x = "UMAP dimensie 1",
    y = "UMAP dimensie 2",
    color = "Bron"
  ) +
  theme(legend.position = "bottom")

library(plotly)

p_interactive <- ggplotly(p, tooltip = c("label", "source"))

p_interactive

p_webgl <- plot_ly(
  data = plot_df_filtered,
  x = ~x,
  y = ~y,
  color = ~source,
  colors = color_map,
  text = ~label,
  hoverinfo = "text",
  type = "scattergl",
  mode = "markers",
  marker = list(size = 3, opacity = 0.7)
)

p_webgl

color_map <- c(
  "cht" = "#66ccff",
  "vt" = "#4b0082",
  "WO2" = "#2ca02c",
  "aat" = "#d62728"
)

p_webgl_all <- p_webgl <- plot_ly(
  data = plot_df_filtered[!(plot_df_filtered$source %in% c("aa2t")), ],
  x = ~x,
  y = ~y,
  color = ~source,
  colors = color_map,
  text = ~label,
  hoverinfo = "text",
  type = "scattergl",
  mode = "markers",
  marker = list(size = 3, opacity = 0.7)
)

p_webgl_all

library(htmlwidgets)

# Opslaan als interactieve HTML
saveWidget(p_webgl, "umap_plot.html", selfcontained = TRUE)


# preflabels die letterlijk hetzelfde zijn

setDT(df_terms)
overlap  <- df_terms[, .(unique_sources = uniqueN(source)), by = label]

overlap[unique_sources ==2,] # antwoord: 5280 preflabels zijn volledig identiek

5280/ (73347/2) # 14,4 % (grofweg)



