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
      # verschillende mogelijke vormen:
      # - NULL -> NA
      # - list met $value -> gebruik dat
      # - atomic (character) -> gebruik direct
      # - list maar geen $value (onverwacht) -> probeer als character
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

# samenvoegen

setDT(aat)
names(aat)[names(aat) == "prefLabel"] <- "label"
aat[, source := "aat"]

setDT(cht)
cht[, source := "cht"]

df_terms <- rbind(cht, aat)

df_terms$term_lower <- tolower(df_terms$label)

# get embedding model

library(text)
library(dplyr)
library(ggplot2)
library(umap) 
library(reticulate)
library(text2vec)
library(word2vec)
library(Rtsne)

#reticulate::py_install("torch", pip = TRUE)
#reticulate::py_install("transformers", pip = TRUE)

# bereken embeddings

tokens <- strsplit(df_terms$label, " ")
it <- itoken(tokens, progressbar = FALSE)

vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)

glove <- GlobalVectors$new(rank = 50, x_max = 10)
wv_main <- glove$fit_transform(tcm, n_iter = 20)
wv_context <- glove$components
embeddings <- wv_main + t(wv_context)

# alleen labels die in het model voorkomen
labels_in_model <- df_terms$label[df_terms$label %in% rownames(embeddings)]
# subset van df_terms
df_plot <- df_terms[df_terms$label %in% labels_in_model, ]

# embeddings voor deze labels
embeddings_labels <- embeddings[labels_in_model, , drop = FALSE]
# Verwijder duplicaten in embeddings
embeddings_labels_unique <- unique(embeddings_labels)

# Bijbehorende labels behouden
labels_unique <- rownames(embeddings_labels_unique)

f_plot <- data.table(
  label = rownames(embeddings_labels),
  source = df_terms$source[match(rownames(embeddings_labels), df_terms$label)]
)
dup_idx <- !duplicated(embeddings_labels)
embeddings_unique <- embeddings_labels[dup_idx, , drop = FALSE]
f_plot_unique <- f_plot[dup_idx]

# 4️⃣ Optioneel: schaal embeddings
embeddings_scaled <- scale(embeddings_unique)

# 5️⃣ PCA naar bv. 30 dimensies voor t-SNE
pca_res <- prcomp(embeddings_scaled, rank. = 30)
pca_emb <- pca_res$x

# 6️⃣ t-SNE uitvoeren
set.seed(42)  # voor reproduceerbaarheid
tsne_res <- Rtsne(pca_emb, dims = 2, perplexity = 100, max_iter = 500, verbose = TRUE)

# 7️⃣ Voeg t-SNE coördinaten toe aan data
f_plot_unique$tsne_x <- tsne_res$Y[,1]
f_plot_unique$tsne_y <- tsne_res$Y[,2]

# 8️⃣ Plot
ggplot(f_plot_unique, aes(x = tsne_x, y = tsne_y, color = source)) +
  geom_point(alpha = 0.7, size = 2) +
  theme_minimal() +
  labs(title = "t-SNE van term-embeddings",
       x = "t-SNE 1",
       y = "t-SNE 2") +
  scale_color_brewer(palette = "Set1")

# vergelijken

library(dplyr)
library(R.utils)

# Stel dat df_terms$source de bron bevat en embeddings_labels de embeddings per woord
df_embeddings <- data.frame(embeddings_labels)
df_embeddings$label <- rownames(embeddings_labels)
df_embeddings$source <- df_terms$source[match(rownames(embeddings_labels), df_terms$label)]

# Gemiddelde embedding per bron
mean_embeddings <- df_embeddings %>%
  group_by(source) %>%
  summarise(across(starts_with("V"), mean))

# Verwijder bronkolom en labels
emb_mat <- as.matrix(mean_embeddings[, -1])

str(emb_mat)
emb_mat <- as.matrix(mean_embeddings[, -1])
dim(emb_mat)
umap_res <- umap(emb_mat)

# Voeg bron terug
umap_df <- data.frame(
  source = mean_embeddings$source,
  umap1 = umap_res$layout[,1],
  umap2 = umap_res$layout[,2]
)

ggplot(umap_df, aes(x = umap1, y = umap2, color = source)) +
  geom_point(size = 5) +
  theme_minimal() +
  labs(title = "Gemiddelde embeddings per bron (UMAP)")


#### met pretrained model

# Laad het pretrained model
model_url <- "https://sparknlp.org/2021/10/04/dutch_cc_300d_nl.html"

if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("bmschmidt/wordVectors")

# Laad het pakket
R.utils::gunzip("C:\\Users\\Admin\\Documents\\Termenentwerk\\cc.nl.300.vec.gz", remove = FALSE)

model <- read.vectors("C:\\Users\\Admin\\Documents\\Termenentwerk\\cc.nl.300.vec", binary = FALSE)

labels_in_model <- df_terms$label[df_terms$label %in% rownames(model)]

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

plot_df_filtered <- plot_df %>%
  filter(x < 15, y < 8)

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

# Toon de plot
p_interactive


p_webgl <- plot_ly(
  data = plot_df_filtered,
  x = ~x,
  y = ~y,
  color = ~source,
  colors = "Set1",
  text = ~label,
  hoverinfo = "text",
  type = "scattergl",
  mode = "markers",
  marker = list(size = 3, opacity = 0.7)
)

p_webgl

library(htmlwidgets)

# Opslaan als interactieve HTML
saveWidget(p_webgl, "umap_plot.html", selfcontained = TRUE)
