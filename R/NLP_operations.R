#' Tokenize Text for Text Analysis
#'
#' This function processes and tokenizes a given text. It converts the text to
#' lowercase, removes special characters and extra spaces, and then splits the
#' text into tokens.
#'
#' @param text A character vector containing the text to be tokenized.
#'
#' @return A list of tokens extracted from the text.
#'
#'
tokenize_text <- function(text) {

  check_and_install_dependencies("text2vec")

  # Convert text to lowercase, remove special characters and extra spaces, and
  # tokenize
  tokens <- text %>%
    tolower() %>%
    stringr::str_replace_all("[^[:alnum:]]|\\s+", " ") %>%
    stringr::str_squish() %>%
    text2vec::space_tokenizer()

  return(tokens)
}


#' Generate Word Vectors from Text using GloVe Model
#'
#' This function takes a text input and uses the `text2vec` package's GloVe
#' model to generate word embeddings.
#'
#' @param text A character vector of text to be processed.
#' @param dimensions The number of dimensions of the word vectors.
#' @param iterations The number of iterations to run the GloVe model.
#' @param overwrite Whether to overwrite a previously generated model.
#'
#' @return A matrix of word embeddings.
#'
#' @export
#'
#' @examples
#' text <- "Sample text for GloVe model generation."
#' word_vectors <- generate_model(text, dimensions = 5, iterations = 10)
#'
generate_model <- function(
    text,
    dimensions = 100,
    iterations = 50,
    overwrite = FALSE
) {

  check_and_install_dependencies("parallel")

  # Create a hash of the data and the model parameters
  data_hash <- rlang::hash(list(
    text, dimensions, iterations)
  )

  previous_hash <- getOption("minutemaker_data_merge_hash")
  previous_model <- getOption("minutemaker_data_merge_model")

  # Check if the model has already been generated and if so, return it
  if (!overwrite && !is.null(previous_hash) && !is.null(previous_model)) {

    # Use the hash of the data and the model parameters to check if the model
    # has already been generated
    if (data_hash == previous_hash) {
      message("Using previously generated Glove model.")
      return(previous_model)
    }
  }

  message("Generating the Glove model...")

  # Store the hash of the data and the model parameters
  options(minutemaker_data_merge_hash = data_hash)

  # Tokenize the text
  tokens <- tokenize_text(text)

  # Create an iterator over tokens and then build a vocabulary from the tokens
  it <- text2vec::itoken(tokens, progressbar = FALSE)
  vocab <- text2vec::create_vocabulary(it)
  vectorizer <- text2vec::vocab_vectorizer(vocab)

  # Create a term-co-occurrence matrix from the tokens using the vectorizer
  tcm <- text2vec::create_tcm(it, vectorizer, skip_grams_window = 5L)

  # Initialize a GloVe model and fit it to the term-co-occurrence matrix to
  # obtain word embeddings
  glove_model <- text2vec::GloVe$new(rank = dimensions, x_max = 10)
  wv_main = glove_model$fit_transform(tcm, n_iter = iterations,
                                      convergence_tol = 0.01,
                                      n_threads = parallel::detectCores())

  # Obtain the context vectors and add them to the main vectors to get the
  # final word vectors (as suggested here: https://text2vec.org/glove.html)
  wv_context = glove_model$components
  word_vectors = wv_main + t(wv_context)

  # Store the model
  options(minutemaker_data_merge_model = word_vectors)

  return(word_vectors)
}


#' Compute Similarity Scores between Texts
#'
#' This function calculates the cosine similarity scores between one text
#' (x_text) and a set of other texts (y_texts) using word embeddings generated
#' by a GloVe model.
#'
#' @param x_text A single text string.
#' @param y_texts A character vector of texts to compare against x_text.
#' @param embedding_matrix A matrix of word embeddings.
#'
#' @return A numeric vector of similarity scores.
#'
#' @export
#'
#' @examples
#' x_text <- "Sample source text."
#' y_texts <- c("Comparative text one.", "Comparative text two.")
#' word_vectors <- generate_model(c(x_text, y_texts))
#' similarities <- compute_text_sim(x_text, y_texts, word_vectors)
#'
compute_text_sim <- function(x_text, y_texts, embedding_matrix) {
  # Function to convert a text to a normalized embedding vector
  get_text_embedding <- function(text, embedding_matrix) {

    # Tokenize and normalize the text
    tokens <- tokenize_text(text) |> unlist()

    # Obtain embeddings for each token and sum them to get a document-level
    # embedding
    terms <- intersect(tokens, rownames(embedding_matrix))

    embeddings <- embedding_matrix[terms, ]

    # Sum the embeddings if there are multiple terms
    doc_embedding <- if (length(terms) > 1) {
      colSums(embeddings, na.rm = TRUE)
    } else { embeddings}

    # Normalize the document embedding
    doc_embedding <- doc_embedding /
      sqrt(sum(doc_embedding^2) / length(doc_embedding))

    return(doc_embedding)
  }

  # Compute the embedding for the source text
  x_emb <- get_text_embedding(x_text, embedding_matrix)

  # Compute cosine similarity with each comparative text
  y_emb <- purrr::map(y_texts, function(y_text) {
    get_text_embedding(y_text, embedding_matrix)
  }) |> do.call(what = rbind)

  text2vec::sim2(t(x_emb), y_emb, method = "cosine", norm = "l2") |>
    as.vector()
}

# Old approach to speaker identification using word2vec. Kept for reference.
#
# get_speaker <- function(
#     y_probes, x_text, method, model
# ) {
#
#   if (method == "word2vec") {
#
#     check_and_install_dependencies("word2vec")
#
#     # Word2vec returns NA if a segment is a single word with a punctuation
#     # mark at the end, don't know why
#     x_text <- gsub("[[:punct:]]$", "", x_text)
#     y_probes$text <- gsub("[[:punct:]]$", "", y_probes$text)
#
#     # Calculate the semantic similarity between the segment in the first
#     # transcript and the candidate segments in the second transcript
#     y_probes$similarity <- word2vec::word2vec_similarity(
#       word2vec::doc2vec(model, y_probes$text),
#       word2vec::doc2vec(model, x_text)) |>
#       as.vector()
#   } else {
#     check_and_install_dependencies("stringdist")
#
#     y_probes$similarity <- stringdist::stringsim(
#       x_text, y_probes$text, method = "cosine")
#   }
#
#   # Select the most likely speaker
#   which_best <- which.max(y_probes$similarity)
#
#   # Sometimes word2vec returns NA, in that case the speaker is not imported
#   if (length(which_best) == 1) {
#     y_probes$speaker[which_best]
#   } else stop()
# }
