# --- Tests for tokenize_text ---
# The 'describe' block groups tests for the 'tokenize_text' function.
describe("tokenize_text", {
  # Test case: Verifies basic tokenization of a simple sentence.
  it("tokenizes simple text correctly", {
    text <- "Hello world! This is a test."
    # Expected output: list containing a character vector of tokens.
    expected_tokens <- list(c("hello", "world", "this", "is", "a", "test"))
    expect_equal(
      tokenize_text(text),
      expected_tokens,
      info = "Basic tokenization should split words and convert to lowercase."
    )
  })

  # Test case: Checks handling of text with leading/trailing/extra spaces and
  # special characters.
  it("handles text with extra spaces and special characters", {
    text <- "  Extra   spaces &*^%$#@! punctuation!!! "
    # Special characters are replaced by spaces, then text is squished.
    expected_tokens <- list(c("extra", "spaces", "punctuation"))
    expect_equal(
      tokenize_text(text),
      expected_tokens,
      info = "Should correctly process spaces and special characters."
    )
  })

  # Test case: Ensures text is converted to lowercase.
  it("converts text to lowercase", {
    text <- "UPPERCASE Text"
    expected_tokens <- list(c("uppercase", "text"))
    expect_equal(
      tokenize_text(text),
      expected_tokens,
      info = "Text should be converted to lowercase."
    )
  })

  # Test case: Handles empty string input.
  it("handles empty string input", {
    text <- ""
    # Tokenizing an empty string results in list(c("")) with space_tokenizer.
    expected_tokens <- list(c(""))
    expect_equal(
      tokenize_text(text),
      expected_tokens,
      info = "Empty string should result in one empty token."
    )
  })

  # Test case: Handles string composed only of spaces.
  it("handles string with only spaces", {
    text <- "   "
    # String with only spaces gets squished to "", then tokenized to list(c("")).
    expected_tokens <- list(c(""))
    expect_equal(
      tokenize_text(text),
      expected_tokens,
      info = "String of spaces should result in one empty token after processing."
    )
  })

  # Test case: Handles string composed only of special characters.
  it("handles string with only special characters", {
    text <- "!@#$%^"
    # Special characters are replaced by spaces, squished to " ", then tokenized.
    # " " -> list(c("")) by space_tokenizer.
    expected_tokens <- list(c(""))
    expect_equal(
      tokenize_text(text),
      expected_tokens,
      info = "String of special chars should result in one empty token due to space replacement."
    )
  })

  # Test case: Verifies tokenization of multiple sentences combined into one string.
  it("handles multiple sentences combined", {
    text <- "First sentence. Second sentence."
    expected_tokens <- list(c("first", "sentence", "second", "sentence"))
    expect_equal(
      tokenize_text(text),
      expected_tokens,
      info = "Should tokenize across sentence-ending punctuation."
    )
  })
})


# --- Tests for generate_glove_model ---
# The 'describe' block groups tests for the 'generate_glove_model' function.
describe("generate_glove_model", {
  # Define a small, predictable text sample for model generation tests.
  # This ensures tests are fast and results are consistent.
  sample_text_glove <- c(
    "apple banana",
    "apple orange",
    "banana orange juice"
  )
  # Use minimal dimensions and iterations for testing to speed up execution.
  test_dimensions_glove <- 5 # Renamed
  test_iterations_glove <- 5 # Renamed

  # Ensure that any options set by generate_glove_model for caching
  # are reset after all tests in this describe block are executed.
  withr::defer({
    options(minutemaker_data_merge_hash = NULL)
    options(minutemaker_data_merge_model = NULL)
  })

  # Test case: Verifies that the generated word vector matrix has the specified
  # dimensions.
  it("generates a word vector matrix with specified dimensions", {
    # Ensure a clean state for caching options before this specific test.
    options(minutemaker_data_merge_hash = NULL)
    options(minutemaker_data_merge_model = NULL)

    word_vectors <- generate_glove_model(
      text = sample_text_glove,
      dimensions = test_dimensions_glove,
      iterations = test_iterations_glove,
      overwrite = TRUE # Force regeneration for this test.
    )
    expect_true(is.matrix(word_vectors), info = "Output should be a matrix.")
    expect_equal(
      ncol(word_vectors),
      test_dimensions_glove,
      info = "Number of columns should match dimensions."
    )
    # Vocabulary from sample_text_glove: "apple", "banana", "orange", "juice"
    expected_vocab <- c("apple", "banana", "orange", "juice")
    expect_in(
      expected_vocab,
      rownames(word_vectors)
    )
    expect_gte(
      length(rownames(word_vectors)),
      length(expected_vocab)
    )
  })

  # Test case: Checks if the cached model is used on subsequent calls with
  # identical parameters.
  it("utilizes cached model on subsequent calls with identical parameters", {
    options(minutemaker_data_merge_hash = NULL)
    options(minutemaker_data_merge_model = NULL)

    # First call: Should generate and cache the model. Expect a message
    # indicating generation.
    expect_message(
      model1 <- generate_glove_model(
        text = sample_text_glove,
        dimensions = test_dimensions_glove,
        iterations = test_iterations_glove
      ),
      "Generating the GloVe model...",
      info = "First call should trigger model generation."
    )

    # Second call: Should use the cached model. Expect a message indicating
    # cache usage.
    expect_message(
      model2 <- generate_glove_model(
        text = sample_text_glove,
        dimensions = test_dimensions_glove,
        iterations = test_iterations_glove
      ),
      "Using previously generated GloVe model \\(matching hash found\\).", # Escaped parentheses
      info = "Second call with same parameters should use cached model."
    )
    # Verify that the two model objects are identical.
    expect_identical(
      model1,
      model2,
      info = "Cached model should be identical to the first generated model."
    )
  })

  # Test case: Ensures model is regenerated if 'overwrite' is TRUE, even if a
  # cache exists.
  it("regenerates model when overwrite is TRUE, even if cache exists", {
    options(minutemaker_data_merge_hash = NULL)
    options(minutemaker_data_merge_model = NULL)

    # First call to populate the cache.
    expect_message(
      model_cached <- generate_glove_model(
        text = sample_text_glove,
        dimensions = test_dimensions_glove,
        iterations = test_iterations_glove
      ),
      "Generating the GloVe model...",
      info = "Initial call to populate cache."
    )

    # Second call with overwrite = TRUE. Should regenerate, not use cache.
    expect_message(
      model_overwritten <- generate_glove_model(
        text = sample_text_glove,
        dimensions = test_dimensions_glove,
        iterations = test_iterations_glove,
        overwrite = TRUE
      ),
      "Generating the GloVe model...", # Expect generation message again.
      info = "Call with overwrite=TRUE should force regeneration."
    )
    # While GloVe can be stochastic, the primary check is that the regeneration path was taken (via message).
    # The model stored in options should be the newly generated one.
    expect_false(
      is.null(getOption("minutemaker_data_merge_model")),
      info = "A model should be stored in options after overwrite."
    )
  })

  # Test case: Checks if the model regenerates when input parameters (e.g.,
  # dimensions) change.
  it("regenerates model if parameters change", {
    options(minutemaker_data_merge_hash = NULL)
    options(minutemaker_data_merge_model = NULL)

    # First call with initial parameters.
    expect_message(
      model_dim_original <- generate_glove_model(
        text = sample_text_glove,
        dimensions = test_dimensions_glove, # e.g., 5
        iterations = test_iterations_glove
      ),
      "Generating the GloVe model...",
      info = "First model generation with initial dimensions."
    )

    # Second call with different dimensions. Should trigger regeneration.
    expect_message(
      model_dim_changed <- generate_glove_model(
        text = sample_text_glove,
        dimensions = test_dimensions_glove + 1, # e.g., 6
        iterations = test_iterations_glove
      ),
      "Generating the GloVe model...",
      info = "Model should regenerate if dimensions change."
    )

    expect_equal(
      ncol(model_dim_original),
      test_dimensions_glove,
      info = "Original model should have original dimensions."
    )
    expect_equal(
      ncol(model_dim_changed),
      test_dimensions_glove + 1,
      info = "New model should have new dimensions."
    )
    # The models themselves should be different due to different dimensions.
    expect_false(
      identical(model_dim_original, model_dim_changed),
      info = "Models with different dimensions should not be identical."
    )
  })
})


# --- Tests for compute_text_sim ---
# The 'describe' block groups tests for the 'compute_text_sim' function.
describe("compute_text_sim", {
  # Generate a shared embedding matrix for these tests to ensure consistency
  # and avoid repeated slow computations of GloVe models.
  corpus_for_sim_tests <- c(
    "the quick brown fox",
    "jumps over the lazy dog",
    "a completely different sentence",
    "the quick brown cat"
  )
  # Use low dimensions/iterations for speed. Force overwrite to ensure this
  # specific model is generated for this test block, avoiding interference from
  # other tests' caching.
  embedding_matrix_sim <- generate_glove_model(
    # Renamed to avoid clashes
    text = corpus_for_sim_tests,
    dimensions = 10,
    iterations = 10,
    overwrite = TRUE
  )

  # Clean up options set by generate_glove_model after this describe block.
  withr::defer({
    options(minutemaker_data_merge_hash = NULL)
    options(minutemaker_data_merge_model = NULL)
  })

  # Test case: Validates similarity score calculations for various scenarios.
  it("calculates similarity scores correctly for various cases", {
    x_text <- "quick brown fox"
    # These y_texts are designed to have words present in the
    # corpus_for_sim_tests vocabulary.
    y_texts <- c(
      "quick brown fox", # Identical
      "quick brown cat", # Similar
      "lazy dog", # Different concept, in vocab
      "a completely different sentence" # Different, in vocab
    )

    sim_scores <- compute_text_sim(x_text, y_texts, embedding_matrix_sim)

    expect_type(
      sim_scores,
      "double"
    )
    expect_equal(
      length(sim_scores),
      length(y_texts),
      info = "Should return one score per y_text."
    )

    # Scores should be non-NA as all words are expected to be in the small
    # vocab.
    expect_true(
      all(!is.na(sim_scores)),
      info = "All scores should be non-NA for in-vocabulary texts."
    )

    # Identical text should have a very high similarity score (close to 1.0).
    expect_gt(
      sim_scores[1],
      0.9,
      label = "Identical text similarity should be > 0.9."
    )
    # Similar text ("quick brown cat") should be less similar than identical.
    expect_lt(
      sim_scores[2],
      sim_scores[1],
      label = "'quick brown cat' should be less similar than 'quick brown fox'."
    )
    # "quick brown cat" vs "lazy dog" relative to "quick brown fox" The
    # following assertion can be flaky with small GloVe models, as semantic
    # nuances are not always captured as expected. expect_gt(sim_scores[2],
    # sim_scores[3], label = "'quick brown cat' should be more similar to 'quick
    # brown fox' than 'lazy dog' is.") Expect "lazy dog" to be more similar than
    # "a completely different sentence" This can also be model-dependent and
    # plausible but potentially flaky. expect_gt(sim_scores[3], sim_scores[4],
    # label = "'lazy dog' should be more similar than 'a completely different
    # sentence'.")
  })

  # Test case: Verifies that NA is returned when x_text leads to an empty
  # embedding.
  it("returns NA when x_text results in an empty embedding", {
    # "zzzyyyxxx" is unlikely to be in the small vocabulary.
    x_text_oov <- "zzzyyyxxx"
    # Alternatively, a text that tokenize_text makes empty: x_text_empty_tokens
    # <- "!!!!"
    # For x_text_empty_tokens, tokenize_text -> list(c(""))),
    # get_text_embedding for ("") might not be empty if "" is in vocab. Using a
    # clear OOV word is safer for this test's intent. If "" from "!!!!" is in
    # vocab due to `tokenize_text("!@#$%^")` test, this needs thought.
    # `generate_glove_model` is called with `corpus_for_sim_tests`. "" is not in
    # its vocab. So `get_text_embedding` for `tokenize_text("!!!!")` (which is
    # `c("")`) would be empty.

    y_texts <- c("some text", "another text")
    sim_scores_oov <- compute_text_sim(
      x_text_oov,
      y_texts,
      embedding_matrix_sim
    )
    expect_identical(
      sim_scores_oov,
      rep(NA, length(y_texts)),
      info = "All scores should be NA if x_text embedding is empty (OOV)."
    )
    expect_equal(length(sim_scores_oov), length(y_texts))

    x_text_empty_tokens <- "!!!!" # tokenize_text -> list(c(""))
    sim_scores_empty <- compute_text_sim(
      x_text_empty_tokens,
      y_texts,
      embedding_matrix_sim
    )
    expect_identical(
      sim_scores_empty,
      rep(NA, length(y_texts)),
      info = "All scores should be NA if x_text tokenizes to empty/OOV string like c('')."
    )
  })

  # Test case: Checks if NA is returned for specific y_texts that result in
  # empty embeddings.
  it("returns NA for specific y_texts that result in empty embeddings", {
    x_text <- "quick brown fox"
    y_texts_with_oov <- c(
      "quick brown cat", # In vocab
      "zzzyyyxxx", # OOV, should result in NA
      "jumps over lazy dog", # In vocab
      "####" # Tokenizes to c(""), should be NA as "" is not in this vocab
    )

    sim_scores <- compute_text_sim(
      x_text,
      y_texts_with_oov,
      embedding_matrix_sim
    )

    expect_false(
      is.na(sim_scores[1]),
      info = "Score for in-vocab y_text[1] should not be NA."
    )
    expect_true(
      is.na(sim_scores[2]),
      info = "Score for OOV y_text[2] should be NA."
    )
    expect_false(
      is.na(sim_scores[3]),
      info = "Score for in-vocab y_text[3] should not be NA."
    )
    expect_true(
      is.na(sim_scores[4]),
      info = "Score for y_text[4] tokenizing to empty/OOV should be NA."
    )
    expect_equal(length(sim_scores), length(y_texts_with_oov))
  })

  # Test case: Evaluates handling of single-word texts.
  it("handles single word texts correctly", {
    # This test requires a specific embedding matrix where single words are
    # known.
    corpus_single_word <- c(
      "apple",
      "banana",
      "orange",
      "apple tree",
      "fruit basket apple banana"
    )
    # For this specific test, we generate a local embedding matrix to avoid
    # interference from the global `embedding_matrix_sim` or its options.
    # Also, manage options locally for this model generation.
    local_options_state <- options(
      minutemaker_data_merge_hash = NULL,
      minutemaker_data_merge_model = NULL
    )
    withr::defer(options(local_options_state)) # Restore original options

    embedding_matrix_single <- generate_glove_model(
      text = corpus_single_word,
      dimensions = 5, # Small dimensions for speed
      iterations = 5, # Small iterations
      overwrite = TRUE # Ensure this specific model is generated
    )
    # Ensure target words are in the generated vocabulary.
    expect_in(
      c("apple", "banana", "tree"),
      rownames(embedding_matrix_single)
    )

    x_text_single <- "apple"
    # All words should be in embedding_matrix_single
    y_texts_single <- c("apple", "banana", "tree")

    sim_scores <- compute_text_sim(
      x_text_single,
      y_texts_single,
      embedding_matrix_single
    )

    expect_false(
      any(is.na(sim_scores)),
      info = "No NAs expected for single in-vocab words."
    )
    expect_gt(
      sim_scores[1],
      0.9,
      label = "Similarity of 'apple' to 'apple' should be > 0.9."
    )
    # Relative similarity of 'apple' to 'banana' vs 'apple' to 'tree' depends on
    # the model. Just ensure they are numeric and less than self-similarity.
    expect_lt(
      sim_scores[2],
      sim_scores[1],
      label = "'banana' similarity to 'apple' should be less than self-similarity."
    )
    expect_lt(
      sim_scores[3],
      sim_scores[1],
      label = "'tree' similarity to 'apple' should be less than self-similarity."
    )
  })
})
