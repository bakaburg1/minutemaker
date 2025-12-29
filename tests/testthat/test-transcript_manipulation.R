test_that("clean_transcript basic cleaning works", {
  # clean_transcript uses dplyr and other helpers that should be available
  # when tests are run with devtools::test() which calls load_all()

  # Note: silent() is an internal helper from minutemaker returning "[...]"
  # is_silent() is also an internal helper.

  input_data <- dplyr::tibble(
    start = 1:6,
    end = 2:7,
    text = c(
      "  leading and trailing spaces  ",
      "double  spaces",
      NA_character_,
      "",
      "repeated text",
      "repeated text"
    )
  )

  expected_data <- dplyr::tibble(
    start = 1:6,
    end = 2:7,
    text = c(
      "leading and trailing spaces",
      "double spaces",
      silent(), # NA becomes silent
      silent(), # "" becomes silent
      "repeated text",
      silent() # Second occurrence of "repeated text" becomes silent
    )
  )

  # clean_transcript also has other cleaning steps for isolated text,
  # repetitions, and low confidence, which are not specifically tested here
  # but will be applied. We expect them not to affect this basic case.
  # For this test, we focus on the text column changes.
  # The function might add/remove columns if confidence scores were present.
  # Here, we only have start, end, text.
  result_data <- clean_transcript(input_data)

  expect_equal(result_data, expected_data)
})

test_that("clean_transcript removes silence correctly", {
  input_data <- dplyr::tibble(
    start = 1:5,
    end = 2:6,
    text = c(
      "  leading and trailing spaces  ",
      NA_character_,
      "",
      "actual text",
      "actual text"
    )
  )

  expected_data_removed_silence <- dplyr::tibble(
    start = c(1L, 4L),
    end = c(2L, 5L),
    text = c("leading and trailing spaces", "actual text")
  )

  result_data_removed_silence <- clean_transcript(
    input_data,
    remove_silence = TRUE
  )

  expect_equal(
    result_data_removed_silence,
    expected_data_removed_silence
  )
})

test_that("clean_transcript handles empty input", {
  empty_transcript_template <- dplyr::tibble(
    start = integer(0),
    end = integer(0),
    text = character(0)
    # Potentially other columns like avg_logprob, no_speech_prob, speaker
    # could exist. clean_transcript should handle their absence or presence.
  )

  # Test with no other columns
  result_empty <- clean_transcript(empty_transcript_template)
  expect_identical(result_empty, empty_transcript_template)

  result_empty_remove_silence <- clean_transcript(
    empty_transcript_template,
    remove_silence = TRUE
  )
  expect_identical(result_empty_remove_silence, empty_transcript_template)

  # Test with optional columns present
  empty_transcript_with_extras <- dplyr::tibble(
    start = integer(0),
    end = integer(0),
    text = character(0),
    avg_logprob = numeric(0),
    no_speech_prob = numeric(0),
    speaker = character(0)
  )
  # clean_transcript removes avg_logprob and no_speech_prob if they exist
  expected_empty_after_extras_removed <- dplyr::tibble(
    start = integer(0),
    end = integer(0),
    text = character(0),
    speaker = character(0) # speaker column should be preserved
  )

  result_empty_with_extras <- clean_transcript(empty_transcript_with_extras)
  expect_identical(
    result_empty_with_extras,
    expected_empty_after_extras_removed
  )

  result_empty_with_extras_remove_silence <- clean_transcript(
    empty_transcript_with_extras,
    remove_silence = TRUE
  )
  expect_identical(
    result_empty_with_extras_remove_silence,
    expected_empty_after_extras_removed
  )
})

test_that("clean_transcript removes isolated text segments", {
  # This tests the removal of text surrounded by silent segments.
  s <- silent()

  # Case 1: Text fully surrounded, and control text also becomes isolated
  input_case1 <- dplyr::tibble(
    text = c(s, s, s, s, "isolated", s, s, s, s, "control")
  )
  # "isolated" (idx 5) becomes s. Then transcript is (s,s,s,s,s,s,s,s,s,"control").
  # Then "control" (idx 10) is checked. Neighbours (idx 6,7,8,9) are all s. So it becomes s.
  expected_text_case1 <- c(s, s, s, s, s, s, s, s, s, s)
  input_case1$start <- seq_len(nrow(input_case1))
  input_case1$end <- input_case1$start + 1
  result_case1 <- clean_transcript(input_case1)
  expect_identical(result_case1$text, expected_text_case1)

  # Case 2: Text at beginning, followed by silent, then control also becomes isolated
  input_case2 <- dplyr::tibble(
    text = c("isolated", s, s, s, s, "control")
  )
  # "isolated" (idx 1) -> s. Then (s,s,s,s,s,"control").
  # "control" (idx 6) -> s.
  expected_text_case2 <- c(s, s, s, s, s, s)
  input_case2$start <- seq_len(nrow(input_case2))
  input_case2$end <- input_case2$start + 1
  result_case2 <- clean_transcript(input_case2)
  expect_identical(result_case2$text, expected_text_case2)

  # Case 3: Control, then silent, then text at end; both control and text become isolated
  input_case3 <- dplyr::tibble(
    text = c("control", s, s, s, s, "isolated")
  )
  # "control" (idx 1) has s,s,s,s after. Becomes s. Transcript: (s,s,s,s,s,"isolated")
  # "isolated" (idx 6) has s,s,s,s before. Becomes s.
  expected_text_case3 <- c(s, s, s, s, s, s)
  input_case3$start <- seq_len(nrow(input_case3))
  input_case3$end <- input_case3$start + 1
  result_case3 <- clean_transcript(input_case3)
  expect_identical(result_case3$text, expected_text_case3)

  # Case 4: Text ("isolated") not fully isolated by s, "control" also not isolated
  input_case4 <- dplyr::tibble(text = c(s, "isolated", s, "control"))
  # "isolated" (idx 2) is surrounded by s (idx 1) and (s, "control") (idx 3,4). Not all silent. Stays.
  # "control" (idx 4) is surrounded by (s, "isolated", s) (idx 1,2,3). Not all silent. Stays.
  expected_text_case4 <- c(s, "isolated", s, "control")
  input_case4$start <- seq_len(nrow(input_case4))
  input_case4$end <- input_case4$start + 1
  result_case4 <- clean_transcript(input_case4)
  expect_identical(result_case4$text, expected_text_case4)

  # Case 5: Text surrounded by some non-silent (should NOT be removed)
  # This case was correct previously.
  input_case5 <- dplyr::tibble(
    text = c(s, s, "not isolated", "another text", s, s, "control")
  )
  expected_text_case5 <- c(
    s,
    s,
    "not isolated",
    "another text",
    s,
    s,
    "control"
  )
  # "control" at the end (idx 7) will be checked. Preceded by (idx 3,4,5,6)
  # c("not isolated", "another text", s, s). Not all silent. Stays.
  input_case5$start <- seq_len(nrow(input_case5))
  input_case5$end <- input_case5$start + 1
  result_case5 <- clean_transcript(input_case5)
  expect_identical(result_case5$text, expected_text_case5)

  # Case 6: Only one segment, should not become silent by this rule
  # This case was correct previously.
  input_case6 <- dplyr::tibble(text = c("single segment"), start = 1L, end = 2L)
  expected_text_case6 <- c("single segment")
  result_case6 <- clean_transcript(input_case6)
  expect_identical(result_case6$text, expected_text_case6)

  # Case 7: Two segments, one text, one silent. Text ("text") becomes silent.
  input_case7 <- dplyr::tibble(text = c("text", s))
  # "text" (idx 1) is followed by s (idx 2). All surrounding (just idx 2) is silent. Becomes s.
  # Then transcript is (s,s). Second s remains s.
  expected_text_case7 <- c(s, s)
  input_case7$start <- as.integer(1:2)
  input_case7$end <- as.integer(2:3) # Ensure integer type
  result_case7 <- clean_transcript(input_case7)
  expect_identical(result_case7$text, expected_text_case7)
})

test_that("clean_transcript removes segments with high word repetition", {
  s <- silent()

  # Note: Initial cleaning (squish, NA/"", duplicates to silent) happens first.
  # These tests assume inputs are either post-that or don't trigger those.

  # Case 1: Word repeated 11 times ( > 10), separated by ", "
  text_rep_11 <- paste(rep("spam", 11), collapse = ", ")
  # Case 2: Word repeated 10 times (<= 10), separated by ", "
  text_rep_10 <- paste(rep("eggs", 10), collapse = ", ")
  # Case 3: No commas, so repetition count based on split is 1
  text_no_comma_rep <- "alpha beta alpha beta"
  # Case 4: Already silent
  text_already_silent <- s
  # Case 5: Will become silent due to other rule (e.g. duplicate)
  # This tests that the repetition rule is applied on the current text state.
  # If text_dup_source is "original", and then text_dup_becomes_silent is also "original",
  # text_dup_becomes_silent will be silent() *before* repetition check.
  text_dup_source <- "original"
  text_dup_becomes_silent <- "original" # This will be silent() before rep check

  input_data <- dplyr::tibble(
    start = 1:6,
    end = 2:7,
    text = c(
      text_rep_11,
      text_rep_10,
      text_no_comma_rep,
      text_already_silent,
      text_dup_source,
      text_dup_becomes_silent
    )
  )

  # Expected after all cleaning:
  # 1. text_rep_11 -> silent()
  # 2. text_rep_10 -> "eggs, eggs, ..., eggs" (remains)
  # 3. text_no_comma_rep -> "alpha beta alpha beta" (remains)
  # 4. text_already_silent -> silent() (remains)
  # 5. text_dup_source -> "original" (remains, first occurrence)
  # 6. text_dup_becomes_silent -> silent() (due to being duplicate of text_dup_source)
  expected_data <- dplyr::tibble(
    start = c(1L, 2L, 3L, 4L, 5L, 6L), # Ensure integer type for start/end
    end = c(2L, 3L, 4L, 5L, 6L, 7L),
    text = c(
      s, # repetition > 10
      text_rep_10, # repetition <= 10
      text_no_comma_rep, # repetition = 1 (no commas)
      s, # already silent
      text_dup_source, # original text
      s # becomes silent due to duplication
    )
  )

  result_data <- clean_transcript(input_data)

  expect_identical(result_data$text, expected_data$text)
  # Check full data frame to ensure start/end are preserved and no row changes
  expect_identical(result_data, expected_data)

  # Case: Text with mixed items, one of which is repeated > 10 times
  text_mixed_high_rep <- paste(c(rep("A", 11), "B", "C"), collapse = ", ")
  input_mixed <- dplyr::tibble(start = 1L, end = 2L, text = text_mixed_high_rep)
  expected_mixed <- dplyr::tibble(start = 1L, end = 2L, text = s) # Should become silent
  expect_identical(clean_transcript(input_mixed), expected_mixed)

  # Case: Text with mixed items, max repetition is <= 10
  text_mixed_low_rep <- paste(
    c(rep("X", 5), rep("Y", 10), "Z"),
    collapse = ", "
  )
  input_mixed_low <- dplyr::tibble(
    start = 1L,
    end = 2L,
    text = text_mixed_low_rep
  )
  # Max repetition for "Y" is 10, so it should remain
  expected_mixed_low <- dplyr::tibble(
    start = 1L,
    end = 2L,
    text = text_mixed_low_rep
  )
  expect_identical(clean_transcript(input_mixed_low), expected_mixed_low)
})

test_that("clean_transcript removes low confidence segments", {
  s <- silent()

  # Base data with confidence scores
  input_template <- dplyr::tibble(
    start = 1:5,
    end = 2:6,
    text = c("text1", "text2", "text3", "text4", "text5"),
    avg_logprob = c(-0.6, -0.4, -0.7, -0.2, NA_real_),
    no_speech_prob = c(0.95, 0.95, 0.8, 0.99, 0.99)
    # speaker column to test its preservation
    # speaker = paste0("spk", 1:5)
  )
  # Add a speaker column to ensure it's preserved
  input_with_speaker <- dplyr::mutate(
    input_template,
    speaker = paste0("spk", 1:5)
  )

  # Expected outcomes:
  # text1: avg_logprob < -0.5 AND no_speech_prob > 0.9  -> silent
  # text2: avg_logprob >= -0.5                           -> stays text2
  # text3: no_speech_prob <= 0.9                         -> stays text3
  # text4: avg_logprob >= -0.5 (but no_speech_prob >0.9) -> stays text4
  # text5: NA in avg_logprob                             -> stays text5 (condition is NA)

  expected_text_after_confidence <- c(s, "text2", "text3", "text4", "text5")

  # Expected data frame structure after clean_transcript
  # (avg_logprob and no_speech_prob columns removed)
  expected_df_structure <- dplyr::tibble(
    start = c(1L, 2L, 3L, 4L, 5L), # Ensure integer type
    end = c(2L, 3L, 4L, 5L, 6L),
    text = expected_text_after_confidence,
    speaker = paste0("spk", 1:5) # Speaker column preserved
  )

  result_data <- clean_transcript(input_with_speaker)
  expect_identical(result_data, expected_df_structure)

  # Case: What if only one confidence column is present? Rule should not apply.
  input_missing_no_speech <- input_with_speaker |>
    dplyr::select(-no_speech_prob)
  # Expect no changes due to confidence, and avg_logprob NOT removed
  expected_missing_no_speech <- input_missing_no_speech
  expect_identical(
    clean_transcript(input_missing_no_speech),
    expected_missing_no_speech
  )

  input_missing_avg_logprob <- input_with_speaker |> dplyr::select(-avg_logprob)
  # Expect no changes due to confidence, and no_speech_prob NOT removed
  expected_missing_avg_logprob <- input_missing_avg_logprob
  expect_identical(
    clean_transcript(input_missing_avg_logprob),
    expected_missing_avg_logprob
  )

  # Case: No confidence columns present. Should run without error.
  input_no_confidence <- dplyr::tibble(
    start = 1L,
    end = 2L,
    text = "text_no_conf",
    speaker = "spk_a"
  )
  expected_no_confidence <- input_no_confidence # no change, no columns removed
  expect_identical(
    clean_transcript(input_no_confidence),
    expected_no_confidence
  )

  # Case: Confidence columns present, but all text becomes silent due to other rules first
  # e.g. all text is "" or NA initially
  input_all_silent_initially <- dplyr::tibble(
    start = 1:2,
    end = 2:3,
    text = c(NA_character_, ""),
    avg_logprob = c(-0.6, -0.7),
    no_speech_prob = c(0.95, 0.96),
    speaker = c("spkA", "spkB")
  )
  # text becomes c(s,s) from NA/"" normalization. Confidence rule still applies
  # to original row structure but text is already silent. Columns should be removed.
  expected_all_silent <- dplyr::tibble(
    start = c(1L, 2L),
    end = c(2L, 3L),
    text = c(s, s),
    speaker = c("spkA", "spkB")
  )
  expect_identical(
    clean_transcript(input_all_silent_initially),
    expected_all_silent
  )
})

# Tests for extract_text_from_transcript() ----

test_that("extract_text_from_transcript errors when text column is missing", {
  transcript_missing_text <- dplyr::tibble(
    start = c(0L, 10L),
    end = c(5L, 15L),
    speaker = c("s1", "s2")
  )

  expect_error(
    extract_text_from_transcript(transcript_missing_text),
    "Transcript data must contain.*start.*end.*text"
  )
})

test_that("extract_text_from_transcript extracts full transcript if no times given", {
  s <- silent() # "[...]"

  # Sample transcript data
  sample_transcript <- dplyr::tibble(
    start = c(0, 10, 20, 30, 40, 50, 60),
    end = c(10, 20, 30, 40, 50, 60, 70),
    text = c(
      "Hello",
      "world",
      s,
      "Speaker 2 here",
      "Yes",
      "Another from Spk2",
      s
    ),
    speaker = c("Alice", "Alice", "Alice", "Bob", "Bob", "Bob", "Bob")
  )

  # Expected output WITH diarization
  expected_with_diarization <- paste(
    "Speaker: Alice\nHello\nworld",
    "Speaker: Bob\nSpeaker 2 here\nYes\nAnother from Spk2",
    sep = "\n\n"
  )
  # Warning no longer expected
  result1 <- extract_text_from_transcript(
    sample_transcript,
    import_diarization = TRUE
  )
  expect_identical(result1, expected_with_diarization)

  # Expected output WITHOUT diarization
  expected_without_diarization <- paste(
    "Hello\nworld",
    "Speaker 2 here\nYes\nAnother from Spk2",
    sep = "\n\n"
  )
  # Warning no longer expected
  result2 <- extract_text_from_transcript(
    sample_transcript,
    import_diarization = FALSE
  )
  expect_identical(result2, expected_without_diarization)

  # Define expected text when all segments are combined into one block
  expected_single_block_text <- paste(
    c("Hello", "world", "Speaker 2 here", "Yes", "Another from Spk2"),
    collapse = "\n"
  )

  # Scenario: No speaker column
  transcript_no_speaker <- dplyr::select(sample_transcript, -speaker)
  # Warning no longer expected
  result3 <- extract_text_from_transcript(transcript_no_speaker) # diarization TRUE by default
  expect_identical(result3, expected_single_block_text)

  # Scenario: All NA speaker
  transcript_na_speaker <- sample_transcript
  transcript_na_speaker$speaker <- NA_character_
  # Warning no longer expected
  result4 <- extract_text_from_transcript(transcript_na_speaker) # diarization TRUE by default
  expect_identical(result4, expected_single_block_text)

  # Scenario: Only one distinct speaker
  transcript_one_speaker <- sample_transcript
  transcript_one_speaker$speaker <- "Carol"
  # Warning no longer expected
  result5 <- extract_text_from_transcript(transcript_one_speaker) # diarization TRUE by default
  expect_identical(result5, expected_single_block_text)

  # Test for warning with long transcript (duration > 3600 seconds)
  long_transcript <- dplyr::tibble(
    start = c(0, 4000), # Max end is 4010, min start is 0. Duration = 4010 > 3600
    end = c(10, 4010),
    text = c("short1", "short2"),
    speaker = c("A", "A") # Single speaker, so diarization will be off
  )
  {
    res_long_warn <- extract_text_from_transcript(long_transcript)
  } |>
    expect_warning("Summarising a transcript covering more than 1 hour")
  # The .data deprecation warning is no longer expected here either

  expected_long_no_diar <- "short1\nshort2"
  expect_identical(
    # If the duration warning is the only one, suppressWarnings will handle it.
    # If other unexpected warnings appear, this test might become flaky.
    suppressWarnings(extract_text_from_transcript(long_transcript)),
    expected_long_no_diar
  )
})

test_that("extract_text_from_transcript uses agenda element times", {
  s <- silent()

  sample_transcript <- dplyr::tibble(
    start = c(0, 10, 20, 30, 40, 50, 60),
    end = c(10, 20, 30, 40, 50, 60, 70),
    text = c(
      "First",
      "Second",
      s,
      "Third (Bob)",
      "Fourth (Bob)",
      "Fifth (Alice)",
      s
    ),
    speaker = c("Alice", "Alice", "Alice", "Bob", "Bob", "Alice", "Alice")
  )
  agenda <- list(from = 10, to = 50) # Numeric seconds

  expected_text_agenda_diar <- paste(
    "Speaker: Alice\nSecond",
    "Speaker: Bob\nThird (Bob)\nFourth (Bob)",
    sep = "\n\n"
  )
  # Warning no longer expected
  result1 <- extract_text_from_transcript(
    sample_transcript,
    agenda_element = agenda
  )
  expect_identical(result1, expected_text_agenda_diar)

  expected_text_agenda_no_diar <- paste(
    "Second",
    "Third (Bob)\nFourth (Bob)",
    sep = "\n\n"
  )
  # Warning no longer expected
  result2 <- extract_text_from_transcript(
    sample_transcript,
    agenda_element = agenda,
    import_diarization = FALSE
  )
  expect_identical(result2, expected_text_agenda_no_diar)

  agenda_empty <- list(from = 100, to = 200)
  # Warning no longer expected
  result3 <- extract_text_from_transcript(
    sample_transcript,
    agenda_element = agenda_empty
  )
  expect_identical(result3, "")

  agenda_one_segment <- list(from = 0, to = 10)
  expected_one_segment_diar <- "Speaker: Alice\nFirst"
  # Warning no longer expected
  result4 <- extract_text_from_transcript(
    sample_transcript,
    agenda_element = agenda_one_segment
  )
  expect_identical(result4, expected_one_segment_diar)

  expected_one_segment_no_diar <- "First"
  # Warning no longer expected
  result5 <- extract_text_from_transcript(
    sample_transcript,
    agenda_element = agenda_one_segment,
    import_diarization = FALSE
  )
  expect_identical(result5, expected_one_segment_no_diar)

  agenda_one_speaker <- list(from = 30, to = 50)
  expected_one_speaker_diar <- "Speaker: Bob\nThird (Bob)\nFourth (Bob)"
  # Warning no longer expected
  result6 <- extract_text_from_transcript(
    sample_transcript,
    agenda_element = agenda_one_speaker
  )
  expect_identical(result6, expected_one_speaker_diar)

  expected_one_speaker_no_diar <- "Third (Bob)\nFourth (Bob)"
  # Warning no longer expected
  result7 <- extract_text_from_transcript(
    sample_transcript,
    agenda_element = agenda_one_speaker,
    import_diarization = FALSE
  )
  expect_identical(result7, expected_one_speaker_no_diar)
})

test_that("extract_text_from_transcript uses explicit start/end times", {
  s <- silent()

  sample_transcript <- dplyr::tibble(
    start = c(0, 10, 20, 30, 40),
    end = c(10, 20, 30, 40, 50),
    text = c("A", "B", s, "D", "E"),
    speaker = c("S1", "S1", "S1", "S2", "S2")
  )
  expected_text <- paste("Speaker: S1\nB", "Speaker: S2\nD", sep = "\n\n")
  # Warning no longer expected
  result1 <- extract_text_from_transcript(
    sample_transcript,
    start_time = 10,
    end_time = 40
  )
  expect_identical(result1, expected_text)

  expected_text_no_diar <- paste("B", "D", sep = "\n\n")
  # Warning no longer expected
  result2 <- extract_text_from_transcript(
    sample_transcript,
    start_time = 10,
    end_time = 40,
    import_diarization = FALSE
  )
  expect_identical(result2, expected_text_no_diar)
})

test_that("extract_text_from_transcript errors for invalid agenda times", {
  sample_transcript <- dplyr::tibble(
    start = 0L,
    end = 1L,
    text = "t",
    speaker = "s"
  )
  agenda_bad_from <- list(from = "bad", to = 10)
  agenda_bad_to <- list(from = 0, to = "bad")
  agenda_missing_from <- list(to = 10)
  agenda_missing_to <- list(from = 0)

  # The error message is specific about numeric format
  msg <- "Agenda element must contain 'from' and 'to' fields in numeric format."

  expect_error(
    extract_text_from_transcript(
      sample_transcript,
      agenda_element = agenda_bad_from
    ),
    msg
  )
  expect_error(
    extract_text_from_transcript(
      sample_transcript,
      agenda_element = agenda_bad_to
    ),
    msg
  )
  # The function also expects both fields to be present if agenda_element is not NULL
  expect_error(
    extract_text_from_transcript(
      sample_transcript,
      agenda_element = agenda_missing_from
    ),
    msg
  )
  expect_error(
    extract_text_from_transcript(
      sample_transcript,
      agenda_element = agenda_missing_to
    ),
    msg
  )
})

# Tests for merge_transcripts() ----

test_that("merge_transcripts basic merging works (no diarization)", {
  s <- silent() # "[...]"

  transcript_x_refined <- dplyr::tibble(
    start = c(0L, 10L, 16L, 22L, 30L),
    end = c(5L, 15L, 21L, 28L, 35L),
    text = c("AAA", s, s, s, "BBB")
  )
  transcript_y_refined <- dplyr::tibble(
    start = c(10L, 20L),
    end = c(15L, 29L),
    text = c("YYY1", "YYY2_covers_two")
  )

  # Expected based on careful trace:
  # x[2] (10-15,s) gets YYY1.
  # x[4] (22-28,s) gets YYY2_covers_two (start/end becomes 20-29 from y).
  # x[3] (16-21,s) remains untouched and silent.
  # No segments are removed by the redundancy check in this scenario.
  expected_data_corrected <- dplyr::tibble(
    start = c(0L, 10L, 16L, 20L, 30L),
    end = c(5L, 15L, 21L, 29L, 35L),
    text = c("AAA", "YYY1", s, "YYY2_covers_two", "BBB")
  )

  {
    result <- merge_transcripts(
      transcript_x_refined,
      transcript_y_refined,
      import_diarization = FALSE
    )
  } |>
    expect_message("Added 2 segments.*Removed 0 segments")

  expect_identical(result, expected_data_corrected)
})

test_that("merge_transcripts handles no empty segments", {
  testthat::skip("GloVe-based diarization import is soft-deprecated.")
  s <- silent()
  transcript_x_no_empty <- dplyr::tibble(
    start = c(0L, 10L),
    end = c(5L, 15L),
    text = c("Text A", "Text B"),
    speaker = c("spk_x1", "spk_x2") # Original speakers in x
  )
  # transcript_y is needed for the diarization import scenario
  transcript_y_targeted_speakers <- dplyr::tibble(
    start = c(0L, 2L, 10L, 12L), # Times to ensure they fall in ranges for x1 and x2
    end = c(1L, 3L, 11L, 13L),
    text = c("y_a1", "y_a2", "y_b1", "y_b2"), # Text content of y doesn't matter for x text
    speaker = c(
      "SPEAKER_FOR_A",
      "SPEAKER_FOR_A",
      "SPEAKER_FOR_B",
      "SPEAKER_FOR_B"
    )
  )

  # Scenario 1: import_diarization = FALSE
  expect_message(
    result_no_diar <- merge_transcripts(
      transcript_x_no_empty,
      transcript_y_targeted_speakers, # y can be anything here, not used for text
      import_diarization = FALSE
    ),
    "No empty segments found in the transcript"
  )
  # clean_transcript is called at the end of merge_transcripts.
  # For this input, clean_transcript(transcript_x_no_empty) is
  # transcript_x_no_empty.
  expect_identical(result_no_diar, transcript_x_no_empty)

  # Scenario 2: import_diarization = TRUE
  # No text segments in x should change. Speakers in x should be updated.
  expected_data_imputed_speakers <- dplyr::tibble(
    start = c(0L, 10L),
    end = c(5L, 15L),
    text = c("Text A", "Text B"),
    speaker = c("SPEAKER_FOR_A", "SPEAKER_FOR_B") # Imputed speakers
  )

  mock_glove_model <- matrix(1, dimnames = list("a", "v1")) # Dummy model

  # Mock compute_text_sim to control speaker assignment
  # y_probes created inside merge_transcripts will group y_texts by speaker.
  # For transcript_y_targeted_speakers, y_probes$text will be like:
  # c("y_a1 y_a2", "y_b1 y_b2") corresponding to speakers
  # c("SPEAKER_FOR_A", "SPEAKER_FOR_B")
  mock_compute_text_sim <- function(x_text, y_texts, embedding_matrix) {
    if (x_text == "Text A") {
      # Higher similarity for SPEAKER_FOR_A's text
      return(c(0.9, 0.1))
    } else if (x_text == "Text B") {
      # Higher similarity for SPEAKER_FOR_B's text
      return(c(0.1, 0.9))
    }
    # Default for any other unexpected calls
    return(rep(NA_real_, length(y_texts)))
  }

  testthat::local_mocked_bindings(
    generate_glove_model = function(...) mock_glove_model,
    compute_text_sim = mock_compute_text_sim,
    .package = "minutemaker"
  )

  {
    result_imputed_speakers <- merge_transcripts(
      transcript_x_no_empty,
      transcript_y_targeted_speakers,
      import_diarization = TRUE
    )
  } |>
    expect_message("Merging") |>
    expect_message("Added 0 segments.*Removed 0 segments") |>
    expect_message("Importing diarization...")

  expect_identical(result_imputed_speakers, expected_data_imputed_speakers)
})

test_that("merge_transcripts handles transcripts with no overlap", {
  testthat::skip("GloVe-based diarization import is soft-deprecated.")
  s <- silent()
  # Transcript X: Segments from 0-5s and 10-15s
  transcript_x_no_overlap <- dplyr::tibble(
    start = c(0L, 10L),
    end = c(5L, 15L),
    text = c("Text A from X", "Text B from X"),
    speaker = c("spk_x1", "spk_x2")
  )
  # Transcript Y: Segments from 20-25s and 30-35s (no overlap with X)
  transcript_y_no_overlap <- dplyr::tibble(
    start = c(20L, 30L),
    end = c(25L, 35L),
    text = c("Text C from Y", "Text D from Y"),
    speaker = c("spk_y1", "spk_y2")
  )

  # Expected result when import_diarization = FALSE:
  # Should be transcript_x after clean_transcript (which does nothing here)
  expected_no_diar <- clean_transcript(transcript_x_no_overlap)

  {
    result_no_diar <- merge_transcripts(
      transcript_x_no_overlap,
      transcript_y_no_overlap,
      import_diarization = FALSE
    )
  } |>
    expect_message("No empty segments found in the transcript")

  expect_identical(result_no_diar, expected_no_diar)

  # Expected result when import_diarization = TRUE:
  # Text should still be from X. Speakers might change based on Y.
  # For this test, assume speakers from X are re-evaluated against Y.
  # If X's text is more similar to spk_y1 or spk_y2.
  # Mock compute_text_sim to control this.
  expected_diar_imputed <- dplyr::tibble(
    start = c(0L, 10L),
    end = c(5L, 15L),
    text = c("Text A from X", "Text B from X"),
    speaker = c("spk_y1", "spk_y2") # Arbitrarily assign Y speakers
  )

  # Dummy model for generate_glove_model
  mock_glove_model <- matrix(1, dimnames = list("a", "v1"))

  # Mock compute_text_sim:
  # y_probes from transcript_y_no_overlap will have texts:
  # c("Text C from Y", "Text D from Y") and speakers c("spk_y1", "spk_y2")
  mock_compute_text_sim_no_overlap <- function(
    x_text,
    y_texts,
    embedding_matrix
  ) {
    if (x_text == "Text A from X") {
      return(c(0.9, 0.1)) # Assign spk_y1
    } else if (x_text == "Text B from X") {
      return(c(0.1, 0.9)) # Assign spk_y2
    }
    return(rep(NA_real_, length(y_texts)))
  }

  testthat::local_mocked_bindings(
    generate_glove_model = function(...) mock_glove_model,
    compute_text_sim = mock_compute_text_sim_no_overlap,
    .package = "minutemaker"
  )

  {
    result_diar <- merge_transcripts(
      transcript_x_no_overlap,
      transcript_y_no_overlap,
      import_diarization = TRUE
    )
  } |>
    expect_message("Merging") |>
    expect_message("Added 0 segments.*Removed 0 segments") |>
    expect_message("Importing diarization")

  expect_identical(result_diar, expected_diar_imputed)
})

test_that("merge_transcripts handles empty transcript_y", {
  testthat::skip("GloVe-based diarization import is soft-deprecated.")
  s <- silent()
  transcript_x_standard <- dplyr::tibble(
    start = c(0L, 10L, 20L),
    end = c(5L, 15L, 25L),
    text = c("Text A", s, "Text C"), # Has a silent segment
    speaker = c("spk_x1", NA_character_, "spk_x3")
  )
  transcript_y_empty <- dplyr::tibble(
    start = integer(0),
    end = integer(0),
    text = character(0),
    speaker = character(0)
  )

  expected_x_standard <- clean_transcript(transcript_x_standard)

  # Scenario 1: import_diarization = FALSE, x has silent segments
  {
    result_standard <- merge_transcripts(
      transcript_x_standard,
      transcript_y_empty,
      import_diarization = FALSE
    )
  } |>
    expect_message("Added 0 segments.*Removed 0 segments")

  expect_identical(result_standard, expected_x_standard)

  # Scenario 2: import_diarization = TRUE, x has silent segments
  # When y is empty, speakers are cleared to NA.
  expected_diar <- expected_x_standard |>
    dplyr::mutate(speaker = NA_character_)

  mock_glove_model <- matrix(1, dimnames = list("a", "v1"))
  testthat::local_mocked_bindings(
    generate_glove_model = function(...) mock_glove_model,
    compute_text_sim = function(...)
      stop(
        "compute_text_sim should not be called if diarization runs with empty y"
      ),
    .package = "minutemaker"
  )

  {
    result_diar <- merge_transcripts(
      transcript_x_standard,
      transcript_y_empty,
      import_diarization = TRUE
    )
  } |>
    expect_message("Merging") |>
    expect_message("Added 0 segments.*Removed 0 segments") |>
    expect_message("Importing diarization...")
  expect_identical(result_diar, expected_diar)

  # Scenario 3: import_diarization = FALSE, x has NO silent segments
  transcript_x_no_silent <- dplyr::tibble(
    start = c(0L, 10L),
    end = c(5L, 15L),
    text = c("A", "B"),
    speaker = c("s1", "s2")
  )
  expected_x_no_silent_cleaned <- clean_transcript(transcript_x_no_silent)

  expect_message(
    res_x_no_silent <- merge_transcripts(
      transcript_x_no_silent,
      transcript_y_empty,
      import_diarization = FALSE
    ),
    "No empty segments found in the transcript"
  )
  expect_identical(res_x_no_silent, expected_x_no_silent_cleaned)
})

test_that("merge_transcripts handles empty transcript_x", {
  testthat::skip("GloVe-based diarization import is soft-deprecated.")
  transcript_x_empty_base <- dplyr::tibble(
    start = integer(0),
    end = integer(0),
    text = character(0)
  )
  transcript_y_standard <- dplyr::tibble(
    start = c(0L, 10L),
    end = c(5L, 15L),
    text = c("Text A from Y", "Text B from Y"),
    speaker = c("spk_y1", "spk_y2")
  )

  # Scenario 1: import_diarization = FALSE, x is empty (no speaker col)
  expected_s1 <- clean_transcript(transcript_x_empty_base)
  {
    result_s1 <- merge_transcripts(
      transcript_x_empty_base,
      transcript_y_standard,
      import_diarization = FALSE
    )
  } |>
    expect_message("No empty segments found in the transcript")

  expect_identical(result_s1, expected_s1)

  # Scenario 2: import_diarization = TRUE, x is empty (no speaker col)
  expected_s2 <- dplyr::tibble(
    start = integer(0),
    end = integer(0),
    text = character(0),
    speaker = character(0)
  )
  mock_glove_model <- matrix(1, dimnames = list("a", "v1"))
  testthat::local_mocked_bindings(
    generate_glove_model = function(...) mock_glove_model,
    compute_text_sim = function(...)
      stop("compute_text_sim should not be called if x is empty"),
    .package = "minutemaker"
  )
  {
    result_s2 <- merge_transcripts(
      transcript_x_empty_base,
      transcript_y_standard,
      import_diarization = TRUE
    )
  } |>
    expect_message("Merging") |>
    expect_message("Added 0 segments.*Removed 0 segments") |>
    expect_message("Importing diarization")

  expect_identical(result_s2, expected_s2)

  # Scenario 3: import_diarization = FALSE, x is empty WITH speaker = char(0)
  transcript_x_empty_with_speaker_char <- dplyr::tibble(
    start = integer(0),
    end = integer(0),
    text = character(0),
    speaker = character(0)
  )
  expected_s3_no_diar <- clean_transcript(transcript_x_empty_with_speaker_char)
  {
    result_s3_no_diar <- merge_transcripts(
      transcript_x_empty_with_speaker_char,
      transcript_y_standard,
      import_diarization = FALSE
    )
  } |>
    expect_message("No empty segments found in the transcript\\.")

  expect_identical(result_s3_no_diar, expected_s3_no_diar)

  # Scenario 4: import_diarization = TRUE, x is empty WITH speaker = char(0)
  expected_s4_diar <- dplyr::tibble(
    start = integer(0),
    end = integer(0),
    text = character(0),
    speaker = character(0)
  )
  testthat::local_mocked_bindings(
    generate_glove_model = function(...) mock_glove_model,
    compute_text_sim = function(...) {
      stop("compute_text_sim should not be called if x is empty")
    },
    .package = "minutemaker"
  )
  {
    result_s4_diar <- merge_transcripts(
      transcript_x_empty_with_speaker_char,
      transcript_y_standard,
      import_diarization = TRUE
    )
  } |>
    expect_message("Merging") |>
    expect_message("Added 0 segments.*Removed 0 segments") |>
    expect_message("Importing diarization")
  expect_identical(result_s4_diar, expected_s4_diar)
})

test_that("merge_transcripts handles empty speaker values", {
  testthat::skip("GloVe-based diarization import is soft-deprecated.")
  s <- silent()
  empty_spk <- "" # Speaker name that is an empty string
  na_spk <- NA_character_ # Speaker name that is NA

  transcript_x_base <- dplyr::tibble(
    start = c(0L, 10L, 20L, 30L, 40L, 50L),
    end = c(5L, 15L, 25L, 35L, 45L, 55L),
    text = c("X_txt1", s, "X_txt3", s, "X_txt5", s),
    speaker = c("spk_x1", na_spk, "spk_x3", empty_spk, "spk_x5", na_spk)
  )

  transcript_y_varied_spk <- dplyr::tibble(
    start = c(10L, 30L, 50L),
    end = c(15L, 35L, 55L),
    text = c("Y_fill_NA", "Y_fill_empty", "Y_fill_spkY3"),
    speaker = c(na_spk, empty_spk, "spk_y3")
  )

  ## Scenario 1: import_diarization = FALSE ----
  # Expected: "" speaker in x becomes NA in output.
  # Message: Added/Removed combined into one string by capture_messages here.
  expected_s1 <- dplyr::tibble(
    start = c(0L, 10L, 20L, 30L, 40L, 50L),
    end = c(5L, 15L, 25L, 35L, 45L, 55L),
    text = c(
      "X_txt1",
      "Y_fill_NA",
      "X_txt3",
      "Y_fill_empty",
      "X_txt5",
      "Y_fill_spkY3"
    ),
    speaker = c("spk_x1", na_spk, "spk_x3", na_spk, "spk_x5", na_spk)
  )

  {
    result_s1 <- merge_transcripts(
      transcript_x_base,
      transcript_y_varied_spk,
      import_diarization = FALSE
    )
  } |>
    expect_message("Added 3 segments.*Removed 0 segments")

  expect_identical(result_s1, expected_s1)

  ## Scenario 2: import_diarization = TRUE ----
  # Expected: "" speaker from y_probes becomes NA in output.
  # Mock adjusted to reflect observed behavior where some spk_y3 also became NA.
  expected_spk_s2 <- c(na_spk, na_spk, na_spk, na_spk, "spk_y3", "spk_y3")
  expected_s2 <- dplyr::tibble(
    start = c(0L, 10L, 20L, 30L, 40L, 50L),
    end = c(5L, 15L, 25L, 35L, 45L, 55L),
    text = c(
      "X_txt1",
      "Y_fill_NA",
      "X_txt3",
      "Y_fill_empty",
      "X_txt5",
      "Y_fill_spkY3"
    ),
    speaker = expected_spk_s2
  )
  mock_glove_model_s2 <- matrix(1, dimnames = list("a", "v1"))
  mock_compute_text_sim_s2 <- function(x_text, y_texts, embedding_matrix) {
    # y_probes$speaker order assumed: NA, "", "spk_y3"
    if (x_text == "X_txt1") return(c(0.7, 0.2, 0.1)) # -> NA
    if (x_text == "Y_fill_NA") return(c(0.7, 0.2, 0.1)) # -> NA
    if (x_text == "X_txt3") return(c(0.2, 0.7, 0.1)) # -> "" (becomes NA in output)
    if (x_text == "Y_fill_empty") return(c(0.1, 0.7, 0.2)) # -> "" (becomes NA in output)
    if (x_text == "X_txt5") return(c(0.1, 0.2, 0.7)) # -> "spk_y3"
    if (x_text == "Y_fill_spkY3") return(c(0.1, 0.2, 0.7)) # -> "spk_y3"
    return(c(0.7, 0.2, 0.1)) # Default to NA for any other texts
  }
  testthat::local_mocked_bindings(
    generate_glove_model = function(...) mock_glove_model_s2,
    compute_text_sim = mock_compute_text_sim_s2,
    .package = "minutemaker"
  )
  {
    result_s2 <- merge_transcripts(
      transcript_x_base,
      transcript_y_varied_spk,
      import_diarization = TRUE
    )
  } |>
    expect_message("Merging") |>
    expect_message("Added 3 segments.*Removed 0 segments") |>
    expect_message("Importing diarization")

  expect_identical(result_s2, expected_s2)

  ## Scenario 3: import_diarization = TRUE, x has no speaker column ----
  transcript_x_no_spk_col <- dplyr::select(transcript_x_base, -speaker)
  expected_s3 <- expected_s2 # Same expectations as S2 for data and messages
  testthat::local_mocked_bindings(
    generate_glove_model = function(...) mock_glove_model_s2,
    compute_text_sim = mock_compute_text_sim_s2,
    .package = "minutemaker"
  )
  {
    result_s3 <- merge_transcripts(
      transcript_x_no_spk_col,
      transcript_y_varied_spk,
      import_diarization = TRUE
    )
  } |>
    expect_message("Merging") |>
    expect_message("Added 3 segments.*Removed 0 segments") |>
    expect_message("Importing diarization")
  expect_identical(result_s3, expected_s3)

  ## Scenario 4: import_diarization = TRUE, y has no speaker column ----
  transcript_y_no_spk_col <- dplyr::select(transcript_y_varied_spk, -speaker)
  mock_glove_model_s4 <- matrix(1, dimnames = list("a", "v1"))
  testthat::local_mocked_bindings(
    generate_glove_model = function(...) mock_glove_model_s4,
    .package = "minutemaker"
  )

  # The call to merge_transcripts is expected to error out.
  # On its way to erroring, it's also expected to produce a warning.
  merge_transcripts(
    transcript_x_base,
    transcript_y_no_spk_col,
    import_diarization = TRUE
  ) |>
    expect_warning("Unknown or uninitialised column: `speaker`") |>
    expect_error("Column `speaker` not found")
})

test_that("merge_transcripts handles segments too short for similarity", {
  testthat::skip("GloVe-based diarization import is soft-deprecated.")
  s <- silent()
  na_spk <- NA_character_

  # Transcript X: some normal, some silent, one originally short
  transcript_x_initial <- dplyr::tibble(
    start = c(0L, 10L, 20L, 30L, 40L, 50L),
    end = c(5L, 15L, 25L, 35L, 45L, 55L),
    text = c("Normal X1", s, "Normal X3", s, "X5short", s),
    speaker = c("spk_X1", na_spk, "spk_X3", na_spk, "spk_X5", na_spk)
  )

  # Transcript Y: some normal text, some very short text to fill x's silent segments
  transcript_y_source <- dplyr::tibble(
    start = c(10L, 30L, 50L), # To fill x[2], x[4], x[6]
    end = c(15L, 35L, 55L),
    text = c("Normal Y_fill1", "Y_short_fill2", "Y_veryshort_fill3"), # Y_short_fill2 and Y_veryshort_fill3 are problematic
    speaker = c("spk_Y_norm", "spk_Y_short", "spk_Y_veryshort")
  )

  ## Scenario 1: import_diarization = TRUE ----
  expected_text_s1 <- c(
    "Normal X1",
    "Normal Y_fill1",
    "Normal X3",
    "Y_short_fill2",
    "X5short",
    "Y_veryshort_fill3"
  )
  expected_speakers_s1 <- c(
    "spk_Y_norm",
    "spk_Y_norm",
    "spk_Y_norm",
    na_spk,
    na_spk,
    na_spk
  )
  expected_data_s1 <- dplyr::tibble(
    start = c(0L, 10L, 20L, 30L, 40L, 50L),
    end = c(5L, 15L, 25L, 35L, 45L, 55L),
    text = expected_text_s1,
    speaker = expected_speakers_s1
  )
  mock_glove <- matrix(
    c(1, 2),
    ncol = 1,
    dimnames = list(c("normal", "y_fill1"), "d1")
  )
  mock_sim <- function(x_text, y_texts, embedding_matrix) {
    num_y_probes <- length(y_texts)
    res <- rep(0.1, num_y_probes)
    if (x_text %in% c("X5short", "Y_short_fill2", "Y_veryshort_fill3")) {
      return(rep(NA_real_, num_y_probes))
    }
    if (x_text %in% c("Normal X1", "Normal Y_fill1", "Normal X3")) {
      idx_of_norm_y_fill1 <- match("Normal Y_fill1", y_texts)
      if (!is.na(idx_of_norm_y_fill1)) {
        res[idx_of_norm_y_fill1] <- 0.9
      }
    }
    return(res)
  }
  testthat::local_mocked_bindings(
    generate_glove_model = function(...) mock_glove,
    compute_text_sim = mock_sim,
    .package = "minutemaker"
  )
  {
    result_s1 <- merge_transcripts(
      transcript_x_initial,
      transcript_y_source,
      import_diarization = TRUE
    )
  } |>
    expect_message("Merging") |>
    expect_message("Added 3 segments.*Removed 0 segments") |>
    expect_message("Importing diarization")

  expect_identical(result_s1, expected_data_s1)

  ## Scenario 2: import_diarization = FALSE (Control) ----
  expected_data_s2 <- dplyr::tibble(
    start = c(0L, 10L, 20L, 30L, 40L, 50L),
    end = c(5L, 15L, 25L, 35L, 45L, 55L),
    text = expected_text_s1, # Text merging still happens
    speaker = c("spk_X1", na_spk, "spk_X3", na_spk, "spk_X5", na_spk) # Original speakers from x
  )

  {
    result_s2 <- merge_transcripts(
      transcript_x_initial,
      transcript_y_source,
      import_diarization = FALSE
    )
  } |>
    expect_message("Added 3 segments.*Removed 0 segments")

  expect_identical(result_s2, expected_data_s2)
})

test_that("merge_transcripts imports diarization correctly", {
  testthat::skip("GloVe-based diarization import is soft-deprecated.")
  s <- silent()

  transcript_x_initial <- dplyr::tibble(
    start = c(0L, 10L, 20L, 30L, 40L), # x[2] and x[4] are silent
    end = c(5L, 15L, 25L, 35L, 45L),
    text = c("Original X1", s, "Original X3", s, "Original X5"),
    speaker = c("SPK_X1", NA_character_, "SPK_X3", NA_character_, "SPK_X5")
  )

  transcript_y_simple_fill <- dplyr::tibble(
    start = c(10L, 30L), # One segment for x[2], one for x[4]
    end = c(15L, 35L),
    text = c("Combined fill Y_A", "Combined fill Y_B"),
    speaker = c("SPEAKER_A", "SPEAKER_B")
  )

  # Based on the current implementation, merge_transcripts with
  # import_diarization=TRUE
  # will re-diarize ALL non-silent segments in the merged transcript_x using
  # speakers from transcript_y.
  # Original speakers in transcript_x that are not part of transcript_y will
  # be lost.
  expected_merged_diag_actual_behavior <- dplyr::tibble(
    start = c(0L, 10L, 20L, 30L, 40L),
    end = c(5L, 15L, 25L, 35L, 45L),
    text = c(
      "Original X1",
      "Combined fill Y_A",
      "Original X3",
      "Combined fill Y_B",
      "Original X5"
    ),
    speaker = c("SPEAKER_A", "SPEAKER_A", "SPEAKER_A", "SPEAKER_B", "SPEAKER_A")
  )

  mock_glove_model <- matrix(1, dimnames = list("a", "v1"))

  testthat::local_mocked_bindings(
    generate_glove_model = function(...) mock_glove_model,
    compute_text_sim = function(x_text, y_texts, embedding_matrix) {
      # y_texts comes from y_probes$text, which for transcript_y_simple_fill is:
      # c("Combined fill Y_A", "Combined fill Y_B")
      # Their speakers are c("SPEAKER_A", "SPEAKER_B") respectively.
      if (x_text == "Combined fill Y_A") {
        return(c(1.0, 0.0)) # Prefers SPEAKER_A
      } else if (x_text == "Combined fill Y_B") {
        return(c(0.0, 1.0)) # Prefers SPEAKER_B
      } else {
        # For "Original X1", "Original X3", "Original X5" This mock will make
        # them all prefer SPEAKER_A (due to which.max on c(0.5, 0.5) taking
        # first)
        return(c(0.5, 0.5))
      }
    },
    .package = "minutemaker"
  )

  {
    result_merged_diag <- merge_transcripts(
      transcript_x_initial,
      transcript_y_simple_fill,
      import_diarization = TRUE
    )
  } |>
    expect_message("Merging") |>
    expect_message("Added 2 segments.*Removed 0 segments") |>
    expect_message("Importing diarization")

  expect_identical(result_merged_diag, expected_merged_diag_actual_behavior)
})
