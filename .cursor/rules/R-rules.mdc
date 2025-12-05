### Personality Setting Preamble

As an AI assistant, you must faithfully follow these guidelines to achieve proficiency in writing and maintaining R packages. Adopt a clear, consistent, and idiomatic approach when authoring R code, documenting functions, or organizing package structure so the output remains reliable and maintainable.

## R Style Guidelines

These rules provide a uniform, generic set of practices for authoring and documenting R code.

### 1. General Style

* Write easy-to-understand but efficient R code. Favor tidyverse functions only when they improve readability, reliability, or security.
* Avoid trivial helper functions. Split logic into helpers only when reused, error-prone, or needing targeted tests. Name helpers after the parent function with a leading `.`, and keep general helpers in separate, unexported files without the prefix.
* Use the base pipe `|>` instead of `%>%`. The lhs is inserted as the first unnamed argument; you may use a placeholder (e.g., `.`) once to position the lhs elsewhere.
* Remember that the pipe operator `|>` allows you to use the dot `.` as a placeholder for the passed data.
* With **dplyr**, prefer `.by` to `group_by()`; use `group_by()` only for multiple grouped steps or complex grouping.
* Namespace all functions except those from **dplyr**, **stringr**, and base R.
* Keep lines under 80 characters; break long statements at logical boundaries.
* Use the shorthand `\()` syntax for inline anonymous functions (e.g., `purrr::map(vec, \(x) x^2)`).
* Avoid `purrr::map_dfr()`. Use `purrr::map()` followed by `dplyr::bind_rows()` or `purrr::list_rbind()`; `bind_rows()` already drops `NULL` values.
* Do not deduplicate set operations manually (e.g., `unique(intersect(...))` is unnecessary).

### 2. Function and Helper Structure

* Function argument formatting:

  * Up to two arguments on a single line:

    ```r
    function_name <- function(arg1, arg2) {
        # Function body
    }
    ```
  * More than two arguments on separate lines:

    ```r
    function_name <- function(
        arg1,
        arg2,
        arg3
    ) {
        # Function body
    }
    ```

### 3. Documentation and Comments

* Provide high-quality documentation via function-level and inline comments; explain the what and why.
* Place comments above the code they describe, never on the same line as the code.
* Comments must start with a capital letter.
* Never describe changes you made in the comments, but describe the code in a state-less manner. Only describe changes if it needed to explain an uncommon choice.
* Roxygen documentation:
  * End sentences with a full stop, except for the title line.
  * Use backticks (`` `code` ``) instead of `\code{}` for code formatting.
  * Avoid `@importFrom` when functions are already namespaced in code.
  * Wrap at roughly 80 columns.

### 4. Messaging and Error Handling

* Use **cli** for user-facing messages:
  * `cli::cli_abort()` instead of `stop()` for terminating logic errors.
  * `cli::cli_warn()` instead of `warning()` for non-terminating logic issues.
  * `cli::cli_alert()` for neutral step headings.
  * `cli::cli_alert_info()` for informational status updates that do not imply action.
  * `cli::cli_alert_success()` for confirmations after a relevant or difficult step completes.
  * `cli::cli_alert_warning()` for non-code cautions when results need careful interpretation (e.g., low-quality input, incomplete data); use `cli::cli_warn()` for runtime logical issues that would have used `warning()`.
  * `cli::cli_alert_danger()` when circumstances make results highly unreliable (e.g., corrupted source files, missing required assets) but the code path itself is sound; use `cli::cli_abort()` for code-related faults that must stop execution.
* Never use `message()` or `cat()` for user-facing messages; always use `cli`.
* Do not manually collapse vectors in messages (e.g., `paste(x, collapse = "")` or `paste(x, collapse = ", ")`); `cli` will handle vector formatting automatically.

### 5. Pipe Usage

* Prefer the base pipe `|>` for chaining. To insert the lhs somewhere other than the first argument, use the `.` placeholder once:

  ```r
  data |> paste("value:", .)
  ```

### 6. dplyr and Data Manipulation

* When summarizing or mutating by group, prefer the `.by` argument:

  ```r
  data |>
    dplyr::summarize(
      total = sum(.data$value),
      .by = "category"
    )
  ```
* Use `group_by()` only when multiple grouped operations or complex grouping logic are required.

### 7. Additional Recommendations

* Favor built-in R functions or well-maintained tidyverse equivalents to avoid unnecessary dependencies.
* For data import/export, rely on `rio` or similar tidyverse-friendly packages when they improve safety and clarity.
* When debugging or exploring data interactively, use `print()`, `str()`, or `glimpse()` to inspect objects.

### 8. Testing, Loading, and Dependency Management

* Test frequently. Run small, focused checks during development.
* Use `Rscript -e '<R code>'` for quick one-liners and `devtools::load_all()` to load package functions during iteration.
* Never update the NAMESPACE manually. Use `devtools::document()` to regenerate documentation and namespace automatically.
* Do not call `library(pkg)` inside functions. Declare dependencies in `DESCRIPTION` under `Imports:` or `Suggests:` and namespace calls (e.g., `pkg::fun()`).
* For repeatedly used packages inside a function, you may specify `@importFrom` in roxygen, but do not duplicate imports when already namespaced (e.g., avoid `@importFrom stringr str_detect` if using `stringr::str_detect()`).
* Adding dependencies: use `usethis::use_package("pkg", type = "Imports", min_version = TRUE)`. For suggested packages, declare under `Suggests:` and check availability with `rlang::check_installed("pkg")` at runtime.

### 9. Tidy Evaluation and R CMD check

* Avoid “no visible binding for global variable” notes from non-standard evaluation in **dplyr**, **ggplot2**, and **data.table**.
* Reference data-frame columns via tidy-eval pronouns:
  * `.data$col` inside verbs such as `mutate()`, `summarise()`, `filter()`, etc.
  * `.data[[col]]` when `col` is a character vector.
  * `.env$var` for variables in the calling environment.
* In tidyselect arguments (`across()`, `.by`, `pick()`, `select()`, etc.), pass column names as strings or tidyselect helpers (not bare symbols) when writing programming-style code:

  ```r
  iris |>
    mutate(
      across(c("Sepal.Length", "Sepal.Width"), ~ .x + 1),
      Species = paste(.data$Species, "test")
    )
  ```
* For ggplot2 aesthetics, use the same pronouns to avoid global-variable notes:

  ```r
  ggplot(iris, aes(x = .data$Sepal.Length, y = .data$Sepal.Width)) +
    geom_point()
  ```
* When constructing expressions programmatically, use **rlang** helpers:

  ```r
  col <- "value"
  df |> summarise(total = sum(.data[[col]]))

  sym_col <- rlang::ensym(col)
  df |> summarise(total = sum(!!sym_col))
  ```
* Resort to `utils::globalVariables()` only when tidy-eval pronouns cannot be applied (e.g., column names produced dynamically by external code). Document the rationale in comments.

---

By following these guidelines, R code will remain consistent, readable, and maintainable across projects.
