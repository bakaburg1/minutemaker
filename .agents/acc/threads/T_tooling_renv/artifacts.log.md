# 2026-01-24

- Observed reproducible segfault in tools:::.check_packages_used() and devtools::test() only when renv autoload was active.
- Verified devtools::check() passes with renv disabled and with explicit renv::activate() after rebuild.
- Fix: moved renv project library aside and ran renv::restore(rebuild = TRUE); segfault resolved.
