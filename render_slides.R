#!/usr/bin/env Rscript
# Render xaringan decks for Applied Data Analysis.
#
#   Rscript render_slides.R 7_week/7_week_p_sk.Rmd    # one deck
#   Rscript render_slides.R 5_week                    # every deck in a folder
#   Rscript render_slides.R                           # everything
#
# NOTE: a UTF-8 locale is required. In the C locale the graphics devices
# escape Slovak diacritics as <U+00E1> instead of drawing them.

if (!grepl("UTF-8", Sys.getlocale("LC_CTYPE"), fixed = TRUE)) {
  stop("Non-UTF-8 locale (", Sys.getlocale("LC_CTYPE"), "). ",
       "Re-run as: LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript render_slides.R ...")
}

.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))

args <- commandArgs(trailingOnly = TRUE)
targets <- if (length(args) == 0) list.files(pattern = "_week$") else args

files <- unlist(lapply(targets, function(t) {
  if (dir.exists(t)) list.files(t, pattern = "^[0-9]+_week_[pe](_sk)?\\.Rmd$", full.names = TRUE) else t
}))

cat("Rendering", length(files), "deck(s)\n\n")
failed <- character()
for (f in files) {
  cat("---", f, "---\n")
  ok <- tryCatch({
    rmarkdown::render(f, quiet = TRUE, envir = new.env()); TRUE
  }, error = function(e) { cat("  FAILED:", conditionMessage(e), "\n"); FALSE })
  if (!ok) failed <- c(failed, f)
}

cat("\n=== ", length(files) - length(failed), "/", length(files), " succeeded ===\n", sep = "")
if (length(failed)) { cat("Failed:\n"); cat(paste0("  ", failed, collapse = "\n"), "\n") }
