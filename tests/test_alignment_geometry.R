# Tests for the particle geometry used by the app's ERM alignments (app.R):
# polydisperse widths/heights (ScottCoffin/ToMEx2.0_EcoToxRisk#8).
# Run from the repository root:
#   Rscript -e "testthat::test_file('tests/test_alignment_geometry.R')"
# The same rules are in PSSDplusplus::align_data() (ToMEx2.0_EcoToxRisk).

library(testthat)
suppressPackageStartupMessages(library(dplyr))

app_file <- if (file.exists("app.R")) "app.R" else "../app.R"
data_file <- if (file.exists("aoc_z_tomex2.RDS")) "aoc_z_tomex2.RDS" else "../aoc_z_tomex2.RDS"
app <- readLines(app_file, warn = FALSE)
starts <- grep("##### ----- WIDTH ------ ###", app, fixed = TRUE)
vol <- grep("############ ------ Volume ------ ##########", app, fixed = TRUE)
ends <- vapply(starts, function(x) vol[vol > x][1], integer(1))

# run one of app.R's width/height blocks (a chain of mutate()s) on a data frame
run_block <- function(df, k) {
  code <- paste0("df %>%\n", paste(app[(starts[k] + 1):(ends[k] - 1)], collapse = "\n"),
                 "\n identity()")
  eval(parse(text = code))
}

# inputs as the alignment has them at this point: environment R.ave and H:W
# ratio, and lengths truncated at the bioaccessible limits
aoc <- readRDS(data_file) %>%
  filter(env_f %in% c("Marine", "Freshwater")) %>%
  mutate(
    R.ave = ifelse(env_f == "Marine", 0.77, 0.67),
    H_W_ratio = R.ave,
    size.length.max.um.trans = pmin(size.length.max.um.used.for.conversions, 88),
    size.length.max.um.ingest = pmin(size.length.max.um.used.for.conversions,
                                     dplyr::coalesce(max.size.ingest.um, 5000))
  )
poly <- aoc$polydispersity == "polydisperse" & !is.na(aoc$size.length.min.um.used.for.conversions)

test_that("no shape matching is lower case (ToMEx codes 'Sphere', 'Fiber', 'Fragment')", {
  for (k in seq_along(starts)) {
    blk <- app[starts[k]:ends[k]]
    expect_false(any(grepl('shape_f == "(sphere|fiber|fragment)"', blk)), label = paste("block", k))
  }
})

test_that("database blocks: every polydisperse particle has a width, spheres are spheres", {
  db_blocks <- which(!vapply(seq_along(starts), function(k)
    any(grepl("uploaded file", app[starts[k]:ends[k]], fixed = TRUE)), logical(1)))
  expect_gte(length(db_blocks), 3)
  for (k in db_blocks) {
    out <- run_block(aoc, k)
    p <- out[poly, ]
    expect_false(anyNA(p$size.width.min.um.used.for.conversions))
    s <- p$shape_f == "Sphere"
    expect_equal(p$size.width.min.um.used.for.conversions[s], p$size.length.min.um.used.for.conversions[s])
    expect_equal(p$size.height.min.um.used.for.conversions[s], p$size.length.min.um.used.for.conversions[s])
    # non-spheres: the environment's H:W ratio, also for the smallest particle
    fr <- p$shape_f == "Fragment"
    expect_equal(p$size.height.min.um.used.for.conversions[fr],
                 p$H_W_ratio[fr] * p$R.ave[fr] * p$size.length.min.um.used.for.conversions[fr])
    # fibers: one diameter, never wider than long, min width <= max width
    f <- p$shape_f == "Fiber" & !is.na(p$size.length.max.um.ingest)
    expect_true(all(p$size.width.min.um.used.for.conversions[f] <= p$size.length.min.um.used.for.conversions[f]))
    expect_true(all(p$size.width.max.um.ingest[f] <= p$size.length.max.um.ingest[f]))
    expect_true(all(p$size.width.max.um.trans[f] <= p$size.length.max.um.trans[f]))
    expect_true(all(p$size.width.min.um.used.for.conversions[f] <= p$size.width.max.um.ingest[f] |
                      p$size.width.max.um.ingest[f] == p$size.length.max.um.ingest[f]))
    # no-bioaccessibility path: largest particle of the whole distribution
    sp <- s & !is.na(p$size.length.max.um.used.for.conversions)
    expect_equal(p$size.height.max.um.used.for.conversions[sp], p$size.length.max.um.used.for.conversions[sp])
  }
})

test_that("calculator block keeps widths and heights supplied by the user", {
  k <- which(vapply(seq_along(starts), function(k)
    any(grepl("uploaded file", app[starts[k]:ends[k]], fixed = TRUE)), logical(1)))
  expect_length(k, 1)
  d <- aoc[poly, ][1:5, ]
  d$size.width.min.um.used.for.conversions <- 3
  d$size.height.min.um.used.for.conversions <- 2
  out <- run_block(d, k)
  expect_equal(out$size.width.min.um.used.for.conversions, rep(3, 5))
  expect_equal(out$size.height.min.um.used.for.conversions, rep(2, 5))
})
