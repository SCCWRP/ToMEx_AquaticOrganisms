# Tests for the app's ERM alignments (app.R, SSD tab block `aoc_ssd_filtered`):
# particle counts of mass-dosed tests, unit conversions, mass/SSA ERMs, the
# "none" bioaccessibility option, measured size distributions and co-dosed
# mixtures (ScottCoffin/ToMEx2.0_EcoToxRisk#8; fields in docs/test_material_fields.md).
# Run from the repository root:
#   Rscript -e "testthat::test_file('tests/test_alignment_erms.R')"
# The same rules are in PSSDplusplus::align_data() / align_mixture().

library(testthat)
suppressPackageStartupMessages(library(dplyr))

root <- if (file.exists("app.R")) "." else ".."
app <- readLines(file.path(root, "app.R"), warn = FALSE)
helpers <- new.env()
local({
  old <- setwd(root)
  on.exit(setwd(old))
  sys.source("functions.R", envir = helpers)
  sys.source("test_material_functions.R", envir = helpers)
})
aoc_all <- readRDS(file.path(root, "aoc_z_tomex2.RDS"))

# the alignment pipe of a reactive block: from `aoc_z <- aoc_z %>%` to combine_mixture_components()
block_code <- function(start_pat = "aoc_ssd_filtered <- eventReactive", obj = "aoc_z") {
  s0 <- grep(start_pat, app, fixed = TRUE)[1]
  s <- grep(paste0(obj, " <- ", obj, " %>%"), app, fixed = TRUE)
  s <- s[s > s0][1]
  e <- grep("combine_mixture_components()", app, fixed = TRUE)
  e <- e[e > s][1]
  code <- paste(app[s:e], collapse = "\n")
  sub("combine_mixture_components\\(\\)[^\n]*$", "combine_mixture_components()", code)
}
ssd_code <- block_code()

# marine surface water defaults (Table S4 of Coffin et al. 2026)
marine <- list(alpha.input = 2.07, a.sa.input = 1.50, a.v.input = 1.48, a.m.input = 1.32,
               a.ssa.input = 1.98, R.ave.input = 0.77, p.ave.input = 1.10, H_W_ratio.input = 0.77)

run_ssd <- function(data, switch = "ingestion", params = marine, dose_check = "Particles/mL",
                    code = ssd_code, obj = "aoc_z") {
  ev <- new.env(parent = helpers)
  vars <- c(params, list(x2D_set = 5000, x1D_set = 1, x1M_set = 1, upper.tissue.trans.size.um = 88,
                         ingestion.translocation.switch = switch, dose_check = dose_check))
  for (nm in names(vars)) assign(nm, vars[[nm]], envir = ev)
  assign(obj, data, envir = ev)
  suppressWarnings(eval(parse(text = code), envir = ev))
  get(obj, envir = ev)
}

ing <- run_ssd(aoc_all, "ingestion")
tra <- run_ssd(aoc_all, "translocation")

test_that("mass-dosed tests: particle counts from the dosed mass and this alignment's geometry (E8)", {
  conv <- ing %>% filter(dose.particles.mL.master.converted.reported == "converted", !is.na(dose.mg.L.master),
                         is.finite(mean.mass.per.particle.mg))
  expect_gt(nrow(conv), 100)
  expect_true(all(conv$particle_count_basis == "from mass (alignment geometry)"))
  # the particles dosed carry exactly the dosed mass (mg/L = particles/mL x mg x 1000)
  expect_equal(conv$dose.particles.mL.master * conv$mean.mass.per.particle.mg * 1000, conv$dose.mg.L.master)
  # mean particle mass = density x mean particle volume (monodisperse: the particle's own volume)
  mono <- conv %>% filter(polydispersity == "monodisperse")
  expect_equal(mono$mean.mass.per.particle.mg,
               helpers$massfnx(v = mono$particle.volume.um3, p = mono$density.g.cm3) * 1e-3)
  # reported counts are kept
  rep_idx <- which(aoc_all$dose.particles.mL.master.converted.reported %in% "reported")
  expect_equal(ing$dose.particles.mL.master[rep_idx], aoc_all$dose.particles.mL.master[rep_idx])
  expect_true(all(ing$particle_count_basis[rep_idx] == "as reported"))
})

test_that("unit conversions use the ERM values of 1 and 5000 um particles, not the lengths (E9)", {
  sphere <- function(L) list(length = L, width = L, height = L)
  v1 <- do.call(helpers$volumefnx, sphere(1)); v2 <- do.call(helpers$volumefnx, sphere(5000))
  s1 <- do.call(helpers$SAfnx, sphere(1)); s2 <- do.call(helpers$SAfnx, sphere(5000))
  m1 <- helpers$massfnx(v1, 1.10); m2 <- helpers$massfnx(v2, 1.10)
  expect_equal(unique(ing$mu.D.v), helpers$mux_polyfnx(1.48, v2, v1))
  expect_equal(unique(ing$mu.D.sa), helpers$mux_polyfnx(1.50, s2, s1))
  expect_equal(unique(ing$mu.D.m), helpers$mux_polyfnx(1.32, m2, m1))
  expect_false(isTRUE(all.equal(unique(ing$mu.D.v), helpers$mux_polyfnx(1.48, 5000, 1))))
  x <- ing %>% filter(!is.na(EC_env_v.particles.mL))
  expect_equal(x$EC_env_v.ug.mL, x$EC_env_v.particles.mL * x$mu.D.m)
  expect_equal(x$EC_env_v.um3.mL, x$EC_env_v.particles.mL * x$mu.D.v)
})

test_that("mass and SSA ERMs start from the bioaccessible particle dose (CF_bio once; ug throughout) (E12)", {
  x <- ing %>% filter(!is.na(EC_env_m.particles.mL))
  expect_gt(nrow(x), 1000)
  expect_equal(x$EC_env_m.particles.mL, x$EC_mono_p.particles.mL * x$mu.m.mono / x$mu.m.poly * x$CF_bio)
  y <- ing %>% filter(!is.na(EC_env_ssa.particles.mL))
  expect_equal(y$EC_env_ssa.particles.mL, y$EC_mono_p.particles.mL * y$mu.ssa.mono / y$mu.ssa.poly * y$CF_bio)
  # monodisperse: mu.m.mono (ug) = density x particle volume
  mono <- ing %>% filter(polydispersity == "monodisperse", !is.na(mu.m.mono), !is.na(particle.volume.um3))
  expect_equal(mono$mu.m.mono, helpers$massfnx(mono$particle.volume.um3, mono$density.g.cm3))
  # polydisperse: smallest particle mass in ug, same as the volume-based mass
  p <- ing %>% filter(polydispersity == "polydisperse", !is.na(mass.per.particle.mg.min), !is.na(particle.volume.um3.min))
  expect_equal(p$mass.per.particle.mg.min * 1000, helpers$massfnx(p$particle.volume.um3.min, p$density.g.cm3))
})

test_that("ingestion and translocation keep partially bioaccessible polydisperse records", {
  some <- ing %>% filter(ingestible_poly == "ingestible (some)", !is.na(dose.particles.mL.master))
  expect_gt(nrow(some), 0)
  expect_true(all(!is.na(some$EC_env_v.particles.mL)))
  some_t <- tra %>% filter(translocatable_poly == "translocatable (some)", !is.na(dose.particles.mL.master))
  expect_gt(nrow(some_t), 0)
  expect_true(all(!is.na(some_t$EC_env_sa.particles.mL)))
})

test_that("database: sphere geometry and size ranges are consistent", {
  # every sphere's stored (midpoint) volume and surface area are those of a sphere
  # (ToMEx 1.0 records stored R.ave x length as the width; fixed in the tidying)
  s <- aoc_all %>% filter(shape_f == "Sphere", !is.na(size.length.um.used.for.conversions),
                          !is.na(particle.volume.um3))
  L <- s$size.length.um.used.for.conversions
  expect_gt(nrow(s), 5000)
  expect_equal(s$particle.volume.um3, pi / 6 * L^3)
  expect_equal(s$particle.surface.area.um2, pi * L^2)
  expect_equal(s$size.width.um.used.for.conversions, L)
  # a polydisperse material spans a range (min = max gives 0/0 power-law means)
  p <- aoc_all %>% filter(polydispersity == "polydisperse", !is.na(size.length.min.um.used.for.conversions),
                          !is.na(size.length.max.um.used.for.conversions))
  expect_gt(nrow(p), 2000)
  expect_true(all(p$size.length.min.um.used.for.conversions < p$size.length.max.um.used.for.conversions))
  # Zhou et al. (10.1039/d0en00952k): measured 105.7 nm, range ~80-130 nm (Fig. 1b)
  z <- aoc_all %>% filter(doi == "10.1039/d0en00952k")
  expect_equal(unique(z$size.length.um.used.for.conversions), 0.1057)
  expect_equal(unique(z$size.length.min.um.used.for.conversions), 0.08)
  expect_equal(unique(z$size.length.max.um.used.for.conversions), 0.13)
  # no NaN, infinite or negative aligned concentrations
  for (d in list(ing, tra)) {
    ec <- d %>% select(starts_with("EC_env_"))
    expect_false(any(vapply(ec, function(v) any(is.nan(v) | is.infinite(v) | (!is.na(v) & v < 0)), logical(1))))
  }
})

test_that("'none' (no bioaccessibility filter) keeps polydisperse records", {
  none <- run_ssd(aoc_all %>% filter(polydispersity == "polydisperse") %>% slice_head(n = 300), "none")
  ok <- !is.na(none$dose.particles.mL.master) & none$size.length.min.um.used.for.conversions <
    none$size.length.max.um.used.for.conversions
  expect_gt(sum(ok), 50)
  expect_true(all(is.finite(none$EC_env_v.particles.mL[ok])))
})

# a few water records for the size-distribution and mixture tests
pick <- function(shape) {
  i <- which(aoc_all$polydispersity == "polydisperse" & aoc_all$shape_f == shape &
               aoc_all$env_f == "Marine" & !is.na(aoc_all$dose.particles.mL.master) &
               aoc_all$dose.particles.mL.master.converted.reported %in% "reported" &
               !grepl("sediment", aoc_all$exposure.route, ignore.case = TRUE) &
               !is.na(aoc_all$max.size.ingest.um) &
               aoc_all$size.length.min.um.used.for.conversions < aoc_all$size.length.max.um.used.for.conversions)
  aoc_all[i[1], ]
}

test_that("measured size distributions: optional, and used only where given (design B)", {
  rec <- bind_rows(pick("Fragment"), pick("Sphere"))
  base <- run_ssd(rec)
  expect_true(all(base$poly_size_method == "assumed_env_powerlaw"))
  # empty fields change nothing
  rec_na <- rec %>% mutate(size.dist.family = NA_character_, size.dist.median.um = NA_real_, size.dist.gsd = NA_real_)
  expect_equal(run_ssd(rec_na)$EC_env_v.particles.mL, base$EC_env_v.particles.mL)
  # a log-normal distribution on the first record only
  rec_ln <- rec_na
  rec_ln$size.dist.family[1] <- "lognormal"
  rec_ln$size.dist.median.um[1] <- sqrt(rec$size.length.min.um.used.for.conversions[1] *
                                          rec$size.length.max.um.used.for.conversions[1])
  rec_ln$size.dist.gsd[1] <- 1.5
  ln <- run_ssd(rec_ln)
  expect_equal(ln$poly_size_method, c("measured_lognormal", "assumed_env_powerlaw"))
  expect_false(isTRUE(all.equal(ln$EC_env_v.particles.mL[1], base$EC_env_v.particles.mL[1])))
  expect_equal(ln$EC_env_v.particles.mL[2], base$EC_env_v.particles.mL[2])
  expect_error(run_ssd(rec_na %>% mutate(size.dist.family = "weibull")), "size.dist.family")
})

test_that("co-dosed mixtures: one record per mixture, components added by share (design C)", {
  rec <- pick("Fragment")
  single <- run_ssd(rec)
  halves <- rec[c(1, 1), ] %>% mutate(mixture_id = "m1", component.fraction = 0.5)
  m <- run_ssd(halves)
  expect_equal(nrow(m), 1)
  expect_equal(m$EC_env_v.particles.mL, single$EC_env_v.particles.mL)
  expect_equal(m$EC_env_m.ug.mL, single$EC_env_m.ug.mL)
  # different shapes: the dominant component's labels, the components in mixture_shapes
  two <- rec[c(1, 1), ] %>% mutate(mixture_id = "m2", component.fraction = c(0.3, 0.7))
  two$shape_f <- factor(c("Fragment", "Sphere"), levels = levels(aoc_all$shape_f))
  parts <- run_ssd(two %>% mutate(mixture_id = NA_character_))
  m2 <- run_ssd(two)
  expect_equal(as.character(m2$shape_f), "Sphere")
  expect_equal(m2$mixture_shapes, "Fragment; Sphere")
  expect_equal(m2$EC_env_v.particles.mL, sum(c(0.3, 0.7) * parts$EC_env_v.particles.mL))
  expect_equal(m2$dose.particles.mL.master, rec$dose.particles.mL.master)
  # a mixture next to ordinary records leaves those unchanged
  both <- bind_rows(pick("Sphere") %>% mutate(mixture_id = NA_character_), two)
  b <- run_ssd(both)
  expect_equal(nrow(b), 2)
  expect_equal(b$EC_env_v.particles.mL[1], run_ssd(pick("Sphere"))$EC_env_v.particles.mL)
})

test_that("mixture shares when counts were derived from the dosed mass", {
  rec <- pick("Fragment") %>% mutate(dose.particles.mL.master.converted.reported = "converted",
                                     dose.mg.L.master = 1)
  two <- rec[c(1, 1), ] %>% mutate(mixture_id = "m3", component.fraction = c(0.3, 0.7),
                                   density.g.cm3 = c(1.05, 1.38))
  two$shape_f <- factor(c("Fragment", "Sphere"), levels = levels(aoc_all$shape_f))
  for (b in c("number", "mass")) {
    two$component.fraction.basis <- b
    parts <- run_ssd(two %>% mutate(mixture_id = NA_character_))
    m <- run_ssd(two)
    comp <- attr(m, "components")
    n_i <- comp$.component_share * parts$dose.particles.mL.master
    mass_i <- n_i * parts$mean.mass.per.particle.mg * 1000
    expect_equal(sum(mass_i), 1)
    if (b == "number") expect_equal(n_i / sum(n_i), c(0.3, 0.7))
    if (b == "mass") expect_equal(mass_i / sum(mass_i), c(0.3, 0.7))
    expect_equal(m$EC_env_v.particles.mL, sum(comp$.component_share * parts$EC_env_v.particles.mL))
  }
})

test_that("test_material_functions.R is a verbatim copy of the PSSDplusplus sources", {
  pk <- file.path(root, "..", "ToMEx2.0_EcoToxRisk", "package", "R")
  skip_if_not(dir.exists(pk), "PSSDplusplus sources (../ToMEx2.0_EcoToxRisk) not found")
  rd <- function(p) gsub("\r", "", paste(readLines(p, warn = FALSE), collapse = "\n"))
  here <- rd(file.path(root, "test_material_functions.R"))
  expect_true(grepl(rd(file.path(pk, "poly_size_dist.R")), here, fixed = TRUE))
  mix <- rd(file.path(pk, "align_mixture.R"))
  comb <- substring(mix, regexpr("#' Combine the aligned components", mix, fixed = TRUE))
  expect_true(grepl(comb, here, fixed = TRUE))
})

test_that("same particle-based ERM doses as PSSDplusplus::align_data() (when >= 0.4.1 is installed)", {
  skip_if_not(requireNamespace("PSSDplusplus", quietly = TRUE) &&
                utils::packageVersion("PSSDplusplus") >= "0.4.1", "PSSDplusplus >= 0.4.1 not installed")
  m <- list(R.ave = 0.77, H_W_ratio = 0.77, p.ave = 1.10, alpha = 2.07, a.sa = 1.5,
            a.v = 1.48, a.m = 1.32, a.ssa = 1.98)
  pa <- list()
  for (env in c("freshwater", "sediment.freshwater", "sediment.marine", "marine"))
    for (k in names(m)) pa[[paste0(k, ".", env, "_input")]] <- m[[k]]
  raw <- aoc_all %>% mutate(.row = row_number())
  pk <- suppressWarnings(suppressMessages(do.call(PSSDplusplus::align_data, c(list(df = raw, verbose = FALSE), pa))))
  # the package sends sediment exposures through per-kg doses; the app drops plants/algae from ingestion
  pk <- pk %>% filter(!grepl("sediment", exposure.route, ignore.case = TRUE), !Group %in% c("Algae", "Plant"))
  a_v <- ing$EC_env_v.particles.mL[pk$.row]
  a_sa <- tra$EC_env_sa.particles.mL[pk$.row]
  expect_equal(is.na(a_v), is.na(pk$EC_env_v.particles.mL_ingest))
  expect_equal(a_v[!is.na(a_v)], pk$EC_env_v.particles.mL_ingest[!is.na(a_v)])
  expect_equal(is.na(a_sa), is.na(pk$EC_env_sa.particles.mL_trans))
  expect_equal(a_sa[!is.na(a_sa)], pk$EC_env_sa.particles.mL_trans[!is.na(a_sa)])
})
