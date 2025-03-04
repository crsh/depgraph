multibridge_pkg <- devtools::as.package("data-raw/.")
multibridge_pkg$path <- NULL

save(multibridge_pkg, file = "data/multibridge_pkg.rda")
