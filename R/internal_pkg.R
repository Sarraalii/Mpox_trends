Sys.setenv(GITHUB_PAT = "d9b68eb06316f7e322c660103b187cbbcf024f72")

if (!require(remotes)) {
  install.packages("remotes")
}
if (!require(phifunc)) {
  remotes::install_github("whocov/phifunc", subdir = "phifunc", dep = TRUE, force = TRUE)
}

if (!require(whomapper)) {
  remotes::install_github("whocov/whomapper", dependencies = TRUE)
}

if (!require(whotools)) {
  remotes::install_github("whocov/whotools", dependencies = TRUE)
}


if (!require(treeio)) {
  remotes::install_github("GuangchuangYu/treeio")
}

### NB - this package is not currently a dep but may be in the future. we leave this for the future.
# if (!require(soc_test)) {
#   remotes::install_github("whocov/soc_test@bayesian", dependencies = TRUE)
# }