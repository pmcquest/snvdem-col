# ============================================================================
# Does the LISA map show a "buffer ring" of outliers between the HH and LL
# clusters?
#
# LISA maps are sometimes drawn (e.g., the illustration at
# https://stats.stackexchange.com/questions/335919) with High-Low/Low-High
# outliers forming a ring around a High-High core, right at the transition to
# a Low-Low periphery -- because a smoothly-declining spatial gradient
# naturally produces "high value, just-barely-low neighbor average" (or vice
# versa) municipios exactly at the inflection point between two clusters.
#
# This script checks whether that pattern holds for Colombia's 2011 sndem
# LISA map by directly inspecting which cluster types are queen-contiguous
# neighbors of which other cluster types. Answer (see printed output):
# High-High and Low-Low NEVER share a boundary (0 of ~919 HH neighbor-pairs
# are Low-Low) -- every transition between the two big clusters passes
# through a wide band of "Not significant" municipios instead. The few
# High-Low/Low-High outliers that do exist are not sitting on that boundary;
# they're two separate, unrelated substantive stories (see the printed
# per-outlier neighbor lists).
# ============================================================================

suppressPackageStartupMessages({
  library(spdep)
  library(dplyr)
})

out_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/cluster/output"
obj <- readRDS(file.path(out_dir, "lisa_weights.rds"))
nb <- obj$nb
shp <- obj$shp

cl <- read.csv(file.path(out_dir, "lisa_clusters_2011.csv"), colClasses = c(MPIO_CDPMP = "character"))
cl <- cl[match(shp$MPIO_CDPMP, cl$MPIO_CDPMP), ]
stopifnot(all(cl$MPIO_CDPMP == shp$MPIO_CDPMP))

cluster <- cl$cluster
names(cluster) <- cl$MPIO_CDPMP

# --- Per-outlier neighbor lists ---------------------------------------------
# For each High-Low / Low-High municipio, print its own cluster label and
# every one of its queen-contiguous neighbors' labels. If the "buffer ring"
# story were true, we'd expect these neighbor lists to contain a mix of
# High-High and Low-Low municipios (i.e., the outlier sits right where the
# two clusters meet).
cat("=== Outlier municipios (HL / LH) and their queen-contiguous neighbors ===\n\n")
outlier_idx <- which(cluster %in% c("High-Low", "Low-High"))
for (i in outlier_idx) {
  nbrs <- nb[[i]]
  nbrs <- nbrs[nbrs != 0]
  cat(sprintf("%s (%s, dept %s) -- self: %s\n", cl$MPIO_CDPMP[i], cl$MPIO_CNMBR[i], cl$DPTO_CCDGO[i], cluster[i]))
  if (length(nbrs) == 0) {
    cat("   no neighbors\n")
  } else {
    for (j in nbrs) {
      cat(sprintf("   neighbor %s (%s): %s\n", cl$MPIO_CDPMP[j], cl$MPIO_CNMBR[j], cluster[j]))
    }
  }
  cat("\n")
}

# --- Cluster-to-cluster adjacency matrix ------------------------------------
# For every queen-contiguous pair of municipios (i, j), tally what cluster
# type i is and what cluster type j is. The (High-High, Low-Low) cell tells
# us directly whether the two big clusters ever physically touch.
cat("\n=== Cluster-to-cluster adjacency counts (every queen-contiguous pair) ===\n")
lv <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not significant", "No neighbors")
adj_tab <- matrix(0, nrow = 6, ncol = 6, dimnames = list(lv, lv))
for (i in seq_along(nb)) {
  nbrs <- nb[[i]]; nbrs <- nbrs[nbrs != 0]
  if (length(nbrs) == 0) next
  for (j in nbrs) {
    adj_tab[cluster[i], cluster[j]] <- adj_tab[cluster[i], cluster[j]] + 1
  }
}
print(adj_tab)

cat("\nHH-LL direct adjacent pairs (out of", sum(adj_tab["High-High", ]), "total HH neighbor-pairs):",
    adj_tab["High-High", "Low-Low"], "\n")
cat("HH-'Not significant' adjacent pairs:", adj_tab["High-High", "Not significant"], "\n")
cat("LL-'Not significant' adjacent pairs:", adj_tab["Low-Low", "Not significant"], "\n")
