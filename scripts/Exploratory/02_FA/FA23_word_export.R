# Export Table 1 (Baseline) and Table 2 (No-Geography/Refined) from FA23.R
# to a native Word document, with proper header spanning and bold footer
# rows -- kableExtra's HTML/CSS styling (used for the console/RStudio view
# in FA23.R) doesn't translate to .docx, so this rebuilds the same df1/df2
# table structure using flextable/officer instead.

library(flextable)
library(officer)

source("G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/02_FA/FA23_setup.R")
# Loads the data, fits the models, and builds df1 (Baseline, 12 vars) and
# df2 (No-Geography, 8 vars) -- the same objects FA23.R renders as kable HTML.

# Convert a final_df (df1 or df2) into a Word-ready flextable
make_word_table <- function(final_df, n_vars) {
  emel_cols <- grep("^EMEL_", names(final_df), value = TRUE)
  cscw_cols <- grep("^CSCW_", names(final_df), value = TRUE)
  ordered_cols <- c("Variable", emel_cols, cscw_cols)
  df <- final_df[, ordered_cols]

  ft <- flextable(df) %>%
    set_header_labels(values = setNames(as.list(sub("^(EMEL|CSCW)_", "", ordered_cols)), ordered_cols)) %>%
    add_header_row(values = c("", rep("Electoral Pillar (EMEL)", length(emel_cols)),
                               rep("Civil Liberties Pillar (CSCW)", length(cscw_cols))),
                    colwidths = c(1, rep(1, length(emel_cols)), rep(1, length(cscw_cols)))) %>%
    merge_h(part = "header") %>%
    theme_vanilla() %>%
    align(align = "center", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%
    bold(part = "header") %>%
    bold(i = (nrow(df) - 3):nrow(df)) %>%
    bg(i = (nrow(df) - 3):nrow(df), bg = "#eeeeee") %>%
    bg(part = "header", i = 1, j = 2:(1 + length(emel_cols)), bg = "#dbe9f6") %>%
    bg(part = "header", i = 1, j = (2 + length(emel_cols)):(1 + length(emel_cols) + length(cscw_cols)), bg = "#f9ede0") %>%
    fontsize(size = 9, part = "all") %>%
    autofit()
  ft
}

ft1 <- make_word_table(df1, 12)
ft2 <- make_word_table(df2, 8)

# Assemble Word Document ----
doc <- read_docx() %>%
  body_add_par("SNVDEM Factor Analysis: Table 1 and Table 2", style = "heading 1") %>%
  body_add_par("Table 1: Baseline Structural Matrix (12 vars, incl. geography)", style = "heading 2") %>%
  body_add_flextable(ft1) %>%
  body_add_par(sprintf("Sampling adequacy: KMO = %.3f | Bartlett p = %.3e (KMO > 0.6 and p < .05 indicate suitability for factor analysis)",
                        base_diag$kmo, base_diag$bart_p), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("Table 2: Refined Structural Matrix (8 vars, no geography)", style = "heading 2") %>%
  body_add_flextable(ft2) %>%
  body_add_par(sprintf("Sampling adequacy: KMO = %.3f | Bartlett p = %.3e (KMO > 0.6 and p < .05 indicate suitability for factor analysis)",
                        trim_diag$kmo, trim_diag$bart_p), style = "Normal")

out_path <- "G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/02_FA/FA_Tables_v2.docx"
print(doc, target = out_path)
cat("Saved:", out_path, "\n")
