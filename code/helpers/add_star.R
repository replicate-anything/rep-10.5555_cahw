# Append significance stars to formatted estimates (|z| thresholds).
add_star <- function(estimate, se) {
  z <- abs(estimate / se)
  star <- ifelse(
    is.na(z),
    "",
    ifelse(
      z >= 2.576,
      "***",
      ifelse(z >= 1.96, "**", ifelse(z >= 1.645, "*", ""))
    )
  )
  paste0(sprintf("%.2f", estimate), star)
}
