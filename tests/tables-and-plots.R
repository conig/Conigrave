library(Conigrave)
d <- mtcars
d$wt[1] <- NA
counts <- ctrx(d, c("wt", "drat"), c("mpg", "hp"), n.matrix = TRUE)
stopifnot(is.numeric(counts$wt), all(counts$wt == 31), all(counts$drat == 32))
r <- ctrx(d, "wt", "mpg", leading.zero = FALSE, describe = TRUE)
stopifnot(r$wt == sub("^(-?)0", "\\1", paste0(sprintf("%.2f", cor(d$wt, d$mpg, use="complete.obs")), "***")),
          abs(r$M - round(mean(d$mpg), 2)) < 1e-8)
if (requireNamespace("ppcor", quietly = TRUE)) {
  control <- ppcor::pcor.test(na.omit(d)$wt, na.omit(d)$mpg, na.omit(d)$hp)
  partial <- ctrx(d, "wt", "mpg", partial = "hp", stars = NULL)
  stopifnot(partial$wt == sprintf("%.2f", control$estimate))
} else {
  msg <- tryCatch(ctrx(d, "wt", "mpg", partial = "hp"), error = conditionMessage)
  stopifnot(grepl("ppcor", msg))
}
# Missing controls and missing pair values must both affect counts.
d$hp[2] <- NA
partial_n <- ctrx(d, c("wt", "drat"), "mpg", partial = "hp", n.matrix = TRUE)
stopifnot(partial_n$wt == 30, partial_n$drat == 31)
mi <- mitools::imputationList(list(d, mtcars))
stopifnot(is.na(ctrx(mi, "wt", "mpg", n.matrix = TRUE)$wt))
stopifnot(inherits(try(ctrx(mitools::imputationList(list(mtcars, mtcars["wt"])),
                            "wt", "mpg"), silent = TRUE), "try-error"))
# The second imputation has a different relationship.
frames <- list(mtcars, transform(mtcars, mpg = mpg + 0.01 * cyl * disp))
mi <- mitools::imputationList(frames)
if (requireNamespace("miceadds", quietly = TRUE)) {
  long <- ctrx(mi, c("mpg", "wt", "disp"), output = "long")
  stopifnot(nrow(long) == 3L, all(long$x != long$y), all(long$n == 32L))
  for (i in seq_len(nrow(long))) {
    ref <- miceadds::micombine.cor(mi, c(long$x[i], long$y[i]))
    stopifnot(all.equal(long$estimate[i], ref$r[1]), all.equal(long$p.value[i], ref$p[1]))
  }
} else {
  msg <- tryCatch(ctrx(mi, c("mpg", "wt")), error = conditionMessage)
  stopifnot(grepl("miceadds", msg))
}
ranked <- suppressWarnings(ctrx(mtcars, "wt", "mpg", method = "spearman", output = "long"))
ref <- suppressWarnings(cor.test(mtcars$wt, mtcars$mpg, method = "spearman"))
stopifnot(all.equal(ranked$estimate, unname(ref$estimate)), all.equal(ranked$p.value, ref$p.value),
          nrow(ctrx(mtcars, "mpg", output = "long")) == 0L)

# A sparse or constant variable must not prevent reporting the other pairs.
sparse <- transform(mtcars, absent = NA_real_, constant = 1)
for (data in list(sparse, mitools::imputationList(list(sparse, sparse)))) {
  if (!is.data.frame(data) && !requireNamespace("miceadds", quietly = TRUE)) next
  tab <- suppressWarnings(ctrx(data, "wt", c("mpg", "absent", "constant"),
                               method = "spearman", output = "long"))
  stopifnot(is.finite(tab$estimate[1]), identical(tab$n, c(32L, 0L, 32L)),
            all(is.na(tab$estimate[-1])), all(is.na(tab$p.value[-1])))
}
