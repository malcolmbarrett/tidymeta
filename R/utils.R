#  variables used in various NSE calls
utils::globalVariables(
  c(".",
    "study",
    "pred",
    "se",
    "pi.lb",
    "pi.ub",
    ".fitted",
    ".pil.fit",
    ".piu.fit",
    "meta",
    "study",
    "type",
    "residuals",
    "rstandard",
    "resid",
    "weights",
    "weight",
    "type",
    "hat",
    "cook.d",
    "rstudent",
    "dffits",
    "cov.r",
    "tau2.del",
    "dfbetas",
    "QE",
    "H2",
    "cumul_estimate",
    "cumul_conf.low",
    "cumul_conf.high",
    "conf.low",
    "conf.high",
    ":=",
    "l1o_estimate",
    "l1o_conf.low",
    "l1o_conf.high",
    "cuts",
    "data",
    "key",
    "value",
    "QE.del",
    "estimate",
    "Q"
    )
)

metafor2df <- function(x) {
  x %>%
    summary() %>%
    as.data.frame(stringsAsFactors = FALSE)
}
