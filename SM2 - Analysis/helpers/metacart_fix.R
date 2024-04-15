find_triplet <- function (xk, nodeMbrship, y, vi, minbucket, minsplit)
{
  leaves <- as.numeric(names(table(nodeMbrship)[table(nodeMbrship) >=
                                                  minsplit]))
  if (is.numeric(xk)) {
    xk.rank <- NULL
    tempQ <- sapply(leaves, function(x) re.cutoff_cpp(y,
                                                      vi, xk[nodeMbrship == x], nodeMbrship == x, nodeMbrship,
                                                      minbucket))
  }
  else {
    xk.rank <- lapply(leaves, function(x) rank(tapply(y[nodeMbrship ==
                                                          x], xk[nodeMbrship == x], mean)))
    names(xk.rank) <- leaves
    tempQ <- sapply(leaves, function(x) re.cutoff_cpp(y,
                                                      vi, xk.rank[[as.character(x)]][as.character(xk[nodeMbrship ==
                                                                                                       (x)])], nodeMbrship == x, nodeMbrship, minbucket))
  }
  if (inherits(tempQ, "list")) {
    if (all(sapply(tempQ, is.null))) {
      list(pleaf = NA, cstar = c(NA, -Inf, NA), rank = NULL)
    }
    else {
      pleaf <- which.max(sapply(tempQ, function(x) if (is.null(x))
        -Inf
        else x[2]))
      list(pleaf = leaves[pleaf], cstar = tempQ[[pleaf]],
           rank = xk.rank[[pleaf]])
    }
  }
  else {
    pleaf <- which.max(tempQ[2, ])
    list(pleaf = leaves[pleaf], cstar = tempQ[, pleaf], rank = xk.rank[[pleaf]])
  }
}

environment(find_triplet) <- asNamespace("metacart")
assignInNamespace("find_triplet", find_triplet, ns = "metacart")

