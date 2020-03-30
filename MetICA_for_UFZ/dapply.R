#Note: This function was copied from an older version of the disto package (commit db3f2c269224710772796472552aee0f6ff230b3).
#I had to use it, because the current version has a bug and yields wrong results.
#For a description of the issue, see https://github.com/talegari/disto/issues/1

dapply <- function(x, margin = 1, fun, subset, simplify = TRUE, nproc = 1){

  assertthat::assert_that(inherits(x, "disto"))
  assertthat::assert_that(assertthat::is.scalar(margin) && margin %in% 1:2)
  assertthat::assert_that(is.function(fun))
  assertthat::assert_that(assertthat::is.count(nproc))

  size <- size(x)
  if(missing(subset)){
    subset <- 1:size
  } else {
    assertthat::assert_that(all(subset %in% 1:size))
  }

  if(.Platform$OS.type == "unix"){
    res <- parallel::mcmapply(function(s) fun(x[s, ])
                              , subset
                              , SIMPLIFY = simplify
                              , mc.cores = nproc
                              )
  } else {
    res <- sapply(subset, function(s) fun(x[s, ]), simplify = simplify)
  }
  return(res)
}
