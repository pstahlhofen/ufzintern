# Note: This file was copied from the bigdist package.
# I had to modify the function 'bigdist_subset', because the original version had a bug.
# In the orginial code, trying to build a subset including the last row of the bigdist object
# would result in an error.
# So I changed '<' to '<=' in the assertation and made it work.
# (I also opened an issue at GitHub but as the last commit was more than a year age,
# I'm not very optimistic that anyone will read it.)

bigdist_subset <- function (x, index, file) 
{
    assertthat::assert_that(inherits(x, "bigdist"))
    index <- unique(index)
    sz <- x[["fbm"]][["ncol"]]
    type <- names(x[["fbm"]][["type"]])
    assertthat::assert_that(bigdist:::is_integerish(index) && all(index) > 
        0)
    assertthat::assert_that(max(index) <= sz)
    filename <- paste0(paste(file, length(index), type, sep = "_"), 
        ".bk")
    assertthat::assert_that(!file.exists(filename))
    assertthat::assert_that(assertthat::is.writeable(dirname(filename)))
    xCopy <- bigstatsr::big_copy(x[["fbm"]], ind.row = index, 
        ind.col = index, type = names(x[["fbm"]][["type"]]), 
        backingfile = paste(file, length(index), type, sep = "_"))
    res <- structure(list(fbm = xCopy), class = "bigdist")
    return(res)
}
