library(matrixStats)


# positives and negatives mixing.
tools::assertError(validateIndices(1:-1, 1))
tools::assertError(validateIndices(-1:1, 1))
tools::assertError(validateIndices(c(1, 0, 0, -4), 2))
tools::assertError(validateIndices(c(-4, 0, 0, 1), 6))


# negative indices
y <- validateIndices(c(-4, 0, -3, -1), 5)
stopifnot(identical(y, c(2, 5)))


# positive indices
y <- validateIndices(c(4, 4, 8, 2, 3), 8)
stopifnot(identical(y, c(4, 4, 8, 2, 3)))

validateIndices(c(4, 4, 8, 2, 3), 9)
stopifnot(identical(y, c(4, 4, 8, 2, 3)))


# positive out of ranges
tools::assertError(validateIndices(c(4, 4, 8, 2, 3), 7))


# negative out of ranges
y <- validateIndices(c(-4, 0, -3, -1), 3)
stopifnot(identical(y, c(2)))


# negative indices exclude all
y <- validateIndices(c(-4, 0, -3, -1, -2, 0), 3)
stopifnot(identical(y, numeric(0)))


# idxs is single integer
y <- validateIndices(4, 5)
stopifnot(identical(y, c(4)))

y <- validateIndices(-4, 5)
stopifnot(identical(y, c(1, 2, 3, 5)))

y <- validateIndices(0, 5)
stopifnot(identical(y, numeric(0)))


# idxs is empty
y <- validateIndices(numeric(0), 5)
stopifnot(identical(y, numeric(0)))


# idxs is NULL
#y <- validateIndices(NULL, 5)
#stopifnot(identical(y, 1:5))

#y <- validateIndices(, 5)
#stopifnot(identical(y, 1:5))


# N is 0
#tools::assertError(validateIndices(4:4, 0))

#y <- validateIndices(-4:-4, 0)
#stopifnot(identical(y, integer(0)))

#y <- validateIndices(0:0, 0)
#stopifnot(identical(y, integer(0)))


# NA in idxs
tools::assertError(validateIndices(c(NA, -2), 2))

y <- validateIndices(c(NA, 0, 2), 2)
stopifnot(identical(y, c(NA, 2)))
